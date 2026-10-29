open Test.Import

let with_project ~file files f =
  let dir, source, position = Implementation.setup ~request_file:file files in
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  Test.run_initialized
    ~handler
    ~cwd:dir
    ~rootUri:(Uri.of_path dir)
    ~timeout:10.
    (fun client ->
       let uri = Uri.of_path (Filename.concat dir file) in
       let* () = Test.open_document ~client ~uri ~source () in
       let* () = f client uri position in
       let* () = Client.request client Shutdown in
       Client.stop client)
;;

let%expect_test "cross-file definitions validate OCaml 5.5 source digests" =
  with_project
    ~file:"b.ml"
    [ "a.mli", "val value : int\n"
    ; "a.ml", "let value = 42\n"
    ; "b.ml", "let use = A.$value\n"
    ]
    (fun client uri position ->
       let+ locations =
         Client.request
           client
           (TextDocumentDefinition
              (DefinitionParams.create
                 ~textDocument:(TextDocumentIdentifier.create ~uri)
                 ~position
                 ()))
       in
       Test.print_locations locations);
  [%expect
    {|
    a.mli
    {
      "end": { "character": 9, "line": 0 },
      "start": { "character": 4, "line": 0 }
    }
    |}]
;;

let%expect_test "references join type declaration anchors across compilation units" =
  with_project
    ~file:"a.ml"
    [ "a.ml", "type $t = A | B of int\n"; "b.ml", "let use (x : A.t) = x\n" ]
    (fun client uri position ->
       let+ locations =
         Client.request
           client
           (TextDocumentReferences
              (ReferenceParams.create
                 ~textDocument:(TextDocumentIdentifier.create ~uri)
                 ~position
                 ~context:(ReferenceContext.create ~includeDeclaration:false)
                 ()))
       in
       List.iter (Option.value locations ~default:[]) ~f:(fun location ->
         print_endline (Filename.basename (Uri.to_path location.Location.uri))));
  [%expect {| b.ml |}]
;;

let%expect_test "call hierarchy resolves callers in closed compilation units" =
  with_project
    ~file:"a.ml"
    [ "a.ml", "let $callee x = x + 1\n"; "b.ml", "let caller x = A.callee x\n" ]
    (fun client uri position ->
       let* items =
         Client.request
           client
           (TextDocumentPrepareCallHierarchy
              (CallHierarchyPrepareParams.create
                 ~textDocument:(TextDocumentIdentifier.create ~uri)
                 ~position
                 ()))
       in
       let item = Option.value_exn items |> List.hd_exn in
       let+ calls =
         Client.request
           client
           (CallHierarchyIncomingCalls (CallHierarchyIncomingCallsParams.create ~item ()))
       in
       List.iter (Option.value calls ~default:[]) ~f:(fun call ->
         print_endline call.CallHierarchyIncomingCall.from.name));
  [%expect {| caller |}]
;;

let%expect_test "Dune definitions resolve unsaved local dependency names" =
  let dir = Test.temp_dir "dune-navigation" in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 3.0)\n";
  let lib = Filename.concat dir "lib"
  and bin = Filename.concat dir "bin" in
  Unix.mkdir lib 0o700;
  Unix.mkdir bin 0o700;
  Test.write_file (Filename.concat lib "dune") "(library (name local_lib))\n";
  Test.write_file (Filename.concat bin "dune") "(executable (name main))\n";
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  Test.run_initialized ~handler ~cwd:dir ~rootUri:(Uri.of_path dir) (fun client ->
    let uri = Uri.of_path (Filename.concat bin "dune") in
    let source, position =
      Test.parse_cursor "(executable (name main) (libraries $local_lib))\n"
    in
    let textDocument =
      TextDocumentItem.create ~uri ~languageId:(Other "dune") ~version:0 ~text:source
    in
    let* () =
      Client.notification
        client
        (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
    in
    let* locations =
      Client.request
        client
        (TextDocumentDefinition
           (DefinitionParams.create
              ~textDocument:(TextDocumentIdentifier.create ~uri)
              ~position
              ()))
    in
    (match locations with
     | Some (`Location [ location ]) ->
       assert (Uri.equal location.uri (Uri.of_path (Filename.concat lib "dune")));
       print_endline "local library resolved"
     | _ -> failwith "expected local Dune dependency");
    let* () = Client.request client Shutdown in
    Client.stop client);
  [%expect {| local library resolved |}]
;;
