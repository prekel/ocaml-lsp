open Test.Import

let command_name = "ocamllsp/show-ppx-expansion"
let kind = CodeActionKind.Other "ocamllsp.showPpxExpansion"
let fixture name = Filename.concat (Sys.getcwd ()) ("ppx_source_fixture/" ^ name)
let uri = DocumentUri.of_path (fixture "fixture.ml")

let position source needle =
  String.split_lines source
  |> List.find_mapi ~f:(fun line text ->
    let rec find character =
      if character + String.length needle > String.length text
      then None
      else if
        String.equal (String.sub text ~pos:character ~len:(String.length needle)) needle
      then Some (Position.create ~line ~character)
      else find (character + 1)
    in
    find 0)
  |> Option.value_exn
;;

let run capture ?(support = true) ?(on_show = fun _ -> assert false) f =
  let on_request (type r) _ (request : r Lsp.Server_request.t)
    : (r Lsp_fiber.Rpc.Reply.t * unit) Fiber.t
    =
    match request with
    | ShowDocumentRequest params ->
      let success = on_show params in
      Fiber.return (Lsp_fiber.Rpc.Reply.now (ShowDocumentResult.create ~success), ())
    | _ -> assert false
  in
  let on_notification _ _ = Fiber.return () in
  let handler =
    Client.Handler.make ~on_request:{ Client.Handler.on_request } ~on_notification ()
  in
  let output =
    Test.run ~handler (fun client ->
      let start () =
        let window =
          WindowClientCapabilities.create
            ~showDocument:(ShowDocumentClientCapabilities.create ~support)
            ()
        in
        Client.start
          client
          (InitializeParams.create
             ~capabilities:(ClientCapabilities.create ~window ())
             ())
      in
      let work () =
        let* (_ : InitializeResult.t) = Client.initialized client in
        let* () = f client in
        let output = capture () in
        let* () = Client.request client Shutdown in
        let+ () = Client.stop client in
        output
      in
      Fiber.fork_and_join_unit start work)
  in
  let (_ : string) = capture () in
  print_endline output
;;

let actions ?(only = [ kind ]) ?end_ client uri start =
  let range = Range.create ~start ~end_:(Option.value end_ ~default:start) in
  let context = CodeActionContext.create ~diagnostics:[] ~only () in
  let+ response =
    Client.request
      client
      (CodeAction
         (CodeActionParams.create
            ~textDocument:(TextDocumentIdentifier.create ~uri)
            ~range
            ~context
            ()))
  in
  Option.value response ~default:[]
  |> List.filter_map ~f:(function
    | `CodeAction { CodeAction.command = Some command; _ }
      when String.equal command.command command_name -> Some command
    | _ -> None)
;;

let execute client (command : Command.t) =
  Client.request
    client
    (ExecuteCommand
       (ExecuteCommandParams.create
          ~command:command.Command.command
          ?arguments:command.arguments
          ()))
;;

let print_error f =
  let+ result = Fiber.collect_errors f in
  match result with
  | Ok _ -> failwith "expected a command error"
  | Error [ { exn = Jsonrpc.Response.Error.E error; _ } ] ->
    print_endline (Yojson.Safe.to_string (Jsonrpc.Response.Error.yojson_of_t error))
  | Error _ -> failwith "unexpected exception"
;;

let print_hover client uri position =
  let+ result =
    Client.request
      client
      (TextDocumentHover
         (HoverParams.create
            ~textDocument:(TextDocumentIdentifier.create ~uri)
            ~position
            ()))
  in
  match result with
  | None -> print_endline "no hover"
  | Some { Hover.contents = `MarkupContent content; _ } -> print_endline content.value
  | Some { Hover.contents = `MarkedString content; _ } -> print_endline content.value
  | Some _ -> assert false
;;

let%expect_test "expansion action is restricted to the deriving attribute" =
  let source = Io.String_path.read_file (fixture "fixture.ml") in
  run
    (fun () -> [%expect.output])
    (fun client ->
       let* () = Test.openDocument ~client ~uri ~source in
       let* () =
         Fiber.sequential_iter
           [ "address"; "string"; "[@@"; "deriving"; "source_locations"; "let read" ]
           ~f:(fun needle ->
             let+ commands = actions client uri (position source needle) in
             Printf.printf "%s: %d\n" needle (List.length commands))
       in
       let start = position source "[@@" in
       let end_ = { start with character = 62 } in
       let* commands = actions client uri end_ in
       Printf.printf "after attribute: %d\n" (List.length commands);
       let* commands = actions ~end_:start client uri (position source "address") in
       Printf.printf "selection starts outside: %d\n" (List.length commands);
       let+ commands = actions ~only:[ CodeActionKind.QuickFix ] client uri start in
       Printf.printf "quickfix only: %d\n" (List.length commands));
  [%expect
    {|
    address: 0
    string: 0
    [@@: 1
    deriving: 1
    source_locations: 1
    let read: 0
    after attribute: 0
    selection starts outside: 0
    quickfix only: 0 |}]
;;

let%expect_test "no expansion action without showDocument support" =
  let source = Io.String_path.read_file (fixture "fixture.ml") in
  run
    (fun () -> [%expect.output])
    ~support:false
    (fun client ->
       let* () = Test.openDocument ~client ~uri ~source in
       let+ commands = actions client uri (position source "deriving") in
       Printf.printf "actions: %d\n" (List.length commands));
  [%expect {| actions: 0 |}]
;;

let%expect_test "open expansion from the live buffer and reject stale actions" =
  let original = Io.String_path.read_file (fixture "fixture.ml") in
  let source = "type address = { city : int } [@@deriving source_locations]\n" in
  let opened = ref [] in
  let on_show params =
    assert (params.ShowDocumentParams.takeFocus = Some true);
    let path = Uri.to_path params.uri in
    opened := path :: !opened;
    assert (Filename.extension path = ".ml");
    print_endline (Io.String_path.read_file path);
    true
  in
  run
    (fun () -> [%expect.output])
    ~on_show
    (fun client ->
       let* () = Test.openDocument ~client ~uri ~source in
       let* commands = actions client uri (position source "source_locations") in
       let command = List.hd commands in
       let* (_ : Yojson.Safe.t) = execute client command in
       assert (String.equal original (Io.String_path.read_file (fixture "fixture.ml")));
       let* () =
         Client.notification
           client
           (TextDocumentDidChange
              (DidChangeTextDocumentParams.create
                 ~textDocument:(VersionedTextDocumentIdentifier.create ~uri ~version:1)
                 ~contentChanges:
                   [ TextDocumentContentChangeEvent.create ~text:(source ^ "\n") () ]))
       in
       let* () = print_error (fun () -> execute client command) in
       let* commands = actions client uri (position source "deriving") in
       let+ (_ : Yojson.Safe.t) = execute client (List.hd commands) in
       ());
  List.iter !opened ~f:Sys.remove;
  [%expect
    {|
    (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    let source_address input =
      let local_helper local_helper = local_helper in
      local_helper (input : int list)
    let _ = source_address

    {"code":-32801,"message":"The document changed; request the code action again"}
    (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    let source_address input =
      let local_helper local_helper = local_helper in
      local_helper (input : int list)
    let _ = source_address |}]
;;

let%expect_test "signature hovers cover recursive groups and included generated modules" =
  let on_show params =
    let path = Uri.to_path params.ShowDocumentParams.uri in
    Printf.printf "opened %s\n" (Filename.extension path);
    print_endline (Io.String_path.read_file path);
    Sys.remove path;
    true
  in
  run
    (fun () -> [%expect.output])
    ~on_show
    (fun client ->
       Fiber.sequential_iter [ "rich.ml"; "rich.mli" ] ~f:(fun filename ->
         let source = Io.String_path.read_file (fixture filename) in
         let uri = DocumentUri.of_path (fixture filename) in
         let* () = Test.openDocument ~client ~uri ~source in
         print_endline filename;
         let* () = print_hover client uri (position source "a =") in
         let* () = print_hover client uri (position source "source_locations") in
         let* () = print_hover client uri (position source "t =") in
         let* commands = actions client uri (position source "second]") in
         let+ (_ : Yojson.Safe.t) = execute client (List.hd commands) in
         ()));
  [%expect
    {|
    rich.ml
    type a = { first : string }

    (* ppx deriving signature *)
    val source_a : string list -> string list
    val second_a : string list -> string list
    val source_b : string list -> string list
    val second_b : string list -> string list
    (* ppx deriving signature *)
    val source_a : string list -> string list
    val second_a : string list -> string list
    val source_b : string list -> string list
    val second_b : string list -> string list
    type t = { name : string }

    (* ppx deriving signature *)
    val source_t : string list -> string list
    val second_t : string list -> string list
    module Generated : sig val convert : string list -> string list end
    opened .ml
    (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    let source_a input =
      let local_helper local_helper = local_helper in
      local_helper (input : string list)
    let _ = source_a
    let second_a input =
      let local_helper local_helper = local_helper in
      local_helper (input : string list)
    let _ = second_a
    let source_b input =
      let local_helper local_helper = local_helper in
      local_helper (input : string list)
    let _ = source_b
    let second_b input =
      let local_helper local_helper = local_helper in
      local_helper (input : string list)
    let _ = second_b

    rich.mli
    type a = { first : string }

    (* ppx deriving signature *)
    val source_a : string list -> string list
    val second_a : string list -> string list
    val source_b : string list -> string list
    val second_b : string list -> string list
    (* ppx deriving signature *)
    val source_a : string list -> string list
    val second_a : string list -> string list
    val source_b : string list -> string list
    val second_b : string list -> string list
    type t = { name : string }

    (* ppx deriving signature *)
    val source_t : string list -> string list
    val second_t : string list -> string list
    module Generated : sig val convert : string list -> string list end
    opened .mli
    (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    val source_a : string list -> string list
    val second_a : string list -> string list
    val source_b : string list -> string list
    val second_b : string list -> string list |}]
;;

let%expect_test "failed showDocument removes the temporary snapshot" =
  let source = Io.String_path.read_file (fixture "fixture.ml") in
  let filename = ref None in
  let on_show params =
    let path = Uri.to_path params.ShowDocumentParams.uri in
    assert (Sys.file_exists path);
    filename := Some path;
    false
  in
  run
    (fun () -> [%expect.output])
    ~on_show
    (fun client ->
       let* () = Test.openDocument ~client ~uri ~source in
       let* commands = actions client uri (position source "deriving") in
       let+ () = print_error (fun () -> execute client (List.hd commands)) in
       assert (not (Sys.file_exists (Option.value_exn !filename)));
       print_endline "temporary file removed");
  [%expect
    {|
    {"code":-32603,"message":"The client could not open the generated code"}
    temporary file removed |}]
;;

let%expect_test "missing typed signature falls back to the full expansion" =
  let source = "type broken = { field : missing } [@@deriving source_locations]\n" in
  run
    (fun () -> [%expect.output])
    (fun client ->
       let* () = Test.openDocument ~client ~uri ~source in
       print_hover client uri (position source "deriving"));
  [%expect
    {|
    (* ppx deriving expansion *)
    let source_broken input =
      let local_helper local_helper = local_helper in
      local_helper (input : missing list)
    let _ = source_broken |}]
;;

let%expect_test "no generated code means no expansion action" =
  let source = "type empty = int [@@deriving source_locations]\n" in
  run
    (fun () -> [%expect.output])
    (fun client ->
       let* () = Test.openDocument ~client ~uri ~source in
       let+ commands = actions client uri (position source "deriving") in
       Printf.printf "actions: %d\n" (List.length commands));
  [%expect {| actions: 0 |}]
;;
