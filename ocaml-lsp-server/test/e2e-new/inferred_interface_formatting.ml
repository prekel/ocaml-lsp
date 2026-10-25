open Test.Import
module H = Ppx_deriving_actions

let request client method_ uri =
  Client.request
    client
    (UnknownRequest
       { meth = method_; params = Some (`List [ DocumentUri.yojson_of_t uri ]) })
;;

let with_project config f =
  let dir = Filename.temp_file "ocamllsp-interface-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let path = Filename.concat dir ".ocamlformat" in
  let channel = open_out path in
  output_string channel config;
  close_out channel;
  Fun.protect
    ~finally:(fun () ->
      Sys.remove path;
      Unix.rmdir dir)
    (fun () -> f dir)
;;

let%expect_test "switch inference uses the target interface project formatting" =
  with_project "profile=janestreet\nmargin=40\n" (fun dir ->
    let path = Filename.concat dir "example.ml" in
    let uri = DocumentUri.of_path path in
    let source =
      "type t = { first : int; second : string; third : bool }\n\
       let consume ~first ~second ~third = first, second, third\n"
    in
    H.run
      (fun () -> [%expect.output])
      (fun client ->
         let* () = Test.openDocument ~client ~uri ~source in
         let* counterpart = request client "ocamllsp/switchImplIntf" uri in
         assert (
           counterpart
           = `List
               [ DocumentUri.yojson_of_t
                   (DocumentUri.of_path (Filename.concat dir "example.mli"))
               ]);
         let* inferred = request client "ocamllsp/inferIntf" uri in
         (match inferred with
          | `String text -> print_endline text
          | _ -> assert false);
         assert (not (Sys.file_exists (Filename.concat dir "example.mli")));
         let code_actions () =
           let start = Position.create ~line:0 ~character:5 in
           Client.request
             client
             (CodeAction
                (CodeActionParams.create
                   ~textDocument:(TextDocumentIdentifier.create ~uri)
                   ~range:(Range.create ~start ~end_:start)
                   ~context:
                     (CodeActionContext.create
                        ~diagnostics:[]
                        ~only:[ CodeActionKind.Other "switch" ]
                        ())
                   ()))
         in
         let* missing = code_actions () in
         assert (missing = None || missing = Some []);
         let interface = Filename.concat dir "example.mli" in
         let channel = open_out interface in
         output_string channel "val existing : int\n";
         close_out channel;
         let+ existing = code_actions () in
         Sys.remove interface;
         match existing with
         | Some [ `CodeAction { CodeAction.title = "Open example.mli"; edit = None; _ } ]
           -> print_endline "Existing interfaces can still be opened"
         | _ -> assert false));
  [%expect
    {|
    type t =
      { first : int
      ; second : string
      ; third : bool
      }

    val consume
      :  first:'a
      -> second:'b
      -> third:'c
      -> 'a * 'b * 'c

    Existing interfaces can still be opened |}]
;;

let%expect_test "formatting errors preserve the inferred interface" =
  with_project "margin=not-an-integer\n" (fun dir ->
    let uri = DocumentUri.of_path (Filename.concat dir "example.ml") in
    H.run
      (fun () -> [%expect.output])
      (fun client ->
         let* () = Test.openDocument ~client ~uri ~source:"let value = 42\n" in
         let+ inferred = request client "ocamllsp/inferIntf" uri in
         match inferred with
         | `String text -> print_endline text
         | _ -> assert false));
  [%expect {| val value : int |}]
;;

let%expect_test "PPX-generated anonymous bindings do not break inferred interfaces" =
  let path = H.fixture "fixture.ml" in
  let uri = DocumentUri.of_path path in
  H.run
    (fun () -> [%expect.output])
    (fun client ->
       let* () = Test.openDocument ~client ~uri ~source:(Io.String_path.read_file path) in
       let+ inferred = request client "ocamllsp/inferIntf" uri in
       match inferred with
       | `String text ->
         print_endline text;
         let path = Filename.temp_file "ocamllsp_inferred_" ".mli" in
         Fun.protect
           ~finally:(fun () -> Sys.remove path)
           (fun () ->
              let channel = open_out path in
              output_string channel text;
              close_out channel;
              assert (
                Sys.command ("ocamlc -i " ^ Filename.quote path ^ " > /dev/null") = 0));
         print_endline "The inferred interface compiles"
       | _ -> assert false);
  [%expect
    {|
    type address = { city : string }

    val source_address : string list -> string list
    val read : string list -> string list

    module Nested : sig
      type t = { name : string }

      val source_t : string list -> string list

      module Generated : sig
        val convert : string list -> string list
      end

      val identity : 'a -> 'a
    end

    The inferred interface compiles |}]
;;
