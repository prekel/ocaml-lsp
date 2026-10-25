open Test.Import

let path = Filename.concat (Sys.getcwd ()) "ppx_source_fixture/fixture.ml"
let uri = DocumentUri.of_path path
let document = TextDocumentIdentifier.create ~uri

let print_hover client ~line ~character =
  let+ hover =
    Client.request
      client
      (TextDocumentHover
         (HoverParams.create
            ~textDocument:document
            ~position:(Position.create ~line ~character)
            ()))
  in
  match hover with
  | None -> print_endline "no hover"
  | Some hover ->
    Hover.yojson_of_t hover |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let%expect_test "generated locations do not replace source types or bindings" =
  let source = Io.String_path.read_file path in
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  let output =
    Test.run ~handler (fun client ->
      let start () =
        Client.start
          client
          (InitializeParams.create ~capabilities:(ClientCapabilities.create ()) ())
      in
      let run () =
        let* (_ : InitializeResult.t) = Client.initialized client in
        let settings =
          `Assoc
            [ "codelens", `Assoc [ "enable", `Bool true ]
            ; ( "inlayHints"
              , `Assoc
                  [ "hintLetBindings", `Bool true
                  ; "hintPatternVariables", `Bool true
                  ; "hintFunctionParams", `Bool true
                  ] )
            ]
        in
        let* () = Client.notification client (ChangeConfiguration { settings }) in
        let* () = Test.openDocument ~client ~uri ~source in
        let* () = Fiber.Ivar.read diagnostics in
        let* () = print_hover client ~line:1 ~character:7 in
        let* () = print_hover client ~line:1 ~character:25 in
        let* () = print_hover client ~line:6 ~character:7 in
        (* A use of generated code is still a normal hover target. *)
        let* () = print_hover client ~line:3 ~character:18 in
        (* The attribute itself must still show its expansion. *)
        let* () = print_hover client ~line:1 ~character:37 in
        let* hints =
          Client.request
            client
            (InlayHint
               (InlayHintParams.create
                  ~textDocument:document
                  ~range:
                    (Range.create
                       ~start:(Position.create ~line:0 ~character:0)
                       ~end_:(Position.create ~line:11 ~character:0))
                  ()))
        in
        List.iter (Option.value hints ~default:[]) ~f:(fun hint ->
          InlayHint.yojson_of_t hint |> Yojson.Safe.to_string |> print_endline);
        let* lenses =
          Client.request
            client
            (TextDocumentCodeLens (CodeLensParams.create ~textDocument:document ()))
        in
        List.iter lenses ~f:(fun lens ->
          CodeLens.yojson_of_t lens |> Yojson.Safe.to_string |> print_endline);
        let output = [%expect.output] in
        let* () = Client.request client Shutdown in
        let+ () = Client.stop client in
        output
      in
      Fiber.fork_and_join_unit start run)
  in
  let (_ : string) = [%expect.output] in
  print_endline output;
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "type address = { city : string }\n***\nThe user's record, not the generated converter."
      },
      "range": {
        "end": { "character": 62, "line": 1 },
        "start": { "character": 0, "line": 1 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "type string\n***\nstring" },
      "range": {
        "end": { "character": 30, "line": 1 },
        "start": { "character": 24, "line": 1 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "type t = { name : string }" },
      "range": {
        "end": { "character": 58, "line": 6 },
        "start": { "character": 2, "line": 6 }
      }
    }
    {
      "contents": {
        "kind": "plaintext",
        "value": "string list -> string list\n***\nThe user's record, not the generated converter."
      },
      "range": {
        "end": { "character": 28, "line": 3 },
        "start": { "character": 14, "line": 3 }
      }
    }
    {
      "contents": {
        "value": "(* ppx deriving expansion *)\nlet source_address input = (input : string list)",
        "language": "ocaml"
      },
      "range": {
        "end": { "character": 62, "line": 1 },
        "start": { "character": 33, "line": 1 }
      }
    }
    {"kind":1,"label":": 'a","paddingLeft":false,"paddingRight":false,"position":{"character":16,"line":8}}
    {"kind":1,"label":": string list","paddingLeft":false,"paddingRight":false,"position":{"character":11,"line":3}}
    {"command":{"command":"","title":"'a -> 'a"},"range":{"end":{"character":20,"line":8},"start":{"character":2,"line":8}}}
    {"command":{"command":"","title":"string list -> string list"},"range":{"end":{"character":31,"line":3},"start":{"character":0,"line":3}}} |}]
;;
