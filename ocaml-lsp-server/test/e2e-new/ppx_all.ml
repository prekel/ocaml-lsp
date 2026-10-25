open Test.Import
module H = Ppx_deriving_actions

let on_show params =
  let path = Uri.to_path params.ShowDocumentParams.uri in
  Printf.printf
    "expansion %s: %s\n"
    (Filename.extension path)
    (String.trim (Io.String_path.read_file path));
  Sys.remove path;
  true
;;

let%expect_test "all extension point categories expose signatures and full expansion" =
  H.run
    (fun () -> [%expect.output])
    ~on_show
    (fun client ->
       Fiber.sequential_iter
         [ ( "extensions.ml"
           , [ "source_locations.expr"
             ; "[@source_locations]"
             ; "source_locations.pat"
             ; "source_locations.typ"
             ; "source_locations.module_expr"
             ; "source_locations.mty"
             ; "source_locations.str"
             ; "source_locations.sugar"
             ; "[@@source_locations]"
             ; "source_locations.cls"
             ; "source_locations.cty"
             ; "source_locations.floating"
             ; "deriving"
             ] )
         ; ( "extensions_intf.mli"
           , [ "source_locations.signature_item"
             ; "source_locations.mod_intf"
             ; "source_locations.mty_intf"
             ; "source_locations.typ_intf"
             ; "[@@source_locations]"
             ; "source_locations.cty_intf"
             ; "source_locations.floating"
             ; "deriving"
             ] )
         ]
         ~f:(fun (filename, needles) ->
           let path = H.fixture filename in
           let source = Io.String_path.read_file path in
           let uri = DocumentUri.of_path path in
           let* () = Test.openDocument ~client ~uri ~source in
           Fiber.sequential_iter needles ~f:(fun needle ->
             Printf.printf "%s: %s\n" filename needle;
             let position = H.position source needle in
             let* () = H.print_hover client uri position in
             let* commands = H.actions client uri position in
             assert (List.length commands = 1);
             let+ (_ : Yojson.Safe.t) = H.execute client (List.hd commands) in
             ())));
  [%expect
    {|
    extensions.ml: source_locations.expr
    (* ppx source_locations.expr signature *)
    int
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    42
    extensions.ml: [@source_locations]
    (* ppx source_locations signature *)
    int
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    42
    extensions.ml: source_locations.pat
    (* ppx source_locations.pat signature *)
    int
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    captured
    extensions.ml: source_locations.typ
    (* ppx source_locations.typ signature *)
    int
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    int
    extensions.ml: source_locations.module_expr
    (* ppx source_locations.module_expr signature *)
    sig val generated : alias end
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    struct let generated = 42 end
    extensions.ml: source_locations.mty
    (* ppx source_locations.mty signature *)
    sig val generated : alias end
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    sig val generated : int end
    extensions.ml: source_locations.str
    (* ppx source_locations.str signature *)
    val generated : alias
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    let generated = 42
    extensions.ml: source_locations.sugar
    (* ppx source_locations.sugar signature *)
    val sugar : alias
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    let sugar = 3
    extensions.ml: [@@source_locations]
    (* ppx source_locations signature *)
    val source_record : string list -> string list
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    type record = {
      label: string }
    let source_record input =
      let local_helper local_helper = local_helper in
      local_helper (input : string list)
    let _ = source_record
    extensions.ml: source_locations.cls
    (* ppx source_locations.cls signature *)
    class c : object  end
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    class c = object  end
    extensions.ml: source_locations.cty
    (* ppx source_locations.cty signature *)
    class type ct = object  end
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    class type ct = object  end
    extensions.ml: source_locations.floating
    (* ppx source_locations.floating signature *)
    val floating : alias
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    let floating = 42
    extensions.ml: deriving
    (* ppx deriving signature *)
    type event += Extra
    val generated_extension : alias
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    type event +=
      | Extra
    let generated_extension = 42
    extensions_intf.mli: source_locations.signature_item
    (* ppx source_locations.signature_item signature *)
    val generated : int
    expansion .mli: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    val generated : int
    extensions_intf.mli: source_locations.mod_intf
    (* ppx source_locations.mod_intf signature *)
    sig val generated : int end
    expansion .mli: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    sig val generated : int end
    extensions_intf.mli: source_locations.mty_intf
    (* ppx source_locations.mty_intf signature *)
    sig val generated : int end
    expansion .mli: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    sig val generated : int end
    extensions_intf.mli: source_locations.typ_intf
    (* ppx source_locations.typ_intf signature *)
    int
    expansion .mli: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    int
    extensions_intf.mli: [@@source_locations]
    (* ppx source_locations signature *)
    val source_record : string list -> string list
    expansion .mli: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    type record = {
      label: string }
    val source_record : string list -> string list
    extensions_intf.mli: source_locations.cty_intf
    (* ppx source_locations.cty_intf signature *)
    class type ct = object  end
    expansion .mli: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    class type ct = object  end
    extensions_intf.mli: source_locations.floating
    (* ppx source_locations.floating signature *)
    val floating : alias
    expansion .mli: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    val floating : int
    extensions_intf.mli: deriving
    (* ppx deriving signature *)
    type event += Extra
    val generated_extension : alias
    expansion .mli: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    type event +=
      | Extra
    val generated_extension : int |}]
;;

let%expect_test
    "unchanged compiler attributes and unresolved extensions are not expansions"
  =
  let source = "let x = (1 [@ocaml.warning \"-26\"])\nlet y = [%not_installed]\n" in
  H.run
    (fun () -> [%expect.output])
    (fun client ->
       let* () = Test.openDocument ~client ~uri:H.uri ~source in
       Fiber.sequential_iter [ "ocaml.warning"; "not_installed" ] ~f:(fun needle ->
         let+ commands = H.actions client H.uri (H.position source needle) in
         Printf.printf "%s: %d actions\n" needle (List.length commands)));
  [%expect
    {|
    ocaml.warning: 0 actions
    not_installed: 0 actions |}]
;;

let%expect_test "plain PPX attributes append generated signatures to type hover" =
  let source = Io.String_path.read_file (H.fixture "extensions.ml") in
  H.run
    (fun () -> [%expect.output])
    (fun client ->
       let* () = Test.openDocument ~client ~uri:H.uri ~source in
       H.print_hover client H.uri (H.position source "record ="));
  [%expect
    {|
    type record = { label : string }

    (* ppx source_locations signature *)
    val source_record : string list -> string list |}]
;;

let%expect_test "compiler metadata stays separate from transformed attributes" =
  let source =
    "type record = { label : string } [@@source_locations] [@@ocaml.warning \"-32\"]\n"
  in
  H.run
    (fun () -> [%expect.output])
    (fun client ->
       let* () = Test.openDocument ~client ~uri:H.uri ~source in
       Fiber.sequential_iter [ "source_locations"; "ocaml.warning" ] ~f:(fun needle ->
         let+ commands = H.actions client H.uri (H.position source needle) in
         Printf.printf "%s: %d actions\n" needle (List.length commands)));
  [%expect
    {|
    source_locations: 1 actions
    ocaml.warning: 0 actions |}]
;;

let%expect_test "PPX actions and hovers use UTF16 cursor positions" =
  let source = "let x = (\"🙂\", [%source_locations.expr])\n" in
  H.run
    (fun () -> [%expect.output])
    ~on_show
    (fun client ->
       let* () = Test.openDocument ~client ~uri:H.uri ~source in
       let position = H.position source "source_locations.expr" in
       let position = { position with character = position.character - 2 } in
       let* () = H.print_hover client H.uri position in
       let* commands = H.actions client H.uri position in
       assert (List.length commands = 1);
       let+ (_ : Yojson.Safe.t) = H.execute client (List.hd commands) in
       ());
  [%expect
    {|
    (* ppx source_locations.expr signature *)
    int
    expansion .ml: (* Generated PPX expansion: a snapshot of the current editor buffer. *)

    42 |}]
;;
