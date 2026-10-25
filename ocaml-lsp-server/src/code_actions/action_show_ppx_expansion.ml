open Import
open Fiber.O

let kind = CodeActionKind.Other "ocamllsp.showPpxExpansion"
let command_name = "ocamllsp/show-ppx-expansion"
let title = "Show generated code"

let logical doc position =
  let offset = Text_document.absolute_position (Document.text_document doc) position in
  Msource.get_logical (Document.source doc) (`Offset offset)
;;

let available (capabilities : ClientCapabilities.t) =
  let open Option.O in
  let capabilities =
    let* window = capabilities.window in
    window.showDocument
  in
  Action_open_related.available capabilities
;;

let t state =
  Code_action.batchable kind (fun pipeline doc params ->
    let open Option.O in
    let* () = Option.some_if (available (State.client_capabilities state)) () in
    let position = params.CodeActionParams.range.start in
    let* deriving = Ppx_deriving.at_position pipeline (logical doc position) in
    let* (_ : string) = Ppx_deriving.expansion pipeline deriving in
    let arguments =
      [ DocumentUri.yojson_of_t (Document.uri doc)
      ; `Int (Document.version doc)
      ; Position.yojson_of_t position
      ]
    in
    let command = Command.create ~title ~command:command_name ~arguments () in
    Some (CodeAction.create ~title ~kind ~command ()))
;;

let error code message =
  Jsonrpc.Response.Error.raise (Jsonrpc.Response.Error.make ~code ~message ())
;;

let command_run server (params : ExecuteCommandParams.t) =
  let state = Server.state server in
  if not (available (State.client_capabilities state))
  then error InvalidParams "The client does not support opening generated code";
  let uri, version, position =
    let invalid () =
      error InvalidParams "Expected a URI, document version and position"
    in
    match params.arguments with
    | Some [ uri; `Int version; position ] ->
      (try DocumentUri.t_of_yojson uri, version, Position.t_of_yojson position with
       | _ -> invalid ())
    | _ -> invalid ()
  in
  let changed () =
    error ContentModified "The document changed; request the code action again"
  in
  let doc =
    match Document_store.get_opt state.store uri with
    | Some doc when Document.version doc = version -> doc
    | _ -> changed ()
  in
  let* expansion =
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin merlin ->
      Document.Merlin.with_pipeline_exn ~name:"show-ppx-expansion" merlin (fun pipeline ->
        let open Option.O in
        let* deriving = Ppx_deriving.at_position pipeline (logical doc position) in
        Ppx_deriving.expansion pipeline deriving)
  in
  (match Document_store.get_opt state.store uri with
   | Some current
     when Document.version current = version
          && String.equal (Document.text current) (Document.text doc) -> ()
   | _ -> changed ());
  let expansion =
    match expansion with
    | Some expansion -> expansion
    | None -> error InvalidParams "No deriving expansion at this position"
  in
  let extension =
    match Document.kind doc with
    | `Merlin merlin when Document.Merlin.kind merlin = Intf -> ".mli"
    | _ -> ".ml"
  in
  let filename, channel = Filename.open_temp_file "ocamllsp-ppx-expansion-" extension in
  let opened = ref false in
  Fiber.finalize
    ~finally:(fun () ->
      close_out_noerr channel;
      if not !opened
      then (
        try Sys.remove filename with
        | Sys_error _ -> ());
      Fiber.return ())
    (fun () ->
       output_string
         channel
         "(* Generated PPX expansion: a snapshot of the current editor buffer. *)\n\n";
       output_string channel expansion;
       output_char channel '\n';
       close_out channel;
       let uri = Uri.of_path filename in
       let request = ShowDocumentParams.create ~uri ~takeFocus:true () in
       let+ { ShowDocumentResult.success } =
         Server.request server (Server_request.ShowDocumentRequest request)
       in
       if not success
       then error InternalError "The client could not open the generated code";
       opened := true;
       `Null)
;;
