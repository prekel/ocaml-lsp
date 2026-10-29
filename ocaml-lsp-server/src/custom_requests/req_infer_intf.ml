open Import
open Fiber.O

let capability = "handleInferIntf", `Bool true
let meth = "ocamllsp/inferIntf"

let rec has_config dir =
  Sys.file_exists (Filename.concat dir ".ocamlformat")
  ||
  let parent = Filename.dirname dir in
  (not (String.equal dir parent)) && has_config parent
;;

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    match params with
    | Some (`List [ json_uri ]) ->
      let json_uri = DocumentUri.t_of_yojson json_uri in
      (match Document_store.get_opt state.store json_uri with
       | None ->
         Jsonrpc.Response.Error.raise
           (Jsonrpc.Response.Error.make
              ~code:InvalidParams
              ~message:
                "ocamllsp/inferIntf received a URI for an unloaded file. Load the file \
                 first."
              ())
       | Some impl ->
         (match Document.kind impl with
          | `Merlin merlin when Document.Merlin.kind merlin = Document.Kind.Impl ->
            let* intf = Inference.infer_intf_for_impl impl in
            let path = Filename.chop_extension (Uri.to_path json_uri) ^ ".mli" in
            let+ formatted =
              if has_config (Filename.dirname path)
              then Ocamlformat_rpc.format_text state.ocamlformat_rpc ~path intf
              else Fiber.return (Error (`Msg "No project formatting configuration"))
            in
            Json.t_of_yojson
              (`String
                  (match formatted with
                   | Ok x -> x
                   | Error _ -> intf))
          | `Merlin _ | `Other ->
            Util.raise_invalid_params
              ~message:"ocamllsp/inferIntf expects an implementation document"
              ()))
    | Some json ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InvalidRequest
           ~message:"The input parameter for ocamllsp/inferIntf is invalid"
           ~data:(`Assoc [ "param", (json :> Json.t) ])
           ())
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InvalidRequest
           ~message:"ocamllsp/inferIntf must receive param: DocumentUri.t"
           ()))
;;
