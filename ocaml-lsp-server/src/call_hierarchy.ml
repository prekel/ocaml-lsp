open Import
open Fiber.O
module Hierarchy = Reference_index.Call_hierarchy

let prepare rpc (state : State.t) (params : Lsp.Types.CallHierarchyPrepareParams.t) =
  let doc = Document_store.get state.store params.textDocument.uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin _ ->
    let+ items =
      Project_snapshot.run rpc state ~doc (fun ~roots ~files ~overlays ~cancel ->
        let graph, stale =
          Hierarchy.create ~cache:state.reference_index ~roots ~files ~overlays ~cancel
        in
        ( Hierarchy.prepare
            graph
            ~encoding:(State.position_encoding state)
            ~source:(Reference_index.canonical (Uri.to_path params.textDocument.uri))
            ~offset:
              (Text_document.absolute_position
                 (Document.text_document doc)
                 params.position)
        , stale ))
    in
    (match items with
     | [] -> None
     | _ -> Some items)
;;

let expand rpc (state : State.t) (item : Lsp.Types.CallHierarchyItem.t) f =
  Hierarchy.validate ~cache:state.reference_index item;
  let* result =
    Project_snapshot.with_document state item.uri (fun doc ->
      match Document.kind doc with
      | `Other -> Fiber.return None
      | `Merlin _ ->
        let+ result =
          Project_snapshot.run rpc state ~doc (fun ~roots ~files ~overlays ~cancel ->
            let graph, stale =
              Hierarchy.create
                ~cache:state.reference_index
                ~roots
                ~files
                ~overlays
                ~cancel
            in
            f graph ~encoding:(State.position_encoding state) item, stale)
        in
        Some result)
  in
  match result with
  | Some _ -> Fiber.return result
  | None ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:ContentModified
         ~message:"The call hierarchy source is unavailable; prepare it again"
         ())
;;

let incoming rpc state (params : Lsp.Types.CallHierarchyIncomingCallsParams.t) =
  expand rpc state params.item Hierarchy.incoming
;;

let outgoing rpc state (params : Lsp.Types.CallHierarchyOutgoingCallsParams.t) =
  expand rpc state params.item Hierarchy.outgoing
;;
