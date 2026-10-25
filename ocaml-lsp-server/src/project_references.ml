open Import
open Fiber.O

let fallback merlin doc position include_declaration =
  let text = Document.text doc in
  let offset = Text_document.absolute_position (Document.text_document doc) position in
  let logical = Msource.get_logical (Document.source doc) (`Offset offset) in
  let+ locations =
    Document.Merlin.with_pipeline_exn ~name:"references" merlin (fun pipeline ->
      Navigation.references pipeline logical ~include_declaration)
  in
  let encoding = Text_document.position_encoding (Document.text_document doc) in
  List.filter_map locations ~f:(fun loc ->
    let first = loc.Loc.loc_start.pos_cnum
    and last = loc.loc_end.pos_cnum in
    if first < 0 || last < first || last > String.length text
    then None
    else
      Some
        (Location.create
           ~uri:(Document.uri doc)
           ~range:
             (Range.create
                ~start:(Reference_index.position text first ~encoding)
                ~end_:(Reference_index.position text last ~encoding))))
;;

let workspace_roots state source =
  let folders = Workspaces.workspace_folders (State.workspaces state) in
  let roots =
    List.map folders ~f:(fun folder ->
      Reference_index.canonical (Uri.to_path folder.WorkspaceFolder.uri))
  in
  if List.exists roots ~f:(fun root -> Reference_index.under root source)
  then roots
  else if roots <> []
  then []
  else (
    let rec find directory =
      if Sys.file_exists (Filename.concat directory "dune-project")
      then [ directory ]
      else (
        let parent = Filename.dirname directory in
        if String.equal parent directory then [] else find parent)
    in
    find (Filename.dirname source))
;;

let compare_location (a : Location.t) (b : Location.t) =
  match Poly.compare (Uri.to_string a.uri) (Uri.to_string b.uri) with
  | Ordering.Eq -> Range.compare a.range b.range
  | order -> order
;;

let documents state roots =
  Document_store.fold state.State.store ~init:[] ~f:(fun doc acc ->
    let path = Reference_index.canonical (Uri.to_path (Document.uri doc)) in
    match Document.kind doc with
    | `Merlin merlin
      when List.exists roots ~f:(fun root -> Reference_index.under root path) ->
      (doc, merlin) :: acc
    | _ -> acc)
;;

let versions documents =
  List.map documents ~f:(fun (doc, _) -> Document.uri doc, Document.version doc)
  |> List.sort ~compare:Poly.compare
;;

let run
      rpc
      ~doc
      (state : State.t)
      { ReferenceParams.textDocument = { uri }; position; context; _ }
  =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let source = Reference_index.canonical (Uri.to_path uri) in
    let roots = workspace_roots state source in
    if roots = []
    then
      let+ locations = fallback merlin doc position context.includeDeclaration in
      Some locations
    else
      let* cancel = Server.cancel_token () in
      let* config = Document.Merlin.mconfig merlin in
      let execute () =
        let snapshots = documents state roots in
        let* overlays =
          Fiber.sequential_map snapshots ~f:(fun (doc, merlin) ->
            Document.Merlin.with_pipeline_exn
              ~name:"reference overlay"
              merlin
              (fun pipeline ->
                 Reference_index.live
                   ?cancel
                   ~cache:state.reference_index
                   ~roots
                   pipeline
                   doc))
        in
        let offset =
          Text_document.absolute_position (Document.text_document doc) position
        in
        let* files, failures =
          Document.Single_pipeline.run state.merlin (fun () ->
            let module Mocaml = Merlin_kernel.Mocaml in
            Mocaml.with_state (Mocaml.new_state ()) (fun () ->
              Reference_index.refresh
                ?units:
                  (List.find_map overlays ~f:(fun file ->
                     Reference_index.target_units file ~source offset))
                state.reference_index
                ~roots
                ~paths:(Mconfig.build_path config @ Mconfig.cmt_path config)
                ~cancel))
        in
        let* found, stale =
          Document.Single_pipeline.run state.merlin (fun () ->
            Reference_index.lookup
              ~cache:state.reference_index
              ~files
              ~overlays
              ~source
              ~offset
              ~include_declaration:context.includeDeclaration
              ~cancel)
        in
        List.iter ((doc, merlin) :: snapshots) ~f:(fun (snapshot, _) ->
          match Document_store.get_opt state.store (Document.uri snapshot) with
          | Some current when Document.version current = Document.version snapshot -> ()
          | _ ->
            Jsonrpc.Response.Error.raise
              (Jsonrpc.Response.Error.make
                 ~code:ContentModified
                 ~message:"A document changed while finding references"
                 ()));
        if versions snapshots <> versions (documents state roots)
        then
          Jsonrpc.Response.Error.raise
            (Jsonrpc.Response.Error.make
               ~code:ContentModified
               ~message:"Open documents changed while finding references"
               ());
        if roots <> workspace_roots (Server.state rpc) source
        then
          Jsonrpc.Response.Error.raise
            (Jsonrpc.Response.Error.make
               ~code:ContentModified
               ~message:"Workspace folders changed while finding references"
               ());
        let* locations =
          match found with
          | Some locations ->
            Fiber.return
              (List.map
                 locations
                 ~f:(Reference_index.location ~encoding:(State.position_encoding state)))
          | None -> fallback merlin doc position context.includeDeclaration
        in
        let message =
          if failures + stale = 0
          then ""
          else
            sprintf
              "Reference results may be incomplete: %d unavailable or outdated build \
               artifacts. Run dune build to refresh project references."
              (failures + stale)
        in
        let* () =
          if
            Reference_index.notice
              state.reference_index
              (String.concat ~sep:"\000" (roots @ [ message ]))
            && not (String.is_empty message)
          then
            Server.notification
              rpc
              (Server_notification.LogMessage
                 (LogMessageParams.create ~type_:Warning ~message))
          else Fiber.return ()
        in
        Fiber.return (Some (List.sort_uniq locations ~compare:compare_location))
      in
      let* result = Fiber.collect_errors execute in
      (match result with
       | Ok result -> Fiber.return result
       | Error errors ->
         if
           List.exists errors ~f:(fun (error : Exn_with_backtrace.t) ->
             match error.exn with
             | Reference_index.Cancelled -> true
             | _ -> false)
         then
           Jsonrpc.Response.Error.raise
             (Jsonrpc.Response.Error.make
                ~code:RequestCancelled
                ~message:"Reference search cancelled"
                ())
         else Exn_with_backtrace.reraise (List.hd_exn errors))
;;
