open Import
open Fiber.O

let roots state source =
  let folders = Workspaces.workspace_folders (State.workspaces state) in
  let roots =
    List.map folders ~f:(fun folder ->
      Reference_index.canonical (Uri.to_path folder.WorkspaceFolder.uri))
  in
  if List.exists roots ~f:(fun root -> Reference_index.under root source)
  then roots
  else if roots <> []
  then if Option.is_some (Installed_library.root source) then roots else []
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

let with_document (state : State.t) uri f =
  match Document_store.get_opt state.store uri with
  | Some doc -> f doc
  | None ->
    let* doc =
      Document.make_from_file
        (State.wheel state)
        state.merlin_config
        state.merlin
        uri
        ~position_encoding:(State.position_encoding state)
    in
    (match doc with
     | None -> Fiber.return None
     | Some doc ->
       Fiber.finalize (fun () -> f doc) ~finally:(fun () -> Document.close doc))
;;

let run rpc (state : State.t) ~doc ?position ?(include_importers = true) f =
  let source = Reference_index.canonical (Uri.to_path (Document.uri doc)) in
  let roots_before = roots state source in
  let* cancel = Server.cancel_token () in
  let execute () =
    match Document.kind doc with
    | `Other -> assert false
    | `Merlin merlin ->
      let* config = Document.Merlin.mconfig merlin in
      let snapshots = documents state roots_before in
      let current_was_open =
        Option.is_some (Document_store.get_opt state.store (Document.uri doc))
      in
      let watched =
        if
          List.exists snapshots ~f:(fun (snapshot, _) ->
            Uri.equal (Document.uri snapshot) (Document.uri doc))
        then snapshots
        else (doc, merlin) :: snapshots
      in
      let* overlays =
        Fiber.sequential_map watched ~f:(fun (doc, merlin) ->
          Document.Merlin.with_pipeline_exn
            ~name:"project snapshot"
            merlin
            (fun pipeline ->
               Reference_index.live
                 ?cancel
                 ~cache:state.reference_index
                 ~roots:roots_before
                 pipeline
                 doc))
      in
      let units =
        Option.map position ~f:(fun position ->
          let offset =
            Text_document.absolute_position (Document.text_document doc) position
          in
          Option.value
            (List.find_map overlays ~f:(fun file ->
               Reference_index.target_units file ~source offset))
            ~default:[])
      in
      let* result, failures, stale =
        Document.Single_pipeline.run state.merlin (fun () ->
          let module Mocaml = Merlin_kernel.Mocaml in
          Mocaml.with_state (Mocaml.new_state ()) (fun () ->
            let files, failures =
              if roots_before = []
              then [], 0
              else
                Reference_index.refresh
                  ?units
                  ~include_importers
                  state.reference_index
                  ~roots:roots_before
                  ~paths:(Mconfig.build_path config @ Mconfig.cmt_path config)
                  ~cancel
            in
            let support =
              Reference_index.supporting_files
                state.reference_index
                ~files:(overlays @ files)
                ~units:
                  (match units with
                   | Some units -> units
                   | None -> Reference_index.called_units (overlays @ files))
                ~cancel
            in
            let result, stale =
              f ~roots:roots_before ~files:(support @ files) ~overlays ~cancel
            in
            result, failures, stale))
      in
      let changed () =
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make
             ~code:ContentModified
             ~message:"Project inputs changed during navigation"
             ())
      in
      if current_was_open
      then (
        match Document_store.get_opt state.store (Document.uri doc) with
        | Some current when Document.version current = Document.version doc -> ()
        | _ -> changed ());
      List.iter watched ~f:(fun (snapshot, _) ->
        match Document_store.get_opt state.store (Document.uri snapshot) with
        | Some current when Document.version current = Document.version snapshot -> ()
        | None
          when Uri.equal (Document.uri snapshot) (Document.uri doc)
               && not current_was_open ->
          (try
             if
               not
                 (String.equal
                    (Io.String_path.read_file (Uri.to_path (Document.uri doc)))
                    (Document.text doc))
             then changed ()
           with
           | Sys_error _ | Unix.Unix_error _ -> changed ())
        | _ -> changed ());
      if
        versions snapshots <> versions (documents state roots_before)
        || roots_before <> roots (Server.state rpc) source
      then changed ();
      let message =
        if failures + stale = 0
        then ""
        else
          sprintf
            "Project navigation results may be incomplete: %d unavailable or outdated \
             build artifacts. Run dune build to refresh the index."
            (failures + stale)
      in
      let* () =
        if
          Reference_index.notice
            state.reference_index
            (String.concat ~sep:"\000" (roots_before @ [ message ]))
          && not (String.is_empty message)
        then
          Server.notification
            rpc
            (Server_notification.LogMessage
               (LogMessageParams.create ~type_:Warning ~message))
        else Fiber.return ()
      in
      Fiber.return result
  in
  let* result = Fiber.collect_errors execute in
  match result with
  | Ok result -> Fiber.return result
  | Error errors ->
    if
      List.exists errors ~f:(fun (e : Exn_with_backtrace.t) ->
        match e.exn with
        | Reference_index.Cancelled -> true
        | _ -> false)
    then
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:RequestCancelled
           ~message:"Project navigation cancelled"
           ())
    else Exn_with_backtrace.reraise (List.hd_exn errors)
;;
