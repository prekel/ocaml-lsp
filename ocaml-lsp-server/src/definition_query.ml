open Import
open Fiber.O

let logical doc position =
  let offset = Text_document.absolute_position (Document.text_document doc) position in
  Msource.get_logical (Document.source doc) (`Offset offset)
;;

let check_version state doc =
  match Document_store.get_opt state.State.store (Document.uri doc) with
  | Some current
    when Document.version current = Document.version doc
         && String.equal (Document.text current) (Document.text doc) -> ()
  | _ ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:ContentModified
         ~message:"A document changed during navigation"
         ())
;;

(* Explicit pattern annotations can leave a [Tpoly] wrapper on pat_type.
   Merlin 4.19 only recognizes a bare [Tconstr] in Locate_type. *)
let locate_type pipeline position =
  let position = Mpipeline.get_lexing_pos pipeline position in
  let browse =
    Mbrowse.of_typedtree (Mtyper.get_typedtree (Mpipeline.typer_result pipeline))
  in
  let target =
    match Mbrowse.enclosing position [ browse ] with
    | (env, node) :: _ ->
      let typ =
        match node with
        | Browse_raw.Expression e -> Some e.exp_type
        | Pattern p -> Some p.pat_type
        | Core_type t -> Some t.ctyp_type
        | Value_description d -> Some d.val_desc.ctyp_type
        | _ -> None
      in
      let rec path typ =
        match Types.get_desc typ with
        | Tpoly (typ, _) -> path typ
        | Tconstr (path, _, _) -> Some (env, path)
        | _ -> None
      in
      Option.bind typ ~f:path
    | [] -> None
  in
  match target with
  | None -> `Invalid_context
  | Some (env, path) ->
    (match
       Merlin_analysis.Locate.from_path
         ~config:(Mpipeline.final_config pipeline)
         ~env
         ~namespace:`Type
         `MLI
         path
     with
     | `Found (_, file, position) -> `Found (file, position)
     | `Builtin -> `Builtin (Path.name path)
     | (`File_not_found _ | `Not_in_env _ | `Not_found _) as result -> result)
;;

let source_uri config path =
  let path = Reference_index.canonical path in
  (* Dune may return a source copy under its build context. Preserve the
     project-relative path; searching by basename could select another unit. *)
  let rec original directory relative =
    let parent = Filename.dirname directory in
    if String.equal parent directory
    then None
    else if Sys.file_exists (Filename.concat parent ".db")
    then (
      let root =
        if String.equal (Filename.basename parent) "_build"
        then Some (Filename.dirname parent)
        else config.Mconfig.merlin.source_root
      in
      Option.bind root ~f:(fun root ->
        let source = Filename.concat root relative in
        try if (Unix.stat source).st_kind = S_REG then Some source else None with
        | Unix.Unix_error _ | Sys_error _ -> None))
    else original parent (Filename.concat (Filename.basename directory) relative)
  in
  Uri.of_path
    (Reference_index.canonical
       (Option.value
          (original (Filename.dirname path) (Filename.basename path))
          ~default:path))
;;

let fallback kind state doc position =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let pos = logical doc position in
    let command, name =
      match kind with
      | `Definition -> Query_protocol.Locate (None, `MLI, pos), "definition"
      | `Declaration -> Query_protocol.Locate (None, `MLI, pos), "declaration"
      | `Type_definition -> Query_protocol.Locate_type pos, "type definition"
    in
    let* result, config =
      Document.Merlin.with_pipeline_exn ~name merlin (fun pipeline ->
        let result =
          match kind with
          | `Type_definition -> locate_type pipeline pos
          | _ -> Query_commands.dispatch pipeline command
        in
        result, Mpipeline.final_config pipeline)
    in
    (match result with
     | `At_origin when kind = `Definition && Document.Merlin.kind merlin = Intf ->
       (* Locate intentionally stops on an interface declaration. The counterpart
         resolver follows that declaration's exported path into its body. *)
       Implementation_query.run state (Document.uri doc) position
     | `Found (path, target_position) ->
       let uri =
         Option.fold path ~init:(Document.uri doc) ~f:(fun _ path ->
           source_uri config path)
       in
       let target_was_open =
         Option.is_some (Document_store.get_opt state.State.store uri)
       in
       Project_snapshot.with_document state uri (fun target ->
         match Document.kind target with
         | `Other -> Fiber.return None
         | `Merlin merlin ->
           let+ range =
             Document.Merlin.with_pipeline_exn
               ~name:"definition range"
               merlin
               (fun pipeline ->
                  (* Recreate offsets in the actual target source. Build annotations
               can use another byte offset after a preprocessor line directive. *)
                  let target_position =
                    Mpipeline.get_lexing_pos
                      pipeline
                      (`Logical
                          ( target_position.pos_lnum
                          , target_position.pos_cnum - target_position.pos_bol ))
                  in
                  let range =
                    Navigation.definition_range
                      pipeline
                      target_position
                      ~type_definition:(kind = `Type_definition)
                      ~source_position:
                        (if Uri.equal (Document.uri doc) uri then Some pos else None)
                  in
                  let convert position =
                    let (`Offset offset) =
                      Msource.get_offset
                        (Document.source target)
                        (Position.logical position)
                    in
                    Reference_index.position
                      (Document.text target)
                      offset
                      ~encoding:(State.position_encoding state)
                  in
                  match range with
                  | Some range ->
                    Range.create ~start:(convert range.start) ~end_:(convert range.end_)
                  | None ->
                    let start =
                      Reference_index.position
                        (Document.text target)
                        target_position.pos_cnum
                        ~encoding:(State.position_encoding state)
                    in
                    Range.create ~start ~end_:start)
           in
           if target_was_open then check_version state target;
           Some (`Location [ Location.create ~uri ~range ]))
     | `At_origin
     | `Builtin _
     | `Invalid_context
     | `Not_found _
     | `Not_in_env _
     | `File_not_found _ -> Fiber.return None)
;;

let run rpc ~doc kind (state : State.t) position =
  let source = Reference_index.canonical (Uri.to_path (Document.uri doc)) in
  let indexed ~navigation =
    match Document.kind doc, kind with
    | `Other, _ -> Fiber.return None
    | _, `Type_definition -> Fiber.return None
    | `Merlin _, (`Definition | `Declaration) ->
      if Project_snapshot.roots state source = []
      then Fiber.return None
      else
        Project_snapshot.run
          rpc
          state
          ~doc
          ~position
          ~include_importers:false
          ~navigation
          (fun ~roots:_ ~files ~overlays ~cancel ->
             let offset =
               Text_document.absolute_position (Document.text_document doc) position
             in
             let targets, stale =
               Reference_index.locate
                 ~files
                 ~overlays
                 ~source
                 ~offset
                 ~implementation:false
                 ~interface_to_implementation:(kind = `Definition)
                 ~cancel
             in
             ( Option.map targets ~f:(fun targets ->
                 `Location
                   (List.map
                      targets
                      ~f:
                        (Reference_index.location
                           ~encoding:(State.position_encoding state))))
             , stale ))
  in
  let* indexed =
    indexed ~navigation:true
    >>= function
    | None -> indexed ~navigation:false
    | result -> Fiber.return result
  in
  let* result =
    match indexed with
    | Some (`Location (_ :: _)) as result -> Fiber.return result
    | Some (`Location []) when kind = `Definition ->
      (match Document.kind doc with
       | `Merlin merlin when Document.Merlin.kind merlin = Intf ->
         Implementation_query.run state (Document.uri doc) position
       | _ -> Fiber.return indexed)
    | Some _ -> Fiber.return indexed
    | None -> fallback kind state doc position
  in
  check_version state doc;
  Fiber.return result
;;
