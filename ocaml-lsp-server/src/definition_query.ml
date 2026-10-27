open Import
open Fiber.O

let location_of_merlin_loc uri : _ -> (_, string) result = function
  | `At_origin -> Error "Already at definition point"
  | `Builtin s ->
    Error (sprintf "%S is a builtin, it is not possible to jump to its definition" s)
  | `File_not_found s -> Error (sprintf "File_not_found: %s" s)
  | `Invalid_context -> Error "Not a valid identifier"
  | `Not_found (ident, where) ->
    let msg =
      let msg = sprintf "%S not found." ident in
      Option.value_map where ~default:msg ~f:(sprintf "%s last looked in %s" msg)
    in
    Error msg
  | `Not_in_env m -> Error (sprintf "Not in environment: %s" m)
  | `Found (path, lex_position) ->
    Ok
      (Position.of_lexical_position lex_position
       |> Option.map ~f:(fun position ->
         let range = { Range.start = position; end_ = position } in
         let uri = Option.value_map path ~default:uri ~f:Uri.of_path in
         let locs = [ { Location.uri; range } ] in
         `Location locs))
;;

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
    let locate config =
      Merlin_analysis.Locate.from_path ~config ~env ~namespace:`Type `MLI path
    in
    let config = Mpipeline.final_config pipeline in
    let result =
      match locate config with
      | `File_not_found _ -> locate (Navigation_config.with_generated_sources config)
      | result -> result
    in
    (match result with
     | `Found (_, file, position) -> `Found (file, position)
     | `Builtin -> `Builtin (Path.name path)
     | (`File_not_found _ | `Not_in_env _ | `Not_found _) as result -> result)
;;

let source_uri ?(missing = false) config path =
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
        let exists path =
          try (Unix.stat path).st_kind = S_REG with
          | Unix.Unix_error _ | Sys_error _ -> false
        in
        if missing || exists source
        then Some source
        else (
          let text file =
            try
              if (Unix.stat file).st_size <= 1_000_000
              then Some (Io.String_path.read_file file)
              else None
            with
            | Unix.Unix_error _ | Sys_error _ -> None
          in
          Option.bind (Dune_rule.promoted_source ~root ~source ~text) ~f:(fun source ->
            Option.some_if (exists source) source))))
    else original parent (Filename.concat (Filename.basename directory) relative)
  in
  Uri.of_path
    (Reference_index.canonical
       (Option.value
          (original (Filename.dirname path) (Filename.basename path))
          ~default:path))
;;

let type_definition state doc position =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let pos = logical doc position in
    let* result, config =
      Document.Merlin.with_pipeline_exn ~name:"type definition" merlin (fun pipeline ->
        locate_type pipeline pos, Mpipeline.final_config pipeline)
    in
    let* result =
      match result with
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
                       ~type_definition:true
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
      | `File_not_found _ -> Fiber.return None
    in
    check_version state doc;
    Fiber.return result
;;

(* Preserve Locate's declaration UID so that the one selected annotation can
   validate its source digest. No project artifact discovery is needed. *)
let native_locate ?config pipeline ~prefix preference position =
  let config =
    match config with
    | Some config -> config
    | None -> Mpipeline.final_config pipeline
  in
  let typer = Mpipeline.typer_result pipeline in
  let local_defs = Mtyper.get_typedtree typer in
  let pos = Mpipeline.get_lexing_pos pipeline position in
  let env, _ = Mbrowse.leaf_node (Mtyper.node_at typer pos) in
  let path =
    match prefix with
    | Some path -> path
    | None ->
      Merlin_analysis.Misc_utils.reconstruct_identifier pipeline pos None
      |> Merlin_kernel.Mreader_lexer.identifier_suffix
      |> List.map ~f:(fun name -> name.Loc.txt)
      |> String.concat ~sep:"."
  in
  if String.is_empty path
  then `Invalid_context, None
  else (
    match
      Merlin_analysis.Locate.from_string ~config ~env ~local_defs ~pos preference path
    with
    | `Found (uid, file, position) -> `Found (file, position), uid
    | `Missing_labels_namespace -> assert false
    | (`Not_found _ | `At_origin | `Not_in_env _ | `File_not_found _ | `Builtin _) as
      result -> result, None)
;;

let source_provenance config uid result =
  let unit_name =
    Option.bind uid ~f:(function
      | Ocaml_typing.Shape.Uid.Item { comp_unit; _ } | Compilation_unit comp_unit ->
        Some comp_unit
      | Internal | Predef _ -> None)
  in
  match unit_name, result with
  | Some unit_name, `Found (Some source, _) ->
    let extension = if String.is_suffix source ~suffix:".mli" then ".cmti" else ".cmt" in
    List.find_map (Mconfig.cmt_path config) ~f:(fun directory ->
      List.find_map
        [ unit_name; Stdlib.String.uncapitalize_ascii unit_name ]
        ~f:(fun name ->
          let artifact = Filename.concat directory (name ^ extension) in
          try
            let info = (Ocaml_typing.Cmt_cache.read artifact).cmt_infos in
            let same_source =
              Option.value_map info.cmt_sourcefile ~default:false ~f:(fun recorded ->
                let recorded = Filename.basename recorded in
                let recorded =
                  if
                    String.is_suffix recorded ~suffix:".pp.ml"
                    || String.is_suffix recorded ~suffix:".pp.mli"
                  then
                    Filename.chop_extension (Filename.chop_extension recorded)
                    ^ Filename.extension recorded
                  else recorded
                in
                String.equal recorded (Filename.basename source))
            in
            if (not (String.equal info.cmt_modname unit_name)) || not same_source
            then None
            else if
              Option.value_map info.cmt_sourcefile ~default:false ~f:(fun file ->
                String.is_suffix file ~suffix:".pp.ml"
                || String.is_suffix file ~suffix:".pp.mli")
            then Some (`Preprocessed (Unix.stat artifact).st_mtime)
            else Option.map info.cmt_source_digest ~f:(fun digest -> `Digest digest)
          with
          | Sys_error _ | Unix.Unix_error _ | Cmt_format.Error _ -> None))
  | _ -> None
;;

(* Only Locate uses the source typing pipeline. Target processing uses a bounded
   syntax cache on the same worker, with no project index or target typing. *)
let locate kind (state : State.t) ?prefix ~doc uri position =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let pos = logical doc position in
    let name = if kind = `Definition then "definition" else "declaration" in
    let open_interfaces =
      Document_store.fold state.store ~init:[] ~f:(fun doc uris ->
        let uri = Document.uri doc in
        if String.is_suffix (Uri.to_path uri) ~suffix:".mli" then uri :: uris else uris)
    in
    let* result, config, selected_name, provenance =
      Document.Merlin.with_pipeline_exn ~name merlin (fun pipeline ->
        let result, uid = native_locate pipeline ~prefix `MLI pos in
        let result, uid =
          match result with
          | `File_not_found _ ->
            let config =
              Navigation_config.with_generated_sources (Mpipeline.final_config pipeline)
            in
            (match native_locate ~config pipeline ~prefix `MLI pos with
             | `File_not_found _, _ -> native_locate ~config pipeline ~prefix `ML pos
             | (`Found (Some path, _), _) as generated ->
               (match native_locate pipeline ~prefix `ML pos with
                | (`Found _, _) as implementation ->
                  let original = source_uri ~missing:true config path in
                  let source = Uri.to_path original in
                  let root =
                    Option.value
                      config.merlin.source_root
                      ~default:(Filename.dirname source)
                  in
                  let text file =
                    try
                      if (Unix.stat file).st_size <= 1_000_000
                      then Some (Io.String_path.read_file file)
                      else None
                    with
                    | Unix.Unix_error _ | Sys_error _ -> None
                  in
                  if
                    List.mem open_interfaces original ~equal:Uri.equal
                    || not (Dune_rule.generated_source ~root ~source ~text)
                  then implementation
                  else generated
                | _ -> generated)
             | result -> result)
          | result -> result, uid
        in
        let selected_name =
          Merlin_kernel.Mreader.reconstruct_identifier
            (Mpipeline.final_config pipeline)
            (Document.source doc)
            (Mpipeline.get_lexing_pos pipeline pos)
          |> List.rev
          |> List.hd
          |> Option.map ~f:(fun name -> name.Loc.txt)
        in
        let config = Mpipeline.final_config pipeline in
        result, config, selected_name, source_provenance config uid result)
    in
    let native_uri, native_position, origin =
      match result with
      | `Found (path, position) ->
        let target = Option.value_map path ~default:uri ~f:(source_uri config) in
        target, position, false
      | _ -> uri, Lexing.dummy_pos, true
    in
    let counterpart_uri =
      let path = Uri.to_path native_uri in
      if origin && kind = `Definition && String.is_suffix path ~suffix:".mli"
      then Some (Uri.of_path (String.sub path ~pos:0 ~len:(String.length path - 1)))
      else if String.is_suffix path ~suffix:".ml"
      then Some (Uri.of_path (path ^ "i"))
      else None
    in
    let native_doc = Document_store.get_opt state.store native_uri in
    let counterpart_doc =
      Option.bind counterpart_uri ~f:(Document_store.get_opt state.store)
    in
    let read uri =
      let path = Uri.to_path uri in
      try
        if (Unix.stat path).st_kind = S_REG
        then Some (Io.String_path.read_file path)
        else None
      with
      | Sys_error _ | Unix.Unix_error _ -> None
    in
    let contents uri opened =
      match opened with
      | Some _ when Uri.equal uri (Document.uri doc) ->
        let text = Document.text doc in
        Some (text, Some text)
      | Some target -> Some (Document.text target, read uri)
      | None -> Option.map (read uri) ~f:(fun text -> text, Some text)
    in
    let cache = state.navigation_syntax in
    let syntax uri text =
      Navigation_syntax.document cache ~file:(Uri.to_path uri) ~text
    in
    let location uri text declaration =
      let first, last = Navigation_syntax.offsets declaration in
      let convert offset =
        Reference_index.position text offset ~encoding:(State.position_encoding state)
      in
      Location.create
        ~uri
        ~range:(Range.create ~start:(convert first) ~end_:(convert last))
    in
    let counterpart source declaration =
      let open Option.O in
      let* uri = counterpart_uri in
      let* text, _ = contents uri counterpart_doc in
      let target = syntax uri text in
      let+ declaration = Navigation_syntax.counterpart ~source ~target declaration in
      location uri text declaration
    in
    let* located =
      match result with
      | `Found _ ->
        Document.Single_pipeline.run state.merlin (fun () ->
          match contents native_uri native_doc with
          | None -> None
          | Some (text, base_text) ->
            let current = syntax native_uri text in
            let valid =
              Uri.equal native_uri uri
              ||
              match provenance, base_text with
              | Some (`Digest expected), Some text ->
                String.equal expected (Digest.string text)
              | Some (`Preprocessed stamp), _ ->
                (try (Unix.stat (Uri.to_path native_uri)).st_mtime <= stamp with
                 | Unix.Unix_error _ -> false)
              | None, _ -> true
              | Some _, None -> false
            in
            let declaration =
              Option.bind base_text ~f:(fun base_text ->
                let base = syntax native_uri base_text in
                let declaration =
                  Navigation_syntax.locate base native_position ~name:selected_name
                in
                if String.equal text base_text
                then declaration
                else
                  Option.bind
                    declaration
                    ~f:(Navigation_syntax.counterpart ~source:base ~target:current))
            in
            if not valid
            then None
            else (
              match declaration with
              | Some declaration ->
                (match counterpart current declaration with
                 | Some _ as result -> result
                 | None -> Some (location native_uri text declaration))
              | None when Option.value_map base_text ~default:true ~f:(String.equal text)
                ->
                let (`Offset offset) =
                  Msource.get_offset
                    (Msource.make text)
                    (`Logical
                        ( native_position.pos_lnum
                        , native_position.pos_cnum - native_position.pos_bol ))
                in
                let start =
                  Reference_index.position
                    text
                    offset
                    ~encoding:(State.position_encoding state)
                in
                Some
                  (Location.create
                     ~uri:native_uri
                     ~range:(Range.create ~start ~end_:start))
              | None -> None))
      | `At_origin ->
        Document.Single_pipeline.run state.merlin (fun () ->
          let source = syntax uri (Document.text doc) in
          let offset =
            Text_document.absolute_position (Document.text_document doc) position
          in
          Option.bind (Navigation_syntax.at source ~offset) ~f:(counterpart source))
      | _ ->
        (match location_of_merlin_loc uri result with
         | Error message ->
           Log.log ~section:"debug" (fun () ->
             Log.msg "locate failed" [ "kind", `String name; "error", `String message ])
         | Ok _ -> ());
        Fiber.return None
    in
    check_version state doc;
    let validate uri previous =
      match previous, Document_store.get_opt state.store uri with
      | None, None -> ()
      | Some previous, Some current
        when Document.version previous = Document.version current
             && String.equal (Document.text previous) (Document.text current) -> ()
      | _ ->
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make
             ~code:ContentModified
             ~message:"A navigation target changed"
             ())
    in
    validate native_uri native_doc;
    Option.iter counterpart_uri ~f:(fun uri -> validate uri counterpart_doc);
    Fiber.return (Option.map located ~f:(fun location -> `Location [ location ]))
;;

let run kind (state : State.t) ?prefix ?doc uri position =
  let doc =
    match doc with
    | Some doc -> doc
    | None -> Document_store.get state.store uri
  in
  check_version state doc;
  match kind with
  | `Definition | `Declaration -> locate kind state ?prefix ~doc uri position
  | `Type_definition -> type_definition state doc position
;;
