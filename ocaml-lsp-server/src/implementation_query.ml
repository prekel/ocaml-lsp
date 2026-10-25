open Import
open Fiber.O

(* Merlin's locate query reports [At_origin] when invoked on a type declaration
   in an interface. Keep the declaration's path through nested modules so that
   we can resolve it with Merlin's shape-backed locator in the implementation's
   typing environment. *)
let declaration_path_at_position ?(strict = true) doc position =
  Document.Merlin.with_pipeline_exn ~name:"implementation" doc (fun pipeline ->
    let position = Mpipeline.get_lexing_pos pipeline (Position.logical position) in
    let enclosing =
      let browse =
        Mpipeline.typer_result pipeline |> Mtyper.get_typedtree |> Mbrowse.of_typedtree
      in
      Mbrowse.enclosing position [ browse ]
    in
    let at name loc =
      Loc.compare_pos position name = 0
      || ((not strict) && position.pos_cnum = loc.Loc.loc_start.pos_cnum)
    in
    match
      List.find_map enclosing ~f:(fun (_, (node : Browse_raw.node)) ->
        match node with
        | Type_declaration d when at d.typ_name.loc d.typ_loc ->
          Some (`Type, d.typ_name.txt, d.typ_name.loc)
        | Value_description d when at d.val_name.loc d.val_loc ->
          Some (`Vals, d.val_name.txt, d.val_name.loc)
        | Module_declaration d when at d.md_name.loc d.md_loc ->
          Option.map d.md_name.txt ~f:(fun name -> `Mod, name, d.md_name.loc)
        | _ -> None)
    with
    | None -> None
    | Some _
      when List.exists enclosing ~f:(fun (_, (node : Browse_raw.node)) ->
             match node with
             | Module_type_declaration _ -> true
             | Module_type { mty_desc = Tmty_functor (Named (_, _, parameter), _); _ }
             | Module_expr { mod_desc = Tmod_functor (Named (_, _, parameter), _); _ } ->
               Loc.compare_pos position parameter.mty_loc = 0
             | _ -> false) -> None
    | Some (namespace, name, selected) ->
      let modules =
        List.filter_map enclosing ~f:(fun (_, (node : Browse_raw.node)) ->
          match node with
          | Module_declaration { md_name = { txt = Some name; loc }; _ }
          | Module_binding { mb_name = { txt = Some name; loc }; _ }
            when loc.loc_start.pos_cnum <> selected.loc_start.pos_cnum -> Some name
          | _ -> None)
        |> List.rev
      in
      Option.map
        (Longident.unflatten (modules @ [ name ]))
        ~f:(fun path -> namespace, path))
;;

(* A functor result has no ordinary path such as [F.t]. Expose each result as a
   temporary module in the query environment, retaining its declaration UIDs
   for Locate. Keeping a module boundary avoids finding an outer [t] when the
   result itself does not export [t]. *)
let rec result_signature seen env mty =
  match Ocaml_typing.Mtype.scrape env mty with
  | Mty_signature _ as mty -> Some (env, mty)
  | Mty_functor (parameter, result) ->
    let env =
      match parameter with
      | Named (Some id, mty) -> Env.add_module id Mp_present mty env
      | Named (None, _) | Unit -> env
    in
    result_signature seen env result
  | Mty_alias path when not (List.exists seen ~f:(Path.same path)) ->
    (match Env.find_module path env with
     | declaration -> result_signature (path :: seen) env declaration.md_type
     | exception Not_found -> None)
  | Mty_alias _ | Mty_ident _ | Mty_for_hole -> None
;;

let rec module_environment env (lid : Longident.t) =
  let open Option.O in
  let* env, signature =
    let* env, lid =
      match lid with
      | Lident _ -> Some (env, lid)
      | Ldot (parent, name) ->
        let+ env, lid = module_environment env parent in
        env, Longident.Ldot (lid, name)
      | Lapply _ -> None
    in
    let* md_type =
      match Env.find_module_by_name lid env with
      | (_ : Path.t), declaration -> Some declaration.md_type
      | exception Not_found -> None
    in
    result_signature [] env md_type
  in
  let id = Ident.create_local "__ocamllsp_implementation" in
  let env = Env.add_module id Mp_present signature env in
  Some (env, Longident.Lident (Ident.name id))
;;

let lookup namespace =
  let lookup env lid =
    try
      let path =
        match namespace with
        | `Type -> fst (Env.find_type_by_name lid env)
        | `Vals -> fst (Env.find_value_by_name lid env)
        | `Mod -> fst (Env.find_module_by_name lid env)
      in
      Some (env, path)
    with
    | Not_found -> None
  in
  fun env lid ->
    match lookup env lid with
    | Some _ as result -> result
    | None ->
      (match lid with
       | Longident.Lident _ | Lapply _ -> None
       | Ldot (parent, name) ->
         let open Option.O in
         let* env, lid = module_environment env parent in
         lookup env (Longident.Ldot (lid, name)))
;;

let declaration_source config uid (loc : Loc.t) =
  let unit_name =
    match uid with
    | Ocaml_typing.Shape.Uid.Item { comp_unit; _ } -> Some comp_unit
    | Compilation_unit name -> Some name
    | Internal | Predef _ -> None
  in
  let owner =
    Option.bind unit_name ~f:(fun name ->
      List.find_map
        (Mconfig.build_path config @ Mconfig.cmt_path config)
        ~f:(fun directory ->
          List.find_map
            [ name; Stdlib.String.uncapitalize_ascii name ]
            ~f:(fun name ->
              if Sys.file_exists (Filename.concat directory (name ^ ".cmi"))
              then Some directory
              else None)))
  in
  let recorded = loc.loc_start.pos_fname in
  let installed =
    Option.to_list owner
    |> List.filter ~f:(fun directory ->
      Option.is_some (Installed_library.root (Filename.concat directory "source.ml")))
  in
  let candidates =
    List.map installed ~f:(fun dir -> Filename.concat dir (Filename.basename recorded))
    @ List.map (config.query.directory :: Mconfig.source_path config) ~f:(fun dir ->
      Filename.concat dir (Filename.basename recorded))
    @ [ recorded ]
  in
  List.find_map candidates ~f:(fun path ->
    try
      if (Unix.stat path).st_kind = S_REG
      then Some (Uri.of_path (Unix.realpath path))
      else None
    with
    | Unix.Unix_error _ | Sys_error _ -> None)
;;

let location_in_document doc (namespace, path) =
  let uri = Document.uri doc in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    Document.Merlin.with_pipeline_exn ~name:"implementation" doc (fun pipeline ->
      let typer = Mpipeline.typer_result pipeline in
      match lookup namespace (Mtyper.get_env typer) path with
      | None -> None
      | Some (env, path) ->
        (match
           let uid, declaration_loc =
             match namespace with
             | `Type ->
               let d = Env.find_type path env in
               d.type_uid, d.type_loc
             | `Vals ->
               let d = Env.find_value path env in
               d.val_uid, d.val_loc
             | `Mod ->
               let d = Env.find_module path env in
               d.md_uid, d.md_loc
           in
           let synthetic =
             String.equal (Ident.name (Path.head path)) "__ocamllsp_implementation"
           in
           match
             ( synthetic
             , Ocaml_typing.Shape.Uid.Tbl.find_opt (Env.get_uid_to_loc_tbl ()) uid )
           with
           | true, Some loc -> `Found (Some uid, None, loc.loc_start)
           | true, None ->
             (* Merlin 4.19 gives synthetic modules an Internal shape. Follow
                the imported declaration in its owning unit instead of feeding
                that shape to Locate (which would assert on an Internal UID). *)
             (match
                declaration_source (Mpipeline.final_config pipeline) uid declaration_loc
              with
              | None -> `Not_in_env (Path.name path)
              | Some uri -> `Declaration (uri, declaration_loc.loc_start))
           | false, _ ->
             Merlin_analysis.Locate.from_path
               ~config:(Mpipeline.final_config pipeline)
               ~env
               ~namespace:(namespace :> Merlin_analysis.Namespaced_path.Namespace.t)
               `ML
               path
         with
         | `Declaration (uri, position) -> Some (`Declaration (uri, position))
         | `Found (_, file, position) ->
           let uri = Option.fold file ~init:uri ~f:(fun _ file -> Uri.of_path file) in
           (* 4.14 UIDs do not distinguish interfaces and implementations.
              Never return Locate's interface-only fallback as a body. *)
           (match Filename.extension (Uri.to_path uri) with
            | ".ml" | ".re" | ".eliom" ->
              let range =
                if
                  Uri.equal
                    uri
                    (Uri.of_path (Mpipeline.final_config pipeline).query.filename)
                then
                  Navigation.definition_range
                    pipeline
                    position
                    ~type_definition:(namespace = `Type)
                    ~source_position:None
                else None
              in
              Option.map (Position.of_lexical_position position) ~f:(fun start ->
                `Target
                  (Location.create
                     ~uri
                     ~range:
                       (Option.value range ~default:(Range.create ~start ~end_:start))))
            | _ -> None)
         | `Builtin | `File_not_found _ | `Not_found _ | `Not_in_env _ -> None))
;;

let with_document (state : State.t) uri f =
  match Document_store.get_opt state.store uri with
  | Some doc -> f doc
  | None ->
    Document.make_from_file
      (State.wheel state)
      state.merlin_config
      state.merlin
      uri
      ~position_encoding:(State.position_encoding state)
    >>= (function
     | None -> Fiber.return None
     | Some doc ->
       Fiber.finalize (fun () -> f doc) ~finally:(fun () -> Document.close doc))
;;

let implementation_source uri =
  match Filename.extension (Uri.to_path uri) with
  | ".ml" | ".re" | ".eliom" -> true
  | _ -> false
;;

let refine_location state (location : Location.t) =
  with_document state location.uri (fun doc ->
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin merlin ->
      let+ range =
        Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
          let pos =
            Mpipeline.get_lexing_pos pipeline (Position.logical location.range.start)
          in
          let range =
            Option.value
              (Navigation.definition_range
                 pipeline
                 pos
                 ~type_definition:true
                 ~source_position:None)
              ~default:location.range
          in
          let convert position =
            let pos = Mpipeline.get_lexing_pos pipeline (Position.logical position) in
            Reference_index.position
              (Document.text doc)
              pos.pos_cnum
              ~encoding:(State.position_encoding state)
          in
          Range.create ~start:(convert range.start) ~end_:(convert range.end_))
      in
      Some { location with range })
;;

let rec declaration_locations state merlin uri path seen =
  let key = Uri.to_string uri, path in
  if List.mem seen key ~equal:Poly.equal
  then Fiber.return []
  else
    let+ results =
      Document.get_impl_intf_counterparts (Some merlin) uri
      |> Fiber.sequential_map ~f:(fun implementation_uri ->
        with_document state implementation_uri (fun doc -> location_in_document doc path)
        >>= function
        | None -> Fiber.return []
        | Some (`Target location) ->
          let+ refined = refine_location state location in
          Option.to_list refined
        | Some (`Declaration (target_uri, target)) ->
          let+ found =
            with_document state target_uri (fun doc ->
              match Document.kind doc, Position.of_lexical_position target with
              | `Merlin merlin, Some position when Document.Merlin.kind merlin = Intf ->
                let* path = declaration_path_at_position ~strict:false merlin position in
                (match path with
                 | None -> Fiber.return None
                 | Some path ->
                   let+ locations =
                     declaration_locations state merlin target_uri path (key :: seen)
                   in
                   Some locations)
              | _ -> Fiber.return None)
          in
          Option.value found ~default:[])
    in
    List.concat results
;;

let resolve_declaration state merlin uri path =
  let+ locations = declaration_locations state merlin uri path [] in
  match locations with
  | [] -> None
  | locations -> Some (`Location locations)
;;

let run (state : State.t) uri position =
  match Document.kind (Document_store.get state.store uri) with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let* declaration =
      if Document.Merlin.kind merlin = Intf
      then declaration_path_at_position merlin position
      else Fiber.return None
    in
    (match declaration with
     | Some path -> resolve_declaration state merlin uri path
     | None ->
       let* located =
         Document.Merlin.dispatch_exn
           merlin
           (Query_protocol.Locate (None, `MLI, Position.logical position))
       in
       let* resolved =
         match located with
         | `Found (file, target) ->
           let target_uri = Option.fold file ~init:uri ~f:(fun _ p -> Uri.of_path p) in
           with_document state target_uri (fun doc ->
             match Document.kind doc, Position.of_lexical_position target with
             | `Merlin target, Some position when Document.Merlin.kind target = Intf ->
               let* path = declaration_path_at_position ~strict:false target position in
               (match path with
                | None -> Fiber.return None
                | Some path -> resolve_declaration state target target_uri path)
             | _ -> Fiber.return None)
         | _ -> Fiber.return None
       in
       (match resolved with
        | Some _ -> Fiber.return resolved
        | None ->
          let* result =
            Document.Merlin.dispatch_exn
              merlin
              (Query_protocol.Locate (None, `ML, Position.logical position))
          in
          (match result with
           | `Found (file, target) ->
             let target_uri = Option.fold file ~init:uri ~f:(fun _ p -> Uri.of_path p) in
             if not (implementation_source target_uri)
             then Fiber.return None
             else
               with_document state target_uri (fun doc ->
                 match Document.kind doc with
                 | `Other -> Fiber.return None
                 | `Merlin target_doc ->
                   let+ range =
                     Document.Merlin.with_pipeline_exn target_doc (fun pipeline ->
                       Navigation.definition_range
                         pipeline
                         target
                         ~type_definition:true
                         ~source_position:None)
                   in
                   Option.map (Position.of_lexical_position target) ~f:(fun start ->
                     `Location
                       [ Location.create
                           ~uri:target_uri
                           ~range:
                             (Option.value
                                range
                                ~default:(Range.create ~start ~end_:start))
                       ]))
           | _ -> Fiber.return None)))
;;
