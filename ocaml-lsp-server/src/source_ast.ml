open Import

type t =
  { pattern_ends : (int, unit) Stdlib.Hashtbl.t
  ; values : (string, Loc.t list) Stdlib.Hashtbl.t
  }

let create parsetree =
  let pattern_ends = Stdlib.Hashtbl.create 64 in
  let values = Stdlib.Hashtbl.create 32 in
  let add_value ({ txt; loc } : string Asttypes.loc) =
    let locations = Option.value (Stdlib.Hashtbl.find_opt values txt) ~default:[] in
    Stdlib.Hashtbl.replace values txt (loc :: locations)
  in
  let binding_pattern =
    { Ast_iterator.default_iterator with
      attribute = (fun _ _ -> ())
    ; pat =
        (fun self pattern ->
          (match pattern.Parsetree.ppat_desc with
           | Ppat_var name | Ppat_alias (_, name) -> add_value name
           | _ -> ());
          Ast_iterator.default_iterator.pat self pattern)
    }
  in
  let iterator =
    { Ast_iterator.default_iterator with
      attribute = (fun _ _ -> ())
    ; pat =
        (fun self pattern ->
          (match pattern.Parsetree.ppat_desc with
           | Ppat_var _ ->
             Stdlib.Hashtbl.replace pattern_ends pattern.ppat_loc.loc_end.pos_cnum ()
           | _ -> ());
          Ast_iterator.default_iterator.pat self pattern)
    ; value_binding =
        (fun self binding ->
          binding_pattern.pat binding_pattern binding.Parsetree.pvb_pat;
          Ast_iterator.default_iterator.value_binding self binding)
    ; value_description =
        (fun self description ->
          add_value description.Parsetree.pval_name;
          Ast_iterator.default_iterator.value_description self description)
    }
  in
  (match parsetree with
   | `Implementation structure -> iterator.structure iterator structure
   | `Interface signature -> iterator.signature iterator signature);
  { pattern_ends; values }
;;

let has_pattern_end t position =
  Stdlib.Hashtbl.mem t.pattern_ends position.Lexing.pos_cnum
;;

let contains_outline t (item : Query_protocol.item) =
  match item.outline_kind with
  | `Value ->
    Option.value (Stdlib.Hashtbl.find_opt t.values item.outline_name) ~default:[]
    |> List.exists ~f:(fun loc ->
      item.location.loc_start.pos_cnum <= loc.Loc.loc_start.pos_cnum
      && loc.loc_end.pos_cnum <= item.location.loc_end.pos_cnum)
  | _ -> true
;;

type type_target =
  | Declaration of Parsetree.type_declaration
  | Core_type of Parsetree.type_declaration

let type_enclosing pipeline target position =
  let module Browse_tree = Merlin_analysis.Browse_tree in
  let module Type_enclosing = Merlin_analysis.Type_enclosing in
  let source =
    match target with
    | Declaration declaration | Core_type declaration -> declaration
  in
  let rec find (node : Browse_tree.t) =
    match node.t_node with
    | Browse_raw.Type_declaration declaration
      when String.equal declaration.typ_name.txt source.ptype_name.txt
           && declaration.typ_name.loc.loc_start.pos_cnum
              = source.ptype_name.loc.loc_start.pos_cnum
           && declaration.typ_name.loc.loc_end.pos_cnum
              = source.ptype_name.loc.loc_end.pos_cnum -> Some node
    | _ -> List.find_map (Lazy.force node.t_children) ~f:find
  in
  let tree =
    Mpipeline.typer_result pipeline
    |> Mtyper.get_typedtree
    |> Mbrowse.of_typedtree
    |> Browse_tree.of_browse
  in
  let verbosity = (Mpipeline.final_config pipeline).query.verbosity in
  let open Option.O in
  let* node = find tree in
  let* loc, typ =
    match target, node.t_node with
    | Declaration _, Browse_raw.Type_declaration declaration ->
      Some
        ( source.ptype_loc
        , Type_enclosing.Type_decl (node.t_env, declaration.typ_id, declaration.typ_type)
        )
    | Core_type _, _ ->
      let cursor = Mpipeline.get_lexing_pos pipeline position in
      let nodes = Mbrowse.enclosing cursor [ [ node.t_env, node.t_node ] ] in
      let reconstructed =
        Merlin_analysis.Misc_utils.reconstruct_identifier pipeline cursor None
        |> Type_enclosing.from_reconstructed ~nodes ~cursor ~verbosity
      in
      let enclosing =
        Type_enclosing.from_nodes
          ~path:(List.map nodes ~f:(fun (env, node) -> env, node, `No))
      in
      List.hd (reconstructed @ enclosing) |> Option.map ~f:(fun (loc, typ, _) -> loc, typ)
    | _ -> None
  in
  Some (loc, Type_enclosing.print_type ~verbosity typ)
;;
