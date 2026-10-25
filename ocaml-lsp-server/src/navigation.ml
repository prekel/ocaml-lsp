open Import
open Typedtree
open Ocaml_parsing.Location
module Browse_tree = Merlin_analysis.Browse_tree

(* These are name locations, not the enclosing binding or declaration. In
   particular, a pattern can both bind a name and refer to a type or label. *)
let declaration_names : Browse_raw.node -> Loc.t list = function
  | Pattern { pat_desc = Tpat_var (_, name); _ }
  | Pattern { pat_desc = Tpat_alias (_, _, name); _ } -> [ name.loc ]
  | Value_description d -> [ d.val_name.loc ]
  | Type_declaration d -> [ d.typ_name.loc ]
  | Constructor_declaration d -> [ d.cd_name.loc ]
  | Extension_constructor d -> [ d.ext_name.loc ]
  | Label_declaration d -> [ d.ld_name.loc ]
  | Module_binding_name d -> [ d.mb_name.loc ]
  | Module_declaration_name d -> [ d.md_name.loc ]
  | Module_type_declaration_name d -> [ d.mtd_name.loc ]
  | Class_declaration d -> [ d.ci_id_name.loc ]
  | Class_description d -> [ d.ci_id_name.loc ]
  | Class_type_declaration d -> [ d.ci_id_name.loc ]
  | Class_field { cf_desc = Tcf_val (name, _, _, _, _); _ } -> [ name.loc ]
  | Module_expr { mod_desc = Tmod_functor (Named (_, name, _), _); _ }
  | Module_type { mty_desc = Tmty_functor (Named (_, name, _), _); _ } -> [ name.loc ]
  | Expression e ->
    let names =
      match e.exp_desc with
      | Texp_letmodule (_, name, _, _, _) -> [ name.loc ]
      | Texp_for (_, pattern, _, _, _, _) -> [ pattern.ppat_loc ]
      | _ -> []
    in
    List.fold_left e.exp_extra ~init:names ~f:(fun names (extra, _, _) ->
      match extra with
      | Texp_newtype' (_, name) -> name.loc :: names
      | _ -> names)
  | Structure_item ({ str_desc = Tstr_class_type declarations; _ }, _) ->
    List.map declarations ~f:(fun (_, name, _) -> name.loc)
  | _ -> []
;;

let browse pipeline =
  Mbrowse.of_typedtree (Mtyper.get_typedtree (Mpipeline.typer_result pipeline))
;;

let same_location a b =
  let equal a b = a.Lexing.pos_cnum = b.Lexing.pos_cnum in
  equal a.Loc.loc_start b.Loc.loc_start && equal a.loc_end b.loc_end
;;

let declarations pipeline position =
  let root = browse pipeline in
  let position = Mpipeline.get_lexing_pos pipeline position in
  (* Use the same leaf and symbol identity as Merlin's occurrences query. A
     textual name or a location alone cannot distinguish shadowed bindings or
     the two meanings of a record pun. *)
  let selected =
    Mbrowse.enclosing position [ root ]
    |> List.find_map ~f:(fun (env, node) ->
      match node with
      | Browse_raw.Pattern { pat_desc = Tpat_any; _ } -> None
      | _ -> Some (Browse_tree.of_node ~env node))
  in
  match selected with
  | None -> []
  | Some selected ->
    let tree = Browse_tree.of_browse root in
    (match Browse_raw.node_is_constructor selected.t_node with
     | Some constructor ->
       Browse_tree.all_constructor_occurrences (selected, constructor.txt) tree
       |> List.concat_map ~f:(fun (occurrence : Browse_tree.t loc) ->
         List.map (declaration_names occurrence.txt.t_node) ~f:(fun name ->
           occurrence.loc, name))
     | None ->
       let path =
         Browse_raw.node_paths selected.t_node
         |> List.find ~f:(fun path -> Loc.compare_pos position path.loc = 0)
       in
       (match path with
        | None -> []
        | Some path ->
          Browse_tree.all_occurrences path.txt tree
          |> List.concat_map ~f:(fun ((node : Browse_tree.t), paths) ->
            let names = declaration_names node.t_node in
            List.filter_map paths ~f:(fun path ->
              if List.exists names ~f:(same_location path.loc)
              then Some (path.loc, path.loc)
              else None))))
;;

let references pipeline position ~include_declaration =
  let locations, _ =
    Query_commands.dispatch pipeline (Occurrences (`Ident_at position, `Buffer))
  in
  let declarations = declarations pipeline position in
  let locations =
    List.filter_map locations ~f:(fun location ->
      match
        List.find declarations ~f:(fun (original, _) -> same_location location original)
      with
      | None -> Some location
      | Some (_, name) -> if include_declaration then Some name else None)
  in
  List.sort_uniq locations ~compare:(fun a b -> Ordering.of_int (Loc.compare a b))
;;

let definition_range pipeline position ~type_definition ~source_position =
  let rec find node =
    let names = declaration_names node.Browse_tree.t_node in
    let name =
      List.find names ~f:(fun name ->
        name.Loc.loc_start.pos_cnum = position.Lexing.pos_cnum)
    in
    match name with
    | Some _ -> name
    | None ->
      (match node.t_node with
       | Browse_raw.Module_binding_name d
         when d.mb_loc.loc_start.pos_cnum = position.pos_cnum -> Some d.mb_name.loc
       | Browse_raw.Module_declaration_name d
         when d.md_loc.loc_start.pos_cnum = position.pos_cnum -> Some d.md_name.loc
       | Browse_raw.Value_description d
         when d.val_loc.loc_start.pos_cnum = position.pos_cnum ->
         (* Interface lookup can point at [val], rather than its name. *)
         Some d.val_name.loc
       | Browse_raw.Type_declaration d
         when d.typ_loc.loc_start.pos_cnum = position.pos_cnum ->
         if type_definition
         then Some d.typ_name.loc
         else
           (* Locate can point at the whole type declaration for a constructor.
              Use the queried symbol's identity to recover its name, rather
              than selecting the type name or guessing by spelling. *)
           Option.bind source_position ~f:(fun source_position ->
             declarations pipeline source_position
             |> List.find_map ~f:(fun (_, name) ->
               if
                 name.loc_start.pos_cnum >= d.typ_loc.loc_start.pos_cnum
                 && name.loc_end.pos_cnum <= d.typ_loc.loc_end.pos_cnum
               then Some name
               else None))
       | _ -> List.find_map (Lazy.force node.t_children) ~f:find)
  in
  find (Browse_tree.of_browse (browse pipeline)) |> Option.bind ~f:Range.of_loc_opt
;;
