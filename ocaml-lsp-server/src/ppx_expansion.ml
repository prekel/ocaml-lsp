open Import

type node =
  | Expression of Parsetree.expression
  | Pattern of Parsetree.pattern
  | Core_type of Parsetree.core_type
  | Module_expr of Parsetree.module_expr
  | Module_type of Parsetree.module_type
  | Structure_item of Parsetree.structure_item
  | Signature_item of Parsetree.signature_item

type t =
  | Deriving of Ppx_deriving.t
  | Node of
      { node : node
      ; marker : Loc.t
      ; name : string
      }

let location = function
  | Expression e -> e.Parsetree.pexp_loc
  | Pattern p -> p.ppat_loc
  | Core_type t -> t.ptyp_loc
  | Module_expr m -> m.pmod_loc
  | Module_type m -> m.pmty_loc
  | Structure_item i -> i.pstr_loc
  | Signature_item i -> i.psig_loc
;;

let print = function
  | Expression e -> Format.asprintf "%a" Pprintast.expression e
  | Pattern p -> Format.asprintf "%a" Pprintast.pattern p
  | Core_type t -> Format.asprintf "%a" Pprintast.core_type t
  | Module_expr m -> Format.asprintf "%a" Pprintast.module_expr m
  | Module_type m -> Format.asprintf "%a" Pprintast.module_type m
  | Structure_item i -> Pprintast.string_of_structure [ i ]
  | Signature_item i -> Format.asprintf "%a" Pprintast.signature [ i ]
;;

let name = function
  | Deriving _ -> "deriving"
  | Node t -> t.name
;;

let range = function
  | Deriving t -> Ppx_deriving.range t
  | Node t -> Range.of_loc t.marker
;;

let same_kind a b =
  match a, b with
  | Expression _, Expression _
  | Pattern _, Pattern _
  | Core_type _, Core_type _
  | Module_expr _, Module_expr _
  | Module_type _, Module_type _
  | Structure_item _, Structure_item _
  | Signature_item _, Signature_item _ -> true
  | _ -> false
;;

let deriving_group = function
  | Structure_item { pstr_desc = Pstr_type _; _ }
  | Signature_item { psig_desc = Psig_type _ | Psig_typesubst _; _ } -> true
  | _ -> false
;;

let in_region (region : Loc.t) (loc : Loc.t) =
  region.loc_start.pos_cnum <= loc.loc_start.pos_cnum
  && loc.loc_end.pos_cnum <= region.loc_end.pos_cnum
  && loc.loc_start.pos_cnum < region.loc_end.pos_cnum
;;

let expanded_nodes pipeline source =
  let result = ref [] in
  let visit node default =
    if same_kind source node && in_region (location source) (location node)
    then result := node :: !result
    else default ()
  in
  let iterator =
    { Ast_iterator.default_iterator with
      expr =
        (fun self e ->
          visit (Expression e) (fun () -> Ast_iterator.default_iterator.expr self e))
    ; pat =
        (fun self p ->
          visit (Pattern p) (fun () -> Ast_iterator.default_iterator.pat self p))
    ; typ =
        (fun self t ->
          visit (Core_type t) (fun () -> Ast_iterator.default_iterator.typ self t))
    ; module_expr =
        (fun self m ->
          visit (Module_expr m) (fun () ->
            Ast_iterator.default_iterator.module_expr self m))
    ; module_type =
        (fun self m ->
          visit (Module_type m) (fun () ->
            Ast_iterator.default_iterator.module_type self m))
    ; structure_item =
        (fun self i ->
          visit (Structure_item i) (fun () ->
            Ast_iterator.default_iterator.structure_item self i))
    ; signature_item =
        (fun self i ->
          visit (Signature_item i) (fun () ->
            Ast_iterator.default_iterator.signature_item self i))
    ; attribute = (fun _ _ -> ())
    }
  in
  (match Mpipeline.ppx_parsetree pipeline with
   | `Implementation structure -> iterator.structure iterator structure
   | `Interface signature -> iterator.signature iterator signature);
  List.rev !result
;;

let expansion pipeline = function
  | Deriving t -> Ppx_deriving.expansion pipeline t
  | Node t ->
    let expanded = expanded_nodes pipeline t.node in
    let text = String.concat ~sep:"\n" (List.map expanded ~f:print) in
    if List.is_empty expanded || String.equal text (print t.node) then None else Some text
;;

let candidates pipeline select =
  let result = ref [] in
  let owner = ref None in
  let with_node node f =
    let previous = !owner in
    owner := Some node;
    f ();
    owner := previous
  in
  let extension node ((name, _) : Parsetree.extension) =
    if select node (location node) name.loc
    then result := Node { node; marker = location node; name = name.txt } :: !result
  in
  let iterator =
    { Ast_iterator.default_iterator with
      expr =
        (fun self e ->
          let node = Expression e in
          (match e.pexp_desc with
           | Pexp_extension ext -> extension node ext
           | _ -> ());
          with_node node (fun () -> Ast_iterator.default_iterator.expr self e))
    ; pat =
        (fun self p ->
          let node = Pattern p in
          (match p.ppat_desc with
           | Ppat_extension ext -> extension node ext
           | _ -> ());
          with_node node (fun () -> Ast_iterator.default_iterator.pat self p))
    ; typ =
        (fun self t ->
          let node = Core_type t in
          (match t.ptyp_desc with
           | Ptyp_extension ext -> extension node ext
           | _ -> ());
          with_node node (fun () -> Ast_iterator.default_iterator.typ self t))
    ; module_expr =
        (fun self m ->
          let node = Module_expr m in
          (match m.pmod_desc with
           | Pmod_extension ext -> extension node ext
           | _ -> ());
          with_node node (fun () -> Ast_iterator.default_iterator.module_expr self m))
    ; module_type =
        (fun self m ->
          let node = Module_type m in
          (match m.pmty_desc with
           | Pmty_extension ext -> extension node ext
           | _ -> ());
          with_node node (fun () -> Ast_iterator.default_iterator.module_type self m))
    ; structure_item =
        (fun self i ->
          let node = Structure_item i in
          (match i.pstr_desc with
           | Pstr_extension (ext, _) -> extension node ext
           | _ -> ());
          with_node node (fun () -> Ast_iterator.default_iterator.structure_item self i))
    ; signature_item =
        (fun self i ->
          let node = Signature_item i in
          (match i.psig_desc with
           | Psig_extension (ext, _) -> extension node ext
           | _ -> ());
          with_node node (fun () -> Ast_iterator.default_iterator.signature_item self i))
    ; class_expr =
        (fun self c ->
          (match c.Parsetree.pcl_desc, !owner with
           | Pcl_extension (name, _), Some node when select node c.pcl_loc name.loc ->
             result := Node { node; marker = c.pcl_loc; name = name.txt } :: !result
           | _ -> ());
          Ast_iterator.default_iterator.class_expr self c)
    ; class_type =
        (fun self c ->
          (match c.Parsetree.pcty_desc, !owner with
           | Pcty_extension (name, _), Some node when select node c.pcty_loc name.loc ->
             result := Node { node; marker = c.pcty_loc; name = name.txt } :: !result
           | _ -> ());
          Ast_iterator.default_iterator.class_type self c)
    ; attribute =
        (fun _ attribute ->
          match !owner with
          | Some node
            when ((not (String.equal attribute.Parsetree.attr_name.txt "deriving"))
                  || not (deriving_group node))
                 && (not (String.is_prefix attribute.attr_name.txt ~prefix:"ocaml."))
                 && (not (String.is_prefix attribute.attr_name.txt ~prefix:"merlin."))
                 && select node attribute.attr_loc attribute.attr_loc ->
            result
            := Node { node; marker = attribute.attr_loc; name = attribute.attr_name.txt }
               :: !result
          | _ -> ())
    }
  in
  (match Mpipeline.reader_parsetree pipeline with
   | `Implementation structure -> iterator.structure iterator structure
   | `Interface signature -> iterator.signature iterator signature);
  !result
;;

let first_expanded pipeline candidates =
  List.find candidates ~f:(fun candidate -> Option.is_some (expansion pipeline candidate))
;;

let at_position ?(hover = false) pipeline position =
  match Ppx_deriving.at_position pipeline position with
  | Some deriving when Option.is_some (Ppx_deriving.expansion pipeline deriving) ->
    Some (Deriving deriving)
  | _ ->
    let position = Mpipeline.get_lexing_pos pipeline position in
    candidates pipeline (fun _ full name ->
      let loc = if hover then name else full in
      Loc.compare_pos position loc = 0 && position.pos_cnum < loc.loc_end.pos_cnum)
    |> first_expanded pipeline
;;

let for_declaration pipeline declaration =
  match Ppx_deriving.for_declaration pipeline declaration with
  | Some deriving -> Some (Deriving deriving)
  | None ->
    candidates pipeline (fun node _ _ ->
      let declarations =
        match node with
        | Structure_item { pstr_desc = Pstr_type (_, declarations); _ }
        | Signature_item { psig_desc = Psig_type (_, declarations); _ } -> declarations
        | _ -> []
      in
      List.exists declarations ~f:(fun source ->
        source.Parsetree.ptype_name.loc.loc_start.pos_cnum
        = declaration.Parsetree.ptype_name.loc.loc_start.pos_cnum))
    |> first_expanded pipeline
;;

let signature pipeline = function
  | Deriving t -> Ppx_deriving.signature pipeline t
  | Node t ->
    let region = location t.node in
    if Ppx_deriving.has_errors pipeline region
    then None
    else (
      let declarations =
        match t.node with
        | Structure_item { pstr_desc = Pstr_type (_, declarations); _ }
        | Signature_item { psig_desc = Psig_type (_, declarations); _ } -> declarations
        | _ -> []
      in
      let exported =
        match t.node with
        | Structure_item _ | Signature_item _ ->
          Ppx_deriving.signature_in_region ~exclude:declarations pipeline region
        | _ -> None
      in
      match exported with
      | Some _ -> exported
      | None ->
        let module Browse_tree = Merlin_analysis.Browse_tree in
        let module Type_enclosing = Merlin_analysis.Type_enclosing in
        let rec find (node : Browse_tree.t) =
          let info =
            if not (in_region region node.t_loc)
            then None
            else (
              match t.node, node.t_node with
              | Expression _, Browse_raw.Expression e ->
                Some (Type_enclosing.Type (node.t_env, e.exp_type))
              | Pattern _, Browse_raw.Pattern p ->
                Some (Type_enclosing.Type (node.t_env, p.pat_type))
              | Core_type _, Browse_raw.Core_type c ->
                Some (Type_enclosing.Type (node.t_env, c.ctyp_type))
              | Module_expr _, Browse_raw.Module_expr m ->
                Some (Type_enclosing.Modtype (node.t_env, m.mod_type))
              | Module_type _, Browse_raw.Module_type m ->
                Some (Type_enclosing.Modtype (node.t_env, m.mty_type))
              | Structure_item _, Browse_raw.Expression e ->
                Some (Type_enclosing.Type (node.t_env, e.exp_type))
              | _ -> None)
          in
          match info with
          | Some _ -> info
          | None -> List.find_map (Lazy.force node.t_children) ~f:find
        in
        let tree =
          Mpipeline.typer_result pipeline
          |> Mtyper.get_typedtree
          |> Mbrowse.of_typedtree
          |> Browse_tree.of_browse
        in
        Option.map
          (find tree)
          ~f:
            (Type_enclosing.print_type
               ~verbosity:(Mpipeline.final_config pipeline).query.verbosity))
;;
