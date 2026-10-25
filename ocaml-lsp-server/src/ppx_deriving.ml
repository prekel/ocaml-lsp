open Import

type t =
  { declarations : Parsetree.type_declaration list
  ; attribute : Parsetree.attribute
  }

let range t = Range.of_loc t.attribute.attr_loc

let same_name (a : string Asttypes.loc) (b : string Asttypes.loc) =
  String.equal a.txt b.txt
  && a.loc.loc_start.pos_cnum = b.loc.loc_start.pos_cnum
  && a.loc.loc_end.pos_cnum = b.loc.loc_end.pos_cnum
;;

let find pipeline select =
  let result = ref None in
  let visit declarations =
    List.iter declarations ~f:(fun (declaration : Parsetree.type_declaration) ->
      List.iter declaration.ptype_attributes ~f:(fun (attribute : Parsetree.attribute) ->
        if
          String.equal attribute.attr_name.txt "deriving" && select declarations attribute
        then result := Some { declarations; attribute }))
  in
  let iterator =
    { Ast_iterator.default_iterator with
      structure_item =
        (fun self item ->
          (match item.Parsetree.pstr_desc with
           | Pstr_type (_, declarations) -> visit declarations
           | _ -> ());
          Ast_iterator.default_iterator.structure_item self item)
    ; signature_item =
        (fun self item ->
          (match item.Parsetree.psig_desc with
           | Psig_type (_, declarations) | Psig_typesubst declarations ->
             visit declarations
           | _ -> ());
          Ast_iterator.default_iterator.signature_item self item)
    ; attribute = (fun _ _ -> ())
    }
  in
  (match Mpipeline.reader_parsetree pipeline with
   | `Implementation structure -> iterator.structure iterator structure
   | `Interface signature -> iterator.signature iterator signature);
  !result
;;

let at_position pipeline position =
  let position = Mpipeline.get_lexing_pos pipeline position in
  find pipeline (fun _ attribute ->
    Loc.compare_pos position attribute.Parsetree.attr_loc = 0
    && position.pos_cnum < attribute.attr_loc.loc_end.pos_cnum)
;;

let for_declaration pipeline declaration =
  find pipeline (fun declarations _ ->
    List.exists declarations ~f:(fun candidate ->
      same_name candidate.Parsetree.ptype_name declaration.Parsetree.ptype_name))
;;

(* Derivers commonly locate generated items at the type group or its attribute.
   Do not descend into an item once selected: its local functions belong to the
   expansion, rather than being additional exported declarations. *)
let contains t (loc : Loc.t) =
  let start, stop =
    List.fold_left
      t.declarations
      ~init:(Stdlib.max_int, Stdlib.min_int)
      ~f:(fun (start, stop) declaration ->
        ( Stdlib.min start declaration.Parsetree.ptype_loc.loc_start.pos_cnum
        , Stdlib.max stop declaration.ptype_loc.loc_end.pos_cnum ))
  in
  start <= loc.loc_start.pos_cnum && loc.loc_start.pos_cnum < stop
;;

let original t declarations =
  List.exists declarations ~f:(fun declaration ->
    List.exists t.declarations ~f:(fun source ->
      same_name declaration.Parsetree.ptype_name source.Parsetree.ptype_name))
;;

let expansion pipeline t =
  let structure = ref [] in
  let signature = ref [] in
  let iterator =
    { Ast_iterator.default_iterator with
      structure_item =
        (fun self item ->
          if contains t item.Parsetree.pstr_loc
          then (
            match item.pstr_desc with
            | Pstr_type (_, declarations) when original t declarations -> ()
            | _ -> structure := item :: !structure)
          else Ast_iterator.default_iterator.structure_item self item)
    ; signature_item =
        (fun self item ->
          if contains t item.Parsetree.psig_loc
          then (
            match item.psig_desc with
            | (Psig_type (_, declarations) | Psig_typesubst declarations)
              when original t declarations -> ()
            | _ -> signature := item :: !signature)
          else Ast_iterator.default_iterator.signature_item self item)
    }
  in
  (match Mpipeline.ppx_parsetree pipeline with
   | `Implementation items -> iterator.structure iterator items
   | `Interface items -> iterator.signature iterator items);
  match !structure, !signature with
  | [], [] -> None
  | items, [] -> Some (Pprintast.string_of_structure (List.rev items))
  | [], items -> Some (Format.asprintf "%a" Pprintast.signature (List.rev items))
  | _ -> None
;;

let signature_location : Types.signature_item -> Loc.t = function
  | Sig_value (_, declaration, _) -> declaration.val_loc
  | Sig_type (_, declaration, _, _) -> declaration.type_loc
  | Sig_typext (_, declaration, _, _) -> declaration.ext_loc
  | Sig_module (_, _, declaration, _, _) -> declaration.md_loc
  | Sig_modtype (_, declaration, _) -> declaration.mtd_loc
  | Sig_class (_, declaration, _, _) -> declaration.cty_loc
  | Sig_class_type (_, declaration, _, _) -> declaration.clty_loc
;;

let exported : Types.signature_item -> bool = function
  | Sig_value (_, _, visibility)
  | Sig_type (_, _, _, visibility)
  | Sig_typext (_, _, _, visibility)
  | Sig_module (_, _, _, _, visibility)
  | Sig_modtype (_, _, visibility)
  | Sig_class (_, _, _, visibility)
  | Sig_class_type (_, _, _, visibility) -> visibility = Exported
;;

let rec public_module_type : Types.module_type -> Types.module_type = function
  | Mty_signature signature -> Mty_signature (public_signature signature)
  | Mty_functor (parameter, result) ->
    let parameter : Types.functor_parameter =
      match parameter with
      | Unit -> Unit
      | Named (name, typ) -> Named (name, public_module_type typ)
    in
    Mty_functor (parameter, public_module_type result)
  | (Mty_ident _ | Mty_alias _ | Mty_for_hole) as typ -> typ

and public_signature (signature : Types.signature) : Types.signature =
  List.filter_map signature ~f:(fun item ->
    if
      (not (exported item))
      || String.equal (Ident.name (Types.signature_item_id item)) "_"
    then None
    else
      Some
        (match item with
         | Sig_module (id, presence, declaration, recursive, visibility) ->
           let declaration =
             { declaration with md_type = public_module_type declaration.md_type }
           in
           Types.Sig_module (id, presence, declaration, recursive, visibility)
         | Sig_modtype (id, declaration, visibility) ->
           let declaration =
             { declaration with
               mtd_type = Option.map declaration.mtd_type ~f:public_module_type
             }
           in
           Types.Sig_modtype (id, declaration, visibility)
         | _ -> item))
;;

let signature pipeline t =
  let result = ref None in
  let original declarations =
    List.exists declarations ~f:(fun (declaration : Typedtree.type_declaration) ->
      List.exists t.declarations ~f:(fun source ->
        same_name declaration.typ_name source.Parsetree.ptype_name))
  in
  let select declarations items env =
    let ids =
      List.map declarations ~f:(fun (d : Typedtree.type_declaration) -> d.typ_id)
    in
    let generated =
      List.filter items ~f:(fun item ->
        contains t (signature_location item)
        && not (List.exists ids ~f:(Ident.equal (Types.signature_item_id item))))
      |> public_signature
    in
    if not (List.is_empty generated)
    then
      let module Printtyp = Merlin_analysis.Type_utils.Printtyp in
      let verbosity = (Mpipeline.final_config pipeline).query.verbosity in
      let signature =
        Printtyp.wrap_printing_env ~verbosity env (fun () ->
          Format.asprintf "%a" Printtyp.signature generated)
      in
      result := Some signature
  in
  let default = Ocaml_typing.Tast_iterator.default_iterator in
  let iterator =
    { default with
      structure =
        (fun self structure ->
          let declarations =
            List.find_map structure.Typedtree.str_items ~f:(fun item ->
              match item.str_desc with
              | Tstr_type (_, declarations) when original declarations ->
                Some declarations
              | _ -> None)
          in
          match declarations with
          | Some declarations ->
            select declarations structure.str_type structure.str_final_env
          | None -> default.structure self structure)
    ; signature =
        (fun self signature ->
          let declarations =
            List.find_map signature.Typedtree.sig_items ~f:(fun item ->
              match item.sig_desc with
              | (Tsig_type (_, declarations) | Tsig_typesubst declarations)
                when original declarations -> Some declarations
              | _ -> None)
          in
          match declarations with
          | Some declarations ->
            select declarations signature.sig_type signature.sig_final_env
          | None -> default.signature self signature)
    }
  in
  (match Mtyper.get_typedtree (Mpipeline.typer_result pipeline) with
   | `Implementation structure -> iterator.structure iterator structure
   | `Interface signature -> iterator.signature iterator signature);
  let has_errors =
    Mtyper.get_errors (Mpipeline.typer_result pipeline)
    |> List.exists ~f:(fun error ->
      match Loc.error_of_exn error with
      | Some (`Ok report) -> report.kind = Report_error && contains t report.main.loc
      | Some `Already_displayed | None -> false)
  in
  if has_errors then None else !result
;;
