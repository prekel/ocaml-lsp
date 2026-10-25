open Ppxlib
open Ast_builder.Default

(* Deliberately reuse non-ghost source locations, as older derivers do. In
   particular, the generated parameter ends at the record field's type. *)
let generate declaration =
  match declaration.ptype_kind with
  | Ptype_record (field :: _) ->
    let loc = { declaration.ptype_loc with loc_ghost = false } in
    let field_loc = { field.pld_type.ptyp_loc with loc_ghost = false } in
    let argument = ppat_var ~loc:field_loc { txt = "input"; loc = field_loc } in
    let argument_type =
      ptyp_constr
        ~loc:field_loc
        { txt = Lident "list"; loc = field_loc }
        [ field.pld_type ]
    in
    let body = pexp_ident ~loc:field_loc { txt = Lident "input"; loc = field_loc } in
    let body = pexp_constraint ~loc:field_loc body argument_type in
    let expression = pexp_fun ~loc Nolabel None argument body in
    let pattern = ppat_var ~loc { txt = "source_" ^ declaration.ptype_name.txt; loc } in
    [ pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:pattern ~expr:expression ] ]
  | _ -> []
;;

let mapper =
  object
    inherit Ast_traverse.map as super

    method! structure structure =
      super#structure structure
      |> List.concat_map (fun item ->
        match item.pstr_desc with
        | Pstr_type (recursive, declarations) ->
          let generated = ref [] in
          let declarations =
            List.map
              (fun declaration ->
                 let attributes =
                   List.filter
                     (fun attribute ->
                        if String.equal attribute.attr_name.txt "deriving"
                        then (
                          Attribute.mark_as_handled_manually attribute;
                          generated := !generated @ generate declaration;
                          false)
                        else true)
                     declaration.ptype_attributes
                 in
                 { declaration with ptype_attributes = attributes })
              declarations
          in
          { item with pstr_desc = Pstr_type (recursive, declarations) } :: !generated
        | _ -> [ item ])
  end
;;

let () = Driver.register_transformation "source_locations" ~impl:mapper#structure
