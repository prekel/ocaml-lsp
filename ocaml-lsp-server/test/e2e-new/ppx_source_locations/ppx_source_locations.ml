open Ppxlib
open Ast_builder.Default

(* Deliberately reuse non-ghost source locations, as older derivers do. In
   particular, the generated parameter ends at the record field's type. *)
let generate ?(prefix = "source_") declaration =
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
    let helper = ppat_var ~loc { txt = "local_helper"; loc } in
    let helper_body = pexp_fun ~loc Nolabel None helper (evar ~loc "local_helper") in
    let body = pexp_apply ~loc (evar ~loc "local_helper") [ Nolabel, body ] in
    let body =
      pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat:helper ~expr:helper_body ] body
    in
    let expression = pexp_fun ~loc Nolabel None argument body in
    let name = prefix ^ declaration.ptype_name.txt in
    let pattern = ppat_var ~loc { txt = name; loc } in
    let binding =
      pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:pattern ~expr:expression ]
    in
    let ignored =
      pstr_value
        ~loc
        Nonrecursive
        [ value_binding ~loc ~pat:(ppat_any ~loc) ~expr:(evar ~loc name) ]
    in
    [ binding; ignored ]
  | _ -> []
;;

let multiple attribute =
  match attribute.attr_payload with
  | PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple _; _ }, _); _ } ] -> true
  | _ -> false
;;

let generation declaration attribute =
  let items = generate declaration in
  let items =
    if multiple attribute then items @ generate ~prefix:"second_" declaration else items
  in
  if String.equal declaration.ptype_name.txt "t"
  then (
    let loc = declaration.ptype_loc in
    let module_ =
      pstr_module
        ~loc
        (module_binding
           ~loc
           ~name:{ txt = Some "Generated"; loc }
           ~expr:
             (pmod_structure
                ~loc
                [ pstr_value
                    ~loc
                    Nonrecursive
                    [ value_binding
                        ~loc
                        ~pat:(pvar ~loc "convert")
                        ~expr:(evar ~loc "source_t")
                    ]
                ; pstr_value
                    ~loc
                    Nonrecursive
                    [ value_binding ~loc ~pat:(ppat_any ~loc) ~expr:(evar ~loc "convert")
                    ]
                ]))
    in
    [ pstr_include ~loc (include_infos ~loc (pmod_structure ~loc (items @ [ module_ ]))) ])
  else items
;;

let generated_signature_declarations declaration attribute =
  let loc = declaration.ptype_loc in
  match declaration.ptype_kind with
  | Ptype_record (field :: _) ->
    let list = ptyp_constr ~loc { txt = Lident "list"; loc } [ field.pld_type ] in
    let typ = ptyp_arrow ~loc Nolabel list list in
    let value name =
      psig_value
        ~loc
        (value_description ~loc ~name:{ txt = name; loc } ~type_:typ ~prim:[])
    in
    let items = [ value ("source_" ^ declaration.ptype_name.txt) ] in
    let items =
      if multiple attribute
      then items @ [ value ("second_" ^ declaration.ptype_name.txt) ]
      else items
    in
    if String.equal declaration.ptype_name.txt "t"
    then
      items
      @ [ psig_module
            ~loc
            (module_declaration
               ~loc
               ~name:{ txt = Some "Generated"; loc }
               ~type_:(pmty_signature ~loc [ value "convert" ]))
        ]
    else items
  | _ -> []
;;

let rewrite declarations generate =
  let handled attribute =
    String.equal attribute.attr_name.txt "deriving"
    || String.equal attribute.attr_name.txt "source_locations"
  in
  let attributes =
    List.concat_map
      (fun declaration -> List.filter handled declaration.ptype_attributes)
      declarations
  in
  let generated =
    List.concat_map
      (fun attribute ->
         Attribute.mark_as_handled_manually attribute;
         List.concat_map (fun declaration -> generate declaration attribute) declarations)
      attributes
  in
  let declarations =
    List.map
      (fun declaration ->
         let attributes =
           List.filter
             (fun attribute -> not (handled attribute))
             declaration.ptype_attributes
         in
         { declaration with ptype_attributes = attributes })
      declarations
  in
  declarations, generated
;;

let is_extension name = String.starts_with ~prefix:"source_locations" name.txt
let int_type ~loc = ptyp_constr ~loc { txt = Lident "int"; loc } []

let generated_structure ?(name = "generated") ~loc () =
  [ pstr_value
      ~loc
      Nonrecursive
      [ value_binding ~loc ~pat:(pvar ~loc name) ~expr:(eint ~loc 42) ]
  ]
;;

let generated_signature ?(name = "generated") ~loc () =
  [ psig_value
      ~loc
      (value_description ~loc ~name:{ txt = name; loc } ~type_:(int_type ~loc) ~prim:[])
  ]
;;

let extension_attributes attributes =
  List.filter
    (fun attribute ->
       if String.equal attribute.attr_name.txt "deriving"
       then (
         Attribute.mark_as_handled_manually attribute;
         false)
       else true)
    attributes
;;

let mapper =
  object
    inherit Ast_traverse.map as super

    method! expression expression =
      let expression = super#expression expression in
      let loc = expression.pexp_loc in
      match expression.pexp_desc with
      | Pexp_extension (name, _) when is_extension name -> eint ~loc 42
      | _ ->
        if
          List.exists
            (fun attribute -> String.equal attribute.attr_name.txt "source_locations")
            expression.pexp_attributes
        then (
          List.iter Attribute.mark_as_handled_manually expression.pexp_attributes;
          eint ~loc 42)
        else expression

    method! pattern pattern =
      let pattern = super#pattern pattern in
      match pattern.ppat_desc with
      | Ppat_extension (name, _) when is_extension name ->
        pvar ~loc:pattern.ppat_loc "captured"
      | _ -> pattern

    method! core_type typ =
      let typ = super#core_type typ in
      match typ.ptyp_desc with
      | Ptyp_extension (name, _) when is_extension name -> int_type ~loc:typ.ptyp_loc
      | _ -> typ

    method! module_expr expr =
      let expr = super#module_expr expr in
      match expr.pmod_desc with
      | Pmod_extension (name, _) when is_extension name ->
        pmod_structure ~loc:expr.pmod_loc (generated_structure ~loc:expr.pmod_loc ())
      | _ -> expr

    method! module_type typ =
      let typ = super#module_type typ in
      match typ.pmty_desc with
      | Pmty_extension (name, _) when is_extension name ->
        pmty_signature ~loc:typ.pmty_loc (generated_signature ~loc:typ.pmty_loc ())
      | _ -> typ

    method! class_expr expr =
      let expr = super#class_expr expr in
      match expr.pcl_desc with
      | Pcl_extension (name, _) when is_extension name ->
        pcl_structure
          ~loc:expr.pcl_loc
          { pcstr_self = ppat_any ~loc:expr.pcl_loc; pcstr_fields = [] }
      | _ -> expr

    method! class_type typ =
      let typ = super#class_type typ in
      match typ.pcty_desc with
      | Pcty_extension (name, _) when is_extension name ->
        pcty_signature
          ~loc:typ.pcty_loc
          { pcsig_self = ptyp_any ~loc:typ.pcty_loc; pcsig_fields = [] }
      | _ -> typ

    method! structure structure =
      super#structure structure
      |> List.concat_map (fun item ->
        match item.pstr_desc with
        | Pstr_type (recursive, declarations) ->
          let declarations, generated = rewrite declarations generation in
          { item with pstr_desc = Pstr_type (recursive, declarations) } :: generated
        | Pstr_typext extension ->
          let extension =
            { extension with
              ptyext_attributes = extension_attributes extension.ptyext_attributes
            }
          in
          { item with pstr_desc = Pstr_typext extension }
          :: generated_structure ~name:"generated_extension" ~loc:item.pstr_loc ()
        | Pstr_extension ((name, payload), _) when is_extension name ->
          (match payload with
           | PStr (_ :: _ as items) -> items
           | _ -> generated_structure ~loc:item.pstr_loc ())
        | Pstr_attribute attribute when is_extension attribute.attr_name ->
          Attribute.mark_as_handled_manually attribute;
          generated_structure ~name:"floating" ~loc:item.pstr_loc ()
        | _ -> [ item ])

    method! signature signature =
      super#signature signature
      |> List.concat_map (fun item ->
        match item.psig_desc with
        | Psig_type (recursive, declarations) ->
          let declarations, generated =
            rewrite declarations generated_signature_declarations
          in
          { item with psig_desc = Psig_type (recursive, declarations) } :: generated
        | Psig_typext extension ->
          let extension =
            { extension with
              ptyext_attributes = extension_attributes extension.ptyext_attributes
            }
          in
          { item with psig_desc = Psig_typext extension }
          :: generated_signature ~name:"generated_extension" ~loc:item.psig_loc ()
        | Psig_extension ((name, _), _) when is_extension name ->
          generated_signature ~loc:item.psig_loc ()
        | Psig_attribute attribute when is_extension attribute.attr_name ->
          Attribute.mark_as_handled_manually attribute;
          generated_signature ~name:"floating" ~loc:item.psig_loc ()
        | _ -> [ item ])
  end
;;

let () =
  Driver.register_transformation
    "source_locations"
    ~impl:mapper#structure
    ~intf:mapper#signature
;;
