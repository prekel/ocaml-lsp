open Import
open Parsetree

type declaration =
  { name : string
  ; namespace : string
  ; path : string list option
  ; loc : Loc.t
  ; anchor : int
  }

type document =
  { text : string
  ; declarations : declaration list
  ; complete : bool
  ; opaque : bool
  }

type t = { mutable documents : (string * document) list }

let create () = { documents = [] }
let offsets d = d.loc.loc_start.pos_cnum, d.loc.loc_end.pos_cnum

let parse file text =
  let config =
    let config = Mconfig.initial in
    { config with
      query =
        { config.query with
          filename = Filename.basename file
        ; directory = Filename.dirname file
        }
    }
  in
  let parsed = Merlin_kernel.Mreader.parse config (Msource.make text, None) in
  let declarations = ref [] in
  let prefix = ref (Some []) in
  let anchor = ref None in
  let opaque = ref false in
  let within reference value f =
    let previous = !reference in
    reference := value;
    Fun.protect f ~finally:(fun () -> reference := previous)
  in
  let add namespace (name : string Asttypes.loc) declaration =
    if not name.loc.loc_ghost
    then
      declarations
      := { name = name.txt
         ; namespace
         ; path = Option.map !prefix ~f:(fun path -> path @ [ name.txt ])
         ; loc = name.loc
         ; anchor = declaration.Loc.loc_start.pos_cnum
         }
         :: !declarations
  in
  let iterator =
    { Ast_iterator.default_iterator with
      attribute =
        (fun _ attribute ->
          match attribute.attr_name.txt with
          | "ocaml.doc"
          | "ocaml.text"
          | "doc"
          | "text"
          | "warning"
          | "warnerror"
          | "deprecated"
          | "alert"
          | "inline"
          | "inlined"
          | "tailcall"
          | "local"
          | "unboxed"
          | "boxed" -> ()
          | name when String.is_prefix name ~prefix:"merlin." -> ()
          | _ -> opaque := true)
    ; extension = (fun _ _ -> opaque := true)
    ; expr =
        (fun self expression ->
          within prefix None (fun () ->
            (match expression.pexp_desc with
             | Pexp_struct_item
                 ( { pstr_desc = Pstr_module { pmb_name = { txt = Some name; loc }; _ }
                   ; _
                   }
                 , _ ) -> add "module" { txt = name; loc } expression.pexp_loc
             | _ -> ());
            Ast_iterator.default_iterator.expr self expression))
    ; pat =
        (fun self pattern ->
          (match pattern.ppat_desc with
           | Ppat_var name | Ppat_alias (_, name) ->
             add "value" name (Option.value !anchor ~default:pattern.ppat_loc)
           | _ -> ());
          Ast_iterator.default_iterator.pat self pattern)
    ; value_binding =
        (fun self binding ->
          within anchor (Some binding.pvb_loc) (fun () ->
            Ast_iterator.default_iterator.value_binding self binding))
    ; value_description =
        (fun self value ->
          add "value" value.pval_name value.pval_loc;
          Ast_iterator.default_iterator.value_description self value)
    ; type_declaration =
        (fun self declaration ->
          add "type" declaration.ptype_name declaration.ptype_loc;
          let path =
            Option.map !prefix ~f:(fun path -> path @ [ declaration.ptype_name.txt ])
          in
          within prefix path (fun () ->
            within anchor (Some declaration.ptype_loc) (fun () ->
              Ast_iterator.default_iterator.type_declaration self declaration)))
    ; constructor_declaration =
        (fun self declaration ->
          add
            "constructor"
            declaration.pcd_name
            (Option.value !anchor ~default:declaration.pcd_loc);
          let path =
            match declaration.pcd_args with
            | Pcstr_record _ ->
              Option.map !prefix ~f:(fun path -> path @ [ declaration.pcd_name.txt ])
            | Pcstr_tuple _ -> !prefix
          in
          within prefix path (fun () ->
            Ast_iterator.default_iterator.constructor_declaration self declaration))
    ; label_declaration =
        (fun self declaration ->
          add
            "label"
            declaration.pld_name
            (Option.value !anchor ~default:declaration.pld_loc);
          Ast_iterator.default_iterator.label_declaration self declaration)
    ; type_extension =
        (fun self extension ->
          let path =
            match extension.ptyext_path.txt with
            | Lident name -> Option.map !prefix ~f:(fun path -> path @ [ name ])
            | Ldot _ | Lapply _ -> None
          in
          within prefix path (fun () ->
            Ast_iterator.default_iterator.type_extension self extension))
    ; extension_constructor =
        (fun self declaration ->
          add "extension" declaration.pext_name declaration.pext_loc;
          let path =
            Option.map !prefix ~f:(fun path -> path @ [ declaration.pext_name.txt ])
          in
          within prefix path (fun () ->
            Ast_iterator.default_iterator.extension_constructor self declaration))
    ; class_declaration =
        (fun self declaration ->
          add "class" declaration.pci_name declaration.pci_loc;
          within prefix None (fun () ->
            Ast_iterator.default_iterator.class_declaration self declaration))
    ; class_description =
        (fun self declaration ->
          add "class" declaration.pci_name declaration.pci_loc;
          within prefix None (fun () ->
            Ast_iterator.default_iterator.class_description self declaration))
    ; class_type_declaration =
        (fun self declaration ->
          add "class type" declaration.pci_name declaration.pci_loc;
          within prefix None (fun () ->
            Ast_iterator.default_iterator.class_type_declaration self declaration))
    ; module_binding =
        (fun self binding ->
          self.attributes self binding.pmb_attributes;
          match binding.pmb_name with
          | { txt = None; _ } -> ()
          | { txt = Some name; loc } ->
            add "module" { txt = name; loc } binding.pmb_loc;
            let path = Option.map !prefix ~f:(fun path -> path @ [ name ]) in
            within prefix path (fun () -> self.module_expr self binding.pmb_expr))
    ; module_declaration =
        (fun self declaration ->
          self.attributes self declaration.pmd_attributes;
          match declaration.pmd_name with
          | { txt = None; _ } -> ()
          | { txt = Some name; loc } ->
            add "module" { txt = name; loc } declaration.pmd_loc;
            let path = Option.map !prefix ~f:(fun path -> path @ [ name ]) in
            within prefix path (fun () -> self.module_type self declaration.pmd_type))
    ; module_type_declaration =
        (fun self declaration ->
          add "module type" declaration.pmtd_name declaration.pmtd_loc;
          within prefix None (fun () ->
            Ast_iterator.default_iterator.module_type_declaration self declaration))
    ; module_expr =
        (fun self expression ->
          self.attributes self expression.pmod_attributes;
          match expression.pmod_desc with
          | Pmod_structure structure -> self.structure self structure
          | Pmod_functor (_, body) | Pmod_constraint (body, _) ->
            self.module_expr self body
          | _ -> ())
    ; module_type =
        (fun self typ ->
          self.attributes self typ.pmty_attributes;
          match typ.pmty_desc with
          | Pmty_signature signature -> self.signature self signature
          | Pmty_functor (_, body) | Pmty_with (body, _) -> self.module_type self body
          | _ -> ())
    ; include_declaration = (fun _ _ -> opaque := true)
    ; include_description = (fun _ _ -> opaque := true)
    }
  in
  (match parsed.parsetree with
   | `Implementation structure -> iterator.structure iterator structure
   | `Interface signature -> iterator.signature iterator signature);
  { text
  ; declarations = List.rev !declarations
  ; complete = parsed.lexer_errors = [] && parsed.parser_errors = []
  ; opaque = !opaque
  }
;;

let document t ~file ~text =
  match
    List.find t.documents ~f:(fun (path, doc) ->
      String.equal path file && String.equal doc.text text)
  with
  | Some (_, doc) -> doc
  | None ->
    if String.length text > 1_000_000
    then { text; declarations = []; complete = false; opaque = true }
    else (
      let doc = parse file text in
      t.documents
      <- (file, doc)
         :: List.sub t.documents ~pos:0 ~len:(min 31 (List.length t.documents));
      doc)
;;

let at doc ~offset =
  List.find doc.declarations ~f:(fun declaration ->
    let first, last = offsets declaration in
    first <= offset && offset <= last)
;;

let locate doc (position : Lexing.position) ~name =
  let (`Offset offset) =
    Msource.get_offset
      (Msource.make doc.text)
      (`Logical (position.pos_lnum, position.pos_cnum - position.pos_bol))
  in
  let candidates =
    List.filter doc.declarations ~f:(fun declaration ->
      (declaration.loc.loc_start.pos_cnum = offset || declaration.anchor = offset)
      && Option.value_map name ~default:true ~f:(String.equal declaration.name))
  in
  match candidates with
  | [ declaration ] -> Some declaration
  | _ -> None
;;

let same a b =
  Option.is_some a.path
  && Poly.equal a.path b.path
  && String.equal a.namespace b.namespace
;;

let counterpart ~source ~target declaration =
  if (not source.complete) || (not target.complete) || source.opaque || target.opaque
  then None
  else (
    let last doc = List.find (List.rev doc.declarations) ~f:(same declaration) in
    match last source with
    | Some visible when offsets visible = offsets declaration -> last target
    | _ -> None)
;;
