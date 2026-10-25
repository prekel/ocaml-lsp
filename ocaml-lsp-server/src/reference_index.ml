open Import
open Typedtree
module H = Stdlib.Hashtbl
module B = Merlin_analysis.Browse_tree
module Uid = Ocaml_typing.Shape.Uid
module Mocaml = Merlin_kernel.Mocaml

type symbol =
  { namespace : string
  ; unit_name : string option
  ; filename : string
  ; first : int
  ; last : int
  ; local : bool
  }

type occurrence =
  { symbol : symbol
  ; first : int
  ; last : int
  ; declaration : bool
  }

(* Callable identities and execution edges are separate from [file.edges],
   which only joins alternate declaration anchors. No typed trees escape collect. *)
type callable =
  { identity : symbol
  ; name : string
  ; first : int
  ; last : int
  ; selection_first : int
  ; selection_last : int
  ; body : bool
  }

type call =
  { caller : symbol
  ; callee : symbol
  ; first : int
  ; last : int
  }

type file =
  { source : string
  ; artifact : string option
  ; interface_digest : string option
  ; unit_name : string
  ; builddir : string
  ; loadpath : string list
  ; imports : (string * string option) list
  ; occurrences : occurrence list
  ; exports : (string * symbol) list
  ; edges : (symbol * symbol) list
  ; callables : callable list
  ; calls : call list
  ; ambiguous : (int * int) list
  ; text : string
  ; digest : string option
  }

type stamp = float * int * int

type shape_info =
  { shape : Ocaml_typing.Shape.t option
  ; locations : Loc.t Uid.Tbl.t
  ; class_locations : Loc.t Uid.Tbl.t
  ; members : ((string * string) * (Uid.t * Loc.t)) list Uid.Tbl.t
  }

type t =
  { artifacts : (string, stamp * file) H.t
  ; sources : (string, stamp * string) H.t
  ; shapes : (string, stamp * shape_info) H.t
  ; support : (string, stamp * string * file) H.t
  ; metadata : (string, stamp * string * string list) H.t
  ; mutable notice : string
  }

let create () =
  { artifacts = H.create 128
  ; sources = H.create 128
  ; shapes = H.create 128
  ; support = H.create 64
  ; metadata = H.create 128
  ; notice = ""
  }
;;

let canonical path =
  try Unix.realpath path with
  | Unix.Unix_error _ -> path
;;

let absolute directory path =
  if Filename.is_relative path then Filename.concat directory path else path
;;

let under root path =
  String.equal root path || String.is_prefix path ~prefix:(root ^ Filename.dir_sep)
;;

let stem path =
  try Filename.chop_extension path with
  | Invalid_argument _ -> path
;;

let stat path =
  let s = Unix.stat path in
  s.Unix.st_mtime, s.st_size, s.st_ino
;;

let read path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

let digest text = Digest.to_hex (Digest.string text)

let uid_unit = function
  | Uid.Item { comp_unit; _ } | Compilation_unit comp_unit -> Some comp_unit
  | Internal | Predef _ -> None
;;

let symbol ?uid ~local source namespace (loc : Loc.t) =
  { namespace
  ; unit_name = Option.bind uid ~f:uid_unit
  ; filename = (if local then source else loc.loc_start.pos_fname)
  ; first = loc.loc_start.pos_cnum
  ; last = loc.loc_end.pos_cnum
  ; local
  }
;;

exception Cancelled

let check_cancel cancel =
  if Option.value_map cancel ~default:false ~f:Fiber.Cancel.fired then raise Cancelled
;;

let shape_info (cmt : Cmt_format.cmt_infos) =
  let classes = Uid.Tbl.copy cmt.cmt_uid_to_loc in
  let members = Uid.Tbl.create 16 in
  let default = Ocaml_typing.Tast_iterator.default_iterator in
  let iterator =
    { default with
      type_declaration =
        (fun iterator d ->
          let fields =
            match d.typ_type.type_kind with
            | Types.Type_record (fields, _) ->
              List.map fields ~f:(fun (d : Types.label_declaration) ->
                ("label", Ident.name d.ld_id), (d.ld_uid, d.ld_loc))
            | Type_variant (constructors, _) ->
              List.map constructors ~f:(fun (d : Types.constructor_declaration) ->
                ("constructor", Ident.name d.cd_id), (d.cd_uid, d.cd_loc))
            | _ -> []
          in
          Uid.Tbl.replace members d.typ_type.type_uid fields;
          default.type_declaration iterator d)
    ; class_declaration =
        (fun iterator d ->
          Uid.Tbl.replace classes d.ci_decl.cty_uid d.ci_decl.cty_loc;
          default.class_declaration iterator d)
    ; class_description =
        (fun iterator d ->
          Uid.Tbl.replace classes d.ci_decl.cty_uid d.ci_decl.cty_loc;
          default.class_description iterator d)
    ; class_type_declaration =
        (fun iterator d ->
          Uid.Tbl.replace classes d.ci_type_decl.clty_uid d.ci_type_decl.clty_loc;
          default.class_type_declaration iterator d)
    }
  in
  (match cmt.cmt_annots with
   | Implementation structure -> iterator.structure iterator structure
   | Interface signature -> iterator.signature iterator signature
   | _ -> ());
  { shape = cmt.cmt_impl_shape
  ; locations = cmt.cmt_uid_to_loc
  ; class_locations = classes
  ; members
  }
;;

let read_shape cache path =
  let path = canonical path in
  let stamp = stat path in
  match H.find_opt cache.shapes path with
  | Some (previous, info) when Poly.equal previous stamp -> info
  | _ ->
    let info = shape_info (Cmt_format.read_cmt path) in
    if Poly.equal stamp (stat path) then H.replace cache.shapes path (stamp, info);
    info
;;

let build_roots roots paths =
  let custom =
    List.filter_map paths ~f:(fun path ->
      let rec find directory =
        let parent = Filename.dirname directory in
        if String.equal parent directory
        then None
        else if Sys.file_exists (Filename.concat parent ".db")
        then Some directory
        else find parent
      in
      find path)
  in
  custom
  @ List.concat_map roots ~f:(fun root ->
    let prefix = root ^ "/_build/" in
    let active =
      List.filter_map paths ~f:(fun path ->
        match String.drop_prefix path ~prefix with
        | None -> None
        | Some rest ->
          Option.map
            (List.hd (String.split rest ~on:'/'))
            ~f:(Filename.concat (root ^ "/_build")))
    in
    if active = [] && custom = []
    then [ Filename.concat root "_build/default" ]
    else if active = [] && not (List.exists custom ~f:(under root))
    then [ Filename.concat root "_build/default" ]
    else active)
  |> List.sort_uniq ~compare:Poly.compare
;;

let collect
      ~cancel
      ~root_env
      ~shape_roots
      ~read_shape
      ~source
      ~unit_name
      ~builddir
      ~loadpath
      ~imports
      ~text
      ~digest:source_digest
      tree
      signature
  =
  let occurrences = ref []
  and exports = ref []
  and edges = ref []
  and ambiguous = ref [] in
  let locals = H.create 64
  and implementations = H.create 16 in
  let rec implementation expression =
    match expression.mod_desc with
    | Tmod_structure structure -> Some structure.str_type
    | Tmod_constraint (expression, _, _, _) -> implementation expression
    | _ -> None
  in
  let local namespace loc = symbol ~local:true source namespace loc in
  let add declaration sym (loc : Loc.t) =
    let first = loc.loc_start.pos_cnum
    and last = loc.loc_end.pos_cnum in
    if (not loc.loc_ghost) && first >= 0 && last > first && last <= String.length text
    then occurrences := { symbol = sym; first; last; declaration } :: !occurrences
  in
  let bind namespace id anchor name =
    let sym = local namespace anchor in
    (match H.find_opt locals (namespace, Ident.unique_name id) with
     | None -> H.add locals (namespace, Ident.unique_name id) sym
     | Some previous -> edges := (previous, sym) :: !edges);
    add true sym name
  in
  let find_local namespace path =
    match path with
    | Path.Pident id -> H.find_opt locals (namespace, Ident.unique_name id)
    | _ -> None
  in
  let descriptor namespace path uid loc =
    match find_local namespace path with
    | Some sym -> sym
    | None -> symbol ~uid ~local:false source namespace loc
  in
  let rec walk f node =
    check_cancel cancel;
    if not (Browse_raw.has_attr ~name:"merlin.hide" node.B.t_node)
    then (
      f node;
      List.iter (Lazy.force node.t_children) ~f:(walk f))
  in
  walk
    (fun node ->
       match node.B.t_node with
       | Pattern { pat_desc = Tpat_var (id, name); _ }
       | Pattern { pat_desc = Tpat_alias (_, id, name); _ } ->
         bind "value" id name.loc name.loc
       | Value_description d -> bind "value" d.val_id d.val_val.val_loc d.val_name.loc
       | Type_declaration d -> bind "type" d.typ_id d.typ_type.type_loc d.typ_name.loc
       | Constructor_declaration d -> bind "constructor" d.cd_id d.cd_loc d.cd_name.loc
       | Label_declaration d -> bind "label" d.ld_id d.ld_loc d.ld_name.loc
       | Extension_constructor d ->
         bind "extension" d.ext_id d.ext_type.ext_loc d.ext_name.loc
       | Module_binding_name { mb_id = Some id; mb_name; mb_loc; _ } ->
         bind "module" id mb_loc mb_name.loc
       | Module_declaration_name { md_id = Some id; md_name; md_loc; _ } ->
         bind "module" id md_loc md_name.loc
       | Module_type_declaration_name d ->
         bind "module-type" d.mtd_id d.mtd_loc d.mtd_name.loc;
         ambiguous
         := (d.mtd_loc.loc_start.pos_cnum, d.mtd_loc.loc_end.pos_cnum) :: !ambiguous
       | Module_binding { mb_id = Some id; mb_expr; _ } ->
         Option.iter (implementation mb_expr) ~f:(fun signature ->
           H.replace implementations (Ident.unique_name id) signature)
       | Class_declaration d ->
         bind "class" d.ci_id_class d.ci_decl.cty_loc d.ci_id_name.loc
       | Class_description d ->
         bind "class" d.ci_id_class d.ci_decl.cty_loc d.ci_id_name.loc
       | Class_type_declaration d ->
         bind "class-type" d.ci_id_class_type d.ci_type_decl.clty_loc d.ci_id_name.loc
       | Module_expr { mod_desc = Tmod_functor (Named (Some id, name, _), _); mod_loc; _ }
       | Module_type
           { mty_desc = Tmty_functor (Named (Some id, name, _), _); mty_loc = mod_loc; _ }
         ->
         bind "module" id name.loc name.loc;
         ambiguous := (mod_loc.loc_start.pos_cnum, mod_loc.loc_end.pos_cnum) :: !ambiguous
       | Module_expr { mod_desc = Tmod_functor _; mod_loc; _ }
       | Module_type { mty_desc = Tmty_functor _; mty_loc = mod_loc; _ } ->
         ambiguous := (mod_loc.loc_start.pos_cnum, mod_loc.loc_end.pos_cnum) :: !ambiguous
       | Expression { exp_desc = Texp_letmodule (Some id, name, _, _, _); _ } ->
         bind "module" id name.loc name.loc
       | Expression { exp_desc = Texp_for (id, pattern, _, _, _, _); _ } ->
         bind "value" id pattern.ppat_loc pattern.ppat_loc
       | _ -> ())
    tree;
  let env env = Ocaml_typing.Envaux.env_of_only_summary env in
  (* Resolve qualified accesses through implementation shapes, rather than
     reusing the UID of a structural signature component. In 4.14 F.x and G.x
     can carry the same declaration UID when both modules implement S. *)
  let module_paths = H.create 16
  and units = H.create 16 in
  let rec module_exports prefix signature =
    let seen = H.create 8 in
    List.iter (List.rev signature) ~f:(function
      | Types.Sig_module (id, _, d, _, Types.Exported)
        when not (H.mem seen (Ident.name id)) ->
        H.add seen (Ident.name id) ();
        let path = prefix @ [ Ident.name id ] in
        H.replace module_paths (Ident.unique_name id) path;
        (match d.md_type with
         | Mty_signature signature -> module_exports path signature
         | _ -> ())
      | _ -> ())
  in
  module_exports [] signature;
  let unit_info name =
    match H.find_opt units name with
    | Some result -> result
    | None ->
      let result =
        List.find_map loadpath ~f:(fun directory ->
          List.find_map
            [ name ^ ".cmt"; Stdlib.String.uncapitalize_ascii name ^ ".cmt" ]
            ~f:(fun filename ->
              let path = Filename.concat directory filename in
              if List.exists shape_roots ~f:(fun root -> under root (canonical path))
              then (
                try Some (read_shape path) with
                | _ -> None)
              else None))
      in
      H.add units name result;
      result
  in
  let module Reduce =
    Ocaml_typing.Shape.Make_reduce (struct
      type env = Env.t

      let fuel = 10

      let read_unit_shape ~unit_name =
        Option.bind (unit_info unit_name) ~f:(fun cmt -> cmt.shape)
      ;;

      let find_shape env id =
        Env.shape_of_path
          ~namespace:Ocaml_typing.Shape.Sig_component_kind.Module
          env
          (Path.Pident id)
      ;;
    end)
  in
  let rec persistent_module environment path =
    let path = Env.normalize_module_path None environment path in
    match path with
    | Path.Pident id when Ident.persistent id -> path
    | Pident id ->
      let parts = H.find module_paths (Ident.unique_name id) in
      List.fold_left
        parts
        ~init:(Path.Pident (Ident.create_persistent unit_name))
        ~f:(fun parent name -> Path.Pdot (parent, name))
    | Pdot (parent, name) -> Pdot (persistent_module environment parent, name)
    | Papply _ -> raise Not_found
  in
  let shaped_uid namespace environment path =
    let kind =
      match namespace with
      | "value" -> Some Ocaml_typing.Shape.Sig_component_kind.Value
      | "type" -> Some Type
      | "module" -> Some Module
      | "module-type" -> Some Module_type
      | "extension" -> Some Extension_constructor
      | "class" -> Some Class
      | "class-type" -> Some Class_type
      | _ -> None
    in
    Option.bind kind ~f:(fun kind ->
      try
        let environment = env environment in
        let path =
          match path with
          | Path.Pdot (parent, name) ->
            Path.Pdot (persistent_module environment parent, name)
          | _ when namespace = "module" -> persistent_module environment path
          | _ -> raise Not_found
        in
        let shape =
          Env.shape_of_path ~namespace:kind environment path
          |> Reduce.weak_reduce environment
        in
        let open Option.O in
        let* uid = shape.uid in
        let* owner = uid_unit uid in
        let* cmt = unit_info owner in
        Some (uid, cmt)
      with
      | Not_found | Ocaml_typing.Envaux.Error _ -> None)
  in
  let shaped namespace environment path =
    let open Option.O in
    let* uid, cmt = shaped_uid namespace environment path in
    let locations =
      if namespace = "class" || namespace = "class-type"
      then cmt.class_locations
      else cmt.locations
    in
    let* loc = Uid.Tbl.find_opt locations uid in
    Some (symbol ~uid ~local:false source namespace loc)
  in
  (* A label/constructor UID may belong to a shared module-type template.
     Resolve its enclosing type first, then select that type's actual member. *)
  let member namespace environment parent name uid loc =
    let resolved =
      let open Option.O in
      match Types.get_desc parent with
      | Tconstr (path, _, _) ->
        let* parent, cmt = shaped_uid "type" environment path in
        let* members = Uid.Tbl.find_opt cmt.members parent in
        let* uid, loc = List.assoc members (namespace, name) in
        Some (symbol ~uid ~local:false source namespace loc)
      | _ -> None
    in
    Option.value resolved ~default:(symbol ~uid ~local:false source namespace loc)
  in
  let original_descriptor = descriptor in
  let descriptor namespace environment path uid loc =
    match find_local namespace path with
    | Some symbol -> symbol
    | None ->
      Option.value
        (shaped namespace environment path)
        ~default:(original_descriptor namespace path uid loc)
  in
  (* Validate against the typed identifier before producing token ranges.
     Unsupported syntax is skipped rather than assigning an entire expression
     to its last component. *)
  let components (name : Longident.t Loc.loc) =
    let names =
      try Longident.flatten name.txt with
      | _ -> []
    in
    let limit = min (String.length text) name.loc.loc_end.pos_cnum in
    let rec comment depth i =
      if i + 1 >= limit
      then limit
      else if text.[i] = '(' && text.[i + 1] = '*'
      then comment (depth + 1) (i + 2)
      else if text.[i] = '*' && text.[i + 1] = ')'
      then if depth = 1 then skip (i + 2) else comment (depth - 1) (i + 2)
      else comment depth (i + 1)
    and skip i =
      if i + 1 < limit && text.[i] = '(' && text.[i + 1] = '*'
      then comment 1 (i + 2)
      else if
        i < limit
        &&
        match text.[i] with
        | ' ' | '\t' | '\r' | '\n' | '.' | '(' | ')' -> true
        | _ -> false
      then skip (i + 1)
      else i
    in
    let rec loop i acc = function
      | [] -> Some (List.rev acc)
      | word :: words ->
        let i = skip i
        and len = String.length word in
        if i >= 0 && i + len <= limit && String.equal (String.sub text ~pos:i ~len) word
        then (
          let start = { name.loc.loc_start with pos_cnum = i } in
          let stop = { start with pos_cnum = i + len } in
          let loc = { name.loc with loc_start = start; loc_end = stop } in
          loop (i + len) (loc :: acc) words)
        else None
    in
    loop name.loc.loc_start.pos_cnum [] names
  in
  let module_symbol env path =
    match find_local "module" path with
    | Some symbol -> Some symbol
    | None ->
      let d = Env.find_module path (env |> Ocaml_typing.Envaux.env_of_only_summary) in
      (match d.md_uid with
       | Uid.Compilation_unit name ->
         Some
           { namespace = "unit"
           ; unit_name = Some name
           ; filename = ""
           ; first = -1
           ; last = -1
           ; local = false
           }
       | uid ->
         Some
           (Option.value
              (shaped "module" env path)
              ~default:(symbol ~uid ~local:false source "module" d.md_loc)))
  in
  let module_prefixes environment name =
    match components name with
    | None -> ()
    | Some locations ->
      let names =
        try Longident.flatten name.txt with
        | _ -> []
      in
      let rec loop prefix names locations =
        match names, locations with
        | _ :: [], _ :: [] | [], _ | _, [] -> ()
        | word :: names, location :: locations ->
          let ident =
            match prefix with
            | None -> Longident.Lident word
            | Some prefix -> Ldot (prefix, word)
          in
          (try
             let path, _ = Env.find_module_by_name ident (env environment) in
             Option.iter (module_symbol environment path) ~f:(fun symbol ->
               add false symbol location)
           with
           | Not_found | Ocaml_typing.Envaux.Error _ -> ());
          loop (Some ident) names locations
      in
      loop None names locations
  in
  let use environment name sym =
    Option.iter (Option.bind (components name) ~f:List.last) ~f:(add false sym);
    module_prefixes environment name
  in
  walk
    (fun node ->
       let attempt f =
         try f () with
         | Not_found
         | Ocaml_typing.Envaux.Error _
         | Ocaml_typing.Env.Error _
         | Ocaml_typing.Persistent_env.Error _
         | Ocaml_typing.Magic_numbers.Cmi.Error _
         | Sys_error _
         | Unix.Unix_error _ -> ()
       in
       attempt (fun () ->
         match node.B.t_node with
         | Expression { exp_desc = Texp_ident (path, name, d); exp_env; _ } ->
           use exp_env name (descriptor "value" exp_env path d.val_uid d.val_loc)
         | Expression { exp_desc = Texp_construct (name, d, _); exp_env; _ }
         | Pattern { pat_desc = Tpat_construct (name, d, _, _); pat_env = exp_env; _ } ->
           let namespace =
             match d.cstr_tag with
             | Cstr_extension _ -> "extension"
             | _ -> "constructor"
           in
           let target =
             match d.cstr_tag with
             | Cstr_extension (path, _) ->
               descriptor namespace exp_env path d.cstr_uid d.cstr_loc
             | _ -> member namespace exp_env d.cstr_res d.cstr_name d.cstr_uid d.cstr_loc
           in
           use exp_env name target
         | Record_field (parent, d, name) ->
           let environment =
             match parent with
             | `Expression e -> e.exp_env
             | `Pattern p -> p.pat_env
           in
           use
             environment
             name
             (member "label" environment d.lbl_res d.lbl_name d.lbl_uid d.lbl_loc)
         | Core_type { ctyp_desc = Ttyp_constr (path, name, _); ctyp_env; _ } ->
           let d = Env.find_type path (env ctyp_env) in
           use ctyp_env name (descriptor "type" ctyp_env path d.type_uid d.type_loc)
         | Module_expr { mod_desc = Tmod_ident (path, name); mod_env; _ } ->
           Option.iter (module_symbol mod_env path) ~f:(use mod_env name)
         | Module_type { mty_desc = Tmty_ident (path, name); mty_env; _ } ->
           let d = Env.find_modtype path (env mty_env) in
           use mty_env name (descriptor "module-type" mty_env path d.mtd_uid d.mtd_loc)
         | Open_description { open_expr = path, name; open_env; _ } ->
           Option.iter (module_symbol open_env path) ~f:(use open_env name)
         | Expression { exp_desc = Texp_new (path, name, d); exp_env; _ } ->
           use exp_env name (descriptor "class" exp_env path d.cty_uid d.cty_loc)
         | Class_expr { cl_desc = Tcl_ident (path, name, _); cl_env; _ } ->
           let d = Env.find_class path (env cl_env) in
           use cl_env name (descriptor "class" cl_env path d.cty_uid d.cty_loc)
         | Class_type { cltyp_desc = Tcty_constr (path, name, _); cltyp_env; _ }
         | Core_type { ctyp_desc = Ttyp_class (path, name, _); ctyp_env = cltyp_env; _ }
           ->
           let d = Env.find_cltype path (env cltyp_env) in
           use
             cltyp_env
             name
             (descriptor "class-type" cltyp_env path d.clty_uid d.clty_loc)
         | _ -> ()))
    tree;
  let callables = ref []
  and calls = ref [] in
  let leading = H.create 32
  and static_bindings = H.create 32 in
  let regions = ref [] in
  let function_type ty =
    match Types.get_desc ty with
    | Tarrow _ -> true
    | _ -> false
  in
  let callable identity name (loc : Loc.t) (selection : Loc.t) body =
    let first = max 0 loc.loc_start.pos_cnum in
    let last = min (String.length text) loc.loc_end.pos_cnum in
    let selection_first = selection.loc_start.pos_cnum in
    let selection_last = selection.loc_end.pos_cnum in
    if
      first <= selection_first
      && selection_first < selection_last
      && selection_last <= last
    then (
      let c = { identity; name; first; last; selection_first; selection_last; body } in
      callables := c :: !callables;
      if body then regions := (loc, identity) :: !regions)
  in
  let rec mark_leading identity expression =
    match expression.exp_desc with
    | Texp_function { cases; _ } ->
      H.replace leading expression.exp_loc.loc_start.pos_cnum identity;
      List.iter cases ~f:(fun case -> mark_leading identity case.c_rhs)
    | _ -> ()
  in
  let named_pattern pattern =
    match pattern.pat_desc with
    | Tpat_var (id, name) | Tpat_alias (_, id, name) ->
      Option.map (find_local "value" (Path.Pident id)) ~f:(fun identity -> identity, name)
    | _ -> None
  in
  let anonymous expression = local "lambda" expression.exp_loc in
  walk
    (fun node ->
       match node.B.t_node with
       | Value_binding vb when function_type vb.vb_pat.pat_type ->
         Option.iter (named_pattern vb.vb_pat) ~f:(fun (identity, name) ->
           H.replace static_bindings identity ();
           let body =
             match vb.vb_expr.exp_desc with
             | Texp_function _ -> true
             | _ -> false
           in
           callable identity name.txt vb.vb_loc name.loc body;
           if body
           then (
             (* The binding name is outside its execution region. *)
             regions
             := (vb.vb_expr.exp_loc, identity)
                :: List.filter !regions ~f:(fun (_, sym) -> not (Poly.equal sym identity));
             mark_leading identity vb.vb_expr))
       | Value_description d when function_type d.val_val.val_type ->
         Option.iter (find_local "value" (Path.Pident d.val_id)) ~f:(fun identity ->
           H.replace static_bindings identity ();
           callable identity d.val_name.txt d.val_loc d.val_name.loc false)
       | Pattern ({ pat_desc = Tpat_var _; _ } as p) when function_type p.pat_type ->
         Option.iter (named_pattern p) ~f:(fun (identity, name) ->
           if not (H.mem static_bindings identity)
           then callable identity name.txt p.pat_loc name.loc false)
       | Expression ({ exp_desc = Texp_function _; exp_loc; _ } as expression)
         when not (H.mem leading exp_loc.loc_start.pos_cnum) ->
         let identity = anonymous expression in
         let first = exp_loc.loc_start.pos_cnum in
         let selection =
           { exp_loc with
             loc_end =
               { exp_loc.loc_start with
                 pos_cnum = min exp_loc.loc_end.pos_cnum (first + 3)
               }
           }
         in
         callable
           identity
           (sprintf "<fun at %d>" exp_loc.loc_start.pos_lnum)
           exp_loc
           selection
           true;
         mark_leading identity expression
       | Structure structure ->
         let loc = Mbrowse.node_loc (Browse_raw.Structure structure) in
         if loc.loc_end.pos_cnum > loc.loc_start.pos_cnum
         then (
           let identity = local "module-initializer" loc in
           let selection =
             { loc with
               loc_end =
                 { loc.loc_start with
                   pos_cnum = min loc.loc_end.pos_cnum (loc.loc_start.pos_cnum + 1)
                 }
             }
           in
           callable identity (unit_name ^ ".<init>") loc selection true)
       | _ -> ())
    tree;
  let owner loc =
    List.filter !regions ~f:(fun (region, _) ->
      region.Loc.loc_start.pos_cnum <= loc.Loc.loc_start.pos_cnum
      && loc.loc_end.pos_cnum <= region.loc_end.pos_cnum)
    |> List.sort ~compare:(fun (a, _) (b, _) ->
      Int.compare
        (a.Loc.loc_end.pos_cnum - a.loc_start.pos_cnum)
        (b.Loc.loc_end.pos_cnum - b.loc_start.pos_cnum))
    |> List.hd
    |> Option.map ~f:snd
  in
  let rec target expression =
    match expression.exp_desc with
    | Texp_ident (path, name, d) ->
      let identity = descriptor "value" expression.exp_env path d.val_uid d.val_loc in
      if identity.local && not (H.mem static_bindings identity)
      then None
      else
        Option.map
          (Option.bind (components name) ~f:List.last)
          ~f:(fun loc -> identity, loc)
    | Texp_apply (fn, _) -> target fn
    | Texp_function _ -> Some (anonymous expression, expression.exp_loc)
    | _ -> None
  in
  walk
    (fun node ->
       match node.B.t_node with
       | Expression { exp_desc = Texp_apply (fn, args); exp_loc; _ } ->
         (try
            let fn =
              match fn.exp_desc, args with
              | ( Texp_ident (_, _, { val_kind = Val_prim primitive; _ })
                , [ (_, Some left); (_, Some right) ] ) ->
                (match primitive.prim_name with
                 | "%revapply" -> right
                 | "%apply" -> left
                 | _ -> fn)
              | _ -> fn
            in
            match owner exp_loc, target fn with
            | Some caller, Some (callee, loc) ->
              let first = loc.loc_start.pos_cnum
              and last = loc.loc_end.pos_cnum in
              if first >= 0 && first < last && last <= String.length text
              then calls := { caller; callee; first; last } :: !calls
            | _ -> ()
          with
          | Not_found | Ocaml_typing.Envaux.Error _ | Env.Error _ -> ())
       | _ -> ())
    tree;
  let export namespace prefix parent id uid loc =
    let key = prefix ^ namespace ^ ":" ^ Ident.name id in
    let local = original_descriptor namespace (Path.Pident id) uid loc in
    let template =
      local.local
      && List.exists !ambiguous ~f:(fun (first, last) ->
        first <= local.first
        && local.last <= last
        && not (first = local.first && local.last = last))
    in
    let path =
      match parent with
      | None -> Path.Pident id
      | Some parent -> Path.Pdot (parent, Ident.name id)
    in
    let sym = if template then shaped namespace root_env path else Some local in
    Option.iter sym ~f:(fun sym -> exports := (key, sym) :: !exports)
  in
  let rec signature_exports prefix parent signature =
    (* str_type may retain an earlier binding hidden by a later declaration.
       Only the last visible component is exported; joining both would merge
       distinct lexical bindings that happen to have the same spelling. *)
    let seen = H.create 16 in
    let signature =
      List.rev signature
      |> List.filter ~f:(fun item ->
        let namespace, id, visibility =
          match item with
          | Types.Sig_value (id, _, visibility) -> "value", id, visibility
          | Sig_type (id, _, _, visibility) -> "type", id, visibility
          | Sig_module (id, _, _, _, visibility) -> "module", id, visibility
          | Sig_modtype (id, _, visibility) -> "module-type", id, visibility
          | Sig_typext (id, _, _, visibility) -> "extension", id, visibility
          | Sig_class (id, _, _, visibility) -> "class", id, visibility
          | Sig_class_type (id, _, _, visibility) -> "class-type", id, visibility
        in
        let key = namespace, Ident.name id in
        if visibility = Types.Hidden || H.mem seen key
        then false
        else (
          H.add seen key ();
          true))
    in
    List.iter signature ~f:(function
      | Types.Sig_value (id, d, _) -> export "value" prefix parent id d.val_uid d.val_loc
      | Sig_type (id, d, _, _) ->
        export "type" prefix parent id d.type_uid d.type_loc;
        let nested = prefix ^ "type:" ^ Ident.name id ^ "/" in
        (match d.type_kind with
         | Type_variant (constructors, _) ->
           List.iter constructors ~f:(fun (d : Types.constructor_declaration) ->
             export "constructor" nested parent d.cd_id d.cd_uid d.cd_loc)
         | Type_record (labels, _) ->
           List.iter labels ~f:(fun (d : Types.label_declaration) ->
             export "label" nested parent d.ld_id d.ld_uid d.ld_loc)
         | _ -> ())
      | Sig_module (id, _, d, _, _) ->
        export "module" prefix parent id d.md_uid d.md_loc;
        let signature =
          match H.find_opt implementations (Ident.unique_name id), d.md_type with
          | Some signature, _ | None, Mty_signature signature -> Some signature
          | _ -> None
        in
        Option.iter signature ~f:(fun signature ->
          let path =
            match parent with
            | None -> Path.Pident id
            | Some parent -> Path.Pdot (parent, Ident.name id)
          in
          signature_exports
            (prefix ^ "module:" ^ Ident.name id ^ "/")
            (Some path)
            signature)
      | Sig_modtype (id, d, _) ->
        export "module-type" prefix parent id d.mtd_uid d.mtd_loc
      | Sig_typext (id, d, _, _) ->
        export "extension" prefix parent id d.ext_uid d.ext_loc
      | Sig_class (id, d, _, _) -> export "class" prefix parent id d.cty_uid d.cty_loc
      | Sig_class_type (id, d, _, _) ->
        export "class-type" prefix parent id d.clty_uid d.clty_loc)
  in
  signature_exports "" None signature;
  { source
  ; artifact = None
  ; interface_digest = None
  ; unit_name
  ; builddir
  ; loadpath
  ; imports
  ; occurrences = !occurrences
  ; exports = !exports
  ; edges = !edges
  ; callables = !callables
  ; calls = !calls
  ; ambiguous = !ambiguous
  ; text
  ; digest = source_digest
  }
;;

let artifacts roots cancel =
  let files = ref []
  and seen = H.create 32 in
  let rec walk directory =
    check_cancel cancel;
    try
      let directory = canonical directory in
      if not (H.mem seen directory)
      then (
        H.add seen directory ();
        Array.iter (Sys.readdir directory) ~f:(fun name ->
          let path = Filename.concat directory name in
          match (Unix.lstat path).Unix.st_kind with
          | S_DIR -> walk path
          | S_REG
            when String.is_suffix name ~suffix:".cmt"
                 || String.is_suffix name ~suffix:".cmti" -> files := path :: !files
          | _ -> ()))
    with
    | Unix.Unix_error _ | Sys_error _ -> ()
  in
  List.iter roots ~f:walk;
  !files
;;

let source_of_cmt roots cmt =
  let paths =
    (match cmt.Cmt_format.cmt_annots with
     | Implementation tree ->
       [ (Mbrowse.node_loc (Browse_raw.Structure tree)).loc_start.pos_fname ]
     | Interface tree ->
       [ (Mbrowse.node_loc (Browse_raw.Signature tree)).loc_start.pos_fname ]
     | _ -> [])
    @ Option.to_list cmt.cmt_sourcefile
  in
  let candidates =
    List.concat_map paths ~f:(fun path ->
      absolute cmt.cmt_builddir path :: List.map roots ~f:(fun root -> absolute root path))
  in
  List.find_map candidates ~f:(fun path ->
    let path = canonical path in
    if
      List.exists roots ~f:(fun root -> under root path)
      && (not
            (List.exists roots ~f:(fun root -> under (Filename.concat root "_opam") path)))
      && (String.is_suffix path ~suffix:".ml" || String.is_suffix path ~suffix:".mli")
      && Sys.file_exists path
    then Some path
    else None)
;;

let load ?source_override ?loadpath_override cache roots ~shape_roots artifact cancel =
  let cmt = Cmt_format.read_cmt artifact in
  H.replace cache.shapes (canonical artifact) (stat artifact, shape_info cmt);
  let open Option.O in
  let* source =
    match source_override with
    | Some source -> Some source
    | None -> source_of_cmt roots cmt
  in
  let* tree, signature, root_env =
    match cmt.cmt_annots with
    | Implementation tree ->
      Some (B.of_node (Browse_raw.Structure tree), tree.str_type, tree.str_final_env)
    | Interface tree ->
      Some (B.of_node (Browse_raw.Signature tree), tree.sig_type, tree.sig_final_env)
    | _ -> None
  in
  let text = read source in
  let loadpath = List.map cmt.cmt_loadpath ~f:(absolute cmt.cmt_builddir) in
  let loadpath =
    Filename.dirname artifact :: Option.value loadpath_override ~default:loadpath
  in
  let config = Mconfig.initial in
  let config =
    { config with
      merlin = { config.merlin with build_path = loadpath }
    ; query = { config.query with filename = source; directory = Filename.dirname source }
    }
  in
  Mocaml.setup_typer_config config;
  Ocaml_typing.Envaux.reset_cache ();
  let source_digest = Option.map cmt.cmt_source_digest ~f:Digest.to_hex in
  let source_digest =
    match source_digest, cmt.cmt_sourcefile with
    | Some expected, _ when String.equal expected (digest text) -> Some expected
    | _, Some input
      when String.is_suffix input ~suffix:".pp.ml"
           || String.is_suffix input ~suffix:".pp.mli" ->
      if (Unix.stat source).st_mtime <= (Unix.stat artifact).st_mtime
      then Some (digest text)
      else None
    | _ -> source_digest
  in
  let file =
    collect
      ~cancel
      ~root_env
      ~shape_roots
      ~read_shape:(read_shape cache)
      ~source
      ~unit_name:cmt.cmt_modname
      ~builddir:cmt.cmt_builddir
      ~loadpath
      ~imports:
        (List.map cmt.cmt_imports ~f:(fun (name, crc) ->
           name, Option.map crc ~f:Digest.to_hex))
      ~text
      ~digest:source_digest
      tree
      signature
  in
  Some
    { file with
      artifact = Some artifact
    ; interface_digest = Option.map cmt.cmt_interface_digest ~f:Digest.to_hex
    }
;;

let refresh ?units t ~roots ~paths ~cancel =
  let directories = build_roots roots paths in
  let paths = artifacts directories cancel in
  let present = H.create (List.length paths) in
  let wanted = H.create (List.length paths) in
  let failures = ref 0 in
  List.iter paths ~f:(fun path ->
    check_cancel cancel;
    H.replace present path ();
    try
      let stamp = stat path in
      let relevant =
        match units with
        | None -> true
        | Some units ->
          let unit_name, imports =
            match H.find_opt t.metadata path with
            | Some (previous, name, imports) when Poly.equal previous stamp ->
              name, imports
            | _ ->
              let cmt = Cmt_format.read_cmt path in
              let imports = List.map cmt.cmt_imports ~f:fst in
              if Poly.equal stamp (stat path)
              then H.replace t.metadata path (stamp, cmt.cmt_modname, imports);
              cmt.cmt_modname, imports
          in
          List.exists units ~f:(fun unit ->
            String.equal unit unit_name || List.mem imports unit ~equal:String.equal)
      in
      if relevant
      then (
        H.replace wanted path ();
        match H.find_opt t.artifacts path with
        | Some (previous, _) when Poly.equal stamp previous -> ()
        | _ ->
          (match load t roots ~shape_roots:(roots @ directories) path cancel with
           | None -> H.remove t.artifacts path
           | Some file ->
             if Poly.equal stamp (stat path)
             then H.replace t.artifacts path (stamp, file)
             else (
               H.remove t.artifacts path;
               incr failures)))
    with
    | Cancelled -> raise Cancelled
    | _ ->
      H.remove t.artifacts path;
      incr failures);
  H.filter_map_inplace
    (fun path file -> if H.mem present path then Some file else None)
    t.artifacts;
  let files =
    H.fold
      (fun path (_, file) acc -> if H.mem wanted path then file :: acc else acc)
      t.artifacts
      []
  in
  H.filter_map_inplace
    (fun path entry -> if H.mem present path then Some entry else None)
    t.metadata;
  H.filter_map_inplace
    (fun path info -> if H.mem present path then Some info else None)
    t.shapes;
  let sources = H.create (List.length files) in
  List.iter files ~f:(fun file -> H.replace sources file.source ());
  H.filter_map_inplace
    (fun path info -> if H.mem sources path then Some info else None)
    t.sources;
  files, (if paths = [] then 1 else 0) + !failures
;;

let live ?cancel ~cache ~roots pipeline doc =
  let config = Mpipeline.final_config pipeline in
  let typedtree = Mtyper.get_typedtree (Mpipeline.typer_result pipeline) in
  let tree = B.of_browse (Mbrowse.of_typedtree typedtree) in
  let signature, root_env =
    match typedtree with
    | `Implementation tree -> tree.str_type, tree.str_final_env
    | `Interface tree -> tree.sig_type, tree.sig_final_env
  in
  let source = canonical (Uri.to_path (Document.uri doc)) in
  collect
    ~cancel
    ~root_env
    ~shape_roots:(roots @ build_roots roots (Mconfig.build_path config))
    ~read_shape:(read_shape cache)
    ~source
    ~unit_name:(Mconfig.unitname config)
    ~builddir:config.query.directory
    ~loadpath:(Mconfig.build_path config)
    ~imports:[]
    ~text:(Document.text doc)
    ~digest:None
    tree
    signature
;;

(* Load only the selected installed compilation units, never walk a switch.
   Their declarations connect project UIDs to relocated .ml/.mli exports. *)
let supporting_files cache ~files ~units ~cancel =
  let seen = H.create 16 in
  List.concat_map files ~f:(fun origin ->
    List.concat_map units ~f:(fun unit_name ->
      check_cancel cancel;
      let owner =
        List.find_map origin.loadpath ~f:(fun directory ->
          List.find_map
            [ unit_name; Stdlib.String.uncapitalize_ascii unit_name ]
            ~f:(fun name ->
              let cmi = Filename.concat directory (name ^ ".cmi") in
              if Sys.file_exists cmi then Some (directory, name) else None))
      in
      match owner with
      | None -> []
      | Some (directory, name) ->
        List.filter_map
          [ ".cmt", ".ml"; ".cmti", ".mli" ]
          ~f:(fun (ext, source_ext) ->
            let artifact = canonical (Filename.concat directory (name ^ ext)) in
            if H.mem seen artifact || Option.is_none (Installed_library.root artifact)
            then None
            else (
              H.add seen artifact ();
              try
                let stamp = stat artifact in
                let cmt = Cmt_format.read_cmt artifact in
                if not (String.equal cmt.cmt_modname unit_name)
                then None
                else (
                  let names =
                    (if String.equal origin.unit_name unit_name
                     then [ stem origin.source ^ source_ext ]
                     else [])
                    @ (match cmt.cmt_annots with
                       | Implementation tree ->
                         [ (Mbrowse.node_loc (Browse_raw.Structure tree)).loc_start
                             .pos_fname
                         ]
                       | Interface tree ->
                         [ (Mbrowse.node_loc (Browse_raw.Signature tree)).loc_start
                             .pos_fname
                         ]
                       | _ -> [])
                    @ Option.to_list cmt.cmt_sourcefile
                  in
                  let source =
                    List.find_map names ~f:(fun recorded ->
                      let basename = Filename.basename recorded in
                      let basename =
                        if String.is_suffix basename ~suffix:(".pp" ^ source_ext)
                        then stem (stem basename) ^ source_ext
                        else basename
                      in
                      let source = canonical (Filename.concat directory basename) in
                      if
                        String.is_suffix source ~suffix:source_ext
                        && Sys.file_exists source
                      then Some source
                      else None)
                  in
                  Option.bind source ~f:(fun source ->
                    let text_digest = digest (read source) in
                    match H.find_opt cache.support artifact with
                    | Some (previous, previous_text, file)
                      when Poly.equal previous stamp
                           && String.equal previous_text text_digest -> Some file
                    | _ ->
                      let file =
                        load
                          ~source_override:source
                          ~loadpath_override:origin.loadpath
                          cache
                          [ directory ]
                          ~shape_roots:[ directory ]
                          artifact
                          cancel
                      in
                      Option.map file ~f:(fun file ->
                        let file = { file with digest = Some text_digest } in
                        if Poly.equal stamp (stat artifact)
                        then H.replace cache.support artifact (stamp, text_digest, file);
                        file)))
              with
              | Cancelled -> raise Cancelled
              | _ ->
                H.remove cache.support artifact;
                None))))
;;

let target_units file ~source offset =
  let occurrences =
    List.filter file.occurrences ~f:(fun o -> o.first <= offset && offset <= o.last)
  in
  let selected =
    List.sort occurrences ~compare:(fun a b ->
      Int.compare (a.last - a.first) (b.last - b.first))
    |> List.hd
  in
  Option.bind selected ~f:(fun o ->
    if not (String.equal source file.source)
    then None
    else Some [ Option.value o.symbol.unit_name ~default:file.unit_name ])
;;

let key namespace path first last = Marshal.to_string (namespace, path, first, last) []

let make_resolver files =
  let by_unit = H.create 64
  and owners = H.create 64
  and resolved = H.create 1024 in
  List.iter files ~f:(fun file ->
    H.replace
      by_unit
      file.unit_name
      (file :: Option.value (H.find_opt by_unit file.unit_name) ~default:[]));
  let resolve (file : file) (sym : symbol) =
    if sym.local
    then Some (key sym.namespace (canonical sym.filename) sym.first sym.last)
    else (
      let direct = canonical (absolute file.builddir sym.filename) in
      let owner_artifact =
        Option.bind sym.unit_name ~f:(fun unit_name ->
          let lookup () =
            let names =
              [ unit_name ^ ".cmi"; Stdlib.String.uncapitalize_ascii unit_name ^ ".cmi" ]
            in
            List.find_map file.loadpath ~f:(fun directory ->
              List.find_map names ~f:(fun name ->
                let path = Filename.concat directory name in
                if Sys.file_exists path then Some (canonical (stem path)) else None))
          in
          let key = file.source, file.artifact, unit_name in
          match H.find_opt owners key with
          | Some result -> result
          | None ->
            let result = lookup () in
            H.add owners key result;
            result)
      in
      let possible =
        match sym.unit_name with
        | None -> files
        | Some name -> Option.value (H.find_opt by_unit name) ~default:[]
      in
      let candidates =
        List.filter possible ~f:(fun candidate ->
          let owner =
            match sym.unit_name with
            | None -> true
            | Some name when String.equal name file.unit_name ->
              String.equal (stem candidate.source) (stem file.source)
            | Some _ ->
              (match owner_artifact, candidate.artifact with
               | Some expected, Some path -> String.equal expected (canonical (stem path))
               | _ -> false)
          in
          let compatible =
            match sym.unit_name, candidate.interface_digest with
            | Some name, Some actual ->
              (match List.assoc file.imports name with
               | Some (Some expected) -> String.equal actual expected
               | _ -> true)
            | _ -> true
          in
          owner
          && compatible
          && Option.value_map
               sym.unit_name
               ~default:true
               ~f:(String.equal candidate.unit_name)
          && (String.equal sym.namespace "unit"
              || String.equal candidate.source direct
              || String.is_suffix candidate.source ~suffix:("/" ^ sym.filename)
              || (Option.is_some (Installed_library.root candidate.source)
                  && String.equal
                       (Filename.basename candidate.source)
                       (Filename.basename sym.filename))))
      in
      let sources =
        List.map candidates ~f:(fun candidate -> candidate.source)
        |> List.sort_uniq ~compare:Poly.compare
      in
      let source =
        match sources with
        | [ source ] -> Some source
        | _ when String.equal sym.namespace "unit" ->
          let stems = List.map sources ~f:stem |> List.sort_uniq ~compare:Poly.compare in
          (match stems with
           | [ source ] -> Some source
           | _ -> None)
        | _ -> if List.mem sources direct ~equal:String.equal then Some direct else None
      in
      Option.bind source ~f:(fun source ->
        if
          List.exists candidates ~f:(fun candidate ->
            String.equal candidate.source source
            && List.exists candidate.ambiguous ~f:(fun (first, last) ->
              first <= sym.first
              && sym.last <= last
              && not (first = sym.first && sym.last = last)))
        then None
        else
          Some
            (key
               sym.namespace
               (if String.equal sym.namespace "unit" then stem source else source)
               sym.first
               sym.last)))
  in
  fun file sym ->
    let lookup_key = file.source, file.artifact, sym in
    match H.find_opt resolved lookup_key with
    | Some result -> result
    | None ->
      let result = resolve file sym in
      H.add resolved lookup_key result;
      result
;;

let lookup ~cache ~files ~overlays ~source ~offset ~include_declaration ~cancel =
  let all = overlays @ files in
  let resolve = make_resolver all in
  let parents = H.create 1024 in
  let rec representative key =
    match H.find_opt parents key with
    | None -> key
    | Some parent when String.equal parent key -> key
    | Some parent ->
      let root = representative parent in
      H.replace parents key root;
      root
  in
  let join a b =
    let a = representative a
    and b = representative b in
    if not (String.equal a b) then H.replace parents a b
  in
  let exports = H.create 256 in
  List.iter all ~f:(fun file ->
    check_cancel cancel;
    List.iter file.edges ~f:(fun (a, b) ->
      match resolve file a, resolve file b with
      | Some a, Some b -> join a b
      | _ -> ());
    List.iter file.exports ~f:(fun (name, sym) ->
      Option.iter (resolve file sym) ~f:(fun symbol ->
        let slot = stem file.source, file.unit_name, name in
        match H.find_opt exports slot with
        | None -> H.add exports slot symbol
        | Some other -> join symbol other)));
  let current = List.find overlays ~f:(fun file -> String.equal file.source source) in
  let has_occurrence = ref false in
  let selected =
    Option.bind current ~f:(fun file ->
      let inside =
        List.filter file.occurrences ~f:(fun occurrence ->
          occurrence.first <= offset && offset < occurrence.last)
      in
      (if inside <> []
       then inside
       else List.filter file.occurrences ~f:(fun occurrence -> occurrence.last = offset))
      |> (fun occurrences ->
      has_occurrence := occurrences <> [];
      occurrences)
      |> List.sort ~compare:(fun (a : occurrence) (b : occurrence) ->
        match Int.compare (a.last - a.first) (b.last - b.first) with
        | Ordering.Eq ->
          Poly.compare
            (not a.declaration, a.symbol.namespace <> "value")
            (not b.declaration, b.symbol.namespace <> "value")
        | order -> order)
      |> List.find_map ~f:(fun occurrence -> resolve file occurrence.symbol))
  in
  let stale = ref 0 in
  let found =
    Option.map selected ~f:(fun selected ->
      let selected = representative selected in
      let indexed =
        List.filter files ~f:(fun file ->
          not
            (List.exists overlays ~f:(fun overlay ->
               String.equal file.source overlay.source)))
      in
      let valid =
        List.filter indexed ~f:(fun file ->
          try
            let stamp = stat file.source in
            let actual =
              match H.find_opt cache.sources file.source with
              | Some (previous, digest) when Poly.equal stamp previous -> Some digest
              | _ ->
                let digest = digest (read file.source) in
                if Poly.equal stamp (stat file.source)
                then (
                  H.replace cache.sources file.source (stamp, digest);
                  Some digest)
                else None
            in
            match file.digest, actual with
            | Some expected, Some actual when String.equal expected actual -> true
            | _ ->
              incr stale;
              false
          with
          | Sys_error _ | Unix.Unix_error _ ->
            incr stale;
            false)
      in
      List.concat_map (overlays @ valid) ~f:(fun file ->
        check_cancel cancel;
        List.filter_map file.occurrences ~f:(fun occurrence ->
          if occurrence.declaration && not include_declaration
          then None
          else (
            match resolve file occurrence.symbol with
            | Some symbol when String.equal (representative symbol) selected ->
              Some (file, occurrence)
            | _ -> None))))
  in
  (if !has_occurrence && Option.is_none found then Some [] else found), !stale
;;

let position text offset ~encoding =
  let source = Msource.make text in
  let (`Logical (line, column)) = Msource.get_logical source (`Offset offset) in
  let character =
    match encoding with
    | `UTF8 -> column
    | `UTF16 ->
      let rec count i units =
        if i >= offset
        then units
        else (
          let code = Char.code text.[i] in
          let bytes, width =
            if code < 0x80
            then 1, 1
            else if code < 0xe0
            then 2, 1
            else if code < 0xf0
            then 3, 1
            else 4, 2
          in
          count (i + bytes) (units + width))
      in
      count (offset - column) 0
  in
  Position.create ~line:(line - 1) ~character
;;

let location ~encoding (file, (occurrence : occurrence)) =
  let start = position file.text occurrence.first ~encoding
  and end_ = position file.text occurrence.last ~encoding in
  Location.create ~uri:(Uri.of_path file.source) ~range:(Range.create ~start ~end_)
;;

let notice t message =
  if String.equal t.notice message
  then false
  else (
    t.notice <- message;
    true)
;;
