open Import

let action_title = "Inline into uses"

type inline_task =
  { inlined_var : Ident.t
  ; inlined_expr : Typedtree.expression (** the expression to inline *)
  }

let find_path_by_name id env =
  try Some (fst (Ocaml_typing.Env.find_value_by_name id env)) with
  | Not_found -> None
;;

let check_shadowing (inlined_expr : Typedtree.expression) new_env =
  let module I = Ocaml_typing.Tast_iterator in
  let orig_env = inlined_expr.exp_env in
  let exception Env_mismatch of (Longident.t * [ `Unbound | `Shadowed ]) in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_ident (path, { txt = ident; _ }, _) ->
      let in_orig_env =
        find_path_by_name ident orig_env
        |> Option.map ~f:(Path.same path)
        |> Option.value ~default:false
      in
      if in_orig_env
      then (
        match find_path_by_name ident new_env with
        | Some path' ->
          if not (Path.same path path')
          then raise_notrace (Env_mismatch (ident, `Shadowed))
        | None -> raise_notrace (Env_mismatch (ident, `Unbound)))
    | _ -> I.default_iterator.expr iter expr
  in
  let iter = { I.default_iterator with expr = expr_iter } in
  try
    iter.expr iter inlined_expr;
    Ok ()
  with
  | Env_mismatch m -> Error m
;;

let string_of_error (ident, reason) =
  let reason =
    match reason with
    | `Unbound -> "unbound"
    | `Shadowed -> "shadowed"
  in
  Format.asprintf "'%a' is %s in inlining context" Pprintast.longident ident reason
;;

let contains loc pos =
  match Position.compare_inclusion pos (Range.of_loc loc) with
  | `Outside _ -> false
  | `Inside -> true
;;

let find_inline_task typedtree pos =
  let exception Found of inline_task in
  let module I = Ocaml_typing.Tast_iterator in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    if contains expr.exp_loc pos
    then (
      match expr.exp_desc with
      | Texp_let
          ( Nonrecursive
          , [ { vb_pat = { pat_desc = Tpat_var (inlined_var, { loc; _ }); _ }
              ; vb_expr = inlined_expr
              ; _
              }
            ]
          , _ )
        when contains loc pos -> raise_notrace (Found { inlined_var; inlined_expr })
      | _ -> I.default_iterator.expr iter expr)
  in
  let structure_item_iter (iter : I.iterator) (item : Typedtree.structure_item) =
    if contains item.str_loc pos
    then (
      match item.str_desc with
      | Tstr_value
          ( Nonrecursive
          , [ { vb_pat = { pat_desc = Tpat_var (inlined_var, { loc; _ }); _ }
              ; vb_expr = inlined_expr
              ; _
              }
            ] )
        when contains loc pos -> raise_notrace (Found { inlined_var; inlined_expr })
      | _ -> I.default_iterator.structure_item iter item)
  in
  let iterator =
    { I.default_iterator with expr = expr_iter; structure_item = structure_item_iter }
  in
  try
    iterator.structure iterator typedtree;
    None
  with
  | Found task -> Some task
;;

(** [find_parsetree_loc pl loc] finds an expression node in the parsetree with
    location [loc] *)
let find_parsetree_loc pipeline loc =
  let exception Found of Parsetree.expression in
  try
    let expr_iter (iter : Ast_iterator.iterator) (expr : Parsetree.expression) =
      if Loc.compare expr.pexp_loc loc = 0
      then raise_notrace (Found expr)
      else Ast_iterator.default_iterator.expr iter expr
    in
    let iterator = { Ast_iterator.default_iterator with expr = expr_iter } in
    (match Mpipeline.reader_parsetree pipeline with
     | `Implementation s -> iterator.structure iterator s
     | `Interface _ -> ());
    None
  with
  | Found e -> Some e
;;

(** [strip_attribute name e] removes all instances of the attribute called
    [name] in [e]. *)
let strip_attribute attr_name expr =
  let module M = Ocaml_parsing.Ast_mapper in
  let expr_map (map : M.mapper) expr =
    { (M.default_mapper.expr map expr) with
      pexp_attributes =
        List.filter expr.pexp_attributes ~f:(fun (a : Parsetree.attribute) ->
          not (String.equal a.attr_name.txt attr_name))
    }
  in
  let mapper = { M.default_mapper with expr = expr_map } in
  mapper.expr mapper expr
;;

let inlined_text pipeline task =
  let open Option.O in
  let+ expr = find_parsetree_loc pipeline task.inlined_expr.exp_loc in
  let expr = strip_attribute "merlin.loc" expr in
  Format.asprintf "(%a)" Pprintast.expression expr
;;

(* Only immediate values and aliases can be duplicated without changing
   allocation identity. Function expressions are moved only into full calls. *)
let rec function_arity (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_function { arg_label = Nolabel; cases = [ { c_guard = None; c_rhs; _ } ]; _ } ->
    1 + function_arity c_rhs
  | Texp_function { arg_label = Nolabel; _ } -> 1
  | _ -> 0
;;

let inline_edits pipeline doc task =
  let module I = Ocaml_typing.Tast_iterator in
  let arity = function_arity task.inlined_expr in
  let atom =
    match task.inlined_expr.exp_desc with
    | Texp_ident _ | Texp_constant (Const_int _ | Const_char _) -> true
    | Texp_construct (_, { cstr_tag = Cstr_constant _; _ }, []) -> true
    | _ -> false
  in
  let error = ref None in
  let locations = ref [] in
  let use env loc full_call =
    locations := loc :: !locations;
    let reject reason = error := Some reason in
    if (not atom) && arity = 0
    then reject "Inlining would repeat evaluation or allocation of the binding"
    else if arity > 0 && not full_call
    then reject "Only direct, fully applied calls of unlabelled functions can be inlined"
    else if not (Code_action.relocatable_expression task.inlined_expr env)
    then reject "Inlining would change the scope of a name or type"
    else (
      match check_shadowing task.inlined_expr env with
      | Ok () -> ()
      | Error e -> reject (string_of_error e))
  in
  let expr (iter : I.iterator) (e : Typedtree.expression) =
    match e.exp_desc with
    | Texp_apply (({ exp_desc = Texp_ident (Pident id, _, _); _ } as fn), args)
      when Ident.same task.inlined_var id ->
      let full_call =
        List.length args = arity
        && List.for_all args ~f:(function
          | Asttypes.Nolabel, Some _ -> true
          | _ -> false)
      in
      use e.exp_env fn.exp_loc full_call;
      List.iter args ~f:(fun (_, arg) -> Option.iter arg ~f:(iter.expr iter))
    | Texp_ident (Pident id, { loc; _ }, _) when Ident.same task.inlined_var id ->
      use e.exp_env loc false
    | _ -> I.default_iterator.expr iter e
  in
  let iter = { I.default_iterator with expr } in
  (match Mtyper.get_typedtree (Mpipeline.typer_result pipeline) with
   | `Interface _ -> ()
   | `Implementation structure -> iter.structure iter structure);
  if Code_action.has_comments pipeline (Range.of_loc task.inlined_expr.exp_loc)
  then error := Some "Inlining would discard comments in the definition";
  match !locations, !error with
  | [], _ -> `Not_applicable
  | _, Some reason -> `Disabled reason
  | locations, None ->
    (match inlined_text pipeline task with
     | None -> `Not_applicable
     | Some newText ->
       `Edits
         (List.rev_map locations ~f:(fun loc ->
            let range = Range.of_loc loc in
            let source = Document.source doc in
            let (`Offset offset) =
              Msource.get_offset source (Position.logical range.start)
            in
            let text = Msource.text source in
            let newText =
              if offset > 0 && (text.[offset - 1] = '~' || text.[offset - 1] = '?')
              then Option.value_exn (Document.substring doc range) ^ ":" ^ newText
              else newText
            in
            TextEdit.create ~range ~newText)))
;;

let code_action pipeline doc (params : CodeActionParams.t) =
  let open Option.O in
  let* typedtree =
    match Mtyper.get_typedtree (Mpipeline.typer_result pipeline) with
    | `Interface _ -> None
    | `Implementation x -> Some x
  in
  let* task = find_inline_task typedtree params.range.start in
  match inline_edits pipeline doc task with
  | `Not_applicable -> None
  | `Disabled reason ->
    Some (Code_action.disabled ~kind:RefactorInline ~title:action_title reason)
  | `Edits edits ->
    Some
      (CodeAction.create
         ~title:action_title
         ~kind:RefactorInline
         ~edit:(Document.edit doc edits)
         ~isPreferred:false
         ())
;;

let t = Code_action.batchable RefactorInline code_action
