open Import

let action_kind = "remove type annotation"

let check_typeable_context pipeline (range : Range.t) =
  let is_exp_constrained = function
    | Typedtree.Texp_constraint _, loc, _ -> Some (loc, loc)
    | Typedtree.Texp_coerce (Some { ctyp_loc; _ }, _), loc, _ -> Some (loc, ctyp_loc)
    | _ -> None
  in
  let is_pat_constrained = function
    | Typedtree.Tpat_constraint _, loc, _ -> Some (loc, loc)
    | _ -> None
  in
  let contains outer inner =
    match Range.of_loc_opt outer, Range.of_loc_opt inner with
    | Some outer, Some inner -> Range.contains outer inner
    | _ -> false
  in
  let is_valid loc p extras =
    let candidates = List.filter_map extras ~f:p in
    let cursor_only = Position.compare range.start range.end_ = 0 in
    let chosen =
      List.rev candidates
      |> List.find ~f:(fun (cover, _) ->
        cursor_only
        || Option.exists (Range.of_loc_opt cover) ~f:(fun outer ->
          Range.contains outer range))
    in
    match chosen with
    | None -> `Invalid
    | Some (cover, edit_loc) ->
      let edit_loc = Loc.union loc edit_loc in
      let retained =
        List.find_map candidates ~f:(fun (inner, _) ->
          Option.some_if
            (Loc.compare cover inner <> 0 && contains edit_loc inner && contains inner loc)
            inner)
        |> Option.value ~default:loc
      in
      `Valid (retained, edit_loc)
  in
  match Code_action.enclosing_nodes pipeline range with
  | (_, Pattern { pat_desc = Tpat_var _; _ })
    :: (_, Value_binding { vb_expr = { exp_desc = Texp_function _; _ }; _ })
    :: _ -> `Invalid
  | (_, Expression e) :: _ -> is_valid e.exp_loc is_exp_constrained e.exp_extra
  | (_, Pattern { pat_desc = Typedtree.Tpat_any; pat_loc; _ })
    :: (_, Pattern { pat_desc = Typedtree.Tpat_alias _; pat_extra; _ })
    :: _ -> is_valid pat_loc is_pat_constrained pat_extra
  | (_, Pattern p) :: _ -> is_valid p.pat_loc is_pat_constrained p.pat_extra
  | _ :: _ | [] -> `Invalid
;;

let code_action_of_type_enclosing doc (loc, constr_loc) =
  let open Option.O in
  let+ src_text = Code_action.source_text doc loc in
  let textedit : TextEdit.t =
    { range = Range.of_loc (Loc.union loc constr_loc); newText = src_text }
  in
  let edit = Text_document.workspace_edit (Document.text_document doc) [ textedit ] in
  let title = String.capitalize action_kind in
  CodeAction.create
    ~title
    ~kind:(CodeActionKind.Other action_kind)
    ~edit
    ~isPreferred:false
    ()
;;

let code_action pipeline doc (params : CodeActionParams.t) =
  let context = check_typeable_context pipeline params.range in
  match context with
  | `Invalid -> None
  | `Valid (loc1, loc2) -> code_action_of_type_enclosing doc (loc1, loc2)
;;

let kind = CodeActionKind.Other action_kind
let t = Code_action.batchable kind code_action
