open Import

let action_kind = "type-annotate"

let check_typeable_context pipeline (range : Range.t) =
  let pos_start = Mpipeline.get_lexing_pos pipeline (Position.logical range.start) in
  let typer = Mpipeline.typer_result pipeline in
  let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
  let nodes = Mbrowse.enclosing pos_start [ browse ] in
  let nodes =
    if Position.compare range.start range.end_ = 0
    then nodes
    else
      List.filter nodes ~f:(fun (_, node) ->
        (* Unlike [node_loc], this includes type constraints and coercions, so
           selecting an annotated node does not select its parent instead. *)
        Option.exists (Range.of_loc_opt (Mbrowse.node_merlin_loc node)) ~f:(fun outer ->
          Range.contains outer range))
  in
  let is_exp_constrained = function
    | Typedtree.Texp_constraint _, _, _ -> true
    | Typedtree.Texp_coerce (Some _, _), _, _ -> true
    | _ -> false
  in
  let is_pat_constrained = function
    | Typedtree.Tpat_constraint _, _, _ -> true
    | _ -> false
  in
  let is_valid node p extras =
    if List.exists ~f:p extras then `Invalid else `Valid (Mbrowse.node_loc node)
  in
  match nodes with
  | (_, Pattern { pat_desc = Tpat_var _; _ })
    :: (_, Value_binding { vb_expr = { exp_desc = Texp_function _; _ }; _ })
    :: _ -> `Invalid
  | (_, (Expression e as node)) :: _ -> is_valid node is_exp_constrained e.exp_extra
  | (_, (Pattern { pat_desc = Typedtree.Tpat_any; _ } as node))
    :: (_, Pattern { pat_desc = Typedtree.Tpat_alias _; pat_extra; _ })
    :: _ -> is_valid node is_pat_constrained pat_extra
  | (_, (Pattern p as node)) :: _ -> is_valid node is_pat_constrained p.pat_extra
  | _ :: _ | [] -> `Invalid
;;

let code_action_of_type_enclosing doc (loc, typ) =
  let open Option.O in
  let+ original_text = Code_action.source_text doc loc in
  let newText = Printf.sprintf "(%s : %s)" original_text typ in
  let textedit : TextEdit.t = { range = Range.of_loc loc; newText } in
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
  let open Option.O in
  let pos_start = Position.logical params.range.start in
  let* target =
    match check_typeable_context pipeline params.range with
    | `Invalid -> None
    | `Valid loc -> Some loc
  in
  let config = Mpipeline.final_config pipeline in
  let config = { config with query = { config.query with verbosity = Lvl 0 } } in
  let pipeline = Mpipeline.make config (Document.source doc) in
  let dispatch index =
    Query_commands.dispatch
      pipeline
      (Query_protocol.Type_enclosing (None, pos_start, Some index))
  in
  let cursor_only = Position.compare params.range.start params.range.end_ = 0 in
  let matches_target loc =
    Loc.compare loc target = 0
    && Option.exists (Range.of_loc_opt loc) ~f:(fun range ->
      Range.contains range params.range)
  in
  let* index =
    if cursor_only
    then Some 0
    else (
      (* Locate the selected node before asking Merlin to print its type. *)
      List.find_mapi (dispatch (-1)) ~f:(fun index (loc, _, _) ->
        Option.some_if (matches_target loc) index))
  in
  let* location, typ, _ = List.nth (dispatch index) index in
  match typ with
  | `Index _ -> None
  | `String value ->
    let* () = Option.some_if (cursor_only || matches_target location) () in
    code_action_of_type_enclosing doc (location, value)
;;

let kind = CodeActionKind.Other action_kind
let t = Code_action.batchable kind code_action
