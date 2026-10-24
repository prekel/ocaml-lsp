open Import
open Option.O

let range_contains_loc range loc =
  match Range.of_loc_opt loc with
  | Some range' -> Range.contains range range'
  | None -> false
;;

let range_contained_by_loc range loc =
  match Range.of_loc_opt loc with
  | Some range' -> Range.contains range' range
  | None -> false
;;

let largest_enclosed_expression typedtree range =
  let exception Found of Typedtree.expression in
  let module I = Ocaml_typing.Tast_iterator in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    if range_contains_loc range expr.exp_loc
    then raise (Found expr)
    else I.default_iterator.expr iter expr
  in
  let iterator = { I.default_iterator with expr = expr_iter } in
  try
    iterator.structure iterator typedtree;
    None
  with
  | Found e -> Some e
;;

let enclosing_structure_item typedtree range =
  let exception Found of Typedtree.structure_item in
  let module I = Ocaml_typing.Tast_iterator in
  let structure_item_iter (iter : I.iterator) (item : Typedtree.structure_item) =
    if range_contained_by_loc range item.str_loc
    then (
      match item.str_desc with
      | Tstr_value _ -> raise (Found item)
      | _ -> I.default_iterator.structure_item iter item)
  in
  let iterator = { I.default_iterator with structure_item = structure_item_iter } in
  try
    iterator.structure iterator typedtree;
    None
  with
  | Found e -> Some e
;;

(* A local binding may move only past stable atoms. Control-flow children are
   separate insertion regions, including branch bodies and lazy expressions. *)
let local_region typedtree selected =
  let stable (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_ident _ | Texp_constant _ -> true
    | _ -> false
  in
  let children expr =
    let module I = Ocaml_typing.Tast_iterator in
    let result = ref [] in
    let iter = { I.default_iterator with expr = (fun _ e -> result := e :: !result) } in
    I.default_iterator.expr iter expr;
    List.rev !result
  in
  let rec visit region (expr : Typedtree.expression) =
    if Loc.compare expr.exp_loc selected.Typedtree.exp_loc = 0
    then Some region
    else (
      let children = children expr in
      match
        List.filter children ~f:(fun e ->
          range_contained_by_loc (Range.of_loc selected.exp_loc) e.Typedtree.exp_loc)
      with
      | [ child ] ->
        (match expr.exp_desc with
         | Texp_function _
         | Texp_let _
         | Texp_match _
         | Texp_try _
         | Texp_ifthenelse _
         | Texp_sequence _
         | Texp_while _
         | Texp_for _
         | Texp_letmodule _
         | Texp_letexception _
         | Texp_open _
         | Texp_lazy _
         | Texp_letop _ -> visit child child
         | Texp_apply
             ({ exp_desc = Texp_ident (_, { txt = Lident ("&&" | "||"); _ }, _); _ }, _)
           -> visit child child
         | Texp_apply _ | Texp_tuple _ | Texp_construct _ ->
           if List.for_all children ~f:(fun e -> e == child || stable e)
           then visit region child
           else None
         | _ -> None)
      | _ -> None)
  in
  let found = ref None in
  let module I = Ocaml_typing.Tast_iterator in
  let expr iter (e : Typedtree.expression) =
    if range_contained_by_loc (Range.of_loc selected.exp_loc) e.exp_loc
    then found := visit e e
    else I.default_iterator.expr iter e
  in
  let iter = { I.default_iterator with expr } in
  iter.structure iter typedtree;
  !found
;;

module LongidentSet = Set.Make (struct
    type t = Longident.t

    let compare = compare
  end)

(** [free expr] returns the free variables in [expr]. *)
let free (expr : Typedtree.expression) =
  let module I = Ocaml_typing.Tast_iterator in
  let idents = ref [] in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_ident (path, { txt = ident; _ }, _) -> idents := (ident, path) :: !idents
    | _ ->
      I.default_iterator.expr iter expr;
      (* if a variable was bound but is no longer, it must be associated with a
         binder inside the expression *)
      idents
      := List.filter !idents ~f:(fun (ident, path) ->
           match Env.find_value_by_name ident expr.exp_env with
           | path', _ -> Path.same path path'
           | exception Not_found -> false)
  in
  let iter = { I.default_iterator with expr = expr_iter } in
  iter.expr iter expr;
  !idents
;;

let must_pass expr env =
  List.filter (free expr) ~f:(fun (ident, path) ->
    match Env.find_value_by_name ident env with
    | path', _ ->
      (* new environment binds ident to a different path than the old one *)
      not (Path.same path path')
    | exception Not_found -> true)
  |> List.map ~f:fst
;;

let extract_local doc typedtree range =
  let* to_extract = largest_enclosed_expression typedtree range in
  let* region = local_region typedtree to_extract in
  let* region_range = Range.of_loc_opt region.exp_loc in
  let* extract_range = Range.of_loc_opt to_extract.exp_loc in
  let* region_text = Document.substring doc region_range in
  let* local_text = Document.substring doc extract_range in
  let start = to_extract.exp_loc.loc_start.pos_cnum - region.exp_loc.loc_start.pos_cnum in
  let stop = to_extract.exp_loc.loc_end.pos_cnum - region.exp_loc.loc_start.pos_cnum in
  let* () =
    Option.some_if (0 <= start && start <= stop && stop <= String.length region_text) ()
  in
  let new_name = Code_action.fresh_name doc "var_name" in
  let body =
    String.sub region_text ~pos:0 ~len:start
    ^ new_name
    ^ String.sub region_text ~pos:stop ~len:(String.length region_text - stop)
  in
  let newText = sprintf "(let %s = %s in\n%s)" new_name local_text body in
  Some [ TextEdit.create ~newText ~range:region_range ]
;;

let extract_function doc typedtree range =
  let* to_extract = largest_enclosed_expression typedtree range in
  let* extract_range = Range.of_loc_opt to_extract.exp_loc in
  let* parent_item = enclosing_structure_item typedtree range in
  let* edit_pos = Position.of_lexical_position parent_item.str_loc.loc_start in
  let* () =
    Option.some_if (Code_action.relocatable_expression to_extract parent_item.str_env) ()
  in
  let new_name = Code_action.fresh_name doc "fun_name" in
  let* args_str =
    let seen = ref LongidentSet.empty in
    let free_vars =
      must_pass to_extract parent_item.str_env
      |> List.filter ~f:(fun ident ->
        if LongidentSet.mem ident !seen
        then false
        else (
          seen := LongidentSet.add ident !seen;
          true))
    in
    let+ args =
      List.map free_vars ~f:(function
        | Longident.Lident id -> Some id
        | _ -> None)
      |> Option.List.all
    in
    let s = String.concat ~sep:" " args in
    if String.is_empty s then "()" else s
  in
  let* func_text = Document.substring doc extract_range in
  let new_function = sprintf "let %s %s = %s\n\n" new_name args_str func_text in
  let new_call = sprintf "%s %s" new_name args_str in
  let insert_range = { Range.start = edit_pos; end_ = edit_pos } in
  Some
    [ TextEdit.create ~newText:new_function ~range:insert_range
    ; TextEdit.create ~newText:new_call ~range:extract_range
    ]
;;

let run_extract_local pipeline doc (params : CodeActionParams.t) =
  let typer = Mpipeline.typer_result pipeline in
  let* typedtree =
    match Mtyper.get_typedtree typer with
    | `Interface _ -> None
    | `Implementation x -> Some x
  in
  let* _ = largest_enclosed_expression typedtree params.range in
  match extract_local doc typedtree params.range with
  | None ->
    Some
      (Code_action.disabled
         ~kind:CodeActionKind.RefactorExtract
         ~title:"Extract local"
         "Extraction would change evaluation order or scope")
  | Some edits ->
    Some
      (CodeAction.create
         ~title:"Extract local"
         ~kind:CodeActionKind.RefactorExtract
         ~edit:(Document.edit doc edits)
         ~isPreferred:false
         ())
;;

let run_extract_function pipeline doc (params : CodeActionParams.t) =
  let typer = Mpipeline.typer_result pipeline in
  let* typedtree =
    match Mtyper.get_typedtree typer with
    | `Interface _ -> None
    | `Implementation x -> Some x
  in
  let* _ = largest_enclosed_expression typedtree params.range in
  match extract_function doc typedtree params.range with
  | None ->
    Some
      (Code_action.disabled
         ~kind:CodeActionKind.RefactorExtract
         ~title:"Extract function"
         "The expression depends on names or types unavailable at the extraction site")
  | Some edits ->
    Some
      (CodeAction.create
         ~title:"Extract function"
         ~kind:CodeActionKind.RefactorExtract
         ~edit:(Document.edit doc edits)
         ~isPreferred:false
         ())
;;

let local = Code_action.batchable RefactorExtract run_extract_local
let function_ = Code_action.batchable RefactorExtract run_extract_function
