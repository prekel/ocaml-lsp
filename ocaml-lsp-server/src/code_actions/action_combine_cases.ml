open Import
open Option.O

let action_kind = "combine-cases"
let kind = CodeActionKind.Other action_kind

let select_complete_lines (range : Range.t) =
  if range.start.line = range.end_.line
  then range
  else (
    let start = { range.start with character = 0 } in
    let end_ =
      if range.end_.character = 0
      then range.end_
      else Position.{ line = range.end_.line + 1; character = 0 }
    in
    Range.create ~start ~end_)
;;

let case_loc (case : Parsetree.case) = Loc.union case.pc_lhs.ppat_loc case.pc_rhs.pexp_loc

let binds_variables pattern =
  let module I = Ast_iterator in
  let bound = ref false in
  let pat iter (p : Parsetree.pattern) =
    (match p.ppat_desc with
     | Ppat_var _ | Ppat_alias _ | Ppat_unpack _ -> bound := true
     | _ -> ());
    I.default_iterator.pat iter p
  in
  let iter = { I.default_iterator with pat } in
  iter.pat iter pattern;
  !bound
;;

let same_body a b =
  let module M = Ocaml_parsing.Ast_mapper in
  let mapper = { M.default_mapper with location = (fun _ _ -> Loc.none) } in
  Poly.equal (mapper.expr mapper a) (mapper.expr mapper b)
;;

let code_action pipeline doc (params : CodeActionParams.t) =
  let range = select_complete_lines params.range in
  let candidates = ref [] in
  let expr iter (e : Parsetree.expression) =
    (match e.pexp_desc with
     | Pexp_match (_, cases) | Pexp_function (_, _, Pfunction_cases (cases, _, _)) ->
       let selected =
         List.filter cases ~f:(fun c -> Range.contains range (Range.of_loc (case_loc c)))
       in
       if List.length selected >= 2 then candidates := selected :: !candidates
     | _ -> ());
    Ast_iterator.default_iterator.expr iter e
  in
  let iter = { Ast_iterator.default_iterator with expr } in
  (match Mpipeline.reader_parsetree pipeline with
   | `Interface _ -> ()
   | `Implementation s -> iter.structure iter s);
  let* cases =
    match !candidates with
    | [ cases ] -> Some cases
    | _ -> None
  in
  let* first = List.hd cases in
  let* last = Base.List.last cases in
  let disabled reason = Some (Code_action.disabled ~kind ~title:"Combine-cases" reason) in
  if
    List.exists cases ~f:(fun c ->
      Option.is_some c.Parsetree.pc_guard || binds_variables c.pc_lhs)
  then disabled "Combining guarded or variable-binding patterns is not supported"
  else if not (List.for_all cases ~f:(fun c -> same_body first.pc_rhs c.Parsetree.pc_rhs))
  then disabled "Only cases with identical bodies can be combined"
  else if Code_action.has_comments pipeline range
  then disabled "Combining these cases would discard comments"
  else (
    let first_range = Range.of_loc first.pc_lhs.ppat_loc in
    let last_range = Range.of_loc last.pc_rhs.pexp_loc in
    let* prefix =
      Text_document.substring
        (Document.text_document doc)
        (Range.create ~start:range.start ~end_:first_range.start)
    in
    let* suffix =
      Text_document.substring
        (Document.text_document doc)
        (Range.create ~start:last_range.end_ ~end_:range.end_)
    in
    let only_space s = String.is_empty (String.strip s) in
    let prefix_without_pipe = Base.String.filter prefix ~f:(fun c -> c <> '|') in
    if not (only_space prefix_without_pipe && only_space suffix)
    then disabled "Select complete adjacent cases from a single match"
    else
      let* patterns =
        List.map cases ~f:(fun c ->
          Text_document.substring
            (Document.text_document doc)
            (Range.of_loc c.Parsetree.pc_lhs.ppat_loc))
        |> Option.all
      in
      let* body =
        Text_document.substring
          (Document.text_document doc)
          (Range.of_loc first.pc_rhs.pexp_loc)
      in
      let rec indent_length i =
        if i < String.length prefix && (prefix.[i] = ' ' || prefix.[i] = '\t')
        then indent_length (i + 1)
        else i
      in
      let indent = String.sub prefix ~pos:0 ~len:(indent_length 0) in
      let newline = if String.is_empty suffix then "" else suffix in
      let newText =
        indent ^ "| " ^ String.concat ~sep:" | " patterns ^ " -> " ^ body ^ newline
      in
      let edit =
        (fun edits -> Text_document.workspace_edit (Document.text_document doc) edits)
          [ TextEdit.create ~range ~newText ]
      in
      Some (CodeAction.create ~title:"Combine-cases" ~kind ~edit ~isPreferred:false ()))
;;

let t = Code_action.batchable kind code_action
