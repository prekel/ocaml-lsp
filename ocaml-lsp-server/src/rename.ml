open Import
open Fiber.O

let position_of_offset source offset =
  let (`Logical (line, character)) = Msource.get_logical source (`Offset offset) in
  Position.create ~line:(line - 1) ~character
;;

let is_parenthesized source ~start_offset ~end_offset =
  end_offset - start_offset >= 3
  && Char.equal source.[start_offset] '('
  && Char.equal source.[end_offset - 1] ')'
;;

(* Merlin includes the syntactically required parentheses in symbolic operator
   locations. Rename only the operator so that clients do not use the
   parentheses as part of its name. *)
let identifier_range source loc =
  let range = Range.of_loc loc in
  let (`Offset start_offset) = Msource.get_offset source (Position.logical range.start) in
  let (`Offset end_offset) = Msource.get_offset source (Position.logical range.end_) in
  let source_text = Msource.text source in
  match is_parenthesized source_text ~start_offset ~end_offset with
  | false -> range
  | true ->
    let rec skip_whitespace_forward offset =
      if offset < end_offset - 1 && Base.Char.is_whitespace source_text.[offset]
      then skip_whitespace_forward (offset + 1)
      else offset
    in
    let operator_start = skip_whitespace_forward (start_offset + 1) in
    let rec skip_whitespace_backward offset =
      if operator_start < offset && Base.Char.is_whitespace source_text.[offset - 1]
      then skip_whitespace_backward (offset - 1)
      else offset
    in
    let operator_end = skip_whitespace_backward (end_offset - 1) in
    let rec contains_only_operator_characters offset =
      offset = operator_end
      || (Base.String.mem {|$&*+-/=>@^|~!?%<:.#|} source_text.[offset]
          && contains_only_operator_characters (offset + 1))
    in
    if operator_start < operator_end && contains_only_operator_characters operator_start
    then
      { Range.start = position_of_offset source operator_start
      ; end_ = position_of_offset source operator_end
      }
    else range
;;

let prepare
      (state : State.t)
      { Lsp.Types.PrepareRenameParams.textDocument = { uri }; position; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let+ locations, _ =
      Document.Merlin.dispatch_exn
        ~name:"prepare-rename"
        merlin
        (Query_protocol.Occurrences (`Ident_at (Position.logical position), `Buffer))
    in
    let source = Document.source doc in
    List.find_map locations ~f:(fun location ->
      let range = identifier_range source location in
      if Position.compare_inclusion position range = `Inside then Some range else None)
;;

let rename (state : State.t) { RenameParams.textDocument = { uri }; position; newName; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (WorkspaceEdit.create ())
  | `Merlin merlin ->
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position), `Buffer)
    in
    let+ locs, _status = Document.Merlin.dispatch_exn ~name:"rename" merlin command in
    let version = Document.version doc in
    let source = Document.source doc in
    let edits =
      List.map locs ~f:(fun (loc : Warnings.loc) ->
        let range = identifier_range source loc in
        let make_edit () = TextEdit.create ~range ~newText:newName in
        match
          let occur_start_pos =
            Position.of_lexical_position loc.loc_start |> Option.value_exn
          in
          occur_start_pos
        with
        | { character = 0; _ } -> make_edit ()
        | pos ->
          let mpos = Position.logical pos in
          let (`Offset index) = Msource.get_offset source mpos in
          assert (index > 0)
          (* [index = 0] if we pass [`Logical (1, 0)], but we handle the case
             when [character = 0] in a separate matching branch *);
          let source_txt = Msource.text source in
          (match source_txt.[index - 1] with
           | '~' (* the occurrence is a named argument *)
           | '?' (* is an optional argument *) ->
             let empty_range_at_occur_end =
               let occur_end_pos = range.Range.end_ in
               { range with start = occur_end_pos }
             in
             TextEdit.create ~range:empty_range_at_occur_end ~newText:(":" ^ newName)
           | _ -> make_edit ()))
      |> List.sort_uniq ~compare:(fun (a : TextEdit.t) (b : TextEdit.t) ->
        match Range.compare a.range b.range with
        | Ordering.Eq -> Ordering.of_int (Base.String.compare a.newText b.newText)
        | order -> order)
    in
    let workspace_edits =
      let documentChanges =
        let open Option.O in
        Option.value
          ~default:false
          (let client_capabilities = State.client_capabilities state in
           let* workspace = client_capabilities.workspace in
           let* edit = workspace.workspaceEdit in
           edit.documentChanges)
      in
      if documentChanges
      then (
        let textDocument =
          OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
        in
        let edits = List.map edits ~f:(fun e -> `TextEdit e) in
        WorkspaceEdit.create
          ~documentChanges:
            [ `TextDocumentEdit (TextDocumentEdit.create ~textDocument ~edits) ]
          ())
      else WorkspaceEdit.create ~changes:[ uri, edits ] ()
    in
    workspace_edits
;;
