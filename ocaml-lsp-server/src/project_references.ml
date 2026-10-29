open Import
open Fiber.O

let fallback merlin doc position include_declaration =
  let text = Document.text doc in
  let offset = Text_document.absolute_position (Document.text_document doc) position in
  let logical = Msource.get_logical (Document.source doc) (`Offset offset) in
  let+ locations =
    Document.Merlin.with_pipeline_exn ~name:"references" merlin (fun pipeline ->
      Navigation.references pipeline logical ~include_declaration)
  in
  let encoding = Text_document.position_encoding (Document.text_document doc) in
  List.filter_map locations ~f:(fun loc ->
    let first = loc.Loc.loc_start.pos_cnum
    and last = loc.loc_end.pos_cnum in
    if first < 0 || last < first || last > String.length text
    then None
    else
      Some
        (Location.create
           ~uri:(Document.uri doc)
           ~range:
             (Range.create
                ~start:(Reference_index.position text first ~encoding)
                ~end_:(Reference_index.position text last ~encoding))))
;;

let compare_location (a : Location.t) (b : Location.t) =
  match Poly.compare (Uri.to_string a.uri) (Uri.to_string b.uri) with
  | 0 -> Range.compare a.range b.range
  | order -> order
;;

let run
      rpc
      ~doc
      ~native
      (state : State.t)
      { ReferenceParams.textDocument = { uri }; position; context; _ }
  =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let source = Reference_index.canonical (Uri.to_path uri) in
    let* found =
      if Project_snapshot.roots state source = []
      then Fiber.return None
      else
        Project_snapshot.run
          rpc
          state
          ~doc
          ~position
          (fun ~roots:_ ~files ~overlays ~cancel ->
             let offset =
               Text_document.absolute_position (Document.text_document doc) position
             in
             let result, stale =
               Reference_index.lookup
                 ~cache:state.reference_index
                 ~files
                 ~overlays
                 ~source
                 ~offset
                 ~include_declaration:context.includeDeclaration
                 ~cancel
             in
             ( Option.map
                 result
                 ~f:
                   (List.map
                      ~f:
                        (Reference_index.location
                           ~encoding:(State.position_encoding state)))
             , stale ))
    in
    let+ locations =
      match found with
      | Some locations -> Fiber.return locations
      | None ->
        let* result = native () in
        (match result with
         | Some xs -> Fiber.return xs
         | None -> fallback merlin doc position context.includeDeclaration)
    in
    Some (List.dedup_and_sort locations ~compare:compare_location)
;;
