open Import
open Fiber.O

type origin =
  | Local
  | External

type dependency =
  { reference : Dune_navigation.dependency
  ; origin : origin
  }

let roots state =
  Workspaces.workspace_folders (State.workspaces state)
  |> List.map ~f:(fun (folder : WorkspaceFolder.t) -> Uri.to_path folder.uri)
;;

let modified () =
  Jsonrpc.Response.Error.raise
    (Jsonrpc.Response.Error.make
       ~code:ContentModified
       ~message:"A Dune document changed during navigation"
       ())
;;

let validate state doc =
  match Document_store.get_opt state.State.store (Document.uri doc) with
  | Some current
    when Document.version current = Document.version doc
         && String.equal (Document.text current) (Document.text doc) -> ()
  | _ -> modified ()
;;

let analyze state doc ~offset =
  validate state doc;
  let revision = !(state.State.workspace_revision) in
  let all () = Document_store.fold state.State.store ~init:[] ~f:List.cons in
  let dune docs = List.filter docs ~f:(fun doc -> Document.syntax doc = Dune) in
  let open_documents = all () in
  let before = dune open_documents in
  let snapshot =
    { Dune_navigation.roots = roots state
    ; documents =
        List.map before ~f:(fun doc -> Uri.to_path (Document.uri doc), Document.text doc)
    ; open_files = List.map open_documents ~f:(fun doc -> Uri.to_path (Document.uri doc))
    ; file = Uri.to_path (Document.uri doc)
    ; encoding = State.position_encoding state
    }
  in
  let+ results = Dune_navigation.analyze state.dune_navigation snapshot ~offset in
  let check targets =
    if revision <> !(state.workspace_revision) then modified ();
    if List.length before <> List.length (dune (all ())) then modified ();
    List.iter before ~f:(validate state);
    List.iter targets ~f:(fun (target : Location.t) ->
      match
        ( List.find open_documents ~f:(fun doc -> Uri.equal (Document.uri doc) target.uri)
        , Document_store.get_opt state.store target.uri )
      with
      | None, None -> ()
      | Some previous, Some _ -> validate state previous
      | _ -> modified ())
  in
  results, check
;;

let definition state doc position =
  let offset = Text_document.absolute_position (Document.text_document doc) position in
  let* results, check = analyze state doc ~offset:(Some offset) in
  let* targets =
    match results with
    | [] -> Fiber.return []
    | { Dune_navigation.targets = []; dependency = Some dependency } :: _ ->
      Dune_external.resolve state.State.dune_external ~background:false dependency.name
    | result :: _ -> Fiber.return result.targets
  in
  check targets;
  Fiber.return (Some (`Location targets))
;;

let highlighting state doc =
  let* results, check = analyze state doc ~offset:None in
  let pending = ref [] in
  let dependencies =
    List.filter_map results ~f:(fun (result : Dune_navigation.result) ->
      Option.bind result.dependency ~f:(fun reference ->
        if reference.targets <> []
        then Some { reference; origin = Local }
        else (
          pending := reference.name :: !pending;
          let targets = Dune_external.peek state.State.dune_external reference.name in
          Option.some_if
            (targets <> [])
            { reference = { reference with targets }; origin = External })))
  in
  let* () = Dune_external.prefetch state.State.dune_external !pending in
  check (List.concat_map dependencies ~f:(fun d -> d.reference.targets));
  Fiber.return dependencies
;;
