open Import
open Fiber.O

let location_of_merlin_loc uri range : _ -> (_, string) result = function
  | `At_origin | `Builtin _ | `Invalid_context | `Not_found _ | `Not_in_env _ -> Ok None
  | `File_not_found s -> Error (sprintf "File_not_found: %s" s)
  | `Found (path, lex_position) ->
    Ok
      (Position.of_lexical_position lex_position
       |> Option.map ~f:(fun position ->
         let range =
           Option.value range ~default:{ Range.start = position; end_ = position }
         in
         let uri =
           match path with
           | None -> uri
           | Some path -> Uri.of_path path
         in
         let locs = [ { Location.uri; range } ] in
         `Location locs))
;;

let run kind (state : State.t) uri position =
  let* () = Fiber.return () in
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    let command, name =
      let pos = Position.logical position in
      match kind with
      | `Definition -> Query_protocol.Locate (None, `ML, pos), "definition"
      | `Declaration -> Query_protocol.Locate (None, `MLI, pos), "declaration"
      | `Type_definition -> Query_protocol.Locate_type pos, "type definition"
    in
    let* result = Document.Merlin.dispatch_exn ~name doc command in
    let* range =
      match result with
      | `Found (path, target_position) ->
        let target_uri = Option.fold path ~init:uri ~f:(fun _ path -> Uri.of_path path) in
        (match Document_store.get_opt state.store target_uri with
         | Some target ->
           (match Document.kind target with
            | `Other -> Fiber.return None
            | `Merlin target ->
              Document.Merlin.with_pipeline_exn
                ~name:"definition range"
                target
                (fun pipeline ->
                   let source_position =
                     if Uri.equal uri target_uri
                     then Some (Position.logical position)
                     else None
                   in
                   Navigation.definition_range
                     pipeline
                     target_position
                     ~type_definition:(kind = `Type_definition)
                     ~source_position))
         | None -> Fiber.return None)
      | _ -> Fiber.return None
    in
    (match location_of_merlin_loc uri range result with
     | Ok s -> Fiber.return s
     | Error err_msg ->
       let kind =
         match kind with
         | `Definition -> "definition"
         | `Declaration -> "declaration"
         | `Type_definition -> "type definition"
       in
       Jsonrpc.Response.Error.raise
         (Jsonrpc.Response.Error.make
            ~code:Jsonrpc.Response.Error.Code.RequestFailed
            ~message:(sprintf "Request \"Jump to %s\" failed." kind)
            ~data:(`String (sprintf "Locate: %s" err_msg))
            ()))
;;
