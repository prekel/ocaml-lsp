open Import

(* Dune's source paths refer to the source tree. A rule-generated unit can exist
   only in its build context. Add the corresponding directories from the same
   configured annotation contexts, without scanning artifacts or guessing units. *)
let with_generated_sources (config : Mconfig.t) =
  let exists file =
    try Sys.file_exists file with
    | Sys_error _ -> false
  in
  let rec context directory =
    let parent = Filename.dirname directory in
    if String.equal parent directory
    then None
    else if exists (Filename.concat parent ".db")
    then Some (directory, parent)
    else context parent
  in
  let directories =
    List.concat_map (Mconfig.cmt_path config) ~f:(fun directory ->
      match context directory with
      | None -> []
      | Some (context, build) ->
        let source_root =
          if String.equal (Filename.basename build) "_build"
          then Some (Filename.dirname build)
          else config.merlin.source_root
        in
        Option.value_map source_root ~default:[] ~f:(fun root ->
          List.filter_map (Mconfig.source_path config) ~f:(fun source ->
            let source = Reference_index.canonical source in
            let relative =
              if String.equal source root
              then Some ""
              else if Reference_index.under root source
              then
                Some
                  (String.sub
                     source
                     ~pos:(String.length root + 1)
                     ~len:(String.length source - String.length root - 1))
              else None
            in
            Option.bind relative ~f:(fun relative ->
              let directory =
                if String.is_empty relative
                then context
                else Filename.concat context relative
              in
              try
                Option.some_if ((Unix.stat directory).st_kind = Unix.S_DIR) directory
              with
              | Unix.Unix_error _ -> None))))
    |> List.dedup_and_sort ~compare:Poly.compare
  in
  { config with
    merlin = { config.merlin with source_path = config.merlin.source_path @ directories }
  }
;;
