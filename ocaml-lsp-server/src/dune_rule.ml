open Import
open Dune_file

type mode =
  | Standard
  | Fallback
  | Promote of sexp list

type t =
  { atom : atom
  ; directory : string
  ; mode : mode
  ; tree : bool
  }

let absolute directory file =
  if Filename.is_relative file then Filename.concat directory file else file
;;

let rec canonical file =
  try Unix.realpath file with
  | Unix.Unix_error _ ->
    let parent = Filename.dirname file in
    if String.equal parent file
    then file
    else (
      match Filename.basename file with
      | "." -> canonical parent
      | ".." -> Filename.dirname (canonical parent)
      | name -> Filename.concat (canonical parent) name)
;;

let literal value =
  static value
  && not (List.exists [ '*'; '?'; '['; '\n'; '\r' ] ~f:(Stdlib.String.contains value))
;;

let rec action directory = function
  | List (Atom command :: args, _) ->
    (match command.value, args with
     | ("with-stdout-to" | "with-stderr-to" | "with-outputs-to"), Atom output :: actions
       ->
       (if literal output.value then [ output, directory ] else [])
       @ actions_outputs directory actions
     | "write-file", [ Atom output; _ ] ->
       if literal output.value then [ output, directory ] else []
     | ("copy" | "copy#"), [ _; Atom output ] ->
       if literal output.value then [ output, directory ] else []
     | "chdir", Atom path :: actions when literal path.value ->
       actions_outputs (absolute directory path.value) actions
     | "setenv", _ :: _ :: actions -> actions_outputs directory actions
     | "with-stdin-from", _ :: actions -> actions_outputs directory actions
     | ( ("progn" | "concurrent" | "ignore-stdout" | "ignore-stderr" | "ignore-outputs")
       , actions ) -> actions_outputs directory actions
     | _ -> [])
  | _ -> []

and actions_outputs directory nodes = List.concat_map nodes ~f:(action directory)

let actions fields =
  match field "action" fields with
  | Some actions -> actions
  | None -> fields
;;

let mode fields =
  match field "mode" fields with
  | Some [ Atom { value = "promote"; _ } ] -> Promote []
  | Some [ List (Atom { value = "promote"; _ } :: options, _) ] -> Promote options
  | Some [ Atom { value = "fallback"; _ } ] -> Fallback
  | _ -> if Option.is_some (field "fallback" fields) then Fallback else Standard
;;

let targets ~directory fields =
  let mode = mode fields in
  let explicit name =
    Option.value (field name fields) ~default:[]
    |> List.filter_map ~f:(function
      | Atom atom when literal atom.value -> Some { atom; directory; mode; tree = false }
      | List ([ Atom { value = "dir"; _ }; Atom atom ], true) when literal atom.value ->
        Some { atom; directory; mode; tree = true }
      | _ -> None)
  in
  explicit "target"
  @ explicit "targets"
  @ List.map
      (actions_outputs directory (actions fields))
      ~f:(fun (atom, directory) -> { atom; directory; mode; tree = false })
;;

let references ~directory fields =
  let rec inputs directory = function
    | List
        (Atom { value = "copy" | "copy#" | "with-stdin-from"; _ } :: Atom input :: rest, _)
      ->
      (if literal input.value then [ input, directory ] else [])
      @ List.concat_map rest ~f:(inputs directory)
    | List (Atom { value = "chdir"; _ } :: Atom path :: rest, _) when literal path.value
      -> List.concat_map rest ~f:(inputs (absolute directory path.value))
    | List
        ( Atom
            { value =
                ( "progn"
                | "concurrent"
                | "ignore-stdout"
                | "ignore-stderr"
                | "ignore-outputs" )
            ; _
            }
          :: rest
        , _ ) -> List.concat_map rest ~f:(inputs directory)
    | List
        ( Atom { value = "with-stdout-to" | "with-stderr-to" | "with-outputs-to"; _ }
          :: _
          :: rest
        , _ ) -> List.concat_map rest ~f:(inputs directory)
    | List (Atom { value = "setenv"; _ } :: _ :: _ :: rest, _) ->
      List.concat_map rest ~f:(inputs directory)
    | _ -> []
  in
  List.map (targets ~directory fields) ~f:(fun t -> t.atom, t.directory)
  @ List.concat_map (actions fields) ~f:(inputs directory)
;;

let rec predicate filename = function
  | Atom { value = ":standard"; _ } -> Some true
  | Atom pattern when static pattern.value ->
    (try
       Some
         (Re.execp
            (Re.compile (Re.Glob.glob ~anchored:true ~period:false pattern.value))
            filename)
     with
     | Re.Glob.Parse_error -> None)
  | List ([ Atom { value = "not"; _ }; node ], _) ->
    Option.map (predicate filename node) ~f:not
  | List (Atom { value = "or"; _ } :: nodes, _) -> predicates filename nodes false ( || )
  | List (Atom { value = "and"; _ } :: nodes, _) -> predicates filename nodes true ( && )
  | _ -> None

and predicates filename nodes initial combine =
  List.fold_left nodes ~init:(Some initial) ~f:(fun previous node ->
    Option.bind previous ~f:(fun previous ->
      Option.map (predicate filename node) ~f:(combine previous)))
;;

let promoted t =
  match t.mode with
  | Standard -> None
  | Fallback -> Some (absolute t.directory t.atom.value)
  | Promote options ->
    let include_ =
      match field "only" options with
      | None -> Some true
      | Some [ node ] -> predicate t.atom.value node
      | _ -> None
    in
    if include_ <> Some true
    then None
    else (
      match field "into" options with
      | None -> Some (absolute t.directory t.atom.value)
      | Some [ Atom path ] when literal path.value ->
        Some (absolute (absolute t.directory path.value) t.atom.value)
      | _ -> None)
;;

let source_target ~root ~source ~text =
  let source = canonical source in
  let rec stanzas directory nodes =
    List.find_map nodes ~f:(function
      | List (Atom { value = "subdir"; _ } :: Atom path :: rest, true)
        when literal path.value ->
        stanzas (canonical (absolute directory path.value)) rest
      | List (Atom { value = "rule"; _ } :: fields, true) ->
        List.find_map (targets ~directory fields) ~f:(fun target ->
          let original = canonical (absolute target.directory target.atom.value) in
          if String.equal original source
          then Some (target, "")
          else if target.tree && Reference_index.under original source
          then
            Some
              ( target
              , String.sub
                  source
                  ~pos:(String.length original + 1)
                  ~len:(String.length source - String.length original - 1) )
          else None)
      | _ -> None)
  in
  let rec find directory =
    let result =
      Option.bind
        (text (Filename.concat directory "dune"))
        ~f:(fun text -> stanzas directory (parse text))
    in
    match result with
    | Some _ -> result
    | None when String.equal directory root -> None
    | None ->
      let parent = Filename.dirname directory in
      if String.equal parent directory || not (Reference_index.under root parent)
      then None
      else find parent
  in
  find (Filename.dirname source)
;;

let generated_source ~root ~source ~text =
  Option.is_some (source_target ~root ~source ~text)
;;

let promoted_source ~root ~source ~text =
  Option.bind (source_target ~root ~source ~text) ~f:(fun (target, relative) ->
    Option.map (promoted target) ~f:(fun destination ->
      if String.is_empty relative
      then destination
      else Filename.concat destination relative))
;;

let workspace_root ~text directory =
  let rec loop directory workspace project =
    let workspace =
      if Option.is_some (text (Filename.concat directory "dune-workspace"))
      then Some directory
      else workspace
    in
    let project =
      if Option.is_some (text (Filename.concat directory "dune-project"))
      then Some directory
      else project
    in
    let parent = Filename.dirname directory in
    if String.equal parent directory
    then workspace, project
    else loop parent workspace project
  in
  let workspace, project = loop directory None None in
  Option.value
    (match workspace with
     | Some _ -> workspace
     | None -> project)
    ~default:directory
;;

let contexts ~text root =
  let parsed =
    Option.value_map (text (Filename.concat root "dune-workspace")) ~default:[] ~f:parse
  in
  let declared =
    List.filter_map parsed ~f:(function
      | List ([ Atom { value = "context"; _ }; Atom { value = "default"; _ } ], true) ->
        Some ("default", false)
      | List ([ Atom { value = "context"; _ }; List (Atom kind :: fields, true) ], true)
        ->
        let name =
          match single (field "name" fields) with
          | Some name -> Some name.value
          | None when String.equal kind.value "default" -> Some "default"
          | None when String.equal kind.value "opam" ->
            Option.map (single (field "switch" fields)) ~f:(fun switch -> switch.value)
          | _ -> None
        in
        Option.map name ~f:(fun name -> name, Option.is_some (field "merlin" fields))
      | _ -> None)
  in
  let selected =
    List.filter_map declared ~f:(fun (name, merlin) -> Option.some_if merlin name)
  in
  let primary = if selected <> [] then selected else [ "default" ] in
  let build =
    absolute root (Option.value (Sys.getenv_opt "DUNE_BUILD_DIR") ~default:"_build")
  in
  let found =
    try
      Sys.readdir build
      |> Array.to_list
      |> List.filter ~f:(fun name ->
        static name
        && name.[0] <> '.'
        && name.[0] <> '_'
        && (not (String.equal name "install"))
        &&
        try (Unix.stat (Filename.concat build name)).st_kind = Unix.S_DIR with
        | Unix.Unix_error _ -> false)
    with
    | Sys_error _ | Unix.Unix_error _ -> []
  in
  let remaining =
    List.map declared ~f:fst @ found
    |> List.sort_uniq ~compare:Poly.compare
    |> List.filter ~f:(fun name -> not (List.mem primary name ~equal:String.equal))
  in
  build, primary, remaining
;;

let resolve t ~source ~text ~point =
  let original = canonical (absolute t.directory t.atom.value) in
  let promoted =
    Option.bind (promoted t) ~f:(fun destination ->
      let relative =
        if String.equal source original
        then ""
        else
          String.sub
            source
            ~pos:(String.length original + 1)
            ~len:(String.length source - String.length original - 1)
      in
      point
        (canonical
           (if String.is_empty relative
            then destination
            else Filename.concat destination relative)))
  in
  match promoted with
  | Some target -> [ target ]
  | None ->
    let root = workspace_root ~text t.directory in
    if not (Reference_index.under root source)
    then []
    else (
      let relative =
        String.sub
          source
          ~pos:(String.length root + 1)
          ~len:(String.length source - String.length root - 1)
      in
      let build, primary, remaining = contexts ~text root in
      let targets names =
        List.filter_map names ~f:(fun context ->
          point (Filename.concat (Filename.concat build context) relative))
      in
      match targets primary with
      | [] -> targets remaining
      | targets -> targets)
;;
