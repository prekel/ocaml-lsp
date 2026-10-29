open Import
module H = Stdlib.Hashtbl
open Dune_file

type document =
  { text : string
  ; sexps : sexp list
  }

type declaration =
  { file : string
  ; atom : atom
  ; project : string
  ; directory : string
  ; public : bool
  }

type t =
  { worker : Document.Single_pipeline.t
  ; documents : (string, document) H.t
  ; libraries : (string, declaration list) H.t
  ; aliases : (string, declaration list) H.t
  ; rules : (string, Dune_rule.t list) H.t
  ; mutable roots : string list
  ; mutable projects : string list
  ; mutable dirty : string list
  ; mutable overlays : string list
  ; mutable scanned_at : float
  ; mutable watching : bool
  }

let create thread =
  { worker = Document.Single_pipeline.create thread
  ; documents = H.create 64
  ; libraries = H.create 64
  ; aliases = H.create 32
  ; rules = H.create 32
  ; roots = []
  ; projects = []
  ; dirty = []
  ; overlays = []
  ; scanned_at = 0.
  ; watching = false
  }
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

let under = Reference_index.under

let source_directory name =
  (not (String.is_empty name))
  && name.[0] <> '.'
  && name.[0] <> '_'
  && not (String.equal name "node_modules")
;;

let in_workspace roots file =
  List.exists roots ~f:(fun root ->
    let rec allowed directory =
      String.equal directory root
      || (source_directory (Filename.basename directory)
          &&
          let parent = Filename.dirname directory in
          (not (String.equal parent directory)) && allowed parent)
    in
    under root file && allowed (Filename.dirname file))
;;

let absolute directory file =
  if Filename.is_relative file then Filename.concat directory file else file
;;

let read file =
  try
    let stat = Unix.stat file in
    if stat.st_kind <> Unix.S_REG || stat.st_size > 1_000_000
    then None
    else (
      let channel = open_in_bin file in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
           let length = in_channel_length channel in
           if length > 1_000_000 then None else Some (really_input_string channel length)))
  with
  | Sys_error _ | Unix.Unix_error _ | End_of_file -> None
;;

let put t file text =
  match H.find_opt t.documents file, text with
  | None, None -> false
  | Some _, None ->
    H.remove t.documents file;
    true
  | Some previous, Some text when String.equal previous.text text -> false
  | _, Some text ->
    H.replace t.documents file { text; sexps = parse text };
    true
;;

let project t directory =
  List.find (t.projects @ t.roots) ~f:(fun root -> under root directory)
  |> Option.value ~default:directory
;;

let rebuild t =
  H.clear t.libraries;
  H.clear t.aliases;
  H.clear t.rules;
  let add table file directory public atom =
    let declaration =
      { file
      ; atom
      ; project = project t directory
      ; directory = canonical directory
      ; public
      }
    in
    let previous = Option.value (H.find_opt table atom.value) ~default:[] in
    H.replace table atom.value (declaration :: previous)
  in
  let library file directory fields =
    let name = single (field "name" fields) in
    let public = single (field "public_name" fields) in
    match name, public with
    | Some name, Some public when String.equal name.value public.value ->
      add t.libraries file directory true name
    | _ ->
      Option.iter name ~f:(add t.libraries file directory false);
      Option.iter public ~f:(add t.libraries file directory true)
  in
  let rec stanzas file directory sexps =
    List.iter sexps ~f:(function
      | List (Atom { value = "library"; _ } :: fields, true) ->
        library file directory fields
      | List (Atom { value = "alias"; _ } :: fields, true) ->
        Option.iter (single (field "name" fields)) ~f:(add t.aliases file directory false)
      | List (Atom { value = "rule"; _ } :: fields, true) ->
        Option.iter
          (single (field "alias" fields))
          ~f:(add t.aliases file directory false);
        List.iter (Dune_rule.targets ~directory fields) ~f:(fun (target : Dune_rule.t) ->
          let source = canonical (absolute target.directory target.atom.value) in
          H.replace
            t.rules
            source
            (target :: Option.value (H.find_opt t.rules source) ~default:[]))
      | List (Atom { value = "subdir"; _ } :: Atom path :: rest, true)
        when static path.value -> stanzas file (absolute directory path.value) rest
      | _ -> ())
  in
  H.iter
    (fun file document ->
       if String.equal (Filename.basename file) "dune" && in_workspace t.roots file
       then stanzas file (Filename.dirname file) document.sexps)
    t.documents
;;

let scan t roots =
  let seen = H.create 64 in
  let projects = ref [] in
  let rec walk directory =
    try
      let entries = Sys.readdir directory in
      Array.iter entries ~f:(fun name ->
        let file = Filename.concat directory name in
        if String.equal name "dune-project" then projects := directory :: !projects;
        if String.equal name "dune"
        then (
          H.replace seen file ();
          ignore (put t file (read file) : bool))
        else if source_directory name && (Unix.lstat file).st_kind = Unix.S_DIR
        then walk file)
    with
    | Sys_error _ | Unix.Unix_error _ -> ()
  in
  List.iter roots ~f:walk;
  let removed =
    H.fold (fun file _ acc -> if H.mem seen file then acc else file :: acc) t.documents []
  in
  List.iter removed ~f:(H.remove t.documents);
  let deepest_first =
    List.sort ~compare:(fun a b -> Int.compare (String.length b) (String.length a))
  in
  t.projects <- deepest_first !projects;
  t.roots <- roots;
  t.scanned_at <- Unix.gettimeofday ();
  rebuild t
;;

let invalidate t files = t.dirty <- List.rev_append files t.dirty
let watch_files t = t.watching <- true

let prepare t ~roots ~dirty =
  let roots = List.map roots ~f:canonical |> List.dedup_and_sort ~compare:Poly.compare in
  let dirty =
    List.map dirty ~f:canonical
    |> List.filter ~f:(in_workspace roots)
    |> List.dedup_and_sort ~compare:Poly.compare
  in
  (* Watch notifications provide prompt updates. A periodic scan also covers
     clients without watchers, newly created directories and project markers. *)
  if
    roots <> t.roots
    || ((not t.watching) && Unix.gettimeofday () -. t.scanned_at > 2.)
    || List.exists dirty ~f:(fun file ->
      String.equal (Filename.basename file) "dune-project")
  then scan t roots
  else (
    let changed =
      List.fold_left dirty ~init:false ~f:(fun changed file ->
        let file = canonical file in
        if String.equal (Filename.basename file) "dune" && in_workspace roots file
        then put t file (read file) || changed
        else changed)
    in
    if changed then rebuild t)
;;

let refresh t ~roots =
  let dirty = t.dirty in
  t.dirty <- [];
  Document.Single_pipeline.run t.worker (fun () -> prepare t ~roots ~dirty)
;;

type reference =
  | Library of atom * string * bool
  | Alias of atom * string * bool
  | Module of atom * string * bool
  | File of atom * string

let reference_atom = function
  | Library (atom, _, _) | Alias (atom, _, _) | Module (atom, _, _) | File (atom, _) ->
    atom
;;

let references ~file sexps =
  let rec atoms directory nodes f =
    List.concat_map nodes ~f:(function
      | Atom atom when static atom.value -> [ f atom ]
      | List ([ Atom { value = ":include"; _ }; Atom atom ], _) ->
        [ File (atom, directory) ]
      | List (nodes, _) -> atoms directory nodes f
      | _ -> [])
  in
  let rec stanzas directory sexps =
    List.concat_map sexps ~f:(function
      | List ([ Atom { value = "include"; _ }; Atom atom ], _) ->
        [ File (atom, directory) ]
      | List (Atom { value = "subdir"; _ } :: Atom path :: rest, _) when static path.value
        -> stanzas (absolute directory path.value) rest
      | List (Atom { value = "rule"; _ } :: fields, _) ->
        List.map (Dune_rule.references ~directory fields) ~f:(fun (atom, directory) ->
          File (atom, directory))
        @ fields_reference directory "rule" fields
      | List (Atom stanza :: fields, _) -> fields_reference directory stanza.value fields
      | _ -> [])
  and fields_reference directory stanza fields =
    List.concat_map fields ~f:(function
      | List (Atom key :: values, _) ->
        (match key.value with
         | "libraries" | "implements" ->
           List.filter_map values ~f:(function
             | Atom atom when static atom.value -> Some (Library (atom, directory, false))
             | List ([ Atom { value = "re_export"; _ }; Atom atom ], _)
               when static atom.value -> Some (Library (atom, directory, false))
             | _ -> None)
         | "modules"
         | "modules_without_implementation"
         | "private_modules"
         | "virtual_modules" ->
           atoms directory values (fun atom -> Module (atom, directory, true))
         | ("name" | "names")
           when List.mem
                  [ "executable"; "executables"; "test"; "tests" ]
                  stanza
                  ~equal:String.equal ->
           atoms directory values (fun atom -> Module (atom, directory, false))
         | "deps" | "preprocessor_deps" -> dependencies directory values
         | "preprocess" | "lint" -> preprocess directory values
         | _ -> [])
      | _ -> [])
  and dependencies directory nodes =
    List.concat_map nodes ~f:(function
      | Atom atom -> [ File (atom, directory) ]
      | List ([ Atom { value = "file"; _ }; Atom atom ], _) -> [ File (atom, directory) ]
      | List ([ Atom { value = ("alias" | "alias_rec") as kind; _ }; Atom atom ], _) ->
        [ Alias (atom, directory, String.equal kind "alias_rec") ]
      | List (Atom key :: values, _) when String.is_prefix key.value ~prefix:":" ->
        dependencies directory values
      | _ -> [])
  and preprocess directory nodes =
    List.concat_map nodes ~f:(function
      | List (Atom { value = "per_module"; _ } :: cases, _) ->
        List.concat_map cases ~f:(function
          | List (spec :: modules, _) ->
            preprocess directory [ spec ]
            @ atoms directory modules (fun atom -> Module (atom, directory, true))
          | _ -> [])
      | List (Atom { value = "pps" | "staged_pps"; _ } :: values, _) ->
        let rec pps = function
          | Atom { value = "--"; _ } :: _ -> []
          | Atom atom :: rest ->
            let rest = pps rest in
            if static atom.value && not (String.is_prefix atom.value ~prefix:"-")
            then Library (atom, directory, true) :: rest
            else rest
          | _ -> []
        in
        pps values
      | _ -> [])
  in
  stanzas (Filename.dirname file) sexps
;;

let valid_file value =
  static value
  && not (List.exists [ '*'; '?'; '['; '\n'; '\r' ] ~f:(Stdlib.String.contains value))
;;

type snapshot =
  { roots : string list
  ; documents : (string * string) list
  ; open_files : string list
  ; file : string
  ; encoding : [ `UTF8 | `UTF16 ]
  }

type dependency =
  { name : string
  ; range : Range.t
  ; ppx : bool
  ; targets : Location.t list
  }

type result =
  { targets : Location.t list
  ; dependency : dependency option
  }

let analyze t { roots; documents; open_files; file; encoding } ~offset =
  let dirty = t.dirty in
  t.dirty <- [];
  Document.Single_pipeline.run t.worker (fun () ->
    prepare t ~roots ~dirty;
    let file = canonical file in
    let open_files = List.map open_files ~f:canonical in
    let documents = List.map documents ~f:(fun (file, text) -> canonical file, text) in
    let overlays = List.map documents ~f:fst in
    let changed =
      List.fold_left t.overlays ~init:false ~f:(fun changed file ->
        if List.mem overlays file ~equal:String.equal
        then changed
        else put t file (read file) || changed)
    in
    let changed =
      List.fold_left documents ~init:changed ~f:(fun changed (file, text) ->
        put t file (Some text) || changed)
    in
    t.overlays <- overlays;
    if changed then rebuild t;
    let point file =
      let exists =
        List.mem open_files file ~equal:String.equal
        ||
        try (Unix.stat file).st_kind = Unix.S_REG with
        | Unix.Unix_error _ -> false
      in
      if exists
      then
        Some
          (Location.create
             ~uri:(Uri.of_path file)
             ~range:(Range.create ~start:Position.zero ~end_:Position.zero))
      else None
    in
    let text file =
      match H.find_opt t.documents (canonical file) with
      | Some document -> Some document.text
      | None -> read file
    in
    let resolve_file source =
      let rec rules directory =
        let found =
          Option.value (H.find_opt t.rules directory) ~default:[]
          |> List.filter ~f:(fun (target : Dune_rule.t) ->
            String.equal directory source || target.tree)
        in
        if found <> []
        then found
        else (
          let parent = Filename.dirname directory in
          if String.equal parent directory then [] else rules parent)
      in
      match rules source with
      | [] -> Option.to_list (point source)
      | targets ->
        List.concat_map targets ~f:(fun target ->
          Dune_rule.resolve target ~source ~text ~point)
        |> List.dedup_and_sort ~compare:Poly.compare
    in
    let resolve = function
      | File (atom, directory) ->
        if valid_file atom.value
        then resolve_file (canonical (absolute directory atom.value))
        else []
      | Module (atom, directory, interface) ->
        let name = atom.value in
        if
          (not (static name))
          || List.exists [ ':'; '\\'; '/'; '.' ] ~f:(Stdlib.String.contains name)
        then []
        else (
          let stems =
            List.dedup_and_sort
              [ Stdlib.String.uncapitalize_ascii name; name ]
              ~compare:Poly.compare
          in
          let extensions = if interface then [ ".mli"; ".ml" ] else [ ".ml" ] in
          let target =
            List.find_map extensions ~f:(fun extension ->
              List.find_map stems ~f:(fun stem ->
                match
                  resolve_file (canonical (Filename.concat directory (stem ^ extension)))
                with
                | [] -> None
                | targets -> Some targets))
          in
          Option.value target ~default:[])
      | (Library (atom, directory, _) | Alias (atom, directory, _)) as reference ->
        let table, visible =
          match reference with
          | Library _ ->
            ( t.libraries
            , fun declaration ->
                declaration.public
                || String.equal declaration.project (project t directory) )
          | Alias (_, _, recursive) ->
            let path = canonical (absolute directory atom.value) in
            let directory = Filename.dirname path in
            ( t.aliases
            , fun declaration ->
                if recursive
                then under directory declaration.directory
                else String.equal directory declaration.directory )
          | _ -> assert false
        in
        let name =
          match reference with
          | Alias _ -> Filename.basename atom.value
          | _ -> atom.value
        in
        let candidates () =
          Option.value (H.find_opt table name) ~default:[] |> List.filter ~f:visible
        in
        (* Validate the selected files by content, even without a watcher and
            even when timestamps/lengths did not change. *)
        let changed =
          List.fold_left
            (candidates ())
            ~init:false
            ~f:(fun changed (declaration : declaration) ->
              if List.mem overlays declaration.file ~equal:String.equal
              then changed
              else put t declaration.file (read declaration.file) || changed)
        in
        if changed then rebuild t;
        List.filter_map (candidates ()) ~f:(fun (declaration : declaration) ->
          Option.map (H.find_opt t.documents declaration.file) ~f:(fun document ->
            let start =
              Reference_index.position document.text declaration.atom.first ~encoding
            in
            let end_ =
              Reference_index.position document.text declaration.atom.last ~encoding
            in
            Location.create
              ~uri:(Uri.of_path declaration.file)
              ~range:(Range.create ~start ~end_)))
        |> List.dedup_and_sort ~compare:Poly.compare
    in
    match H.find_opt t.documents file with
    | None -> []
    | Some document ->
      references ~file document.sexps
      |> List.filter ~f:(fun reference ->
        match offset with
        | Some offset ->
          let atom = reference_atom reference in
          atom.first <= offset && offset < atom.last
        | None ->
          (match reference with
           | Library _ -> true
           | _ -> false))
      |> List.map ~f:(fun reference ->
        let targets = resolve reference in
        let dependency =
          match reference with
          | Library (atom, _, ppx) ->
            let start = Reference_index.position document.text atom.first ~encoding in
            let end_ = Reference_index.position document.text atom.last ~encoding in
            Some { name = atom.value; range = Range.create ~start ~end_; ppx; targets }
          | _ -> None
        in
        { targets; dependency }))
;;
