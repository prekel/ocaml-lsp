open Import
module H = Stdlib.Hashtbl

type atom =
  { value : string
  ; first : int
  ; last : int
  }

type sexp =
  | Atom of atom
  | List of sexp list * bool

(* A small, location-preserving Dune reader. It never evaluates actions,
   variables or includes. Incomplete lists are useful for source navigation,
   but are never indexed as declarations. *)
let parse text =
  let length = String.length text in
  let cursor = ref 0 in
  let nodes = ref 0 in
  let exception Invalid in
  let whitespace = function
    | ' ' | '\t' | '\r' | '\n' | '\012' -> true
    | _ -> false
  in
  let rec skip () =
    if !cursor < length
    then (
      match text.[!cursor] with
      | ';' ->
        while !cursor < length && text.[!cursor] <> '\n' do
          incr cursor
        done;
        skip ()
      | c when whitespace c ->
        incr cursor;
        skip ()
      | _ -> ())
  in
  let rec sequence depth nested acc =
    skip ();
    if !cursor = length
    then List.rev acc, not nested
    else if text.[!cursor] = ')'
    then (
      incr cursor;
      if nested then List.rev acc, true else raise Invalid)
    else (
      incr nodes;
      if depth > 128 || !nodes > 100_000 then raise Invalid;
      let node =
        match text.[!cursor] with
        | '(' ->
          incr cursor;
          let items, complete = sequence (depth + 1) true [] in
          List (items, complete)
        | '"' ->
          incr cursor;
          let first = !cursor in
          if
            !cursor + 1 < length
            && text.[!cursor] = '\\'
            && (text.[!cursor + 1] = '|' || text.[!cursor + 1] = '>')
          then (
            (* End-of-line strings are opaque: parentheses in a shell script
               must not turn into Dune declarations or references. *)
            while !cursor < length && text.[!cursor] <> '\n' do
              incr cursor
            done;
            Atom { value = ""; first; last = !cursor })
          else (
            while !cursor < length && text.[!cursor] <> '"' do
              if text.[!cursor] = '\\' then incr cursor;
              if !cursor < length then incr cursor
            done;
            if !cursor = length then raise Invalid;
            let last = !cursor in
            incr cursor;
            let raw = String.sub text ~pos:first ~len:(last - first) in
            let value =
              try Scanf.unescaped raw with
              | Scanf.Scan_failure _ | Failure _ -> ""
            in
            Atom { value; first; last })
        | _ ->
          let first = !cursor in
          while
            !cursor < length
            && (not (whitespace text.[!cursor]))
            && not (List.mem [ '('; ')'; ';'; '"' ] text.[!cursor] ~equal:Char.equal)
          do
            incr cursor
          done;
          if !cursor = first then raise Invalid;
          Atom
            { value = String.sub text ~pos:first ~len:(!cursor - first)
            ; first
            ; last = !cursor
            }
      in
      sequence depth nested (node :: acc))
  in
  if length > 1_000_000
  then []
  else (
    try fst (sequence 0 false []) with
    | Invalid -> [])
;;

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

let static value =
  (not (String.is_empty value))
  && Option.is_none (String.substr_index value ~pattern:"%{")
  && not (Stdlib.String.contains value '\000')
;;

let field name fields =
  List.find_map fields ~f:(function
    | List (Atom key :: values, true) when String.equal key.value name -> Some values
    | _ -> None)
;;

let single = function
  | Some [ Atom atom ] when static atom.value -> Some atom
  | _ -> None
;;

let project t directory =
  List.find (t.projects @ t.roots) ~f:(fun root -> under root directory)
  |> Option.value ~default:directory
;;

let rebuild t =
  H.clear t.libraries;
  H.clear t.aliases;
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
          ~f:(add t.aliases file directory false)
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
  let roots = List.map roots ~f:canonical |> List.sort_uniq ~compare:Poly.compare in
  let dirty =
    List.map dirty ~f:canonical
    |> List.filter ~f:(in_workspace roots)
    |> List.sort_uniq ~compare:Poly.compare
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
  | Library of atom * string
  | Alias of atom * string * bool
  | Module of atom * string * bool
  | File of atom * string

let reference ~file sexps offset =
  let selected atom = atom.first <= offset && offset < atom.last in
  let rec atoms directory nodes f =
    List.find_map nodes ~f:(function
      | Atom atom when selected atom && static atom.value -> f atom
      | List (Atom { value = ":include"; _ } :: [ Atom atom ], _) ->
        if selected atom then Some (File (atom, directory)) else None
      | List (nodes, _) -> atoms directory nodes f
      | _ -> None)
  in
  let rec stanzas directory sexps =
    List.find_map sexps ~f:(function
      | List (Atom { value = "include"; _ } :: [ Atom atom ], _) when selected atom ->
        Some (File (atom, directory))
      | List (Atom { value = "subdir"; _ } :: Atom path :: rest, _) when static path.value
        -> stanzas (absolute directory path.value) rest
      | List (Atom stanza :: fields, _) -> fields_reference directory stanza.value fields
      | _ -> None)
  and fields_reference directory stanza fields =
    List.find_map fields ~f:(function
      | List (Atom key :: values, _) ->
        (match key.value with
         | "libraries" | "implements" ->
           List.find_map values ~f:(function
             | Atom atom when selected atom && static atom.value ->
               Some (Library (atom, directory))
             | List ([ Atom { value = "re_export"; _ }; Atom atom ], _)
               when selected atom && static atom.value -> Some (Library (atom, directory))
             | _ -> None)
         | "modules"
         | "modules_without_implementation"
         | "private_modules"
         | "virtual_modules" ->
           atoms directory values (fun atom -> Some (Module (atom, directory, true)))
         | ("name" | "names")
           when List.mem
                  [ "executable"; "executables"; "test"; "tests" ]
                  stanza
                  ~equal:String.equal ->
           atoms directory values (fun atom -> Some (Module (atom, directory, false)))
         | "deps" | "preprocessor_deps" -> dependencies directory values
         | "preprocess" | "lint" -> preprocess directory values
         | _ -> None)
      | _ -> None)
  and dependencies directory nodes =
    List.find_map nodes ~f:(function
      | Atom atom when selected atom -> Some (File (atom, directory))
      | List ([ Atom { value = "file"; _ }; Atom atom ], _) when selected atom ->
        Some (File (atom, directory))
      | List ([ Atom { value = ("alias" | "alias_rec") as kind; _ }; Atom atom ], _)
        when selected atom ->
        Some (Alias (atom, directory, String.equal kind "alias_rec"))
      | List (Atom key :: values, _) when String.is_prefix key.value ~prefix:":" ->
        dependencies directory values
      | _ -> None)
  and preprocess directory nodes =
    List.find_map nodes ~f:(function
      | List (Atom { value = "per_module"; _ } :: cases, _) ->
        List.find_map cases ~f:(function
          | List (spec :: modules, _) ->
            (match preprocess directory [ spec ] with
             | Some _ as result -> result
             | None ->
               atoms directory modules (fun atom -> Some (Module (atom, directory, true))))
          | _ -> None)
      | List (Atom { value = "pps" | "staged_pps"; _ } :: values, _) ->
        let rec pps = function
          | Atom { value = "--"; _ } :: _ -> None
          | Atom atom :: rest ->
            if
              selected atom
              && static atom.value
              && not (String.is_prefix atom.value ~prefix:"-")
            then Some (Library (atom, directory))
            else pps rest
          | _ -> None
        in
        pps values
      | _ -> None)
  in
  stanzas (Filename.dirname file) sexps
;;

let valid_file value =
  static value
  && not (List.exists [ '*'; '?'; '['; '\n'; '\r' ] ~f:(Stdlib.String.contains value))
;;

let run t ~roots ~documents ~open_files ~file ~offset ~encoding =
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
             ~range:(Range.create ~start:Position.start ~end_:Position.start))
      else None
    in
    match H.find_opt t.documents file with
    | None -> []
    | Some document ->
      (match reference ~file document.sexps offset with
       | None -> []
       | Some (File (atom, directory)) ->
         if valid_file atom.value
         then Option.to_list (point (canonical (absolute directory atom.value)))
         else []
       | Some (Module (atom, directory, interface)) ->
         let name = atom.value in
         if
           (not (static name))
           || List.exists [ ':'; '\\'; '/'; '.' ] ~f:(Stdlib.String.contains name)
         then []
         else (
           let stems =
             List.sort_uniq
               [ Stdlib.String.uncapitalize_ascii name; name ]
               ~compare:Poly.compare
           in
           let extensions = if interface then [ ".mli"; ".ml" ] else [ ".ml" ] in
           let target =
             List.find_map extensions ~f:(fun extension ->
               List.find_map stems ~f:(fun stem ->
                 point (canonical (Filename.concat directory (stem ^ extension)))))
           in
           Option.to_list target)
       | Some ((Library (atom, directory) | Alias (atom, directory, _)) as reference) ->
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
           List.fold_left (candidates ()) ~init:false ~f:(fun changed declaration ->
             if List.mem overlays declaration.file ~equal:String.equal
             then changed
             else put t declaration.file (read declaration.file) || changed)
         in
         if changed then rebuild t;
         List.filter_map (candidates ()) ~f:(fun declaration ->
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
         |> List.sort_uniq ~compare:Poly.compare))
;;
