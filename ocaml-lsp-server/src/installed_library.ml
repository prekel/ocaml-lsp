open Import
module H = Stdlib.Hashtbl
module Cmt = Ocaml_typing.Cmt_format
module Cmi = Ocaml_typing.Cmi_format

let canonical path =
  try Unix.realpath path with
  | Unix.Unix_error _ -> Merlin_utils.Misc.canonicalize_filename path
;;

let is_directory path =
  try (Unix.stat path).st_kind = Unix.S_DIR with
  | Unix.Unix_error _ -> false
;;

let under root path =
  String.equal root path || String.is_prefix path ~prefix:(root ^ Filename.dir_sep)
;;

let root path =
  let path = canonical path in
  let stdlib =
    Option.value
      (Sys.getenv_opt "OCAMLLIB")
      ~default:
        (Option.value
           (Sys.getenv_opt "CAMLLIB")
           ~default:Merlin_kernel.Standard_library.path)
    |> canonical
  in
  let switch = Option.map (Sys.getenv_opt "OPAM_SWITCH_PREFIX") ~f:canonical in
  let rec loop directory =
    let parent = Filename.dirname directory in
    if
      String.equal (Filename.basename directory) "lib"
      && (is_directory (Filename.concat parent ".opam-switch")
          || Option.value_map switch ~default:false ~f:(String.equal parent))
    then Some directory
    else if String.equal parent directory
    then None
    else loop parent
  in
  match loop (Filename.dirname path) with
  | Some _ as root -> root
  | None -> Option.some_if (under stdlib path) stdlib
;;

type stamp = int * int * float * float

let stamp path : stamp option =
  try
    let s = Unix.stat path in
    Some (s.st_ino, s.st_size, s.st_mtime, s.st_ctime)
  with
  | Unix.Unix_error _ -> None
;;

type metadata =
  { unit_name : string
  ; source : string option
  ; digest : string option
  ; args : string list
  ; builddir : string
  ; loadpath : string list
  ; imports : (string * Digest.t option) list
  }

type index =
  { directories : (string * (stamp option * string list)) list
  ; modules : (string, string list) H.t
  }

type file_stamp = stamp option * Digest.t option

type t =
  { artifacts : (string, file_stamp * metadata option) H.t
  ; interfaces : (string, file_stamp * (string * Digest.t option) option) H.t
  ; indices : (string, index) H.t
  }

let create () =
  { artifacts = H.create 16; interfaces = H.create 32; indices = H.create 2 }
;;

let file_stamp path =
  (* Unix.stat timestamps on supported older runtimes can miss rapid in-place
     writes of the same size. Hash the bytes before reusing decoded metadata;
     this still avoids unmarshalling entire typed trees for each query. *)
  let digest =
    try Some (Digest.file path) with
    | Sys_error _ -> None
  in
  stamp path, digest
;;

let cached table path read =
  let current = file_stamp path in
  match H.find_opt table path with
  | Some (previous, value) when Poly.equal current previous -> value
  | _ ->
    let value =
      try Some (read path) with
      | Sys_error _
      | Unix.Unix_error _
      | End_of_file
      | Failure _
      | Invalid_argument _
      | Cmt.Error _
      | Ocaml_typing.Magic_numbers.Cmi.Error _ -> None
    in
    if Poly.equal current (file_stamp path)
    then H.replace table path (current, value)
    else H.remove table path;
    value
;;

let metadata t path =
  cached t.artifacts path (fun path ->
    let cmt = Cmt.read_cmt path in
    { unit_name = cmt.cmt_modname
    ; source = cmt.cmt_sourcefile
    ; digest = cmt.cmt_source_digest
    ; args = Stdlib.Array.to_list cmt.cmt_args
    ; builddir = cmt.cmt_builddir
    ; loadpath = cmt.cmt_loadpath
    ; imports = cmt.cmt_imports
    })
;;

let entries directory =
  try
    Sys.readdir directory
    |> Stdlib.Array.to_list
    |> List.sort ~compare:(fun a b -> Ordering.of_int (Stdlib.String.compare a b))
  with
  | Sys_error _ -> []
;;

let module_index t root =
  match H.find_opt t.indices root with
  | Some index
    when List.for_all index.directories ~f:(fun (path, previous) ->
           Poly.equal previous (stamp path, entries path)) -> index.modules
  | _ ->
    let directories = ref [] in
    let modules = H.create 128 in
    let rec scan directory =
      let names = entries directory in
      directories := (directory, (stamp directory, names)) :: !directories;
      List.iter names ~f:(fun name ->
        let path = Filename.concat directory name in
        (* Do not follow directory symlinks into other switches or cycles. *)
        match Unix.lstat path with
        | s when s.st_kind = Unix.S_DIR -> scan path
        | _ when String.is_suffix name ~suffix:".cmi" ->
          let name = Filename.remove_extension name |> String.capitalize_ascii in
          H.replace
            modules
            name
            (path :: Option.value (H.find_opt modules name) ~default:[])
        | _ -> ()
        | exception Unix.Unix_error _ -> ())
    in
    scan root;
    H.replace t.indices root { directories = !directories; modules };
    modules
;;

let dependency_dirs t root imports =
  let index = module_index t root in
  List.filter_map imports ~f:(fun (name, expected) ->
    let candidates = Option.value (H.find_opt index name) ~default:[] in
    let matching =
      List.filter candidates ~f:(fun path ->
        match
          cached t.interfaces path (fun path ->
            let cmi = Cmi.read_cmi path in
            cmi.cmi_name, Option.join (List.assoc cmi.cmi_crcs cmi.cmi_name))
        with
        | Some (actual_name, Some actual) ->
          String.equal name actual_name
          && Option.value_map expected ~default:false ~f:(Digest.equal actual)
        | _ -> false)
    in
    match matching with
    | path :: _ -> Some (Filename.dirname path)
    | [] -> None)
;;

let source_basename source =
  let source = Filename.basename source in
  if String.is_suffix source ~suffix:".pp.ml"
  then Filename.remove_extension (Filename.remove_extension source) ^ ".ml"
  else if String.is_suffix source ~suffix:".pp.mli"
  then Filename.remove_extension (Filename.remove_extension source) ^ ".mli"
  else source
;;

let find_metadata t path =
  let directory = Filename.dirname path in
  let basename = Filename.basename path in
  let stem = Filename.remove_extension basename |> String.lowercase_ascii in
  let extension = if Filename.extension path = ".mli" then ".cmti" else ".cmt" in
  let candidates =
    entries directory
    |> List.filter_map ~f:(fun name ->
      let unit_stem = Filename.remove_extension name |> String.lowercase_ascii in
      if
        Filename.extension name <> extension
        || not
             (String.equal unit_stem stem
              || String.is_suffix unit_stem ~suffix:("__" ^ stem))
      then None
      else
        let open Option.O in
        let* metadata = metadata t (Filename.concat directory name) in
        let* source = metadata.source in
        Option.some_if (String.equal (source_basename source) basename) metadata)
  in
  match candidates with
  | [] -> None
  | [ candidate ] -> Some candidate
  | candidates ->
    let digest =
      try Some (Digest.file path) with
      | Sys_error _ -> None
    in
    List.find candidates ~f:(fun candidate ->
      Option.value_map digest ~default:false ~f:(fun digest ->
        Option.value_map candidate.digest ~default:false ~f:(Digest.equal digest)))
;;

(* Preserve type-checking options, not stale build commands or preprocessors. *)
let typing_flags args =
  let rec loop = function
    | [] -> []
    | "-open" :: name :: rest -> "-open" :: name :: loop rest
    | (( "-rectypes"
       | "-principal"
       | "-nolabels"
       | "-labels"
       | "-nopervasives"
       | "-no-app-funct"
       | "-strict-sequence"
       | "-strict-formats"
       | "-unsafe"
       | "-safe-string"
       | "-unsafe-string"
       | "-thread" ) as flag)
      :: rest -> flag :: loop rest
    | ( "-pp"
      | "-ppx"
      | "-o"
      | "-I"
      | "-intf-suffix"
      | "-impl"
      | "-intf"
      | "-w"
      | "-warn-error"
      | "-alert" )
      :: _value
      :: rest -> loop rest
    | _ :: rest -> loop rest
  in
  loop args
;;

let config t ~root ~path (initial : Mconfig.t) =
  let path = canonical path in
  let directory = Filename.dirname path in
  let metadata = find_metadata t path in
  let paths, unit_name, flags =
    match metadata with
    | None -> [ directory ], None, []
    | Some metadata ->
      let loadpath =
        List.filter_map metadata.loadpath ~f:(fun path ->
          let path =
            if Filename.is_relative path
            then Filename.concat metadata.builddir path
            else path
          in
          (* A relocated installation must not load modules from its old
             build tree or from another switch. Imported CRCs identify the
             dependencies in the installation we are actually browsing. *)
          Option.some_if (under root (canonical path) && is_directory path) path)
      in
      ( (directory :: dependency_dirs t root metadata.imports) @ loadpath
      , Some metadata.unit_name
      , typing_flags metadata.args )
  in
  let paths =
    List.sort_uniq ~compare:(fun a b -> Ordering.of_int (Stdlib.String.compare a b)) paths
  in
  let stdlib = Filename.concat root "ocaml" in
  let merlin =
    { initial.merlin with
      build_path = paths
    ; source_path = paths
    ; cmt_path = paths
    ; unit_name
    ; stdlib = Option.some_if (is_directory stdlib) stdlib
    }
  in
  let config, () =
    Mconfig.parse_arguments
      ~wd:directory
      ~warning:ignore
      []
      flags
      { initial with merlin }
      ()
  in
  Mconfig.normalize config
;;
