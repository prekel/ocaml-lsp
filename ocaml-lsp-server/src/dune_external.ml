open Import
open Fiber.O
open Dune_file
module H = Stdlib.Hashtbl

type package =
  { name : string
  ; directory : string
  ; meta : string
  }

type entry =
  { package : package option
  ; findlib_metadata : string option
  ; metadata : string option
  ; parsed : sexp list
  ; targets : Location.t list
  ; checked : float
  }

type cached =
  | Ready of entry
  | Running of entry Fiber.Ivar.t

type t =
  { interactive : Lev_fiber.Thread.t
  ; pool : Fiber.Pool.t
  ; cache : (string, cached) H.t
  ; processes : (int, unit) H.t
  ; mutable stopped : bool
  ; mutable encoding : [ `UTF8 | `UTF16 ]
  }

let create ~interactive ~pool =
  { interactive
  ; pool
  ; cache = H.create 64
  ; processes = H.create 2
  ; stopped = false
  ; encoding = `UTF16
  }
;;

let set_encoding t encoding = t.encoding <- encoding

let kill pid =
  try Unix.kill (if Sys.win32 then pid else -pid) Sys.sigkill with
  | Unix.Unix_error _ -> ()
;;

let stop t =
  t.stopped <- true;
  H.iter (fun pid () -> kill pid) t.processes;
  let running =
    H.fold
      (fun _ entry running ->
         match entry with
         | Ready _ -> running
         | Running ivar -> ivar :: running)
      t.cache
      []
  in
  Fiber.all_concurrently_unit
    (List.map running ~f:(fun ivar ->
       let+ (_ : entry) = Fiber.Ivar.read ivar in
       ()))
;;

let negative () =
  { package = None
  ; findlib_metadata = None
  ; metadata = None
  ; parsed = []
  ; targets = []
  ; checked = Unix.gettimeofday ()
  }
;;

(* The subprocess runs on a separate worker, never on the Dune/Merlin queue.
   Bound both its lifetime and its output; reap it even on timeout or bad output. *)
let query t binary name =
  let input, output = Unix.pipe ~cloexec:true () in
  let null =
    Unix.openfile
      (if Sys.win32 then "NUL" else "/dev/null")
      [ Unix.O_RDWR; Unix.O_CLOEXEC ]
      0
  in
  let pid =
    try
      Spawn.spawn
        ~prog:binary
        ~argv:[ binary; "query"; "-format"; "%p\n%d\n%m"; name ]
        ?setpgid:(if Sys.win32 then None else Some Spawn.Pgid.new_process_group)
        ~stdin:null
        ~stderr:null
        ~stdout:output
        ()
    with
    | exn ->
      Unix.close input;
      Unix.close output;
      Unix.close null;
      raise exn
  in
  H.replace t.processes pid ();
  Unix.close output;
  Unix.close null;
  let reaped = ref false in
  Fun.protect
    ~finally:(fun () ->
      H.remove t.processes pid;
      Unix.close input;
      if not !reaped
      then (
        kill pid;
        try ignore (Unix.waitpid [] pid) with
        | Unix.Unix_error _ -> ()))
    (fun () ->
       let deadline = Unix.gettimeofday () +. 2. in
       let buffer = Buffer.create 256 in
       let bytes = Bytes.create 4096 in
       Unix.set_nonblock input;
       let rec collect () =
         let remaining = deadline -. Unix.gettimeofday () in
         if remaining <= 0. || Buffer.length buffer > 65_536
         then None
         else (
           let ready, _, _ = Unix.select [ input ] [] [] remaining in
           if ready = []
           then None
           else (
             match Unix.read input bytes 0 (Bytes.length bytes) with
             | 0 -> finish ()
             | length ->
               Buffer.add_subbytes buffer bytes 0 length;
               collect ()))
       and finish () =
         match Unix.waitpid [ Unix.WNOHANG ] pid with
         | 0, _ ->
           if Unix.gettimeofday () >= deadline
           then None
           else (
             ignore (Unix.select [] [] [] 0.005);
             finish ())
         | _, status ->
           reaped := true;
           if status <> Unix.WEXITED 0
           then None
           else (
             match
               String.split_lines (Buffer.contents buffer)
               |> List.filter ~f:(fun s -> not (String.is_empty s))
             with
             | [ actual; directory; meta ]
               when String.equal actual name
                    && (not (Filename.is_relative directory))
                    && not (Filename.is_relative meta) -> Some { name; directory; meta }
             | _ -> None)
       in
       if t.stopped then None else collect ())
;;

let exists file =
  try (Unix.stat file).st_kind = Unix.S_REG with
  | Unix.Unix_error _ -> false
;;

let read file =
  try
    let channel = open_in_bin file in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
         let length = in_channel_length channel in
         if length > 1_000_000 then None else Some (really_input_string channel length))
  with
  | Sys_error _ | Unix.Unix_error _ | End_of_file -> None
;;

let point file =
  Location.create
    ~uri:(Uri.of_path file)
    ~range:(Range.create ~start:Position.start ~end_:Position.start)
;;

let absolute directory file =
  if Filename.is_relative file then Filename.concat directory file else file
;;

let metadata_targets package text parsed ~encoding =
  let root = Filename.dirname package.meta in
  let library =
    List.find_map parsed ~f:(function
      | List (Atom { value = "library"; _ } :: fields, true) ->
        (match single (field "name" fields) with
         | Some name when String.equal name.value package.name -> Some (name, fields)
         | _ -> None)
      | _ -> None)
  in
  let source =
    Option.bind library ~f:(fun (_, fields) ->
      Option.bind
        (single (field "main_module_name" fields))
        ~f:(fun main ->
          let rec modules nodes =
            List.concat_map nodes ~f:(function
              | List (Atom { value = "source"; _ } :: fields, true) ->
                (match single (field "path" fields) with
                 | Some name when String.equal name.value main.value -> [ fields ]
                 | _ -> [])
              | List (nodes, _) -> modules nodes
              | _ -> [])
          in
          let source_fields =
            modules (Option.value (field "modules" fields) ~default:[])
          in
          let explicit extension =
            List.concat_map source_fields ~f:(fun fields ->
              match field extension fields with
              | Some [ List ([ Atom { value = "path"; _ }; Atom path ], true) ]
                when static path.value -> [ absolute root path.value ]
              | Some [ Atom path ] when static path.value -> [ absolute root path.value ]
              | _ -> [])
          in
          let stems = [ Stdlib.String.uncapitalize_ascii main.value; main.value ] in
          let conventional extension =
            List.map stems ~f:(fun stem ->
              Filename.concat package.directory (stem ^ extension))
          in
          let candidates =
            explicit "intf" @ conventional ".mli" @ explicit "impl" @ conventional ".ml"
          in
          List.find_map candidates ~f:(fun file ->
            Option.some_if (exists file) (point file))))
  in
  match source, library, text with
  | Some source, _, _ -> [ source ]
  | None, Some (name, _), Some text ->
    let start = Reference_index.position text name.first ~encoding in
    let end_ = Reference_index.position text name.last ~encoding in
    [ Location.create
        ~uri:(Uri.of_path (Filename.concat root "dune-package"))
        ~range:(Range.create ~start ~end_)
    ]
  | _ when exists package.meta -> [ point package.meta ]
  | _ -> []
;;

let compute t binary name previous ~encoding =
  let now = Unix.gettimeofday () in
  let package =
    match previous with
    | Some ({ package = Some package; _ } as previous)
      when exists package.meta
           && (now -. previous.checked < 2.
               || read package.meta = previous.findlib_metadata) -> Some package
    | _ -> Option.bind binary ~f:(fun binary -> query t binary name)
  in
  match package with
  | None -> negative ()
  | Some package ->
    let changed =
      match previous with
      | None -> true
      | Some e -> now -. e.checked >= 2. || e.package <> Some package
    in
    let metadata, parsed =
      match previous with
      | Some e when not changed -> e.metadata, e.parsed
      | _ ->
        let metadata =
          read (Filename.concat (Filename.dirname package.meta) "dune-package")
        in
        metadata, Option.value_map metadata ~default:[] ~f:parse
    in
    let targets = metadata_targets package metadata parsed ~encoding in
    { package = Some package
    ; findlib_metadata =
        (match previous with
         | Some e when not changed -> e.findlib_metadata
         | _ -> read package.meta)
    ; metadata
    ; parsed
    ; targets
    ; checked =
        (match previous with
         | Some e when not changed -> e.checked
         | _ -> now)
    }
;;

let valid_name name =
  (not (String.is_empty name))
  && name.[0] <> '-'
  && Stdlib.String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | '+' -> true
         | _ -> false)
       name
;;

let fresh e = Unix.gettimeofday () -. e.checked < 2.

let await t result =
  let* token = Server.cancel_token () in
  match token with
  | None -> Fiber.Ivar.read result
  | Some token ->
    let waiting = Fiber.Ivar.create () in
    let fill value =
      let* previous = Fiber.Ivar.peek waiting in
      match previous with
      | Some _ -> Fiber.return ()
      | None -> Fiber.Ivar.fill waiting value
    in
    let* () =
      task_if_running t.pool ~f:(fun () ->
        let* value = Fiber.Ivar.read result in
        fill (Some value))
    in
    let+ value, _ =
      Fiber.Cancel.with_handler
        token
        (fun () -> Fiber.Ivar.read waiting)
        ~on_cancel:(fun () -> fill None)
    in
    (match value with
     | Some value -> value
     | None ->
       Jsonrpc.Response.Error.raise
         (Jsonrpc.Response.Error.make
            ~code:RequestCancelled
            ~message:"Dune navigation cancelled"
            ()))
;;

let resolve t name =
  if t.stopped || not (valid_name name)
  then Fiber.return []
  else (
    match H.find_opt t.cache name with
    | Some (Running result) ->
      let+ result = await t result in
      result.targets
    | Some (Ready ({ package = None; _ } as entry)) when fresh entry -> Fiber.return []
    | previous ->
      let previous =
        match previous with
        | Some (Ready e) -> Some e
        | _ -> None
      in
      let result = Fiber.Ivar.create () in
      H.replace t.cache name (Running result);
      let binary = Bin.which "ocamlfind" in
      let* () =
        task_if_running t.pool ~f:(fun () ->
          let* entry =
            match
              Lev_fiber.Thread.task t.interactive ~f:(fun () ->
                try
                  if t.stopped
                  then negative ()
                  else compute t binary name previous ~encoding:t.encoding
                with
                | Sys_error _
                | Unix.Unix_error _
                | End_of_file
                | Failure _
                | Invalid_argument _ -> negative ())
            with
            | Error `Stopped -> Fiber.return (negative ())
            | Ok task ->
              let+ entry = Lev_fiber.Thread.await task in
              (match entry with
               | Ok entry -> entry
               | Error _ -> negative ())
          in
          H.replace t.cache name (Ready entry);
          Fiber.Ivar.fill result entry)
      in
      let+ entry = await t result in
      entry.targets)
;;
