open Import
open Fiber.O

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

let run_command ?(cwd = Spawn.Working_dir.Inherit) cancel prog stdin_value args =
  Fiber.of_thunk (fun () ->
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
    let stderr_i, stderr_o = Unix.pipe ~cloexec:true () in
    let pid =
      let argv = prog :: args in
      Spawn.spawn ~cwd ~prog ~argv ~stdin:stdin_i ~stdout:stdout_o ~stderr:stderr_o ()
      |> Stdune.Pid.of_int
    in
    Unix.close stdin_i;
    Unix.close stdout_o;
    Unix.close stderr_o;
    let maybe_cancel =
      match cancel with
      | None ->
        fun f ->
          let+ res = f () in
          res, Fiber.Cancel.Not_cancelled
      | Some token ->
        let on_cancel () =
          Unix.kill (Pid.to_int pid) Sys.sigterm;
          Fiber.return ()
        in
        fun f -> Fiber.Cancel.with_handler token ~on_cancel f
    in
    maybe_cancel
    @@ fun () ->
    let blockity =
      if Sys.win32
      then `Blocking
      else (
        Unix.set_nonblock stdin_o;
        Unix.set_nonblock stdout_i;
        `Non_blocking true)
    in
    let make fd what =
      let fd = Lev_fiber.Fd.create fd blockity in
      Lev_fiber.Io.create fd what
    in
    let* stdin_o = make stdin_o Output in
    let* stdout_i = make stdout_i Input in
    let* stderr_i = make stderr_i Input in
    let stdin () =
      Fiber.finalize
        ~finally:(fun () ->
          Lev_fiber.Io.close stdin_o;
          Fiber.return ())
        (fun () ->
           Lev_fiber.Io.with_write stdin_o ~f:(fun w ->
             Lev_fiber.Io.Writer.add_string w stdin_value;
             Lev_fiber.Io.Writer.flush w))
    in
    let read from () =
      Fiber.finalize
        ~finally:(fun () ->
          Lev_fiber.Io.close from;
          Fiber.return ())
        (fun () -> Lev_fiber.Io.with_read from ~f:Lev_fiber.Io.Reader.to_string)
    in
    let+ status, (stdout, stderr) =
      Fiber.fork_and_join
        (fun () -> Lev_fiber.waitpid ~pid:(Pid.to_int pid))
        (fun () ->
           Fiber.fork_and_join_unit stdin (fun () ->
             Fiber.fork_and_join (read stdout_i) (read stderr_i)))
    in
    { stdout; stderr; status })
;;

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

let message = function
  | Unsupported_syntax syntax ->
    sprintf "formatting %s files is not supported" (Document.Syntax.human_name syntax)
  | Missing_binary { binary } ->
    sprintf
      "Unable to find %s binary. You need to install %s manually to use the formatting \
       feature."
      binary
      binary
  | Unknown_extension uri ->
    Printf.sprintf "Unable to format. File %s has an unknown extension" (Uri.to_path uri)
  | Unexpected_result { message } -> message
;;

type formatter =
  | Reason of Document.Kind.t
  | Ocaml of Uri.t

let args = function
  | Ocaml uri ->
    let name = Uri.to_path uri in
    let flag =
      if String.is_suffix name ~suffix:".mli" || String.is_suffix name ~suffix:".ml"
      then []
      else [ "--impl" ]
    in
    flag @ [ sprintf "--name=%s" (Uri.to_path uri); "-" ]
  | Reason kind ->
    [ "--parse"; "re"; "--print"; "re" ]
    @
      (match kind with
      | Impl -> []
      | Intf -> [ "--interface=true" ])
;;

let binary_name t =
  match t with
  | Ocaml _ -> "ocamlformat"
  | Reason _ -> "refmt"
;;

let binary t =
  let name = binary_name t in
  match Bin.which name with
  | None -> Result.Error (Missing_binary { binary = name })
  | Some b -> Ok b
;;

let formatter doc =
  match Document.syntax doc with
  | (Dune | Cram | Ocamllex | Menhir) as s -> Error (Unsupported_syntax s)
  | Ocaml -> Ok (Ocaml (Document.uri doc))
  | Reason ->
    Ok
      (Reason
         (match Document.kind doc with
          | `Merlin m -> Document.Merlin.kind m
          | `Other -> Code_error.raise "unable to format non merlin document" []))
;;

let exec ?cwd cancel refmt args stdin =
  let+ res, cancel = run_command ?cwd cancel refmt stdin args in
  match cancel with
  | Cancelled () ->
    let e = Jsonrpc.Response.Error.make ~code:RequestCancelled ~message:"cancelled" () in
    raise (Jsonrpc.Response.Error.E e)
  | Not_cancelled ->
    (match res.status with
     | Unix.WEXITED 0 -> Result.Ok res
     | _ -> Result.Error (Unexpected_result { message = res.stderr }))
;;

let run doc cancel : (TextEdit.t list, error) result Fiber.t =
  let res =
    let open Result.O in
    let* formatter = formatter doc in
    let args = args formatter in
    let+ binary = binary formatter in
    binary, args, Document.source doc |> Msource.text
  in
  match res with
  | Error e -> Fiber.return (Error e)
  | Ok (binary, args, contents) ->
    exec cancel binary args contents
    |> Fiber.map
         ~f:(Result.map ~f:(fun { stdout = to_; _ } -> Diff.edit ~from:contents ~to_))
;;

(* Snapshot the effective project settings, then run the explicit selection
   request outside its ignore listings. --no-disable alone does not override
   .ocamlformat-ignore: ocamlformat applies ignore files after CLI options. *)
let with_range_args formatter binary cancel padding f =
  match formatter with
  | Reason _ ->
    let margin =
      Sys.getenv_opt "REFMT_PRINT_WIDTH"
      |> Option.bind ~f:Int.of_string
      |> Option.value ~default:80
    in
    f (sprintf "--print-width=%d" (max 1 (margin - padding)) :: args formatter)
  | Ocaml uri ->
    let path = Uri.to_path uri in
    let* configuration =
      exec
        ~cwd:(Spawn.Working_dir.Path (Filename.dirname path))
        cancel
        binary
        [ "--print-config" ]
        ""
    in
    (match configuration with
     | Error e -> Fiber.return (Error e)
     | Ok { stderr = configuration; _ } ->
       let settings =
         String.split_lines configuration
         |> List.filter_map ~f:(fun line ->
           (* Strip provenance, e.g. "margin=60 (file .ocamlformat:2)". *)
           match String.split line ~on:' ' |> List.hd with
           | Some setting
             when List.length (String.split setting ~on:'=') = 2
                  && not (String.is_prefix setting ~prefix:"profile=") ->
             (* Every effective option is printed. Reapplying the profile
                partway through would reset the preceding explicit options. *)
             Some setting
           | _ -> None)
       in
       let margin =
         List.find_map settings ~f:(fun setting ->
           String.drop_prefix setting ~prefix:"margin=" |> Option.bind ~f:Int.of_string)
         |> Option.value ~default:80
       in
       let directory = Filename.temp_file "ocamllsp-selection-" "" in
       Sys.remove directory;
       Unix.mkdir directory 0o700;
       let config_path = Filename.concat directory ".ocamlformat" in
       Fiber.finalize
         ~finally:(fun () ->
           if Sys.file_exists config_path then Sys.remove config_path;
           Unix.rmdir directory;
           Fiber.return ())
         (fun () ->
            let channel = open_out config_path in
            Fun.protect
              ~finally:(fun () -> close_out channel)
              (fun () ->
                 output_string channel (String.concat ~sep:"\n" settings);
                 output_string channel "\ndisable=false\n");
            let extension =
              if String.is_suffix path ~suffix:".mli" then ".mli" else ".ml"
            in
            let uri = Uri.of_path (Filename.concat directory ("selection" ^ extension)) in
            f
              ([ sprintf "--root=%s" directory
               ; "--no-disable"
               ; sprintf "--margin=%d" (max 1 (margin - padding))
               ]
               @ args (Ocaml uri))))
;;

let range_formatter doc =
  match Document.syntax doc with
  | Ocamllex | Menhir -> Ok (Ocaml (Document.uri doc))
  | _ -> formatter doc
;;

let ( let++ ) x f = Fiber.map x ~f:(Result.map ~f)

let whitespace = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false
;;

let format_snippet ~start ~stop ~padding formatter binary cancel contents =
  let selected = String.sub contents ~pos:start ~len:(stop - start) in
  let stripped = String.trim selected ~drop:whitespace in
  if String.is_empty stripped
  then Fiber.return (Ok selected)
  else (
    let leading_length =
      let rec loop index =
        if index < String.length selected && whitespace selected.[index]
        then loop (index + 1)
        else index
      in
      loop 0
    in
    let trailing_start = leading_length + String.length stripped in
    let leading = String.sub selected ~pos:0 ~len:leading_length in
    let trailing =
      String.sub
        selected
        ~pos:trailing_start
        ~len:(String.length selected - trailing_start)
    in
    let padding =
      Base.String.fold leading ~init:padding ~f:(fun column -> function
        | '\n' -> 0
        | _ -> column + 1)
    in
    with_range_args formatter binary cancel padding (fun args ->
      let suffix =
        String.sub contents ~pos:stop ~len:(String.length contents - stop)
        |> String.trim ~drop:whitespace
      in
      let is_in =
        String.is_prefix suffix ~prefix:"in"
        && (String.length suffix = 2
            || not
                 (match suffix.[2] with
                  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
                  | _ -> false))
      in
      let args =
        match formatter with
        | Ocaml _ when is_in || String.is_prefix suffix ~prefix:";;" ->
          "--let-binding-spacing=compact" :: args
        | _ -> args
      in
      let++ { stdout = formatted; _ } = exec cancel binary args stripped in
      let formatted =
        String.split_lines formatted
        |> String.concat ~sep:("\n" ^ Bytes.to_string (Bytes.make padding ' '))
      in
      leading ^ formatted ^ trailing))
;;

let run_on_range doc range cancel : (TextEdit.t list, error) result Fiber.t =
  let contents = Document.text doc in
  let start, stop = Text_document.absolute_range (Document.text_document doc) range in
  if start > stop || Position.compare range.start range.end_ = Ordering.Gt
  then
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:InvalidParams
         ~message:"Invalid formatting range"
         ())
  else if start = stop
  then Fiber.return (Ok [])
  else (
    let res =
      let open Result.O in
      let* formatter = range_formatter doc in
      let+ binary = binary formatter in
      binary, formatter
    in
    match res with
    | Error e -> Fiber.return (Error e)
    | Ok (binary, formatter) ->
      let padding = range.start.character in
      let++ formatted =
        format_snippet formatter binary cancel ~start ~stop ~padding contents
      in
      let selected = String.sub contents ~pos:start ~len:(stop - start) in
      if String.equal selected formatted
      then []
      else [ TextEdit.create ~range ~newText:formatted ])
;;
