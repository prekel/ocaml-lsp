open Import

type t
type file
type occurrence

exception Cancelled

(** All operations that read typed trees or compiled artifacts must run on
    [Document.Single_pipeline]'s serialized Merlin worker. Cached files contain
    only extracted identities, locations and source text, never an [Env.t]. *)
val create : unit -> t

val canonical : string -> string
val under : string -> string -> bool

val refresh
  :  ?units:string list
  -> t
  -> roots:string list
  -> paths:string list
  -> cancel:Fiber.Cancel.t option
  -> file list * int

val live
  :  ?cancel:Fiber.Cancel.t
  -> cache:t
  -> roots:string list
  -> Mpipeline.t
  -> Document.t
  -> file

val target_units : file -> source:string -> int -> string list option

val lookup
  :  cache:t
  -> files:file list
  -> overlays:file list
  -> source:string
  -> offset:int
  -> include_declaration:bool
  -> cancel:Fiber.Cancel.t option
  -> (file * occurrence) list option * int

val location : encoding:[ `UTF8 | `UTF16 ] -> file * occurrence -> Location.t
val position : string -> int -> encoding:[ `UTF8 | `UTF16 ] -> Position.t
val notice : t -> string -> bool
