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
  -> ?include_importers:bool
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

(** Matching annotations for selected installed units only; no dependency-body scan. *)
val supporting_files
  :  t
  -> files:file list
  -> units:string list
  -> cancel:Fiber.Cancel.t option
  -> file list

val called_units : file list -> string list
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

module Call_hierarchy : sig
  type graph

  val create
    :  cache:t
    -> files:file list
    -> overlays:file list
    -> roots:string list
    -> cancel:Fiber.Cancel.t option
    -> graph * int

  val prepare
    :  graph
    -> encoding:[ `UTF8 | `UTF16 ]
    -> source:string
    -> offset:int
    -> Lsp.Types.CallHierarchyItem.t list

  val validate : cache:t -> Lsp.Types.CallHierarchyItem.t -> unit

  val incoming
    :  graph
    -> encoding:[ `UTF8 | `UTF16 ]
    -> Lsp.Types.CallHierarchyItem.t
    -> Lsp.Types.CallHierarchyIncomingCall.t list

  val outgoing
    :  graph
    -> encoding:[ `UTF8 | `UTF16 ]
    -> Lsp.Types.CallHierarchyItem.t
    -> Lsp.Types.CallHierarchyOutgoingCall.t list
end
