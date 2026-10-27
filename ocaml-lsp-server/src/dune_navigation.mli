open Import

type t

val create : Lev_fiber.Thread.t -> t

(** Queue filesystem changes without doing any work on the protocol thread. *)
val invalidate : t -> string list -> unit

(** Enable notification-driven discovery after the client accepts the watcher. *)
val watch_files : t -> unit

val refresh : t -> roots:string list -> unit Fiber.t

(** All filesystem access and parsing runs on the dedicated worker. [documents]
    supplies the current unsaved Dune buffers, replacing their disk contents. *)
val run
  :  t
  -> roots:string list
  -> documents:(string * string) list
  -> open_files:string list
  -> file:string
  -> offset:int
  -> encoding:[ `UTF8 | `UTF16 ]
  -> Location.t list Fiber.t
