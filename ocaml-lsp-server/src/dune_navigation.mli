open Import

type t

val create : Lev_fiber.Thread.t -> t

(** Queue filesystem changes without doing any work on the protocol thread. *)
val invalidate : t -> string list -> unit

(** Enable notification-driven discovery after the client accepts the watcher. *)
val watch_files : t -> unit

val refresh : t -> roots:string list -> unit Fiber.t

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

(** [None] analyzes dependency references for highlighting; [Some] resolves
    only the reference under the cursor. Work runs on the dedicated worker. *)
val analyze : t -> snapshot -> offset:int option -> result list Fiber.t
