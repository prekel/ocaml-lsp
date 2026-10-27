open Import

type t

val create
  :  interactive:Lev_fiber.Thread.t
  -> background:Lev_fiber.Thread.t
  -> pool:Fiber.Pool.t
  -> t Fiber.t

val stop : t -> unit Fiber.t
val set_on_change : t -> (unit -> unit Fiber.t) -> unit
val set_encoding : t -> [ `UTF8 | `UTF16 ] -> unit
val resolve : t -> background:bool -> string -> Location.t list Fiber.t
val peek : t -> string -> Location.t list
val prefetch : t -> string list -> unit Fiber.t
val poll : t -> active:(unit -> bool) -> unit Fiber.t
