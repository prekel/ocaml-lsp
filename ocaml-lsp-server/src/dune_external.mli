open Import

type t

val create : interactive:Lev_fiber.Thread.t -> pool:Fiber.Pool.t -> t
val stop : t -> unit Fiber.t
val set_encoding : t -> [ `UTF8 | `UTF16 ] -> unit
val resolve : t -> string -> Location.t list Fiber.t
