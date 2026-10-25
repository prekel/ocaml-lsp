(** Fetch merlin configuration with dune subprocesses *)

open Import

type t

val should_read_dot_merlin : bool ref
val config : t -> Mconfig.t Fiber.t
val destroy : t -> unit Fiber.t

(** Installed library sources are browsed without project diagnostics. *)
val is_installed : t -> bool

module DB : sig
  type config := t
  type t

  val create : worker:Lev_fiber.Thread.t -> unit -> t
  val stop : t -> unit Fiber.t
  val run : t -> unit Fiber.t
  val get : t -> Uri.t -> config
end
