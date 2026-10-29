open Import

(** Installed OCaml sources are configured independently of the enclosing
    workspace. Artifact access must run on the Merlin worker. *)
type t

val create : unit -> t

(** Canonical library root for a source in a verified opam switch or the
    running compiler's standard library. *)
val root : string -> string option

val config : t -> root:string -> path:string -> Mconfig.t -> Mconfig.t
