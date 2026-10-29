open Import
open Dune_file

type t =
  { atom : atom
  ; directory : string
  ; mode : mode
  ; tree : bool
  }

and mode =
  | Standard
  | Fallback
  | Promote of sexp list

val targets : directory:string -> sexp list -> t list
val references : directory:string -> sexp list -> (atom * string) list
val promoted : t -> string option

val promoted_source
  :  root:string
  -> source:string
  -> text:(string -> string option)
  -> string option

val generated_source
  :  root:string
  -> source:string
  -> text:(string -> string option)
  -> bool

val resolve
  :  t
  -> source:string
  -> text:(string -> string option)
  -> point:(string -> Location.t option)
  -> Location.t list
