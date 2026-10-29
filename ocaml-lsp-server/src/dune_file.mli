type atom =
  { value : string
  ; first : int
  ; last : int
  }

type sexp =
  | Atom of atom
  | List of sexp list * bool

val parse : string -> sexp list
val static : string -> bool
val field : string -> sexp list -> sexp list option
val single : sexp list option -> atom option
