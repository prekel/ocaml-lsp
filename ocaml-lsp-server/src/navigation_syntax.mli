type t
type document
type declaration

val create : unit -> t

(** Content-validated cache of at most 32 file versions; skip parsing files over
    1 MB. Run on the Merlin worker. The built-in reader uses no project flags,
    preprocessors or typing, and only extracted declaration locations are kept. *)
val document : t -> file:string -> text:string -> document

val at : document -> offset:int -> declaration option
val locate : document -> Lexing.position -> name:string option -> declaration option
val offsets : declaration -> int * int

(** Match an exported declaration through its module path and namespace. Refuse
    uncertain includes or PPX forms, recovered syntax, local and hidden bindings.
    Callers establish that the documents are revisions or exact counterparts. *)
val counterpart : source:document -> target:document -> declaration -> declaration option
