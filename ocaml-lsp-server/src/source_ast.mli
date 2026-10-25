open Import

(** Positions and bindings in the reader AST, before PPX expansion. Generated
    nodes may reuse these locations without setting [loc_ghost]. *)
type t

val create
  :  [ `Implementation of Parsetree.structure | `Interface of Parsetree.signature ]
  -> t

val has_pattern_end : t -> Lexing.position -> bool
val contains_outline : t -> Query_protocol.item -> bool

type type_target =
  | Declaration of Parsetree.type_declaration
  | Core_type of Parsetree.type_declaration

(** Resolve a type hover within its original declaration, rather than a PPX
    binding sharing the declaration's location. *)
val type_enclosing
  :  Mpipeline.t
  -> type_target
  -> Msource.position
  -> (Loc.t * string) option
