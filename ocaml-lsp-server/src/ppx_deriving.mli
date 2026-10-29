open Import

type t

val at_position : Mpipeline.t -> Msource.position -> t option
val for_declaration : Mpipeline.t -> Parsetree.type_declaration -> t option
val range : t -> Range.t
val expansion : Mpipeline.t -> t -> string option
val signature : Mpipeline.t -> t -> string option

val signature_in_region
  :  ?exclude:Parsetree.type_declaration list
  -> Mpipeline.t
  -> Loc.t
  -> string option

val has_errors : Mpipeline.t -> Loc.t -> bool

(** Exported declarations only, including within module signatures. *)
val public_signature : Types.signature -> Types.signature
