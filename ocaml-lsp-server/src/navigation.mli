open Import

val references : Mpipeline.t -> Msource.position -> include_declaration:bool -> Loc.t list

(** Recover the identifier range for a local Merlin locate result. *)
val definition_range
  :  Mpipeline.t
  -> Lexing.position
  -> type_definition:bool
  -> source_position:Msource.position option
  -> Range.t option
