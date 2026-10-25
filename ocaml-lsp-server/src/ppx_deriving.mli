open Import

type t

val at_position : Mpipeline.t -> Msource.position -> t option
val for_declaration : Mpipeline.t -> Parsetree.type_declaration -> t option
val range : t -> Range.t
val expansion : Mpipeline.t -> t -> string option
val signature : Mpipeline.t -> t -> string option
