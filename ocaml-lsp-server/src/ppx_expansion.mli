open Import

type t

(** Extension points and attributes whose source has changed after PPX. Hover
    targets the extension name; code actions cover the whole extension. *)
val at_position : ?hover:bool -> Mpipeline.t -> Msource.position -> t option

val for_declaration : Mpipeline.t -> Parsetree.type_declaration -> t option
val name : t -> string
val range : t -> Range.t
val expansion : Mpipeline.t -> t -> string option
val signature : Mpipeline.t -> t -> string option
