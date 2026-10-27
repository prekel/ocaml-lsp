open Import

type origin =
  | Local
  | External

type dependency =
  { reference : Dune_navigation.dependency
  ; origin : origin
  }

val roots : State.t -> string list

val definition
  :  State.t
  -> Document.t
  -> Position.t
  -> [> `Location of Location.t list ] option Fiber.t

val highlighting : State.t -> Document.t -> dependency list Fiber.t
