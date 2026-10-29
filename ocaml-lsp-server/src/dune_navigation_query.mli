open Import

val roots : State.t -> string list

val definition
  :  State.t
  -> Document.t
  -> Position.t
  -> [> `Location of Location.t list ] option Fiber.t

val highlighting : State.t -> Document.t -> Dune_navigation.dependency list Fiber.t
