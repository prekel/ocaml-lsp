open Import

val roots : State.t -> string -> string list

val with_document
  :  State.t
  -> Uri.t
  -> (Document.t -> 'a option Fiber.t)
  -> 'a option Fiber.t

(** Run index operations on the Merlin worker, validating all open buffers,
    workspace folders and temporary documents before returning the result. *)
val run
  :  State.t Server.t
  -> State.t
  -> doc:Document.t
  -> ?position:Position.t
  -> (roots:string list
      -> files:Reference_index.file list
      -> overlays:Reference_index.file list
      -> cancel:Fiber.Cancel.t option
      -> 'a * int)
  -> 'a Fiber.t
