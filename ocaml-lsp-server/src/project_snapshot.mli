open Import

val roots : State.t -> string -> string list

val with_document
  :  State.t
  -> Uri.t
  -> (Document.t -> 'a option Fiber.t)
  -> 'a option Fiber.t

(** Run index operations on the Merlin worker, validating participating buffers,
    workspace folders and temporary documents before returning the result.
    [navigation] restricts the snapshot to the source and selected target sources,
    and tries the compiler load path before scanning project artifacts. It must
    be used with [position] and [include_importers:false]. *)
val run
  :  State.t Server.t
  -> State.t
  -> doc:Document.t
  -> ?position:Position.t
  -> ?include_importers:bool
  -> ?navigation:bool
  -> (roots:string list
      -> files:Reference_index.file list
      -> overlays:Reference_index.file list
      -> cancel:Fiber.Cancel.t option
      -> 'a * int)
  -> 'a Fiber.t
