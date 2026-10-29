open Import

type t =
  { kind : CodeActionKind.t
  ; run :
      [ `Batchable of
          Mpipeline.t -> Document.t -> CodeActionParams.t -> CodeAction.t option
      | `Non_batchable of Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t
      ]
    (** A code action is either "batchable" or "non-batchable". Batchable
      actions do not use fibers internally, so they can be safely run
      inside a [with_pipeline] context. Non-batchable actions can use
      fibers. *)
  }

val batchable
  :  CodeActionKind.t
  -> (Mpipeline.t -> Document.t -> CodeActionParams.t -> CodeAction.t option)
  -> t

val non_batchable
  :  CodeActionKind.t
  -> (Document.t -> CodeActionParams.t -> CodeAction.t option Fiber.t)
  -> t

val source_text : Document.t -> Loc.t -> string option
val disabled : kind:CodeActionKind.t -> title:string -> string -> CodeAction.t
val fresh_name : Document.t -> string -> string
val has_comments : Mpipeline.t -> Range.t -> bool
val relocatable_expression : Typedtree.expression -> Env.t -> bool
val enclosing_nodes : Mpipeline.t -> Range.t -> Mbrowse.t
