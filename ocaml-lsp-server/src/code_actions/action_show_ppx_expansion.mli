open Import

val kind : CodeActionKind.t
val command_name : string
val available : ClientCapabilities.t -> bool
val t : State.t -> Code_action.t
val command_run : State.t Server.t -> ExecuteCommandParams.t -> Json.t Fiber.t
