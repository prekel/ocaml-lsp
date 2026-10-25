open Import

val prepare
  :  State.t Server.t
  -> State.t
  -> Lsp.Types.CallHierarchyPrepareParams.t
  -> Lsp.Types.CallHierarchyItem.t list option Fiber.t

val incoming
  :  State.t Server.t
  -> State.t
  -> Lsp.Types.CallHierarchyIncomingCallsParams.t
  -> Lsp.Types.CallHierarchyIncomingCall.t list option Fiber.t

val outgoing
  :  State.t Server.t
  -> State.t
  -> Lsp.Types.CallHierarchyOutgoingCallsParams.t
  -> Lsp.Types.CallHierarchyOutgoingCall.t list option Fiber.t
