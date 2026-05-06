open Base

(** Total mapping from patch-agent stdio RPC events into onton's canonical
    [Types.Stream_event.t].

    Patch-agent exposes stop-reason strings that do not exactly match onton's
    variant names. ["stop"] maps to [Types.Stop_reason.End_turn].
    Patch-agent's ["error"] is also treated as [Types.Stop_reason.End_turn]
    silently because it is an expected value with no direct onton
    counterpart. Fully unknown strings default to
    [Types.Stop_reason.End_turn] and emit a warning to stderr. *)

val parse_stop_reason : string -> Types.Stop_reason.t
val map_event : Patch_agent_rpc.event -> Types.Stream_event.t
