open Base

(** Total mapping from patch-agent stdio RPC events into onton's canonical
    [Types.Stream_event.t].

    Patch-agent exposes stop-reason strings that do not exactly match onton's
    variant names. ["stop"] maps to [Types.Stop_reason.End_turn]. Unsupported
    values, including patch-agent's ["error"], also default to
    [Types.Stop_reason.End_turn]. The mapper emits a warning to stderr when it
    takes that fallback path. *)

val parse_stop_reason : string -> Types.Stop_reason.t

val map_event : Patch_agent_rpc.event -> Types.Stream_event.t
