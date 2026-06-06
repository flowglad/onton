(* @archlint.module interface
   @archlint.domain runtime-logging *)

(** Tiny convenience wrappers around [Telemetry_dispatch.emit] for the two most
    common activity-log shapes. The [Runtime.t] argument is retained so callers
    do not need to know that activity logging is routed through Telemetry sinks.
*)

val log_event : Runtime.t -> ?patch_id:Types.Patch_id.t -> string -> unit
(** Append a free-form event message. [patch_id] is optional because some events
    (startup, reconciliation) aren't scoped to a single patch. *)

val log_stream_entry :
  Runtime.t ->
  patch_id:Types.Patch_id.t ->
  Activity_log.Stream_entry.kind ->
  unit
(** Append a structured stream entry (Tool_use / Text_chunk / Stream_error /
    Finished) — these are always patch-scoped. *)
