(** Tiny convenience wrappers around [Runtime.update_activity_log] for the two
    most common log-line shapes. Both stamp [Unix.gettimeofday ()] internally so
    callers don't need to thread a clock just to drop a line into the activity
    log. *)

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
