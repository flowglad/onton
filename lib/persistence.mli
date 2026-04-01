(** JSON state persistence for runtime snapshots.

    Saves and loads [Runtime.snapshot] to/from a JSON file on disk. Used for
    crash recovery — the system can resume from the last persisted state rather
    than starting from scratch. *)

val save : path:string -> Runtime.snapshot -> (unit, string) result
(** Write the snapshot to [path] as JSON. Uses atomic rename for crash safety.
    Returns [Error msg] on I/O failure. *)

val load : path:string -> (Runtime.snapshot, string) result
(** Read a snapshot from [path]. Restores the persisted gameplan and
    orchestrator graph directly from the saved JSON. Returns [Error msg] if the
    file is missing, malformed, or incompatible. *)

val snapshot_to_yojson : Runtime.snapshot -> Yojson.Safe.t
(** Convert a snapshot to its JSON representation. *)

val snapshot_of_yojson : Yojson.Safe.t -> (Runtime.snapshot, string) result
(** Parse a snapshot from JSON, restoring the persisted gameplan and
    orchestrator graph. *)

val patch_agent_to_yojson : Patch_agent.t -> Yojson.Safe.t
(** Convert a patch agent to JSON. Exposed for testing. *)

val patch_agent_of_yojson : Yojson.Safe.t -> (Patch_agent.t, string) result
(** Parse a patch agent from the current JSON schema. Exposed for testing. *)
