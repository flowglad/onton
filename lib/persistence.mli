(** JSON state persistence for runtime snapshots.

    Saves and loads [Runtime.snapshot] to/from a JSON file on disk. Used for
    crash recovery — the system can resume from the last persisted state rather
    than starting from scratch. *)

val save : path:string -> Runtime.snapshot -> (unit, string) result
(** Write the snapshot to [path] as JSON. Uses atomic rename for crash safety.
    Returns [Error msg] on I/O failure. *)

val load :
  path:string -> gameplan:Types.Gameplan.t -> (Runtime.snapshot, string) result
(** Read a snapshot from [path]. The [gameplan] is needed to reconstruct the
    dependency graph (which is derived, not stored). The gameplan stored in the
    JSON is ignored — the caller-provided [gameplan] is used as the canonical
    source. Returns [Error msg] if the file is missing, malformed, incompatible,
    or if the persisted agent IDs don't match the gameplan's patch IDs. *)

val snapshot_to_yojson : Runtime.snapshot -> Yojson.Safe.t
(** Convert a snapshot to its JSON representation. *)

val snapshot_of_yojson :
  gameplan:Types.Gameplan.t ->
  Yojson.Safe.t ->
  (Runtime.snapshot, string) result
(** Parse a snapshot from JSON. *)

val patch_agent_to_yojson : Patch_agent.t -> Yojson.Safe.t
(** Convert a patch agent to JSON. Exposed for testing. *)

val patch_agent_of_yojson : Yojson.Safe.t -> (Patch_agent.t, string) result
(** Parse a patch agent from JSON. Supports backward-compat migration from
    legacy [session_failed]/[tried_fresh] booleans. Exposed for testing. *)
