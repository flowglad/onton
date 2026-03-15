(** JSON state persistence for runtime snapshots.

    Saves and loads [Runtime.snapshot] to/from a JSON file on disk. Used for
    crash recovery — the system can resume from the last persisted state rather
    than starting from scratch. *)

val save : path:string -> Runtime.snapshot -> (unit, string) result
(** Write the snapshot to [path] as JSON. Returns [Error msg] on I/O failure. *)

val load :
  path:string -> gameplan:Types.Gameplan.t -> (Runtime.snapshot, string) result
(** Read a snapshot from [path]. The [gameplan] is needed to reconstruct the
    dependency graph (which is derived, not stored). Returns [Error msg] if the
    file is missing, malformed, or incompatible. *)

val snapshot_to_yojson : Runtime.snapshot -> Yojson.Safe.t
(** Convert a snapshot to its JSON representation. *)

val snapshot_of_yojson :
  gameplan:Types.Gameplan.t ->
  Yojson.Safe.t ->
  (Runtime.snapshot, string) result
(** Parse a snapshot from JSON. *)
