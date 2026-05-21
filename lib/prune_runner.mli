val run_prune :
  net:_ Eio.Net.t -> clock:_ Eio.Time.clock -> refresh:bool -> unit -> int
(** Remove stored projects whose gameplan patches are all merged.

    When [refresh] is [true], every project with at least one non-terminal patch
    (i.e. an agent with [merged = false] and a recorded PR number) is reconciled
    with the forge first: per-patch [Forge.pr_state] queries update the
    in-memory agents map before the all-merged classification runs. This catches
    projects where merges happened out-of-band (e.g. via the GitHub UI) after
    the supervisor stopped polling. Refresh is conservative — any forge error
    leaves the stored [merged] flag untouched so the project is kept, not
    accidentally deleted.

    Returns [0] on success and [1] when one or more projects report prune errors
    (for example lock, snapshot-load, or prune-time filesystem errors).
    Forge-refresh failures alone do not set the exit code; they are surfaced as
    informational notes on the kept project. *)
