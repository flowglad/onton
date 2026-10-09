(* @archlint.module interface
   @archlint.domain prune-runner *)

val run_prune :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  process_mgr:_ Eio.Process.mgr ->
  github_token:string ->
  refresh:bool ->
  unit ->
  int
(** Remove stored projects whose gameplan patches are all terminal (merged or
    closed) and have no unfinished reconciliation owner. Before removing data,
    reclaim only that project's recovery namespace using captured ref tips and
    reachability evidence from locked surviving projects. Keep required refs,
    projects sharing an in-use repository, and managed clones still referenced
    by another project. Probe, snapshot and inventory errors retain the data.
    Registration and lifetime leases also exclude startup and workers that
    bypass the exclusive supervisor lock. Eligible directories are retired by
    atomic rename; interrupted journal cleanup cannot traverse a recreated
    project.

    When [refresh] is [true], every project with at least one non-terminal patch
    (i.e. an agent with [merged = false] and a recorded PR number) is reconciled
    with the forge first: per-patch [Forge.pr_state] queries update the
    in-memory agents map before the all-merged classification runs. This catches
    projects where merges happened out-of-band (e.g. via the GitHub UI) after
    the supervisor stopped polling. Refresh is conservative — any forge error
    leaves the stored [merged] flag untouched so the project is kept, not
    accidentally deleted.

    [github_token] is the legacy name of the explicit forge-token CLI value.
    When it is empty, prune falls back to [GITHUB_TOKEN] / [gh auth token] or
    [SRHT_TOKEN], according to each stored project's forge.

    Returns [0] on success and [1] when one or more projects report prune errors
    (for example lock, snapshot-load, or prune-time filesystem errors).
    Forge-refresh failures alone do not set the exit code; they are surfaced as
    informational notes on the kept project. *)
