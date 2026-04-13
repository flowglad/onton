open Types

(** Startup reconciliation: discover existing GitHub PRs, recover worktrees, and
    identify stale busy agents at launch.

    When onton starts fresh (no persisted state), each patch agent begins with
    [has_pr = false]. If PRs already exist from a previous run, the startup
    reconciler queries GitHub to discover them and returns updates to apply
    before the main loop begins. This avoids re-spawning Claude for patches that
    already have open or merged PRs.

    When resuming from a persisted snapshot, the reconciler also:
    - Discovers existing git worktrees and matches them to patches by branch
    - Identifies agents that were persisted with [busy=true] (crashed sessions)
      so the caller can reset them *)

type pr_discovery = {
  patch_id : Patch_id.t;
  pr_number : Pr_number.t;
  base_branch : Branch.t;
  merged : bool;
}
[@@deriving show, eq]
(** A discovered PR for a patch, including the PR's base ref. *)

type worktree_recovery = {
  worktree_patch_id : Patch_id.t;
  worktree_path : string;
}
[@@deriving show, eq]
(** A recovered worktree matched to a patch by branch name. The path is logged
    for diagnostics; worktrees are not tracked in orchestrator state since they
    are always derived from [Worktree.worktree_dir] at runtime. *)

type t = {
  discovered : pr_discovery list;
  recovered_worktrees : worktree_recovery list;
  reset_pending : Patch_id.t list;
  errors : (Patch_id.t * string) list;
  worktree_errors : string list;
}
[@@deriving show, eq]
(** Result of startup reconciliation. [recovered_worktrees] contains worktrees
    found on disk that match patch branches. [reset_pending] lists patch IDs
    whose agents had [busy=true] at persist time (stale sessions). [errors]
    contains per-patch PR discovery failures. [worktree_errors] contains global
    worktree listing failures (not per-patch). *)

val discover_pr :
  net:_ Eio.Net.t ->
  github:Github.t ->
  branch:Branch.t ->
  ((Pr_number.t * Branch.t * bool) option, string) Result.t
(** Query the GitHub REST API for a branch, returning the first non-CLOSED PR as
    [(pr_number, base_branch, merged)] or [None]. *)

val recover_worktrees :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  patches:Patch.t list ->
  worktree_recovery list * string option
(** Discover existing worktrees and match them to patches by branch name.
    Returns matched worktrees and an optional error string if listing failed.
    Can be called before [reconcile] to capture worktree state early. *)

val reconcile :
  net:_ Eio.Net.t ->
  github:Github.t ->
  patches:Patch.t list ->
  ?repo_root:string ->
  ?process_mgr:_ Eio.Process.mgr ->
  ?agents:Patch_agent.t list ->
  ?pre_recovered_worktrees:worktree_recovery list ->
  unit ->
  t
(** [reconcile ~net ~github ~patches ?repo_root ?process_mgr ?agents
     ?pre_recovered_worktrees ()] queries GitHub REST API for each patch's
    branch to find existing PRs and identifies stale busy agents from [agents]
    (default [[]]). When [pre_recovered_worktrees] is provided, uses it directly
    instead of scanning the filesystem (avoids racing with concurrent worktree
    creation). Otherwise discovers worktrees under [repo_root] (default ["."])
    using [process_mgr]. *)
