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
}
[@@deriving show, eq]
(** Result of startup reconciliation. [recovered_worktrees] contains worktrees
    found on disk that match patch branches. [reset_pending] lists patch IDs
    whose agents had [busy=true] at persist time (stale sessions). [errors]
    contains per-patch PR discovery failures with diagnostic messages. *)

val reconcile :
  process_mgr:_ Eio.Process.mgr ->
  token:string ->
  owner:string ->
  repo:string ->
  patches:Patch.t list ->
  ?repo_root:string ->
  ?agents:Patch_agent.t list ->
  unit ->
  t
(** [reconcile ~process_mgr ~token ~owner ~repo ~patches ?repo_root ?agents ()]
    queries GitHub for each patch's branch to find existing PRs, discovers
    existing worktrees under [repo_root] (default ["."]), and identifies stale
    busy agents from [agents] (default [[]]). *)
