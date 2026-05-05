(** Pure plans for sequences of worktree operations.

    Every plan is a list of {!op}. The fundamental safety invariant — enforced
    by {!ensures_worktree_before_fs} and verified by property tests in
    [test_worktree_plan.ml] — is that any filesystem operation that runs inside
    a worktree directory must be preceded by [Ensure_worktree] in the same plan.
    This is what was violated by the bug in patch 48: a [Fetch_origin] ran in a
    directory that had never been created.

    The runner that executes plans lives in the binary (it needs Eio
    capabilities), but the data is here so it can be inspected and
    property-tested without I/O. *)

type op =
  | Ensure_worktree
      (** Materialize the worktree on disk if missing. Must precede any
          subsequent op in the plan. *)
  | Fetch_origin  (** [git fetch origin] inside the worktree. *)
  | Rebase_onto of Types.Branch.t
      (** [git rebase --onto <branch>] inside the worktree. The branch is
          typically [origin/<base>]. *)
[@@deriving show, eq, sexp_of, compare]

type t = op list [@@deriving show, eq, sexp_of, compare]

val for_rebase : new_base:Types.Branch.t -> t
(** Plan for [Orchestrator.Rebase]: ensure the worktree, fetch fresh refs, then
    rebase onto [origin/<new_base>]. *)

val for_merge_conflict : base:Types.Branch.t -> t
(** Plan for the merge-conflict resolution path (post rebase-in-progress check):
    ensure the worktree, fetch, then rebase onto [origin/<base>]. *)

val ensures_worktree_before_fs : t -> bool
(** Returns [true] iff every [Fetch_origin] / [Rebase_onto] is preceded by an
    [Ensure_worktree] in the plan. The safety invariant. *)
