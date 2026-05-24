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
  | Capture_anchor of { ref_name : string; slot : int }
      (** Resolve [ref_name] (e.g. ["origin/main"]) to a SHA via
          [Worktree.read_branch_sha] and store it in [slot]. The slot is
          consumed later by a {!Record_anchor_on_success} op. A failed
          resolution stores [None] in the slot and causes a subsequent
          [Record_anchor_on_success] to emit [Anchor_capture_failed]. *)
  | Rebase_onto of Types.Branch.t
      (** [git rebase --onto <branch>] inside the worktree. The branch is
          typically [origin/<base>]. *)
  | Record_anchor_on_success of { slot : int; base : Types.Branch.t }
      (** Emit an {!anchor_event} from [slot]'s captured SHA iff the most recent
          {!Rebase_onto} returned [Ok] or [Noop] (or no rebase ran). A populated
          slot yields [Anchor_recorded { base; sha; ... }]; an empty slot or a
          SHA that fails {!Anchor.make}'s validation yields
          [Anchor_capture_failed]. *)
[@@deriving show, eq, sexp_of, compare]

type t = op list [@@deriving show, eq, sexp_of, compare]

(** Side-channel observations the executor emits as it runs a plan, folded into
    agent state by the runner. *)
type anchor_event = Anchor_recorded of Anchor.t | Anchor_capture_failed
[@@deriving show, eq, sexp_of, compare]

val origin_of : Types.Branch.t -> Types.Branch.t
(** Prepends ["origin/"] to a branch name. *)

val for_rebase : new_base:Types.Branch.t -> t
(** Plan for [Orchestrator.Rebase]: ensure the worktree, fetch fresh refs, then
    rebase onto [origin/<new_base>]. *)

val for_merge_conflict : base:Types.Branch.t -> t
(** Plan for the merge-conflict resolution path (post rebase-in-progress check):
    ensure the worktree, fetch, then rebase onto [origin/<base>]. *)

val for_start : base:Types.Branch.t -> t
(** Plan executed by the runner Start path BEFORE the LLM session begins: ensure
    worktree, fetch, capture [origin/<base>]'s tip into slot 0, then record an
    anchor referencing that SHA. Records the initial anchor for a
    freshly-branched-off-dep patch — closing the production bug blind spot where
    Start never wrote an anchor and the first rebase (post-squash) had nothing
    safe to fall back to. *)

val ensures_worktree_before_fs : t -> bool
(** Returns [true] iff every non-{!Ensure_worktree} op is preceded by an
    [Ensure_worktree] in the plan. The safety invariant. *)
