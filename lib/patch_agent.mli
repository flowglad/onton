open Base

(** Per-patch agent state machine.

    Encodes the spec fragment for actions: Start, Respond, Complete. The type
    [t] is private — external code can inspect fields but must use smart
    constructors that enforce spec preconditions. *)

type session_fallback = Fresh_available | Tried_fresh | Given_up
[@@deriving show, eq, sexp_of, compare, yojson]

type t = private {
  patch_id : Types.Patch_id.t;
  has_pr : bool;
  pr_number : Types.Pr_number.t option;
  has_session : bool;
  busy : bool;
  merged : bool;
  needs_intervention : bool;
  queue : Types.Operation_kind.t list;
  satisfies : bool;
  changed : bool;
  has_conflict : bool;
  base_branch : Types.Branch.t option;
  ci_failure_count : int;
  ci_fix_running : bool;
  session_fallback : session_fallback;
  human_messages : string list;
  ci_checks : Types.Ci_check.t list;
  mergeable : bool;
  merge_ready : bool;
  checks_passing : bool;
  no_unresolved_comments : bool;
  current_op : Types.Operation_kind.t option;
  worktree_path : string option;
  head_branch : Types.Branch.t option;
  branch_blocked : bool;
}
[@@deriving show, eq, sexp_of, compare]

val create : Types.Patch_id.t -> t
(** Initial state for a patch: no PR, not busy, empty queue. *)

val create_adhoc : patch_id:Types.Patch_id.t -> pr_number:Types.Pr_number.t -> t
(** Initial state for an ad-hoc patch: has PR, not busy, empty queue.
    Corresponds to the spec's Add action:
    {v PatchCtx ~> Add | p: Patch. --- has-pr' p. ~busy' p. ~in-gameplan' p. v}
*)

(** {2 Spec actions} *)

val start : t -> base_branch:Types.Branch.t -> t
(** [PatchCtx ~> Start] — begin work on a patch. Preconditions (checked):
    [~has_pr], [~busy]. Caller must verify [in_gameplan] and [deps_satisfied]
    externally. Postconditions: [has_session], [busy], [satisfies],
    [base_branch = Some base_branch]. *)

val rebase : t -> base_branch:Types.Branch.t -> t
(** [PatchCtx ~> Rebase] — orchestrator-executed rebase. Preconditions:
    [has_pr], [~merged], [~busy], [Rebase] in [queue], [Rebase] is
    [highest_priority]. Postconditions: [busy]; dequeues [Rebase]; preserves
    [has_session] (does not force true); updates [base_branch]. *)

val respond : t -> Types.Operation_kind.t -> t
(** [PatchCtx, Comments ~> Respond] — respond to queued feedback. Preconditions
    (checked): [has_pr], [~merged], [~busy], [~needs_intervention], [k] in
    [queue], [k] is [highest_priority]. Postconditions per spec: sets
    [has_session], [busy]; dequeues [k]; conditionally updates [satisfies],
    [changed], [has_conflict], and resolves [pending_comments]. *)

val complete : t -> t
(** [PatchCtx ~> Complete] — session finished. Preconditions (checked): [busy].
    Postconditions: [~busy]; recalculates [needs_intervention] from
    [ci_failure_count], [session_fallback], and [Human] in queue. *)

(** {2 State mutation helpers} *)

val enqueue : t -> Types.Operation_kind.t -> t
(** Add an operation to the queue (idempotent). *)

val mark_merged : t -> t
(** Mark the patch as merged. *)

val add_human_message : t -> string -> t
(** Add a human message to the pending list. *)

val set_session_failed : t -> t
(** Mark session fallback as [Given_up]. *)

val set_tried_fresh : t -> t
(** Advance session fallback to [Tried_fresh]. No-op if already [Tried_fresh] or
    [Given_up] — the fallback state only moves forward. *)

val clear_session_fallback : t -> t
(** Reset session fallback to [Fresh_available]. *)

val on_session_failure : t -> is_fresh:bool -> t
(** Handle a Claude session failure. Pure decision:
    - Start path (no PR) + fresh failure: reset to [Fresh_available] for retry
    - Resume failure: escalate to [Tried_fresh] (will try fresh next)
    - Respond path fresh failure: escalate to [Given_up] → needs_intervention *)

val on_pr_discovery_failure : t -> t
(** Handle a successful Claude run where PR discovery failed. Resets fallback so
    the patch retries from scratch. *)

val set_has_conflict : t -> t
(** Mark the patch as having a merge conflict. *)

val clear_has_conflict : t -> t
(** Clear the merge conflict flag. Spec: [mergeable' p = world-mergeable p]. *)

val set_base_branch : t -> Types.Branch.t -> t
(** Update the base branch. *)

val set_mergeable : t -> bool -> t
(** Set the mergeable flag from GitHub merge state. *)

val set_merge_ready : t -> bool -> t
(** Set the merge_ready flag from GitHub mergeStateStatus. *)

val set_checks_passing : t -> bool -> t
(** Set the checks_passing flag from GitHub CI status. *)

val set_no_unresolved_comments : t -> bool -> t
(** Set the no_unresolved_comments flag from GitHub review state. *)

val set_worktree_path : t -> string -> t
(** Store the resolved worktree path for this patch. *)

val set_head_branch : t -> Types.Branch.t -> t
(** Store the PR's head branch (fetched from GitHub). *)

val is_approved : t -> main_branch:Types.Branch.t -> bool
(** Derived predicate:
    [has_pr && merge_ready && not busy && not needs_intervention && base_branch
     = main_branch]. A patch is only approved when its PR targets [main_branch]
    directly. [merge_ready] reflects GitHub's [mergeStateStatus = CLEAN], which
    encapsulates required reviews, passing checks, and branch protection. *)

val increment_ci_failure_count : t -> t
(** Increment the CI failure counter. *)

val set_ci_fix_running : t -> t
(** Mark CI fix as in progress. Suppresses CI re-enqueue until cleared. *)

val clear_ci_fix_running : t -> t
(** Clear the CI fix-running flag and reset [ci_failure_count] to 0. Called by
    the poller when CI checks pass after a fix. *)

val set_needs_intervention : t -> t
(** Set the needs-intervention flag (e.g., session failure escalation). *)

val clear_needs_intervention : t -> t
(** Clear the needs-intervention flag (e.g., after manual resolution). Also
    resets [session_fallback] to [Fresh_available]. *)

val set_branch_blocked : t -> t
(** Set the branch-blocked flag (branch is checked out in repo root). *)

val clear_branch_blocked : t -> t
(** Clear the branch-blocked flag (branch is no longer in repo root). *)

val set_ci_checks : t -> Types.Ci_check.t list -> t
(** Replace the stored CI check details. *)

val reset_busy : t -> t
(** Reset a stale [busy] flag from a crashed session. If [busy], clears it and
    re-evaluates [needs_intervention] using the same logic as [complete]
    ([ci_failure_count >= 3 || session_failed], unless [Human] is queued). No-op
    if not busy. *)

(** {2 Queries} *)

val highest_priority : t -> Types.Operation_kind.t option
(** The highest-priority operation in the queue, or [None] if empty. *)

(** {2 Persistence support} *)

val set_pr_number : t -> Types.Pr_number.t -> t
(** Store [pr_number] and set [has_pr = true]. Not a plain field setter —
    establishes the PR-present state. *)

val restore :
  patch_id:Types.Patch_id.t ->
  has_pr:bool ->
  pr_number:Types.Pr_number.t option ->
  has_session:bool ->
  busy:bool ->
  merged:bool ->
  needs_intervention:bool ->
  queue:Types.Operation_kind.t list ->
  satisfies:bool ->
  changed:bool ->
  has_conflict:bool ->
  base_branch:Types.Branch.t option ->
  ci_failure_count:int ->
  ci_fix_running:bool ->
  session_fallback:session_fallback ->
  human_messages:string list ->
  ci_checks:Types.Ci_check.t list ->
  mergeable:bool ->
  merge_ready:bool ->
  checks_passing:bool ->
  no_unresolved_comments:bool ->
  worktree_path:string option ->
  head_branch:Types.Branch.t option ->
  branch_blocked:bool ->
  t
(** Reconstruct agent state from persisted field values. Bypasses precondition
    checks — use only for deserialization. *)
