open Base

(** Per-patch agent state machine.

    Encodes the spec fragment for actions: Start, Respond, Complete. The type
    [t] is private — external code can inspect fields but must use smart
    constructors that enforce spec preconditions. *)

type session_fallback = Fresh_available | Tried_fresh | Given_up
[@@deriving show, eq, sexp_of, compare, yojson]

type t = private {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  pr_number : Types.Pr_number.t option;
  has_session : bool;
  busy : bool;
  merged : bool;
  queue : Types.Operation_kind.t list;
  satisfies : bool;
  changed : bool;
  has_conflict : bool;
  base_branch : Types.Branch.t option;
  notified_base_branch : Types.Branch.t option;
  ci_failure_count : int;
  session_fallback : session_fallback;
  human_messages : string list;
  ci_checks : Types.Ci_check.t list;
  merge_ready : bool;
  is_draft : bool;
  pr_description_applied : bool;
  implementation_notes_delivered : bool;
  start_attempts_without_pr : int;
  conflict_noop_count : int;
  checks_passing : bool;
  current_op : Types.Operation_kind.t option;
  current_message_id : Types.Message_id.t option;
  generation : int;
  worktree_path : string option;
  branch_blocked : bool;
  llm_session_id : string option;
}
[@@deriving show, eq, sexp_of, compare]

val create : branch:Types.Branch.t -> Types.Patch_id.t -> t
(** Initial state for a patch: no PR, not busy, empty queue. *)

val create_adhoc :
  patch_id:Types.Patch_id.t ->
  branch:Types.Branch.t ->
  pr_number:Types.Pr_number.t ->
  t
(** Initial state for an ad-hoc patch: has PR, not busy, empty queue.
    Corresponds to the spec's Add action:
    {v PatchCtx ~> Add | p: Patch. --- has-pr' p. ~busy' p. ~in-gameplan' p. v}
*)

(** {2 Derived predicates} *)

val has_pr : t -> bool
(** [true] when [pr_number] is [Some _]. *)

val needs_intervention : t -> bool
(** Derived predicate: true when [Human] is not in [queue] and any of:
    [ci_failure_count >= 3], [session_fallback = Given_up],
    [(not has_pr) && start_attempts_without_pr >= 2], or
    [conflict_noop_count >= 2]. *)

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
    Postconditions: [~busy]. [needs_intervention] is derived automatically from
    [ci_failure_count], [session_fallback], [start_attempts_without_pr], and
    [Human] in queue. *)

(** {2 State mutation helpers} *)

val enqueue : t -> Types.Operation_kind.t -> t
(** Add an operation to the queue (idempotent). *)

val mark_merged : t -> t
(** Mark the patch as merged. *)

val add_human_message : t -> string -> t
(** Add a human message to the pending list. *)

val restore_human_messages : t -> string list -> t
(** Replace [human_messages] wholesale. Used to preserve messages across a
    failed Human session — [complete] clears them assuming delivery succeeded,
    but on failure they must be restored for the retry. *)

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

val on_pre_session_failure : t -> t
(** Handle a failure that occurs before a Claude session starts (worktree
    creation, process spawn error). Increments [start_attempts_without_pr] for
    no-PR agents so they hit [needs_intervention] after 2 failures instead of
    retrying indefinitely. No-op for agents that already have a PR. *)

val set_has_conflict : t -> t
(** Mark the patch as having a merge conflict. *)

val clear_has_conflict : t -> t
(** Clear the merge conflict flag. Does NOT reset [conflict_noop_count]; call
    [reset_conflict_noop_count] explicitly when a conflict is truly resolved
    (not just a noop). *)

val reset_conflict_noop_count : t -> t
(** Reset [conflict_noop_count] to 0. Call when a conflict is genuinely resolved
    (successful rebase, agent resolution, or poll no longer reports conflict).
*)

val set_base_branch : t -> Types.Branch.t -> t
(** Update the base branch. *)

val set_notified_base_branch : t -> Types.Branch.t -> t
(** Record that the agent session has been informed of this base branch. *)

val base_branch_changed : t -> bool
(** [true] when [base_branch] differs from [notified_base_branch], meaning the
    agent session has not yet been told about the current base branch. *)

val set_merge_ready : t -> bool -> t
(** Set the merge_ready flag from GitHub mergeStateStatus. *)

val set_is_draft : t -> bool -> t
(** Set the draft flag from GitHub PR state. *)

val set_pr_description_applied : t -> bool -> t
(** Record whether the orchestrator has successfully applied the PR body. *)

val set_implementation_notes_delivered : t -> bool -> t
(** Record whether implementation notes were successfully delivered. *)

val increment_start_attempts_without_pr : t -> t
(** Record a successful Start run that still failed to discover a PR. *)

val increment_conflict_noop_count : t -> t
(** Record a conflict resolution attempt where rebase returned Noop (stale refs
    or no real diff). After 2 noop attempts, [needs_intervention] triggers. *)

val set_checks_passing : t -> bool -> t
(** Set the checks_passing flag from GitHub CI status. *)

val set_worktree_path : t -> string -> t
(** Store the resolved worktree path for this patch. *)

val is_approved : t -> main_branch:Types.Branch.t -> bool
(** Derived predicate:
    [has_pr && merge_ready && not is_draft && not busy && not needs_intervention
     && base_branch = main_branch]. A patch is only approved when its PR targets
    [main_branch] directly and is no longer a draft. [merge_ready] reflects
    GitHub's [mergeStateStatus = CLEAN], which encapsulates required reviews,
    passing checks, and branch protection. *)

val increment_ci_failure_count : t -> t
(** Increment the CI failure counter. *)

val reset_ci_failure_count : t -> t
(** Reset [ci_failure_count] to 0. Called by the poller when CI checks pass
    after failures. [needs_intervention] is re-derived automatically. *)

val reset_intervention_state : t -> t
(** Reset [session_fallback] to [Fresh_available], [ci_failure_count] to 0, and
    [start_attempts_without_pr] to 0. Used after manual resolution (e.g.,
    sending a human message) to give the patch a fresh start. *)

val set_branch_blocked : t -> t
(** Set the branch-blocked flag (branch is checked out in repo root). *)

val clear_branch_blocked : t -> t
(** Clear the branch-blocked flag (branch is no longer in repo root). *)

val set_ci_checks : t -> Types.Ci_check.t list -> t
(** Replace the stored CI check details. *)

val reset_busy : t -> t
(** Reset a stale [busy] flag from a crashed session. If [busy], clears it.
    [needs_intervention] is derived automatically. No-op if not busy. *)

val set_current_message_id : t -> Types.Message_id.t option -> t
(** Track the currently accepted delivery message for this patch. *)

val bump_generation : t -> t
(** Advance the patch generation used for deterministic message IDs. *)

val set_llm_session_id : t -> string option -> t
(** Store the LLM backend's session ID for explicit session resumption. Cleared
    automatically when [session_fallback] escalates (old session is stale). *)

val resume_current_message : t -> op:Types.Operation_kind.t option -> t
(** Resume execution of an already accepted message without reapplying its
    queue-consuming state transition. [~op] restores [current_op] from the
    outbox so that [complete] can clear [human_messages] correctly. *)

(** {2 Queries} *)

val highest_priority : t -> Types.Operation_kind.t option
(** The highest-priority operation in the queue, or [None] if empty. *)

(** {2 Persistence support} *)

val set_pr_number : t -> Types.Pr_number.t -> t
(** Store [pr_number] (making [has_pr] true). Not a plain field setter —
    establishes the PR-present state and resets PR-bootstrap lifecycle facts. *)

val clear_pr : t -> t
(** Remove the PR number and reset all PR-related state, returning the agent to
    the no-PR bootstrap path. *)

val restore :
  patch_id:Types.Patch_id.t ->
  branch:Types.Branch.t ->
  pr_number:Types.Pr_number.t option ->
  has_session:bool ->
  busy:bool ->
  merged:bool ->
  queue:Types.Operation_kind.t list ->
  satisfies:bool ->
  changed:bool ->
  has_conflict:bool ->
  base_branch:Types.Branch.t option ->
  notified_base_branch:Types.Branch.t option ->
  ci_failure_count:int ->
  session_fallback:session_fallback ->
  human_messages:string list ->
  ci_checks:Types.Ci_check.t list ->
  merge_ready:bool ->
  is_draft:bool ->
  pr_description_applied:bool ->
  implementation_notes_delivered:bool ->
  start_attempts_without_pr:int ->
  conflict_noop_count:int ->
  checks_passing:bool ->
  current_op:Types.Operation_kind.t option ->
  current_message_id:Types.Message_id.t option ->
  generation:int ->
  worktree_path:string option ->
  branch_blocked:bool ->
  llm_session_id:string option ->
  t
(** Reconstruct agent state from persisted field values. Bypasses precondition
    checks — use only for deserialization. *)
