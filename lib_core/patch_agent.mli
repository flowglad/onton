open Base

(** Per-patch agent state machine.

    Encodes the spec fragment for actions: Start, Respond, Complete. The type
    [t] is private — external code can inspect fields but must use smart
    constructors that enforce spec preconditions. *)

type session_fallback = Fresh_available | Tried_fresh | Given_up
[@@deriving show, eq, sexp_of, compare, yojson]

type op_state = Queued | Running
[@@deriving show, eq, sexp_of, compare, yojson]

type t = private {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  pr_status : Patch_pr_status.t;
      (** Lifecycle status of the patch's PR. Use the accessor functions
          ([has_pr], [is_pr_present], [is_pr_missing], [pr_number]) rather than
          pattern-matching the field — see {!Patch_pr_status}. *)
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
  inflight_human_messages : string list;
  ci_checks : Types.Ci_check.t list;
  merge_ready : bool;
  is_draft : bool;
  pr_body_delivered : bool;
  pr_body_artifact_miss_count : int;
      (** Consecutive Pr_body sessions that produced [Respond_pr_body_miss]:
          either the artifact was missing/empty AND a Write tool call did not
          complete (agent blocked mid-call), or the GitHub [update_pr_body]
          PATCH call failed ([`Patch_failed]). Contributes to
          [needs_intervention] at [>= 2]. Zero in fresh snapshots and older
          snapshots that didn't persist the field. Reset by
          [reset_intervention_state] and by [Respond_ok] for [Pr_body]. *)
  start_attempts_without_pr : int;
  conflict_noop_count : int;
  no_commits_push_count : int;
  push_failure_count : int;
      (** Consecutive [Session_push_failed] outcomes (Session_ok or session
          retry with [Push_rejected]/[Push_error]) since the last successful
          push. At [>= 3] contributes to [needs_intervention]. Reset on
          successful push ([Push_ok] / [Push_up_to_date]) and by
          [reset_intervention_state]. A {e permanent} rejection
          ([Push_reject_classify.is_permanent]) short-circuits this counter by
          setting [session_fallback = Given_up] directly — see
          {!Orchestrator.apply_session_result}. *)
  branch_rebased_onto : Types.Branch.t option;
  branch_rebased_onto_sha : string option;
      (** SHA the base ref resolved to at the time of the most recent successful
          rebase / start. Used by [Worktree.rebase_onto] as the
          [--onto NEW_BASE OLD_SHA] anchor when the base transitions from one
          dep's branch to another (or to [main]): without it, the rebase falls
          back to plain [git rebase NEW_BASE] which replays {e all} commits
          between the local main and HEAD, leaving the old dep's commits on the
          branch even after the dep's squash-merge appears on origin/main.
          [None] when no rebase has succeeded yet, or when the orchestrator was
          restarted before this field was added (it is back-filled to [None] via
          [Patch_agent.restore], and old snapshots persist with [null]). *)
  anchor_history : Anchor_history.t;
      (** Newest-first log of {!Anchor.t} values recorded over this agent's
          lifetime. Updated via {!record_anchor}; mirrored into
          [branch_rebased_onto] and [branch_rebased_onto_sha] as a derived view.
          {!Rebase_decision.plan} consults the history when the newest anchor is
          unreachable from HEAD and walks back to the oldest still- reachable
          entry. Capped at {!Anchor_history.cap}. *)
  checks_passing : bool;
  current_op : Types.Operation_kind.t option;
  current_op_state : op_state;
      (** Sub-state of [current_op]. [Queued] when the action fiber is alive but
          its work has not begun (typically waiting on the Claude semaphore).
          [Running] once the work has actually started. Meaningful only when
          [busy] is true; reset to [Queued] on [complete]. Drives the TUI
          "(queued)" suffix so a saturated semaphore can be told apart from an
          actively-running session. *)
  current_message_id : Types.Message_id.t option;
  generation : int;
  worktree_path : string option;
  branch_blocked : bool;
  llm_session_id : string option;
  automerge_enabled : bool;
  automerge_deadline : float option;
  automerge_inflight : bool;
  automerge_failure_count : int;
  delivered_ci_run_ids : int list;
      (** CheckRun [databaseId]s already delivered as CI feedback. Sorted and
          deduplicated. Drives per-run deduplication in the CI delivery path so
          a single failing run is never delivered twice. Cleared on [clear_pr].
      *)
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
(** [true] when this agent has a recorded PR identity (either [Present] or
    [Missing] in [pr_status]). Used as the orchestrator-invariant predicate:
    every graph node must be in the gameplan or have a PR identity. *)

val is_pr_present : t -> bool
(** [true] only when [pr_status = Present _]. Use this for any callsite that is
    about to act on the PR (rebase, respond, merge, set draft) — a [Missing] PR
    cannot be acted on. *)

val is_pr_missing : t -> bool
(** [true] only when [pr_status = Missing _]. Contributes to
    {!needs_intervention}. *)

val pr_number : t -> Types.Pr_number.t option
(** The recorded PR number, if any. [Some] for both [Present] and [Missing];
    [None] for [Absent]. *)

val needs_intervention : t -> bool
(** Derived predicate. True iff the agent is not [merged] AND any of:
    - [session_fallback = Given_up] (bypasses the Human exemption)
    - [is_pr_missing t] (PR vanished from the remote — bypasses the Human
      exemption; queued Human entries are deferred until [Missing → Present]
      recovery rather than dispatched while [Missing])
    - [Human] not in queue AND any of: [ci_failure_count >= 3],
      [(not has_pr) && start_attempts_without_pr >= 2],
      [conflict_noop_count >= 2], [no_commits_push_count >= 2],
      [push_failure_count >= 3], [pr_body_artifact_miss_count >= 2]. *)

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

val add_human_messages : t -> string list -> t
(** Prepend multiple messages to the pending list, preserving their order. *)

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
(** Handle a successful Claude run where PR discovery failed. Increments the
    durable attempt counter so [needs_intervention] fires after repeated
    failures. No-op when the agent already has a PR. *)

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

val set_branch_rebased_onto : t -> Types.Branch.t -> t
(** Record that the local branch has been rebased onto this base (either by an
    explicit successful [Rebase] action, or by the initial [Start] which plants
    the branch on its base). Drives [detect_notified_base_drift]. *)

val set_branch_rebased_onto_sha : t -> string option -> t
(** Record the SHA the base ref resolved to at the moment of a successful rebase
    / start. [None] (or a whitespace-only string) clears the field — used when
    the SHA could not be read; the next rebase will then fall back to the legacy
    plain [git rebase target] semantics. *)

val record_anchor : t -> Anchor.t -> t
(** Push [anchor] onto {!field-anchor_history} (newest-first, deduped by
    [(base, sha)]) and update the legacy view fields [branch_rebased_onto] and
    [branch_rebased_onto_sha] to mirror the new anchor. Pure and total. *)

val anchor_history : t -> Anchor_history.t

val base_branch_changed : t -> bool
(** [true] when [base_branch] differs from [notified_base_branch], meaning the
    agent session has not yet been told about the current base branch. *)

val set_merge_ready : t -> bool -> t
(** Set the merge_ready flag from GitHub mergeStateStatus. *)

val set_is_draft : t -> bool -> t
(** Set the draft flag from GitHub PR state. *)

val set_pr_body_delivered : t -> bool -> t
(** Record whether the LLM-authored PR body has been written to the artifact and
    PATCHed onto the PR. Set to [true] on Pr_body Respond_ok regardless of
    whether the artifact existed (so we don't loop on missing artifacts —
    documented fallback is to keep the gameplan-derived body). *)

val increment_start_attempts_without_pr : t -> t
(** Record a successful Start run that still failed to discover a PR. *)

val increment_conflict_noop_count : t -> t
(** Record a conflict resolution attempt where rebase returned Noop (stale refs
    or no real diff). After 2 noop attempts, [needs_intervention] triggers. *)

val increment_no_commits_push_count : t -> t
(** Record a session that ended with no commits on the branch (HEAD == base).
    After 2 such sessions, [needs_intervention] triggers — the agent is not
    committing its work and further retries are wasted. *)

val reset_no_commits_push_count : t -> t
(** Reset [no_commits_push_count] to 0. Called on [Session_ok] with a successful
    push, because the agent has demonstrated it can commit. *)

val increment_push_failure_count : t -> t
(** Record a [Session_push_failed] outcome. At [>= 3], [needs_intervention]
    triggers — the push has been refused by the remote three sessions in a row
    (e.g. lease races that don't resolve, or any transient server-side block). A
    {e permanent} rejection (workflow scope, branch protection, push rule)
    bypasses this counter entirely; see {!Orchestrator.apply_session_result}. *)

val reset_push_failure_count : t -> t
(** Reset [push_failure_count] to 0. Called on a successful push ([Push_ok] /
    [Push_up_to_date]) in [Orchestrator.apply_session_result]. *)

val increment_pr_body_artifact_miss_count : t -> t
(** Record a Pr_body session that ended without durable PR body delivery. Called
    from [Orchestrator.apply_respond_outcome] on [Respond_pr_body_miss], which
    covers two cases: (a) missing/empty artifact AND an observed non-completed
    Write tool call (agent blocked mid-call); (b)
    [artifact_outcome = `Patch_failed] (notes written but the GitHub
    [update_pr_body] call failed). After 2 such sessions, [needs_intervention]
    triggers. *)

val reset_pr_body_artifact_miss_count : t -> t
(** Reset [pr_body_artifact_miss_count] to 0. *)

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
(** Increment the CI failure counter. Called from
    [Orchestrator.apply_respond_outcome] on [Respond_ok] for a Ci delivery, so
    the counter only reflects CI fix attempts that actually delivered a payload
    with failure conclusions. *)

val reset_ci_failure_count : t -> t
(** Reset [ci_failure_count] to 0. Called by the poller when CI checks pass
    after failures. [needs_intervention] is re-derived automatically. *)

val reset_intervention_state : t -> t
(** Reset [session_fallback] to [Fresh_available], [ci_failure_count] to 0,
    [start_attempts_without_pr] to 0, [conflict_noop_count] to 0,
    [no_commits_push_count] to 0, [push_failure_count] to 0, and
    [pr_body_artifact_miss_count] to 0. Used after manual resolution (e.g.,
    sending a human message) to give the patch a fresh start. *)

val set_branch_blocked : t -> t
(** Set the branch-blocked flag (branch is checked out in repo root). *)

val clear_branch_blocked : t -> t
(** Clear the branch-blocked flag (branch is no longer in repo root). *)

val set_ci_checks : t -> Types.Ci_check.t list -> t
(** Replace the stored CI check details. *)

val record_delivered_ci_run_ids : t -> int list -> t
(** Mark the given CheckRun [databaseId]s as delivered so the CI feedback path
    will not re-deliver them. Merges with the existing set, sorts, and dedups.
    [ids] should contain only CheckRuns that carried an id (StatusContext
    entries without stable numeric ids cannot be deduped). *)

val reset_busy : t -> t
(** Reset a stale [busy] flag from a crashed session. If [busy], clears it.
    [needs_intervention] is derived automatically. No-op if not busy. *)

val set_current_message_id : t -> Types.Message_id.t option -> t
(** Track the currently accepted delivery message for this patch. *)

val bump_generation : t -> t
(** Advance the patch generation used for deterministic message IDs. *)

val set_llm_session_id : t -> string option -> t
(** Store the LLM backend's session ID for explicit session resumption.
    Preserved across fallback escalation so the operator can resume the session
    after intervention. Cleared on start-path fresh-failure reset (clean retry)
    and when the session is known dead (no-resume, give-up). *)

val mark_inflight_human_messages_delivered : t -> t
(** Clear [inflight_human_messages] for an active Human response once the
    backend has emitted evidence that it accepted the turn. Does not complete
    the session or change fallback state. No-op for non-Human operations. *)

val set_automerge_enabled : t -> bool -> t
(** Enable or disable automerge for this patch. When the value actually changes,
    the inflight flag is cleared and [automerge_failure_count] is reset;
    disabling additionally clears any pending deadline so the next enable starts
    a fresh timer. Calling with the current value is a no-op — the failure
    count, inflight flag, and [automerge_deadline] are NOT reset in that case.
    If the preserved deadline has already elapsed, the next reconcile tick will
    fire immediately rather than waiting [automerge_idle_timeout]; callers that
    need a fresh timer must call [clear_automerge_deadline] explicitly after
    this function. *)

val set_automerge_deadline : t -> float -> t
(** Record the Unix timestamp at which the supervisor should merge this patch if
    it is still approved. *)

val clear_automerge_deadline : t -> t
(** Clear a pending automerge deadline without disabling automerge. *)

val set_automerge_inflight : t -> bool -> t
(** Set the [automerge_inflight] flag. The reconciler sets it [true] when it
    claims a merge decision; the caller clears it on every exit path (success,
    failure, exception). *)

val increment_automerge_failure_count : t -> t
(** Record a failed automerge call. After [automerge_max_failures] consecutive
    failures the patch is no longer an automerge candidate. *)

val reset_automerge_failure_count : t -> t
(** Reset the consecutive-failure counter to zero. Called on a successful merge
    and whenever automerge is re-toggled. *)

val resume_current_message : t -> op:Types.Operation_kind.t option -> t
(** Resume execution of an already accepted message without reapplying its
    queue-consuming state transition. [~op] restores [current_op] from the
    outbox so that [complete] can clear [human_messages] correctly. Resets
    [current_op_state] to [Queued] — the resumed fiber must call [mark_running]
    when it actually begins work. *)

val mark_running : t -> t
(** Transition [current_op_state] from [Queued] to [Running]. Called from the
    action fiber once the Claude semaphore has been acquired and real work is
    about to begin. No-op when [busy] is false (fiber already exited). *)

(** {2 Queries} *)

val highest_priority : t -> Types.Operation_kind.t option
(** The highest-priority operation in the queue, or [None] if empty. *)

(** {2 Persistence support} *)

val set_pr_number : t -> Types.Pr_number.t -> t
(** Store [pr_number] (making [has_pr] true). Dispatches on
    {!Patch_pr_status.classify_set_present}:
    - [Set_present_recover_same] (prior was [Missing N] or [Present N] with the
      same number): preserves all world-state — the body is still delivered, CI
      runs already accounted, [notified_base_branch] still authoritative.
    - [Set_present_adopt_new] (prior was [Absent], or the number differs):
      resets PR-bootstrap fields ([is_draft = true],
      [pr_body_delivered = false], [start_attempts_without_pr = 0]) plus
      PR-keyed CI history that no longer matches the new PR's check runs
      ([ci_checks = []], [ci_failure_count = 0], [delivered_ci_run_ids = []]).
      Does NOT touch [base_branch] / [notified_base_branch] — those are owned by
      [start] during bootstrap and by the poller during renumbering. *)

val clear_pr : t -> t
(** Remove the PR number and reset PR-related state, returning the agent to the
    no-PR bootstrap path. Tightens to [Present]-only: raises [Invalid_argument]
    on [Absent] or [Missing]. The gameplan-recreate path is the only legitimate
    caller; ad-hoc-vanished should use {!mark_pr_missing} instead. *)

val mark_pr_missing : t -> t
(** Transition the agent from [Present pr] to [Missing pr]: the remote no longer
    has the PR. Minimal: clears only the world-state assertions that are no
    longer authoritative ([is_draft], [merge_ready], [checks_passing],
    [ci_checks]). Everything else — queue (including Human, Ci, Pr_body,
    Findings, Rebase, Review_comments, Merge_conflict), counters,
    [notified_base_branch], [delivered_ci_run_ids], [pr_body_delivered],
    [current_op], [busy], [base_branch] — is preserved so a [Missing → Present]
    recovery via {!set_pr_number} (same-number) is a near-no-op.

    {b Human messages on a [Missing] agent are deferred, not actively
       dispatched.} {!needs_intervention} fires on [is_pr_missing], so the
    planner's Respond/Rebase branches gate the agent off. Queued Human entries
    survive the [Missing] phase and dispatch normally once the PR recovers to
    [Present] (via {!Patch_pr_status.classify_recovery_on_observe} →
    {!Orchestrator.set_pr_number}). The operator's escape if the PR is not
    coming back is [Orchestrator.remove_agent] (i.e., [-N] in the TUI).

    Raises [Invalid_argument] on [Absent] (cannot mark missing what was never
    had) or [Missing] (idempotency is the caller's responsibility — see
    {!Orchestrator.mark_pr_missing} for the idempotent integration-level
    wrapper). *)

val restore :
  patch_id:Types.Patch_id.t ->
  branch:Types.Branch.t ->
  pr_status:Patch_pr_status.t ->
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
  inflight_human_messages:string list ->
  ci_checks:Types.Ci_check.t list ->
  merge_ready:bool ->
  is_draft:bool ->
  pr_body_delivered:bool ->
  pr_body_artifact_miss_count:int ->
  start_attempts_without_pr:int ->
  conflict_noop_count:int ->
  no_commits_push_count:int ->
  push_failure_count:int ->
  branch_rebased_onto:Types.Branch.t option ->
  branch_rebased_onto_sha:string option ->
  anchor_history:Anchor_history.t ->
  checks_passing:bool ->
  current_op:Types.Operation_kind.t option ->
  current_op_state:op_state ->
  current_message_id:Types.Message_id.t option ->
  generation:int ->
  worktree_path:string option ->
  branch_blocked:bool ->
  llm_session_id:string option ->
  automerge_enabled:bool ->
  automerge_deadline:float option ->
  automerge_inflight:bool ->
  automerge_failure_count:int ->
  delivered_ci_run_ids:int list ->
  t
(** Reconstruct agent state from persisted field values. Bypasses precondition
    checks — use only for deserialization. *)
