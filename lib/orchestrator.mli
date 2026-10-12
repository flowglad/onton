(* @archlint.module interface
   @archlint.domain orchestrator *)

open Base
open Types

(** Top-level orchestrator wiring.

    Manages the dependency graph and per-patch agent state. Scheduling and
    reconciliation are owned by [Patch_controller]; this module provides the
    durable state and primitive state transitions. *)

type t

(** {2 Construction} *)

val create : patches:Patch.t list -> main_branch:Branch.t -> t
(** Build orchestrator state from a list of patches and a main branch. *)

type action =
  | Start of Patch_id.t * Branch.t
  | Respond of Patch_id.t * Operation_kind.t
  | Rebase of Patch_id.t * Branch.t
  | Reconcile_branch of Patch_id.t * int
[@@deriving sexp_of, show, eq]

type message_status = Pending | Acked | Completed | Obsolete
[@@deriving sexp_of, show, eq]

type patch_agent_message = {
  message_id : Message_id.t;
  patch_id : Patch_id.t;
  generation : int;
  action : action;
  payload_hash : string;
  status : message_status;
}
[@@deriving sexp_of, show, eq]

val require_dependency_merge : t -> Patch_id.t -> dep:Patch_id.t -> t
(** Upgrade an existing dependency, or add it, to require a merge before Start.
*)

val apply_gameplan_merge_requirements : t -> Gameplan.t -> t
(** Apply the graph-owned merge requirements to fresh or resumed state. *)

val fire : t -> action -> t
(** Apply a single action to the orchestrator state. *)

val accept_message : t -> Message_id.t -> t * action option
(** Durably accept a pending message and fire its action exactly once. Returns
    [Some action] only on first acceptance. Duplicate acceptance is a no-op. *)

val resume_message : t -> Message_id.t -> t * action option
(** Resume execution of an already accepted but incomplete message. Returns
    [Some action] only when the message is still the patch's current message. *)

val reconcile_message : t -> patch_agent_message -> t
(** Insert or refresh a desired pending message. Existing equivalent messages
    are preserved; other pending messages for the same patch are marked
    obsolete. *)

val mark_message_obsolete : t -> Message_id.t -> t

val mark_patch_pending_messages_obsolete_except :
  t -> Patch_id.t -> keep:Message_id.t list -> t

val find_message : t -> Message_id.t -> patch_agent_message option
val all_messages : t -> patch_agent_message list
val current_message : t -> Patch_id.t -> patch_agent_message option
val runnable_messages : t -> patch_agent_message list
val message_id : patch_agent_message -> Message_id.t
val message_patch_id : patch_agent_message -> Patch_id.t
val message_action : patch_agent_message -> action
val message_status : patch_agent_message -> message_status

(** {2 External event application} *)

val complete : t -> Patch_id.t -> t

val complete_start_after_pr_discovery : t -> Patch_id.t -> t
(** Finish a Start after its supervisor-owned PR creation/discovery step. When a
    PR is present, completes normally and consumes any Human guidance carried by
    the Start. When no PR was established, restores that guidance through the
    failed-completion path before retry/intervention. *)

val enqueue : t -> Patch_id.t -> Operation_kind.t -> t
val mark_merged : t -> Patch_id.t -> t
val remove_agent : t -> Patch_id.t -> t

val send_human_message : t -> Patch_id.t -> string -> t
(** Append a human message to the agent's [human_messages] queue, reset
    intervention state (including WONTDO), and enqueue [Operation_kind.Human].
*)

val set_pr_number : t -> Patch_id.t -> Pr_number.t -> t
val clear_pr : t -> Patch_id.t -> t

val mark_pr_missing : t -> Patch_id.t -> t
(** Transition the patch's [pr_status] from [Present] to [Missing], or no-op if
    already [Missing]. The effectful caller (typically the poller's
    PR-rediscovery path) uses this when the remote has lost an ad-hoc PR.

    Idempotent on [Missing] (the second poll cycle after a vanish will hit the
    same classification; without idempotency that would crash). Raises
    [Invalid_argument] on [Absent] — that case represents a caller bug (cannot
    lose what was never had). See {!Patch_pr_status.classify_mark_missing} for
    the pure decision this dispatches on. *)

val set_session_failed : t -> Patch_id.t -> t

(** Record the SHA the base ref resolved to at the moment of a successful rebase
    / start. Called by the runner fiber after [W.read_branch_sha] captures the
    new value. [None] clears the field. *)

val set_tried_fresh : t -> Patch_id.t -> t
val clear_session_fallback : t -> Patch_id.t -> t
val on_session_failure : t -> Patch_id.t -> is_fresh:bool -> t
val on_pr_discovery_failure : t -> Patch_id.t -> t

val begin_forge_observation :
  t ->
  Types.Patch_id.t ->
  request:Forge_observation.request ->
  t * Forge_observation.ticket option

val accept_forge_observation :
  ?confirmed_base:string ->
  t ->
  Types.Patch_id.t ->
  ticket:Forge_observation.ticket ->
  confirmed_head:string option ->
  Forge_observation.t ->
  t * (Poll_outcome.t, string) Result.t

val defer_forge_revision_pair : t -> Patch_id.t -> t
val set_base_branch : t -> Patch_id.t -> Branch.t -> t
val set_notified_base_branch : t -> Patch_id.t -> Branch.t -> t
val increment_ci_failure_count : t -> Patch_id.t -> t
val reset_ci_failure_count : t -> Patch_id.t -> t
val set_ci_checks : t -> Patch_id.t -> Ci_check.t list -> t

val record_delivered_ci_run_ids : t -> Patch_id.t -> int list -> t
(** Record CheckRun [databaseId]s as delivered to the agent for this patch so
    subsequent CI deliveries do not re-deliver the same runs. See
    {!Patch_agent.record_delivered_ci_run_ids}. *)

val set_checks_passing : t -> Patch_id.t -> bool -> t
val set_merge_ready : t -> Patch_id.t -> bool -> t
val set_head_oid : t -> Patch_id.t -> string option -> t

val observe_publication_head :
  ?confirmed_remote_head:string -> t -> Patch_id.t -> string option -> t

val set_review_decision : t -> Patch_id.t -> string option -> t
val set_unresolved_comment_count : t -> Patch_id.t -> int -> t
val set_mergeability_unknown : t -> Patch_id.t -> bool -> t
val set_merge_queue_required : t -> Patch_id.t -> bool -> t

val set_merge_queue_entry :
  t -> Patch_id.t -> Pr_state.merge_queue_entry option -> t

val set_native_stack : t -> Patch_id.t -> bool -> t

val observe_merge_queue :
  t ->
  Patch_id.t ->
  required:bool ->
  entry:Pr_state.merge_queue_entry option ->
  t
(** Apply a poll observation of merge-queue state. Delegates the agent-level
    state rule to {!Automerge_state.observe_merge_queue}, including clearing any
    stale automerge deadline once a queue entry is present. *)

val set_merge_commit_sha : t -> Patch_id.t -> string option -> t
val set_base_contains_merged_siblings : t -> Patch_id.t -> bool -> t
val set_is_draft : t -> Patch_id.t -> bool -> t
val set_pr_body_delivered : t -> Patch_id.t -> bool -> t

val acknowledge_pr_body_refresh :
  t -> Patch_id.t -> publication:Patch_agent.t -> t

val reset_pr_body_artifact_miss_count : t -> Patch_id.t -> t
val increment_start_attempts_without_pr : t -> Patch_id.t -> t
val reset_intervention_state : t -> Patch_id.t -> t
val set_branch_blocked : t -> Patch_id.t -> t
val clear_branch_blocked : t -> Patch_id.t -> t
val reset_busy : t -> Patch_id.t -> t

val mark_running : t -> Patch_id.t -> t
(** Transition the patch's [current_op_state] from [Queued] to [Running]. Called
    from action fibers right after the Claude semaphore has been acquired so the
    TUI can distinguish queued (waiting on slot) from running. No-op if the
    agent is no longer busy. *)

val set_worktree_path : t -> Patch_id.t -> string -> t
val set_llm_session_id : t -> Patch_id.t -> string option -> t

val mark_inflight_human_messages_delivered : t -> Patch_id.t -> t
(** Clear inflight Human messages after backend acceptance of a PR-backed Human
    Respond. Human-carrying Starts deliberately retain guidance until
    {!complete_start_after_pr_discovery}. See
    {!Patch_agent.mark_inflight_human_messages_delivered}. *)

val set_automerge_enabled : t -> Patch_id.t -> bool -> t

val set_automerge_deadline : t -> Patch_id.t -> float -> t
(** Arm an automerge deadline via {!Automerge_state.arm_deadline}. If the patch
    is already known to be in the merge queue, this clears the deadline instead
    of recording an incoherent timer. *)

val clear_automerge_deadline : t -> Patch_id.t -> t
val set_automerge_inflight : t -> Patch_id.t -> bool -> t
val set_review_requested_for_oid : t -> Patch_id.t -> string option -> t
val set_review_request_inflight : t -> Patch_id.t -> bool -> t
val increment_automerge_failure_count : t -> Patch_id.t -> t
val reset_automerge_failure_count : t -> Patch_id.t -> t

val entered_merge_queue : t -> Patch_id.t -> Pr_state.merge_queue_entry -> t
(** Record that the PR is now in GitHub's merge queue, regardless of whether
    that was learned from an automerge enqueue response, manual enqueue, or
    polling. Delegates to {!Automerge_state.entered_merge_queue}. *)

val apply_automerge_failure_state :
  t -> Patch_id.t -> retry_deadline:float -> max_failures:int -> t
(** Apply the pure automerge failure/backoff transition from
    {!Automerge_state.merge_call_failed}. *)

(** {2 Queries} *)

val agent : t -> Patch_id.t -> Patch_agent.t
val find_agent : t -> Patch_id.t -> Patch_agent.t option
val all_agents : t -> Patch_agent.t list
val graph : t -> Graph.t
val main_branch : t -> Branch.t
val set_main_branch : t -> Branch.t -> t

val set_max_ci_failures : t -> max_ci_failures:int -> t
(** Stamp the per-project CI-failure cap onto the orchestrator and every current
    agent, and remember it for agents added later
    ([add_agent]/[add_planned_patch]). Called once at startup by
    [Runtime.create] with the resolved config value — both for fresh
    orchestrators and for snapshot restores, so a changed [--max-ci-failures]
    takes effect on resume. Does not bump agent [generation]s (config stamp, not
    a state transition). *)

val agents_map : t -> Patch_agent.t Map.M(Patch_id).t

val add_agent :
  ?complexity:int option ->
  t ->
  patch_id:Patch_id.t ->
  branch:Branch.t ->
  base_branch:Branch.t ->
  pr_number:Pr_number.t ->
  t
(** Add an ad-hoc agent for a PR not in the gameplan. No-op if the patch_id
    already exists. The agent starts with [has_pr = true] and no deps by
    default.

    [base_branch] is inspected only for dependency-edge inference: if it matches
    the [branch] of another unmerged tracked patch, a graph edge from the new
    patch to that patch is recorded so the existing rebase machinery
    (detect_rebases, initial_base) treats the stacked ad-hoc PR like any other
    stacked patch. If [base_branch] is the main branch, an unknown branch, or a
    merged patch's branch, no edge is inferred. The agent's own [base_branch]
    field is populated by the poller on the next tick. Corresponds to the spec's
    Add action. *)

val add_planned_patch : t -> Patch.t -> deps:Patch_id.t list -> t
(** Register a runtime-added gameplan patch. No-op if the patch id already
    exists. Unlike {!add_agent} this births a PR-less agent (via
    [Patch_agent.create], the startup constructor), so the poller promotes it to
    [Ready_start] once [deps] are satisfied and its base is fresh. The caller
    must insert [patch] into the gameplan in the same snapshot update — a
    [Start] action requires the patch to be present in [gameplan.patches]. *)

(** {2 Persistence support} *)

(** [detail] carries the same formatted reason that [session_driver] writes to
    the activity log (truncated to ~500 chars) — backend, exit code, stderr
    excerpt. It surfaces in [events.jsonl] via [show_session_result] so
    [debug_upload] bundles are self-diagnosing. *)
type session_result = Session_result.t =
  | Session_ok
  | Session_process_error of { is_fresh : bool; detail : string option }
  | Session_no_resume
  | Session_timed_out of { session_id : string option; detail : string option }
      (** Deadline interruption: preserve the session and retry without
          consuming the fresh/resume failure budget. [session_id] is the ID
          captured by this attempt, or its resumed ID; [None] clears any stale
          ID from a previous failed attempt. *)
  | Session_failed of { is_fresh : bool; detail : string option }
  | Session_wontdo of string
      (** Explicit pre-commit opt-out. Retains the explanation and pauses work
          until a human reprompt or intervention reset. *)
  | Session_give_up
  | Session_no_commits
  | Session_context_exhausted
      (** The session exhausted the model's context window. Clears
          [llm_session_id] so the next session starts fresh and bumps
          [context_exhaustion_count]; at [>= 2] the agent surfaces for
          intervention. *)
[@@deriving show, eq, sexp_of]

val apply_session_result : t -> Patch_id.t -> session_result -> t
(** Apply a Claude session outcome to the orchestrator. Pure function.
    [Session_ok] -> clear_session_fallback. [Session_process_error] ->
    on_session_failure + on_pre_session_failure + complete_failed.
    [Session_timed_out] -> set the resumable [session_id] carried by the result,
    clear_session_fallback + complete_failed. [Session_failed] ->
    on_session_failure + complete_failed. [Session_no_resume] -> clear
    llm_session_id \+ complete_failed. [Session_wontdo reason] -> persist reason
    \+ complete, consuming inflight guidance without restoring it to the inbox
    and preserving the session for a human reprompt. [Session_give_up] ->
    set_session_failed + set_tried_fresh + clear llm_session_id +
    complete_failed.

    [Session_ok] and [Session_no_commits] defer completion to the action's
    outcome transition. [Session_no_commits] clears session fallback and
    increments its implementation budget; two empty sessions need intervention.
    Publication failures belong to [Branch_reconcile] and cannot change the
    implementation-session budget. A completed PR-backed Human turn has already
    consumed its guidance at acceptance; failed Start restores retained guidance
    through [apply_start_outcome _ Start_failed]. *)

type start_outcome = Start_ok | Start_failed | Start_stale
[@@deriving show, eq, sexp_of]

val apply_start_outcome : t -> Patch_id.t -> start_outcome -> t
(** Apply the outcome of a Start action fiber. [Start_failed] ->
    [complete_failed], restoring any Human guidance whose Start did not
    successfully complete. [Start_ok] -> identity (caller must use
    {!complete_start_after_pr_discovery} after PR discovery). [Start_stale] ->
    identity. *)

type respond_outcome =
  | Respond_ok
  | Respond_failed
  | Respond_retry_push
  | Respond_no_commits
  | Respond_stale
  | Respond_skip_empty
  | Respond_pr_body_miss
  | Respond_review_unresolved
[@@deriving show, eq, sexp_of]

val apply_respond_outcome :
  t -> Patch_id.t -> Operation_kind.t -> respond_outcome -> t
(** Apply the outcome of a Respond action fiber. [Respond_ok] -> complete +
    reset_no_commits_push_count + kind-specific transitions (Merge_conflict ->
    preserves owner conflict evidence; Review_comments ->
    reset_review_unresolved_cycle_count so the cap counts only consecutive
    non-converged cycles; Pr_body -> set_pr_body_delivered +
    reset_pr_body_artifact_miss_count so the cap counts only consecutive
    misses). [Respond_failed] -> complete_failed (restores inflight human
    messages). [Respond_skip_empty], [Respond_retry_push], and
    [Respond_no_commits] -> complete. [Respond_stale] -> identity.
    [Respond_pr_body_miss] -> complete + increment_pr_body_artifact_miss_count
    (does NOT set_pr_body_delivered — the reconciler re-enqueues Pr_body
    naturally). [Respond_review_unresolved] -> complete +
    increment_review_unresolved_cycle_count — the still-unresolved threads
    re-deliver via the next poll until the cap (>=2) surfaces the agent through
    [needs_intervention]. Every non-stale completion for [Uncommitted_changes]
    also enqueues [Rebase], so both committed and discarded cleanup paths retry
    the blocked rebase. *)

type force_complete_reason = Cancelled | Unexpected_exception
[@@deriving show, eq, sexp_of]

val apply_force_complete :
  ?message_id:Message_id.t -> t -> Patch_id.t -> force_complete_reason -> t
(** Pure applicator for runner fibers that exited abnormally while the agent was
    [busy]. The single source of truth for the [bin/main.ml] [with_busy_guard]
    finally and [mark_session_failed] sites that previously called [complete]
    directly — and so silently dropped [inflight_human_messages].

    Semantics:
    - Unknown patch: identity.
    - [Unexpected_exception]: always advances [session_fallback] via
      [set_session_failed] then [set_tried_fresh] (preserving the prior
      [mark_session_failed] semantics, which pushed [Fresh_available] all the
      way to [Given_up]). This still runs even when the agent is not busy. When
      [message_id] is supplied, the transition is applied only while that
      message still owns the agent. This prevents a stale runner fiber from
      completing a newer operation for the same patch.

    - [Cancelled]: leaves [session_fallback] alone — a clean cancel should not
      poison the fallback chain.
    - If [busy] AND [inflight_human_messages <> []]: routes through
      [complete_failed], which restores inflight back to [human_messages] and
      re-enqueues [Operation_kind.Human].
    - If [busy] AND inflight is empty: routes through plain [complete].
    - If not [busy]: skip the complete step. *)

val restore :
  ?promotion_claimed:bool ->
  graph:Graph.t ->
  agents:Patch_agent.t Map.M(Patch_id).t ->
  outbox:patch_agent_message Map.M(Message_id).t ->
  main_branch:Branch.t ->
  unit ->
  t
(** Reconstruct orchestrator from persisted components. *)

val start_eligibility :
  t ->
  base_contains_merged_siblings:bool ->
  Branch.t ->
  Start_eligibility.decision
(** [start_eligibility t ~base_contains_merged_siblings base] is the freshness
    verdict for a hypothetical [Start]/[Rebase] action whose base is [base],
    evaluated against [t]'s dependency graph and the base patch's recorded
    rebase anchor. [base_contains_merged_siblings] is the launching patch's
    poll-derived base-containment cache. Used to gate [Start] and [Rebase] in
    {!runnable_messages}; exposed for tests/TUI introspection.

    Returns [Allow] when the base is main, when the base patch is merged
    (effectively main), or when the base patch's local branch is rebased onto
    its structurally-correct base ([branch_rebased_onto = Graph.initial_base]),
    has no unresolved conflict ([has_conflict] — a conflicted rebase keeps the
    gate closed until the resolution force-pushes the rewritten tip), and
    contains the launching patch's merged siblings. Otherwise returns
    [Defer reason]. Freshness is dependency-scoped: an unrelated advance of
    [origin/main] never defers a [Start]. See {!Start_eligibility}. *)

val execution_mode : t -> Execution_mode.t
val set_execution_mode : t -> Execution_mode.t -> t
val is_integration_root : t -> Patch_id.t -> bool
val is_feature_descendant : t -> Patch_id.t -> bool

val respond_pr_number :
  t -> Patch_id.t -> Operation_kind.t -> Pr_number.t option
(** PR receiving a response. Descendant implementation notes belong to the
    integration root PR; all other responses address the patch's own PR. *)

val branch_only_published : t -> Patch_id.t -> bool
val terminal_branch : t -> Patch_id.t -> Branch.t
val open_deps : t -> Patch_id.t -> Patch_id.t list
val expected_base : t -> Patch_id.t -> Branch.t option
val construction_open : t -> bool
val claim_promotion : t -> t
val release_promotion : t -> t
val additions_allowed : t -> dependencies:Patch_id.t list -> bool
val invalidate_root_readiness : t -> t
val refresh_base_branch : t -> Patch_id.t -> t
val promotion_claimed : t -> bool
val settle_restored_promotion : t -> t

val reconcile_branch :
  t ->
  Types.Patch_id.t ->
  Branch_reconcile.event ->
  t * Branch_reconcile.effect_command list

val message_of_action : Patch_agent.t -> action -> patch_agent_message
(** Branch reconciliation messages are identified by operation, independently of
    polling's patch generation. Other message identities retain their
    generation. *)

val record_session_completion :
  t -> Patch_id.t -> Session_result.completion -> t
