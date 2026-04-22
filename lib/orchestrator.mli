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
val enqueue : t -> Patch_id.t -> Operation_kind.t -> t
val mark_merged : t -> Patch_id.t -> t
val remove_agent : t -> Patch_id.t -> t

val send_human_message : t -> Patch_id.t -> string -> t
(** Append a human message to the agent's [human_messages] queue, reset
    intervention state, and enqueue [Operation_kind.Human]. *)

val set_pr_number : t -> Patch_id.t -> Pr_number.t -> t
val clear_pr : t -> Patch_id.t -> t
val set_session_failed : t -> Patch_id.t -> t
val set_tried_fresh : t -> Patch_id.t -> t
val clear_session_fallback : t -> Patch_id.t -> t
val on_session_failure : t -> Patch_id.t -> is_fresh:bool -> t
val on_pr_discovery_failure : t -> Patch_id.t -> t
val set_has_conflict : t -> Patch_id.t -> t
val clear_has_conflict : t -> Patch_id.t -> t
val reset_conflict_noop_count : t -> Patch_id.t -> t
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
val set_is_draft : t -> Patch_id.t -> bool -> t
val set_pr_body_delivered : t -> Patch_id.t -> bool -> t
val increment_start_attempts_without_pr : t -> Patch_id.t -> t
val reset_intervention_state : t -> Patch_id.t -> t
val set_branch_blocked : t -> Patch_id.t -> t
val clear_branch_blocked : t -> Patch_id.t -> t
val reset_busy : t -> Patch_id.t -> t
val set_worktree_path : t -> Patch_id.t -> string -> t
val set_llm_session_id : t -> Patch_id.t -> string option -> t
val set_automerge_enabled : t -> Patch_id.t -> bool -> t
val set_automerge_deadline : t -> Patch_id.t -> float -> t
val clear_automerge_deadline : t -> Patch_id.t -> t
val set_automerge_inflight : t -> Patch_id.t -> bool -> t
val increment_automerge_failure_count : t -> Patch_id.t -> t
val reset_automerge_failure_count : t -> Patch_id.t -> t

(** {2 Queries} *)

val agent : t -> Patch_id.t -> Patch_agent.t
val find_agent : t -> Patch_id.t -> Patch_agent.t option
val all_agents : t -> Patch_agent.t list
val graph : t -> Graph.t
val main_branch : t -> Branch.t
val set_main_branch : t -> Branch.t -> t
val agents_map : t -> Patch_agent.t Map.M(Patch_id).t

val add_agent :
  t -> patch_id:Patch_id.t -> branch:Branch.t -> pr_number:Pr_number.t -> t
(** Add an ad-hoc agent for a PR not in the gameplan. No-op if the patch_id
    already exists. The agent starts with [has_pr = true] and no dependencies.
    Corresponds to the spec's Add action. *)

(** {2 Persistence support} *)

type session_result =
  | Session_ok
  | Session_process_error of { is_fresh : bool }
  | Session_no_resume
  | Session_failed of { is_fresh : bool }
  | Session_give_up
  | Session_worktree_missing
  | Session_push_failed
  | Session_no_commits
[@@deriving show, eq, sexp_of]

val apply_session_result : t -> Patch_id.t -> session_result -> t
(** Apply a Claude session outcome to the orchestrator. Pure function.
    [Session_ok] -> clear_session_fallback. [Session_process_error] ->
    on_session_failure + on_pre_session_failure + complete_failed.
    [Session_failed] -> on_session_failure + complete_failed.
    [Session_no_resume] -> on_session_failure (not fresh) + clear llm_session_id
    \+ complete_failed. [Session_give_up] -> set_session_failed +
    set_tried_fresh + clear llm_session_id + complete_failed.
    [Session_worktree_missing] -> on_pre_session_failure + complete_failed.

    {b Deferred completion}: [Session_push_failed] and [Session_no_commits] do
    NOT complete the agent — they only adjust state ([clear_session_fallback] in
    both cases; [increment_no_commits_push_count] for [Session_no_commits]).
    [busy] remains [true]. The caller MUST follow up in the same atomic snapshot
    write with either [apply_start_outcome _ Start_failed] (Start path) or
    [apply_respond_outcome _ _ Respond_retry_push] (Respond path) to clear
    [busy]. This two-phase design is deliberate: the LLM session itself
    succeeded (messages were delivered; commits were made locally), so any
    inflight human payload must be consumed by plain [complete] rather than
    restored by [complete_failed] — the latter would re-enqueue Human and cause
    an infinite re-delivery loop. After 2 consecutive [Session_no_commits]
    outcomes, [needs_intervention] fires via [no_commits_push_count >= 2]. *)

val combine_session_and_push :
  session:session_result -> push:Worktree.push_result -> session_result
(** Pure: fold the LLM session outcome and the supervisor's post-session push
    outcome into a single [session_result] for [apply_session_result].
    - A pre-existing LLM failure ([Session_process_error], [Session_failed],
      [Session_no_resume], [Session_give_up], [Session_worktree_missing],
      [Session_push_failed]) is preserved unchanged — the push outcome doesn't
      change anything.
    - [Session_ok] with [Push_ok] or [Push_up_to_date] stays [Session_ok].
    - [Session_ok] with [Push_rejected] or [Push_error] becomes
      [Session_push_failed] — the LLM ran fine but commits didn't ship.
    - [Session_ok] with [Push_no_commits] becomes [Session_no_commits] — the LLM
      ran cleanly but left no commits on the branch, so the push was skipped (a
      base-equal branch can't become a PR). *)

type start_outcome = Start_ok | Start_failed | Start_stale
[@@deriving show, eq, sexp_of]

val apply_start_outcome : t -> Patch_id.t -> start_outcome -> t
(** Apply the outcome of a Start action fiber. [Start_failed] -> complete.
    [Start_ok] -> identity (caller must [complete] after PR discovery).
    [Start_stale] -> identity. *)

type respond_outcome =
  | Respond_ok
  | Respond_failed
  | Respond_retry_push
  | Respond_stale
  | Respond_skip_empty
[@@deriving show, eq, sexp_of]

val apply_respond_outcome :
  t -> Patch_id.t -> Operation_kind.t -> respond_outcome -> t
(** Apply the outcome of a Respond action fiber. [Respond_ok] -> complete +
    kind-specific transitions (Merge_conflict -> clear_has_conflict +
    reset_conflict_noop_count; Pr_body -> set_pr_body_delivered).
    [Respond_failed] -> complete_failed (restores inflight human messages).
    [Respond_skip_empty] -> complete. [Respond_retry_push] -> complete.
    [Respond_stale] -> identity. *)

(** Side effects emitted by rebase result application. The runner is responsible
    for executing these (e.g. force-pushing the branch to the remote). Modeled
    as data so property tests can assert on effect presence. *)
type rebase_effect = Push_branch [@@deriving show, eq, sexp_of]

val apply_rebase_result :
  t ->
  Patch_id.t ->
  Worktree.rebase_result ->
  Branch.t ->
  t * rebase_effect list
(** Apply a rebase outcome to the orchestrator state. Pure function. [Ok] ->
    set_base_branch + clear_has_conflict + reset_conflict_noop_count + complete
    \+ [[Push_branch]]. [Noop] -> set_base_branch + complete + [[]]. [Conflict]
    -> set_base_branch + set_has_conflict + enqueue Merge_conflict + complete.
    [Error _] -> set_session_failed + set_tried_fresh + complete. *)

type rebase_push_resolution =
  | Rebase_push_ok
  | Rebase_push_failed
  | Rebase_push_error
[@@deriving show, eq, sexp_of]

val apply_rebase_push_result :
  t -> Patch_id.t -> Worktree.push_result option -> t * rebase_push_resolution
(** Pure second stage of rebase handling. Takes the push outcome from executing
    the [Push_branch] effect ([None] when no push was requested). Returns the
    updated state and resolution.

    - [Rebase_push_ok]: push succeeded or was up-to-date; no further action.
    - [Rebase_push_failed]: push was rejected (lease failure); resets conflict
      state and enqueues [Merge_conflict] so the next poll cycle retries.
    - [Rebase_push_error]: infrastructure error; enqueues [Rebase] to retry
      without entering the conflict path. *)

type conflict_rebase_decision =
  | Conflict_resolved
  | Deliver_to_agent
  | Conflict_failed
[@@deriving show, eq, sexp_of]

val apply_conflict_rebase_result :
  t ->
  Patch_id.t ->
  Worktree.rebase_result ->
  Branch.t ->
  t * conflict_rebase_decision * rebase_effect list
(** Apply a rebase outcome during merge-conflict resolution. Pure function. [Ok]
    -> clear_has_conflict + reset_conflict_noop_count + complete +
    [Conflict_resolved] + [[Push_branch]]. [Noop] -> clear_has_conflict +
    increment_conflict_noop_count + complete + [Conflict_resolved] +
    [[Push_branch]] (local is correct; has_conflict cleared so it purely tracks
    GitHub state — the poller will re-set and re-enqueue if conflict persists).
    [Conflict] -> set_has_conflict + [Deliver_to_agent] + [[]]. [Error _] ->
    set_session_failed + complete + [Conflict_failed]. *)

type conflict_resolution =
  | Conflict_done
  | Conflict_retry_push
  | Conflict_needs_agent
  | Conflict_give_up
[@@deriving show, eq, sexp_of]

val apply_conflict_push_result :
  t ->
  Patch_id.t ->
  conflict_rebase_decision ->
  Worktree.push_result option ->
  t * conflict_resolution
(** Pure second stage of merge-conflict resolution. Takes the rebase decision
    from [apply_conflict_rebase_result] and the outcome of executing the
    [Push_branch] effect ([None] when no push was requested). Returns the final
    resolution and updated state.

    - [Conflict_done]: conflict fully resolved (rebase succeeded and push
      landed).
    - [Conflict_retry_push]: rebase succeeded locally but push failed;
      re-enqueues [Merge_conflict] so the next cycle retries.
    - [Conflict_needs_agent]: the coding agent must resolve the conflict
      manually (rebase was noop/conflict — push result is irrelevant because the
      rebase itself didn't resolve the conflict).
    - [Conflict_give_up]: unrecoverable rebase error. *)

val restore :
  graph:Graph.t ->
  agents:Patch_agent.t Map.M(Patch_id).t ->
  outbox:patch_agent_message Map.M(Message_id).t ->
  main_branch:Branch.t ->
  t
(** Reconstruct orchestrator from persisted components. *)
