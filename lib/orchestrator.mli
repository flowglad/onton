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
val set_base_branch : t -> Patch_id.t -> Branch.t -> t
val increment_ci_failure_count : t -> Patch_id.t -> t
val reset_ci_failure_count : t -> Patch_id.t -> t
val set_ci_checks : t -> Patch_id.t -> Ci_check.t list -> t
val set_checks_passing : t -> Patch_id.t -> bool -> t
val set_merge_ready : t -> Patch_id.t -> bool -> t
val set_is_draft : t -> Patch_id.t -> bool -> t
val set_pr_description_applied : t -> Patch_id.t -> bool -> t
val set_implementation_notes_delivered : t -> Patch_id.t -> bool -> t
val increment_start_attempts_without_pr : t -> Patch_id.t -> t
val reset_intervention_state : t -> Patch_id.t -> t
val set_branch_blocked : t -> Patch_id.t -> t
val clear_branch_blocked : t -> Patch_id.t -> t
val reset_busy : t -> Patch_id.t -> t
val set_worktree_path : t -> Patch_id.t -> string -> t
val set_head_branch : t -> Patch_id.t -> Branch.t -> t

(** {2 Queries} *)

val agent : t -> Patch_id.t -> Patch_agent.t
val find_agent : t -> Patch_id.t -> Patch_agent.t option
val all_agents : t -> Patch_agent.t list
val graph : t -> Graph.t
val main_branch : t -> Branch.t
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
[@@deriving show, eq, sexp_of]

val apply_session_result : t -> Patch_id.t -> session_result -> t
(** Apply a Claude session outcome to the orchestrator. Pure function.
    [Session_ok] -> clear_session_fallback. [Session_process_error] /
    [Session_failed] -> on_session_failure + complete. [Session_no_resume] ->
    on_session_failure (not fresh) + complete. [Session_give_up] ->
    set_session_failed + set_tried_fresh + complete. [Session_worktree_missing]
    -> complete. *)

val apply_rebase_result :
  t -> Patch_id.t -> Worktree.rebase_result -> Branch.t -> t
(** Apply a rebase outcome to the orchestrator state. Pure function. [Ok] ->
    set_base_branch + clear_has_conflict + complete. [Noop] -> set_base_branch +
    clear_has_conflict + complete. [Conflict] -> set_base_branch +
    set_has_conflict + enqueue Merge_conflict + complete. [Error _] ->
    set_session_failed + set_tried_fresh \+ complete. *)

val restore :
  graph:Graph.t ->
  agents:Patch_agent.t Map.M(Patch_id).t ->
  outbox:patch_agent_message Map.M(Message_id).t ->
  main_branch:Branch.t ->
  t
(** Reconstruct orchestrator from persisted components. *)
