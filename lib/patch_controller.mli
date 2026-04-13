open Types

type github_effect =
  | Set_pr_draft of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      draft : bool;
    }
  | Set_pr_base of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      base : Branch.t;
    }
[@@deriving show, eq, sexp_of]

type poll_log_entry = { message : string; patch_id : Patch_id.t }
[@@deriving show, eq]

type poll_observation = {
  poll_result : Poller.t;
  base_branch : Branch.t option;
  branch_in_root : bool;
  worktree_path : string option;
}

val discovery_intents : Orchestrator.t -> (Patch_id.t * Branch.t) list
(** Patches that have run at least once ([has_session]) but lack a PR and are
    not merged. Returns [(patch_id, branch)] pairs for tick-based PR discovery
    in the poller. *)

val reconcile_patch :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  patch:Patch.t ->
  Orchestrator.t * github_effect list
(** Reconcile durable per-patch lifecycle facts into queue updates and GitHub
    effects. The same snapshot always produces the same result. *)

val apply_poll_result :
  Orchestrator.t ->
  Patch_id.t ->
  poll_observation ->
  Orchestrator.t * poll_log_entry list * bool
(** Apply a GitHub poll observation to durable state. Returns the updated
    orchestrator, log entries, and whether the patch became newly branch-
    blocked in this step. This is the controller-owned pure poll ingestion step.
*)

val apply_replacement_pr :
  Orchestrator.t ->
  Patch_id.t ->
  pr_number:Pr_number.t ->
  base_branch:Branch.t ->
  merged:bool ->
  Orchestrator.t
(** Apply replacement-PR discovery after a closed PR is re-mapped to a new open
    PR for the same patch. *)

val reconcile_all :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  Orchestrator.t * github_effect list
(** Reconcile all gameplan patches. Ad-hoc patches are ignored. *)

val plan_actions :
  Orchestrator.t -> patches:Patch.t list -> Orchestrator.action list
(** Compute runnable actions from the current snapshot after reconciliation.
    This is the evergreen scheduler used by the main loop. *)

val plan_messages :
  Orchestrator.t ->
  patches:Patch.t list ->
  Orchestrator.patch_agent_message list
(** Compute durable runnable messages from the current snapshot after
    reconciliation. Accepted but incomplete messages are replayed; new desired
    actions become pending messages in the outbox. *)

val plan_tick_messages :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  Orchestrator.t * github_effect list * Orchestrator.patch_agent_message list
(** Reconcile durable state, emit missing GitHub effects, and compute durable
    runnable patch-agent messages for the same snapshot. *)

val plan_tick :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  Orchestrator.t * github_effect list * Orchestrator.action list
(** Reconcile durable state, emit missing GitHub effects, and compute runnable
    actions for the same snapshot. *)

val tick :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  Orchestrator.t * github_effect list * Orchestrator.action list
(** Reconcile durable state, emit missing GitHub effects, and fire the planned
    actions into the orchestrator state. The returned action list is the set of
    actions that were fired. *)

val apply_github_effect_success :
  Orchestrator.t -> github_effect -> Orchestrator.t
(** Apply the durable state changes that follow a successful GitHub effect. *)
