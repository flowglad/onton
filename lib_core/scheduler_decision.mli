open Types

type input = {
  graph : Graph.t;
  mode : Execution_mode.t;
  agents : Patch_agent.t list;
  main : Branch.t;
}
(** Immutable scheduling projection. The coordinator and local adapter construct
    the same view; planning never updates a patch owner. Unknown peers fail
    closed instead of being replaced by freshly created default agents. *)

type decision =
  | Start of Patch_id.t * Branch.t
  | Respond of Patch_id.t * Operation_kind.t
  | Rebase of Patch_id.t * Branch.t
  | Reconcile_branch of Patch_id.t * int

val plan : input -> (Patch_id.t * decision option) list
val for_patch : input -> Patch_id.t -> decision option
val selected_base : input -> Patch_id.t -> Branch.t option
val dependencies_admitted : input -> Patch_id.t -> bool
val open_dependency_ready : Patch_agent.t -> bool
val ready_for_review : input -> Patch_id.t -> bool
val evidence_id : input -> Patch_id.t -> string option
val draft_allowed : input -> Patch_id.t -> bool

type recovery_intent = { pr_base_update : bool; pr_draft_update : bool }

val recovery_intent :
  input -> gameplan_patch:bool -> Patch_id.t -> recovery_intent

val automerge_candidate :
  ?ignore_inflight:bool -> Patch_agent.t -> main:Branch.t -> bool
