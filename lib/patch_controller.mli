open Types

type github_effect =
  | Set_pr_description of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      body : string;
    }
  | Set_pr_draft of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      draft : bool;
    }
[@@deriving show, eq, sexp_of]

val reconcile_patch :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  patch:Patch.t ->
  Orchestrator.t * github_effect list
(** Reconcile durable per-patch lifecycle facts into queue updates and GitHub
    effects. The same snapshot always produces the same result. *)

val reconcile_all :
  Orchestrator.t ->
  project_name:string ->
  gameplan:Gameplan.t ->
  Orchestrator.t * github_effect list
(** Reconcile all gameplan patches. Ad-hoc patches are ignored. *)

val apply_github_effect_success :
  Orchestrator.t -> github_effect -> Orchestrator.t
(** Apply the durable state changes that follow a successful GitHub effect. *)
