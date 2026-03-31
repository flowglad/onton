open Types

(** Pure decision logic for post-action completion.

    Extracts the orchestrator state transitions and GitHub side effects that
    follow a successful Start (PR discovered) or Respond (feedback delivered).
    All functions are pure — the runner in [main.ml] interprets the returned
    effects. *)

(** Side effects to perform against the GitHub API. *)
type github_effect =
  | Set_pr_description of { pr_number : Pr_number.t; body : string }
  | Set_pr_draft of { pr_number : Pr_number.t; draft : bool }
[@@deriving show, eq, sexp_of]

val on_start_discovered :
  Orchestrator.t ->
  patch_id:Patch_id.t ->
  pr_number:Pr_number.t ->
  base_branch:Branch.t ->
  main_branch:Branch.t ->
  pr_body:string ->
  Orchestrator.t * github_effect list
(** After a successful Start whose PR was discovered on GitHub.

    Transitions: sets pr_number, enqueues Implementation_notes, completes.
    Effects: sets PR description; sets draft status (draft for non-main,
    un-draft for main). *)

val on_start_discovery_failed :
  Orchestrator.t -> patch_id:Patch_id.t -> Orchestrator.t
(** After a Start where PR discovery failed (all retries exhausted).

    Escalates session_fallback via [on_pr_discovery_failure] and completes. No
    GitHub effects. *)

val on_respond_ok :
  Orchestrator.t ->
  patch_id:Patch_id.t ->
  kind:Operation_kind.t ->
  main_branch:Branch.t ->
  Orchestrator.t * github_effect list
(** After a successful Respond completion.

    Transitions: completes the agent; clears has_conflict for Merge_conflict.
    Effects: un-drafts the PR after Implementation_notes when base is main. *)
