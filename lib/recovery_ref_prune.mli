(* @archlint.module interface
   @archlint.domain prune-decision *)

type outcome = Pruned | Retained of Branch_reconcile.Commit.t list

val repository_id : Branch_reconcile_executor.io -> (string, string) result
(** Canonical common Git directory, shared by a repository's linked worktrees.
*)

val reclaim :
  io:Branch_reconcile_executor.io ->
  project:string ->
  protected_projects:string list ->
  required:Branch_reconcile.Commit.t list ->
  (outcome, string) result
(** Observe reachability and compare-and-delete only this project's captured
    recovery refs. A failed probe never proves absence. Call with the project
    and all surviving owners sharing this repository locked throughout; foreign
    recovery refs used as evidence must remain stable until deletion finishes. A
    partial deletion or process interruption is safe to retry. *)
