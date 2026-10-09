(* @archlint.module exempt
   @archlint.exempt-reason test-support *)

open Onton
open Onton_core
module C = Onton_core_test_support.Conflict_fixture

let conflict t pid =
  let agent = Orchestrator.agent t pid in
  let base =
    match agent.Patch_agent.base_branch with
    | Some base -> base
    | None -> Orchestrator.main_branch t
  in
  C.conflict
    ~base:(Types.Branch.to_string base)
    ~step:(fun t event -> fst (Orchestrator.reconcile_branch t pid event))
    ~state:(fun t -> (Orchestrator.agent t pid).Patch_agent.branch_reconcile)
    t

let resolve t pid =
  let t =
    C.resolve
      ~step:(fun t event -> fst (Orchestrator.reconcile_branch t pid event))
      ~state:(fun t -> (Orchestrator.agent t pid).Patch_agent.branch_reconcile)
      t
  in
  match
    Branch_reconcile.publication_observation_target
      (Orchestrator.agent t pid).Patch_agent.branch_reconcile
  with
  | None -> t
  | Some identity ->
      let head =
        Some
          (Branch_reconcile.Commit.to_string identity.Branch_reconcile.revision)
      in
      Orchestrator.observe_publication_head
        (Orchestrator.set_head_oid t pid head)
        pid head
