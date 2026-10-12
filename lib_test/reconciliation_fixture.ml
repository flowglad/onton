(* @archlint.module test
   @archlint.domain test-support *)

open Base
open Onton
open Onton_core
open Types
module B = Branch_reconcile
module P = Onton_core_test_support.Publication_fixture
module R = Onton_core_test_support.Replay_fixture

(* Scheduling fixtures supply the repository's rewrite/no-op observation. The
   completion itself must traverse captured, pinned, integrated and confirmed
   owner states so cascade/readiness assertions exercise the runtime policy. *)
let prepare ~noop t pid base =
  let state t = (Orchestrator.agent t pid).Patch_agent.branch_reconcile in
  let step t event = fst (Orchestrator.reconcile_branch t pid event) in
  let reply = P.reply ~step ~state in
  let revision label =
    match B.Commit.make (P.sha label) with
    | Some sha -> sha
    | None ->
        QCheck2.Test.fail_reportf "invalid reconciliation fixture revision"
  in
  let key = Patch_id.to_string pid in
  let origin = revision (key ^ ":materialization") in
  let t =
    if Option.is_none (B.materialization (state t)) then
      step t (B.Materialized (B.New_branch origin))
    else t
  in
  let sequence =
    match B.operation (state t) with None -> 1 | Some op -> op.B.id + 1
  in
  let source =
    match List.hd (B.integrations (state t)) with
    | Some receipt -> receipt.B.integrated_revision
    | None -> revision (key ^ ":implementation")
  in
  let target = revision (Branch.to_string base ^ ":base") in
  let candidate =
    if noop then source
    else revision (key ^ ":rewrite:" ^ Int.to_string sequence)
  in
  let policy =
    if Orchestrator.is_integration_root t pid then B.Preserve_ancestry
    else B.Rewrite
  in
  let t =
    step t
      (B.Request
         B.
           {
             base = Branch.to_string base;
             policy;
             purpose = Reconcile_request ("fixture:" ^ Int.to_string sequence);
           })
  in
  let boundary =
    match List.hd (R.boundaries (state t)) with
    | Some boundary -> boundary
    | None -> B.Plain
  in
  let t =
    reply t (B.Observed (R.observation ~source ~target ~boundary ~noop))
  in
  let t = reply t B.Pinned in
  (t, source, target, candidate)

let state t pid = (Orchestrator.agent t pid).Patch_agent.branch_reconcile
let step t pid event = fst (Orchestrator.reconcile_branch t pid event)

let reply t pid result =
  P.reply ~step:(fun t e -> step t pid e) ~state:(fun t -> state t pid) t result

let verify_scope t pid =
  P.verify_scope ~step:(fun t e -> step t pid e) ~state:(fun t -> state t pid) t

let confirm t pid candidate =
  let t =
    reply t pid (B.Remote { sha = Some candidate; topology = B.Equal })
    |> fun t -> verify_scope t pid
  in
  if not (Option.equal B.equal_phase (B.phase (state t pid)) (Some B.Settled))
  then QCheck2.Test.fail_reportf "fixture reconciliation did not settle";
  let head = B.Commit.to_string candidate in
  let t = Orchestrator.set_head_oid t pid (Some head) in
  let t = Orchestrator.observe_publication_head t pid (Some head) in
  Forge_poll_fixture.readiness (Orchestrator.complete t pid) pid

let rebase ?(noop = false) t pid base =
  let t, _, _, candidate = prepare ~noop t pid base in
  let t =
    if noop then t
    else
      reply t pid (B.Integrated candidate) |> fun t ->
      verify_scope t pid |> fun t -> reply t pid B.Published
  in
  confirm t pid candidate

let conflict t pid base =
  let t, source, _, _ = prepare ~noop:false t pid base in
  let t =
    reply t pid
      (B.Conflict { head = source; sequencer = "fixture-rebase"; conflicts = 1 })
  in
  Orchestrator.complete t pid

let resolve_conflict t pid =
  match B.operation (state t pid) with
  | None -> QCheck2.Test.fail_reportf "missing conflicted owner"
  | Some op -> (
      match (op.B.source, op.B.target) with
      | None, _ | _, None ->
          QCheck2.Test.fail_reportf "missing captured revisions"
      | Some source, Some target -> (
          let t = step t pid B.Recover in
          let event =
            P.completion
              ~state:(fun t -> state t pid)
              t
              (R.repair_inspection ~source ~target ~boundary:op.B.boundary
                 ~sequencer:"fixture-rebase" ~conflicts:1)
          in
          let t, effects = Orchestrator.reconcile_branch t pid event in
          let token =
            List.find_map effects ~f:(function
              | B.Repair token -> Some token
              | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
          in
          match token with
          | None -> QCheck2.Test.fail_reportf "repair was not offered"
          | Some token ->
              let t = step t pid (B.Repair_started token) in
              let t = step t pid (B.Repair_completed { token; at = 100. }) in
              let t =
                reply t pid
                  (R.repair_inspection ~source ~target ~boundary:op.B.boundary
                     ~sequencer:"fixture-rebase" ~conflicts:0)
              in
              let candidate = R.commit (100000 + op.B.id) in
              let t =
                reply t pid (B.Integrated candidate) |> fun t ->
                verify_scope t pid
              in
              let t = reply t pid B.Published in
              confirm t pid candidate))
