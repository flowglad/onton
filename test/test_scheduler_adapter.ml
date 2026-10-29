(* @archlint.module test
   @archlint.domain scheduler-decision *)

open Base
open Onton
open Onton_core
open Types

let patch id deps =
  Patch.
    {
      id = Patch_id.of_string id;
      title = id;
      description = "";
      branch = Branch.of_string ("patch/" ^ id);
      dependencies = deps;
      spec = "";
      acceptance_criteria = [];
      files = [];
      classification = "";
      changes = [];
      test_stubs_introduced = [];
      test_stubs_implemented = [];
      complexity = None;
      precedents = [];
      required_context = [];
    }

let () =
  let a = patch "a" [] and b = patch "b" [ Patch_id.of_string "a" ] in
  let patches = [ a; b ] in
  let t = Orchestrator.create ~patches ~main_branch:(Branch.of_string "main") in
  let view : Scheduler_decision.input =
    Scheduler_decision.
      {
        graph = Orchestrator.graph t;
        mode = Orchestrator.execution_mode t;
        agents = Orchestrator.all_agents t;
        main = Orchestrator.main_branch t;
      }
  in
  let main = Orchestrator.main_branch t in
  let parent_agent = Orchestrator.agent t a.Patch.id in
  assert (Scheduler_decision.dependencies_admitted view a.Patch.id);
  assert (
    Option.equal Branch.equal
      (Scheduler_decision.selected_base view a.Patch.id)
      (Some main));
  assert (Option.is_some (Scheduler_decision.evidence_id view a.Patch.id));
  assert (Option.is_some (Scheduler_decision.for_patch view a.Patch.id));
  assert (not (Scheduler_decision.ready_for_review view a.Patch.id));
  assert (not (Scheduler_decision.open_dependency_ready parent_agent));
  assert (not (Scheduler_decision.draft_allowed view a.Patch.id));
  let recovery =
    Scheduler_decision.recovery_intent view ~gameplan_patch:true a.Patch.id
  in
  assert (not recovery.Scheduler_decision.pr_base_update);
  assert (not recovery.Scheduler_decision.pr_draft_update);
  assert (not (Scheduler_decision.automerge_candidate parent_agent ~main));
  let expected =
    Scheduler_decision.plan view
    |> List.filter_map ~f:(function
      | _, None -> None
      | _, Some (Scheduler_decision.Start (id, base)) ->
          Some (Orchestrator.Start (id, base))
      | _, Some (Scheduler_decision.Respond (id, k)) ->
          Some (Orchestrator.Respond (id, k))
      | _, Some (Scheduler_decision.Rebase (id, base)) ->
          Some (Orchestrator.Rebase (id, base))
      | _, Some (Scheduler_decision.Reconcile_branch (id, op)) ->
          Some (Orchestrator.Reconcile_branch (id, op)))
  in
  let actual = Patch_controller.plan_actions t ~patches in
  assert (List.equal Orchestrator.equal_action actual expected);
  assert (List.length actual = 1);
  let action =
    Option.value (List.hd actual)
      ~default:(Orchestrator.Start (a.Patch.id, Branch.of_string "main"))
  in
  let message =
    Orchestrator.message_of_action (Orchestrator.agent t a.Patch.id) action
  in
  let t = Orchestrator.reconcile_message t message in
  let state =
    Coordinator_state.create ~run_id:"run" ~config_id:"config" ~plan_id:"plan"
      ~plan_bytes:"frozen-plan" ~config_bytes:"nonsecret-config" ~capacity:2
      ~budget_bytes:8388608
  in
  let op : Coordinator_state.operation =
    Coordinator_state.
      {
        id = Message_id.to_string (Orchestrator.message_id message);
        patch = a.Patch.id;
        used = 0;
        budget = 3;
        retry_after = 0L;
        attention = false;
        unfinished = true;
        maintenance = false;
      }
  in
  let state = fst (Coordinator_state.step state (Coordinator_state.Admit op)) in
  let context =
    Orchestrator.assigned_context t state ~run_id:"run" ~config_id:"config"
      ~plan_id:"plan" ~message
  in
  (match context with
  | Error reason -> failwith reason
  | Ok c ->
      assert (String.equal (Worker_protocol_v3.selected_base c) "main");
      assert (String.equal (Worker_protocol_v3.execution_mode c) "mainline"));
  assert (
    Result.is_error
      (Orchestrator.assigned_context t state ~run_id:"run" ~config_id:"config"
         ~plan_id:"wrong-plan" ~message));
  let stale = { message with Orchestrator.payload_hash = "changed" } in
  assert (
    Result.is_error
      (Orchestrator.assigned_context t state ~run_id:"run" ~config_id:"config"
         ~plan_id:"plan" ~message:stale));
  let shifted =
    Orchestrator.set_base_branch t a.Patch.id (Branch.of_string "other")
  in
  assert (
    Result.is_error
      (Orchestrator.assigned_context shifted state ~run_id:"run"
         ~config_id:"config" ~plan_id:"plan" ~message));
  let feature_mode =
    Result.ok_or_failwith (Execution_mode.infer (Graph.of_patches patches))
  in
  let feature =
    Orchestrator.create ~patches ~main_branch:(Branch.of_string "main")
    |> fun o -> Orchestrator.set_execution_mode o feature_mode
  in
  let root_action = Orchestrator.Start (a.Patch.id, Branch.of_string "main") in
  let root_message =
    Orchestrator.message_of_action
      (Orchestrator.agent feature a.Patch.id)
      root_action
  in
  let feature = Orchestrator.reconcile_message feature root_message in
  let root_op : Coordinator_state.operation =
    Coordinator_state.
      {
        id = Message_id.to_string (Orchestrator.message_id root_message);
        patch = a.Patch.id;
        used = 0;
        budget = 3;
        retry_after = 0L;
        attention = false;
        unfinished = true;
        maintenance = false;
      }
  in
  let root_state =
    fst
      (Coordinator_state.step
         (Coordinator_state.create ~run_id:"run" ~config_id:"config"
            ~plan_id:"plan" ~plan_bytes:"frozen-plan"
            ~config_bytes:"nonsecret-config" ~capacity:2 ~budget_bytes:8388608)
         (Coordinator_state.Admit root_op))
  in
  (match
     Orchestrator.assigned_context feature root_state ~run_id:"run"
       ~config_id:"config" ~plan_id:"plan" ~message:root_message
   with
  | Ok c ->
      assert (String.equal (Worker_protocol_v3.execution_mode c) "feature");
      assert (String.equal (Worker_protocol_v3.root_role c) "root")
  | Error reason -> failwith reason);
  Stdio.printf "local scheduler shared planning parity: OK\n"

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"scheduler decision API covers generated patch identities"
       ~count:100 (QCheck2.Gen.int_range 1 100_000) (fun seed ->
         try
           let suffix = Int.to_string seed in
           let root = patch ("root-" ^ suffix) [] in
           let child = patch ("child-" ^ suffix) [ root.Patch.id ] in
           let patches = [ root; child ] in
           let main = Branch.of_string "main" in
           let orchestrator = Orchestrator.create ~patches ~main_branch:main in
           let view : Scheduler_decision.input =
             Scheduler_decision.
               {
                 graph = Orchestrator.graph orchestrator;
                 mode = Orchestrator.execution_mode orchestrator;
                 agents = Orchestrator.all_agents orchestrator;
                 main;
               }
           in
           let agent = Orchestrator.agent orchestrator root.Patch.id in
           let recovery =
             Scheduler_decision.recovery_intent view ~gameplan_patch:true
               root.Patch.id
           in
           let base = Scheduler_decision.selected_base view root.Patch.id in
           let evidence = Scheduler_decision.evidence_id view root.Patch.id in
           let decisions = Scheduler_decision.plan view in
           let decision = Scheduler_decision.for_patch view root.Patch.id in
           List.length decisions = List.length patches
           && Scheduler_decision.dependencies_admitted view root.Patch.id
           && Option.equal Branch.equal base (Some main)
           && Option.is_some evidence && Option.is_some decision
           && (not (Scheduler_decision.ready_for_review view root.Patch.id))
           && (not (Scheduler_decision.open_dependency_ready agent))
           && (not (Scheduler_decision.draft_allowed view root.Patch.id))
           && (not recovery.Scheduler_decision.pr_base_update)
           && (not recovery.Scheduler_decision.pr_draft_update)
           && not (Scheduler_decision.automerge_candidate agent ~main)
         with _ -> false))
