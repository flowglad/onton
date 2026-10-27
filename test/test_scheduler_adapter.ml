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
