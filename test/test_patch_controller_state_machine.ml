open Base
open Onton
open Onton.Types

let main = Branch.of_string "main"

let make_patch pid branch =
  Patch.
    {
      id = pid;
      title = "Patch";
      description = "";
      branch;
      dependencies = [];
      spec = "";
      acceptance_criteria = [];
      files = [];
      classification = "";
      changes = [];
      test_stubs_introduced = [];
      test_stubs_implemented = [];
    }

let make_gameplan patch =
  Gameplan.
    {
      project_name = "test-project";
      problem_statement = "";
      solution_summary = "";
      design_decisions = "";
      patches = [ patch ];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }

let messages_equal a b = List.equal Orchestrator.equal_patch_agent_message a b

let normalized_plan orch ~gameplan =
  let orch1, effects1, messages1 =
    Patch_controller.plan_tick_messages orch ~project_name:"test-project"
      ~gameplan
  in
  let orch2, effects2, messages2 =
    Patch_controller.plan_tick_messages orch1 ~project_name:"test-project"
      ~gameplan
  in
  Map.equal Patch_agent.equal
    (Orchestrator.agents_map orch1)
    (Orchestrator.agents_map orch2)
  && List.equal Patch_controller.equal_github_effect effects1 effects2
  && messages_equal messages1 messages2

let accept_all_messages orch messages =
  List.fold messages ~init:orch ~f:(fun acc msg ->
      fst (Orchestrator.accept_message acc (Orchestrator.message_id msg)))

let () =
  let open QCheck2 in
  let open Onton_test_support.Test_generators in
  let prop_replay_deterministic =
    Test.make
      ~name:
        "patch_controller_state_machine: accepted messages replay to the same \
         durable plan"
      ~count:300
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        begin try
          let patch = make_patch pid branch in
          let gameplan = make_gameplan patch in
          let orch = Orchestrator.create ~patches:[ patch ] ~main_branch:main in
          let orch1, _effects1, messages1 =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          let orch1 = accept_all_messages orch1 messages1 in
          normalized_plan orch1 ~gameplan
        with _ -> false
        end)
  in
  let prop_resume_keeps_same_message_id =
    Test.make
      ~name:
        "patch_controller_state_machine: accepted but incomplete message keeps \
         the same logical id across replay"
      ~count:300
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        begin try
          let patch = make_patch pid branch in
          let gameplan = make_gameplan patch in
          let orch = Orchestrator.create ~patches:[ patch ] ~main_branch:main in
          let orch1, _effects1, messages1 =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          match List.hd messages1 with
          | None -> true
          | Some msg ->
              let orch1, _ =
                Orchestrator.accept_message orch1 (Orchestrator.message_id msg)
              in
              let orch1 = Orchestrator.reset_busy orch1 pid in
              let _orch2, _effects2, messages2 =
                Patch_controller.plan_tick_messages orch1
                  ~project_name:"test-project" ~gameplan
              in
              List.exists messages2 ~f:(fun msg2 ->
                  Message_id.equal
                    (Orchestrator.message_id msg)
                    (Orchestrator.message_id msg2))
        with _ -> false
        end)
  in
  QCheck_base_runner.run_tests ~verbose:true
    [ prop_replay_deterministic; prop_resume_keeps_same_message_id ]
  |> Stdlib.exit
