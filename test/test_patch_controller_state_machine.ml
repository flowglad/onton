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
      final_state_spec = "";
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
  let prop_human_after_review_completes =
    Test.make
      ~name:
        "patch_controller_state_machine: human message queued during \
         review_comments is delivered after review completes"
      ~count:300
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        begin try
          let patch = make_patch pid branch in
          let gameplan = make_gameplan patch in
          let orch = Orchestrator.create ~patches:[ patch ] ~main_branch:main in
          (* Step 1: Start → get PR *)
          let orch, _eff, msgs =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          let orch = accept_all_messages orch msgs in
          (* Simulate start completing and PR being discovered *)
          let orch = Orchestrator.complete orch pid in
          let orch = Orchestrator.set_pr_number orch pid (Pr_number.of_int 1) in
          (* Step 2: Enqueue Review_comments via poller *)
          let orch =
            Orchestrator.enqueue orch pid Operation_kind.Review_comments
          in
          (* Step 3: Plan + accept Review_comments *)
          let orch, _eff, msgs =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          let orch = accept_all_messages orch msgs in
          (* Agent should now be busy with Review_comments *)
          let agent = Orchestrator.agent orch pid in
          assert agent.Patch_agent.busy;
          (* Step 4: Human sends a message while agent is busy *)
          let orch = Orchestrator.send_human_message orch pid "fix the typo" in
          (* Step 5: Review_comments session completes *)
          let orch = Orchestrator.complete orch pid in
          (* Step 6: Next tick should plan Human Respond *)
          let _orch2, _eff2, msgs2 =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          List.exists msgs2 ~f:(fun msg ->
              let is_human_respond =
                match Orchestrator.message_action msg with
                | Orchestrator.Respond (p, kind) ->
                    Patch_id.equal p pid
                    && Operation_kind.equal kind Operation_kind.Human
                | Orchestrator.Start _ | Orchestrator.Rebase _ -> false
              in
              is_human_respond)
        with _ -> false
        end)
  in
  (* Comprehensive scenario: a PRIOR Human delivery exists (Completed in outbox),
     then Review_comments runs, human sends another message during it, and
     after review completes the NEW human message should still be planned.
     Tests that message ID collision with the Completed message does not block. *)
  let prop_second_human_after_review =
    Test.make
      ~name:
        "patch_controller_state_machine: second human message after prior \
         human delivery + review_comments"
      ~count:300
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        begin try
          let patch = make_patch pid branch in
          let gameplan = make_gameplan patch in
          let orch = Orchestrator.create ~patches:[ patch ] ~main_branch:main in
          (* Start → get PR *)
          let orch, _eff, msgs =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          let orch = accept_all_messages orch msgs in
          let orch = Orchestrator.complete orch pid in
          let orch = Orchestrator.set_pr_number orch pid (Pr_number.of_int 1) in
          (* First Human message: send, plan, accept, complete *)
          let orch = Orchestrator.send_human_message orch pid "first message" in
          let orch, _eff, msgs =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          let orch = accept_all_messages orch msgs in
          let orch = Orchestrator.complete orch pid in
          (* Now outbox has a Completed Human Respond message *)
          (* Enqueue Review_comments via poller *)
          let orch =
            Orchestrator.enqueue orch pid Operation_kind.Review_comments
          in
          let orch, _eff, msgs =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          let orch = accept_all_messages orch msgs in
          (* Agent busy with Review_comments *)
          (* Send second human message while busy *)
          let orch =
            Orchestrator.send_human_message orch pid "second message"
          in
          (* Complete Review_comments *)
          let orch = Orchestrator.complete orch pid in
          (* Next tick should plan Human Respond for second message *)
          let _orch2, _eff2, msgs2 =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          List.exists msgs2 ~f:(fun msg ->
              let is_human_respond =
                match Orchestrator.message_action msg with
                | Orchestrator.Respond (p, kind) ->
                    Patch_id.equal p pid
                    && Operation_kind.equal kind Operation_kind.Human
                | Orchestrator.Start _ | Orchestrator.Rebase _ -> false
              in
              is_human_respond)
        with _ -> false
        end)
  in
  (* Regression: when a Human session fails, the human_messages must be
     preserved and Human re-enqueued so the retry delivers them. *)
  let prop_human_messages_survive_session_failure =
    Test.make
      ~name:
        "patch_controller_state_machine: human messages survive session \
         failure and are re-delivered"
      ~count:300
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        begin try
          let patch = make_patch pid branch in
          let gameplan = make_gameplan patch in
          let orch = Orchestrator.create ~patches:[ patch ] ~main_branch:main in
          (* Start → get PR *)
          let orch, _eff, msgs =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          let orch = accept_all_messages orch msgs in
          let orch = Orchestrator.complete orch pid in
          let orch = Orchestrator.set_pr_number orch pid (Pr_number.of_int 1) in
          (* Send a human message *)
          let orch = Orchestrator.send_human_message orch pid "fix the typo" in
          (* Plan + accept the Human Respond *)
          let orch, _eff, msgs =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          let orch = accept_all_messages orch msgs in
          let agent = Orchestrator.agent orch pid in
          assert agent.Patch_agent.busy;
          assert (
            Option.equal Operation_kind.equal agent.Patch_agent.current_op
              (Some Operation_kind.Human));
          (* Simulate session failure (e.g. No_session_to_resume) *)
          let orch =
            Orchestrator.apply_session_result orch pid
              Orchestrator.Session_no_resume
          in
          (* After failure: human_messages must still exist and Human
             must be re-enqueued so the retry delivers them *)
          let agent = Orchestrator.agent orch pid in
          let messages_preserved =
            not (List.is_empty agent.Patch_agent.human_messages)
          in
          let human_requeued =
            List.mem agent.Patch_agent.queue Operation_kind.Human
              ~equal:Operation_kind.equal
          in
          (* The retry tick should plan Human Respond again *)
          let _orch2, _eff2, msgs2 =
            Patch_controller.plan_tick_messages orch
              ~project_name:"test-project" ~gameplan
          in
          let replanned =
            List.exists msgs2 ~f:(fun msg ->
                let is_human_respond =
                  match Orchestrator.message_action msg with
                  | Orchestrator.Respond (p, kind) ->
                      Patch_id.equal p pid
                      && Operation_kind.equal kind Operation_kind.Human
                  | Orchestrator.Start _ | Orchestrator.Rebase _ -> false
                in
                is_human_respond)
          in
          messages_preserved && human_requeued && replanned
        with _ -> false
        end)
  in
  QCheck_base_runner.run_tests ~verbose:true
    [
      prop_replay_deterministic;
      prop_resume_keeps_same_message_id;
      prop_human_after_review_completes;
      prop_second_human_after_review;
      prop_human_messages_survive_session_failure;
    ]
  |> Stdlib.exit
