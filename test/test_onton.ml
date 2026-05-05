let () =
  let patches =
    [
      Onton_core.Types.Patch.
        {
          id = Onton_core.Types.Patch_id.of_string "1";
          title = "Test patch";
          description = "";
          branch = Onton_core.Types.Branch.of_string "test-1";
          dependencies = [];
          spec = "";
          acceptance_criteria = [];
          files = [];
          classification = "";
          changes = [];
          test_stubs_introduced = [];
          test_stubs_implemented = [];
          complexity = None;
        };
    ]
  in
  let main_branch = Onton_core.Types.Branch.of_string "main" in
  let orch = Onton.Orchestrator.create ~patches ~main_branch in
  let gameplan =
    Onton_core.Types.Gameplan.
      {
        project_name = "test-project";
        problem_statement = "";
        solution_summary = "";
        final_state_spec = "";
        patches;
        current_state_analysis = "";
        explicit_opinions = "";
        acceptance_criteria = [];
        open_questions = [];
      }
  in
  let _orch, _effects, actions =
    Onton.Patch_controller.tick orch ~project_name:"test-project" ~gameplan
  in
  (match actions with
  | [ Onton.Orchestrator.Start (pid, base) ] ->
      assert (
        Onton_core.Types.Patch_id.equal pid
          (Onton_core.Types.Patch_id.of_string "1"));
      assert (Onton_core.Types.Branch.equal base main_branch)
  | [] | _ :: _ :: _
  | [ Onton.Orchestrator.Respond _ ]
  | [ Onton.Orchestrator.Rebase _ ] ->
      failwith "expected exactly one Start action for patch \"1\"");
  print_endline "all tests passed"
