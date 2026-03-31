let () =
  let patches =
    [
      Onton.Types.Patch.
        {
          id = Onton.Types.Patch_id.of_string "1";
          title = "Test patch";
          description = "";
          branch = Onton.Types.Branch.of_string "test-1";
          dependencies = [];
          spec = "";
          acceptance_criteria = [];
          files = [];
          classification = "";
          changes = [];
          test_stubs_introduced = [];
          test_stubs_implemented = [];
        };
    ]
  in
  let main_branch = Onton.Types.Branch.of_string "main" in
  let orch = Onton.Orchestrator.create ~patches ~main_branch in
  let gameplan =
    Onton.Types.Gameplan.
      {
        project_name = "test-project";
        problem_statement = "";
        solution_summary = "";
        design_decisions = "";
        patches;
        current_state_analysis = "";
        explicit_opinions = "";
        acceptance_criteria = [];
      }
  in
  let _orch, _effects, actions =
    Onton.Patch_controller.tick orch ~project_name:"test-project" ~gameplan
  in
  (match actions with
  | [ Onton.Orchestrator.Start (pid, base) ] ->
      assert (
        Onton.Types.Patch_id.equal pid (Onton.Types.Patch_id.of_string "1"));
      assert (Onton.Types.Branch.equal base main_branch)
  | [] | _ :: _ :: _
  | [ Onton.Orchestrator.Respond _ ]
  | [ Onton.Orchestrator.Rebase _ ] ->
      failwith "expected exactly one Start action for patch \"1\"");
  print_endline "all tests passed"
