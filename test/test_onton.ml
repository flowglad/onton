let () =
  let patches =
    [
      Onton.Types.Patch.
        {
          id = Onton.Types.Patch_id.of_string "1";
          title = "Test patch";
          branch = Onton.Types.Branch.of_string "test-1";
          dependencies = [];
        };
    ]
  in
  let main_branch = Onton.Types.Branch.of_string "main" in
  let orch = Onton.Orchestrator.create ~patches ~main_branch in
  let _orch, actions = Onton.Orchestrator.tick orch ~patches in
  assert (List.length actions = 1);
  print_endline "all tests passed"
