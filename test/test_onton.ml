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
  (match actions with
  | [ Onton.Orchestrator.Start (pid, base) ] ->
      assert (
        Onton.Types.Patch_id.equal pid (Onton.Types.Patch_id.of_string "1"));
      assert (Onton.Types.Branch.equal base main_branch)
  | [ Onton.Orchestrator.Respond _ ] | _ :: _ :: _ | [] ->
      failwith "expected exactly one Start action for patch \"1\"");
  print_endline "all tests passed"
