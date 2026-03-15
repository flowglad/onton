let () =
  let patches =
    [
      Onton.Types.Patch.
        {
          id = Onton.Types.Patch_id.of_int 1;
          title = "First patch";
          branch = Onton.Types.Branch.of_string "patch-1";
          dependencies = [];
        };
    ]
  in
  let main_branch = Onton.Types.Branch.of_string "main" in
  let orch = Onton.Orchestrator.create ~patches ~main_branch in
  let orch, actions = Onton.Orchestrator.tick orch ~patches in
  Printf.printf "tick fired %d actions\n" (List.length actions);
  let agents = Onton.Orchestrator.all_agents orch in
  List.iter
    (fun a ->
      Printf.printf "patch %d: has_pr=%b busy=%b\n"
        (Onton.Types.Patch_id.to_int a.Onton.Patch_agent.patch_id)
        a.Onton.Patch_agent.has_pr a.Onton.Patch_agent.busy)
    agents
