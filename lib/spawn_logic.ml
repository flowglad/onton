type spawn = Orchestrator.action

let plan_spawns orch ~patches = Orchestrator.pending_actions orch ~patches

let classify = function
  | Orchestrator.Start (pid, _) -> `Start pid
  | Orchestrator.Respond (pid, _) -> `Respond pid
  | Orchestrator.Rebase (pid, _) -> `Rebase pid

let patch_id_of = function
  | Orchestrator.Start (pid, _) -> pid
  | Orchestrator.Respond (pid, _) -> pid
  | Orchestrator.Rebase (pid, _) -> pid
