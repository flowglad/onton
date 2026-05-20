module type S = sig
  val execute :
    patch_id:Types.Patch_id.t ->
    agent:Patch_agent.t ->
    fetch_lock:Eio.Mutex.t ->
    fail_label:string ->
    ancestor_ids:Types.Patch_id.t list ->
    Worktree_plan.t ->
    Worktree.rebase_result * string
end

module Make (_ : Worktree.S) (_ : Run_env.S) : S
