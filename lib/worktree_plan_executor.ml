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

module Make (W : Worktree.S) (Env : Run_env.S) : S = struct
  module WS = Worktree_setup.Make (W) (Env)

  let execute ~patch_id ~(agent : Patch_agent.t) ~fetch_lock ~fail_label
      ~ancestor_ids (plan : Worktree_plan.t) =
    let default_path =
      Worktree.worktree_dir ~project_name:Env.project_name ~patch_id
    in
    let rec loop ~path = function
      | [] -> (Worktree.Ok, path)
      | Worktree_plan.Ensure_worktree :: rest -> (
          match WS.ensure_worktree ~patch_id ~agent () with
          | Some p -> loop ~path:p rest
          | None ->
              Runtime_logging.log_event Env.runtime ~patch_id
                (Printf.sprintf
                   "Cannot %s — worktree missing and could not be created"
                   fail_label);
              ( Worktree.Error
                  (Printf.sprintf "%s failed: worktree missing" fail_label),
                path ))
      | Worktree_plan.Fetch_origin :: rest -> (
          match W.fetch_origin ~fetch_lock ~path with
          | Result.Ok () -> loop ~path rest
          | Result.Error msg ->
              Runtime_logging.log_event Env.runtime ~patch_id
                (Printf.sprintf "Fetch failed before %s — %s" fail_label msg);
              ( Worktree.Error
                  (Printf.sprintf "fetch before rebase failed: %s" msg),
                path ))
      | Worktree_plan.Rebase_onto target :: rest -> (
          match
            W.rebase_onto ~path ~target ~project_name:Env.project_name
              ~ancestor_ids
          with
          | Worktree.Ok -> loop ~path rest
          | (Worktree.Noop | Worktree.Conflict _ | Worktree.Error _) as r ->
              (r, path))
    in
    loop ~path:default_path plan
end
