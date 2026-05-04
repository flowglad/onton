open Base

let resolve_worktree_path ~process_mgr ~repo_root ~project_name ~patch_id
    ~(agent : Patch_agent.t) ?branch () =
  match agent.Patch_agent.worktree_path with
  | Some p -> p
  | None ->
      let search_branch =
        match branch with Some b -> b | None -> agent.Patch_agent.branch
      in
      let found =
        Worktree.find_for_branch ~process_mgr ~repo_root search_branch
      in
      let path =
        match found with
        | Some p -> p
        | None -> Worktree.worktree_dir ~project_name ~patch_id
      in
      path

let ensure_worktree ~runtime ~process_mgr ~clock ~fs ~repo_root ~project_name
    ~patch_id ~(agent : Patch_agent.t) ~(user_config : User_config.t)
    ~worktree_mutex ~hook_mutex ?branch ?base_ref () =
  let log_event = Runtime_logging.log_event in
  let path =
    resolve_worktree_path ~process_mgr ~repo_root ~project_name ~patch_id ~agent
      ?branch ()
  in
  if Stdlib.Sys.file_exists path then (
    Runtime.update_orchestrator runtime (fun orch ->
        Orchestrator.set_worktree_path orch patch_id path);
    Some path)
  else
    let br =
      match branch with Some b -> b | None -> agent.Patch_agent.branch
    in
    match Worktree.find_for_branch ~process_mgr ~repo_root br with
    | Some existing ->
        log_event runtime ~patch_id
          (Printf.sprintf "Found existing worktree for branch at %s" existing);
        Runtime.update_orchestrator runtime (fun orch ->
            Orchestrator.set_worktree_path orch patch_id existing);
        Some existing
    | None ->
        if Worktree.is_checked_out_in_repo_root ~process_mgr ~repo_root br then (
          let main_root = Worktree.resolve_main_root ~process_mgr ~repo_root in
          log_event runtime ~patch_id
            (Printf.sprintf
               "Cannot create worktree — branch %s is checked out in the main \
                working tree (%s). Switch the main tree to another branch \
                (e.g. `git -C %s checkout <default-branch>`) and try again."
               (Types.Branch.to_string br)
               main_root main_root);
          None)
        else
          let base =
            match base_ref with
            | Some b -> b
            | None -> (
                match agent.Patch_agent.base_branch with
                | Some b -> Types.Branch.to_string b
                | None -> "HEAD")
          in
          log_event runtime ~patch_id
            (Printf.sprintf "Creating worktree at %s" path);
          (match
             Eio.Mutex.use_ro worktree_mutex (fun () ->
                 ignore
                   (Worktree.create ~process_mgr ~repo_root ~project_name
                      ~patch_id ~branch:br ~base_ref:base))
           with
          | () -> ()
          | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
          | exception exn ->
              log_event runtime ~patch_id
                (Printf.sprintf "Worktree creation failed — %s"
                   (Stdlib.Printexc.to_string exn)));
          if Stdlib.Sys.file_exists path then (
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.set_worktree_path orch patch_id path);
            (match user_config.User_config.on_worktree_create with
            | Some script -> (
                let env =
                  [
                    ("ONTON_WORKTREE_PATH", path);
                    ("ONTON_PATCH_ID", Types.Patch_id.to_string patch_id);
                    ("ONTON_BRANCH", Types.Branch.to_string br);
                  ]
                in
                let cwd = Eio.Path.(fs / path) in
                (* [hook_mutex] serializes hook invocations across patch
                   fibers — npm/dune/bun aren't parallelism-safe across
                   shared caches, and per-hook fan-out is the dominant
                   subprocess multiplier (see issue #209). *)
                match
                  Eio.Mutex.use_ro hook_mutex (fun () ->
                      User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env
                        ())
                with
                | Ok () ->
                    log_event runtime ~patch_id "Ran on_worktree_create hook"
                | Error msg ->
                    log_event runtime ~patch_id
                      (Printf.sprintf "Hook on_worktree_create failed — %s" msg)
                )
            | None -> ());
            Some path)
          else (
            log_event runtime ~patch_id
              (Printf.sprintf "Worktree still missing at %s" path);
            None)
