open Base

module type ENV = Run_env.S

type ensure_result = Path of string | Missing | Refused

module type S = sig
  val resolve_worktree_path :
    patch_id:Types.Patch_id.t ->
    agent:Patch_agent.t ->
    ?branch:Types.Branch.t ->
    unit ->
    string

  val ensure_worktree :
    patch_id:Types.Patch_id.t ->
    agent:Patch_agent.t ->
    ?branch:Types.Branch.t ->
    ?base_ref:string ->
    unit ->
    ensure_result
end

module Make (W : Worktree.S) (Env : ENV) : S = struct
  let resolve_worktree_path ~patch_id ~(agent : Patch_agent.t) ?branch () =
    (* When the caller passes [?branch], they're asking for that branch's
     worktree specifically — the agent's stored [worktree_path] may be from
     a previous branch and would be stale. Only short-circuit on the stored
     path when no [branch] is supplied. *)
    match (branch, agent.Patch_agent.worktree_path) with
    | None, Some p -> p
    | _ ->
        let search_branch =
          match branch with Some b -> b | None -> agent.Patch_agent.branch
        in
        let found = W.find_for_branch search_branch in
        let path =
          match found with
          | Some p -> p
          | None ->
              Worktree.worktree_dir ~project_name:Env.project_name ~patch_id
        in
        path

  let ensure_worktree ~patch_id ~(agent : Patch_agent.t) ?branch ?base_ref () =
    let runtime = Env.runtime in
    let clock = Env.clock in
    let fs = Env.fs in
    let project_name = Env.project_name in
    let user_config = Env.user_config in
    let worktree_mutex = Env.worktree_mutex in
    let hook_mutex = Env.hook_mutex in
    let log_event = Runtime_logging.log_event in
    let start_point_refusal_rejection refusal =
      let reason =
        Start_point_plan.short_label (Start_point_plan.Refuse refusal)
      in
      Push_reject_classify.Local_state_unsafe { reason }
    in
    let path = resolve_worktree_path ~patch_id ~agent ?branch () in
    if Stdlib.Sys.file_exists path then (
      Runtime.update_orchestrator runtime (fun orch ->
          Orchestrator.set_worktree_path orch patch_id path);
      Path path)
    else
      let br =
        match branch with Some b -> b | None -> agent.Patch_agent.branch
      in
      (* The previous worktree directory is gone — prune git's registry first so
       a stale entry (deleted dir but still listed by [git worktree list])
       does not steer [find_for_branch] back to the same dead path and
       prevent [Worktree.create] from re-registering it. *)
      W.prune_admin ();
      let found = W.find_for_branch br in
      (* Treat a hit whose directory is gone as a miss — defends against races
       (another process re-registering between our prune and our list) or
       git versions that leave half-pruned state. We log the discard so the
       user can see why we are recreating despite git's bookkeeping. *)
      let live_existing =
        match found with
        | Some p when Stdlib.Sys.file_exists p -> Some p
        | Some stale ->
            log_event runtime ~patch_id
              (Printf.sprintf
                 "Ignoring stale worktree registration (git lists %s but \
                  directory is gone) — will recreate"
                 stale);
            None
        | None -> None
      in
      match live_existing with
      | Some existing ->
          log_event runtime ~patch_id
            (Printf.sprintf "Found existing worktree for branch at %s" existing);
          Runtime.update_orchestrator runtime (fun orch ->
              Orchestrator.set_worktree_path orch patch_id existing);
          Path existing
      | None -> (
          if W.is_checked_out_in_repo_root br then (
            let main_root = W.resolve_main_root () in
            let refusal = Start_point_plan.Branch_checked_out_in_main_root in
            log_event runtime ~patch_id
              (Printf.sprintf
                 "Cannot create worktree — branch %s is checked out in the \
                  main working tree (%s). Switch the main tree to another \
                  branch (e.g. `git -C %s checkout <default-branch>`) and try \
                  again."
                 (Types.Branch.to_string br)
                 main_root main_root);
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.apply_session_result orch patch_id
                  (Orchestrator.Session_push_failed
                     (Some (start_point_refusal_rejection refusal))));
            Refused)
          else
            let base =
              match base_ref with
              | Some b -> b
              | None -> (
                  match agent.Patch_agent.base_branch with
                  | Some b -> Types.Branch.to_string b
                  | None -> "HEAD")
            in
            (* Refresh [refs/remotes/origin/<branch>] before we feed it to the
               start-point planner — without this the planner would observe a
               stale local view of remote, which is the failure mode that
               wiped PR #315. The planner correctly handles
               [remote_ref = None] (brand-new branch) so all three result
               variants are non-fatal here; the typed result lets us log the
               routine no-upstream case calmly and reserve the alarming
               "failed" wording for real fetch errors. *)
            let fetch_lock = Env.fetch_mutex in
            (match
               W.fetch_origin_branch ~fetch_lock
                 ~branch:(Types.Branch.to_string br)
             with
            | Fetch_branch_ok -> ()
            | Fetch_branch_no_remote_ref ->
                log_event runtime ~patch_id
                  (Printf.sprintf
                     "No remote ref for %s yet (brand-new branch — skipping \
                      pre-create fetch)"
                     (Types.Branch.to_string br))
            | Fetch_branch_error msg ->
                log_event runtime ~patch_id
                  (Printf.sprintf "Pre-create fetch failed (continuing): %s" msg));
            let main_branch =
              Types.Branch.to_string
                (Runtime.read runtime (fun snap ->
                     Orchestrator.main_branch snap.Runtime.orchestrator))
            in
            (* Also refresh [origin/<main>] so [Worktree.create]'s base-freshness
               probe (defense-in-depth behind the orchestrator's scheduling
               gate) sees the current main tip rather than a stale local view.
               Best-effort — a fetch failure leaves the probe to report
               [Unknown_freshness], which proceeds. *)
            (match W.fetch_origin_branch ~fetch_lock ~branch:main_branch with
            | Fetch_branch_ok | Fetch_branch_no_remote_ref -> ()
            | Fetch_branch_error msg ->
                log_event runtime ~patch_id
                  (Printf.sprintf
                     "Pre-create fetch of origin/%s failed (continuing): %s"
                     main_branch msg));
            log_event runtime ~patch_id
              (Printf.sprintf "Creating worktree at %s" path);
            let create_outcome =
              match
                Eio.Mutex.use_ro worktree_mutex (fun () ->
                    W.create ~project_name ~patch_id ~branch:br ~base_ref:base
                      ~main_branch)
              with
              | Ok _ -> `Created
              | Error refusal -> `Refused refusal
              | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
              | exception exn -> `Raised exn
            in
            let log_decision label =
              log_event runtime ~patch_id
                (Printf.sprintf "Worktree start-point: %s" label)
            in
            let created =
              match create_outcome with
              | `Created ->
                  log_decision "ok";
                  true
              | `Refused refusal ->
                  let label =
                    Start_point_plan.short_label
                      (Start_point_plan.Refuse refusal)
                  in
                  log_decision label;
                  log_event runtime ~patch_id
                    (Printf.sprintf "Worktree creation refused — %s"
                       (Start_point_plan.show_refusal refusal));
                  Runtime.update_orchestrator runtime (fun orch ->
                      Orchestrator.apply_session_result orch patch_id
                        (Orchestrator.Session_push_failed
                           (Some (start_point_refusal_rejection refusal))));
                  false
              | `Raised exn ->
                  log_event runtime ~patch_id
                    (Printf.sprintf "Worktree creation failed — %s"
                       (Stdlib.Printexc.to_string exn));
                  false
            in
            match (created, Stdlib.Sys.file_exists path) with
            | true, true ->
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
                          W.run_hook ~clock ~script ~cwd ~env ())
                    with
                    | Ok () ->
                        log_event runtime ~patch_id
                          "Ran on_worktree_create hook"
                    | Error msg ->
                        log_event runtime ~patch_id
                          (Printf.sprintf "Hook on_worktree_create failed — %s"
                             msg))
                | None -> ());
                Path path
            | true, false ->
                log_event runtime ~patch_id
                  (Printf.sprintf "Worktree still missing at %s" path);
                Missing
            | false, _ -> (
                match create_outcome with
                | `Refused _ -> Refused
                | `Created -> Missing
                | `Raised _ -> (
                    (* [Worktree.create] raised. A concurrent fiber may have already
                 added a real worktree for [br] before our attempt collided.
                 [find_for_branch] queries [git worktree list], which only
                 reports atomically-registered worktrees — so a hit here is a
                 genuine adoptable path, not a partially-created stub. The
                 [on_worktree_create] hook is intentionally skipped on this
                 branch: the winning creator is already responsible for it,
                 mirroring the existing "Found existing worktree for branch"
                 path above. *)
                    match W.find_for_branch br with
                    | Some existing ->
                        log_event runtime ~patch_id
                          (Printf.sprintf
                             "Adopting concurrently-created worktree at %s"
                             existing);
                        Runtime.update_orchestrator runtime (fun orch ->
                            Orchestrator.set_worktree_path orch patch_id
                              existing);
                        Path existing
                    | None -> Missing)))
end
