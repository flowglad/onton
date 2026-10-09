(* @archlint.module shell
   @archlint.domain orchestrator *)

open Base

module type ENV = Run_env.S

type ensure_result =
  | Path of string
  | Unavailable of Worktree_provision.failure

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

  val execute_reconciliation :
    patch_id:Types.Patch_id.t ->
    operation:Branch_reconcile.operation ->
    Branch_reconcile.command ->
    Branch_reconcile.result

  val ensure_owned :
    owner:Runtime.patch_write -> ?base_ref:string -> unit -> ensure_result
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

  let is_ready ~path ~branch =
    match W.ensure_ready ~path ~branch with
    | Ok ready -> ready
    | Error msg -> failwith msg

  exception Provenance_unavailable of string
  exception Hook_unavailable of string

  let checkpoint_event ?(guard = fun _ -> None) ~patch_id event =
    let checkpoint = Project_store.snapshot_path Env.project_name in
    Project_store.ensure_dir (Stdlib.Filename.dirname checkpoint);
    match
      Runtime.update_persisting Env.runtime
        ~persist:(Persistence.save_snapshot ~path:checkpoint) (fun snap ->
          match
            guard (Orchestrator.agent snap.Runtime.orchestrator patch_id)
          with
          | Some reason -> (snap, Error reason)
          | None ->
              let orchestrator, _ =
                Orchestrator.reconcile_branch snap.Runtime.orchestrator patch_id
                  event
              in
              ({ snap with Runtime.orchestrator }, Ok ()))
    with
    | Ok (Ok ()) -> ()
    | Ok (Error reason) -> raise (Hook_unavailable reason)
    | Error message -> raise (Provenance_unavailable message)

  let owner_state ~patch_id =
    Runtime.read Env.runtime (fun snap ->
        (Orchestrator.agent snap.Runtime.orchestrator patch_id)
          .Patch_agent.branch_reconcile)

  let checkpoint_provenance ~patch_id ~path ~branch =
    if Option.is_none (Branch_reconcile.materialization (owner_state ~patch_id))
    then
      match W.materialization ~path ~project_name:Env.project_name ~branch with
      | Error message -> raise (Provenance_unavailable message)
      | Ok None -> ()
      | Ok (Some receipt) ->
          checkpoint_event ~patch_id (Branch_reconcile.Materialized receipt)

  let hook_decision ~patch_id ~path ~branch =
    Worktree_hook.next
      (Branch_reconcile.worktree_hook (owner_state ~patch_id))
      ~path:(Worktree_backend.canonical path)
      ~branch:(Types.Branch.to_string branch)

  let checkpoint_hook ?guard ~patch_id event =
    checkpoint_event ?guard ~patch_id (Branch_reconcile.Worktree_hook event)

  let ensure_hook ~patch_id ~path ~branch =
    match hook_decision ~patch_id ~path ~branch with
    | Worktree_hook.Ready -> ()
    | Worktree_hook.Stop reason -> raise (Hook_unavailable reason)
    | Worktree_hook.Run run -> (
        let env =
          [
            ("ONTON_WORKTREE_PATH", path);
            ("ONTON_PATCH_ID", Types.Patch_id.to_string patch_id);
            ("ONTON_BRANCH", Types.Branch.to_string branch);
          ]
        in
        let result =
          Eio.Mutex.use_ro Env.hook_mutex (fun () ->
              checkpoint_hook ~guard:Patch_agent.reconciliation_hold_reason
                ~patch_id (Worktree_hook.Started run);
              W.run_hook ~clock:Env.clock ~script:run.request.script
                ~cwd:Eio.Path.(Env.fs / path)
                ~env ())
        in
        let outcome =
          match result with
          | Ok () -> Worktree_hook.Succeeded
          | Error reason -> Worktree_hook.Failed reason
        in
        checkpoint_hook ~patch_id
          (Worktree_hook.Completed
             { id = run.request.id; attempt = run.attempt; outcome });
        Runtime_logging.log_event Env.runtime ~patch_id
          (match result with
          | Ok () -> "Ran on_worktree_create hook"
          | Error reason -> "Hook on_worktree_create failed — " ^ reason);
        (* Hooks can change Git or move the checkout. Their durable completion
           acknowledges execution, not the continued validity of the path. *)
        match W.inspect_existing ~path ~branch with
        | Ok true -> ()
        | Ok false ->
            raise (Hook_unavailable "worktree_create_hook_checkout_missing")
        | Error reason -> raise (Provenance_unavailable reason))

  let plan_hook ~patch_id ~path ~branch =
    match hook_decision ~patch_id ~path ~branch with
    | Worktree_hook.Stop reason -> raise (Hook_unavailable reason)
    | Worktree_hook.Run _ -> ()
    | Worktree_hook.Ready ->
        Option.iter Env.user_config.User_config.on_worktree_create
          ~f:(fun script ->
            checkpoint_hook ~guard:Patch_agent.reconciliation_hold_reason
              ~patch_id
              (Worktree_hook.Plan
                 {
                   id = Session_id.mint ();
                   path = Worktree_backend.canonical path;
                   branch = Types.Branch.to_string branch;
                   script;
                 });
            match hook_decision ~patch_id ~path ~branch with
            | Worktree_hook.Run _ -> ()
            | Worktree_hook.Stop reason -> raise (Hook_unavailable reason)
            | Worktree_hook.Ready ->
                raise
                  (Hook_unavailable "worktree_create_hook_invalid_configuration"))

  let ensure_worktree_impl ~patch_id ~(agent : Patch_agent.t) ?branch ?base_ref
      () =
    let runtime = Env.runtime in
    let project_name = Env.project_name in
    let worktree_mutex = Env.worktree_mutex in
    let log_event = Runtime_logging.log_event in
    let path = resolve_worktree_path ~patch_id ~agent ?branch () in
    let br = Option.value branch ~default:agent.Patch_agent.branch in
    (match hook_decision ~patch_id ~path ~branch:br with
    | Worktree_hook.Stop reason -> raise (Hook_unavailable reason)
    | Worktree_hook.Ready | Worktree_hook.Run _ -> ());
    if is_ready ~path ~branch:br then (
      checkpoint_provenance ~patch_id ~path ~branch:br;
      ensure_hook ~patch_id ~path ~branch:br;
      Runtime.update_orchestrator runtime (fun orch ->
          Orchestrator.set_worktree_path orch patch_id path);
      Path path)
    else
      let br =
        match branch with Some b -> b | None -> agent.Patch_agent.branch
      in
      (* Only prune a stale registration for the branch this patch needs.
       Full repository reconciliation is maintenance work and an unrelated
       ownership defect must not block this patch from starting. *)
      W.prune_stale_for_branch br;
      let found = W.find_for_branch br in
      (* Treat a hit whose directory is gone as a miss — defends against races
       (another process re-registering between our prune and our list) or
       git versions that leave half-pruned state. We log the discard so the
       user can see why we are recreating despite git's bookkeeping. *)
      let live_existing =
        match found with
        | Some p when is_ready ~path:p ~branch:br -> Some p
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
          checkpoint_provenance ~patch_id ~path:existing ~branch:br;
          ensure_hook ~patch_id ~path:existing ~branch:br;
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
            Unavailable (Worktree_provision.creation_failure refusal))
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
            (* When the base is the main branch, also refresh
               [refs/remotes/origin/<main>] and cut from its resolved SHA. The
               orchestrator never advances the local main ref, so cutting from
               it right after a dependency's squash-merge starts the branch on
               yesterday's main, missing that dependency's commits — the
               connector-adapter-shape-unification patch-4 stale cut that
               forced an immediate freshen rebase (which then conflicted). For
               a dependency-branch base the local ref is authoritative (its
               worktree writes locally first; origin lags until push), so no
               fetch is attempted: {!Start_point_plan.base_start_point} encodes
               the asymmetry, and on fetch/resolve failure it falls back
               fail-open to the local ref with the freshen-rebase detectors as
               the backstop, exactly the pre-fetch behavior.

               This deliberately shares [Env.fetch_mutex] with the branch fetch
               above (and the rest of the runtime's fetch traffic). Git updates
               refs in the same managed clone, so serializing both fetches keeps
               ref observation ordered at the cost of one more locked network
               round trip during main-base worktree creation. *)
            let base_is_main =
              Runtime.read runtime (fun snap ->
                  Types.Branch.equal
                    (Orchestrator.main_branch snap.Runtime.orchestrator)
                    (Types.Branch.of_string base))
            in
            let rec fetch_base attempt =
              match W.fetch_origin_branch ~fetch_lock ~branch:base with
              | Fetch_branch_ok as result -> result
              | Fetch_branch_no_remote_ref as result -> result
              | Fetch_branch_error _ as result ->
                  if attempt >= 3 then result
                  else (
                    Eio.Fiber.yield ();
                    fetch_base (attempt + 1))
            in
            let fetched_remote_sha =
              if not base_is_main then None
              else
                match fetch_base 1 with
                | Fetch_branch_ok ->
                    W.read_branch_sha ~path:(W.resolve_main_root ())
                      ~ref_name:("refs/remotes/origin/" ^ base)
                | Fetch_branch_no_remote_ref ->
                    log_event runtime ~patch_id
                      (Printf.sprintf
                         "No remote ref for base %s — cutting from the local \
                          ref"
                         base);
                    None
                | Fetch_branch_error msg ->
                    log_event runtime ~patch_id
                      (Printf.sprintf
                         "Base fetch failed (continuing with possibly-stale \
                          local %s): %s"
                         base msg);
                    None
            in
            let start_point =
              Start_point_plan.base_start_point ~base_branch:base ~base_is_main
                ~fetched_remote_sha
            in
            log_event runtime ~patch_id
              (Printf.sprintf "Worktree base start point: %s (%s)"
                 (Start_point_plan.base_start_point_short_label start_point)
                 (Start_point_plan.base_start_point_ref start_point));
            log_event runtime ~patch_id
              (Printf.sprintf "Creating worktree at %s" path);
            plan_hook ~patch_id ~path ~branch:br;
            let create_outcome =
              match
                Eio.Mutex.use_ro worktree_mutex (fun () ->
                    W.create ~project_name ~patch_id ~branch:br
                      ~base_ref:
                        (Start_point_plan.base_start_point_ref start_point))
              with
              | Ok checkout -> `Created checkout
              | Error refusal -> `Refused refusal
              | exception exn when Worktree.has_cancellation exn -> raise exn
              | exception exn -> `Raised exn
            in
            let path =
              match create_outcome with
              | `Created checkout -> Worktree.path checkout
              | `Refused _ | `Raised _ -> path
            in
            let log_decision label =
              log_event runtime ~patch_id
                (Printf.sprintf "Worktree start-point: %s" label)
            in
            let created =
              match create_outcome with
              | `Created _ ->
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
                  false
              | `Raised exn ->
                  log_event runtime ~patch_id
                    (Printf.sprintf "Worktree creation failed — %s"
                       (Stdlib.Printexc.to_string exn));
                  false
            in
            (* A successful create carries a validated, published checkout.
               Re-inspection here would turn a transient recovery failure into
               a refusal after provisioning has already succeeded. *)
            match created with
            | true ->
                checkpoint_provenance ~patch_id ~path ~branch:br;
                Runtime.update_orchestrator runtime (fun orch ->
                    Orchestrator.set_worktree_path orch patch_id path);
                ensure_hook ~patch_id ~path ~branch:br;
                Path path
            | false -> (
                match create_outcome with
                | `Refused refusal ->
                    Unavailable (Worktree_provision.creation_failure refusal)
                | `Created _ ->
                    Unavailable
                      (Worktree_provision.Temporary
                         "checkout_missing_after_creation")
                | `Raised error -> (
                    (* Creation may have completed before its acknowledgement
                       was lost. Adopt the owned checkout and complete its durable
                       hook obligation before reporting readiness. *)
                    match W.find_for_branch br with
                    | Some existing when is_ready ~path:existing ~branch:br ->
                        checkpoint_provenance ~patch_id ~path:existing
                          ~branch:br;
                        ensure_hook ~patch_id ~path:existing ~branch:br;
                        log_event runtime ~patch_id
                          (Printf.sprintf
                             "Adopting concurrently-created worktree at %s"
                             existing);
                        Runtime.update_orchestrator runtime (fun orch ->
                            Orchestrator.set_worktree_path orch patch_id
                              existing);
                        Path existing
                    | Some _ | None ->
                        Unavailable
                          (Worktree_provision.Temporary (Exn.to_string error))))
          )

  let ensure_worktree ~patch_id ~agent ?branch ?base_ref () =
    try ensure_worktree_impl ~patch_id ~agent ?branch ?base_ref () with
    | exn when Worktree.has_cancellation exn -> raise exn
    | Hook_unavailable reason -> Unavailable (Worktree_provision.Unsafe reason)
    | Provenance_unavailable reason ->
        Runtime_logging.log_event Env.runtime ~patch_id
          ("Materialization checkpoint unavailable — " ^ reason);
        Unavailable (Worktree_provision.materialization_failure reason)
    | exn ->
        let reason = Exn.to_string exn in
        Runtime_logging.log_event Env.runtime ~patch_id
          ("Worktree unavailable — " ^ reason);
        Unavailable (Worktree_provision.Temporary reason)

  let execute_reconciliation ~patch_id ~(operation : Branch_reconcile.operation)
      command =
    if
      not
        (Option.value_map operation.pending ~default:false
           ~f:(Branch_reconcile.equal_command command))
    then
      Branch_reconcile.Retryable
        { reason = "provisioning_command_not_current"; retry_after = None }
    else
      let agent =
        Runtime.read Env.runtime (fun snap ->
            Orchestrator.agent snap.Runtime.orchestrator patch_id)
      in
      let checkout =
        if
          Branch_reconcile.equal_purpose operation.intent.purpose
            Branch_reconcile.Verify_publication
        then
          try
            let path =
              match W.find_for_branch agent.branch with
              | Some path -> path
              | None -> resolve_worktree_path ~patch_id ~agent ()
            in
            match W.inspect_existing ~path ~branch:agent.branch with
            | Ok true ->
                Runtime.update_orchestrator Env.runtime (fun orch ->
                    Orchestrator.set_worktree_path orch patch_id path);
                Path path
            | Ok false ->
                Unavailable
                  (Worktree_provision.Unsafe
                     "legacy_publication_checkout_missing")
            | Error reason ->
                Unavailable (Worktree_provision.materialization_failure reason)
          with
          | exn when Worktree.has_cancellation exn -> raise exn
          | exn ->
              Unavailable (Worktree_provision.Temporary (Exn.to_string exn))
        else if Option.is_none operation.source then
          ensure_worktree ~patch_id ~agent ~base_ref:operation.intent.base ()
        else Path (resolve_worktree_path ~patch_id ~agent ())
      in
      match checkout with
      | Unavailable failure -> Worktree_provision.reconciliation_result failure
      | Path _ when Branch_reconcile.is_provisioning operation.intent.purpose ->
          Branch_reconcile.Checkout_ready
      | Path path ->
          let operation =
            Runtime.read Env.runtime (fun snap ->
                Option.value
                  (Branch_reconcile.operation
                     (Orchestrator.agent snap.Runtime.orchestrator patch_id)
                       .Patch_agent.branch_reconcile)
                  ~default:operation)
          in
          W.reconcile ~path ~project_name:Env.project_name ~branch:agent.branch
            ~operation command

  let ensure_owned ~owner ?base_ref () =
    Runtime.with_owned_patch owner (fun runtime patch_id ->
        if not (phys_equal runtime Env.runtime) then
          invalid_arg "checkout ownership belongs to another runtime";
        (* Each invocation is a new inspection, including after restart. Retries
         of a captured request keep its durable identity and obey owner backoff. *)
        let request = "checkout:" ^ Session_id.mint () in
        let intent =
          Runtime.read runtime (fun snap ->
              let orch = snap.Runtime.orchestrator in
              let agent = Orchestrator.agent orch patch_id in
              Branch_reconcile.
                {
                  base =
                    Option.value base_ref
                      ~default:
                        (Types.Branch.to_string
                           (Option.value agent.Patch_agent.base_branch
                              ~default:(Orchestrator.main_branch orch)));
                  policy =
                    (if Orchestrator.is_integration_root orch patch_id then
                       Preserve_ancestry
                     else Rewrite);
                  purpose = Provision_checkout request;
                })
        in
        let checkpoint = Project_store.snapshot_path Env.project_name in
        let persist snapshot =
          try
            Project_store.ensure_dir (Stdlib.Filename.dirname checkpoint);
            Persistence.save_snapshot ~path:checkpoint snapshot
          with exn ->
            if Worktree.has_cancellation exn then raise exn
            else Error (Exn.to_string exn)
        in
        let now () = Eio.Time.now Env.clock in
        let rec advance () =
          let agent =
            Runtime.read runtime (fun snap ->
                Orchestrator.agent snap.Runtime.orchestrator patch_id)
          in
          match
            Worktree_provision.next ~intent ~at:(now ())
              agent.Patch_agent.branch_reconcile
          with
          | Worktree_provision.Ready -> (
              match agent.worktree_path with
              | Some path -> Path path
              | None ->
                  Unavailable
                    (Worktree_provision.Temporary
                       "provisioned_checkout_path_missing"))
          | Worktree_provision.Wait reason ->
              Unavailable (Worktree_provision.Temporary reason)
          | Worktree_provision.Stop reason ->
              Unavailable (Worktree_provision.Unsafe reason)
          | Worktree_provision.Run event -> (
              match
                Branch_reconcile_runner.run_owned ~owner ~persist ~now
                  ~execute:(execute_reconciliation ~patch_id)
                  event
              with
              | Branch_reconcile_runner.Idle -> advance ()
              | Branch_reconcile_runner.Waiting
              | Branch_reconcile_runner.Repair_needed _ ->
                  Unavailable
                    (Worktree_provision.Temporary
                       "branch_reconciliation_pending")
              | Branch_reconcile_runner.Intervention reason ->
                  Unavailable (Worktree_provision.Unsafe reason)
              | Branch_reconcile_runner.Checkpoint_failed reason ->
                  Unavailable (Worktree_provision.Temporary reason))
        in
        advance ())
end
