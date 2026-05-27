open Base
open Types

module Poller_env = struct
  module type S = sig
    include Run_env.S

    val process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t
    val github_owner : string
    val github_repo : string
    val main_branch : Branch.t
    val poll_interval : float
    val repo_root : string
    val find_pr_number : patch_id:Patch_id.t -> Pr_number.t option

    val register_pr_number :
      patch_id:Patch_id.t -> pr_number:Pr_number.t -> unit

    val unregister_pr_number : patch_id:Patch_id.t -> unit
    val findings_registry : Findings_registry.t

    val review_clients :
      (module Review_service_client.S
         with type error = Review_service_client.error)
      list

    val event_log : Event_log.t
    val branch_of : Patch_id.t -> Branch.t
  end
end

module type STARTUP_RECONCILER = sig
  val discover_pr :
    branch:Branch.t -> ((Pr_number.t * Branch.t * bool) option, string) Result.t
end

module Make
    (Forge : Forge.S with type error = Github.error)
    (W : Worktree.S)
    (Env : Poller_env.S) =
struct
  module WS_Env : Worktree_setup.ENV = struct
    let runtime = Env.runtime
    let clock = Env.clock
    let fs = Env.fs
    let project_name = Env.project_name
    let user_config = Env.user_config
    let worktree_mutex = Env.worktree_mutex
    let hook_mutex = Env.hook_mutex
    let fetch_mutex = Env.fetch_mutex
  end

  module WS = Worktree_setup.Make (W) (WS_Env)

  (** Per-agent poll intent, collected inside [read] and executed outside. *)
  type poll_intent =
    | Skip_no_pr of Patch_id.t
    | Poll of {
        patch_id : Patch_id.t;
        pr_number : Pr_number.t;
        was_merged : bool;
      }

  let poll_review_backends ~runtime ~patch_id ~findings_registry ~review_clients
      ~owner ~repo ~pr_number : Review_service.finding list =
    List.concat_map review_clients
      ~f:(fun
          (module R : Review_service_client.S
            with type error = Review_service_client.error)
        ->
        match R.list_findings ~owner ~repo ~pr_number () with
        | Error err ->
            Runtime_logging.log_event runtime ~patch_id
              (Printf.sprintf "Poll error — %s" (R.show_error err));
            []
        | Ok response ->
            let parsed_count = List.length response.Review_service.findings in
            if not (Int.equal parsed_count response.Review_service.count) then
              Runtime_logging.log_event runtime ~patch_id
                (Printf.sprintf
                   "Review backend %s declared %d finding(s) but parsed %d — \
                    possible review-service schema drift"
                   R.name response.Review_service.count parsed_count);
            List.iter response.Review_service.dropped_findings
              ~f:(fun (e : Review_service.finding_parse_error) ->
                let json =
                  if String.length e.Review_service.json <= 500 then
                    e.Review_service.json
                  else String.sub e.Review_service.json ~pos:0 ~len:497 ^ "..."
                in
                Runtime_logging.log_event runtime ~patch_id
                  (Printf.sprintf
                     "Review backend %s dropped finding at index %d while \
                      parsing: %s; json=%s"
                     R.name e.Review_service.index e.Review_service.error json));
            let keyed_findings =
              List.map response.Review_service.findings
                ~f:(fun (f : Review_service.finding) ->
                  let key =
                    Findings_registry.make_key ~backend_name:R.name ~owner ~repo
                      ~pr_number ~finding_id:f.Review_service.id
                  in
                  (key, f))
            in
            Findings_registry.remove_stale_for_scope findings_registry
              ~backend_name:R.name ~owner ~repo ~pr_number
              ~keep_keys:(List.map keyed_findings ~f:fst);
            List.map keyed_findings ~f:(fun (key, f) ->
                Findings_registry.register findings_registry ~key
                  {
                    Findings_registry.backend_name = R.name;
                    owner;
                    repo;
                    pr_number;
                    finding_id = f.Review_service.id;
                  };
                { f with Review_service.id = key }))

  (** Translate a [Github] result into a [Poll_outcome.t]. Pure-modulo-show; the
      boundary lets [Poll_cycle.classify] reason about timeouts the same way it
      reasons about other failures, which is the invariant the interleaving
      property tests exercise. *)
  let poll_outcome_of_github_result = function
    | Ok pr_state -> Poll_outcome.Ok_pr_state pr_state
    | Error (Github.Timeout { seconds; _ }) ->
        Poll_outcome.Timed_out { seconds }
    | Error (Github.Transport_error { msg; _ }) ->
        Poll_outcome.Transport_failed { msg }
    | Error (Github.Http_error { status; body; _ }) ->
        Poll_outcome.Http_failed
          { status; msg = Github.extract_github_message body }
    | Error (Github.Graphql_error msgs) -> Poll_outcome.Graphql_failed msgs
    | Error (Github.Json_parse_error msg) -> Poll_outcome.Json_parse_failed msg

  let run (module StartupReconciler : STARTUP_RECONCILER) =
    let runtime = Env.runtime in
    let clock = Env.clock in
    let process_mgr = Env.process_mgr in
    let project_name = Env.project_name in
    let github_owner = Env.github_owner in
    let github_repo = Env.github_repo in
    let main = Env.main_branch in
    let poll_interval = Env.poll_interval in
    let repo_root = Env.repo_root in
    let find_pr_number = Env.find_pr_number in
    let register_pr_number = Env.register_pr_number in
    let unregister_pr_number = Env.unregister_pr_number in
    let branch_of = Env.branch_of in
    let event_log = Env.event_log in
    let review_clients = Env.review_clients in
    let findings_registry = Env.findings_registry in
    let skip_logged : (Patch_id.t, bool) Stdlib.Hashtbl.t =
      Stdlib.Hashtbl.create 16
    in
    let skip_logged_mutex = Eio.Mutex.create () in
    let mark_skip_logged patch_id =
      Eio.Mutex.use_rw ~protect:true skip_logged_mutex (fun () ->
          if Stdlib.Hashtbl.mem skip_logged patch_id then false
          else (
            Stdlib.Hashtbl.replace skip_logged patch_id true;
            true))
    in
    (* Per-patch "PR vanished" log dedup. Mirrors [skip_logged] above: the
       poll loop re-classifies Mark_pr_missing on every cycle while the PR
       remains absent (Orchestrator.mark_pr_missing is now idempotent), so
       without dedup the activity log fills with duplicates. Reset on a
       successful [Switch_to_pr] so a future vanish-reopen-vanish cycle
       re-logs. *)
    let vanish_logged : (Patch_id.t, bool) Stdlib.Hashtbl.t =
      Stdlib.Hashtbl.create 16
    in
    let vanish_logged_mutex = Eio.Mutex.create () in
    let vanish_already_logged patch_id =
      Eio.Mutex.use_ro vanish_logged_mutex (fun () ->
          Stdlib.Hashtbl.mem vanish_logged patch_id)
    in
    let mark_vanish_logged patch_id =
      Eio.Mutex.use_rw ~protect:true vanish_logged_mutex (fun () ->
          Stdlib.Hashtbl.replace vanish_logged patch_id true)
    in
    let clear_vanish_logged patch_id =
      Eio.Mutex.use_rw ~protect:true vanish_logged_mutex (fun () ->
          Stdlib.Hashtbl.remove vanish_logged patch_id)
    in
    let rec loop () =
      let intents =
        Runtime.read runtime (fun snap ->
            let agents = Orchestrator.all_agents snap.Runtime.orchestrator in
            List.filter_map agents ~f:(fun (agent : Patch_agent.t) ->
                (* Poll open PRs. Also keep polling a *merged* agent whose
                   merge-commit SHA has not yet been recorded, so dependents'
                   base-containment gate can eventually resolve (the SHA almost
                   always arrives in the same poll as the merge, but this
                   self-heals the rare lag / a pre-existing merged snapshot). *)
                let needs_merge_sha =
                  agent.Patch_agent.merged
                  && Option.is_none agent.Patch_agent.merge_commit_sha
                in
                if
                  Patch_agent.has_pr agent
                  && ((not agent.Patch_agent.merged) || needs_merge_sha)
                then
                  match find_pr_number ~patch_id:agent.Patch_agent.patch_id with
                  | None -> Some (Skip_no_pr agent.Patch_agent.patch_id)
                  | Some pr_number ->
                      Some
                        (Poll
                           {
                             patch_id = agent.Patch_agent.patch_id;
                             pr_number;
                             was_merged = agent.Patch_agent.merged;
                           })
                else None))
      in
      (* Handle Skip_no_pr intents up front — they need no network I/O. *)
      let poll_intents =
        List.filter_map intents ~f:(function
          | Skip_no_pr patch_id ->
              if mark_skip_logged patch_id then
                Runtime_logging.log_event runtime ~patch_id
                  "Skipping poll — no PR number registered";
              None
          | Poll { patch_id; pr_number; was_merged } ->
              Some (patch_id, pr_number, was_merged))
      in
      (* Phase 1a: parallel per-patch PR state fetch.

       This is the head-of-line-blocking fix: when one patch's TCP connect
       is wedged in [SYN_SENT], the forge returns [Timeout] after the default
       GitHub request timeout — and every other patch's fiber proceeds
       independently. *)
      let outcomes =
        Eio.Fiber.List.map ~max_fibers:16
          (fun (patch_id, pr_number, was_merged) ->
            let outcome =
              poll_outcome_of_github_result (Forge.pr_state pr_number)
            in
            (patch_id, pr_number, was_merged, outcome))
          poll_intents
      in
      (* Phase 1b: pure classification. *)
      let plans =
        List.map outcomes ~f:(fun (patch_id, pr_number, was_merged, outcome) ->
            let cls =
              Poll_cycle.classify { patch_id; pr_number; was_merged; outcome }
            in
            (patch_id, pr_number, cls))
      in
      (* Phase 1c: per-patch effectful tail. Builds observations from
       [Apply_pr_state] plans, handles [Rediscover_pr] / [Skip_fork] /
       [Log_error]. *)
      let observations =
        List.filter_map plans ~f:(fun (patch_id, pr_number, cls) ->
            match cls with
            | Poll_cycle.Log_error { message } ->
                Runtime_logging.log_event runtime ~patch_id message;
                None
            | Poll_cycle.Skip_fork _ ->
                if mark_skip_logged patch_id then
                  Runtime_logging.log_event runtime ~patch_id
                    (Printf.sprintf
                       "Skipping PR #%d — fork PRs are not supported"
                       (Pr_number.to_int pr_number));
                None
            | Poll_cycle.Rediscover_pr { head_branch } ->
                Runtime_logging.log_event runtime ~patch_id
                  (Printf.sprintf "PR #%d closed — looking for replacement"
                     (Pr_number.to_int pr_number));
                let branch =
                  match head_branch with
                  | Some b -> b
                  | None ->
                      Runtime.read runtime (fun snap ->
                          match
                            Orchestrator.find_agent snap.Runtime.orchestrator
                              patch_id
                          with
                          | Some agent -> agent.Patch_agent.branch
                          | None -> branch_of patch_id)
                in
                let in_gameplan =
                  Runtime.read runtime (fun snap ->
                      List.exists snap.Runtime.gameplan.Gameplan.patches
                        ~f:(fun (p : Patch.t) ->
                          Patch_id.equal p.Patch.id patch_id))
                in
                let discover_result :
                    (Rediscover_decision.replacement option, string) Result.t =
                  match StartupReconciler.discover_pr ~branch with
                  | Ok (Some (new_pr, base_branch, merged)) ->
                      Ok
                        (Some
                           Rediscover_decision.{ new_pr; base_branch; merged })
                  | Ok None -> Ok None
                  | Error msg -> Error msg
                in
                let cls =
                  Rediscover_decision.classify
                    {
                      patch_id;
                      pr_number;
                      in_gameplan;
                      result = discover_result;
                    }
                in
                (match cls with
                | Rediscover_decision.Switch_to_pr
                    { new_pr; base_branch; merged } ->
                    Runtime_logging.log_event runtime ~patch_id
                      (Printf.sprintf "Switched to PR #%d"
                         (Pr_number.to_int new_pr));
                    register_pr_number ~patch_id ~pr_number:new_pr;
                    (* Recovery transition: clear the vanish-log dedup so a
                       future vanish/reopen/vanish cycle re-logs once. *)
                    clear_vanish_logged patch_id;
                    Runtime.update_orchestrator runtime (fun orch ->
                        Patch_controller.apply_replacement_pr orch patch_id
                          ~pr_number:new_pr ~base_branch ~merged)
                | Rediscover_decision.Clear_pr_for_recreate ->
                    Runtime_logging.log_event runtime ~patch_id
                      "No open PR found — cleared stale PR state, will create \
                       on next session";
                    unregister_pr_number ~patch_id;
                    Runtime.update_orchestrator runtime (fun orch ->
                        Orchestrator.clear_pr orch patch_id)
                | Rediscover_decision.Mark_pr_missing ->
                    (* Ad-hoc PR vanished from remote. Surface for
                       intervention; keep the external pr_number registration
                       so a re-opened PR is discovered on the next poll. The
                       operator's [-N] clears the registration via
                       [apply_remove_pr] when they intend to remove.

                       Dedup the "vanished" log line via the pure
                       [Rediscover_decision.classify_vanish_log] decision so
                       the activity log stays clean while the agent remains
                       Missing across many poll cycles. *)
                    (match
                       Rediscover_decision.classify_vanish_log cls
                         ~already_logged:(vanish_already_logged patch_id)
                     with
                    | Rediscover_decision.Log_emit ->
                        Runtime_logging.log_event runtime ~patch_id
                          (Printf.sprintf
                             "PR #%d vanished from remote — marking agent for \
                              intervention; use -%d to remove if intentional"
                             (Pr_number.to_int pr_number)
                             (Pr_number.to_int pr_number));
                        mark_vanish_logged patch_id
                    | Rediscover_decision.Log_skip -> ());
                    Runtime.update_orchestrator runtime (fun orch ->
                        Orchestrator.mark_pr_missing orch patch_id)
                | Rediscover_decision.Log_error { message } ->
                    Runtime_logging.log_event runtime ~patch_id
                      (Printf.sprintf "PR re-discovery failed — %s" message));
                None
            | Poll_cycle.Apply_pr_state
                { pr_state; poll_result; ci_checks_truncated } ->
                (* Augment with findings from every configured review backend.
                 Failures inside [poll_review_backends] are logged but
                 non-fatal. *)
                let findings =
                  if List.is_empty review_clients then []
                  else
                    poll_review_backends ~runtime ~patch_id ~findings_registry
                      ~review_clients ~owner:github_owner ~repo:github_repo
                      ~pr_number:(Pr_number.to_int pr_number)
                in
                let pr_state = { pr_state with Pr_state.findings } in
                let branch_in_root =
                  match pr_state.Pr_state.head_branch with
                  | Some b ->
                      Worktree.is_checked_out_in_repo_root ~process_mgr
                        ~repo_root b
                  | None -> false
                in
                let failed_ci =
                  List.filter poll_result.Poller.ci_checks
                    ~f:(fun (c : Ci_check.t) ->
                      List.mem Patch_decision.failure_conclusions
                        c.Ci_check.conclusion ~equal:String.equal)
                in
                let worktree_candidate =
                  let agent =
                    Runtime.read runtime (fun snap ->
                        Orchestrator.find_agent snap.Runtime.orchestrator
                          patch_id)
                  in
                  match agent with
                  | None -> None
                  | Some agent ->
                      if Option.is_some agent.Patch_agent.worktree_path then
                        None
                      else
                        let path =
                          WS.resolve_worktree_path ~patch_id ~agent ()
                        in
                        let default =
                          Worktree.worktree_dir ~project_name ~patch_id
                        in
                        if not (String.equal path default) then Some path
                        else None
                in
                let observation =
                  Patch_controller.
                    {
                      poll_result;
                      base_branch = pr_state.Pr_state.base_branch;
                      branch_in_root;
                      worktree_path = worktree_candidate;
                    }
                in
                Some (patch_id, observation, failed_ci, ci_checks_truncated))
      in
      (* Phase 2: Single atomic update — apply all poll results + reconcile.
       This prevents the runner from seeing an intermediate state where
       poll results are applied but the reconciler hasn't run yet. *)
      let per_patch_sides, reconcile_logs =
        Runtime.update_orchestrator_returning runtime (fun orch ->
            (* Apply all poll results *)
            let orch, sides =
              List.fold observations ~init:(orch, [])
                ~f:(fun
                    (orch, sides) (patch_id, obs, failed_ci, ci_truncated) ->
                  match Orchestrator.find_agent orch patch_id with
                  | None -> (orch, sides)
                  | Some agent_before ->
                      let orch, log_entries, newly_blocked =
                        Patch_controller.apply_poll_result orch patch_id obs
                      in
                      let agent_after = Orchestrator.agent orch patch_id in
                      Event_log.log_poll event_log ~patch_id
                        ~poll_result:obs.Patch_controller.poll_result
                        ~agent_before ~agent_after
                        ~logs:
                          (List.map log_entries
                             ~f:(fun (e : Patch_controller.poll_log_entry) ->
                               e.Patch_controller.message));
                      ( orch,
                        ( patch_id,
                          log_entries,
                          newly_blocked,
                          failed_ci,
                          ci_truncated )
                        :: sides ))
            in
            (* Recompute the base-containment cache for every agent: does each
               patch's resolved base branch already contain the squash commit of
               every *merged* dependency of that patch? This is the only git I/O
               of the reconcile phase; it is skipped (trivially [true]) for
               patches based on main or with no merged deps, so the [is_ancestor]
               calls are limited to fan-in patches mid-merge. Fail-closed to
               [false] when a dep's merge SHA is not yet recorded — the
               dependent then defers until [detect_sibling_stale_bases] rebases
               the base (which also fetches the merge commit into the repo). *)
            let graph = Orchestrator.graph orch in
            let has_merged pid =
              match Orchestrator.find_agent orch pid with
              | Some a -> a.Patch_agent.merged
              | None -> false
            in
            let merge_sha pid =
              match Orchestrator.find_agent orch pid with
              | Some a -> a.Patch_agent.merge_commit_sha
              | None -> None
            in
            let main_root = W.resolve_main_root () in
            let ancestor_oracle ancestor ~descendant =
              W.is_ancestor ~path:main_root ~ancestor ~descendant
            in
            let orch =
              List.fold (Orchestrator.all_agents orch) ~init:orch
                ~f:(fun orch (a : Patch_agent.t) ->
                  let contains =
                    Base_containment.contains_merged_siblings ~graph
                      ~patch_id:a.Patch_agent.patch_id ~has_merged ~merge_sha
                      ~branch_of ~main ~ancestor_oracle
                  in
                  Orchestrator.set_base_contains_merged_siblings orch
                    a.Patch_agent.patch_id contains)
            in
            (* Reconcile — detect merges and enqueue rebases *)
            let agents = Orchestrator.all_agents orch in
            let patch_views =
              List.map agents ~f:(fun (a : Patch_agent.t) ->
                  Reconciler.
                    {
                      id = a.Patch_agent.patch_id;
                      has_pr = Patch_agent.has_pr a;
                      merged = a.Patch_agent.merged;
                      busy = a.Patch_agent.busy;
                      needs_intervention = Patch_agent.needs_intervention a;
                      branch_blocked = a.Patch_agent.branch_blocked;
                      queue = a.Patch_agent.queue;
                      base_branch =
                        Option.value a.Patch_agent.base_branch ~default:main;
                      branch_rebased_onto = a.Patch_agent.branch_rebased_onto;
                      base_contains_merged_siblings =
                        a.Patch_agent.base_contains_merged_siblings;
                    })
            in
            let merged_patches =
              List.filter_map agents ~f:(fun (a : Patch_agent.t) ->
                  if a.Patch_agent.merged then Some a.Patch_agent.patch_id
                  else None)
            in
            let actions =
              Reconciler.reconcile ~graph:(Orchestrator.graph orch) ~main
                ~merged_pr_patches:merged_patches ~branch_of patch_views
            in
            let rec_logs = ref [] in
            let orch =
              List.fold actions ~init:orch ~f:(fun orch action ->
                  match action with
                  | Reconciler.Mark_merged pid ->
                      Orchestrator.mark_merged orch pid
                  | Reconciler.Enqueue_rebase pid ->
                      rec_logs :=
                        ("rebase enqueued by reconciler", pid) :: !rec_logs;
                      Orchestrator.enqueue orch pid Operation_kind.Rebase
                  | Reconciler.Start_operation _ -> orch)
            in
            (orch, (List.rev sides, List.rev !rec_logs)))
      in
      (* Phase 3: Side effects — outside the lock *)
      List.iter per_patch_sides
        ~f:(fun
            (patch_id, log_entries, newly_blocked, _failed_ci, ci_truncated) ->
          List.iter log_entries
            ~f:(fun (entry : Patch_controller.poll_log_entry) ->
              Runtime_logging.log_event runtime
                ~patch_id:entry.Patch_controller.patch_id
                entry.Patch_controller.message);
          (if newly_blocked then
             let b =
               Runtime.read runtime (fun snap ->
                   match
                     Orchestrator.find_agent snap.Runtime.orchestrator patch_id
                   with
                   | Some a -> Branch.to_string a.Patch_agent.branch
                   | None -> "unknown")
             in
             let main_root = W.resolve_main_root () in
             Runtime_logging.log_event runtime ~patch_id
               (Printf.sprintf
                  "Cannot work on patch — branch %s is checked out in the main \
                   working tree (%s); release it (e.g. `git -C %s checkout \
                   <default-branch>`) before continuing"
                  b main_root main_root));
          if ci_truncated then
            Runtime_logging.log_event runtime ~patch_id
              "Warning — CI check list was truncated (>100 checks); some \
               failures may not appear in the prompt");
      List.iter reconcile_logs ~f:(fun (msg, pid) ->
          Runtime_logging.log_event runtime ~patch_id:pid msg);
      Eio.Time.sleep clock poll_interval;
      loop ()
    in
    loop ()
end
