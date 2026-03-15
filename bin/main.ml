open Onton
open Onton.Types

(** {1 Configuration} *)

type config = {
  gameplan_path : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  main_branch : string;
  poll_interval : float;
  repo_root : string;
}

(** {1 Helper: extract snapshot fields} *)

let snap_orch (snap : Runtime.snapshot) = snap.Runtime.orchestrator
let snap_gameplan (snap : Runtime.snapshot) = snap.Runtime.gameplan

let snap_with_orch (snap : Runtime.snapshot) orch =
  {
    Runtime.orchestrator = orch;
    activity_log = snap.Runtime.activity_log;
    gameplan = snap.Runtime.gameplan;
  }

(** {1 Fibers} *)

(** TUI rendering fiber — redraws the terminal at ~10 fps. *)
let tui_fiber ~runtime ~clock ~stdout =
  Eio.Flow.copy_string (Tui._enter_tui ()) stdout;
  let rec loop () =
    let frame_str =
      Runtime.read runtime (fun snap ->
          let views =
            Tui._views_of_orchestrator ~orchestrator:(snap_orch snap)
              ~gameplan:(snap_gameplan snap)
          in
          let width =
            match Term.get_size () with
            | Some size -> size.Term.cols
            | None -> 80
          in
          let gp = snap_gameplan snap in
          let frame =
            Tui._render_frame ~width ~activity:[]
              ~project_name:gp.Gameplan.project_name views
          in
          Tui._paint_frame frame)
    in
    Eio.Flow.copy_string frame_str stdout;
    Eio.Time.sleep clock 0.1;
    loop ()
  in
  loop ()

(** Poller fiber — periodically polls GitHub for PR state changes and
    reconciles. *)
let poller_fiber ~runtime ~clock ~net ~github ~config =
  let main = Branch.of_string config.main_branch in
  let rec loop () =
    Runtime.read runtime (fun snap ->
        let agents = Orchestrator.all_agents (snap_orch snap) in
        Base.List.iter agents ~f:(fun (agent : Patch_agent.t) ->
            if agent.Patch_agent.has_pr && not agent.Patch_agent.merged then
              let gp = snap_gameplan snap in
              let patch =
                Base.List.find_exn gp.Gameplan.patches ~f:(fun (p : Patch.t) ->
                    Patch_id.equal p.Patch.id agent.Patch_agent.patch_id)
              in
              let pr_number =
                Pr_number.of_int
                  (Base.Int.of_string (Patch_id.to_string patch.Patch.id))
              in
              match Github.pr_state ~net github pr_number with
              | Error _err -> ()
              | Ok pr_state ->
                  let poll_result =
                    Poller.poll ~was_merged:agent.Patch_agent.merged pr_state
                  in
                  Runtime.update_orchestrator runtime (fun orch ->
                      let orch =
                        if poll_result.Poller.merged then
                          Orchestrator.mark_merged orch
                            agent.Patch_agent.patch_id
                        else orch
                      in
                      let orch =
                        if poll_result.Poller.has_conflict then
                          Orchestrator.set_has_conflict orch
                            agent.Patch_agent.patch_id
                        else orch
                      in
                      Base.List.fold poll_result.Poller.queue ~init:orch
                        ~f:(fun acc kind ->
                          Orchestrator.enqueue acc agent.Patch_agent.patch_id
                            kind));
                  (* Reconcile after polling *)
                  Runtime.update runtime (fun snap ->
                      let agents = Orchestrator.all_agents (snap_orch snap) in
                      let patch_views =
                        Base.List.map agents ~f:(fun (a : Patch_agent.t) ->
                            Reconciler.
                              {
                                id = a.Patch_agent.patch_id;
                                has_pr = a.Patch_agent.has_pr;
                                merged = a.Patch_agent.merged;
                                busy = a.Patch_agent.busy;
                                needs_intervention =
                                  a.Patch_agent.needs_intervention;
                                queue = a.Patch_agent.queue;
                                base_branch =
                                  Base.Option.value a.Patch_agent.base_branch
                                    ~default:main;
                              })
                      in
                      let merged_patches =
                        Base.List.filter_map agents
                          ~f:(fun (a : Patch_agent.t) ->
                            if a.Patch_agent.merged then
                              Some a.Patch_agent.patch_id
                            else None)
                      in
                      let gp = snap_gameplan snap in
                      let branch_of pid =
                        match
                          Base.List.find gp.Gameplan.patches
                            ~f:(fun (p : Patch.t) ->
                              Patch_id.equal p.Patch.id pid)
                        with
                        | Some p -> p.Patch.branch
                        | None -> main
                      in
                      let actions =
                        Reconciler.reconcile
                          ~graph:(Orchestrator.graph (snap_orch snap))
                          ~main ~merged_pr_patches:merged_patches ~branch_of
                          patch_views
                      in
                      let orch =
                        Base.List.fold actions ~init:(snap_orch snap)
                          ~f:(fun orch action ->
                            match action with
                            | Reconciler.Mark_merged pid ->
                                Orchestrator.mark_merged orch pid
                            | Reconciler.Enqueue_rebase pid ->
                                Orchestrator.enqueue orch pid
                                  Operation_kind.Rebase
                            | Reconciler.Start_operation _ -> orch)
                      in
                      let orch, _actions =
                        Orchestrator.tick orch
                          ~patches:(snap_gameplan snap).Gameplan.patches
                      in
                      snap_with_orch snap orch)));
    Eio.Time.sleep clock config.poll_interval;
    loop ()
  in
  loop ()

(** Runner fiber — executes orchestrator actions by spawning Claude processes.
*)
let runner_fiber ~runtime ~env ~config =
  let main = Branch.of_string config.main_branch in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let rec loop () =
    let actions =
      Runtime.read runtime (fun snap ->
          Orchestrator.pending_actions (snap_orch snap)
            ~patches:(snap_gameplan snap).Gameplan.patches)
    in
    Base.List.iter actions ~f:(fun action ->
        match action with
        | Orchestrator.Start (patch_id, base_branch) ->
            let gameplan =
              Runtime.read runtime (fun snap -> snap_gameplan snap)
            in
            let patch =
              Base.List.find_exn gameplan.Gameplan.patches
                ~f:(fun (p : Patch.t) -> Patch_id.equal p.Patch.id patch_id)
            in
            let prompt =
              Prompt.render_patch_prompt patch gameplan
                ~base_branch:(Branch.to_string base_branch)
            in
            Runtime.update runtime (fun snap ->
                let orch, _actions =
                  Orchestrator.tick (snap_orch snap)
                    ~patches:(snap_gameplan snap).Gameplan.patches
                in
                snap_with_orch snap orch);
            let worktree_path =
              Worktree.worktree_dir ~repo_root:config.repo_root ~patch_id
            in
            let cwd = Eio.Path.(fs / worktree_path) in
            let _result =
              Claude_runner.run ~process_mgr ~cwd ~patch_id ~prompt
                ~session_id:None
            in
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.complete orch patch_id)
        | Orchestrator.Respond (patch_id, kind) ->
            let prompt =
              match kind with
              | Operation_kind.Ci -> Prompt.render_ci_failure_prompt []
              | Operation_kind.Review_comments -> Prompt.render_review_prompt []
              | Operation_kind.Merge_conflict ->
                  Prompt.render_merge_conflict_prompt
                    ~base_branch:(Branch.to_string main)
              | Operation_kind.Human -> Prompt.render_human_message_prompt []
              | Operation_kind.Rebase ->
                  Prompt.render_merge_conflict_prompt
                    ~base_branch:(Branch.to_string main)
            in
            Runtime.update runtime (fun snap ->
                let orch, _actions =
                  Orchestrator.tick (snap_orch snap)
                    ~patches:(snap_gameplan snap).Gameplan.patches
                in
                snap_with_orch snap orch);
            let worktree_path =
              Worktree.worktree_dir ~repo_root:config.repo_root ~patch_id
            in
            let cwd = Eio.Path.(fs / worktree_path) in
            let _result =
              Claude_runner.run ~process_mgr ~cwd ~patch_id ~prompt
                ~session_id:None
            in
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.complete orch patch_id));
    Eio.Time.sleep clock 1.0;
    loop ()
  in
  loop ()

(** {1 Main entry point} *)

let run config =
  match Gameplan_parser.parse_file config.gameplan_path with
  | Error msg ->
      Printf.eprintf "Error parsing gameplan: %s\n" msg;
      Stdlib.exit 1
  | Ok parsed ->
      let gameplan = parsed.Gameplan_parser.gameplan in
      let main_branch = Branch.of_string config.main_branch in
      let runtime = Runtime.create ~gameplan ~main_branch in
      let github =
        Github.create ~token:config.github_token ~owner:config.github_owner
          ~repo:config.github_repo
      in
      Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      let net = Eio.Stdenv.net env in
      let stdout = Eio.Stdenv.stdout env in
      Term.Raw.with_raw (fun () ->
          Fun.protect
            ~finally:(fun () -> Eio.Flow.copy_string (Tui._exit_tui ()) stdout)
            (fun () ->
              Eio.Fiber.all
                [
                  (fun () -> tui_fiber ~runtime ~clock ~stdout);
                  (fun () -> poller_fiber ~runtime ~clock ~net ~github ~config);
                  (fun () -> runner_fiber ~runtime ~env ~config);
                ]))

(** {1 CLI via Cmdliner} *)

let gameplan_path_arg =
  let open Cmdliner in
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"GAMEPLAN" ~doc:"Path to the gameplan file.")

let github_token_arg =
  let open Cmdliner in
  Arg.(
    value & opt string ""
    & info [ "token" ] ~docv:"TOKEN" ~doc:"GitHub API token."
        ~env:(Cmd.Env.info "GITHUB_TOKEN"))

let github_owner_arg =
  let open Cmdliner in
  Arg.(
    value & opt string ""
    & info [ "owner" ] ~docv:"OWNER" ~doc:"GitHub repository owner."
        ~env:(Cmd.Env.info "GITHUB_OWNER"))

let github_repo_arg =
  let open Cmdliner in
  Arg.(
    value & opt string ""
    & info [ "repo" ] ~docv:"REPO" ~doc:"GitHub repository name."
        ~env:(Cmd.Env.info "GITHUB_REPO"))

let main_branch_arg =
  let open Cmdliner in
  Arg.(
    value & opt string "main"
    & info [ "main-branch" ] ~docv:"BRANCH"
        ~doc:"Main branch name (default: main).")

let poll_interval_arg =
  let open Cmdliner in
  Arg.(
    value & opt float 30.0
    & info [ "poll-interval" ] ~docv:"SECONDS"
        ~doc:"Polling interval in seconds (default: 30).")

let repo_root_arg =
  let open Cmdliner in
  Arg.(
    value & opt string "."
    & info [ "repo-root" ] ~docv:"PATH"
        ~doc:"Path to the git repository root (default: .).")

let main_cmd =
  let open Cmdliner in
  let run_cmd gameplan_path github_token github_owner github_repo main_branch
      poll_interval repo_root =
    let config =
      {
        gameplan_path;
        github_token;
        github_owner;
        github_repo;
        main_branch;
        poll_interval;
        repo_root;
      }
    in
    run config
  in
  let term =
    Term.(
      const run_cmd $ gameplan_path_arg $ github_token_arg $ github_owner_arg
      $ github_repo_arg $ main_branch_arg $ poll_interval_arg $ repo_root_arg)
  in
  let info =
    Cmd.info "onton" ~version:"0.1.0"
      ~doc:"Orchestrate parallel patch development with Claude."
  in
  Cmd.v info term

let () = Stdlib.exit (Cmdliner.Cmd.eval main_cmd)
