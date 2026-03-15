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

let validate_config config =
  let errors = ref [] in
  if Base.String.is_empty (Base.String.strip config.github_token) then
    errors := "--token / GITHUB_TOKEN is required" :: !errors;
  if Base.String.is_empty (Base.String.strip config.github_owner) then
    errors := "--owner / GITHUB_OWNER is required" :: !errors;
  if Base.String.is_empty (Base.String.strip config.github_repo) then
    errors := "--repo / GITHUB_REPO is required" :: !errors;
  if Float.compare config.poll_interval 0.0 <= 0 then
    errors :=
      Printf.sprintf "--poll-interval must be > 0 (got %g)" config.poll_interval
      :: !errors;
  match !errors with [] -> Ok () | errs -> Error errs

(** {1 PR number registry}

    Maps patch_id -> Pr_number.t. The current data model does not persist PR
    numbers on Patch_agent.t (it only tracks [has_pr : bool]), so we maintain a
    separate table populated when PRs are created and used for polling.

    PR numbers are discovered by querying GitHub for open PRs matching the
    patch's branch name after Claude completes work. *)

module Pr_registry = struct
  type t = (string, Pr_number.t) Hashtbl.t

  let create () : t = Hashtbl.create 64

  let register (t : t) ~patch_id ~pr_number =
    Hashtbl.replace t (Patch_id.to_string patch_id) pr_number

  let find (t : t) ~patch_id = Hashtbl.find_opt t (Patch_id.to_string patch_id)
end

(** Discover PR number for a branch by calling [gh pr list]. Returns [Ok] with
    the PR number or [Error] with a diagnostic message. *)
let discover_pr_number ~process_mgr ~token ~owner ~repo ~branch ~base_branch =
  let args =
    [
      "gh";
      "pr";
      "list";
      "--repo";
      Printf.sprintf "%s/%s" owner repo;
      "--head";
      Branch.to_string branch;
      "--base";
      Branch.to_string base_branch;
      "--json";
      "number";
      "--limit";
      "1";
    ]
  in
  (* Inherit the current environment and inject GH_TOKEN so gh authenticates
     even in headless/CI environments without cached credentials. *)
  let base_env = Unix.environment () in
  let env = Array.append [| Printf.sprintf "GH_TOKEN=%s" token |] base_env in
  try
    let buf = Buffer.create 256 in
    Eio.Process.run ~stdout:(Eio.Flow.buffer_sink buf) ~env process_mgr args;
    let output = Buffer.contents buf in
    match Yojson.Basic.from_string output with
    | `List (`Assoc fields :: _) -> (
        match Base.List.Assoc.find fields ~equal:String.equal "number" with
        | Some (`Int n) -> Ok (Pr_number.of_int n)
        | _ -> Error (Printf.sprintf "unexpected JSON shape: %s" output))
    | `List [] -> Error "no PRs found for branch"
    | _ -> Error (Printf.sprintf "unexpected JSON: %s" output)
  with
  | Eio.Exn.Io _ as e ->
      Error (Printf.sprintf "gh command failed: %s" (Printexc.to_string e))
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
  | exn ->
      Error (Printf.sprintf "unexpected error: %s" (Printexc.to_string exn))

(** {1 Helper: extract snapshot fields} *)

let snap_orch (snap : Runtime.snapshot) = snap.Runtime.orchestrator
let snap_gameplan (snap : Runtime.snapshot) = snap.Runtime.gameplan

let snap_with_orch (snap : Runtime.snapshot) orch =
  {
    Runtime.orchestrator = orch;
    activity_log = snap.Runtime.activity_log;
    gameplan = snap.Runtime.gameplan;
  }

(** {1 Activity log → TUI conversion} *)

let activity_entries_of_log (log : Activity_log.t) =
  (* Collect both kinds with timestamps for chronological merge *)
  let transitions =
    Base.List.map (Activity_log.recent_transitions log ~limit:10)
      ~f:(fun (t : Activity_log.Transition_entry.t) ->
        ( t.Activity_log.Transition_entry.timestamp,
          Tui.Transition
            {
              patch_id =
                Patch_id.to_string t.Activity_log.Transition_entry.patch_id;
              from_label = Tui.label t.Activity_log.Transition_entry.from_status;
              to_status = t.Activity_log.Transition_entry.to_status;
              to_label = Tui.label t.Activity_log.Transition_entry.to_status;
              action = t.Activity_log.Transition_entry.action;
            } ))
  in
  let events =
    Base.List.map (Activity_log.recent_events log ~limit:10)
      ~f:(fun (e : Activity_log.Event.t) ->
        ( e.Activity_log.Event.timestamp,
          Tui.Event
            {
              patch_id =
                Base.Option.map e.Activity_log.Event.patch_id
                  ~f:Patch_id.to_string;
              message = e.Activity_log.Event.message;
            } ))
  in
  (* Sort newest-first by timestamp, then strip timestamps *)
  let merged =
    Base.List.sort (transitions @ events) ~compare:(fun (t1, _) (t2, _) ->
        Base.Float.descending t1 t2)
  in
  Base.List.map merged ~f:snd

(** {1 Fibers} *)

(** TUI rendering fiber — redraws the terminal at ~10 fps. *)
let tui_fiber ~runtime ~clock ~stdout =
  Eio.Flow.copy_string (Tui._enter_tui ()) stdout;
  let rec loop () =
    (* Snapshot minimal state under read lock *)
    let orch, gp, log =
      Runtime.read runtime (fun snap ->
          (snap_orch snap, snap_gameplan snap, snap.Runtime.activity_log))
    in
    (* Render outside the lock to minimize contention *)
    let views = Tui._views_of_orchestrator ~orchestrator:orch ~gameplan:gp in
    let width =
      match Term.get_size () with Some size -> size.Term.cols | None -> 80
    in
    let activity = activity_entries_of_log log in
    let frame =
      Tui._render_frame ~width ~activity ~project_name:gp.Gameplan.project_name
        views
    in
    let frame_str = Tui._paint_frame frame in
    Eio.Flow.copy_string frame_str stdout;
    Eio.Time.sleep clock 0.1;
    loop ()
  in
  loop ()

(** Per-agent poll intent, collected inside [read] and executed outside. *)
type poll_intent =
  | Skip_no_pr of Patch_id.t
  | Poll of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      was_merged : bool;
    }

(** Poller fiber — periodically polls GitHub for PR state changes and
    reconciles. Collects intents inside [read], executes mutations outside to
    avoid deadlock (Eio.Mutex cannot upgrade from read to write lock). *)
let poller_fiber ~runtime ~clock ~net ~github ~config ~pr_registry =
  let main = Branch.of_string config.main_branch in
  (* Track which patches have already logged a skip event to avoid flooding *)
  let skip_logged : (string, bool) Hashtbl.t = Hashtbl.create 16 in
  let rec loop () =
    (* Phase 1: collect poll intents under read lock *)
    let intents =
      Runtime.read runtime (fun snap ->
          let agents = Orchestrator.all_agents (snap_orch snap) in
          Base.List.filter_map agents ~f:(fun (agent : Patch_agent.t) ->
              if agent.Patch_agent.has_pr && not agent.Patch_agent.merged then
                match
                  Pr_registry.find pr_registry
                    ~patch_id:agent.Patch_agent.patch_id
                with
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
    (* Phase 2: execute intents outside read lock *)
    Base.List.iter intents ~f:(fun intent ->
        match intent with
        | Skip_no_pr patch_id ->
            let key = Patch_id.to_string patch_id in
            if not (Hashtbl.mem skip_logged key) then (
              Hashtbl.replace skip_logged key true;
              Runtime.update_activity_log runtime (fun log ->
                  Activity_log.add_event log
                    (Activity_log.Event.create ~timestamp:(Unix.gettimeofday ())
                       ~patch_id "skipping poll: no PR number registered")))
        | Poll { patch_id; pr_number; was_merged } -> (
            match Github.pr_state ~net github pr_number with
            | Error err ->
                Runtime.update_activity_log runtime (fun log ->
                    Activity_log.add_event log
                      (Activity_log.Event.create
                         ~timestamp:(Unix.gettimeofday ()) ~patch_id
                         (Printf.sprintf "poll error: %s"
                            (Github.show_error err))))
            | Ok pr_state ->
                let poll_result = Poller.poll ~was_merged pr_state in
                Runtime.update_orchestrator runtime (fun orch ->
                    let orch =
                      if poll_result.Poller.merged then
                        Orchestrator.mark_merged orch patch_id
                      else orch
                    in
                    let orch =
                      if poll_result.Poller.has_conflict then
                        Orchestrator.set_has_conflict orch patch_id
                      else orch
                    in
                    Base.List.fold poll_result.Poller.queue ~init:orch
                      ~f:(fun acc kind ->
                        Orchestrator.enqueue acc patch_id kind))));
    (* Phase 3: reconcile — separate write lock, no nesting *)
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
                  needs_intervention = a.Patch_agent.needs_intervention;
                  queue = a.Patch_agent.queue;
                  base_branch =
                    Base.Option.value a.Patch_agent.base_branch ~default:main;
                })
        in
        let merged_patches =
          Base.List.filter_map agents ~f:(fun (a : Patch_agent.t) ->
              if a.Patch_agent.merged then Some a.Patch_agent.patch_id else None)
        in
        let gp = snap_gameplan snap in
        let branch_of pid =
          match
            Base.List.find gp.Gameplan.patches ~f:(fun (p : Patch.t) ->
                Patch_id.equal p.Patch.id pid)
          with
          | Some p -> p.Patch.branch
          | None -> main
        in
        let actions =
          Reconciler.reconcile
            ~graph:(Orchestrator.graph (snap_orch snap))
            ~main ~merged_pr_patches:merged_patches ~branch_of patch_views
        in
        let orch =
          Base.List.fold actions ~init:(snap_orch snap) ~f:(fun orch action ->
              match action with
              | Reconciler.Mark_merged pid -> Orchestrator.mark_merged orch pid
              | Reconciler.Enqueue_rebase pid ->
                  Orchestrator.enqueue orch pid Operation_kind.Rebase
              | Reconciler.Start_operation _ -> orch)
        in
        let orch, _actions =
          Orchestrator.tick orch ~patches:(snap_gameplan snap).Gameplan.patches
        in
        snap_with_orch snap orch);
    Eio.Time.sleep clock config.poll_interval;
    loop ()
  in
  loop ()

(** Runner fiber — executes orchestrator actions by spawning Claude processes.
*)
let runner_fiber ~runtime ~env ~config ~pr_registry =
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
        | Orchestrator.Start (patch_id, base_branch) -> (
            let gameplan =
              Runtime.read runtime (fun snap -> snap_gameplan snap)
            in
            match
              Base.List.find gameplan.Gameplan.patches ~f:(fun (p : Patch.t) ->
                  Patch_id.equal p.Patch.id patch_id)
            with
            | None ->
                Runtime.update_activity_log runtime (fun log ->
                    Activity_log.add_event log
                      (Activity_log.Event.create
                         ~timestamp:(Unix.gettimeofday ()) ~patch_id
                         "runner: patch not found in gameplan, skipping"))
            | Some patch -> (
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
                let result =
                  try
                    Ok
                      (Claude_runner.run ~process_mgr ~cwd ~patch_id ~prompt
                         ~session_id:None)
                  with exn -> Error (Printexc.to_string exn)
                in
                match result with
                | Error msg ->
                    Runtime.update_activity_log runtime (fun log ->
                        Activity_log.add_event log
                          (Activity_log.Event.create
                             ~timestamp:(Unix.gettimeofday ()) ~patch_id
                             (Printf.sprintf "Claude process error: %s" msg)));
                    Runtime.update_orchestrator runtime (fun orch ->
                        let orch =
                          Orchestrator.set_session_failed orch patch_id
                        in
                        Orchestrator.complete orch patch_id)
                | Ok r when r.Claude_runner.exit_code = 0 -> (
                    (* Discover and register PR number after Claude creates it.
                       Retry briefly to tolerate GitHub indexing lag. *)
                    let rec discover attempts =
                      match
                        discover_pr_number ~process_mgr
                          ~token:config.github_token ~owner:config.github_owner
                          ~repo:config.github_repo ~branch:patch.Patch.branch
                          ~base_branch
                      with
                      | Ok _ as ok -> ok
                      | Error _ as err when attempts <= 1 -> err
                      | Error _ ->
                          Eio.Time.sleep clock 2.0;
                          discover (attempts - 1)
                    in
                    match discover 3 with
                    | Ok pr_number ->
                        Pr_registry.register pr_registry ~patch_id ~pr_number;
                        Runtime.update_orchestrator runtime (fun orch ->
                            Orchestrator.complete orch patch_id)
                    | Error msg ->
                        Runtime.update_activity_log runtime (fun log ->
                            Activity_log.add_event log
                              (Activity_log.Event.create
                                 ~timestamp:(Unix.gettimeofday ()) ~patch_id
                                 (Printf.sprintf "PR discovery failed: %s" msg)));
                        Runtime.update_orchestrator runtime (fun orch ->
                            Orchestrator.set_session_failed orch patch_id))
                | Ok r ->
                    Runtime.update_activity_log runtime (fun log ->
                        Activity_log.add_event log
                          (Activity_log.Event.create
                             ~timestamp:(Unix.gettimeofday ()) ~patch_id
                             (Printf.sprintf
                                "Claude exited with code %d, marking session \
                                 failed"
                                r.Claude_runner.exit_code)));
                    Runtime.update_orchestrator runtime (fun orch ->
                        let orch =
                          Orchestrator.set_session_failed orch patch_id
                        in
                        Orchestrator.complete orch patch_id)))
        | Orchestrator.Respond (patch_id, kind) -> (
            let agent =
              Runtime.read runtime (fun snap ->
                  Orchestrator.agent (snap_orch snap) patch_id)
            in
            let base =
              Base.Option.value_map agent.Patch_agent.base_branch
                ~default:(Branch.to_string main) ~f:Branch.to_string
            in
            let pending_comments =
              Base.List.map agent.Patch_agent.pending_comments
                ~f:(fun (pc : Patch_agent.pending_comment) ->
                  pc.Patch_agent.comment)
            in
            let prompt =
              match kind with
              | Operation_kind.Ci -> Prompt.render_ci_failure_prompt []
              | Operation_kind.Review_comments ->
                  Prompt.render_review_prompt pending_comments
              | Operation_kind.Merge_conflict ->
                  Prompt.render_merge_conflict_prompt ~base_branch:base
              | Operation_kind.Human ->
                  Prompt.render_human_message_prompt
                    (Base.List.map pending_comments ~f:(fun (c : Comment.t) ->
                         c.Comment.body))
              | Operation_kind.Rebase ->
                  Prompt.render_merge_conflict_prompt ~base_branch:base
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
            let result =
              try
                Ok
                  (Claude_runner.run ~process_mgr ~cwd ~patch_id ~prompt
                     ~session_id:None)
              with exn -> Error (Printexc.to_string exn)
            in
            match result with
            | Error msg ->
                Runtime.update_activity_log runtime (fun log ->
                    Activity_log.add_event log
                      (Activity_log.Event.create
                         ~timestamp:(Unix.gettimeofday ()) ~patch_id
                         (Printf.sprintf "Claude process error: %s" msg)));
                Runtime.update_orchestrator runtime (fun orch ->
                    let orch = Orchestrator.set_session_failed orch patch_id in
                    Orchestrator.complete orch patch_id)
            | Ok r when r.Claude_runner.exit_code = 0 ->
                Runtime.update_orchestrator runtime (fun orch ->
                    Orchestrator.complete orch patch_id)
            | Ok r ->
                Runtime.update_activity_log runtime (fun log ->
                    Activity_log.add_event log
                      (Activity_log.Event.create
                         ~timestamp:(Unix.gettimeofday ()) ~patch_id
                         (Printf.sprintf
                            "Claude respond exited with code %d, marking \
                             session failed"
                            r.Claude_runner.exit_code)));
                Runtime.update_orchestrator runtime (fun orch ->
                    let orch = Orchestrator.set_session_failed orch patch_id in
                    Orchestrator.complete orch patch_id)));
    Eio.Time.sleep clock 1.0;
    loop ()
  in
  loop ()

(** {1 Main entry point} *)

let normalize_config config =
  {
    config with
    github_token = Base.String.strip config.github_token;
    github_owner = Base.String.strip config.github_owner;
    github_repo = Base.String.strip config.github_repo;
  }

let run config =
  let config = normalize_config config in
  match validate_config config with
  | Error errs ->
      Base.List.iter errs ~f:(fun e -> Printf.eprintf "Error: %s\n" e);
      Stdlib.exit 1
  | Ok () -> (
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
          let pr_registry = Pr_registry.create () in
          Eio_main.run @@ fun env ->
          let clock = Eio.Stdenv.clock env in
          let net = Eio.Stdenv.net env in
          let stdout = Eio.Stdenv.stdout env in
          Term.Raw.with_raw (fun () ->
              Fun.protect
                ~finally:(fun () ->
                  Eio.Flow.copy_string (Tui._exit_tui ()) stdout)
                (fun () ->
                  Eio.Fiber.all
                    [
                      (fun () -> tui_fiber ~runtime ~clock ~stdout);
                      (fun () ->
                        poller_fiber ~runtime ~clock ~net ~github ~config
                          ~pr_registry);
                      (fun () ->
                        runner_fiber ~runtime ~env ~config ~pr_registry);
                    ])))

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
