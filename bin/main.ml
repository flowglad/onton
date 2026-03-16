open Onton
open Onton.Types

(** {1 Configuration} *)

type config = {
  gameplan_path : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  main_branch : Branch.t;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
}

let validate_config config =
  let errors =
    Base.List.filter_map
      [
        ( Base.String.is_empty (Base.String.strip config.github_token),
          "--token / GITHUB_TOKEN is required" );
        ( Base.String.is_empty (Base.String.strip config.github_owner),
          "--owner / GITHUB_OWNER is required" );
        ( Base.String.is_empty (Base.String.strip config.github_repo),
          "--repo / GITHUB_REPO is required" );
        ( Float.compare config.poll_interval 0.0 <= 0,
          Printf.sprintf "--poll-interval must be > 0 (got %g)"
            config.poll_interval );
        ( Base.String.is_empty
            (Base.String.strip (Branch.to_string config.main_branch)),
          "--main-branch must be non-empty" );
        ( config.max_concurrency < 1,
          Printf.sprintf "--max-concurrency must be >= 1 (got %d)"
            config.max_concurrency );
      ]
      ~f:(fun (cond, msg) -> if cond then Some msg else None)
  in
  match errors with [] -> Ok () | errs -> Error errs

(** {1 PR number registry}

    Maps patch_id -> Pr_number.t. The current data model does not persist PR
    numbers on Patch_agent.t (it only tracks [has_pr : bool]), so we maintain a
    separate table populated when PRs are created and used for polling.

    PR numbers are discovered by querying GitHub for open PRs matching the
    patch's branch name after Claude completes work. *)

module Pr_registry = struct
  type t = { mutex : Eio.Mutex.t; table : (Patch_id.t, Pr_number.t) Hashtbl.t }

  let create () : t = { mutex = Eio.Mutex.create (); table = Hashtbl.create 64 }

  let register (t : t) ~patch_id ~pr_number =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        Hashtbl.replace t.table patch_id pr_number)

  let find (t : t) ~patch_id =
    Eio.Mutex.use_ro t.mutex (fun () -> Hashtbl.find_opt t.table patch_id)
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

(** {1 Activity log → TUI conversion} *)

let activity_entries_of_log (log : Activity_log.t) =
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
  let merged =
    Base.List.sort (transitions @ events) ~compare:(fun (t1, _) (t2, _) ->
        Base.Float.descending t1 t2)
  in
  Base.List.map merged ~f:snd

(** {1 Branch lookup map}

    Built once at startup to avoid O(n) linear scans per [branch_of] call. *)

let build_branch_map (gameplan : Gameplan.t) ~default =
  let map =
    Base.List.fold gameplan.Gameplan.patches
      ~init:(Base.Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) ->
        Base.Map.set acc ~key:p.Patch.id ~data:p.Patch.branch)
  in
  fun pid -> Base.Option.value (Base.Map.find map pid) ~default

(** {1 Shared helpers} *)

let log_event runtime ?patch_id msg =
  Runtime.update_activity_log runtime (fun log ->
      Activity_log.add_event log
        (Activity_log.Event.create ~timestamp:(Unix.gettimeofday ()) ?patch_id
           msg))

let mark_session_failed runtime patch_id =
  Runtime.update_orchestrator runtime (fun orch ->
      let orch = Orchestrator.set_session_failed orch patch_id in
      Orchestrator.complete orch patch_id)

(** Run a Claude process and handle the result. Returns [`Ok] on successful
    Claude exit (code 0), otherwise [`Failed]. *)
let run_claude_and_handle ~runtime ~process_mgr ~fs ~repo_root ~patch_id ~prompt
    ~session_id =
  let worktree_path = Worktree.worktree_dir ~repo_root ~patch_id in
  let cwd = Eio.Path.(fs / worktree_path) in
  let result =
    try Ok (Claude_runner.run ~process_mgr ~cwd ~patch_id ~prompt ~session_id)
    with exn -> Error (Printexc.to_string exn)
  in
  match result with
  | Error msg ->
      log_event runtime ~patch_id
        (Printf.sprintf "Claude process error: %s" msg);
      mark_session_failed runtime patch_id;
      `Failed
  | Ok r when r.Claude_runner.exit_code = 0 -> `Ok
  | Ok r ->
      log_event runtime ~patch_id
        (Printf.sprintf "Claude exited with code %d, marking session failed"
           r.Claude_runner.exit_code);
      mark_session_failed runtime patch_id;
      `Failed

(** {1 Fibers} *)

exception Quit_tui
(** Raised by the input fiber to signal a clean exit. *)

(** TUI rendering fiber — redraws the terminal at ~10 fps. *)
let tui_fiber ~runtime ~clock ~stdout =
  Eio.Flow.copy_string (Tui.enter_tui ()) stdout;
  let rec loop () =
    let orch, gp, log =
      Runtime.read runtime (fun snap ->
          ( snap.Runtime.orchestrator,
            snap.Runtime.gameplan,
            snap.Runtime.activity_log ))
    in
    let views = Tui.views_of_orchestrator ~orchestrator:orch ~gameplan:gp in
    let width =
      match Term.get_size () with Some size -> size.Term.cols | None -> 80
    in
    let activity = activity_entries_of_log log in
    let frame =
      Tui.render_frame ~width ~activity ~project_name:gp.Gameplan.project_name
        views
    in
    Eio.Flow.copy_string (Tui.paint_frame frame) stdout;
    Eio.Time.sleep clock 0.1;
    loop ()
  in
  loop ()

(** Input fiber — reads keypresses and dispatches TUI commands. *)
let input_fiber ~runtime ~selected =
  let rec loop () =
    match Term.Key.read () with
    | None -> log_event runtime "input fiber: stdin closed (EOF or I/O error)"
    | Some key -> (
        let cmd = Tui_input.of_key key in
        match cmd with
        | Tui_input.Quit -> raise Quit_tui
        | Tui_input.Refresh | Tui_input.Help | Tui_input.Select | Tui_input.Back
        | Tui_input.Noop | Tui_input.Move_up | Tui_input.Move_down
        | Tui_input.Page_up | Tui_input.Page_down ->
            let count =
              Runtime.read runtime (fun snap ->
                  Base.List.length
                    (Orchestrator.all_agents snap.Runtime.orchestrator))
            in
            selected := Tui_input.apply_move ~count ~selected:!selected cmd;
            loop ())
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
    reconciles. *)
let poller_fiber ~runtime ~clock ~net ~github ~config ~pr_registry ~branch_of =
  let main = config.main_branch in
  let skip_logged : (Patch_id.t, bool) Hashtbl.t = Hashtbl.create 16 in
  let rec loop () =
    let intents =
      Runtime.read runtime (fun snap ->
          let agents = Orchestrator.all_agents snap.Runtime.orchestrator in
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
    Base.List.iter intents ~f:(fun intent ->
        match intent with
        | Skip_no_pr patch_id ->
            if not (Hashtbl.mem skip_logged patch_id) then (
              Hashtbl.replace skip_logged patch_id true;
              log_event runtime ~patch_id
                "skipping poll: no PR number registered")
        | Poll { patch_id; pr_number; was_merged } -> (
            match Github.pr_state ~net github pr_number with
            | Error err ->
                log_event runtime ~patch_id
                  (Printf.sprintf "poll error: %s" (Github.show_error err))
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
    (* Reconcile *)
    Runtime.update runtime (fun snap ->
        let orch = snap.Runtime.orchestrator in
        let agents = Orchestrator.all_agents orch in
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
        let gp = snap.Runtime.gameplan in
        let actions =
          Reconciler.reconcile ~graph:(Orchestrator.graph orch) ~main
            ~merged_pr_patches:merged_patches ~branch_of patch_views
        in
        let orch =
          Base.List.fold actions ~init:orch ~f:(fun orch action ->
              match action with
              | Reconciler.Mark_merged pid -> Orchestrator.mark_merged orch pid
              | Reconciler.Enqueue_rebase pid ->
                  Orchestrator.enqueue orch pid Operation_kind.Rebase
              | Reconciler.Start_operation _ -> orch)
        in
        let orch, _actions =
          Orchestrator.tick orch ~patches:gp.Gameplan.patches
        in
        { snap with Runtime.orchestrator = orch });
    Eio.Time.sleep clock config.poll_interval;
    loop ()
  in
  loop ()

(** Runner fiber — executes orchestrator actions by spawning Claude processes
    concurrently. *)
let runner_fiber ~runtime ~env ~config ~pr_registry =
  let main = config.main_branch in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let semaphore = Eio.Semaphore.make config.max_concurrency in
  let with_claude_slot f =
    Eio.Semaphore.acquire semaphore;
    Fun.protect ~finally:(fun () -> Eio.Semaphore.release semaphore) f
  in
  let rec loop () =
    let actions, gameplan =
      Runtime.read runtime (fun snap ->
          let actions =
            Orchestrator.pending_actions snap.Runtime.orchestrator
              ~patches:snap.Runtime.gameplan.Gameplan.patches
          in
          (actions, snap.Runtime.gameplan))
    in
    (* Fire all actions to mark agents busy, preventing re-dispatch on the next
       loop iteration. Note: there is a benign TOCTOU gap between reading
       pending_actions and firing — if another fiber modifies state between
       these calls, fire may encounter already-started patches (Start is a
       no-op on has_pr=true agents) or stale Respond targets (the agent
       preconditions are re-checked by Patch_agent.respond). *)
    if not (Base.List.is_empty actions) then
      Runtime.update_orchestrator runtime (fun orch ->
          Base.List.fold actions ~init:orch ~f:(fun orch action ->
              Orchestrator.fire orch action));
    (* Spawn all actions concurrently, limited by max_concurrency semaphore *)
    let action_fibers =
      Base.List.filter_map actions ~f:(fun action ->
          match action with
          | Orchestrator.Start (patch_id, base_branch) -> (
              match
                Base.List.find gameplan.Gameplan.patches
                  ~f:(fun (p : Patch.t) -> Patch_id.equal p.Patch.id patch_id)
              with
              | None ->
                  log_event runtime ~patch_id
                    "runner: patch not found in gameplan, skipping";
                  mark_session_failed runtime patch_id;
                  None
              | Some patch ->
                  Some
                    (fun () ->
                      let result =
                        with_claude_slot (fun () ->
                            let agent =
                              Runtime.read runtime (fun snap ->
                                  Orchestrator.agent snap.Runtime.orchestrator
                                    patch_id)
                            in
                            if
                              agent.Patch_agent.merged
                              || agent.Patch_agent.needs_intervention
                              || not agent.Patch_agent.busy
                            then (
                              log_event runtime ~patch_id
                                "runner: action stale after semaphore wait, \
                                 skipping";
                              `Stale)
                            else
                              let prompt =
                                Prompt.render_patch_prompt patch gameplan
                                  ~base_branch:(Branch.to_string base_branch)
                              in
                              run_claude_and_handle ~runtime ~process_mgr ~fs
                                ~repo_root:config.repo_root ~patch_id ~prompt
                                ~session_id:None)
                      in
                      match result with
                      | `Stale | `Failed -> ()
                      | `Ok ->
                          let rec discover remaining =
                            match
                              discover_pr_number ~process_mgr
                                ~token:config.github_token
                                ~owner:config.github_owner
                                ~repo:config.github_repo
                                ~branch:patch.Patch.branch ~base_branch
                            with
                            | Ok pr_number ->
                                Pr_registry.register pr_registry ~patch_id
                                  ~pr_number;
                                Runtime.update_orchestrator runtime (fun orch ->
                                    Orchestrator.complete orch patch_id)
                            | Error _ when remaining > 0 ->
                                Eio.Time.sleep clock 2.0;
                                discover (remaining - 1)
                            | Error msg ->
                                log_event runtime ~patch_id
                                  (Printf.sprintf "PR discovery failed: %s" msg);
                                mark_session_failed runtime patch_id
                          in
                          discover 2))
          | Orchestrator.Respond (patch_id, kind) ->
              Some
                (fun () ->
                  let result =
                    with_claude_slot (fun () ->
                        let agent =
                          Runtime.read runtime (fun snap ->
                              Orchestrator.agent snap.Runtime.orchestrator
                                patch_id)
                        in
                        if
                          agent.Patch_agent.merged
                          || agent.Patch_agent.needs_intervention
                          || not agent.Patch_agent.busy
                        then (
                          log_event runtime ~patch_id
                            "runner: action stale after semaphore wait, \
                             skipping";
                          `Stale)
                        else
                          let base =
                            Base.Option.value_map agent.Patch_agent.base_branch
                              ~default:(Branch.to_string main)
                              ~f:Branch.to_string
                          in
                          let pending_comments =
                            Base.List.map agent.Patch_agent.pending_comments
                              ~f:(fun (pc : Patch_agent.pending_comment) ->
                                pc.Patch_agent.comment)
                          in
                          let prompt =
                            match kind with
                            | Operation_kind.Ci ->
                                (* TODO: Patch_agent doesn't store Ci_check.t
                                   details yet — only ci_failure_count.
                                   Propagate check details from Poller to
                                   surface them here. *)
                                Prompt.render_ci_failure_prompt []
                            | Operation_kind.Review_comments ->
                                Prompt.render_review_prompt pending_comments
                            | Operation_kind.Merge_conflict ->
                                Prompt.render_merge_conflict_prompt
                                  ~base_branch:base
                            | Operation_kind.Human ->
                                Prompt.render_human_message_prompt
                                  (Base.List.map pending_comments
                                     ~f:(fun (c : Comment.t) -> c.Comment.body))
                            | Operation_kind.Rebase ->
                                Prompt.render_merge_conflict_prompt
                                  ~base_branch:base
                          in
                          run_claude_and_handle ~runtime ~process_mgr ~fs
                            ~repo_root:config.repo_root ~patch_id ~prompt
                            ~session_id:None)
                  in
                  match result with
                  | `Stale | `Failed -> ()
                  | `Ok ->
                      Runtime.update_orchestrator runtime (fun orch ->
                          Orchestrator.complete orch patch_id)))
    in
    if not (Base.List.is_empty action_fibers) then Eio.Fiber.all action_fibers;
    Eio.Time.sleep clock 1.0;
    loop ()
  in
  loop ()

(** {1 Main entry point} *)

let run config =
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
          let runtime =
            Runtime.create ~gameplan ~main_branch:config.main_branch
          in
          let github =
            Github.create ~token:config.github_token ~owner:config.github_owner
              ~repo:config.github_repo
          in
          let pr_registry = Pr_registry.create () in
          let branch_of =
            build_branch_map gameplan ~default:config.main_branch
          in
          Eio_main.run @@ fun env ->
          let clock = Eio.Stdenv.clock env in
          let net = Eio.Stdenv.net env in
          let stdout = Eio.Stdenv.stdout env in
          let selected = ref 0 in
          Term.Raw.with_raw (fun () ->
              Fun.protect
                ~finally:(fun () ->
                  Eio.Flow.copy_string (Tui.exit_tui ()) stdout)
                (fun () ->
                  try
                    Eio.Fiber.all
                      [
                        (fun () -> tui_fiber ~runtime ~clock ~stdout);
                        (fun () -> input_fiber ~runtime ~selected);
                        (fun () ->
                          poller_fiber ~runtime ~clock ~net ~github ~config
                            ~pr_registry ~branch_of);
                        (fun () ->
                          runner_fiber ~runtime ~env ~config ~pr_registry);
                      ]
                  with Quit_tui -> ())))

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

let max_concurrency_arg =
  let open Cmdliner in
  Arg.(
    value & opt int 5
    & info [ "max-concurrency" ] ~docv:"N"
        ~doc:"Maximum number of concurrent Claude processes (default: 5)."
        ~env:(Cmd.Env.info "ONTON_MAX_CONCURRENCY"))

let main_cmd =
  let open Cmdliner in
  let run_cmd gameplan_path github_token github_owner github_repo main_branch
      poll_interval repo_root max_concurrency =
    let config =
      {
        gameplan_path;
        github_token = Base.String.strip github_token;
        github_owner = Base.String.strip github_owner;
        github_repo = Base.String.strip github_repo;
        main_branch = Branch.of_string main_branch;
        poll_interval;
        repo_root;
        max_concurrency;
      }
    in
    run config
  in
  let term =
    Term.(
      const run_cmd $ gameplan_path_arg $ github_token_arg $ github_owner_arg
      $ github_repo_arg $ main_branch_arg $ poll_interval_arg $ repo_root_arg
      $ max_concurrency_arg)
  in
  let info =
    Cmd.info "onton" ~version:"0.1.0"
      ~doc:"Orchestrate parallel patch development with Claude."
  in
  Cmd.v info term

let () = Stdlib.exit (Cmdliner.Cmd.eval main_cmd)
