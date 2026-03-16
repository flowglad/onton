open Onton
open Onton.Types

(** {1 Configuration} *)

type config = {
  project : string option;
  github_token : string;
  github_owner : string;
  github_repo : string;
  main_branch : Branch.t;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  headless : bool;
}

let validate_resolved_config ~github_token ~github_owner ~github_repo
    ~main_branch ~poll_interval ~max_concurrency =
  let errors =
    Base.List.filter_map
      [
        ( Base.String.is_empty (Base.String.strip github_token),
          "--token / GITHUB_TOKEN is required" );
        ( Base.String.is_empty (Base.String.strip github_owner),
          "--owner / GITHUB_OWNER is required" );
        ( Base.String.is_empty (Base.String.strip github_repo),
          "--repo / GITHUB_REPO is required" );
        ( Base.String.is_empty (Base.String.strip (Branch.to_string main_branch)),
          "--main-branch cannot be empty" );
        ( Float.compare poll_interval 0.0 <= 0,
          Printf.sprintf "--poll-interval must be > 0 (got %g)" poll_interval );
        ( max_concurrency < 1,
          Printf.sprintf "--max-concurrency must be >= 1 (got %d)"
            max_concurrency );
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

(** {1 Activity log helpers} *)

(** Merge events and transitions from an activity log into a single
    timestamp-tagged list. [compare] controls sort direction. *)
let merged_log_entries ~(log : Activity_log.t) ~limit ~compare
    ~(map_event : Activity_log.Event.t -> 'a)
    ~(map_transition : Activity_log.Transition_entry.t -> 'a) =
  let events =
    Base.List.map (Activity_log.recent_events log ~limit) ~f:(fun e ->
        (e.Activity_log.Event.timestamp, map_event e))
  in
  let transitions =
    Base.List.map (Activity_log.recent_transitions log ~limit) ~f:(fun t ->
        (t.Activity_log.Transition_entry.timestamp, map_transition t))
  in
  Base.List.sort (events @ transitions) ~compare

let activity_entries_of_log ?(limit = 10) (log : Activity_log.t) =
  merged_log_entries ~log ~limit
    ~compare:(fun (t1, _) (t2, _) -> Base.Float.descending t1 t2)
    ~map_event:(fun (e : Activity_log.Event.t) ->
      Tui.Event
        {
          patch_id =
            Base.Option.map e.Activity_log.Event.patch_id ~f:Patch_id.to_string;
          message = e.Activity_log.Event.message;
        })
    ~map_transition:(fun (t : Activity_log.Transition_entry.t) ->
      Tui.Transition
        {
          patch_id = Patch_id.to_string t.Activity_log.Transition_entry.patch_id;
          from_label = Tui.label t.Activity_log.Transition_entry.from_status;
          to_status = t.Activity_log.Transition_entry.to_status;
          to_label = Tui.label t.Activity_log.Transition_entry.to_status;
          action = t.Activity_log.Transition_entry.action;
        })
  |> Base.List.map ~f:snd

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

(** TUI rendering fiber — redraws the terminal at ~10 fps.

    [selected] and [view_mode] are shared mutable refs updated by the input
    fiber. *)
let tui_fiber ~runtime ~clock ~stdout ~selected ~view_mode =
  Eio.Flow.copy_string (Tui.enter_tui ()) stdout;
  let rec loop () =
    let orch, gp, log =
      Runtime.read runtime (fun snap ->
          ( snap.Runtime.orchestrator,
            snap.Runtime.gameplan,
            snap.Runtime.activity_log ))
    in
    let views = Tui.views_of_orchestrator ~orchestrator:orch ~gameplan:gp in
    let size = Term.get_size () in
    let width = match size with Some s -> s.Term.cols | None -> 80 in
    let height = match size with Some s -> s.Term.rows | None -> 24 in
    let limit =
      match !view_mode with
      | Tui.Timeline_view -> 100
      | Tui.List_view | Tui.Detail_view _ -> 10
    in
    let activity = activity_entries_of_log ~limit log in
    let frame =
      Tui.render_frame ~width ~height ~selected:!selected ~view_mode:!view_mode
        ~activity ~project_name:gp.Gameplan.project_name views
    in
    Eio.Flow.copy_string (Tui.paint_frame frame) stdout;
    Eio.Time.sleep clock 0.1;
    loop ()
  in
  loop ()

(** Input fiber — reads keypresses and dispatches TUI commands.

    Supports two modes:
    - Normal mode: single-key navigation (j/k, arrows, q to quit, enter for
      detail)
    - Text mode: entered via [:], accumulates a line buffer, dispatched on Enter

    Text-mode commands (parsed by {!Tui_input.parse_line}):
    - ["N> message"] — send human message to patch N
    - ["+123"] — register ad-hoc PR #123 for the selected patch *)
let input_fiber ~runtime ~selected ~view_mode ~pr_registry =
  let buf = Buffer.create 64 in
  let text_mode = ref false in
  let saved_list_selected = ref 0 in
  let history = Tui_input.History.create () in
  let saved_draft = ref "" in
  let rec loop () =
    match Term.Key.read () with
    | None -> log_event runtime "input fiber: stdin closed (EOF or I/O error)"
    | Some key -> (
        if !text_mode then
          match key with
          | Term.Key.Escape ->
              Buffer.clear buf;
              saved_draft := "";
              Tui_input.History.reset_browse history;
              text_mode := false;
              loop ()
          | Term.Key.Enter ->
              let line = Buffer.contents buf in
              Tui_input.History.push history line;
              (* Always exit browse mode on Enter, even for empty input that push ignores *)
              Tui_input.History.reset_browse history;
              Buffer.clear buf;
              saved_draft := "";
              text_mode := false;
              (match Tui_input.parse_line line with
              | Some (Tui_input.Send_message (patch_id, msg)) ->
                  let patch_exists =
                    Runtime.read runtime (fun snap ->
                        Base.Map.mem
                          (Orchestrator.agents_map snap.Runtime.orchestrator)
                          patch_id)
                  in
                  if patch_exists then (
                    Runtime.update_orchestrator runtime (fun orch ->
                        Orchestrator.send_human_message orch patch_id msg);
                    log_event runtime ~patch_id
                      (Printf.sprintf "Human message sent: %s" msg))
                  else
                    log_event runtime
                      (Printf.sprintf
                         "Cannot send human message: unknown patch %s"
                         (Patch_id.to_string patch_id))
              | Some (Tui_input.Add_pr pr_number) -> (
                  let patch_id_opt =
                    Runtime.read runtime (fun snap ->
                        let agents =
                          Orchestrator.all_agents snap.Runtime.orchestrator
                        in
                        let count = Base.List.length agents in
                        if count = 0 then None
                        else
                          let idx =
                            Base.Int.max 0 (Base.Int.min !selected (count - 1))
                          in
                          Some
                            (Base.List.nth_exn agents idx).Patch_agent.patch_id)
                  in
                  match patch_id_opt with
                  | None ->
                      log_event runtime
                        "Cannot register ad-hoc PR: no selectable patch"
                  | Some patch_id ->
                      Pr_registry.register pr_registry ~patch_id ~pr_number;
                      Runtime.update_orchestrator runtime (fun orch ->
                          let orch =
                            Orchestrator.set_pr_number orch patch_id pr_number
                          in
                          Orchestrator.clear_needs_intervention orch patch_id);
                      log_event runtime ~patch_id
                        (Printf.sprintf "Ad-hoc PR #%d registered"
                           (Pr_number.to_int pr_number)))
              | Some
                  ( Tui_input.Quit | Tui_input.Refresh | Tui_input.Help
                  | Tui_input.Move_up | Tui_input.Move_down | Tui_input.Page_up
                  | Tui_input.Page_down | Tui_input.Select | Tui_input.Back
                  | Tui_input.Timeline | Tui_input.Noop )
              | None ->
                  log_event runtime
                    (Printf.sprintf "Unrecognised input: %s" line));
              loop ()
          | Term.Key.Backspace | Term.Key.Delete ->
              let len = Buffer.length buf in
              if len > 0 then (
                let contents = Buffer.contents buf in
                Buffer.clear buf;
                Buffer.add_string buf (String.sub contents 0 (len - 1)));
              loop ()
          | Term.Key.Char c ->
              Buffer.add_char buf c;
              loop ()
          | Term.Key.Up ->
              let was_browsing = Tui_input.History.is_browsing history in
              (match Tui_input.History.older history with
              | Some s ->
                  if not was_browsing then saved_draft := Buffer.contents buf;
                  Buffer.clear buf;
                  Buffer.add_string buf s
              | None -> ());
              loop ()
          | Term.Key.Down ->
              (if Tui_input.History.is_browsing history then
                 match Tui_input.History.newer history with
                 | Tui_input.History.Entry s ->
                     Buffer.clear buf;
                     Buffer.add_string buf s
                 | Tui_input.History.At_fresh ->
                     Buffer.clear buf;
                     Buffer.add_string buf !saved_draft);
              loop ()
          | Term.Key.Left | Term.Key.Right | Term.Key.Home | Term.Key.End
          | Term.Key.Page_up | Term.Key.Page_down | Term.Key.Tab | Term.Key.F _
          | Term.Key.Ctrl _ | Term.Key.Unknown _ ->
              loop ()
        else
          let cmd = Tui_input.of_key key in
          match cmd with
          | Tui_input.Quit -> raise Quit_tui
          | Tui_input.Move_up | Tui_input.Move_down | Tui_input.Page_up
          | Tui_input.Page_down ->
              (match !view_mode with
              | Tui.List_view ->
                  let count =
                    Runtime.read runtime (fun snap ->
                        Base.List.length
                          (Orchestrator.all_agents snap.Runtime.orchestrator))
                  in
                  selected :=
                    Tui_input.apply_move ~count ~selected:!selected cmd
              | Tui.Timeline_view ->
                  let count =
                    Runtime.read runtime (fun snap ->
                        let log = snap.Runtime.activity_log in
                        let events =
                          Base.List.length
                            (Activity_log.recent_events log ~limit:100)
                        in
                        let transitions =
                          Base.List.length
                            (Activity_log.recent_transitions log ~limit:100)
                        in
                        events + transitions)
                  in
                  selected :=
                    Tui_input.apply_move ~count ~selected:!selected cmd
              | Tui.Detail_view _ -> ());
              loop ()
          | Tui_input.Select -> (
              match !view_mode with
              | Tui.List_view ->
                  let agents =
                    Runtime.read runtime (fun snap ->
                        Orchestrator.all_agents snap.Runtime.orchestrator)
                  in
                  let count = Base.List.length agents in
                  if count > 0 then (
                    let idx =
                      Base.Int.max 0 (Base.Int.min !selected (count - 1))
                    in
                    selected := idx;
                    let agent = Base.List.nth_exn agents idx in
                    view_mode := Tui.Detail_view agent.Patch_agent.patch_id);
                  loop ()
              | Tui.Detail_view _ ->
                  text_mode := true;
                  loop ()
              | Tui.Timeline_view -> loop ())
          | Tui_input.Back -> (
              match !view_mode with
              | Tui.Detail_view _ ->
                  view_mode := Tui.List_view;
                  loop ()
              | Tui.Timeline_view ->
                  view_mode := Tui.List_view;
                  selected := !saved_list_selected;
                  loop ()
              | Tui.List_view -> loop ())
          | Tui_input.Timeline -> (
              match !view_mode with
              | Tui.Timeline_view ->
                  view_mode := Tui.List_view;
                  selected := !saved_list_selected;
                  loop ()
              | Tui.List_view | Tui.Detail_view _ ->
                  saved_list_selected := !selected;
                  view_mode := Tui.Timeline_view;
                  selected := 0;
                  loop ())
          | Tui_input.Refresh | Tui_input.Help | Tui_input.Noop
          | Tui_input.Send_message _ | Tui_input.Add_pr _ ->
              loop ())
  in
  loop ()

(** Format a Unix timestamp as HH:MM:SS for headless log lines. *)
let format_time ts =
  let t = Unix.localtime ts in
  Printf.sprintf "%02d:%02d:%02d" t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

let format_event (e : Activity_log.Event.t) =
  let pid_str =
    match e.Activity_log.Event.patch_id with
    | Some pid -> Printf.sprintf "[%s] " (Patch_id.to_string pid)
    | None -> ""
  in
  pid_str ^ e.Activity_log.Event.message

let format_transition (t : Activity_log.Transition_entry.t) =
  Printf.sprintf "[%s] %s -> %s"
    (Patch_id.to_string t.Activity_log.Transition_entry.patch_id)
    (Tui.label t.Activity_log.Transition_entry.from_status)
    (Tui.label t.Activity_log.Transition_entry.to_status)

(** Headless logging fiber — prints events and transitions to stdout as plain
    text, without TUI escape codes. Uses a count-based cursor to avoid losing
    entries with identical timestamps. *)
let headless_fiber ~runtime ~clock ~stdout =
  let seen_count = ref 0 in
  let rec loop () =
    let entries =
      Runtime.read runtime (fun snap ->
          merged_log_entries ~log:snap.Runtime.activity_log ~limit:50
            ~compare:(fun (t1, _) (t2, _) -> Base.Float.ascending t1 t2)
            ~map_event:format_event ~map_transition:format_transition)
    in
    let total = Base.List.length entries in
    if total > !seen_count then (
      let new_entries = Base.List.drop entries !seen_count in
      Base.List.iter new_entries ~f:(fun (ts, msg) ->
          Eio.Flow.copy_string
            (Printf.sprintf "%s %s\n" (format_time ts) msg)
            stdout);
      seen_count := total);
    Eio.Time.sleep clock 1.0;
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
                    let orch =
                      Base.List.fold poll_result.Poller.queue ~init:orch
                        ~f:(fun acc kind ->
                          Orchestrator.enqueue acc patch_id kind)
                    in
                    Base.List.fold pr_state.Github.Pr_state.comments ~init:orch
                      ~f:(fun acc comment ->
                        Orchestrator.add_pending_comment acc patch_id comment
                          ~valid:true))));
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
let runner_fiber ~runtime ~env ~config ~project_name ~pr_registry =
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
                                Prompt.render_patch_prompt ~project_name patch
                                  gameplan
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
                                    let orch =
                                      Orchestrator.set_pr_number orch patch_id
                                        pr_number
                                    in
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
                                Prompt.render_ci_failure_unknown_prompt
                                  ~project_name
                            | Operation_kind.Review_comments ->
                                Prompt.render_review_prompt ~project_name
                                  pending_comments
                            | Operation_kind.Merge_conflict ->
                                Prompt.render_merge_conflict_prompt
                                  ~project_name ~base_branch:base
                            | Operation_kind.Human ->
                                Prompt.render_human_message_prompt ~project_name
                                  (Base.List.map pending_comments
                                     ~f:(fun (c : Comment.t) -> c.Comment.body))
                            | Operation_kind.Rebase ->
                                Prompt.render_merge_conflict_prompt
                                  ~project_name ~base_branch:base
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

(** {1 Persistence fiber} *)

(** Periodic persistence fiber — saves runtime snapshot every 5 seconds. *)
let persistence_fiber ~runtime ~clock ~project_name =
  let path = Project_store.snapshot_path project_name in
  Project_store.ensure_dir (Stdlib.Filename.dirname path);
  let rec loop () =
    Eio.Time.sleep clock 5.0;
    let snap = Runtime.read runtime (fun s -> s) in
    (match Persistence.save ~path snap with
    | Ok () -> ()
    | Error msg ->
        log_event runtime (Printf.sprintf "persistence save error: %s" msg));
    loop ()
  in
  loop ()

(** {1 Main entry point} *)

(** Try to load a persisted snapshot for a project. *)
let load_snapshot ~project_name ~gameplan =
  let path = Project_store.snapshot_path project_name in
  if Stdlib.Sys.file_exists path then
    match Persistence.load ~path ~gameplan with
    | Ok snap -> Some snap
    | Error _ -> None
  else None

(** Resolve CLI args into a config ready to run.
    - [--gameplan] provided: parse it, persist config + gameplan source, derive
      project name.
    - [PROJECT] only: load stored config + gameplan. CLI flags override stored
      values. *)
let resolve_config ~project ~gameplan_path ~github_token ~github_owner
    ~github_repo ~main_branch ~poll_interval ~repo_root ~max_concurrency
    ~headless =
  match (project, gameplan_path) with
  | None, None ->
      Error [ "Provide a PROJECT name to resume or --gameplan to start new." ]
  | _, Some gp_path -> (
      match Gameplan_parser.parse_file gp_path with
      | Error msg -> Error [ Printf.sprintf "Error parsing gameplan: %s" msg ]
      | Ok parsed ->
          let gameplan = parsed.Gameplan_parser.gameplan in
          let project_name =
            match project with
            | Some p -> p
            | None -> gameplan.Gameplan.project_name
          in
          let token = Base.String.strip github_token in
          let owner = Base.String.strip github_owner in
          let repo = Base.String.strip github_repo in
          Project_store.save_config ~project_name ~github_token:token
            ~github_owner:owner ~github_repo:repo
            ~main_branch:(Branch.to_string main_branch)
            ~poll_interval ~repo_root ~max_concurrency;
          Project_store.save_gameplan_source ~project_name ~source_path:gp_path;
          let existing_snapshot = load_snapshot ~project_name ~gameplan in
          Ok
            ( {
                project = Some project_name;
                github_token = token;
                github_owner = owner;
                github_repo = repo;
                main_branch;
                poll_interval;
                repo_root;
                max_concurrency;
                headless;
              },
              gameplan,
              existing_snapshot ))
  | Some proj, None -> (
      if not (Project_store.project_exists proj) then
        Error
          [
            Printf.sprintf
              "No stored project %S. Use --gameplan to start a new project."
              proj;
          ]
      else
        let stored_gp_path = Project_store.gameplan_path proj in
        if not (Stdlib.Sys.file_exists stored_gp_path) then
          Error
            [ Printf.sprintf "Stored gameplan not found for project %S." proj ]
        else
          match Gameplan_parser.parse_file stored_gp_path with
          | Error msg ->
              Error [ Printf.sprintf "Error parsing stored gameplan: %s" msg ]
          | Ok parsed -> (
              let gameplan = parsed.Gameplan_parser.gameplan in
              match Project_store.load_config ~project_name:proj with
              | Error msg ->
                  Error [ Printf.sprintf "Error loading config: %s" msg ]
              | Ok stored ->
                  let token =
                    if Base.String.is_empty (Base.String.strip github_token)
                    then stored.Project_store.github_token
                    else Base.String.strip github_token
                  in
                  let owner =
                    if Base.String.is_empty (Base.String.strip github_owner)
                    then stored.Project_store.github_owner
                    else Base.String.strip github_owner
                  in
                  let repo =
                    if Base.String.is_empty (Base.String.strip github_repo) then
                      stored.Project_store.github_repo
                    else Base.String.strip github_repo
                  in
                  let branch =
                    if Base.String.equal (Branch.to_string main_branch) "main"
                    then Branch.of_string stored.Project_store.main_branch
                    else main_branch
                  in
                  let existing_snapshot =
                    load_snapshot ~project_name:proj ~gameplan
                  in
                  Ok
                    ( {
                        project = Some proj;
                        github_token = token;
                        github_owner = owner;
                        github_repo = repo;
                        main_branch = branch;
                        poll_interval = stored.Project_store.poll_interval;
                        repo_root = stored.Project_store.repo_root;
                        max_concurrency = stored.Project_store.max_concurrency;
                        headless;
                      },
                      gameplan,
                      existing_snapshot )))

let run_with_config (config : config) gameplan existing_snapshot =
  let project_name =
    match config.project with Some p -> p | None -> assert false
  in
  match
    validate_resolved_config ~github_token:config.github_token
      ~github_owner:config.github_owner ~github_repo:config.github_repo
      ~main_branch:config.main_branch ~poll_interval:config.poll_interval
      ~max_concurrency:config.max_concurrency
  with
  | Error errs ->
      Base.List.iter errs ~f:(fun e -> Printf.eprintf "Error: %s\n" e);
      Stdlib.exit 1
  | Ok () ->
      let runtime =
        match existing_snapshot with
        | Some snap ->
            Printf.eprintf "Resuming project %S from saved state.\n%!"
              project_name;
            let rt = Runtime.create ~gameplan ~main_branch:config.main_branch in
            Runtime.update rt (fun _ -> snap);
            rt
        | None ->
            Printf.eprintf "Starting new project %S.\n%!" project_name;
            Runtime.create ~gameplan ~main_branch:config.main_branch
      in
      let github =
        Github.create ~token:config.github_token ~owner:config.github_owner
          ~repo:config.github_repo
      in
      let pr_registry = Pr_registry.create () in
      let branch_of = build_branch_map gameplan ~default:config.main_branch in
      Eio_main.run @@ fun env ->
      let process_mgr = Eio.Stdenv.process_mgr env in
      let startup =
        Startup_reconciler.reconcile ~process_mgr ~token:config.github_token
          ~owner:config.github_owner ~repo:config.github_repo
          ~patches:gameplan.Gameplan.patches
      in
      let errored_ids =
        Base.List.map startup.Startup_reconciler.errors
          ~f:(fun (patch_id, err) ->
            log_event runtime ~patch_id
              (Printf.sprintf "startup discovery error: %s" err);
            patch_id)
        |> Base.Hash_set.of_list (module Patch_id)
      in
      (* Seed Pr_registry from snapshot for patches Startup_reconciler errored on *)
      Runtime.read runtime (fun snap ->
          Orchestrator.all_agents snap.Runtime.orchestrator)
      |> Base.List.iter ~f:(fun (agent : Patch_agent.t) ->
          if Base.Hash_set.mem errored_ids agent.Patch_agent.patch_id then
            Base.Option.iter agent.Patch_agent.pr_number ~f:(fun pr_number ->
                Pr_registry.register pr_registry
                  ~patch_id:agent.Patch_agent.patch_id ~pr_number));
      Base.List.iter startup.Startup_reconciler.discovered ~f:(fun d ->
          let pid = d.Startup_reconciler.patch_id in
          let pr = d.Startup_reconciler.pr_number in
          let base = d.Startup_reconciler.base_branch in
          let merged = d.Startup_reconciler.merged in
          Pr_registry.register pr_registry ~patch_id:pid ~pr_number:pr;
          Runtime.update_orchestrator runtime (fun orch ->
              let orch =
                Orchestrator.fire orch (Orchestrator.Start (pid, base))
              in
              let orch = Orchestrator.set_pr_number orch pid pr in
              let orch = Orchestrator.complete orch pid in
              if merged then Orchestrator.mark_merged orch pid else orch));
      let clock = Eio.Stdenv.clock env in
      let net = Eio.Stdenv.net env in
      let stdout = Eio.Stdenv.stdout env in
      let common_fibers =
        [
          (fun () ->
            poller_fiber ~runtime ~clock ~net ~github ~config ~pr_registry
              ~branch_of);
          (fun () ->
            runner_fiber ~runtime ~env ~config ~project_name ~pr_registry);
          (fun () -> persistence_fiber ~runtime ~clock ~project_name);
        ]
      in
      if config.headless then
        Eio.Fiber.all
          ((fun () -> headless_fiber ~runtime ~clock ~stdout) :: common_fibers)
      else
        let selected = ref 0 in
        let view_mode = ref Tui.List_view in
        Term.Raw.with_raw (fun () ->
            Fun.protect
              ~finally:(fun () ->
                Eio.Flow.copy_string (Tui.exit_tui ()) stdout;
                let snap = Runtime.read runtime (fun s -> s) in
                ignore
                  (Persistence.save
                     ~path:(Project_store.snapshot_path project_name)
                     snap))
              (fun () ->
                try
                  Eio.Fiber.all
                    ((fun () ->
                       tui_fiber ~runtime ~clock ~stdout ~selected ~view_mode)
                    :: (fun () ->
                      input_fiber ~runtime ~selected ~view_mode ~pr_registry)
                    :: common_fibers)
                with Quit_tui -> ()))

let run ~project ~gameplan_path ~github_token ~github_owner ~github_repo
    ~main_branch ~poll_interval ~repo_root ~max_concurrency ~headless =
  match
    resolve_config ~project ~gameplan_path ~github_token ~github_owner
      ~github_repo ~main_branch ~poll_interval ~repo_root ~max_concurrency
      ~headless
  with
  | Error errs ->
      Base.List.iter errs ~f:(fun e -> Printf.eprintf "Error: %s\n" e);
      Stdlib.exit 1
  | Ok (config, gameplan, existing_snapshot) ->
      run_with_config config gameplan existing_snapshot

(** {1 CLI via Cmdliner} *)

let project_arg =
  let open Cmdliner in
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"PROJECT"
        ~doc:
          "Project name to resume. If omitted, derived from --gameplan's \
           project name.")

let gameplan_path_arg =
  let open Cmdliner in
  Arg.(
    value
    & opt (some string) None
    & info [ "gameplan" ] ~docv:"GAMEPLAN" ~doc:"Path to the gameplan file.")

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

let headless_arg =
  let open Cmdliner in
  Arg.(
    value & flag
    & info [ "headless" ] ~doc:"Run without TUI (plain log output).")

let main_cmd =
  let open Cmdliner in
  let run_cmd project gameplan_path github_token github_owner github_repo
      main_branch poll_interval repo_root max_concurrency headless =
    run ~project ~gameplan_path ~github_token ~github_owner ~github_repo
      ~main_branch:(Branch.of_string (Base.String.strip main_branch))
      ~poll_interval ~repo_root ~max_concurrency ~headless
  in
  let term =
    Term.(
      const run_cmd $ project_arg $ gameplan_path_arg $ github_token_arg
      $ github_owner_arg $ github_repo_arg $ main_branch_arg $ poll_interval_arg
      $ repo_root_arg $ max_concurrency_arg $ headless_arg)
  in
  let info =
    Cmd.info "onton" ~version:"0.1.0"
      ~doc:
        "Orchestrate parallel patch development with Claude.\n\n\
         Usage:\n\
        \  onton [PROJECT] --gameplan GAMEPLAN [OPTIONS]   Start a new project\n\
        \  onton PROJECT [OPTIONS]                         Resume a saved \
         project"
  in
  Cmd.v info term

let () = Stdlib.exit (Cmdliner.Cmd.eval main_cmd)
