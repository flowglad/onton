open Onton
open Onton_core
open Onton_core.Types
module Managed_repo = Onton.Managed_repo

(** {1 Configuration} *)

type config = {
  project : string option;
  backend : string;
  model : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  main_branch : Branch.t;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  headless : bool;
  user_config : User_config.t;
}

module type STARTUP_RECONCILER = Poller_fiber.STARTUP_RECONCILER

let default_backend = "claude"

let known_backends =
  [ "claude"; "codex"; "opencode"; "pi"; "gemini"; "patch-agent" ]

(** Resolve a CLI [--backend]/[--model] pair (or stored equivalents) into the
    canonical [(backend, model)] tuple used internally. Empty [backend] falls
    back to [default_backend]. An empty [model] is preserved — the backend
    dispatch then omits [--model] from the underlying CLI call so each
    provider's own default applies. *)
let resolve_backend_model ~backend ~model =
  let backend =
    if Base.String.is_empty (Base.String.strip backend) then default_backend
    else Base.String.strip backend
  in
  (backend, Base.String.strip model)

let validate_resolved_config ~backend ~github_token ~github_owner ~github_repo
    ~main_branch ~poll_interval ~max_concurrency =
  let errors =
    Base.List.filter_map
      [
        ( not (Base.List.mem known_backends backend ~equal:String.equal),
          Printf.sprintf "--backend must be one of: %s (got %S)"
            (String.concat ", " known_backends)
            backend );
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
    patch's branch name after the backend session completes. *)

module Pr_registry = struct
  type t = { mutex : Eio.Mutex.t; table : (Patch_id.t, Pr_number.t) Hashtbl.t }

  let create () : t = { mutex = Eio.Mutex.create (); table = Hashtbl.create 64 }

  let register (t : t) ~patch_id ~pr_number =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        Hashtbl.replace t.table patch_id pr_number)

  let find (t : t) ~patch_id =
    Eio.Mutex.use_ro t.mutex (fun () -> Hashtbl.find_opt t.table patch_id)

  let unregister (t : t) ~patch_id =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        Hashtbl.remove t.table patch_id)
end

let build_branch_map (gameplan : Gameplan.t) ~default =
  let map =
    Base.List.fold gameplan.Gameplan.patches
      ~init:(Base.Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) ->
        match Base.Map.add acc ~key:p.Patch.id ~data:p.Patch.branch with
        | `Ok acc -> acc
        | `Duplicate ->
            failwith
              (Printf.sprintf "Duplicate patch id in gameplan: %s"
                 (Patch_id.to_string p.Patch.id)))
  in
  fun pid -> Base.Option.value (Base.Map.find map pid) ~default

let patch_complexity ~(gameplan : Gameplan.t) ~patch_id =
  Base.List.find gameplan.Gameplan.patches ~f:(fun (p : Patch.t) ->
      Patch_id.equal p.Patch.id patch_id)
  |> Base.Option.bind ~f:(fun (p : Patch.t) -> p.Patch.complexity)

let log_event runtime ?patch_id msg =
  Runtime_logging.log_event runtime ?patch_id msg

module type FIBER_ENV = sig
  val runtime : Runtime.t
  val clock : float Eio.Time.clock_ty Eio.Time.clock
  val fs : Eio.Fs.dir_ty Eio.Path.t
  val process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t
  val config : config
  val project_name : string
  val pr_registry : Pr_registry.t
  val findings_registry : Findings_registry.t

  val review_clients :
    (module Review_service_client.S
       with type error = Review_service_client.error)
    list

  val transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t
  val event_log : Event_log.t
  val branch_of : Patch_id.t -> Branch.t

  val pick_backend :
    complexity:int option -> Backend_registry.kind * Backend_routing.decision

  val worktree_mutex : Eio.Mutex.t
  val hook_mutex : Eio.Mutex.t
end

module Make_fibers
    (Forge : Onton.Forge.S with type error = Github.error)
    (W : Worktree.S)
    (Env : FIBER_ENV) =
struct
  module WS_Env : Worktree_setup.ENV = struct
    let runtime = Env.runtime
    let clock = Env.clock
    let fs = Env.fs
    let project_name = Env.project_name
    let user_config = Env.config.user_config
    let worktree_mutex = Env.worktree_mutex
    let hook_mutex = Env.hook_mutex
  end

  module SD_Env : Session_driver.ENV = struct
    let runtime = Env.runtime
    let clock = Env.clock
    let fs = Env.fs
    let project_name = Env.project_name
    let owner = Env.config.github_owner
    let repo = Env.config.github_repo
    let transcripts = Env.transcripts
    let user_config = Env.config.user_config
    let worktree_mutex = Env.worktree_mutex
    let hook_mutex = Env.hook_mutex
  end

  module WS = Worktree_setup.Make (W) (WS_Env)
  module Session_driver = Session_driver.Make (W) (SD_Env)
  module Long_lived_sessions = Long_lived_sessions.Make (Session_driver)

  module Busy_guard_env = struct
    let runtime = Env.runtime
    let event_log = Env.event_log
  end

  module With_busy_guard = With_busy_guard.Make (Busy_guard_env)
  module Worktree_plan_executor = Worktree_plan_executor.Make (W) (WS_Env)

  module Env_poller : Poller_fiber.Poller_env.S = struct
    let runtime = Env.runtime
    let clock = Env.clock
    let fs = Env.fs
    let project_name = Env.project_name
    let user_config = Env.config.user_config
    let worktree_mutex = Env.worktree_mutex
    let hook_mutex = Env.hook_mutex
    let process_mgr = Env.process_mgr
    let github_owner = Env.config.github_owner
    let github_repo = Env.config.github_repo
    let main_branch = Env.config.main_branch
    let poll_interval = Env.config.poll_interval
    let repo_root = Env.config.repo_root
    let find_pr_number = Pr_registry.find Env.pr_registry
    let register_pr_number = Pr_registry.register Env.pr_registry
    let unregister_pr_number = Pr_registry.unregister Env.pr_registry
    let findings_registry = Env.findings_registry
    let review_clients = Env.review_clients
    let event_log = Env.event_log
    let branch_of = Env.branch_of
  end

  module Poller = Poller_fiber.Make (Forge) (W) (Env_poller)

  module Headless_Env : Headless_fiber.Headless_env.S = struct
    let runtime = Env.runtime
    let clock = Env.clock
  end

  module Persistence_Env : Persistence_fiber.Persistence_env.S = struct
    let runtime = Env.runtime
    let clock = Env.clock
    let project_name = Env.project_name
  end

  module Headless = Headless_fiber.Make (Headless_Env)
  module Persistence_fiber_runner = Persistence_fiber.Make (Persistence_Env)

  (** Execute declarative GitHub effects and record successful observations back
      into durable state. *)
  let execute_github_effects ~runtime effects =
    Base.List.iter effects ~f:(fun github_effect ->
        let label =
          match github_effect with
          | Patch_controller.Set_pr_draft { pr_number; draft; _ } ->
              Printf.sprintf "set_pr_draft (PR #%d, draft=%b)"
                (Pr_number.to_int pr_number)
                draft
          | Patch_controller.Set_pr_base { pr_number; base; _ } ->
              Printf.sprintf "set_pr_base (PR #%d, base=%s)"
                (Pr_number.to_int pr_number)
                (Branch.to_string base)
        in
        try
          let result =
            match github_effect with
            | Patch_controller.Set_pr_draft { patch_id = _; pr_number; draft }
              ->
                Forge.set_draft ~pr_number ~draft
            | Patch_controller.Set_pr_base { patch_id = _; pr_number; base } ->
                Forge.update_pr_base ~pr_number ~base
          in
          match result with
          | Ok () ->
              Runtime.update_orchestrator runtime (fun orch ->
                  Patch_controller.apply_github_effect_success orch
                    github_effect)
          | Error err ->
              Printf.eprintf "%s failed: %s\n%!" label (Forge.show_error err)
        with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn ->
            Printf.eprintf "%s crashed: %s\n%!" label (Printexc.to_string exn))

  (** {1 Activity log helpers} *)

  (** Merge events and transitions from an activity log into a single
      timestamp-tagged list. [compare] controls sort direction. *)
  let merged_log_entries ~(log : Activity_log.t) ~limit ~compare
      ~(map_event : Activity_log.Event.t -> 'a)
      ~(map_transition : Activity_log.Transition_entry.t -> 'a)
      ~(map_stream : Activity_log.Stream_entry.t -> 'a) =
    let events =
      Base.List.map (Activity_log.recent_events log ~limit) ~f:(fun e ->
          (e.Activity_log.Event.timestamp, map_event e))
    in
    let transitions =
      Base.List.map (Activity_log.recent_transitions log ~limit) ~f:(fun t ->
          (t.Activity_log.Transition_entry.timestamp, map_transition t))
    in
    let stream =
      Base.List.map (Activity_log.recent_stream_entries log ~limit) ~f:(fun s ->
          (s.Activity_log.Stream_entry.timestamp, map_stream s))
    in
    Base.List.sort (events @ transitions @ stream) ~compare

  let format_stream_kind (kind : Activity_log.Stream_entry.kind) =
    match kind with
    | Activity_log.Stream_entry.Tool_use (name, input) ->
        if String.length input > 0 then Printf.sprintf "Tool %s — %s" name input
        else Printf.sprintf "Tool %s" name
    | Activity_log.Stream_entry.Text_chunk text -> text
    | Activity_log.Stream_entry.Finished reason ->
        Printf.sprintf "Finished — %s" reason
    | Activity_log.Stream_entry.Stream_error msg ->
        Printf.sprintf "Stream error — %s" msg

  let activity_entries_of_log ?(limit = 10) (log : Activity_log.t) =
    merged_log_entries ~log ~limit
      ~compare:(fun (t1, _) (t2, _) -> Base.Float.descending t1 t2)
      ~map_event:(fun (e : Activity_log.Event.t) ->
        Tui.Event
          {
            patch_id =
              Base.Option.map e.Activity_log.Event.patch_id
                ~f:Patch_id.to_string;
            message = e.Activity_log.Event.message;
            timestamp = e.Activity_log.Event.timestamp;
          })
      ~map_transition:(fun (t : Activity_log.Transition_entry.t) ->
        Tui.Transition
          {
            patch_id =
              Patch_id.to_string t.Activity_log.Transition_entry.patch_id;
            from_label = Tui.label t.Activity_log.Transition_entry.from_status;
            to_status = t.Activity_log.Transition_entry.to_status;
            to_label = Tui.label t.Activity_log.Transition_entry.to_status;
            action = t.Activity_log.Transition_entry.action;
            timestamp = t.Activity_log.Transition_entry.timestamp;
          })
      ~map_stream:(fun (s : Activity_log.Stream_entry.t) ->
        Tui.Event
          {
            patch_id =
              Some (Patch_id.to_string s.Activity_log.Stream_entry.patch_id);
            message = format_stream_kind s.Activity_log.Stream_entry.kind;
            timestamp = s.Activity_log.Stream_entry.timestamp;
          })
    |> Base.List.map ~f:snd

  (** {1 Shared helpers} *)

  (** Pluralize a count for inline rendering: [pluralize 1 "comment"] →
      ["1 comment"], [pluralize 2 "comment"] → ["2 comments"]. Pass [~plural]
      when the plural is irregular. *)
  let pluralize ?plural n singular =
    let many = match plural with Some p -> p | None -> singular ^ "s" in
    Printf.sprintf "%d %s" n (if n = 1 then singular else many)

  (** Reconcile per-patch automerge deadlines. For each deadline that has
      elapsed, merge the PR on GitHub and mark the patch merged on success. On
      failure the failure counter is incremented and the deadline pushed out by
      a fresh idle window, so the retry is at least 5 minutes out regardless of
      how often the runner tick fires. Retries continue until the consecutive
      failure cap is reached, at which point reconciliation stops issuing merge
      calls until the user toggles automerge off/on. *)
  let reconcile_and_execute_automerge ~runtime =
    let now = Unix.gettimeofday () in
    let decisions =
      Runtime.update_orchestrator_returning runtime (fun orch ->
          Patch_controller.reconcile_automerge orch ~now)
    in
    (* Dispatch concurrently with a bounded fiber pool. A slow merge call can
     take up to GitHub's request timeout (~30s), so serial iteration would
     compound that over [N] decisions.

     This call still blocks the enclosing [amloop] fiber until all in-flight
     merges return, which means under simultaneous slow responses the 1s
     automerge cadence can degrade to one-per-timeout. That trade-off is
     intentional: bounded concurrency avoids a thundering herd against GitHub
     rate limits and keeps exactly one automerge fiber alive on the switch
     (the runner loop is already decoupled — it does not wait on this). The
     1s cadence is not load-bearing either: deadlines fire on 5-minute idle
     windows, and [automerge_inflight] already prevents double-claiming. *)
    Eio.Fiber.List.iter ~max_fibers:4
      (fun Patch_controller.
             { merge_patch_id = patch_id; merge_pr_number = pr_number } ->
        let label =
          Printf.sprintf "automerge PR #%d" (Pr_number.to_int pr_number)
        in
        (* [reconcile_automerge] set [automerge_inflight = true] when it emitted
         this decision. Every exit path below must either call
         [apply_automerge_success]/[apply_automerge_failure] (both of which
         clear the inflight flag) or run the [Fun.protect] finaliser. *)
        let inflight_cleared = ref false in
        let clear_inflight_if_needed () =
          if not !inflight_cleared then (
            inflight_cleared := true;
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.set_automerge_inflight orch patch_id false))
        in
        (* Push the deadline out by one idle window after a non-terminal response
         (GitHub queued the merge or responded with an unrecognised shape) and
         clear [inflight] in the same orchestrator update. The deadline that
         fired this decision is already in the past, and merely clearing
         [inflight] would make the patch eligible again on the very next
         reconcile tick — producing a tight loop of PUT /merge calls until the
         poller observes a terminal state. A fresh idle window gives the
         poller time to catch up. Doing both writes atomically avoids a brief
         window where a concurrent read sees the pushed-out deadline with
         [inflight = true]. *)
        let push_deadline_and_clear_inflight () =
          if not !inflight_cleared then (
            inflight_cleared := true;
            let now_ts = Unix.gettimeofday () in
            Runtime.update_orchestrator runtime (fun orch ->
                let orch =
                  Orchestrator.set_automerge_inflight orch patch_id false
                in
                Orchestrator.set_automerge_deadline orch patch_id
                  (now_ts +. Patch_controller.automerge_idle_timeout)))
        in
        Fun.protect ~finally:clear_inflight_if_needed (fun () ->
            (* Re-read the patch just before hitting GitHub. The original
             decision came from an earlier orchestrator snapshot; between then
             and now the patch may have been merged, lost [merge_ready], gone
             busy again, or had its failure cap hit (via a parallel tick). Any
             of these make the call both unnecessary and noisy (GitHub 405).
             We call [is_automerge_candidate] with [~ignore_inflight:true]
             because we ourselves set the inflight flag when claiming this
             decision — the default [ignore_inflight:false] would see our own
             flag and short-circuit every merge. *)
            let still_candidate =
              Runtime.read runtime (fun snap ->
                  match
                    Orchestrator.find_agent snap.Runtime.orchestrator patch_id
                  with
                  | None -> false
                  | Some agent ->
                      let main_branch =
                        Orchestrator.main_branch snap.Runtime.orchestrator
                      in
                      (* Verify the agent's current PR still matches the one this
                       decision was emitted for. If the poller has remapped
                       the patch to a replacement PR between reconcile and
                       execute, hitting GitHub with the stale [pr_number]
                       would either merge the wrong PR (on the rare chance
                       the old PR is still open) or 405 and bump
                       [automerge_failure_count] for no reason.
                       [is_automerge_candidate] gates on everything else (not
                       merged, automerge enabled, approval, CI, empty queue,
                       failure cap) and [~ignore_inflight:true] opts out of
                       the inflight short-circuit that the predicate applies
                       by default — necessary here because this re-check runs
                       while we hold the flag. *)
                      (match agent.Patch_agent.pr_number with
                        | Some current -> Pr_number.equal current pr_number
                        | None -> false)
                      && Patch_controller.is_automerge_candidate
                           ~ignore_inflight:true agent ~main_branch)
            in
            if not still_candidate then (
              log_event runtime ~patch_id
                (Printf.sprintf "%s skipped — no longer a candidate" label);
              (* Clear inflight here explicitly (rather than relying on the
               [Fun.protect] finaliser) and deliberately leave the deadline in
               place — the next [reconcile_automerge] tick will clear it via
               the [(false, Some _)] branch if candidacy is genuinely lost,
               or re-arm it if the patch became a candidate again. Keeping
               deadline-clearing centralised in reconcile avoids a redundant
               write here and the divergence risk of two call sites managing
               the same invariant. *)
              clear_inflight_if_needed ())
            else
              try
                match Forge.merge_pr ~pr_number ~merge_method:`Squash with
                | Ok Forge.Merge_succeeded ->
                    inflight_cleared := true;
                    Runtime.update_orchestrator runtime (fun orch ->
                        Patch_controller.apply_automerge_success orch patch_id);
                    log_event runtime ~patch_id
                      (Printf.sprintf "Automerge complete — PR #%d merged"
                         (Pr_number.to_int pr_number))
                | Ok (Forge.Merge_queued msg) ->
                    (* GitHub accepted the request into its native auto-merge
                     queue. Not a failure — don't bump the counter. Push the
                     deadline forward (atomically with clearing [inflight]) so
                     we don't re-fire before the poller observes the eventual
                     merge. *)
                    push_deadline_and_clear_inflight ();
                    log_event runtime ~patch_id
                      (Printf.sprintf
                         "Automerge queued by GitHub — awaiting checks (%s)" msg)
                | Ok Forge.Merge_unconfirmed ->
                    (* 2xx response with an unexpected shape. Not authoritative
                     either way — let the poller confirm via PR state rather
                     than guess. Don't count as failure, but push the
                     deadline forward (atomic with clearing [inflight]) so we
                     don't retry every tick. *)
                    push_deadline_and_clear_inflight ();
                    log_event runtime ~patch_id
                      (Printf.sprintf
                         "%s accepted but merge not confirmed — awaiting poll"
                         label)
                | Error err ->
                    inflight_cleared := true;
                    Runtime.update_orchestrator runtime (fun orch ->
                        Patch_controller.apply_automerge_failure orch
                          ~now:(Unix.gettimeofday ()) patch_id);
                    log_event runtime ~patch_id
                      (Printf.sprintf "Automerge failed — %s"
                         (Forge.show_error err))
              with
              | Eio.Cancel.Cancelled _ as exn -> raise exn
              | exn ->
                  inflight_cleared := true;
                  Runtime.update_orchestrator runtime (fun orch ->
                      Patch_controller.apply_automerge_failure orch
                        ~now:(Unix.gettimeofday ()) patch_id);
                  log_event runtime ~patch_id
                    (Printf.sprintf "%s crashed — %s" label
                       (Printexc.to_string exn))))
      decisions

  let read_optional_file path =
    try
      if Stdlib.Sys.file_exists path then
        let ic = Stdlib.In_channel.open_text path in
        Stdlib.Fun.protect
          ~finally:(fun () -> Stdlib.In_channel.close ic)
          (fun () -> Some (Stdlib.In_channel.input_all ic))
      else None
    with _ -> None

  (** Read an artifact file. Returns [Some contents] if the file exists and is
      readable, [None] otherwise. *)
  let read_artifact_file = read_optional_file

  (** Apply the agent-authored notes artifact to the PR. Composes the final body
      as: gameplan description + specs + Implementation Notes (from artifact).
      Returns a tag describing the outcome so the caller can correlate with
      session-level signals (e.g. a Missing artifact alongside a failed Write
      tool call indicates the agent was blocked mid-call, not that it chose to
      skip notes). *)
  let apply_pr_body_artifact ~runtime ~project_name ~patch_id ~pr_number ~patch
      ~gameplan : [ `Ok | `Missing | `Empty | `Patch_failed ] =
    let artifact_path =
      Project_store.pr_body_artifact_path ~project_name ~patch_id
    in
    match read_artifact_file artifact_path with
    | None ->
        log_event runtime ~patch_id
          (Printf.sprintf
             "pr-body: artifact missing at %s; keeping initial PR body"
             artifact_path);
        `Missing
    | Some notes when String.length (String.trim notes) = 0 ->
        log_event runtime ~patch_id
          "pr-body: artifact empty; keeping initial PR body";
        `Empty
    | Some notes -> (
        let description =
          Prompt.render_pr_description ~project_name patch gameplan
        in
        let spec_suffix = Prompt.render_spec_suffix patch gameplan in
        let body =
          Printf.sprintf "%s%s\n\n## Implementation Notes\n\n%s" description
            spec_suffix (String.trim notes)
        in
        match Forge.update_pr_body ~pr_number ~body with
        | Ok () ->
            log_event runtime ~patch_id
              (Printf.sprintf "pr-body: PATCHed PR #%d"
                 (Pr_number.to_int pr_number));
            `Ok
        | Error e ->
            log_event runtime ~patch_id
              (Printf.sprintf "pr-body: PATCH failed — %s" (Forge.show_error e));
            `Patch_failed)

  (** Terminal failure — forces [Given_up] so [complete] raises intervention.
      Used for non-retryable errors (patch not found, PR discovery failed).

      Routes through the pure [Orchestrator.apply_force_complete] decision so
      any inflight human messages are restored to the inbox rather than silently
      dropped, and emits a [log_force_complete] audit event so a dispatched
      [Respond] action always has a matching close in the JSONL log. *)
  let mark_session_failed event_log runtime patch_id =
    let snapshot = ref None in
    Runtime.update_orchestrator runtime (fun orch ->
        match Orchestrator.find_agent orch patch_id with
        | None -> orch
        | Some before ->
            let orch' =
              Orchestrator.apply_force_complete orch patch_id
                Orchestrator.Unexpected_exception
            in
            let after = Orchestrator.agent orch' patch_id in
            snapshot := Some (before, after);
            orch');
    Base.Option.iter !snapshot ~f:(fun (before, after) ->
        (* Only emit a force-complete audit event if the agent was actually
         busy. The "patch not found in gameplan" call site at the Start
         dispatch fires this before [with_busy_guard] runs, so no Respond
         was ever dispatched — logging a force-complete close in that case
         would falsely claim a Respond/close pair. *)
        if before.Patch_agent.busy then
          Event_log.log_force_complete event_log ~patch_id
            ~reason:Orchestrator.Unexpected_exception ~agent_before:before
            ~agent_after:after)

  (** {1 Fibers} *)

  exception Quit_tui
  (** Raised by the input fiber to signal a clean exit. *)

  (** Build a map from patch_id to the most recent event message for patches in
      needs_intervention. Scans the full event list so the reason survives even
      after the truncated activity feed has aged out the triggering entry. *)
  let intervention_reasons_of_log (log : Activity_log.t)
      ~(orchestrator : Orchestrator.t) =
    let agents = Orchestrator.all_agents orchestrator in
    let needs =
      Base.List.filter_map agents ~f:(fun (a : Patch_agent.t) ->
          if Patch_agent.needs_intervention a || a.Patch_agent.branch_blocked
          then Some a.Patch_agent.patch_id
          else None)
      |> Base.Hash_set.of_list (module Patch_id)
    in
    if Base.Hash_set.is_empty needs then Base.Map.Poly.empty
    else
      let events = Activity_log.recent_events log ~limit:1000 in
      Base.List.fold events ~init:Base.Map.Poly.empty ~f:(fun acc e ->
          match e.Activity_log.Event.patch_id with
          | Some pid when Base.Hash_set.mem needs pid ->
              if Base.Map.Poly.mem acc pid then acc
              else
                Base.Map.Poly.set acc ~key:pid
                  ~data:e.Activity_log.Event.message
          | _ -> acc)

  (** TUI rendering fiber — redraws the terminal at ~10 fps.

      [list_selected], [detail_scroll], [timeline_scroll], and [view_mode] are
      shared mutable refs updated by the input fiber. *)
  let tui_fiber ~runtime ~clock ~stdout ~tui_state ~transcripts ~backend_name
      ~resolve_routing =
    let {
      Tui_state.list_selected;
      detail_scroll;
      detail_follow;
      timeline_scroll;
      view_mode;
      sorted_patch_ids;
      input_mode;
      prompt_line;
      show_help;
      status_msg;
      patches_start_row;
      patches_scroll_offset;
      patches_visible_count;
      _;
    } =
      tui_state
    in
    Eio.Flow.copy_string (Tui.enter_tui ()) stdout;
    let first = ref true in
    let prev_output = ref "" in
    let rec loop () =
      (* Skip sleep on first iteration and after SIGCONT resume *)
      if !first then first := false
      else if Atomic.exchange Term.Raw.redraw_needed false then ()
      else Eio.Time.sleep clock 0.1;
      let orch, gp, log =
        Runtime.read runtime (fun snap ->
            ( snap.Runtime.orchestrator,
              snap.Runtime.gameplan,
              snap.Runtime.activity_log ))
      in
      let size = Term.get_size () in
      let width = match size with Some s -> s.Term.cols | None -> 80 in
      let height = match size with Some s -> s.Term.rows | None -> 24 in
      let limit =
        match !view_mode with
        | Tui.Timeline_view -> 100
        | Tui.Detail_view _ -> 100
        | Tui.List_view -> 10
      in
      let activity = activity_entries_of_log ~limit log in
      let intervention_reasons =
        intervention_reasons_of_log log ~orchestrator:orch
      in
      let views =
        Tui.views_of_orchestrator ~orchestrator:orch ~gameplan:gp ~activity
          ~resolve_routing ~intervention_reasons ()
      in
      sorted_patch_ids :=
        Base.List.map views ~f:(fun (pv : Tui.patch_view) -> pv.Tui.patch_id);
      let transcript =
        match !view_mode with
        | Tui.Detail_view pid -> (
            match Hashtbl.find_opt transcripts pid with
            | Some t -> t
            | None -> "")
        | Tui.List_view | Tui.Timeline_view -> ""
      in
      (* Expire status messages *)
      let now = Unix.gettimeofday () in
      (match !status_msg with
      | Some msg when Tui.msg_expired ~now msg -> status_msg := None
      | Some _ | None -> ());
      let scroll_offset =
        match !view_mode with
        | Tui.Detail_view _ ->
            if !detail_follow then Base.Int.max_value else !detail_scroll
        | Tui.Timeline_view -> !timeline_scroll
        | Tui.List_view -> 0
      in
      let frame =
        Tui.render_frame ~width ~height ~selected:!list_selected ~scroll_offset
          ~view_mode:!view_mode ~activity ~project_name:gp.Gameplan.project_name
          ~backend_name ~show_help:!show_help
          ~show_manage:
            (Tui_input.equal_input_mode !input_mode Tui_input.Manage_patch)
          ~now:(Unix.gettimeofday ()) ~transcript ?status_msg:!status_msg
          ?prompt_line:!prompt_line views
      in
      (* Write back the clamped scroll offset so delta-based input in
       input_fiber works from a real value, not a sentinel like max_value.
       Re-engage auto-follow if the view ended up at the bottom. *)
      detail_scroll := Tui.detail_scroll_offset frame;
      if Tui.detail_at_bottom frame then detail_follow := true;
      patches_start_row := Tui.patches_start_row frame;
      patches_scroll_offset := Tui.patches_scroll_offset frame;
      patches_visible_count := Tui.patch_count frame;
      let output = Tui.paint_frame frame in
      if not (String.equal output !prev_output) then (
        Eio.Flow.copy_string output stdout;
        prev_output := output);
      loop ()
    in
    loop ()

  (** Input fiber — reads keypresses and dispatches TUI commands.

      Supports two modes:
      - Normal mode: single-key navigation and direct actions
      - Prompt mode: purpose-specific mini-prompts for PR numbers, worktree
        paths, and messages *)

  (** Normalize pasted text for single-line input: strip trailing newlines,
      replace internal newlines/carriage returns with spaces. *)
  let normalize_paste text =
    let text =
      Base.String.rstrip text ~drop:(fun c ->
          Char.equal c '\n' || Char.equal c '\r')
    in
    let text = Base.String.tr text ~target:'\n' ~replacement:' ' in
    Base.String.tr text ~target:'\r' ~replacement:' '

  let input_fiber ~runtime ~process_mgr ~tui_state ~pr_registry ~project_name
      ~owner ~repo ~resolve_routing =
    let {
      Tui_state.list_selected;
      detail_scroll;
      detail_follow;
      timeline_scroll;
      detail_scrolls;
      view_mode;
      sorted_patch_ids;
      input_mode;
      prompt_line;
      show_help;
      status_msg;
      patches_start_row;
      patches_scroll_offset;
      patches_visible_count;
    } =
      tui_state
    in
    let buf = Tui_input.Edit_buffer.create () in
    let selected_pid () =
      let pids = !sorted_patch_ids in
      let count = Base.List.length pids in
      if count = 0 || !list_selected < 0 then None
      else
        let idx = Base.Int.max 0 (Base.Int.min !list_selected (count - 1)) in
        Some (Base.List.nth_exn pids idx)
    in
    let sync_input () =
      match !input_mode with
      | Tui_input.Normal | Tui_input.Manage_patch -> prompt_line := None
      | Tui_input.Prompt_pr | Tui_input.Prompt_worktree
      | Tui_input.Prompt_message | Tui_input.Prompt_broadcast ->
          let prefix = Tui_input.prompt_prefix !input_mode in
          let contents = Tui_input.Edit_buffer.contents buf in
          let cursor_col =
            Term.visible_length prefix
            + Term.visible_length
                (String.sub contents 0 (Tui_input.Edit_buffer.cursor buf))
          in
          prompt_line :=
            Some { Tui.prompt_text = prefix ^ contents; cursor_col };
          Atomic.set Term.Raw.redraw_needed true
    in
    let history = Tui_input.History.create () in
    let saved_draft = ref "" in
    let eof_count = ref 0 in
    let last_click_time = ref 0.0 in
    let last_click_row = ref (-1) in
    let rec loop () =
      sync_input ();
      match Term.Key_io.read () with
      | None ->
          (* Transient EOF can happen if a child process (e.g. script/gh)
           briefly interferes with the terminal. Retry a few times before
           giving up. *)
          eof_count := !eof_count + 1;
          if !eof_count >= 10 then
            log_event runtime
              "Stdin closed — input fiber giving up after 10 consecutive EOFs"
          else (
            Eio.Fiber.yield ();
            loop ())
      | Some key -> (
          eof_count := 0;
          if !show_help then (
            show_help := false;
            loop ())
          else if Tui_input.equal_input_mode !input_mode Tui_input.Manage_patch
          then (
            (match key with
            | Term.Key.Escape -> input_mode := Tui_input.Normal
            | Term.Key.Char 'm' -> (
                input_mode := Tui_input.Normal;
                let target_patch_id =
                  match !view_mode with
                  | Tui.Detail_view patch_id -> Some patch_id
                  | Tui.List_view -> selected_pid ()
                  | Tui.Timeline_view -> None
                in
                match target_patch_id with
                | Some patch_id ->
                    let busy, has_pr =
                      Runtime.read runtime (fun snap ->
                          let agent =
                            Orchestrator.agent snap.Runtime.orchestrator
                              patch_id
                          in
                          (agent.Patch_agent.busy, Patch_agent.has_pr agent))
                    in
                    if busy then
                      log_event runtime ~patch_id
                        "Cannot force-mark as merged — patch is currently busy"
                    else if not has_pr then
                      log_event runtime ~patch_id
                        "Cannot force-mark as merged — patch has no PR"
                    else (
                      Runtime.update_orchestrator runtime (fun orch ->
                          Orchestrator.mark_merged orch patch_id);
                      log_event runtime ~patch_id "Force-marked as merged")
                | None -> ())
            | Term.Key.Char 'a' -> (
                input_mode := Tui_input.Normal;
                let target_patch_id =
                  match !view_mode with
                  | Tui.Detail_view patch_id -> Some patch_id
                  | Tui.List_view -> selected_pid ()
                  | Tui.Timeline_view -> None
                in
                match target_patch_id with
                | Some patch_id -> (
                    let enabled_after =
                      Runtime.update_orchestrator_returning runtime (fun orch ->
                          match Orchestrator.find_agent orch patch_id with
                          | None -> (orch, None)
                          | Some agent ->
                              let v = not agent.Patch_agent.automerge_enabled in
                              let orch =
                                Orchestrator.set_automerge_enabled orch patch_id
                                  v
                              in
                              (orch, Some v))
                    in
                    match enabled_after with
                    | Some true ->
                        log_event runtime ~patch_id "Automerge enabled"
                    | Some false ->
                        log_event runtime ~patch_id "Automerge disabled"
                    | None -> ())
                | None -> ())
            | Term.Key.Char _ | Term.Key.Enter | Term.Key.Tab | Term.Key.Paste _
            | Term.Key.Backspace | Term.Key.Up | Term.Key.Down | Term.Key.Left
            | Term.Key.Right | Term.Key.Home | Term.Key.End | Term.Key.Page_up
            | Term.Key.Page_down | Term.Key.Delete | Term.Key.F _
            | Term.Key.Ctrl _ | Term.Key.Mouse _ | Term.Key.Unknown _ ->
                ());
            loop ())
          else if not (Tui_input.equal_input_mode !input_mode Tui_input.Normal)
          then
            match key with
            | Term.Key.Paste text ->
                Tui_input.Edit_buffer.insert_string buf (normalize_paste text);
                loop ()
            | Term.Key.Escape ->
                Tui_input.Edit_buffer.clear buf;
                saved_draft := "";
                Tui_input.History.reset_browse history;
                input_mode := Tui_input.Normal;
                loop ()
            | Term.Key.Enter ->
                let line = Tui_input.Edit_buffer.contents buf in
                Tui_input.History.push history line;
                Tui_input.History.reset_browse history;
                let mode = !input_mode in
                Tui_input.Edit_buffer.clear buf;
                saved_draft := "";
                input_mode := Tui_input.Normal;
                let line = Base.String.strip line in
                (match mode with
                | Tui_input.Prompt_message -> (
                    if not (Base.String.is_empty line) then
                      match !view_mode with
                      | Tui.Detail_view patch_id ->
                          let patch_exists =
                            Runtime.read runtime (fun snap ->
                                Base.Map.mem
                                  (Orchestrator.agents_map
                                     snap.Runtime.orchestrator)
                                  patch_id)
                          in
                          if patch_exists then (
                            Runtime.update_orchestrator runtime (fun orch ->
                                Orchestrator.send_human_message orch patch_id
                                  line);
                            log_event runtime ~patch_id
                              (Printf.sprintf "Sent human message — %s" line))
                          else
                            log_event runtime
                              (Printf.sprintf
                                 "Cannot send human message — unknown patch %s"
                                 (Patch_id.to_string patch_id))
                      | Tui.List_view | Tui.Timeline_view -> ())
                | Tui_input.Prompt_broadcast ->
                    if not (Base.String.is_empty line) then begin
                      let views =
                        Runtime.read runtime (fun snap ->
                            Tui.views_of_orchestrator
                              ~orchestrator:snap.Runtime.orchestrator
                              ~gameplan:snap.Runtime.gameplan ~activity:[]
                              ~resolve_routing ())
                      in
                      let active =
                        Base.List.filter views ~f:(fun (pv : Tui.patch_view) ->
                            (not
                               (Tui.equal_display_status pv.Tui.status
                                  Tui.Merged))
                            && (not
                                  (Tui.equal_display_status pv.Tui.status
                                     Tui.Pending))
                            && (not
                                  (Tui.equal_display_status pv.Tui.status
                                     Tui.Needs_help))
                            && not
                                 (Tui.equal_display_status pv.Tui.status
                                    Tui.Blocked_by_dep))
                      in
                      let count = Base.List.length active in
                      Base.List.iter active ~f:(fun (pv : Tui.patch_view) ->
                          Runtime.update_orchestrator runtime (fun orch ->
                              Orchestrator.send_human_message orch
                                pv.Tui.patch_id line));
                      log_event runtime
                        (Printf.sprintf "Broadcast to %s — %s"
                           (pluralize count "active patch"
                              ~plural:"active patches")
                           line)
                    end
                | Tui_input.Prompt_pr -> (
                    match Base.Int.of_string_opt line with
                    | Some n when n > 0 ->
                        let pr_number = Pr_number.of_int n in
                        let patch_id = Patch_id.of_string (Int.to_string n) in
                        let already_exists =
                          Runtime.read runtime (fun snap ->
                              Base.Option.is_some
                                (Orchestrator.find_agent
                                   snap.Runtime.orchestrator patch_id))
                        in
                        if already_exists then
                          log_event runtime ~patch_id
                            (Printf.sprintf "Ad-hoc PR #%d already registered" n)
                        else (
                          status_msg :=
                            Some
                              {
                                Tui.level = Tui.Info;
                                text = Printf.sprintf "Fetching PR #%d…" n;
                                expires_at = None;
                              };
                          match Forge.pr_state pr_number with
                          | Error err ->
                              status_msg := None;
                              log_event runtime ~patch_id
                                (Printf.sprintf "Cannot add ad-hoc PR #%d — %s"
                                   n (Forge.show_error err))
                          | Ok pr_state when Pr_state.is_fork pr_state ->
                              status_msg := None;
                              log_event runtime ~patch_id
                                (Printf.sprintf
                                   "Cannot add ad-hoc PR #%d — fork PRs not \
                                    supported"
                                   n)
                          | Ok pr_state -> (
                              status_msg := None;
                              match pr_state.Pr_state.head_branch with
                              | None ->
                                  log_event runtime ~patch_id
                                    (Printf.sprintf
                                       "Cannot add ad-hoc PR #%d — no head \
                                        branch"
                                       n)
                              | Some branch ->
                                  Pr_registry.register pr_registry ~patch_id
                                    ~pr_number;
                                  Runtime.update_orchestrator runtime
                                    (fun orch ->
                                      let base_branch =
                                        Option.value
                                          pr_state.Pr_state.base_branch
                                          ~default:
                                            (Orchestrator.main_branch orch)
                                      in
                                      Orchestrator.add_agent orch ~patch_id
                                        ~branch ~base_branch ~pr_number);
                                  log_event runtime ~patch_id
                                    (Printf.sprintf "Ad-hoc PR #%d added (%s)" n
                                       (Branch.to_string branch))))
                    | _ ->
                        if not (Base.String.is_empty line) then
                          log_event runtime
                            (Printf.sprintf "Invalid PR number — %s" line))
                | Tui_input.Prompt_worktree -> (
                    if not (Base.String.is_empty line) then
                      let path = line in
                      match selected_pid () with
                      | None ->
                          log_event runtime
                            "Cannot add worktree — no selectable patch"
                      | Some patch_id -> (
                          let busy =
                            Runtime.read runtime (fun snap ->
                                (Orchestrator.agent snap.Runtime.orchestrator
                                   patch_id)
                                  .Patch_agent.busy)
                          in
                          if busy then
                            log_event runtime ~patch_id
                              "Warning — patch is currently running, changing \
                               worktree may affect the live session";
                          let expected =
                            Worktree.worktree_dir ~project_name ~patch_id
                          in
                          try
                            let raw_path = Worktree.normalize_path path in
                            if not (Stdlib.Sys.file_exists raw_path) then
                              failwith ("Worktree path not found: " ^ raw_path);
                            if not (Stdlib.Sys.is_directory raw_path) then
                              failwith
                                ("Worktree path is not a directory: " ^ raw_path);
                            let canonical_real = Unix.realpath raw_path in
                            let git_file =
                              Stdlib.Filename.concat raw_path ".git"
                            in
                            if
                              (not (Stdlib.Sys.file_exists git_file))
                              || Stdlib.Sys.is_directory git_file
                            then
                              failwith
                                ("Path is not a git worktree (no .git file): "
                               ^ raw_path);
                            let canonical_expected =
                              try Unix.realpath expected
                              with Unix.Unix_error (Unix.ENOENT, _, _) ->
                                expected
                            in
                            if
                              not
                                (String.equal canonical_real canonical_expected)
                            then (
                              let parent = Stdlib.Filename.dirname expected in
                              (try Unix.mkdir parent 0o755 with
                              | Unix.Unix_error (Unix.ENOENT, _, _) ->
                                  failwith
                                    ("Cannot create parent directory: " ^ parent)
                              | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
                              (match Unix.lstat expected with
                              | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                                  ()
                              | { Unix.st_kind = Unix.S_LNK; _ } ->
                                  Unix.unlink expected
                              | {
                               Unix.st_kind =
                                 ( Unix.S_REG | Unix.S_DIR | Unix.S_CHR
                                 | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK );
                               _;
                              } ->
                                  failwith
                                    (Printf.sprintf
                                       "Cannot overwrite non-symlink at %s"
                                       expected));
                              Unix.symlink canonical_real expected);
                            Runtime.update_orchestrator runtime (fun orch ->
                                let orch =
                                  Orchestrator.reset_intervention_state orch
                                    patch_id
                                in
                                Orchestrator.set_worktree_path orch patch_id
                                  canonical_real);
                            status_msg := None;
                            if String.equal canonical_real canonical_expected
                            then
                              log_event runtime ~patch_id
                                (Printf.sprintf
                                   "Worktree already at expected path %s"
                                   canonical_real)
                            else
                              log_event runtime ~patch_id
                                (Printf.sprintf
                                   "Registered worktree — symlinked %s → %s"
                                   expected canonical_real)
                          with
                          | Failure msg ->
                              status_msg :=
                                Some
                                  {
                                    Tui.level = Tui.Error;
                                    text = msg;
                                    expires_at = None;
                                  };
                              log_event runtime ~patch_id
                                (Printf.sprintf "Failed to add worktree — %s"
                                   msg)
                          | exn ->
                              let msg = Printexc.to_string exn in
                              status_msg :=
                                Some
                                  {
                                    Tui.level = Tui.Error;
                                    text =
                                      Printf.sprintf
                                        "Failed to add worktree: %s" msg;
                                    expires_at = None;
                                  };
                              log_event runtime ~patch_id
                                (Printf.sprintf "Failed to add worktree — %s"
                                   msg)))
                | Tui_input.Normal | Tui_input.Manage_patch -> ());
                loop ()
            | Term.Key.Backspace ->
                Tui_input.Edit_buffer.delete_before buf;
                loop ()
            | Term.Key.Char c ->
                Tui_input.Edit_buffer.insert_char buf c;
                loop ()
            | Term.Key.Ctrl 'z' ->
                Term.Raw.suspend ();
                loop ()
            | Term.Key.Up ->
                let was_browsing = Tui_input.History.is_browsing history in
                (match Tui_input.History.older history with
                | Some s ->
                    if not was_browsing then
                      saved_draft := Tui_input.Edit_buffer.contents buf;
                    Tui_input.Edit_buffer.set buf s
                | None -> ());
                loop ()
            | Term.Key.Down ->
                (if Tui_input.History.is_browsing history then
                   match Tui_input.History.newer history with
                   | Tui_input.History.Entry s ->
                       Tui_input.Edit_buffer.set buf s
                   | Tui_input.History.At_fresh ->
                       Tui_input.Edit_buffer.set buf !saved_draft);
                loop ()
            | Term.Key.Left ->
                Tui_input.Edit_buffer.move_left buf;
                loop ()
            | Term.Key.Right ->
                Tui_input.Edit_buffer.move_right buf;
                loop ()
            | Term.Key.Home ->
                Tui_input.Edit_buffer.move_home buf;
                loop ()
            | Term.Key.End ->
                Tui_input.Edit_buffer.move_end buf;
                loop ()
            | Term.Key.Delete ->
                Tui_input.Edit_buffer.delete_at buf;
                loop ()
            | Term.Key.Ctrl 'a' ->
                Tui_input.Edit_buffer.move_home buf;
                loop ()
            | Term.Key.Ctrl 'e' ->
                Tui_input.Edit_buffer.move_end buf;
                loop ()
            | Term.Key.Ctrl 'k' ->
                ignore (Tui_input.Edit_buffer.kill_to_end buf);
                loop ()
            | Term.Key.Ctrl 'u' ->
                ignore (Tui_input.Edit_buffer.kill_to_start buf);
                loop ()
            | Term.Key.Tab | Term.Key.Page_up | Term.Key.Page_down
            | Term.Key.F _ | Term.Key.Ctrl _ | Term.Key.Mouse _
            | Term.Key.Unknown _ ->
                loop ()
          else if Term.Key.equal key (Term.Key.Ctrl 'z') then (
            Term.Raw.suspend ();
            loop ())
          else
            match key with
            | Term.Key.Paste text -> (
                (* In detail view, auto-enter message mode and buffer the paste *)
                match !view_mode with
                | Tui.Detail_view _ ->
                    Tui_input.Edit_buffer.clear buf;
                    Tui_input.Edit_buffer.insert_string buf
                      (normalize_paste text);
                    input_mode := Tui_input.Prompt_message;
                    loop ()
                | Tui.List_view | Tui.Timeline_view -> loop ())
            | Term.Key.Mouse ev -> (
                match (ev, !view_mode) with
                | ( Term_key.Click
                      { button = Term_key.Left; row; press = true; _ },
                    Tui.List_view ) ->
                    let start = !patches_start_row in
                    let count = !patches_visible_count in
                    let screen_idx = row - start in
                    let abs_idx = !patches_scroll_offset + screen_idx in
                    if start > 0 && screen_idx >= 0 && screen_idx < count then (
                      let now = Unix.gettimeofday () in
                      let is_double =
                        Float.compare (now -. !last_click_time) 0.3 <= 0
                        && !last_click_row = abs_idx
                      in
                      last_click_time := now;
                      last_click_row := abs_idx;
                      if is_double then (
                        let pids = !sorted_patch_ids in
                        let pid_count = Base.List.length pids in
                        if abs_idx < pid_count then (
                          list_selected := abs_idx;
                          let pid = Base.List.nth_exn pids abs_idx in
                          view_mode := Tui.Detail_view pid;
                          match Hashtbl.find_opt detail_scrolls pid with
                          | Some (offset, follow) ->
                              detail_scroll := offset;
                              detail_follow := follow
                          | None ->
                              detail_scroll := 0;
                              detail_follow := true))
                      else list_selected := abs_idx);
                    loop ()
                | ( Term_key.Click
                      { button = Term_key.Left; row; press = true; _ },
                    Tui.Detail_view pid ) ->
                    let size = Term.get_size () in
                    let height =
                      match size with Some s -> s.Term.rows | None -> 24
                    in
                    if row >= height - 1 then (
                      Hashtbl.replace detail_scrolls pid
                        (!detail_scroll, !detail_follow);
                      view_mode := Tui.List_view);
                    loop ()
                | Term_key.Scroll { dir; _ }, Tui.List_view ->
                    let count = Base.List.length !sorted_patch_ids in
                    let delta =
                      match dir with Term_key.Up -> -1 | Term_key.Down -> 1
                    in
                    list_selected :=
                      Base.Int.max (-1)
                        (Base.Int.min (count - 1) (!list_selected + delta));
                    loop ()
                | Term_key.Scroll { dir; _ }, Tui.Detail_view _ ->
                    let delta =
                      match dir with Term_key.Up -> -3 | Term_key.Down -> 3
                    in
                    detail_scroll := Base.Int.max 0 (!detail_scroll + delta);
                    loop ()
                | Term_key.Scroll { dir; _ }, Tui.Timeline_view ->
                    let delta =
                      match dir with Term_key.Up -> -3 | Term_key.Down -> 3
                    in
                    timeline_scroll := Base.Int.max 0 (!timeline_scroll + delta);
                    loop ()
                | ( Term_key.Click
                      {
                        button =
                          Term_key.Left | Term_key.Middle | Term_key.Right;
                        _;
                      },
                    (Tui.List_view | Tui.Detail_view _ | Tui.Timeline_view) ) ->
                    loop ())
            | Term.Key.Char '*'
              when Tui.equal_view_mode !view_mode Tui.List_view ->
                Tui_input.Edit_buffer.clear buf;
                input_mode := Tui_input.Prompt_broadcast;
                loop ()
            | Term.Key.Char '+'
              when Tui.equal_view_mode !view_mode Tui.List_view ->
                Tui_input.Edit_buffer.clear buf;
                input_mode := Tui_input.Prompt_pr;
                loop ()
            | Term.Key.Char 'w'
              when Tui.equal_view_mode !view_mode Tui.List_view ->
                Tui_input.Edit_buffer.clear buf;
                input_mode := Tui_input.Prompt_worktree;
                loop ()
            | Term.Key.Char 'o'
              when match !view_mode with
                   | Tui.Detail_view _ -> true
                   | Tui.List_view -> Option.is_some (selected_pid ())
                   | Tui.Timeline_view -> false ->
                let target_patch_id =
                  match !view_mode with
                  | Tui.Detail_view patch_id -> Some patch_id
                  | Tui.List_view -> selected_pid ()
                  | Tui.Timeline_view -> None
                in
                (match target_patch_id with
                | Some patch_id -> (
                    match Pr_registry.find pr_registry ~patch_id with
                    | Some pr_number -> (
                        let url =
                          Printf.sprintf "https://github.com/%s/%s/pull/%d"
                            owner repo
                            (Pr_number.to_int pr_number)
                        in
                        let open_cmd =
                          if Sys.file_exists "/usr/bin/open" then "open"
                          else "xdg-open"
                        in
                        match Eio.Process.run process_mgr [ open_cmd; url ] with
                        | () -> (
                            match !status_msg with
                            | Some
                                {
                                  Tui.text =
                                    "No PR to open" | "Could not open browser";
                                  _;
                                } ->
                                status_msg := None
                            | Some _ | None -> ())
                        | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
                        | exception _ ->
                            status_msg :=
                              Some
                                {
                                  Tui.level = Tui.Error;
                                  text = "Could not open browser";
                                  expires_at = None;
                                })
                    | None ->
                        status_msg :=
                          Some
                            {
                              Tui.level = Tui.Info;
                              text = "No PR to open";
                              expires_at = None;
                            })
                | None -> ());
                loop ()
            | Term.Key.Char 'm'
              when match !view_mode with
                   | Tui.Detail_view _ -> true
                   | Tui.List_view -> Option.is_some (selected_pid ())
                   | Tui.Timeline_view -> false ->
                input_mode := Tui_input.Manage_patch;
                loop ()
            | Term.Key.Char _ | Term.Key.Enter | Term.Key.Tab
            | Term.Key.Backspace | Term.Key.Escape | Term.Key.Up | Term.Key.Down
            | Term.Key.Left | Term.Key.Right | Term.Key.Home | Term.Key.End
            | Term.Key.Page_up | Term.Key.Page_down | Term.Key.Delete
            | Term.Key.F _ | Term.Key.Ctrl _ | Term.Key.Unknown _ -> (
                let cmd = Tui_input.of_key key in
                match cmd with
                | Tui_input.Quit -> raise Quit_tui
                | Tui_input.Move_up | Tui_input.Move_down | Tui_input.Page_up
                | Tui_input.Page_down | Tui_input.Scroll_top
                | Tui_input.Scroll_bottom ->
                    (match !view_mode with
                    | Tui.List_view ->
                        let count = Base.List.length !sorted_patch_ids in
                        list_selected :=
                          Tui_input.apply_move ~count ~selected:!list_selected
                            cmd
                    | Tui.Timeline_view ->
                        let total =
                          Runtime.read runtime (fun snap ->
                              let log = snap.Runtime.activity_log in
                              Base.List.length
                                (activity_entries_of_log ~limit:100 log))
                        in
                        let height =
                          match Term.get_size () with
                          | Some s -> s.Term.rows
                          | None -> 24
                        in
                        (* Keep in sync with render_frame Timeline_view reserved *)
                        let reserved = 9 in
                        let max_rows = Base.Int.max 0 (height - reserved) in
                        let max_offset = Base.Int.max 0 (total - max_rows) in
                        timeline_scroll :=
                          Tui_input.apply_move ~count:(max_offset + 1)
                            ~selected:(Base.Int.min !timeline_scroll max_offset)
                            cmd
                    | Tui.Detail_view _ -> (
                        match cmd with
                        | Tui_input.Scroll_top ->
                            detail_follow := false;
                            detail_scroll := 0
                        | Tui_input.Scroll_bottom -> detail_follow := true
                        | Tui_input.Move_up | Tui_input.Move_down
                        | Tui_input.Page_up | Tui_input.Page_down
                        | Tui_input.Quit | Tui_input.Help | Tui_input.Select
                        | Tui_input.Back | Tui_input.Timeline | Tui_input.Noop
                        | Tui_input.Send_message _ | Tui_input.Add_pr _
                        | Tui_input.Add_worktree _ | Tui_input.Remove_patch
                        | Tui_input.Open_in_browser ->
                            let delta =
                              match cmd with
                              | Tui_input.Move_up -> -1
                              | Tui_input.Move_down -> 1
                              | Tui_input.Page_up -> -10
                              | Tui_input.Page_down -> 10
                              | Tui_input.Quit | Tui_input.Help
                              | Tui_input.Select | Tui_input.Back
                              | Tui_input.Timeline | Tui_input.Noop
                              | Tui_input.Send_message _ | Tui_input.Add_pr _
                              | Tui_input.Add_worktree _
                              | Tui_input.Remove_patch
                              | Tui_input.Open_in_browser | Tui_input.Scroll_top
                              | Tui_input.Scroll_bottom ->
                                  0
                            in
                            if delta <> 0 then detail_follow := false;
                            detail_scroll :=
                              Base.Int.max 0 (!detail_scroll + delta)));
                    loop ()
                | Tui_input.Select -> (
                    match !view_mode with
                    | Tui.List_view ->
                        let pids = !sorted_patch_ids in
                        let count = Base.List.length pids in
                        if count > 0 && !list_selected >= 0 then (
                          let idx = Base.Int.min !list_selected (count - 1) in
                          list_selected := idx;
                          let pid = Base.List.nth_exn pids idx in
                          view_mode := Tui.Detail_view pid;
                          (* Restore per-patch scroll+follow or default to follow *)
                          match Hashtbl.find_opt detail_scrolls pid with
                          | Some (offset, follow) ->
                              detail_scroll := offset;
                              detail_follow := follow
                          | None ->
                              detail_scroll := 0;
                              detail_follow := true);
                        loop ()
                    | Tui.Detail_view _ ->
                        input_mode := Tui_input.Prompt_message;
                        loop ()
                    | Tui.Timeline_view -> loop ())
                | Tui_input.Back -> (
                    match !view_mode with
                    | Tui.Detail_view pid ->
                        (* Save per-patch scroll position and follow state *)
                        Hashtbl.replace detail_scrolls pid
                          (!detail_scroll, !detail_follow);
                        view_mode := Tui.List_view;
                        loop ()
                    | Tui.Timeline_view ->
                        view_mode := Tui.List_view;
                        loop ()
                    | Tui.List_view -> loop ())
                | Tui_input.Timeline -> (
                    match !view_mode with
                    | Tui.Timeline_view ->
                        view_mode := Tui.List_view;
                        loop ()
                    | Tui.List_view | Tui.Detail_view _ ->
                        (match !view_mode with
                        | Tui.Detail_view pid ->
                            Hashtbl.replace detail_scrolls pid
                              (!detail_scroll, !detail_follow)
                        | Tui.List_view | Tui.Timeline_view -> ());
                        view_mode := Tui.Timeline_view;
                        timeline_scroll := 0;
                        loop ())
                | Tui_input.Help ->
                    show_help := true;
                    loop ()
                | Tui_input.Remove_patch ->
                    (match !view_mode with
                    | Tui.List_view -> (
                        match selected_pid () with
                        | None ->
                            log_event runtime
                              "Cannot remove patch — no selectable patch"
                        | Some patch_id ->
                            let busy, in_gameplan =
                              Runtime.read runtime (fun snap ->
                                  let agent =
                                    Orchestrator.agent snap.Runtime.orchestrator
                                      patch_id
                                  in
                                  let in_gp =
                                    Base.List.exists
                                      snap.Runtime.gameplan.Gameplan.patches
                                      ~f:(fun (p : Patch.t) ->
                                        Patch_id.equal p.Patch.id patch_id)
                                  in
                                  (agent.Patch_agent.busy, in_gp))
                            in
                            if in_gameplan then
                              log_event runtime ~patch_id
                                "Cannot remove gameplan patch — only ad-hoc \
                                 patches can be removed"
                            else (
                              if busy then
                                log_event runtime ~patch_id
                                  "Warning — patch is currently running, it \
                                   may create a GitHub PR before stopping";
                              Runtime.update_orchestrator runtime (fun orch ->
                                  Orchestrator.remove_agent orch patch_id);
                              Pr_registry.unregister pr_registry ~patch_id;
                              log_event runtime ~patch_id "Removed ad-hoc patch")
                        )
                    | Tui.Detail_view _ | Tui.Timeline_view -> ());
                    loop ()
                | Tui_input.Noop | Tui_input.Send_message _ | Tui_input.Add_pr _
                | Tui_input.Add_worktree _ | Tui_input.Open_in_browser ->
                    loop ()))
    in
    loop ()

  (** Poller fiber — periodically polls GitHub for PR state changes and
      reconciles. *)
  let poll_review_backends ~runtime ~patch_id ~findings_registry ~review_clients
      ~owner ~repo ~pr_number : Review_service.finding list =
    Base.List.concat_map review_clients
      ~f:(fun
          (module R : Review_service_client.S
            with type error = Review_service_client.error)
        ->
        match R.list_findings ~owner ~repo ~pr_number () with
        | Error err ->
            log_event runtime ~patch_id
              (Printf.sprintf "Poll error — %s" (R.show_error err));
            []
        | Ok response ->
            let parsed_count =
              Base.List.length response.Review_service.findings
            in
            if not (Int.equal parsed_count response.Review_service.count) then
              log_event runtime ~patch_id
                (Printf.sprintf
                   "Review backend %s declared %d finding(s) but parsed %d — \
                    possible review-service schema drift"
                   R.name response.Review_service.count parsed_count);
            Base.List.iter response.Review_service.dropped_findings
              ~f:(fun (e : Review_service.finding_parse_error) ->
                let json =
                  if String.length e.Review_service.json <= 500 then
                    e.Review_service.json
                  else String.sub e.Review_service.json 0 497 ^ "..."
                in
                log_event runtime ~patch_id
                  (Printf.sprintf
                     "Review backend %s dropped finding at index %d while \
                      parsing: %s; json=%s"
                     R.name e.Review_service.index e.Review_service.error json));
            let keyed_findings =
              Base.List.map response.Review_service.findings
                ~f:(fun (f : Review_service.finding) ->
                  let key =
                    Findings_registry.make_key ~backend_name:R.name ~owner ~repo
                      ~pr_number ~finding_id:f.Review_service.id
                  in
                  (key, f))
            in
            Findings_registry.remove_stale_for_scope findings_registry
              ~backend_name:R.name ~owner ~repo ~pr_number
              ~keep_keys:(Base.List.map keyed_findings ~f:fst);
            Base.List.map keyed_findings ~f:(fun (key, f) ->
                Findings_registry.register findings_registry ~key
                  {
                    Findings_registry.backend_name = R.name;
                    owner;
                    repo;
                    pr_number;
                    finding_id = f.Review_service.id;
                  };
                { f with Review_service.id = key }))

  let poller_fiber startup_reconciler = Poller.run startup_reconciler

  (** Runner fiber — executes orchestrator actions by driving backend sessions
      concurrently. *)
  let runner_fiber ?status_msg () =
    let runtime = Env.runtime in
    let clock = Env.clock in
    let config = Env.config in
    let pick_backend = Env.pick_backend in
    let project_name = Env.project_name in
    let pr_registry = Env.pr_registry in
    let findings_registry = Env.findings_registry in
    let review_clients = Env.review_clients in
    let event_log = Env.event_log in
    let main = config.main_branch in
    let set_status ~level ~text ?expires_at () =
      match status_msg with
      | Some r -> r := Some { Tui.level; text; expires_at }
      | None -> ()
    in
    let semaphore = Eio.Semaphore.make config.max_concurrency in
    let with_session_slot f =
      Eio.Semaphore.acquire semaphore;
      Fun.protect ~finally:(fun () -> Eio.Semaphore.release semaphore) f
    in
    (* Serializes [git fetch origin] across worktrees to avoid ref-lock races on
     the shared [refs/remotes/origin/*] store. See [Worktree.fetch_origin]. *)
    let fetch_mutex = Eio.Mutex.create () in
    let long_lived_sessions = Long_lived_sessions.create () in
    let patch_agent_provider =
      match Stdlib.Sys.getenv_opt "PATCH_AGENT_PROVIDER" with
      | Some s when not (Base.String.is_empty (Base.String.strip s)) ->
          Base.String.strip s
      | Some _ | None -> "anthropic"
    in
    let patch_agent_effort =
      match Stdlib.Sys.getenv_opt "PATCH_AGENT_EFFORT" with
      | Some s when not (Base.String.is_empty (Base.String.strip s)) ->
          Base.String.strip s
      | Some _ | None -> "medium"
    in
    let run_llm_session ~sw ~gameplan_prompt ~patch_prompt ~kind ~patch_id
        ~prompt ~agent ~on_pr_detected ~complexity =
      match pick_backend ~complexity with
      | Backend_registry.Ephemeral backend, _decision ->
          Session_driver.run ~kind ~patch_id ~prompt ~agent ~on_pr_detected
            ~backend ~complexity
      | Backend_registry.Long_lived backend, decision -> (
          let patch_agent_model_result =
            match
              Backend_registry.resolve_model
                ~backend:decision.Backend_routing.backend
                ~model:decision.Backend_routing.model ~complexity
            with
            | Some m when not (Base.String.is_empty (Base.String.strip m)) ->
                Ok (Base.String.strip m)
            | Some _ | None ->
                Error
                  "patch-agent backend requires a concrete non-empty model \
                   after routing"
          in
          match patch_agent_model_result with
          | Error message ->
              log_event runtime ~patch_id message;
              (`Failed, [])
          | Ok patch_agent_model ->
              let session =
                match Long_lived_sessions.find long_lived_sessions patch_id with
                | Some session ->
                    Session_driver.update_long_lived_session_prompts session
                      ~gameplan_prompt ~patch_prompt;
                    session
                | None ->
                    let session =
                      Session_driver.create_long_lived_session ~backend
                        ~provider:patch_agent_provider ~model:patch_agent_model
                        ~effort:patch_agent_effort ~gameplan_prompt
                        ~patch_prompt
                    in
                    Long_lived_sessions.register long_lived_sessions patch_id
                      session;
                    session
              in
              Session_driver.run_long_lived ~sw ~kind ~patch_id ~prompt ~agent
                ~on_pr_detected ~session ~complexity)
    in
    let shutdown_finished_long_lived_sessions ~sw () =
      let finished =
        Runtime.read runtime (fun snap ->
            Base.List.fold (Long_lived_sessions.to_alist long_lived_sessions)
              ~init:[] ~f:(fun acc (patch_id, session) ->
                if Session_driver.long_lived_session_failed session then
                  (patch_id, session) :: acc
                else
                  match
                    Orchestrator.find_agent snap.Runtime.orchestrator patch_id
                  with
                  | None -> (patch_id, session) :: acc
                  | Some agent
                    when agent.Patch_agent.merged && not agent.Patch_agent.busy
                    ->
                      (patch_id, session) :: acc
                  | Some _ -> acc))
      in
      Base.List.iter finished ~f:(fun (patch_id, session) ->
          Long_lived_sessions.remove long_lived_sessions patch_id;
          Eio.Fiber.fork_daemon ~sw (fun () ->
              (try Session_driver.shutdown_long_lived_session session with
              | Eio.Cancel.Cancelled _ -> ()
              | exn ->
                  log_event runtime ~patch_id
                    (Printf.sprintf "long-lived backend shutdown error — %s"
                       (Printexc.to_string exn)));
              `Stop_daemon))
    in
    let rec loop sw =
      shutdown_finished_long_lived_sessions ~sw ();
      let gameplan = Runtime.read runtime (fun snap -> snap.Runtime.gameplan) in
      let lifecycle_effects, messages, pre_fire_agents =
        Runtime.update_orchestrator_returning runtime (fun orch ->
            let orch, effects, messages =
              Patch_controller.plan_tick_messages orch ~project_name ~gameplan
            in
            let pre_fire_agents =
              Base.List.filter_map messages
                ~f:(fun (msg : Orchestrator.patch_agent_message) ->
                  match
                    ( Orchestrator.message_status msg,
                      Orchestrator.message_action msg )
                  with
                  | ( Orchestrator.Pending,
                      ( Orchestrator.Respond (pid, _)
                      | Orchestrator.Rebase (pid, _) ) ) ->
                      Some (pid, Orchestrator.agent orch pid)
                  | Orchestrator.Pending, Orchestrator.Start _
                  | Orchestrator.Acked, _
                  | Orchestrator.Completed, _
                  | Orchestrator.Obsolete, _ ->
                      None)
            in
            let orch, dispatched =
              Base.List.fold messages ~init:(orch, [])
                ~f:(fun
                    (acc, dispatched)
                    (msg : Orchestrator.patch_agent_message)
                  ->
                  match Orchestrator.message_status msg with
                  | Orchestrator.Pending ->
                      let acc, action =
                        Orchestrator.accept_message acc
                          (Orchestrator.message_id msg)
                      in
                      let dispatched =
                        match action with
                        | Some _ -> msg :: dispatched
                        | None -> dispatched
                      in
                      (acc, dispatched)
                  | Orchestrator.Acked ->
                      let acc, action =
                        Orchestrator.resume_message acc
                          (Orchestrator.message_id msg)
                      in
                      let dispatched =
                        match action with
                        | Some _ -> msg :: dispatched
                        | None -> dispatched
                      in
                      (acc, dispatched)
                  | Orchestrator.Completed | Orchestrator.Obsolete ->
                      (acc, dispatched))
            in
            (orch, (effects, List.rev dispatched, pre_fire_agents)))
      in
      execute_github_effects ~runtime lifecycle_effects;
      (* Log dispatched actions to event log *)
      Base.List.iter messages
        ~f:(fun (msg : Orchestrator.patch_agent_message) ->
          let action = Orchestrator.message_action msg in
          let agent_before =
            match
              Base.List.Assoc.find pre_fire_agents ~equal:Patch_id.equal
                (Orchestrator.message_patch_id msg)
            with
            | Some a -> a
            | None ->
                (* Start actions create the agent during fire, so there is no
                 pre-fire snapshot in [pre_fire_agents]. In that case we log
                 the post-create/default agent state as [agent_before]. *)
                Runtime.read runtime (fun snap ->
                    Orchestrator.agent snap.Runtime.orchestrator
                      (Orchestrator.message_patch_id msg))
          in
          Event_log.log_action event_log ~action ~agent_before);
      (* Spawn all actions concurrently, limited by max_concurrency semaphore *)
      let action_fibers =
        Base.List.filter_map messages
          ~f:(fun (msg : Orchestrator.patch_agent_message) ->
            match Orchestrator.message_action msg with
            | Orchestrator.Start (patch_id, base_branch) -> (
                match
                  Base.List.find gameplan.Gameplan.patches
                    ~f:(fun (p : Patch.t) -> Patch_id.equal p.Patch.id patch_id)
                with
                | None ->
                    log_event runtime ~patch_id
                      "Skipping start — patch not found in gameplan";
                    mark_session_failed event_log runtime patch_id;
                    None
                | Some patch ->
                    Some
                      (fun () ->
                        With_busy_guard.run ~patch_id (fun () ->
                            let result =
                              with_session_slot (fun () ->
                                  let agent =
                                    Runtime.read runtime (fun snap ->
                                        Orchestrator.agent
                                          snap.Runtime.orchestrator patch_id)
                                  in
                                  if
                                    agent.Patch_agent.merged
                                    || Patch_agent.needs_intervention agent
                                    || agent.Patch_agent.branch_blocked
                                    || not agent.Patch_agent.busy
                                  then (
                                    log_event runtime ~patch_id
                                      "Skipping action — became stale during \
                                       semaphore wait";
                                    `Stale)
                                  else (
                                    Runtime.update_orchestrator runtime
                                      (fun orch ->
                                        Orchestrator.mark_running orch patch_id);
                                    match
                                      WS.ensure_worktree ~patch_id ~agent
                                        ~branch:patch.Patch.branch
                                        ~base_ref:(Branch.to_string base_branch)
                                        ()
                                    with
                                    | None ->
                                        Runtime.update_orchestrator runtime
                                          (fun orch ->
                                            Orchestrator.apply_session_result
                                              orch patch_id
                                              Orchestrator
                                              .Session_worktree_missing);
                                        `Failed
                                    | Some _wt_path ->
                                        let agents_md =
                                          read_optional_file
                                            (Stdlib.Filename.concat _wt_path
                                               "AGENTS.md")
                                        in
                                        let prompt =
                                          Prompt.render_patch_prompt
                                            ~project_name ?agents_md
                                            ?pr_number:
                                              agent.Patch_agent.pr_number patch
                                            gameplan
                                            ~base_branch:
                                              (Branch.to_string base_branch)
                                        in
                                        (* PR detection from stream text is a hint
                                     only — always confirmed via the GitHub
                                     REST API after the
                                     backend session finishes *)
                                        let on_pr_detected _pr_number = () in
                                        let complexity =
                                          patch_complexity ~gameplan ~patch_id
                                        in
                                        let gameplan_prompt =
                                          Prompt.render_gameplan_layer
                                            ~project_name gameplan
                                        in
                                        let functional_changes =
                                          Prompt.owned_functional_changes
                                            gameplan patch
                                        in
                                        let patch_prompt =
                                          Prompt.render_patch_layer
                                            ~project_name patch
                                            ?pr_number:
                                              agent.Patch_agent.pr_number
                                            ~functional_changes
                                            ~base_branch:
                                              (Branch.to_string base_branch)
                                            ()
                                        in
                                        let r, _tool_failures =
                                          run_llm_session ~sw ~gameplan_prompt
                                            ~patch_prompt ~kind:None ~patch_id
                                            ~prompt ~agent ~on_pr_detected
                                            ~complexity
                                        in
                                        (r
                                          :> [ `Failed
                                             | `Ok
                                             | `Retry_push
                                             | `Stale ])))
                            in
                            let start_outcome =
                              match result with
                              | `Stale -> Orchestrator.Start_stale
                              | `Failed | `Retry_push ->
                                  Orchestrator.Start_failed
                              | `Ok -> Orchestrator.Start_ok
                            in
                            Runtime.update_orchestrator runtime (fun orch ->
                                Orchestrator.apply_start_outcome orch patch_id
                                  start_outcome);
                            match start_outcome with
                            | Orchestrator.Start_failed ->
                                let agent =
                                  Runtime.read runtime (fun snap ->
                                      Orchestrator.agent
                                        snap.Runtime.orchestrator patch_id)
                                in
                                if Patch_agent.needs_intervention agent then
                                  set_status ~level:Tui.Error
                                    ~text:
                                      (Printf.sprintf
                                         "Patch %s: session failed — human \
                                          review needed"
                                         (Patch_id.to_string patch_id))
                                    ()
                            | Orchestrator.Start_ok ->
                                (* Supervisor-owned PR creation: the agent
                                 commits and the supervisor pushed at session
                                 end; now we open the draft PR with a
                                 gameplan-derived title and body.

                                 Re-derive the base branch from current
                                 orchestrator state — dependencies may have
                                 merged while the agent session was running,
                                 making the base captured at dispatch time
                                 stale. *)
                                let fresh_base =
                                  Runtime.read runtime (fun snap ->
                                      let orch = snap.Runtime.orchestrator in
                                      let has_merged pid =
                                        (Orchestrator.agent orch pid)
                                          .Patch_agent.merged
                                      in
                                      let branch_of pid =
                                        (Base.List.find_exn
                                           gameplan.Gameplan.patches
                                           ~f:(fun (p : Patch.t) ->
                                             Patch_id.equal p.Patch.id pid))
                                          .Patch.branch
                                      in
                                      Graph.initial_base
                                        (Orchestrator.graph orch) patch_id
                                        ~has_merged ~branch_of
                                        ~main:(Orchestrator.main_branch orch))
                                in
                                let pr_title =
                                  Printf.sprintf "[%s] Patch %s: %s"
                                    project_name
                                    (Patch_id.to_string patch.Patch.id)
                                    patch.Patch.title
                                in
                                let pr_body =
                                  Prompt.render_pr_description ~project_name
                                    patch gameplan
                                  ^ Prompt.render_spec_suffix patch gameplan
                                in
                                (match
                                   Forge.create_pull_request ~title:pr_title
                                     ~head:patch.Patch.branch ~base:fresh_base
                                     ~body:pr_body ~draft:true
                                 with
                                | Ok pr_number ->
                                    log_event runtime ~patch_id
                                      (Printf.sprintf "PR #%d created"
                                         (Pr_number.to_int pr_number));
                                    Pr_registry.register pr_registry ~patch_id
                                      ~pr_number;
                                    Runtime.update_orchestrator runtime
                                      (fun orch ->
                                        Orchestrator.set_pr_number orch patch_id
                                          pr_number)
                                | Error e -> (
                                    match e with
                                    | Github.Http_error
                                        { status = 422; body; _ }
                                      when Github
                                           .response_error_message_contains body
                                             ~substring:
                                               "pull request already exists"
                                      -> (
                                        (* PR already exists — discover it rather
                                         than treating this as failure. We
                                         only fall back on this specific 422;
                                         other 422s (no commits, head missing,
                                         etc.) propagate with the original
                                         error message. *)
                                        match
                                          Forge.list_prs
                                            ~branch:patch.Patch.branch
                                            ~state:`Open ()
                                        with
                                        | Ok ((pr_number, _, _) :: _) ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "PR #%d already existed, \
                                                  associated"
                                                 (Pr_number.to_int pr_number));
                                            Pr_registry.register pr_registry
                                              ~patch_id ~pr_number;
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator.set_pr_number orch
                                                  patch_id pr_number)
                                        | Ok [] ->
                                            log_event runtime ~patch_id
                                              "PR creation failed (422 \
                                               already-exists) and discovery \
                                               found no open PRs";
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator
                                                .on_pr_discovery_failure orch
                                                  patch_id)
                                        | Error disc_err ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "PR creation failed (422 \
                                                  already-exists) and \
                                                  discovery also failed — %s"
                                                 (Forge.show_error disc_err));
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator
                                                .on_pr_discovery_failure orch
                                                  patch_id))
                                    | Github.Http_error _
                                    | Github.Json_parse_error _
                                    | Github.Graphql_error _ | Github.Timeout _
                                    | Github.Transport_error _ ->
                                        log_event runtime ~patch_id
                                          (Printf.sprintf
                                             "PR creation failed — %s"
                                             (Forge.show_error e));
                                        Runtime.update_orchestrator runtime
                                          (fun orch ->
                                            Orchestrator.on_pr_discovery_failure
                                              orch patch_id)));
                                Runtime.update_orchestrator runtime (fun orch ->
                                    Orchestrator.complete orch patch_id)
                            | Orchestrator.Start_stale -> ())))
            | Orchestrator.Rebase (patch_id, new_base) ->
                Some
                  (fun () ->
                    With_busy_guard.run ~patch_id (fun () ->
                        (* Rebase is orchestrator-executed (no session slot), so
                         work begins immediately under the busy guard. *)
                        Runtime.update_orchestrator runtime (fun orch ->
                            Orchestrator.mark_running orch patch_id);
                        let agent =
                          Runtime.read runtime (fun snap ->
                              Orchestrator.agent snap.Runtime.orchestrator
                                patch_id)
                        in
                        let ancestor_ids =
                          Runtime.read runtime (fun snap ->
                              Graph.transitive_ancestors
                                (Orchestrator.graph snap.Runtime.orchestrator)
                                patch_id)
                        in
                        let rebase_result, wt_path =
                          Worktree_plan_executor.execute ~patch_id ~agent
                            ~fetch_lock:fetch_mutex ~fail_label:"rebase"
                            ~ancestor_ids
                            (Worktree_plan.for_rebase ~new_base)
                        in
                        (match rebase_result with
                        | Worktree.Ok ->
                            log_event runtime ~patch_id
                              (Printf.sprintf "Rebased onto %s"
                                 (Branch.to_string new_base))
                        | Worktree.Noop ->
                            log_event runtime ~patch_id
                              "Rebase noop — already up-to-date"
                        | Worktree.Conflict _ ->
                            log_event runtime ~patch_id
                              "Rebase conflict — enqueued merge-conflict"
                        | Worktree.Error msg ->
                            log_event runtime ~patch_id
                              (Printf.sprintf "Rebase failed — %s" msg));
                        let agent_before, (agent_after, effects) =
                          Runtime.update_orchestrator_returning runtime
                            (fun orch ->
                              let agent_before =
                                Orchestrator.agent orch patch_id
                              in
                              let orch, effects =
                                Orchestrator.apply_rebase_result orch patch_id
                                  rebase_result new_base
                              in
                              let agent_after =
                                Orchestrator.agent orch patch_id
                              in
                              (orch, (agent_before, (agent_after, effects))))
                        in
                        let push_outcome =
                          Base.List.find_map effects
                            ~f:(fun Orchestrator.Push_branch ->
                              let branch = agent.Patch_agent.branch in
                              let result =
                                W.force_push_with_lease ~path:wt_path ~branch
                                  ~base:new_base
                              in
                              (match result with
                              | Worktree.Push_ok ->
                                  log_event runtime ~patch_id
                                    "Force-pushed after rebase"
                              | Worktree.Push_up_to_date ->
                                  log_event runtime ~patch_id
                                    "Push noop after rebase — already \
                                     up-to-date"
                              | Worktree.Push_no_commits ->
                                  log_event runtime ~patch_id
                                    "Force-push skipped after rebase — branch \
                                     has no commits ahead of base"
                              | Worktree.Push_rejected ->
                                  log_event runtime ~patch_id
                                    "Force-push rejected — lease violated"
                              | Worktree.Push_worktree_missing ->
                                  log_event runtime ~patch_id
                                    (Printf.sprintf
                                       "Worktree disappeared (%s) — rebase \
                                        will reconstruct on retry"
                                       wt_path)
                              | Worktree.Push_error msg ->
                                  log_event runtime ~patch_id
                                    (Printf.sprintf "Force-push failed — %s" msg));
                              Some result)
                        in
                        let resolution =
                          Runtime.update_orchestrator_returning runtime
                            (fun orch ->
                              let orch, resolution =
                                Orchestrator.apply_rebase_push_result orch
                                  patch_id push_outcome
                              in
                              (orch, resolution))
                        in
                        (match resolution with
                        | Orchestrator.Rebase_push_ok -> ()
                        | Orchestrator.Rebase_push_failed ->
                            log_event runtime ~patch_id
                              "Enqueued merge-conflict after rebase push \
                               rejection"
                        | Orchestrator.Rebase_push_error ->
                            log_event runtime ~patch_id
                              "Enqueued rebase retry after push error");
                        Event_log.log_rebase event_log ~patch_id
                          ~result:rebase_result ~agent_before ~agent_after))
            | Orchestrator.Respond (patch_id, kind) ->
                (* Use pre-fire agent state for human_messages — fire/respond
                 clears them as a postcondition. *)
                let pre_fire_agent =
                  Base.List.Assoc.find pre_fire_agents patch_id
                    ~equal:Patch_id.equal
                in
                Some
                  (fun () ->
                    (* For Review_comments and Ci, fetch fresh state from
                     GitHub before acquiring a session slot to avoid blocking
                     concurrency on GitHub API I/O. The Ci fetch is the
                     freshness gate against delivering a failure that's
                     already been superseded by a newer run. *)
                    let is_review =
                      Operation_kind.equal kind Operation_kind.Review_comments
                    in
                    let is_ci = Operation_kind.equal kind Operation_kind.Ci in
                    let pr_number =
                      Runtime.read runtime (fun snap ->
                          (Orchestrator.agent snap.Runtime.orchestrator patch_id)
                            .Patch_agent.pr_number)
                    in
                    let fresh_pr_state =
                      if is_review || is_ci then (
                        match pr_number with
                        | Some pr_num -> (
                            log_event runtime ~patch_id
                              (if is_ci then
                                 "Fetching fresh CI state from GitHub"
                               else "Fetching fresh review comments from GitHub");
                            match Forge.pr_state pr_num with
                            | Ok pr_state -> Some pr_state
                            | Error _err ->
                                log_event runtime ~patch_id
                                  (if is_ci then
                                     "Failed to fetch fresh CI state"
                                   else "Failed to fetch fresh review comments");
                                None)
                        | None ->
                            if is_ci then
                              log_event runtime ~patch_id
                                "No PR number yet — skipping CI state fetch";
                            None)
                      else None
                    in
                    let prefetched_comments =
                      if is_review then
                        match fresh_pr_state with
                        | Some pr_state -> pr_state.Pr_state.comments
                        | None -> []
                      else []
                    in
                    let prefetched_findings =
                      if Operation_kind.equal kind Operation_kind.Findings then
                        match pr_number with
                        | Some pr_num
                          when not (Base.List.is_empty review_clients) ->
                            log_event runtime ~patch_id
                              "Fetching fresh findings from review backends";
                            poll_review_backends ~runtime ~patch_id
                              ~findings_registry ~review_clients
                              ~owner:config.github_owner
                              ~repo:config.github_repo
                              ~pr_number:(Pr_number.to_int pr_num)
                        | Some _ | None -> []
                      else []
                    in
                    (* Ci freshness gate: if we have no PR number, couldn't
                     fetch, or the fetched state shows no current failures,
                     skip the delivery — don't wake the agent for a failure
                     that's already been superseded. The [set_ci_checks]
                     write is deferred to inside [with_busy_guard] so it
                     can't corrupt state on a stale/cancelled delivery nor
                     race with the poller. *)
                    let ci_skip_reason =
                      if is_ci then
                        match (pr_number, fresh_pr_state) with
                        | None, _ -> Some "no PR number"
                        | Some _, None -> Some "fetch failed"
                        | Some _, Some pr_state ->
                            if
                              Base.List.exists pr_state.Pr_state.ci_checks
                                ~f:Ci_check.is_failure
                            then None
                            else (
                              log_event runtime ~patch_id
                                "Fresh CI state shows no failures — skipping \
                                 CI delivery";
                              Some "no current failures")
                      else None
                    in
                    With_busy_guard.run ~patch_id (fun () ->
                        let result =
                          match ci_skip_reason with
                          | Some reason ->
                              (* Fast path: skip decision already made. Don't
                               consume a session slot for a no-op — that
                               slot can go to another agent. *)
                              log_event runtime ~patch_id
                                (Printf.sprintf "Skipped ci delivery — %s"
                                   reason);
                              `Skip_empty
                          | None ->
                              with_session_slot (fun () ->
                                  Runtime.update_orchestrator runtime
                                    (fun orch ->
                                      Orchestrator.mark_running orch patch_id);
                                  (* Write fresh ci_checks under the busy guard
                                   so the write can't race with the poller or
                                   land after a concurrent complete/merge.
                                   Must happen before the agent re-read so
                                   [agent.ci_checks] reflects the fresh
                                   list. *)
                                  (match (is_ci, fresh_pr_state) with
                                  | true, Some pr_state ->
                                      Runtime.update_orchestrator runtime
                                        (fun orch ->
                                          Orchestrator.set_ci_checks orch
                                            patch_id pr_state.Pr_state.ci_checks)
                                  | _ -> ());
                                  let agent =
                                    Runtime.read runtime (fun snap ->
                                        Orchestrator.agent
                                          snap.Runtime.orchestrator patch_id)
                                  in
                                  let delivery =
                                    Patch_decision.respond_delivery ~agent ~kind
                                      ~pre_fire_agent ~prefetched_comments
                                      ~prefetched_findings
                                      ~main_branch:(Branch.to_string main)
                                  in
                                  let render_base_changed_prefix base_change =
                                    match base_change with
                                    | Some bc ->
                                        log_event runtime ~patch_id
                                          (Printf.sprintf
                                             "Base branch changed from %s to \
                                              %s — notifying agent"
                                             bc.Patch_decision.old_base
                                             bc.Patch_decision.new_base);
                                        Prompt.render_base_branch_changed
                                          ~old_base:bc.Patch_decision.old_base
                                          ~new_base:bc.Patch_decision.new_base
                                    | None -> ""
                                  in
                                  match delivery with
                                  | Patch_decision.Respond_stale ->
                                      log_event runtime ~patch_id
                                        "Skipping action — became stale during \
                                         semaphore wait";
                                      `Stale
                                  | Patch_decision.Skip_empty ->
                                      log_event runtime ~patch_id
                                        (if
                                           Operation_kind.equal kind
                                             Operation_kind.Findings
                                         then
                                           "Skipped findings — pre-session \
                                            refresh returned no findings; no \
                                            resolution posted"
                                         else
                                           Printf.sprintf
                                             "Skipped %s — nothing to deliver"
                                             (Operation_kind.to_label kind));
                                      `Skip_empty
                                  | Patch_decision.Deliver
                                      {
                                        payload =
                                          Patch_decision.Merge_conflict_payload;
                                        base_change;
                                      } -> (
                                      let base =
                                        Base.Option.value_map
                                          agent.Patch_agent.base_branch
                                          ~default:(Branch.to_string main)
                                          ~f:Branch.to_string
                                      in
                                      let base_changed_prefix =
                                        render_base_changed_prefix base_change
                                      in
                                      let wt_path_opt =
                                        WS.ensure_worktree ~patch_id ~agent ()
                                      in
                                      let wt_path =
                                        match wt_path_opt with
                                        | Some p -> p
                                        | None ->
                                            Worktree.worktree_dir ~project_name
                                              ~patch_id
                                      in
                                      (* Helper: capture git context and deliver
                                   an enriched prompt to the agent. *)
                                      let deliver_to_agent ?conflict_info () =
                                        let pr_number =
                                          agent.Patch_agent.pr_number
                                        in
                                        let rebase_still_in_progress =
                                          W.rebase_in_progress ~path:wt_path
                                        in
                                        let git_status =
                                          W.git_status ~path:wt_path
                                        in
                                        let git_diff =
                                          W.conflict_diff ~path:wt_path
                                        in
                                        Event_log.log_conflict_delivery
                                          event_log ~patch_id ~path:wt_path
                                          ~rebase_in_progress:
                                            rebase_still_in_progress ~git_status
                                          ~git_diff;
                                        let patch =
                                          Base.List.find
                                            gameplan.Gameplan.patches
                                            ~f:(fun (p : Patch.t) ->
                                              Patch_id.equal p.Patch.id patch_id)
                                        in
                                        let agents_md =
                                          read_optional_file
                                            (Stdlib.Filename.concat wt_path
                                               "AGENTS.md")
                                        in
                                        let prompt =
                                          let raw =
                                            Prompt.render_merge_conflict_prompt
                                              ~project_name ?agents_md
                                              ?pr_number ?patch ~gameplan
                                              ~base_branch:base ~git_status
                                              ~git_diff ?conflict_info ()
                                          in
                                          if String.equal base_changed_prefix ""
                                          then raw
                                          else base_changed_prefix ^ "\n" ^ raw
                                        in
                                        let on_pr_detected _pr_number = () in
                                        let complexity =
                                          patch_complexity ~gameplan ~patch_id
                                        in
                                        let gameplan_prompt =
                                          Prompt.render_gameplan_layer
                                            ~project_name gameplan
                                        in
                                        let patch_prompt =
                                          match patch with
                                          | Some p ->
                                              let functional_changes =
                                                Prompt.owned_functional_changes
                                                  gameplan p
                                              in
                                              Prompt.render_patch_layer
                                                ~project_name p ?pr_number
                                                ~functional_changes
                                                ~base_branch:base ()
                                          | None -> ""
                                        in
                                        let result, _tool_failures =
                                          run_llm_session ~sw ~gameplan_prompt
                                            ~patch_prompt
                                            ~kind:
                                              (Some
                                                 Operation_kind.Merge_conflict)
                                            ~patch_id ~prompt ~agent
                                            ~on_pr_detected ~complexity
                                        in
                                        (match result with
                                        | `Ok
                                          when not
                                                 (String.equal
                                                    base_changed_prefix "") ->
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator
                                                .set_notified_base_branch orch
                                                  patch_id
                                                  (Branch.of_string base))
                                        | _ -> ());
                                        (result
                                          :> [ `Failed
                                             | `Ok
                                             | `Pr_body_miss
                                             | `Retry_push
                                             | `Skip_empty
                                             | `Stale ])
                                      in
                                      let ancestor_ids =
                                        Runtime.read runtime (fun snap ->
                                            Graph.transitive_ancestors
                                              (Orchestrator.graph
                                                 snap.Runtime.orchestrator)
                                              patch_id)
                                      in
                                      if W.rebase_in_progress ~path:wt_path then (
                                        log_event runtime ~patch_id
                                          "Delivering merge-conflict — rebase \
                                           already in progress";
                                        (* Match the fresh-rebase path: rebase
                                         target is [origin/<base>], not the
                                         (possibly stale) local tracking ref.
                                         See Worktree_plan.for_merge_conflict. *)
                                        let conflict_info =
                                          W.read_in_progress_conflict_info
                                            ~path:wt_path
                                            ~target:
                                              (Types.Branch.of_string
                                                 ("origin/" ^ base))
                                            ~project_name ~ancestor_ids
                                        in
                                        deliver_to_agent ?conflict_info ())
                                      else
                                        (* Plan-driven: the planner guarantees
                                     Ensure_worktree precedes Fetch_origin
                                     and Rebase_onto. The executor short-
                                     circuits on the first failure. Plans
                                     target origin/<base> so we rebase
                                     against fresh refs, not the stale
                                     local tracking ref. *)
                                        let rebase_result, _wt_path =
                                          Worktree_plan_executor.execute
                                            ~patch_id ~agent
                                            ~fetch_lock:fetch_mutex
                                            ~fail_label:"merge-conflict rebase"
                                            ~ancestor_ids
                                            (Worktree_plan.for_merge_conflict
                                               ~base:
                                                 (Types.Branch.of_string base))
                                        in
                                        let conflict_info =
                                          match rebase_result with
                                          | Worktree.Conflict ci -> Some ci
                                          | Worktree.Ok | Worktree.Noop
                                          | Worktree.Error _ ->
                                              None
                                        in
                                        (match rebase_result with
                                        | Worktree.Ok ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "Conflict rebase onto %s \
                                                  succeeded"
                                                 base)
                                        | Worktree.Noop ->
                                            log_event runtime ~patch_id
                                              "Conflict rebase noop — local \
                                               already up-to-date, will push"
                                        | Worktree.Conflict _ ->
                                            log_event runtime ~patch_id
                                              "Conflict rebase hit conflicts — \
                                               delivering to agent"
                                        | Worktree.Error msg ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "Conflict rebase failed — %s"
                                                 msg));
                                        let ( decision,
                                              agent_before,
                                              agent_after,
                                              effects ) =
                                          Runtime.update_orchestrator_returning
                                            runtime (fun orch ->
                                              let agent_before =
                                                Orchestrator.agent orch patch_id
                                              in
                                              let orch, decision, effects =
                                                Orchestrator
                                                .apply_conflict_rebase_result
                                                  orch patch_id rebase_result
                                                  (Types.Branch.of_string base)
                                              in
                                              let agent_after =
                                                Orchestrator.agent orch patch_id
                                              in
                                              ( orch,
                                                ( decision,
                                                  agent_before,
                                                  agent_after,
                                                  effects ) ))
                                        in
                                        Event_log.log_conflict_rebase event_log
                                          ~patch_id ~result:rebase_result
                                          ~decision ~agent_before ~agent_after;
                                        let push_outcome =
                                          Base.List.find_map effects
                                            ~f:(fun Orchestrator.Push_branch ->
                                              let branch =
                                                agent.Patch_agent.branch
                                              in
                                              let result =
                                                W.force_push_with_lease
                                                  ~path:wt_path ~branch
                                                  ~base:
                                                    (Types.Branch.of_string base)
                                              in
                                              (match result with
                                              | Worktree.Push_ok ->
                                                  log_event runtime ~patch_id
                                                    "Force-pushed to resolve \
                                                     conflict"
                                              | Worktree.Push_up_to_date ->
                                                  log_event runtime ~patch_id
                                                    "Conflict push noop — \
                                                     already up-to-date"
                                              | Worktree.Push_no_commits ->
                                                  log_event runtime ~patch_id
                                                    "Conflict force-push \
                                                     skipped — branch has no \
                                                     commits ahead of base"
                                              | Worktree.Push_rejected ->
                                                  log_event runtime ~patch_id
                                                    "Conflict force-push \
                                                     rejected — lease violated"
                                              | Worktree.Push_worktree_missing
                                                ->
                                                  log_event runtime ~patch_id
                                                    (Printf.sprintf
                                                       "Worktree disappeared \
                                                        (%s) — conflict \
                                                        resolution will \
                                                        reconstruct on retry"
                                                       wt_path)
                                              | Worktree.Push_error msg ->
                                                  log_event runtime ~patch_id
                                                    (Printf.sprintf
                                                       "Conflict force-push \
                                                        failed — %s"
                                                       msg));
                                              Some result)
                                        in
                                        let resolution =
                                          Runtime.update_orchestrator_returning
                                            runtime (fun orch ->
                                              let orch, resolution =
                                                Orchestrator
                                                .apply_conflict_push_result orch
                                                  patch_id decision push_outcome
                                              in
                                              (orch, resolution))
                                        in
                                        match resolution with
                                        | Orchestrator.Conflict_done -> `Ok
                                        | Orchestrator.Conflict_retry_push ->
                                            log_event runtime ~patch_id
                                              "Re-enqueued conflict resolution \
                                               after push failure";
                                            `Retry_push
                                        | Orchestrator.Conflict_needs_agent ->
                                            (* [conflict_info] is [None] when
                                             the rebase returned [Ok]/[Noop]
                                             and only the push subsequently
                                             failed; we degrade to a
                                             no-recovery-section prompt
                                             rather than blocking delivery. *)
                                            deliver_to_agent ?conflict_info ()
                                        | Orchestrator.Conflict_give_up ->
                                            `Failed)
                                  | Patch_decision.Deliver
                                      {
                                        payload =
                                          ( Patch_decision.Human_payload _
                                          | Patch_decision.Ci_payload _
                                          | Patch_decision.Review_payload _
                                          | Patch_decision.Findings_payload _
                                          | Patch_decision.Pr_body_payload ) as
                                          payload;
                                        base_change;
                                      } ->
                                      let pr_number =
                                        agent.Patch_agent.pr_number
                                      in
                                      let base_changed_prefix =
                                        render_base_changed_prefix base_change
                                      in
                                      (* Resolve the patch + base branch for
                                       layered prompt prefixes (Ci, Review).
                                       Ad-hoc PRs have no gameplan-defined
                                       patch — [patch_for_layer] is then
                                       [None] and the renderers omit the
                                       gameplan+patch prefix. *)
                                      let patch_for_layer =
                                        Base.List.find gameplan.Gameplan.patches
                                          ~f:(fun (p : Patch.t) ->
                                            Patch_id.equal p.Patch.id patch_id)
                                      in
                                      let base_branch_for_layer =
                                        Base.Option.value_map
                                          agent.Patch_agent.base_branch
                                          ~default:(Branch.to_string main)
                                          ~f:Branch.to_string
                                      in
                                      let wt_path =
                                        WS.resolve_worktree_path ~patch_id
                                          ~agent ()
                                      in
                                      let agents_md =
                                        read_optional_file
                                          (Stdlib.Filename.concat wt_path
                                             "AGENTS.md")
                                      in
                                      log_event runtime ~patch_id
                                        (match payload with
                                        | Patch_decision.Review_payload
                                            { comments } ->
                                            Printf.sprintf "Delivering %s (%s)"
                                              (Operation_kind.to_label kind)
                                              (pluralize
                                                 (Base.List.length comments)
                                                 "comment")
                                        | Patch_decision.Findings_payload
                                            { findings } ->
                                            Printf.sprintf "Delivering %s (%s)"
                                              (Operation_kind.to_label kind)
                                              (pluralize
                                                 (Base.List.length findings)
                                                 "finding")
                                        | Patch_decision.Human_payload
                                            { messages } ->
                                            Printf.sprintf "Delivering %s (%s)"
                                              (Operation_kind.to_label kind)
                                              (pluralize
                                                 (Base.List.length messages)
                                                 "message")
                                        | Patch_decision.Ci_payload _
                                        | Patch_decision.Pr_body_payload
                                        | Patch_decision.Merge_conflict_payload
                                          ->
                                            Printf.sprintf "Delivering %s"
                                              (Operation_kind.to_label kind));
                                      let prompt =
                                        match payload with
                                        | Patch_decision.Ci_payload
                                            { failed_checks } ->
                                            if Base.List.is_empty failed_checks
                                            then
                                              Prompt
                                              .render_ci_failure_unknown_prompt
                                                ~project_name ?agents_md
                                                ?pr_number
                                                ?patch:patch_for_layer ~gameplan
                                                ~base_branch:
                                                  base_branch_for_layer ()
                                            else
                                              Prompt.render_ci_failure_prompt
                                                ~project_name ?agents_md
                                                ?pr_number
                                                ?patch:patch_for_layer ~gameplan
                                                ~base_branch:
                                                  base_branch_for_layer
                                                failed_checks
                                        | Patch_decision.Review_payload
                                            { comments } ->
                                            let current_head_sha =
                                              Base.Option.bind fresh_pr_state
                                                ~f:(fun ps ->
                                                  ps.Pr_state.head_oid)
                                            in
                                            Prompt.render_review_prompt
                                              ~project_name ?agents_md
                                              ?pr_number ?current_head_sha
                                              ?patch:patch_for_layer ~gameplan
                                              ~base_branch:base_branch_for_layer
                                              comments
                                        | Patch_decision.Findings_payload
                                            { findings } ->
                                            let current_head_sha =
                                              Base.Option.bind fresh_pr_state
                                                ~f:(fun ps ->
                                                  ps.Pr_state.head_oid)
                                            in
                                            let artifact_path =
                                              Project_store
                                              .findings_wontfix_artifact_path
                                                ~project_name ~patch_id
                                            in
                                            Project_store.ensure_dir
                                              (Stdlib.Filename.dirname
                                                 artifact_path);
                                            (try Unix.unlink artifact_path
                                             with
                                             | Unix.Unix_error
                                                 (Unix.ENOENT, _, _)
                                             ->
                                               ());
                                            Prompt.render_findings_prompt
                                              ~project_name ?agents_md
                                              ?pr_number ?current_head_sha
                                              ?patch:patch_for_layer ~gameplan
                                              ~base_branch:base_branch_for_layer
                                              ~artifact_path findings
                                        | Patch_decision.Human_payload
                                            { messages } ->
                                            Prompt.render_human_message_prompt
                                              ~project_name messages
                                        | Patch_decision.Pr_body_payload ->
                                            let patch =
                                              Base.List.find_exn
                                                gameplan.Gameplan.patches
                                                ~f:(fun (p : Patch.t) ->
                                                  Patch_id.equal p.Patch.id
                                                    patch_id)
                                            in
                                            let pr_body =
                                              Prompt.render_pr_description
                                                ~project_name patch gameplan
                                            in
                                            let spec_suffix =
                                              Prompt.render_spec_suffix patch
                                                gameplan
                                            in
                                            let artifact_path =
                                              Project_store
                                              .pr_body_artifact_path
                                                ~project_name ~patch_id
                                            in
                                            Project_store.ensure_dir
                                              (Stdlib.Filename.dirname
                                                 artifact_path);
                                            (* Clear any stale artifact from a
                                             prior Pr_body session. The path
                                             is stable per-patch, so without
                                             this the classifier could read
                                             outdated notes when the current
                                             session doesn't write (blocked,
                                             no-op, or legitimately chose not
                                             to). *)
                                            (try Unix.unlink artifact_path
                                             with
                                             | Unix.Unix_error
                                                 (Unix.ENOENT, _, _)
                                             ->
                                               ());
                                            Prompt.render_pr_body_prompt
                                              ~project_name
                                              ~pr_number:
                                                (Base.Option.value_exn pr_number)
                                              ~pr_body ~spec_suffix
                                              ~artifact_path
                                        | Patch_decision.Merge_conflict_payload
                                          ->
                                            (* Invariant: Merge_conflict is handled
                                         in the dedicated match arm above *)
                                            assert false
                                      in
                                      let prompt =
                                        if String.equal base_changed_prefix ""
                                        then prompt
                                        else base_changed_prefix ^ "\n" ^ prompt
                                      in
                                      let on_pr_detected _pr_number = () in
                                      let base =
                                        Base.Option.value_map
                                          agent.Patch_agent.base_branch
                                          ~default:(Branch.to_string main)
                                          ~f:Branch.to_string
                                      in
                                      (* Lock in CI run dedup before firing the
                                       session. Recording pre-flight (rather
                                       than post-) means a session that starts
                                       but later fails still counts as
                                       "delivered", so we don't re-nag the
                                       agent with the same failing run on the
                                       next tick. *)
                                      (match payload with
                                      | Patch_decision.Ci_payload
                                          { failed_checks } ->
                                          let ids =
                                            Base.List.filter_map failed_checks
                                              ~f:(fun (c : Ci_check.t) ->
                                                c.Ci_check.id)
                                          in
                                          if not (Base.List.is_empty ids) then
                                            Runtime.update_orchestrator runtime
                                              (fun orch ->
                                                Orchestrator
                                                .record_delivered_ci_run_ids
                                                  orch patch_id ids)
                                      | Patch_decision.Human_payload _
                                      | Patch_decision.Review_payload _
                                      | Patch_decision.Findings_payload _
                                      | Patch_decision.Pr_body_payload
                                      | Patch_decision.Merge_conflict_payload ->
                                          ());
                                      (* Snapshot the pr-body artifact before
                                       the session so the post-session sync
                                       step (after Session_driver.run) can
                                       detect content changes from any kind
                                       of session, not only Pr_body. For
                                       Pr_body the artifact was just unlinked
                                       above, so the snapshot is None. *)
                                      let pr_body_pre_snapshot =
                                        read_artifact_file
                                          (Project_store.pr_body_artifact_path
                                             ~project_name ~patch_id)
                                      in
                                      let complexity =
                                        patch_complexity ~gameplan ~patch_id
                                      in
                                      let gameplan_prompt =
                                        Prompt.render_gameplan_layer
                                          ~project_name gameplan
                                      in
                                      let patch_prompt =
                                        match patch_for_layer with
                                        | Some p ->
                                            let functional_changes =
                                              Prompt.owned_functional_changes
                                                gameplan p
                                            in
                                            Prompt.render_patch_layer
                                              ~project_name p ?pr_number
                                              ~functional_changes
                                              ~base_branch:base_branch_for_layer
                                              ()
                                        | None -> ""
                                      in
                                      let result, tool_failures =
                                        run_llm_session ~sw ~gameplan_prompt
                                          ~patch_prompt ~kind:(Some kind)
                                          ~patch_id ~prompt ~agent
                                          ~on_pr_detected ~complexity
                                      in
                                      let result =
                                        (result
                                          :> [ `Failed
                                             | `Ok
                                             | `Pr_body_miss
                                             | `Retry_push ])
                                      in
                                      (match result with
                                      | `Ok when Base.Option.is_some base_change
                                        ->
                                          Runtime.update_orchestrator runtime
                                            (fun orch ->
                                              Orchestrator
                                              .set_notified_base_branch orch
                                                patch_id (Branch.of_string base))
                                      | _ -> ());
                                      (* Artifact-driven phase (Pr_body): read
                                   the agent's artifact and PATCH the PR body.
                                   When the artifact is missing AND we saw a
                                   Write tool call that did not complete, the
                                   agent was likely blocked mid-call (e.g. by
                                   OpenCode's --dir sandbox) — signal retry
                                   via Respond_pr_body_miss so the reconciler
                                   re-enqueues Pr_body once before escalating
                                   to needs_intervention. When the artifact is
                                   missing but we saw no Write failure, the
                                   agent legitimately chose not to add notes:
                                   fall through to Respond_ok as before. *)
                                      let session_ok =
                                        match result with
                                        | `Ok -> true
                                        | _ -> false
                                      in
                                      let result =
                                        match payload with
                                        | Patch_decision.Pr_body_payload
                                          when session_ok -> (
                                            let pr =
                                              Base.Option.value_exn pr_number
                                            in
                                            let patch =
                                              Base.List.find_exn
                                                gameplan.Gameplan.patches
                                                ~f:(fun (p : Patch.t) ->
                                                  Patch_id.equal p.Patch.id
                                                    patch_id)
                                            in
                                            let artifact_outcome =
                                              apply_pr_body_artifact ~runtime
                                                ~project_name ~patch_id
                                                ~pr_number:pr ~patch ~gameplan
                                            in
                                            match
                                              Patch_decision
                                              .classify_pr_body_respond
                                                ~artifact_outcome ~tool_failures
                                            with
                                            | `Pr_body_miss -> `Pr_body_miss
                                            | `Ok -> result)
                                        | Patch_decision.Findings_payload
                                            { findings } ->
                                            if session_ok then (
                                              let artifact_path =
                                                Project_store
                                                .findings_wontfix_artifact_path
                                                  ~project_name ~patch_id
                                              in
                                              Findings_resolver
                                              .resolve_after_session
                                                ~review_clients
                                                ~log:(fun msg ->
                                                  log_event runtime ~patch_id
                                                    msg)
                                                ~findings_registry
                                                ~artifact_path
                                                ~delivered:findings
                                                ~actor:
                                                  (Printf.sprintf "onton:%s"
                                                     (Patch_id.to_string
                                                        patch_id))
                                                ();
                                              result)
                                            else
                                              let ids =
                                                Base.List.map findings
                                                  ~f:(fun
                                                      (f :
                                                        Review_service.finding)
                                                    -> f.Review_service.id)
                                              in
                                              log_event runtime ~patch_id
                                                (Printf.sprintf
                                                   "Session failed before \
                                                    resolving findings; \
                                                    forgetting delivered \
                                                    finding registry entries: \
                                                    %s"
                                                   (String.concat ", " ids));
                                              Base.List.iter findings
                                                ~f:(fun
                                                    (f : Review_service.finding)
                                                  ->
                                                  Findings_registry.forget
                                                    findings_registry
                                                    ~key:f.Review_service.id);
                                              result
                                        | Patch_decision.Human_payload _
                                        | Patch_decision.Ci_payload _
                                        | Patch_decision.Review_payload _
                                        | Patch_decision.Pr_body_payload
                                        | Patch_decision.Merge_conflict_payload
                                          ->
                                            result
                                      in
                                      (* Opportunistic pr-body sync. If the
                                       agent updated the artifact during a
                                       non-Pr_body session, PATCH the PR.
                                       Pure planner returns Sync_skip for
                                       kind=Pr_body (its delivery path is
                                       owned by classify_pr_body_respond),
                                       so this block is a no-op for that
                                       kind and never double-PATCHes. *)
                                      (* Re-derive session_ok from the (possibly
                                       rebound) result so the planner sees the
                                       final outcome — the Pr_body_payload arm
                                       above can flip result to Pr_body_miss. *)
                                      let session_ok =
                                        match result with
                                        | `Ok -> true
                                        | _ -> false
                                      in
                                      let pr_body_post_snapshot =
                                        read_artifact_file
                                          (Project_store.pr_body_artifact_path
                                             ~project_name ~patch_id)
                                      in
                                      let plan =
                                        Patch_decision.plan_artifact_sync ~kind
                                          ~session_ok ~pre:pr_body_pre_snapshot
                                          ~post:pr_body_post_snapshot
                                      in
                                      let patch_result =
                                        match plan with
                                        | Patch_decision.Sync_skip -> None
                                        | Patch_decision.Sync_attempt_pr_body
                                          -> (
                                            let pr =
                                              match pr_number with
                                              | Some n -> n
                                              | None ->
                                                  (* Invariant: Sync_attempt_pr_body
                                                   is only reachable inside the
                                                   Deliver arm, which requires a
                                                   PR to exist. *)
                                                  failwith
                                                    (Printf.sprintf
                                                       "BUG: \
                                                        Sync_attempt_pr_body \
                                                        reached with no \
                                                        pr_number for %s"
                                                       (Patch_id.to_string
                                                          patch_id))
                                            in
                                            (* Ad-hoc agents (added via
                                             Orchestrator.add_agent) have a
                                             pr_number but no gameplan entry,
                                             so a missing patch here is not a
                                             bug — skip the opportunistic
                                             sync and let
                                             classify_artifact_sync_outcome
                                             map patch_result=None to
                                             Sync_no_op. *)
                                            match
                                              Base.List.find
                                                gameplan.Gameplan.patches
                                                ~f:(fun (p : Patch.t) ->
                                                  Patch_id.equal p.Patch.id
                                                    patch_id)
                                            with
                                            | Some patch ->
                                                Some
                                                  (apply_pr_body_artifact
                                                     ~runtime ~project_name
                                                     ~patch_id ~pr_number:pr
                                                     ~patch ~gameplan)
                                            | None ->
                                                log_event runtime ~patch_id
                                                  "pr-body: skipping \
                                                   opportunistic sync — patch \
                                                   has no gameplan entry \
                                                   (likely ad-hoc)";
                                                None)
                                      in
                                      (match
                                         Patch_decision
                                         .classify_artifact_sync_outcome ~plan
                                           ~patch_result
                                       with
                                      | Patch_decision.Sync_no_op -> ()
                                      | Patch_decision.Sync_delivered ->
                                          log_event runtime ~patch_id
                                            (Printf.sprintf
                                               "pr-body: synced \
                                                opportunistically from %s \
                                                session"
                                               (Operation_kind.to_label kind));
                                          Runtime.update_orchestrator runtime
                                            (fun orch ->
                                              let orch =
                                                Orchestrator
                                                .set_pr_body_delivered orch
                                                  patch_id true
                                              in
                                              Orchestrator
                                              .reset_pr_body_artifact_miss_count
                                                orch patch_id)
                                      | Patch_decision.Sync_patch_failed ->
                                          log_event runtime ~patch_id
                                            "pr-body: opportunistic sync PATCH \
                                             failed; leaving state — next \
                                             Pr_body cycle will retry");
                                      (result
                                        :> [ `Failed
                                           | `Ok
                                           | `Pr_body_miss
                                           | `Retry_push
                                           | `Skip_empty
                                           | `Stale ]))
                        in
                        let respond_outcome =
                          match result with
                          | `Stale -> Orchestrator.Respond_stale
                          | `Skip_empty -> Orchestrator.Respond_skip_empty
                          | `Failed -> Orchestrator.Respond_failed
                          | `Retry_push -> Orchestrator.Respond_retry_push
                          | `Pr_body_miss -> Orchestrator.Respond_pr_body_miss
                          | `Ok -> Orchestrator.Respond_ok
                        in
                        Runtime.update_orchestrator runtime (fun orch ->
                            Orchestrator.apply_respond_outcome orch patch_id
                              kind respond_outcome);
                        match respond_outcome with
                        | Orchestrator.Respond_failed ->
                            let agent =
                              Runtime.read runtime (fun snap ->
                                  Orchestrator.agent snap.Runtime.orchestrator
                                    patch_id)
                            in
                            if Patch_agent.needs_intervention agent then
                              set_status ~level:Tui.Error
                                ~text:
                                  (Printf.sprintf
                                     "Patch %s: session failed — human review \
                                      needed"
                                     (Patch_id.to_string patch_id))
                                ()
                        | Orchestrator.Respond_pr_body_miss ->
                            (* Triggered by classify_pr_body_respond for either
                             [`Patch_failed] (notes written, GitHub PATCH call
                             failed) or [`Missing | `Empty] + observed Write
                             tool failure (blocked mid-Write). The specific
                             cause is already logged by apply_pr_body_artifact;
                             keep this line cause-agnostic so it doesn't
                             mislabel a [`Patch_failed] miss as a Write
                             failure. The reconciler re-enqueues Pr_body; at
                             cap (>=2) needs_intervention fires. *)
                            let agent =
                              Runtime.read runtime (fun snap ->
                                  Orchestrator.agent snap.Runtime.orchestrator
                                    patch_id)
                            in
                            log_event runtime ~patch_id
                              (Printf.sprintf
                                 "pr-body: miss recorded (miss count: %d)%s"
                                 agent.Patch_agent.pr_body_artifact_miss_count
                                 (if Patch_agent.needs_intervention agent then
                                    "; escalating to human review"
                                  else "; will re-enqueue"));
                            if Patch_agent.needs_intervention agent then
                              set_status ~level:Tui.Error
                                ~text:
                                  (Printf.sprintf
                                     "Patch %s: pr-body artifact repeatedly \
                                      missing — human review needed"
                                     (Patch_id.to_string patch_id))
                                ()
                        | Orchestrator.Respond_ok ->
                            if
                              Operation_kind.equal kind
                                Operation_kind.Merge_conflict
                            then
                              set_status ~level:Tui.Info
                                ~text:
                                  (Printf.sprintf
                                     "Patch %s: conflict resolved, rebasing…"
                                     (Patch_id.to_string patch_id))
                                ~expires_at:(Unix.gettimeofday () +. 10.0)
                                ()
                        | Orchestrator.Respond_stale
                        | Orchestrator.Respond_retry_push
                        | Orchestrator.Respond_skip_empty ->
                            ())))
      in
      (* Spawn action fibers without waiting for completion. Each fiber is
       guarded by with_busy_guard (ensures complete on exit) and
       with_session_slot (semaphore backpressure). The runner loop continues
       immediately to pick up newly-queued actions from the poller. *)
      Base.List.iter action_fibers ~f:(fun f ->
          Eio.Fiber.fork_daemon ~sw (fun () ->
              f ();
              `Stop_daemon));
      Eio.Time.sleep clock 1.0;
      loop sw
    in
    Eio.Switch.run @@ fun sw ->
    (* Single long-lived automerge fiber. Spawning fork_daemon once before the
     loop (rather than per-tick from inside [loop]) keeps at most one automerge
     fiber alive on the switch — per-tick forking would let brief no-op fibers
     accumulate during a slow GitHub call even though [automerge_inflight]
     guards the real merge. The fiber paces itself with its own 1s sleep,
     independent of the runner tick. *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
        let rec amloop () =
          (* Top-level guard: [reconcile_and_execute_automerge] catches
           exceptions per-decision, but [Eio.Fiber.List.iter] (or a future
           refactor) could still let one escape. Re-raise [Cancelled] so
           switch teardown propagates normally; swallow any other exception
           to the activity log so a single bad tick can't kill the fiber
           permanently and leave automerge silently disabled. *)
          (try reconcile_and_execute_automerge ~runtime with
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | exn ->
              log_event runtime
                (Printf.sprintf "automerge fiber error — %s"
                   (Printexc.to_string exn)));
          (* [Eio.Time.sleep] is the only yield point outside the guard above.
           On switch teardown it raises [Cancelled]; catching it here and
           returning [`Stop_daemon] gives the [fork_daemon] contract a clean
           voluntary-exit signal (matching the other [fork_daemon] callers
           in this file) rather than exiting by exception. *)
          match
            try Ok (Eio.Time.sleep clock 1.0)
            with Eio.Cancel.Cancelled _ -> Error `Cancelled
          with
          | Error `Cancelled -> `Stop_daemon
          | Ok () -> amloop ()
        in
        amloop ());
    loop sw
end

(** {1 Main entry point} *)

(** Try to load a persisted snapshot for a project. *)
let load_snapshot ~project_name =
  let path = Project_store.snapshot_path project_name in
  if Stdlib.Sys.file_exists path then
    match Persistence.load ~path with
    | Ok snap -> Ok (Some snap)
    | Error msg -> Error msg
  else Ok None

(** Resolve owner/repo/token with CLI flags, falling back to git remote and
    [gh auth token] when flags are empty. *)
let resolve_github_credentials ~github_token ~repo_root =
  let token =
    let t = Base.String.strip github_token in
    if Base.String.is_empty t then Managed_repo.infer_github_token () else t
  in
  let owner, repo =
    let module Repo = (val Repo_git.make ~repo_root) in
    match Repo.infer_owner_repo () with
    | Some (o, r) -> (o, r)
    | None -> ("", "")
  in
  (token, owner, repo)

(** Attach persisted snapshot to a resolved config, propagating load errors. *)
let with_snapshot_load ~project_name config gameplan =
  match load_snapshot ~project_name with
  | Ok existing_snapshot -> Ok (config, gameplan, existing_snapshot)
  | Error msg ->
      Error
        [
          Printf.sprintf "Error loading snapshot for project %S: %s"
            project_name msg;
        ]

(** Resolve CLI args into a config ready to run.
    - [--gameplan] provided: parse it, persist config + gameplan source, derive
      project name.
    - [PROJECT] only: load stored config + gameplan. CLI flags override stored
      values. *)
let resolve_config ~project ~gameplan_path ~github_token ~backend ~model
    ~main_branch ~poll_interval ~(repo_root : string option) ~max_concurrency
    ~headless =
  let repo_root_for_fresh =
    Repo_root.normalize (Base.Option.value repo_root ~default:".")
  in
  let resolve_branch ~repo_root mb_opt =
    match mb_opt with
    | Some b -> b
    | None ->
        let module Repo = (val Repo_git.make ~repo_root) in
        Repo.infer_default_branch ()
  in
  match (project, gameplan_path) with
  | None, None ->
      let repo_root = repo_root_for_fresh in
      let token, owner, repo =
        resolve_github_credentials ~github_token ~repo_root
      in
      let project_name =
        if Base.String.is_empty owner || Base.String.is_empty repo then "adhoc"
        else Printf.sprintf "%s-%s" owner repo
      in
      let gameplan : Gameplan.t =
        {
          project_name;
          repo_owner = owner;
          repo_name = repo;
          problem_statement = "";
          solution_summary = "";
          final_state_spec = "";
          patches = [];
          current_state_analysis = "";
          explicit_opinions = "";
          acceptance_criteria = [];
          open_questions = [];
          functional_changes = [];
          context_resources = [];
        }
      in
      let backend, model = resolve_backend_model ~backend ~model in
      let main_branch = resolve_branch ~repo_root main_branch in
      Project_store.save_config ~project_name ~github_token:token
        ~github_owner:owner ~github_repo:repo ~backend ~model
        ~main_branch:(Branch.to_string main_branch)
        ~poll_interval ~repo_root ~max_concurrency;
      let config =
        {
          project = Some project_name;
          backend;
          model;
          github_token = token;
          github_owner = owner;
          github_repo = repo;
          main_branch;
          poll_interval;
          repo_root;
          max_concurrency;
          headless;
          user_config = User_config.load ~github_owner:owner ~github_repo:repo;
        }
      in
      with_snapshot_load ~project_name config gameplan
  | _, Some gp_path -> (
      match Gameplan_parser.parse_file gp_path with
      | Error msg -> Error [ Printf.sprintf "Error parsing gameplan: %s" msg ]
      | Ok parsed -> (
          let gameplan = parsed.Gameplan_parser.gameplan in
          let project_name =
            match project with
            | Some p -> p
            | None -> gameplan.Gameplan.project_name
          in
          let owner = Base.String.strip gameplan.Gameplan.repo_owner in
          let repo = Base.String.strip gameplan.Gameplan.repo_name in
          let target_error =
            if Base.String.is_empty owner || Base.String.is_empty repo then
              Some
                (Printf.sprintf
                   "Gameplan %s is missing required top-level `owner` and/or \
                    `repo`. Every gameplan must declare exactly one repository \
                    — see skills/write-gameplan/SKILL.md §\"One Repo Per \
                    Gameplan\"."
                   gp_path)
            else
              match Github_target.validate_target ~owner ~repo with
              | Ok () -> None
              | Error msg ->
                  Some
                    (Printf.sprintf
                       "Gameplan %s declares an invalid GitHub target: %s"
                       gp_path msg)
          in
          match target_error with
          | Some msg -> Error [ msg ]
          | None -> (
              (* [--repo] is ignored when [--gameplan] is passed: the gameplan
               itself is the source of truth for which repo to operate on, and
               onton manages its own checkout under the project data dir. *)
              (match repo_root with
              | Some user_repo when not (Base.String.is_empty user_repo) ->
                  Printf.eprintf
                    "onton: --repo %s ignored when --gameplan is set; using \
                     onton-managed checkout for %s/%s\n\
                     %!"
                    user_repo owner repo
              | _ -> ());
              let token =
                let t = Base.String.strip github_token in
                if Base.String.is_empty t then
                  Managed_repo.infer_github_token ()
                else t
              in
              match
                Managed_repo.ensure_managed_repo ~project_name ~token ~owner
                  ~repo
              with
              | Error msg ->
                  Error
                    [
                      Printf.sprintf
                        "Could not prepare onton-managed checkout for %s/%s: %s"
                        owner repo msg;
                    ]
              | Ok repo_root ->
                  let backend, model = resolve_backend_model ~backend ~model in
                  let main_branch = resolve_branch ~repo_root main_branch in
                  Project_store.save_config ~project_name ~github_token:token
                    ~github_owner:owner ~github_repo:repo ~backend ~model
                    ~main_branch:(Branch.to_string main_branch)
                    ~poll_interval ~repo_root ~max_concurrency;
                  Project_store.save_gameplan_source ~project_name
                    ~source_path:gp_path;
                  let config =
                    {
                      project = Some project_name;
                      backend;
                      model;
                      github_token = token;
                      github_owner = owner;
                      github_repo = repo;
                      main_branch;
                      poll_interval;
                      repo_root;
                      max_concurrency;
                      headless;
                      user_config =
                        User_config.load ~github_owner:owner ~github_repo:repo;
                    }
                  in
                  with_snapshot_load ~project_name config gameplan)))
  | Some proj, None -> (
      if not (Project_store.project_exists proj) then
        Error
          [
            Printf.sprintf
              "No stored project %S. Use --gameplan to start a new project."
              proj;
          ]
      else
        let stored_gp_path = Project_store.stored_gameplan_path proj in
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
                  (* CLI flags override stored config; stored config overrides
                     git-remote inference *)
                  let merge_cli_stored cli stored_val =
                    let c = Base.String.strip cli in
                    if Base.String.is_empty c then stored_val else c
                  in
                  let resolved_backend_str =
                    merge_cli_stored backend stored.Project_store.backend
                  in
                  let resolved_model_str =
                    merge_cli_stored model stored.Project_store.model
                  in
                  let backend, model =
                    resolve_backend_model ~backend:resolved_backend_str
                      ~model:resolved_model_str
                  in
                  let token_from_stored =
                    merge_cli_stored github_token
                      stored.Project_store.github_token
                  in
                  (* Always route through [Repo_root.normalize] — including
                     the stored value — so legacy configs that persisted a
                     worktree path (or a trailing [/.]) self-heal on load. *)
                  let repo_root =
                    match repo_root with
                    | Some rr -> Repo_root.normalize rr
                    | None -> Repo_root.normalize stored.Project_store.repo_root
                  in
                  let token, inferred_owner, inferred_repo =
                    resolve_github_credentials ~github_token:token_from_stored
                      ~repo_root
                  in
                  (* Precedence on resume: gameplan > stored config > inferred
                     from git remote. Gameplan-authored sessions have non-empty
                     owner/repo in the parsed gameplan and they are the
                     source-of-truth. Legacy sessions have empty values there
                     and fall through to the stored config (backfill path),
                     then to inference. *)
                  let pick_owner_repo gp stored_v inferred_v =
                    let s = Base.String.strip gp in
                    if not (Base.String.is_empty s) then s
                    else
                      let s = Base.String.strip stored_v in
                      if Base.String.is_empty s then inferred_v else s
                  in
                  let owner =
                    pick_owner_repo gameplan.Gameplan.repo_owner
                      stored.Project_store.github_owner inferred_owner
                  in
                  let repo =
                    pick_owner_repo gameplan.Gameplan.repo_name
                      stored.Project_store.github_repo inferred_repo
                  in
                  (* If the stored repo_root is the onton-managed checkout for
                     this project, refresh it from origin before continuing.
                     Best-effort: an offline resume should still proceed. *)
                  (if
                     String.equal repo_root
                       (Project_store.managed_repo_dir proj)
                   then
                     if
                       Base.String.is_empty (Base.String.strip owner)
                       || Base.String.is_empty (Base.String.strip repo)
                     then
                       Printf.eprintf
                         "onton: warning: stored project %S has no GitHub \
                          owner/repo; skipping managed checkout refresh\n\
                          %!"
                         proj
                     else
                       match
                         Managed_repo.ensure_managed_repo ~project_name:proj
                           ~token ~owner ~repo
                       with
                       | Ok _ -> ()
                       | Error msg ->
                           Printf.eprintf
                             "onton: warning: %s (resuming with local state)\n\
                              %!"
                             msg);
                  let branch =
                    match main_branch with
                    | Some b -> b
                    | None -> Branch.of_string stored.Project_store.main_branch
                  in
                  (* Persist the resolved config so the next launch without
                     CLI overrides picks up the current values. *)
                  Project_store.save_config ~project_name:proj
                    ~github_token:token ~github_owner:owner ~github_repo:repo
                    ~backend ~model ~main_branch:(Branch.to_string branch)
                    ~poll_interval:stored.Project_store.poll_interval ~repo_root
                    ~max_concurrency:stored.Project_store.max_concurrency;
                  let config =
                    {
                      project = Some proj;
                      backend;
                      model;
                      github_token = token;
                      github_owner = owner;
                      github_repo = repo;
                      main_branch = branch;
                      poll_interval = stored.Project_store.poll_interval;
                      repo_root;
                      max_concurrency = stored.Project_store.max_concurrency;
                      headless;
                      user_config =
                        User_config.load ~github_owner:owner ~github_repo:repo;
                    }
                  in
                  with_snapshot_load ~project_name:proj config gameplan))

let run_with_config ~no_lock (config : config) gameplan existing_snapshot =
  let project_name =
    match config.project with Some p -> p | None -> assert false
  in
  match
    validate_resolved_config ~backend:config.backend
      ~github_token:config.github_token ~github_owner:config.github_owner
      ~github_repo:config.github_repo ~main_branch:config.main_branch
      ~poll_interval:config.poll_interval
      ~max_concurrency:config.max_concurrency
  with
  | Error errs ->
      Base.List.iter errs ~f:(fun e -> Printf.eprintf "Error: %s\n" e);
      Stdlib.exit 1
  | Ok () ->
      Git_env.set_github_token config.github_token;
      let module Repo = (val Repo_git.make ~repo_root:config.repo_root) in
      (match Repo.validate_branch_resolves ~main_branch:config.main_branch with
      | Ok () -> ()
      | Error msg ->
          Printf.eprintf "Error: %s\n" msg;
          Stdlib.exit 1);
      (* Preflight: ensure RLIMIT_NOFILE is high enough for [max_concurrency]
         long-lived backend subprocesses (each holding 3 pipes = 6 FDs),
         parallel git subprocesses, HTTPS connections, the activity log,
         snapshot files, and the project lock. Budget 256 FDs/slot plus 512
         headroom. Auto-raise the soft limit to the hard cap silently; fail
         fast with an actionable [ulimit -n] message only if the effective
         limit is still insufficient. See issue #209. *)
      let () =
        let open Onton.Rlimit in
        let required = (config.max_concurrency * 256) + 512 in
        let cur = get_nofile () in
        if cur.soft < required then begin
          let after = try_raise_nofile_soft ~target:required in
          if after.soft < required then begin
            Printf.eprintf
              "onton: soft FD limit %d is below the required %d (hard cap %d).\n\
              \       Raise it with: ulimit -n %d\n\
              \       macOS system ceiling: sudo launchctl limit maxfiles \
               <soft> <hard>.\n\
               %!"
              after.soft required after.hard required;
            Stdlib.exit 1
          end
        end
      in
      let lock =
        if no_lock then None
        else
          let project_dir = Project_store.project_dir project_name in
          match
            Project_lock.acquire ~project_dir ~on_stale:(fun stale_pid ->
                if stale_pid > 0 then
                  Printf.eprintf "onton: reclaiming stale lock from pid %d\n%!"
                    stale_pid)
          with
          | Ok l -> Some l
          | Error e ->
              Printf.eprintf "onton: %s\n"
                (Format.asprintf "%a" Project_lock.pp_error e);
              Printf.eprintf
                "       pass --no-lock (or set ONTON_NO_LOCK=1) to bypass.\n%!";
              (* 75 = EX_TEMPFAIL from sysexits(3): transient, try again. *)
              Stdlib.exit 75
      in
      Stdlib.Fun.protect ~finally:(fun () ->
          Base.Option.iter lock ~f:Project_lock.release)
      @@ fun () ->
      Eio_main.run @@ fun env ->
      if config.headless then
        Sys.set_signal Sys.sigint
          (Sys.Signal_handle
             (fun _ ->
               Printf.eprintf "\nInterrupted.\n%!";
               (try Unix.kill 0 Sys.sigterm with Unix.Unix_error _ -> ());
               Stdlib.exit 130));
      let runtime =
        match existing_snapshot with
        | Some snap ->
            Printf.eprintf "Resuming project %S from saved state.\n%!"
              project_name;
            Runtime.create ~gameplan ~main_branch:config.main_branch
              ~snapshot:snap ()
        | None ->
            Printf.eprintf "Starting new project %S.\n%!" project_name;
            Runtime.create ~gameplan ~main_branch:config.main_branch ()
      in
      Unix.putenv "ONTON_SNAPSHOT_PATH"
        (Project_store.snapshot_path project_name);
      let net = Eio.Stdenv.net env in
      let clock = Eio.Stdenv.clock env in
      let forge =
        Github.make ~net ~clock ~token:config.github_token
          ~owner:config.github_owner ~repo:config.github_repo
      in
      let module Forge = (val forge) in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let worktree_client =
        Worktree.make ~process_mgr ~repo_root:config.repo_root
      in
      let module WorktreeClient = (val worktree_client) in
      (match Forge.check_repo_access () with
      | Ok () -> ()
      | Error err ->
          Printf.eprintf "Error: cannot access GitHub repo %s/%s: %s\n"
            config.github_owner config.github_repo (Github.show_error err);
          Stdlib.exit 1);
      let module Reconciler = Startup_reconciler.Make (Forge) (WorktreeClient)
      in
      let pr_registry = Pr_registry.create () in
      (* Seed registry from any agents that already have a PR number — covers
         both gameplan patches restored from a snapshot and ad-hoc agents. The
         reconciliation fiber and poller will overwrite/extend this as needed. *)
      Runtime.read runtime (fun snap ->
          Orchestrator.all_agents snap.Runtime.orchestrator)
      |> Base.List.iter ~f:(fun (agent : Patch_agent.t) ->
          Base.Option.iter agent.Patch_agent.pr_number ~f:(fun pr_number ->
              Pr_registry.register pr_registry
                ~patch_id:agent.Patch_agent.patch_id ~pr_number));
      let branch_of = build_branch_map gameplan ~default:config.main_branch in
      let session_timeout = 1800.0 in
      let setsid_exec =
        let candidate =
          match Sys.getenv_opt "ONTON_SETSID_EXEC" with
          | Some "" -> None
          | Some p -> Some p
          | None ->
              Some
                (Filename.concat
                   (Filename.dirname Sys.executable_name)
                   "onton-setsid-exec")
        in
        match candidate with
        | Some p when Sys.file_exists p -> Some p
        | Some p ->
            Eio.traceln
              "onton-setsid-exec not found at %s; grandchildren will reparent \
               to PID 1 on teardown"
              p;
            None
        | None -> None
      in
      let cli_model_opt =
        if Base.String.is_empty config.model then None else Some config.model
      in
      let repo_config =
        let config_dir =
          User_config.config_dir ~github_owner:config.github_owner
            ~github_repo:config.github_repo
        in
        match Repo_config.load ~config_dir ~known_backends () with
        | Ok t -> t
        | Error msg ->
            Printf.eprintf "Error: %s\n" msg;
            Stdlib.exit 1
      in
      (match
         Backend_preflight.validate ~default_backend:config.backend
           ~cli_model:cli_model_opt ~repo_config ()
       with
      | Ok () -> ()
      | Error errs ->
          Base.List.iter errs ~f:(fun e -> Printf.eprintf "Error: %s\n" e);
          Stdlib.exit 1);
      let registry =
        Backend_registry.create ~process_mgr ~clock ~timeout:session_timeout
          ~setsid_exec
      in
      (* [pick_backend ~complexity] resolves a per-patch (backend, model)
         tuple via the pure [Backend_routing.decide] and looks it up in the
         registry. Calling with [~complexity:None] yields the run's default
         backend (used for ad-hoc sessions and for the TUI display name). *)
      let pick_backend ~complexity =
        let ({ Backend_routing.backend; model } as decision) =
          Backend_routing.decide ~repo_config ~default_backend:config.backend
            ~cli_model:cli_model_opt ~complexity
        in
        (Backend_registry.get registry ~backend ~model, decision)
      in
      let default_backend = pick_backend ~complexity:None in
      let backend_name = function
        | Backend_registry.Ephemeral backend -> backend.Llm_backend.name
        | Backend_registry.Long_lived (Llm_backend_long_lived.T { name; _ }) ->
            name
      in
      (* Display-side counterpart to [pick_backend]: returns the routing
         decision with the [auto] sentinel resolved to the concrete model
         name, so the TUI shows what will actually run. *)
      let resolve_routing ~complexity : Backend_routing.decision =
        let dec : Backend_routing.decision =
          Backend_routing.decide ~repo_config ~default_backend:config.backend
            ~cli_model:cli_model_opt ~complexity
        in
        {
          dec with
          model =
            Backend_registry.resolve_model ~backend:dec.Backend_routing.backend
              ~model:dec.Backend_routing.model ~complexity;
        }
      in
      let stdout = Eio.Stdenv.stdout env in
      (* Capture agent state and worktree list BEFORE launching concurrent
         fibers, so the reconciler sees the pre-session state rather than racing
         with the runner which creates worktrees and sets agents busy. *)
      let pre_agents =
        Runtime.read runtime (fun snap ->
            Orchestrator.all_agents snap.Runtime.orchestrator)
      in
      let pre_worktrees, pre_wt_error =
        Reconciler.recover_worktrees ~patches:gameplan.Gameplan.patches
      in
      let reconciliation_fiber () =
        let startup =
          Reconciler.reconcile ~patches:gameplan.Gameplan.patches
            ~agents:pre_agents ~pre_recovered_worktrees:pre_worktrees ()
        in
        let errored_ids =
          Base.List.map startup.Startup_reconciler.errors
            ~f:(fun (patch_id, err) ->
              log_event runtime ~patch_id
                (Printf.sprintf "Startup discovery failed — %s" err);
              patch_id)
          |> Base.Hash_set.of_list (module Patch_id)
        in
        (* For errored patches, preserve any PR numbers from the persisted snapshot *)
        Runtime.read runtime (fun snap ->
            Orchestrator.all_agents snap.Runtime.orchestrator)
        |> Base.List.iter ~f:(fun (agent : Patch_agent.t) ->
            if Base.Hash_set.mem errored_ids agent.Patch_agent.patch_id then
              Base.Option.iter agent.Patch_agent.pr_number ~f:(fun pr_number ->
                  Pr_registry.register pr_registry
                    ~patch_id:agent.Patch_agent.patch_id ~pr_number));
        let open Startup_reconciler in
        Base.List.iter startup.discovered
          ~f:(fun
              { pr_number = pr; patch_id = pid; base_branch = base; merged } ->
            Runtime.update_orchestrator runtime (fun orch ->
                match Orchestrator.find_agent orch pid with
                | Some agent when Patch_agent.has_pr agent ->
                    Pr_registry.register pr_registry ~patch_id:pid ~pr_number:pr;
                    if merged then Orchestrator.mark_merged orch pid else orch
                | Some _ ->
                    Pr_registry.register pr_registry ~patch_id:pid ~pr_number:pr;
                    let orch =
                      Orchestrator.fire orch (Orchestrator.Start (pid, base))
                    in
                    let orch = Orchestrator.set_pr_number orch pid pr in
                    let orch = Orchestrator.complete orch pid in
                    if merged then Orchestrator.mark_merged orch pid else orch
                | None -> orch));
        Base.List.iter startup.reset_pending ~f:(fun patch_id ->
            log_event runtime ~patch_id
              "Reset stale busy agent from crashed session";
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.reset_busy orch patch_id));
        Base.List.iter startup.recovered_worktrees ~f:(fun wr ->
            log_event runtime ~patch_id:wr.worktree_patch_id
              (Printf.sprintf "Recovered worktree at %s" wr.worktree_path));
        Base.List.iter
          (startup.worktree_errors @ Base.Option.to_list pre_wt_error)
          ~f:(fun err ->
            log_event runtime (Printf.sprintf "Startup worktree error — %s" err))
      in
      let transcripts =
        let t = Hashtbl.create 16 in
        Runtime.read runtime (fun snap ->
            Base.Hashtbl.iteri snap.Runtime.transcripts ~f:(fun ~key ~data ->
                Hashtbl.replace t key data));
        t
      in
      let event_log =
        Event_log.create ~path:(Project_store.event_log_path project_name)
      in
      Telemetry_dispatch.register_sink (Event_log.sink event_log);
      Telemetry_dispatch.register_sink
        (Activity_log_sink.sink
           ~update:(Runtime.update_activity_log runtime)
           ());
      let review_clients =
        Base.List.map repo_config.Repo_config.review_backends ~f:(fun backend ->
            Review_service_client.make ~net ~clock ~backend)
      in
      let findings_registry = Findings_registry.create () in
      let worktree_mutex = Eio.Mutex.create () in
      let hook_mutex = Eio.Mutex.create () in
      let module Fiber_env : FIBER_ENV = struct
        let runtime = runtime
        let clock = clock
        let fs = Eio.Stdenv.fs env
        let process_mgr = process_mgr
        let config = config
        let project_name = project_name
        let pr_registry = pr_registry
        let findings_registry = findings_registry
        let review_clients = review_clients
        let transcripts = transcripts
        let event_log = event_log
        let branch_of = branch_of
        let pick_backend = pick_backend
        let worktree_mutex = worktree_mutex
        let hook_mutex = hook_mutex
      end in
      let module Fibers = Make_fibers (Forge) (WorktreeClient) (Fiber_env) in
      let open Fibers in
      let common_fibers =
        [
          reconciliation_fiber;
          (fun () -> poller_fiber (module Reconciler : STARTUP_RECONCILER));
          (fun () -> Persistence_fiber_runner.run ~transcripts ());
        ]
      in
      if config.headless then
        Eio.Fiber.all
          (Headless.run ~stdout :: (fun () -> runner_fiber ()) :: common_fibers)
      else
        let tui_state = Tui_state.create () in
        let raw_state = Term.Raw.enter () in
        Fun.protect
          ~finally:(fun () ->
            Term.Raw.clear_suspend_handlers ();
            Term.Raw.leave raw_state;
            Eio.Flow.copy_string (Tui.exit_tui ()) stdout;
            let snap = Runtime.snapshot_unsync runtime in
            ignore
              (Persistence.save
                 ~path:(Project_store.snapshot_path project_name)
                 snap))
          (fun () ->
            Term.Raw.install_suspend_handlers raw_state;
            try
              Eio.Fiber.all
                ((fun () ->
                   tui_fiber ~runtime ~clock ~stdout ~tui_state ~transcripts
                     ~backend_name:(backend_name (fst default_backend))
                     ~resolve_routing)
                :: (fun () ->
                  input_fiber ~runtime ~process_mgr ~tui_state ~pr_registry
                    ~project_name ~owner:config.github_owner
                    ~repo:config.github_repo ~resolve_routing)
                :: (fun () -> runner_fiber ~status_msg:tui_state.status_msg ())
                :: common_fibers)
            with Quit_tui -> ())

(** {1 Prune}

    Remove every persisted project whose gameplan patches are all marked merged
    in the saved snapshot. Operates only on the per-project data directory
    (snapshot, events, gameplan, config, artifacts) — git worktrees under
    [~/worktrees/<project>/] are left in place because they can contain
    user-visible state (build outputs, untracked files) that the user may want
    to inspect or reuse, and traversing a populated build tree is far slower
    than removing the small data directory. Pruned projects are reported with
    the worktree path so the user can decide whether to clean it up.

    Skips any project whose lock is held by a live process — pruning state out
    from under a running [onton] would invalidate its in-memory view. *)

let run ~project ~gameplan_path ~github_token ~backend ~model
    ~(main_branch : Branch.t option) ~poll_interval ~(repo_root : string option)
    ~max_concurrency ~headless ~no_lock =
  match
    resolve_config ~project ~gameplan_path ~github_token ~backend ~model
      ~main_branch ~poll_interval ~repo_root ~max_concurrency ~headless
  with
  | Error errs ->
      Base.List.iter errs ~f:(fun e -> Printf.eprintf "Error: %s\n" e);
      Stdlib.exit 1
  | Ok (config, gameplan, existing_snapshot) ->
      run_with_config ~no_lock config gameplan existing_snapshot

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

let backend_arg =
  let open Cmdliner in
  Arg.(
    value & opt string ""
    & info [ "backend" ] ~docv:"BACKEND"
        ~doc:
          "LLM backend to use: claude, codex, opencode, pi, gemini, or \
           patch-agent.")

let model_arg =
  let open Cmdliner in
  Arg.(
    value & opt string ""
    & info [ "model" ] ~docv:"MODEL"
        ~doc:
          "Model name to pass to the selected backend (e.g. [sonnet], [opus], \
           [sonnet-4-6] for claude; [gpt-5.5] for codex). The literal value \
           [auto] picks a model per patch from the gameplan's [complexity] \
           field (1/2/3 → cheap/standard/strongest tier of the selected \
           backend). The per-backend ladder can be overridden by writing \
           [~/.config/onton/<owner>/<repo>/config.json] with a [routing] map — \
           see lib/repo_config.mli for the schema. When omitted, onton does \
           not pass --model to the underlying CLI, so the backend's own \
           default applies.")

let repo_arg =
  let open Cmdliner in
  Arg.(
    value
    & opt (some string) None
    & info [ "repo" ] ~docv:"PATH"
        ~doc:"Path to the git repository (default: current directory).")

let main_branch_arg =
  let open Cmdliner in
  Arg.(
    value
    & opt (some string) None
    & info [ "main-branch" ] ~docv:"BRANCH"
        ~doc:"Main branch name. Auto-detected from the git remote when omitted.")

let poll_interval_arg =
  let open Cmdliner in
  Arg.(
    value & opt float 30.0
    & info [ "poll-interval" ] ~docv:"SECONDS"
        ~doc:"Polling interval in seconds (default: 30).")

let max_concurrency_arg =
  let open Cmdliner in
  Arg.(
    value & opt int 5
    & info [ "max-concurrency" ] ~docv:"N"
        ~doc:"Maximum number of concurrent backend sessions (default: 5)."
        ~env:(Cmd.Env.info "ONTON_MAX_CONCURRENCY"))

let headless_arg =
  let open Cmdliner in
  Arg.(
    value & flag
    & info [ "headless" ] ~doc:"Run without TUI (plain log output).")

let upload_debug_arg =
  let open Cmdliner in
  Arg.(
    value & flag
    & info [ "upload-debug" ]
        ~doc:
          "Upload project debug state for troubleshooting. Requires a project \
           name.")

let no_lock_arg =
  let open Cmdliner in
  Arg.(
    value & flag
    & info [ "no-lock" ]
        ~doc:
          "Bypass the per-project advisory lock. Only use when a stale lock \
           cannot be reclaimed automatically."
        ~env:(Cmd.Env.info "ONTON_NO_LOCK"))

let prune_arg =
  let open Cmdliner in
  Arg.(
    value & flag
    & info [ "prune" ]
        ~doc:
          "Remove every stored project whose gameplan patches are all merged. \
           Skips projects whose lock is held by a live onton process. Does not \
           require any other arguments.")

let main_cmd =
  let open Cmdliner in
  let run_cmd project gameplan_path github_token backend model main_branch
      poll_interval repo_root max_concurrency headless upload_debug no_lock
      prune =
    if prune then Stdlib.exit (Prune_runner.run_prune ())
    else if upload_debug then (
      match project with
      | None ->
          Printf.eprintf
            "Error: --upload-debug requires a project name.\n\
             Usage: onton PROJECT --upload-debug\n";
          Stdlib.exit 1
      | Some project_name ->
          if not (Project_store.project_exists project_name) then (
            Printf.eprintf "Error: no stored project %S.\n" project_name;
            Printf.eprintf "Known projects: %s\n"
              (String.concat ", " (Project_store.list_projects ()));
            Stdlib.exit 1);
          Eio_main.run @@ fun env ->
          Debug_upload.run ~net:(Eio.Stdenv.net env) ~project_name
            ~version:Version.s)
    else
      let main_branch =
        Base.Option.map main_branch ~f:(fun s ->
            Branch.of_string (Base.String.strip s))
      in
      run ~project ~gameplan_path ~github_token
        ~backend:(Base.String.strip backend)
        ~model:(Base.String.strip model) ~main_branch ~poll_interval ~repo_root
        ~max_concurrency ~headless ~no_lock
  in
  let term =
    Term.(
      const run_cmd $ project_arg $ gameplan_path_arg $ github_token_arg
      $ backend_arg $ model_arg $ main_branch_arg $ poll_interval_arg $ repo_arg
      $ max_concurrency_arg $ headless_arg $ upload_debug_arg $ no_lock_arg
      $ prune_arg)
  in
  let info =
    Cmd.info "onton" ~version:Version.s
      ~doc:
        "Orchestrate parallel patch development with an LLM coding agent.\n\n\
         Usage:\n\
        \  onton [PROJECT] --gameplan GAMEPLAN [OPTIONS]   Start a new project\n\
        \  onton PROJECT [OPTIONS]                         Resume a saved \
         project"
  in
  Cmd.v info term

let () = Stdlib.exit (Cmdliner.Cmd.eval main_cmd)
