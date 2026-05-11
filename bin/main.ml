open Onton
open Onton_core
open Onton_core.Types

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

(** Run a subprocess, capture stdout, and guarantee the pipe is closed on any
    exception path. Returns the exit status and captured output, or [None] if
    opening the pipe itself raised. *)
let read_process_capture open_ic =
  match open_ic () with
  | exception _ -> None
  | ic ->
      let status = ref None in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          if Option.is_none !status then
            try ignore (Unix.close_process_in ic) with _ -> ())
        (fun () ->
          let buf = Buffer.create 128 in
          (try
             while true do
               Buffer.add_char buf (input_char ic)
             done
           with End_of_file -> ());
          let s = Unix.close_process_in ic in
          status := Some s;
          Some (s, Buffer.contents buf))

type process_capture = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let read_channel_all ic =
  let buf = Buffer.create 128 in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file -> ());
  Buffer.contents buf

(** Run [git -C repo_root ...] using argv rather than the shell, capture stdout
    and stderr, and close all process pipes on exception paths. *)
let run_git_capture ~repo_root args =
  let argv = Array.of_list ("git" :: "-C" :: repo_root :: args) in
  let env = Git_env.clean_env () in
  match Unix.open_process_args_full "git" argv env with
  | exception _ -> None
  | in_ch, out_ch, err_ch ->
      let status = ref None in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          if Option.is_none !status then
            try ignore (Unix.close_process_full (in_ch, out_ch, err_ch))
            with _ -> ())
        (fun () ->
          close_out_noerr out_ch;
          let stdout = read_channel_all in_ch in
          let stderr = read_channel_all err_ch in
          let s = Unix.close_process_full (in_ch, out_ch, err_ch) in
          status := Some s;
          Some { status = s; stdout; stderr })

let git_stdout ~repo_root args =
  match run_git_capture ~repo_root args with
  | Some { status = Unix.WEXITED 0; stdout; _ } ->
      Some (Base.String.strip stdout)
  | Some { status = Unix.WEXITED _; _ }
  | Some { status = Unix.WSIGNALED _; _ }
  | Some { status = Unix.WSTOPPED _; _ }
  | None ->
      None

let git_success ~repo_root args = Option.is_some (git_stdout ~repo_root args)

let format_git_failure { stdout; stderr; _ } =
  let detail =
    [ stderr; stdout ]
    |> Base.List.map ~f:(fun s -> Base.String.strip s)
    |> Base.List.filter ~f:(fun s -> not (Base.String.is_empty s))
    |> String.concat "\n"
  in
  if Base.String.is_empty detail then "no output" else detail

(** Infer GitHub owner/repo from [git remote get-url origin] in [repo_root].
    Parses both HTTPS and SSH remote URLs. Uses argv (no shell). *)
let infer_owner_repo ~repo_root =
  try
    match run_git_capture ~repo_root [ "remote"; "get-url"; "origin" ] with
    | Some { status = Unix.WEXITED 0; stdout; _ } -> (
        let url = Base.String.strip stdout in
        let re =
          Re.Pcre.re {|github\.com[:/]([^/]+)/([^/\s]+?)(?:\.git)?/?$|}
          |> Re.compile
        in
        match Re.exec_opt re url with
        | Some g -> Some (Re.Group.get g 1, Re.Group.get g 2)
        | None -> None)
    | Some { status = Unix.WEXITED _; _ }
    | Some { status = Unix.WSIGNALED _; _ }
    | Some { status = Unix.WSTOPPED _; _ }
    | None ->
        None
  with _ -> None

(** Resolve GitHub token: check GITHUB_TOKEN env var, then try [gh auth token].
    Uses argv (no shell). *)
let infer_github_token () =
  match Stdlib.Sys.getenv_opt "GITHUB_TOKEN" with
  | Some t when not (Base.String.is_empty (Base.String.strip t)) ->
      Base.String.strip t
  | _ -> (
      try
        match
          read_process_capture (fun () ->
              Unix.open_process_args_in "gh" [| "gh"; "auth"; "token" |])
        with
        | Some (Unix.WEXITED 0, out) ->
            let t = Base.String.strip out in
            if Base.String.is_empty t then "" else t
        | Some (Unix.WEXITED _, _)
        | Some (Unix.WSIGNALED _, _)
        | Some (Unix.WSTOPPED _, _)
        | None ->
            ""
      with _ -> "")

(** Detect the default branch of a git repository. Tries: 1.
    [git symbolic-ref refs/remotes/origin/HEAD], verifying the target tracking
    ref actually resolves (origin/HEAD is set at clone time and is NOT refreshed
    by [git fetch] — a stale value can point at a branch that has since been
    renamed or deleted upstream). 2. [git rev-parse --verify refs/heads/main] →
    "main" 3. [git rev-parse --verify refs/heads/master] → "master" 4. Fallback:
    "main" *)
let infer_default_branch ~repo_root =
  let resolves ref_name =
    git_success ~repo_root [ "rev-parse"; "--verify"; ref_name ]
  in
  let head_probes () =
    if resolves "refs/heads/main" then "main"
    else if resolves "refs/heads/master" then "master"
    else "main"
  in
  match
    git_stdout ~repo_root [ "symbolic-ref"; "refs/remotes/origin/HEAD" ]
  with
  | Some ref_path ->
      let prefix = "refs/remotes/origin/" in
      let candidate =
        if Base.String.is_prefix ref_path ~prefix then
          Base.String.chop_prefix_exn ref_path ~prefix
        else ref_path
      in
      if resolves (prefix ^ candidate) then candidate else head_probes ()
  | None -> head_probes ()

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

(** Verify that the configured [main_branch] resolves as
    [refs/remotes/origin/<branch>] in the local clone. If the local tracking ref
    is missing, attempt one [git fetch origin] and re-check. That best-effort
    fetch runs during startup and may block on a network round-trip when origin
    is slow or unreachable. Returns an actionable error when the branch cannot
    be resolved — e.g., when it was renamed or deleted upstream and the stored
    config still names the old branch. Without this guard the rebase loop in
    [Worktree.rebase_onto] would fail every poll with
    [merge-base --is-ancestor: Not a valid object name] and silently retry
    forever. *)
let validate_branch_resolves ~repo_root ~main_branch =
  let branch_str = Branch.to_string main_branch in
  let ref_name = "refs/remotes/origin/" ^ branch_str in
  let resolves () =
    git_success ~repo_root [ "rev-parse"; "--verify"; ref_name ]
  in
  if resolves () then Ok ()
  else
    let () = Printf.eprintf "onton: fetching origin to verify branch...\n%!" in
    match run_git_capture ~repo_root [ "fetch"; "origin"; "--quiet" ] with
    | Some { status = Unix.WEXITED 0; _ } ->
        if resolves () then Ok ()
        else
          Error
            (Printf.sprintf
               "configured main branch %S does not resolve as origin/%s in %s\n\
               \  (the branch may have been renamed or deleted upstream, or \
                the local clone has not fetched it).\n\
               \  Refresh the local default and retry:\n\
               \    git -C %s remote set-head origin -a\n\
               \    git -C %s fetch --prune\n\
               \  Or override at launch with --main-branch <name>."
               branch_str branch_str repo_root repo_root repo_root)
    | Some ({ status = Unix.WEXITED _; _ } as failed)
    | Some ({ status = Unix.WSIGNALED _; _ } as failed)
    | Some ({ status = Unix.WSTOPPED _; _ } as failed) ->
        Error
          (Printf.sprintf
             "configured main branch %S does not resolve as origin/%s in %s, \
              and git fetch origin failed:\n\
             \  %s\n\
             \  Refresh the local default and retry:\n\
             \    git -C %s remote set-head origin -a\n\
             \    git -C %s fetch --prune\n\
             \  Or override at launch with --main-branch <name>."
             branch_str branch_str repo_root
             (format_git_failure failed)
             repo_root repo_root)
    | None ->
        Error
          (Printf.sprintf
             "configured main branch %S does not resolve as origin/%s in %s, \
              and git fetch origin could not be started.\n\
             \  Refresh the local default and retry:\n\
             \    git -C %s remote set-head origin -a\n\
             \    git -C %s fetch --prune\n\
             \  Or override at launch with --main-branch <name>."
             branch_str branch_str repo_root repo_root repo_root)

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

(** Execute declarative GitHub effects and record successful observations back
    into durable state. *)
let execute_github_effects ~runtime ~net ~github effects =
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
          | Patch_controller.Set_pr_draft { patch_id = _; pr_number; draft } ->
              Github.set_draft ~net github ~pr_number ~draft
          | Patch_controller.Set_pr_base { patch_id = _; pr_number; base } ->
              Github.update_pr_base ~net github ~pr_number ~base
        in
        match result with
        | Ok () ->
            Runtime.update_orchestrator runtime (fun orch ->
                Patch_controller.apply_github_effect_success orch github_effect)
        | Error err ->
            Printf.eprintf "%s failed: %s\n%!" label (Github.show_error err)
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
            Base.Option.map e.Activity_log.Event.patch_id ~f:Patch_id.to_string;
          message = e.Activity_log.Event.message;
          timestamp = e.Activity_log.Event.timestamp;
        })
    ~map_transition:(fun (t : Activity_log.Transition_entry.t) ->
      Tui.Transition
        {
          patch_id = Patch_id.to_string t.Activity_log.Transition_entry.patch_id;
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

(** Look up the gameplan-author's 1/2/3 complexity for a patch. [None] when the
    patch is missing or the gameplan didn't specify — backends running under
    [--model auto] should treat that as the highest tier. *)
let patch_complexity ~(gameplan : Gameplan.t) ~patch_id =
  Base.List.find gameplan.Gameplan.patches ~f:(fun (p : Patch.t) ->
      Patch_id.equal p.Patch.id patch_id)
  |> Base.Option.bind ~f:(fun (p : Patch.t) -> p.Patch.complexity)

(** {1 Shared helpers} *)

(** Pluralize a count for inline rendering: [pluralize 1 "comment"] →
    ["1 comment"], [pluralize 2 "comment"] → ["2 comments"]. Pass [~plural] when
    the plural is irregular. *)
let pluralize ?plural n singular =
  let many = match plural with Some p -> p | None -> singular ^ "s" in
  Printf.sprintf "%d %s" n (if n = 1 then singular else many)

let log_event runtime ?patch_id msg =
  Runtime_logging.log_event runtime ?patch_id msg

(** Reconcile per-patch automerge deadlines. For each deadline that has elapsed,
    merge the PR on GitHub and mark the patch merged on success. On failure the
    failure counter is incremented and the deadline pushed out by a fresh idle
    window, so the retry is at least 5 minutes out regardless of how often the
    runner tick fires. Retries continue until the consecutive failure cap is
    reached, at which point reconciliation stops issuing merge calls until the
    user toggles automerge off/on. *)
let reconcile_and_execute_automerge ~runtime ~net ~github =
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
              match
                Github.merge_pr ~net github ~pr_number ~merge_method:`Squash
              with
              | Ok Github.Merge_succeeded ->
                  inflight_cleared := true;
                  Runtime.update_orchestrator runtime (fun orch ->
                      Patch_controller.apply_automerge_success orch patch_id);
                  log_event runtime ~patch_id
                    (Printf.sprintf "Automerge complete — PR #%d merged"
                       (Pr_number.to_int pr_number))
              | Ok (Github.Merge_queued msg) ->
                  (* GitHub accepted the request into its native auto-merge
                     queue. Not a failure — don't bump the counter. Push the
                     deadline forward (atomically with clearing [inflight]) so
                     we don't re-fire before the poller observes the eventual
                     merge. *)
                  push_deadline_and_clear_inflight ();
                  log_event runtime ~patch_id
                    (Printf.sprintf
                       "Automerge queued by GitHub — awaiting checks (%s)" msg)
              | Ok Github.Merge_unconfirmed ->
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
                       (Github.show_error err))
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
    session-level signals (e.g. a Missing artifact alongside a failed Write tool
    call indicates the agent was blocked mid-call, not that it chose to skip
    notes). *)
let apply_pr_body_artifact ~runtime ~net ~github ~project_name ~patch_id
    ~pr_number ~patch ~gameplan : [ `Ok | `Missing | `Empty | `Patch_failed ] =
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
      match Github.update_pr_body ~net github ~pr_number ~body with
      | Ok () ->
          log_event runtime ~patch_id
            (Printf.sprintf "pr-body: PATCHed PR #%d"
               (Pr_number.to_int pr_number));
          `Ok
      | Error e ->
          log_event runtime ~patch_id
            (Printf.sprintf "pr-body: PATCH failed — %s" (Github.show_error e));
          `Patch_failed)

(** Terminal failure — forces [Given_up] so [complete] raises intervention. Used
    for non-retryable errors (patch not found, PR discovery failed).

    Routes through the pure [Orchestrator.apply_force_complete] decision so any
    inflight human messages are restored to the inbox rather than silently
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

let resolve_worktree_path = Worktree_setup.resolve_worktree_path
let ensure_worktree = Worktree_setup.ensure_worktree

(** Execute a [Worktree_plan.t] against the live filesystem. The plan is built
    purely (see [Worktree_plan.for_rebase] / [for_merge_conflict]); this
    function is the single place that turns its ops into git invocations.

    Short-circuits on the first failing op:
    - [Ensure_worktree] failure →
      [Worktree.Error "<label> failed: worktree missing"]
    - [Fetch_origin] error → [Worktree.Error "fetch before rebase failed: ..."]
      (this exact prefix matches the historical log/event format)
    - [Rebase_onto] returns its result verbatim; only [Worktree.Ok] continues.

    Returns the final result paired with the worktree path so callers can use it
    for follow-on effects like [Worktree.force_push_with_lease]. *)
let execute_worktree_plan ~runtime ~process_mgr ~clock ~fs ~repo_root
    ~project_name ~patch_id ~(agent : Patch_agent.t) ~user_config
    ~worktree_mutex ~hook_mutex ~fetch_lock ~fail_label ~ancestor_ids
    (plan : Worktree_plan.t) =
  let default_path = Worktree.worktree_dir ~project_name ~patch_id in
  let rec loop ~path = function
    | [] -> (Worktree.Ok, path)
    | Worktree_plan.Ensure_worktree :: rest -> (
        match
          ensure_worktree ~runtime ~process_mgr ~clock ~fs ~repo_root
            ~project_name ~patch_id ~agent ~user_config ~worktree_mutex
            ~hook_mutex ()
        with
        | Some p -> loop ~path:p rest
        | None ->
            log_event runtime ~patch_id
              (Printf.sprintf
                 "Cannot %s — worktree missing and could not be created"
                 fail_label);
            ( Worktree.Error
                (Printf.sprintf "%s failed: worktree missing" fail_label),
              path ))
    | Worktree_plan.Fetch_origin :: rest -> (
        match Worktree.fetch_origin ~fetch_lock ~process_mgr ~path with
        | Result.Ok () -> loop ~path rest
        | Result.Error msg ->
            log_event runtime ~patch_id
              (Printf.sprintf "Fetch failed before %s — %s" fail_label msg);
            ( Worktree.Error
                (Printf.sprintf "fetch before rebase failed: %s" msg),
              path ))
    | Worktree_plan.Rebase_onto target :: rest -> (
        match
          Worktree.rebase_onto ~process_mgr ~path ~target ~project_name
            ~ancestor_ids
        with
        | Worktree.Ok -> loop ~path rest
        | (Worktree.Noop | Worktree.Conflict _ | Worktree.Error _) as r ->
            (r, path))
  in
  loop ~path:default_path plan

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
        if Patch_agent.needs_intervention a || a.Patch_agent.branch_blocked then
          Some a.Patch_agent.patch_id
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
              Base.Map.Poly.set acc ~key:pid ~data:e.Activity_log.Event.message
        | _ -> acc)

(** TUI rendering fiber — redraws the terminal at ~10 fps.

    [list_selected], [detail_scroll], [timeline_scroll], and [view_mode] are
    shared mutable refs updated by the input fiber. *)
let tui_fiber ~runtime ~clock ~stdout ~list_selected ~detail_scroll
    ~detail_follow ~timeline_scroll ~view_mode ~show_help ~status_msg
    ~transcripts ~sorted_patch_ids ~input_mode ~prompt_line ~patches_start_row
    ~patches_scroll_offset ~patches_visible_count ~backend_name ~resolve_routing
    =
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
          match Hashtbl.find_opt transcripts pid with Some t -> t | None -> "")
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
    - Prompt mode: purpose-specific mini-prompts for PR numbers, worktree paths,
      and messages *)

(** Normalize pasted text for single-line input: strip trailing newlines,
    replace internal newlines/carriage returns with spaces. *)
let normalize_paste text =
  let text =
    Base.String.rstrip text ~drop:(fun c ->
        Char.equal c '\n' || Char.equal c '\r')
  in
  let text = Base.String.tr text ~target:'\n' ~replacement:' ' in
  Base.String.tr text ~target:'\r' ~replacement:' '

let input_fiber ~runtime ~process_mgr ~net ~github ~list_selected ~detail_scroll
    ~detail_follow ~timeline_scroll ~detail_scrolls ~view_mode ~pr_registry
    ~project_name ~show_help ~status_msg ~sorted_patch_ids ~input_mode
    ~prompt_line ~patches_start_row ~patches_scroll_offset
    ~patches_visible_count ~owner ~repo ~resolve_routing =
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
    | Tui_input.Prompt_pr | Tui_input.Prompt_worktree | Tui_input.Prompt_message
    | Tui_input.Prompt_broadcast ->
        let prefix = Tui_input.prompt_prefix !input_mode in
        let contents = Tui_input.Edit_buffer.contents buf in
        let cursor_col =
          Term.visible_length prefix
          + Term.visible_length
              (String.sub contents 0 (Tui_input.Edit_buffer.cursor buf))
        in
        prompt_line := Some { Tui.prompt_text = prefix ^ contents; cursor_col };
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
                          Orchestrator.agent snap.Runtime.orchestrator patch_id
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
                              Orchestrator.set_automerge_enabled orch patch_id v
                            in
                            (orch, Some v))
                  in
                  match enabled_after with
                  | Some true -> log_event runtime ~patch_id "Automerge enabled"
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
                              Orchestrator.send_human_message orch patch_id line);
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
                             (Tui.equal_display_status pv.Tui.status Tui.Merged))
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
                            Orchestrator.send_human_message orch pv.Tui.patch_id
                              line));
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
                              (Orchestrator.find_agent snap.Runtime.orchestrator
                                 patch_id))
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
                        match Github.pr_state ~net github pr_number with
                        | Error err ->
                            status_msg := None;
                            log_event runtime ~patch_id
                              (Printf.sprintf "Cannot add ad-hoc PR #%d — %s" n
                                 (Github.show_error err))
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
                                     "Cannot add ad-hoc PR #%d — no head branch"
                                     n)
                            | Some branch ->
                                Pr_registry.register pr_registry ~patch_id
                                  ~pr_number;
                                Runtime.update_orchestrator runtime (fun orch ->
                                    let base_branch =
                                      Option.value pr_state.Pr_state.base_branch
                                        ~default:(Orchestrator.main_branch orch)
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
                            not (String.equal canonical_real canonical_expected)
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
                          if String.equal canonical_real canonical_expected then
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
                              (Printf.sprintf "Failed to add worktree — %s" msg)
                        | exn ->
                            let msg = Printexc.to_string exn in
                            status_msg :=
                              Some
                                {
                                  Tui.level = Tui.Error;
                                  text =
                                    Printf.sprintf "Failed to add worktree: %s"
                                      msg;
                                  expires_at = None;
                                };
                            log_event runtime ~patch_id
                              (Printf.sprintf "Failed to add worktree — %s" msg)
                        ))
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
                 | Tui_input.History.Entry s -> Tui_input.Edit_buffer.set buf s
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
          | Term.Key.Tab | Term.Key.Page_up | Term.Key.Page_down | Term.Key.F _
          | Term.Key.Ctrl _ | Term.Key.Mouse _ | Term.Key.Unknown _ ->
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
                  Tui_input.Edit_buffer.insert_string buf (normalize_paste text);
                  input_mode := Tui_input.Prompt_message;
                  loop ()
              | Tui.List_view | Tui.Timeline_view -> loop ())
          | Term.Key.Mouse ev -> (
              match (ev, !view_mode) with
              | ( Term_key.Click { button = Term_key.Left; row; press = true; _ },
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
              | ( Term_key.Click { button = Term_key.Left; row; press = true; _ },
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
                      button = Term_key.Left | Term_key.Middle | Term_key.Right;
                      _;
                    },
                  (Tui.List_view | Tui.Detail_view _ | Tui.Timeline_view) ) ->
                  loop ())
          | Term.Key.Char '*' when Tui.equal_view_mode !view_mode Tui.List_view
            ->
              Tui_input.Edit_buffer.clear buf;
              input_mode := Tui_input.Prompt_broadcast;
              loop ()
          | Term.Key.Char '+' when Tui.equal_view_mode !view_mode Tui.List_view
            ->
              Tui_input.Edit_buffer.clear buf;
              input_mode := Tui_input.Prompt_pr;
              loop ()
          | Term.Key.Char 'w' when Tui.equal_view_mode !view_mode Tui.List_view
            ->
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
                        Printf.sprintf "https://github.com/%s/%s/pull/%d" owner
                          repo
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
          | Term.Key.Char _ | Term.Key.Enter | Term.Key.Tab | Term.Key.Backspace
          | Term.Key.Escape | Term.Key.Up | Term.Key.Down | Term.Key.Left
          | Term.Key.Right | Term.Key.Home | Term.Key.End | Term.Key.Page_up
          | Term.Key.Page_down | Term.Key.Delete | Term.Key.F _
          | Term.Key.Ctrl _ | Term.Key.Unknown _ -> (
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
                        Tui_input.apply_move ~count ~selected:!list_selected cmd
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
                      let reserved = 11 in
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
                      | Tui_input.Page_up | Tui_input.Page_down | Tui_input.Quit
                      | Tui_input.Help | Tui_input.Select | Tui_input.Back
                      | Tui_input.Timeline | Tui_input.Noop
                      | Tui_input.Send_message _ | Tui_input.Add_pr _
                      | Tui_input.Add_worktree _ | Tui_input.Remove_patch
                      | Tui_input.Open_in_browser ->
                          let delta =
                            match cmd with
                            | Tui_input.Move_up -> -1
                            | Tui_input.Move_down -> 1
                            | Tui_input.Page_up -> -10
                            | Tui_input.Page_down -> 10
                            | Tui_input.Quit | Tui_input.Help | Tui_input.Select
                            | Tui_input.Back | Tui_input.Timeline
                            | Tui_input.Noop | Tui_input.Send_message _
                            | Tui_input.Add_pr _ | Tui_input.Add_worktree _
                            | Tui_input.Remove_patch | Tui_input.Open_in_browser
                            | Tui_input.Scroll_top | Tui_input.Scroll_bottom ->
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
                                "Warning — patch is currently running, it may \
                                 create a GitHub PR before stopping";
                            Runtime.update_orchestrator runtime (fun orch ->
                                Orchestrator.remove_agent orch patch_id);
                            Pr_registry.unregister pr_registry ~patch_id;
                            log_event runtime ~patch_id "Removed ad-hoc patch"))
                  | Tui.Detail_view _ | Tui.Timeline_view -> ());
                  loop ()
              | Tui_input.Noop | Tui_input.Send_message _ | Tui_input.Add_pr _
              | Tui_input.Add_worktree _ | Tui_input.Open_in_browser ->
                  loop ()))
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
  (* Track seen entries by set of (timestamp, message) to handle entries that
     sort before previously-displayed ones (e.g. an event logged just before
     a stream entry but displayed in the next poll cycle). *)
  let seen = Hashtbl.create 256 in
  let rec loop () =
    let entries =
      Runtime.read runtime (fun snap ->
          merged_log_entries ~log:snap.Runtime.activity_log ~limit:500
            ~compare:(fun (t1, _) (t2, _) -> Base.Float.ascending t1 t2)
            ~map_event:format_event ~map_transition:format_transition
            ~map_stream:(fun (s : Activity_log.Stream_entry.t) ->
              Printf.sprintf "[%s] %s"
                (Patch_id.to_string s.Activity_log.Stream_entry.patch_id)
                (format_stream_kind s.Activity_log.Stream_entry.kind)))
    in
    Base.List.iter entries ~f:(fun (ts, msg) ->
        let key = (ts, msg) in
        if not (Hashtbl.mem seen key) then (
          Hashtbl.replace seen key true;
          Eio.Flow.copy_string
            (Printf.sprintf "%s %s\n" (format_time ts) msg)
            stdout));
    (* Bound the seen set: rebuild from current entries so it never grows
       beyond the merged_log_entries window (~1500 entries max). *)
    if Hashtbl.length seen > 2000 then (
      let current = Hashtbl.create 256 in
      Base.List.iter entries ~f:(fun key -> Hashtbl.replace current key true);
      Hashtbl.reset seen;
      Hashtbl.iter (fun k v -> Hashtbl.replace seen k v) current);
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
let poll_review_backends ~net ~clock ~runtime ~patch_id ~findings_registry
    ~review_backends ~owner ~repo ~pr_number : Review_service.finding list =
  Base.List.concat_map review_backends ~f:(fun (b : Review_backend.t) ->
      match
        Review_service_client.list_findings ~net ~clock ~backend:b ~owner ~repo
          ~pr_number ()
      with
      | Error err ->
          log_event runtime ~patch_id
            (Printf.sprintf "Poll error — %s"
               (Review_service_client.show_error err));
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
                 b.Review_backend.name response.Review_service.count
                 parsed_count);
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
                   b.Review_backend.name e.Review_service.index
                   e.Review_service.error json));
          let keyed_findings =
            Base.List.map response.Review_service.findings
              ~f:(fun (f : Review_service.finding) ->
                let key =
                  Findings_registry.make_key ~backend_name:b.Review_backend.name
                    ~owner ~repo ~pr_number ~finding_id:f.Review_service.id
                in
                (key, f))
          in
          Findings_registry.remove_stale_for_scope findings_registry
            ~backend_name:b.Review_backend.name ~owner ~repo ~pr_number
            ~keep_keys:(Base.List.map keyed_findings ~f:fst);
          Base.List.map keyed_findings ~f:(fun (key, f) ->
              Findings_registry.register findings_registry ~key
                {
                  Findings_registry.backend = b;
                  owner;
                  repo;
                  pr_number;
                  finding_id = f.Review_service.id;
                };
              { f with Review_service.id = key }))

let poller_fiber ~runtime ~clock ~net ~process_mgr ~github ~config ~project_name
    ~pr_registry ~branch_of ~event_log ~review_backends ~findings_registry =
  let main = config.main_branch in
  let skip_logged : (Patch_id.t, bool) Hashtbl.t = Hashtbl.create 16 in
  let rec loop () =
    let intents =
      Runtime.read runtime (fun snap ->
          let agents = Orchestrator.all_agents snap.Runtime.orchestrator in
          Base.List.filter_map agents ~f:(fun (agent : Patch_agent.t) ->
              if Patch_agent.has_pr agent && not agent.Patch_agent.merged then
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
    (* Phase 1: I/O — collect poll observations outside the lock *)
    let observations =
      Base.List.filter_map intents ~f:(fun intent ->
          match intent with
          | Skip_no_pr patch_id ->
              if not (Hashtbl.mem skip_logged patch_id) then (
                Hashtbl.replace skip_logged patch_id true;
                log_event runtime ~patch_id
                  "Skipping poll — no PR number registered");
              None
          | Poll { patch_id; pr_number; was_merged } -> (
              match Github.pr_state ~net github pr_number with
              | Error err ->
                  log_event runtime ~patch_id
                    (Printf.sprintf "Poll error — %s" (Github.show_error err));
                  None
              | Ok pr_state when Pr_state.is_fork pr_state ->
                  if not (Hashtbl.mem skip_logged patch_id) then (
                    Hashtbl.replace skip_logged patch_id true;
                    log_event runtime ~patch_id
                      (Printf.sprintf
                         "Skipping PR #%d — fork PRs are not supported"
                         (Pr_number.to_int pr_number)));
                  None
              | Ok pr_state ->
                  (* Augment the GitHub-derived [pr_state] with findings from
                     every configured review backend. Failures are logged
                     but non-fatal — a flaky review service shouldn't stop
                     the rest of the poll cycle. *)
                  let findings =
                    if Base.List.is_empty review_backends then []
                    else
                      poll_review_backends ~net ~clock ~runtime ~patch_id
                        ~findings_registry ~review_backends
                        ~owner:config.github_owner ~repo:config.github_repo
                        ~pr_number:(Pr_number.to_int pr_number)
                  in
                  let pr_state = { pr_state with Pr_state.findings } in
                  let poll_result = Poller.poll ~was_merged pr_state in
                  (* PR was closed — re-discover the current open PR.
                     This path does its own I/O and separate atomic update;
                     it doesn't participate in the batched update below. *)
                  if poll_result.Poller.closed then (
                    log_event runtime ~patch_id
                      (Printf.sprintf "PR #%d closed — looking for replacement"
                         (Pr_number.to_int pr_number));
                    let branch =
                      match pr_state.Pr_state.head_branch with
                      | Some b -> b
                      | None ->
                          Runtime.read runtime (fun snap ->
                              match
                                Orchestrator.find_agent
                                  snap.Runtime.orchestrator patch_id
                              with
                              | Some agent -> agent.Patch_agent.branch
                              | None -> branch_of patch_id)
                    in
                    (match
                       Startup_reconciler.discover_pr ~net ~github ~branch
                     with
                    | Ok (Some (new_pr, base_branch, merged)) ->
                        log_event runtime ~patch_id
                          (Printf.sprintf "Switched to PR #%d"
                             (Pr_number.to_int new_pr));
                        Pr_registry.register pr_registry ~patch_id
                          ~pr_number:new_pr;
                        Runtime.update_orchestrator runtime (fun orch ->
                            Patch_controller.apply_replacement_pr orch patch_id
                              ~pr_number:new_pr ~base_branch ~merged)
                    | Ok None ->
                        log_event runtime ~patch_id
                          "No open PR found — cleared stale PR state, will \
                           create on next session";
                        Pr_registry.unregister pr_registry ~patch_id;
                        Runtime.update_orchestrator runtime (fun orch ->
                            Orchestrator.clear_pr orch patch_id)
                    | Error msg ->
                        log_event runtime ~patch_id
                          (Printf.sprintf "PR re-discovery failed — %s" msg));
                    None)
                  else
                    let branch_in_root =
                      match pr_state.Pr_state.head_branch with
                      | Some b ->
                          Worktree.is_checked_out_in_repo_root ~process_mgr
                            ~repo_root:config.repo_root b
                      | None -> false
                    in
                    let failed_ci =
                      Base.List.filter poll_result.Poller.ci_checks
                        ~f:(fun (c : Ci_check.t) ->
                          Base.List.mem Patch_decision.failure_conclusions
                            c.Ci_check.conclusion ~equal:Base.String.equal)
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
                              resolve_worktree_path ~process_mgr
                                ~repo_root:config.repo_root ~project_name
                                ~patch_id ~agent ()
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
                    Some
                      ( patch_id,
                        observation,
                        failed_ci,
                        pr_state.Pr_state.ci_checks_truncated )))
    in
    (* Phase 2: Single atomic update — apply all poll results + reconcile.
       This prevents the runner from seeing an intermediate state where
       poll results are applied but the reconciler hasn't run yet. *)
    let per_patch_sides, reconcile_logs =
      Runtime.update_orchestrator_returning runtime (fun orch ->
          (* Apply all poll results *)
          let orch, sides =
            Base.List.fold observations ~init:(orch, [])
              ~f:(fun (orch, sides) (patch_id, obs, failed_ci, ci_truncated) ->
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
                        (Base.List.map log_entries
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
          (* Reconcile — detect merges and enqueue rebases *)
          let agents = Orchestrator.all_agents orch in
          let patch_views =
            Base.List.map agents ~f:(fun (a : Patch_agent.t) ->
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
                      Base.Option.value a.Patch_agent.base_branch ~default:main;
                    branch_rebased_onto = a.Patch_agent.branch_rebased_onto;
                  })
          in
          let merged_patches =
            Base.List.filter_map agents ~f:(fun (a : Patch_agent.t) ->
                if a.Patch_agent.merged then Some a.Patch_agent.patch_id
                else None)
          in
          let actions =
            Reconciler.reconcile ~graph:(Orchestrator.graph orch) ~main
              ~merged_pr_patches:merged_patches ~branch_of patch_views
          in
          let rec_logs = ref [] in
          let orch =
            Base.List.fold actions ~init:orch ~f:(fun orch action ->
                match action with
                | Reconciler.Mark_merged pid ->
                    Orchestrator.mark_merged orch pid
                | Reconciler.Enqueue_rebase pid ->
                    rec_logs :=
                      ("rebase enqueued by reconciler", pid) :: !rec_logs;
                    Orchestrator.enqueue orch pid Operation_kind.Rebase
                | Reconciler.Start_operation _ -> orch)
          in
          (orch, (Base.List.rev sides, Base.List.rev !rec_logs)))
    in
    (* Phase 3: Side effects — outside the lock *)
    Base.List.iter per_patch_sides
      ~f:(fun
          (patch_id, log_entries, newly_blocked, _failed_ci, ci_truncated) ->
        Base.List.iter log_entries
          ~f:(fun (entry : Patch_controller.poll_log_entry) ->
            log_event runtime ~patch_id:entry.Patch_controller.patch_id
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
           let main_root =
             Worktree.resolve_main_root ~process_mgr ~repo_root:config.repo_root
           in
           log_event runtime ~patch_id
             (Printf.sprintf
                "Cannot work on patch — branch %s is checked out in the main \
                 working tree (%s); release it (e.g. `git -C %s checkout \
                 <default-branch>`) before continuing"
                b main_root main_root));
        if ci_truncated then
          log_event runtime ~patch_id
            "Warning — CI check list was truncated (>100 checks); some \
             failures may not appear in the prompt");
    Base.List.iter reconcile_logs ~f:(fun (msg, pid) ->
        log_event runtime ~patch_id:pid msg);
    Eio.Time.sleep clock config.poll_interval;
    loop ()
  in
  loop ()

(** Runner fiber — executes orchestrator actions by driving backend sessions
    concurrently. *)
let runner_fiber ~runtime ~env ~config ~pick_backend ~project_name ~pr_registry
    ~findings_registry ~review_backends ~transcripts ~github ~net ~event_log
    ?status_msg () =
  let main = config.main_branch in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let fs = Eio.Stdenv.fs env in
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
  let worktree_mutex = Eio.Mutex.create () in
  (* Serializes [git fetch origin] across worktrees to avoid ref-lock races on
     the shared [refs/remotes/origin/*] store. See [Worktree.fetch_origin]. *)
  let fetch_mutex = Eio.Mutex.create () in
  (* Serializes [on_worktree_create] invocations across patch fibers. The
     hook runs user build commands ([npm install], [dune build], …) which
     aren't parallelism-safe across shared caches AND fan out into dozens
     of short-lived children. Running 10 hooks in parallel is how issue
     #209 blew through the system FD table. *)
  let hook_mutex = Eio.Mutex.create () in
  let long_lived_sessions :
      (Patch_id.t, Session_driver.long_lived_session) Stdlib.Hashtbl.t =
    Stdlib.Hashtbl.create 16
  in
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
  let run_llm_session ~sw ~gameplan_prompt ~patch_prompt ~kind ~patch_id ~prompt
      ~agent ~on_pr_detected ~complexity =
    match pick_backend ~complexity with
    | Backend_registry.Ephemeral backend, _decision ->
        Session_driver.run ~kind ~runtime ~process_mgr ~clock ~fs ~project_name
          ~patch_id ~repo_root:config.repo_root ~prompt ~agent
          ~owner:config.github_owner ~repo:config.github_repo ~on_pr_detected
          ~transcripts ~user_config:config.user_config ~worktree_mutex
          ~hook_mutex ~backend ~complexity ~event_log
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
                "patch-agent backend requires a concrete non-empty model after \
                 routing"
        in
        match patch_agent_model_result with
        | Error message ->
            log_event runtime ~patch_id message;
            (`Failed, [])
        | Ok patch_agent_model ->
            let session =
              match Stdlib.Hashtbl.find_opt long_lived_sessions patch_id with
              | Some session ->
                  Session_driver.update_long_lived_session_prompts session
                    ~gameplan_prompt ~patch_prompt;
                  session
              | None ->
                  let session =
                    Session_driver.create_long_lived_session ~backend
                      ~provider:patch_agent_provider ~model:patch_agent_model
                      ~effort:patch_agent_effort ~gameplan_prompt ~patch_prompt
                  in
                  Stdlib.Hashtbl.replace long_lived_sessions patch_id session;
                  session
            in
            Session_driver.run_long_lived ~sw ~kind ~runtime ~process_mgr ~clock
              ~fs ~project_name ~patch_id ~repo_root:config.repo_root ~prompt
              ~agent ~owner:config.github_owner ~repo:config.github_repo
              ~on_pr_detected ~transcripts ~user_config:config.user_config
              ~worktree_mutex ~hook_mutex ~session ~complexity ~event_log)
  in
  let shutdown_finished_long_lived_sessions ~sw () =
    let finished =
      Runtime.read runtime (fun snap ->
          Stdlib.Hashtbl.fold
            (fun patch_id session acc ->
              if Session_driver.long_lived_session_failed session then
                (patch_id, session) :: acc
              else
                match
                  Orchestrator.find_agent snap.Runtime.orchestrator patch_id
                with
                | None -> (patch_id, session) :: acc
                | Some agent
                  when agent.Patch_agent.merged && not agent.Patch_agent.busy ->
                    (patch_id, session) :: acc
                | Some _ -> acc)
            long_lived_sessions [])
    in
    Base.List.iter finished ~f:(fun (patch_id, session) ->
        Stdlib.Hashtbl.remove long_lived_sessions patch_id;
        Eio.Fiber.fork_daemon ~sw (fun () ->
            (try Session_driver.shutdown_long_lived_session session with
            | Eio.Cancel.Cancelled _ -> ()
            | exn ->
                log_event runtime ~patch_id
                  (Printf.sprintf "long-lived backend shutdown error — %s"
                     (Printexc.to_string exn)));
            `Stop_daemon))
  in
  let with_busy_guard ~patch_id f =
    let cancelled = ref false in
    let exception_raised = ref false in
    Fun.protect
      ~finally:(fun () ->
        (* Three exit modes:
           - cancelled: f raised [Eio.Cancel.Cancelled]; clean teardown.
           - exception_raised: f raised any other exception; session
             fallback must advance.
           - neither: f returned normally (already called [complete]
             itself, so the busy=false guard below skips the rest). The
             reason value is unused in that case, but [Cancelled] is the
             safe default — it leaves [session_fallback] untouched if a
             future code path in [f] ever exits normally with [busy=true]. *)
        let reason =
          if !cancelled then Orchestrator.Cancelled
          else if !exception_raised then Orchestrator.Unexpected_exception
          else Orchestrator.Cancelled
        in
        (* Check-and-complete must be atomic to avoid racing with another
           fiber that completes the same patch between read and update.
           Routes through the pure [apply_force_complete] decision so any
           inflight human messages are restored to the inbox rather than
           silently dropped — a regression that previously cost a delivered
           Brand.refined instruction in the id-brands run. *)
        let snapshot = ref None in
        Runtime.update_orchestrator runtime (fun orch ->
            match Orchestrator.find_agent orch patch_id with
            | None -> orch
            | Some before ->
                if before.Patch_agent.busy then (
                  let orch' =
                    Orchestrator.apply_force_complete orch patch_id reason
                  in
                  let after = Orchestrator.agent orch' patch_id in
                  snapshot := Some (before, after);
                  orch')
                else orch);
        Base.Option.iter !snapshot ~f:(fun (before, after) ->
            Event_log.log_force_complete event_log ~patch_id ~reason
              ~agent_before:before ~agent_after:after;
            log_event runtime ~patch_id
              (Printf.sprintf
                 "Forced complete (%s) — runner fiber exited with busy=true"
                 (Orchestrator.show_force_complete_reason reason))))
      (fun () ->
        try f () with
        | Eio.Cancel.Cancelled _ as exn ->
            cancelled := true;
            raise exn
        | exn ->
            exception_raised := true;
            log_event runtime ~patch_id
              (Printf.sprintf "Unexpected action exception — %s"
                 (Printexc.to_string exn))
        (* Do not call [mark_session_failed] here — the [finally] block
           routes through [apply_force_complete ~reason:Unexpected_exception]
           which subsumes its effect. *))
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
                  (acc, dispatched) (msg : Orchestrator.patch_agent_message) ->
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
    execute_github_effects ~runtime ~net ~github lifecycle_effects;
    (* Log dispatched actions to event log *)
    Base.List.iter messages ~f:(fun (msg : Orchestrator.patch_agent_message) ->
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
                      with_busy_guard ~patch_id (fun () ->
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
                                    ensure_worktree ~runtime ~process_mgr ~clock
                                      ~fs ~repo_root:config.repo_root
                                      ~project_name ~patch_id ~agent
                                      ~user_config:config.user_config
                                      ~worktree_mutex ~hook_mutex
                                      ~branch:patch.Patch.branch
                                      ~base_ref:(Branch.to_string base_branch)
                                      ()
                                  with
                                  | None ->
                                      Runtime.update_orchestrator runtime
                                        (fun orch ->
                                          Orchestrator.apply_session_result orch
                                            patch_id
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
                                        Prompt.render_patch_prompt ~project_name
                                          ?agents_md
                                          ?pr_number:agent.Patch_agent.pr_number
                                          patch gameplan
                                          ~base_branch:
                                            (Branch.to_string base_branch)
                                      in
                                      (* PR detection from stream text is a hint
                                     only — always confirmed via the GitHub
                                     REST API (Github.list_prs) after the
                                     backend session finishes *)
                                      let on_pr_detected _pr_number = () in
                                      let complexity =
                                        patch_complexity ~gameplan ~patch_id
                                      in
                                      let gameplan_prompt =
                                        Prompt.render_gameplan_layer
                                          ~project_name gameplan
                                      in
                                      let patch_prompt =
                                        Prompt.render_patch_layer ~project_name
                                          patch
                                          ?pr_number:agent.Patch_agent.pr_number
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
                            | `Failed | `Retry_push -> Orchestrator.Start_failed
                            | `Ok -> Orchestrator.Start_ok
                          in
                          Runtime.update_orchestrator runtime (fun orch ->
                              Orchestrator.apply_start_outcome orch patch_id
                                start_outcome);
                          match start_outcome with
                          | Orchestrator.Start_failed ->
                              let agent =
                                Runtime.read runtime (fun snap ->
                                    Orchestrator.agent snap.Runtime.orchestrator
                                      patch_id)
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
                                    Graph.initial_base (Orchestrator.graph orch)
                                      patch_id ~has_merged ~branch_of
                                      ~main:(Orchestrator.main_branch orch))
                              in
                              let pr_title =
                                Printf.sprintf "[%s] Patch %s: %s" project_name
                                  (Patch_id.to_string patch.Patch.id)
                                  patch.Patch.title
                              in
                              let pr_body =
                                Prompt.render_pr_description ~project_name patch
                                  gameplan
                                ^ Prompt.render_spec_suffix patch gameplan
                              in
                              (match
                                 Github.create_pull_request ~net github
                                   ~title:pr_title ~head:patch.Patch.branch
                                   ~base:fresh_base ~body:pr_body ~draft:true
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
                                  | Github.Http_error { status = 422; body; _ }
                                    when Github.response_error_message_contains
                                           body
                                           ~substring:
                                             "pull request already exists" -> (
                                      (* PR already exists — discover it rather
                                         than treating this as failure. We
                                         only fall back on this specific 422;
                                         other 422s (no commits, head missing,
                                         etc.) propagate with the original
                                         error message. *)
                                      match
                                        Github.list_prs ~net github
                                          ~branch:patch.Patch.branch ~base:None
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
                                                already-exists) and discovery \
                                                also failed — %s"
                                               (Github.show_error disc_err));
                                          Runtime.update_orchestrator runtime
                                            (fun orch ->
                                              Orchestrator
                                              .on_pr_discovery_failure orch
                                                patch_id))
                                  | Github.Http_error _
                                  | Github.Json_parse_error _
                                  | Github.Graphql_error _
                                  | Github.Transport_error _ ->
                                      log_event runtime ~patch_id
                                        (Printf.sprintf
                                           "PR creation failed — %s"
                                           (Github.show_error e));
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
                  with_busy_guard ~patch_id (fun () ->
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
                        execute_worktree_plan ~runtime ~process_mgr ~clock ~fs
                          ~repo_root:config.repo_root ~project_name ~patch_id
                          ~agent ~user_config:config.user_config ~worktree_mutex
                          ~hook_mutex ~fetch_lock:fetch_mutex
                          ~fail_label:"rebase" ~ancestor_ids
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
                              Worktree.force_push_with_lease ~process_mgr
                                ~path:wt_path ~branch ~base:new_base
                            in
                            (match result with
                            | Worktree.Push_ok ->
                                log_event runtime ~patch_id
                                  "Force-pushed after rebase"
                            | Worktree.Push_up_to_date ->
                                log_event runtime ~patch_id
                                  "Push noop after rebase — already up-to-date"
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
                                     "Worktree disappeared (%s) — rebase will \
                                      reconstruct on retry"
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
                            (if is_ci then "Fetching fresh CI state from GitHub"
                             else "Fetching fresh review comments from GitHub");
                          match Github.pr_state ~net github pr_num with
                          | Ok pr_state -> Some pr_state
                          | Error _err ->
                              log_event runtime ~patch_id
                                (if is_ci then "Failed to fetch fresh CI state"
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
                        when not (Base.List.is_empty review_backends) ->
                          log_event runtime ~patch_id
                            "Fetching fresh findings from review backends";
                          poll_review_backends ~net ~clock ~runtime ~patch_id
                            ~findings_registry ~review_backends
                            ~owner:config.github_owner ~repo:config.github_repo
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
                              "Fresh CI state shows no failures — skipping CI \
                               delivery";
                            Some "no current failures")
                    else None
                  in
                  with_busy_guard ~patch_id (fun () ->
                      let result =
                        match ci_skip_reason with
                        | Some reason ->
                            (* Fast path: skip decision already made. Don't
                               consume a session slot for a no-op — that
                               slot can go to another agent. *)
                            log_event runtime ~patch_id
                              (Printf.sprintf "Skipped ci delivery — %s" reason);
                            `Skip_empty
                        | None ->
                            with_session_slot (fun () ->
                                Runtime.update_orchestrator runtime (fun orch ->
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
                                        Orchestrator.set_ci_checks orch patch_id
                                          pr_state.Pr_state.ci_checks)
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
                                           "Base branch changed from %s to %s \
                                            — notifying agent"
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
                                      ensure_worktree ~runtime ~process_mgr
                                        ~clock ~fs ~repo_root:config.repo_root
                                        ~project_name ~patch_id ~agent
                                        ~user_config:config.user_config
                                        ~worktree_mutex ~hook_mutex ()
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
                                        Worktree.rebase_in_progress ~process_mgr
                                          ~path:wt_path
                                      in
                                      let git_status =
                                        Worktree.git_status ~process_mgr
                                          ~path:wt_path
                                      in
                                      let git_diff =
                                        Worktree.conflict_diff ~process_mgr
                                          ~path:wt_path
                                      in
                                      Event_log.log_conflict_delivery event_log
                                        ~patch_id ~path:wt_path
                                        ~rebase_in_progress:
                                          rebase_still_in_progress ~git_status
                                        ~git_diff;
                                      let patch =
                                        Base.List.find gameplan.Gameplan.patches
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
                                            ~project_name ?agents_md ?pr_number
                                            ?patch ~gameplan ~base_branch:base
                                            ~git_status ~git_diff ?conflict_info
                                            ()
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
                                            Prompt.render_patch_layer
                                              ~project_name p ?pr_number
                                              ~base_branch:base ()
                                        | None -> ""
                                      in
                                      let result, _tool_failures =
                                        run_llm_session ~sw ~gameplan_prompt
                                          ~patch_prompt
                                          ~kind:
                                            (Some Operation_kind.Merge_conflict)
                                          ~patch_id ~prompt ~agent
                                          ~on_pr_detected ~complexity
                                      in
                                      (match result with
                                      | `Ok
                                        when not
                                               (String.equal base_changed_prefix
                                                  "") ->
                                          Runtime.update_orchestrator runtime
                                            (fun orch ->
                                              Orchestrator
                                              .set_notified_base_branch orch
                                                patch_id (Branch.of_string base))
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
                                    if
                                      Worktree.rebase_in_progress ~process_mgr
                                        ~path:wt_path
                                    then (
                                      log_event runtime ~patch_id
                                        "Delivering merge-conflict — rebase \
                                         already in progress";
                                      (* Match the fresh-rebase path: rebase
                                         target is [origin/<base>], not the
                                         (possibly stale) local tracking ref.
                                         See Worktree_plan.for_merge_conflict. *)
                                      let conflict_info =
                                        Worktree.read_in_progress_conflict_info
                                          ~process_mgr ~path:wt_path
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
                                        execute_worktree_plan ~runtime
                                          ~process_mgr ~clock ~fs
                                          ~repo_root:config.repo_root
                                          ~project_name ~patch_id ~agent
                                          ~user_config:config.user_config
                                          ~worktree_mutex ~hook_mutex
                                          ~fetch_lock:fetch_mutex
                                          ~fail_label:"merge-conflict rebase"
                                          ~ancestor_ids
                                          (Worktree_plan.for_merge_conflict
                                             ~base:(Types.Branch.of_string base))
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
                                               "Conflict rebase failed — %s" msg));
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
                                              .apply_conflict_rebase_result orch
                                                patch_id rebase_result
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
                                              Worktree.force_push_with_lease
                                                ~process_mgr ~path:wt_path
                                                ~branch
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
                                                  "Conflict force-push skipped \
                                                   — branch has no commits \
                                                   ahead of base"
                                            | Worktree.Push_rejected ->
                                                log_event runtime ~patch_id
                                                  "Conflict force-push \
                                                   rejected — lease violated"
                                            | Worktree.Push_worktree_missing ->
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
                                      | Orchestrator.Conflict_give_up -> `Failed
                                    )
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
                                      resolve_worktree_path ~process_mgr
                                        ~repo_root:config.repo_root
                                        ~project_name ~patch_id ~agent ()
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
                                      | Patch_decision.Merge_conflict_payload ->
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
                                              ?pr_number ?patch:patch_for_layer
                                              ~gameplan
                                              ~base_branch:base_branch_for_layer
                                              ()
                                          else
                                            Prompt.render_ci_failure_prompt
                                              ~project_name ?agents_md
                                              ?pr_number ?patch:patch_for_layer
                                              ~gameplan
                                              ~base_branch:base_branch_for_layer
                                              failed_checks
                                      | Patch_decision.Review_payload
                                          { comments } ->
                                          let current_head_sha =
                                            Base.Option.bind fresh_pr_state
                                              ~f:(fun ps ->
                                                ps.Pr_state.head_oid)
                                          in
                                          Prompt.render_review_prompt
                                            ~project_name ?agents_md ?pr_number
                                            ?current_head_sha
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
                                           | Unix.Unix_error (Unix.ENOENT, _, _)
                                           ->
                                             ());
                                          Prompt.render_findings_prompt
                                            ~project_name ?agents_md ?pr_number
                                            ?current_head_sha
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
                                            Project_store.pr_body_artifact_path
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
                                           | Unix.Unix_error (Unix.ENOENT, _, _)
                                           ->
                                             ());
                                          Prompt.render_pr_body_prompt
                                            ~project_name
                                            ~pr_number:
                                              (Base.Option.value_exn pr_number)
                                            ~pr_body ~spec_suffix ~artifact_path
                                      | Patch_decision.Merge_conflict_payload ->
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
                                              .record_delivered_ci_run_ids orch
                                                patch_id ids)
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
                                      Prompt.render_gameplan_layer ~project_name
                                        gameplan
                                    in
                                    let patch_prompt =
                                      match patch_for_layer with
                                      | Some p ->
                                          Prompt.render_patch_layer
                                            ~project_name p ?pr_number
                                            ~base_branch:base_branch_for_layer
                                            ()
                                      | None -> ""
                                    in
                                    let result, tool_failures =
                                      run_llm_session ~sw ~gameplan_prompt
                                        ~patch_prompt ~kind:(Some kind)
                                        ~patch_id ~prompt ~agent ~on_pr_detected
                                        ~complexity
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
                                            apply_pr_body_artifact ~runtime ~net
                                              ~github ~project_name ~patch_id
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
                                            .resolve_after_session ~net ~clock
                                              ~log:(fun msg ->
                                                log_event runtime ~patch_id msg)
                                              ~findings_registry ~artifact_path
                                              ~delivered:findings
                                              ~actor:
                                                (Printf.sprintf "onton:%s"
                                                   (Patch_id.to_string patch_id))
                                              ();
                                            result)
                                          else
                                            let ids =
                                              Base.List.map findings
                                                ~f:(fun
                                                    (f : Review_service.finding)
                                                  -> f.Review_service.id)
                                            in
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "Session failed before \
                                                  resolving findings; \
                                                  forgetting delivered finding \
                                                  registry entries: %s"
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
                                      | Patch_decision.Merge_conflict_payload ->
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
                                      | Patch_decision.Sync_attempt_pr_body -> (
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
                                                (apply_pr_body_artifact ~runtime
                                                   ~net ~github ~project_name
                                                   ~patch_id ~pr_number:pr
                                                   ~patch ~gameplan)
                                          | None ->
                                              log_event runtime ~patch_id
                                                "pr-body: skipping \
                                                 opportunistic sync — patch \
                                                 has no gameplan entry (likely \
                                                 ad-hoc)";
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
                                              Orchestrator.set_pr_body_delivered
                                                orch patch_id true
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
                          Orchestrator.apply_respond_outcome orch patch_id kind
                            respond_outcome);
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
        (try reconcile_and_execute_automerge ~runtime ~net ~github with
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

(** {1 Persistence fiber} *)

(** Periodic persistence fiber — saves runtime snapshot every 5 seconds. *)
let persistence_fiber ~runtime ~clock ~project_name ~transcripts =
  let path = Project_store.snapshot_path project_name in
  Project_store.ensure_dir (Stdlib.Filename.dirname path);
  let rec loop () =
    Eio.Time.sleep clock 5.0;
    (* Sync transcripts into the snapshot before saving *)
    Runtime.update runtime (fun snap ->
        let t = snap.Runtime.transcripts in
        Base.Hashtbl.clear t;
        Hashtbl.iter (fun k v -> Base.Hashtbl.set t ~key:k ~data:v) transcripts;
        snap);
    let snap = Runtime.snapshot_unsync runtime in
    (match Persistence.save ~path snap with
    | Ok () -> ()
    | Error msg ->
        log_event runtime (Printf.sprintf "Persistence save failed — %s" msg));
    loop ()
  in
  loop ()

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
    if Base.String.is_empty t then infer_github_token () else t
  in
  let owner, repo =
    match infer_owner_repo ~repo_root with
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
    | None -> Branch.of_string (infer_default_branch ~repo_root)
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
      let gameplan =
        Gameplan.
          {
            project_name;
            problem_statement = "";
            solution_summary = "";
            final_state_spec = "";
            patches = [];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
            open_questions = [];
            functional_changes = [];
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
      | Ok parsed ->
          let gameplan = parsed.Gameplan_parser.gameplan in
          let project_name =
            match project with
            | Some p -> p
            | None -> gameplan.Gameplan.project_name
          in
          let repo_root = repo_root_for_fresh in
          let token, owner, repo =
            resolve_github_credentials ~github_token ~repo_root
          in
          let backend, model = resolve_backend_model ~backend ~model in
          let main_branch = resolve_branch ~repo_root main_branch in
          Project_store.save_config ~project_name ~github_token:token
            ~github_owner:owner ~github_repo:repo ~backend ~model
            ~main_branch:(Branch.to_string main_branch)
            ~poll_interval ~repo_root ~max_concurrency;
          Project_store.save_gameplan_source ~project_name ~source_path:gp_path;
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
          with_snapshot_load ~project_name config gameplan)
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
                  let owner =
                    let s =
                      Base.String.strip stored.Project_store.github_owner
                    in
                    if Base.String.is_empty s then inferred_owner else s
                  in
                  let repo =
                    let s =
                      Base.String.strip stored.Project_store.github_repo
                    in
                    if Base.String.is_empty s then inferred_repo else s
                  in
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
      (match
         validate_branch_resolves ~repo_root:config.repo_root
           ~main_branch:config.main_branch
       with
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
      let github =
        Github.create ~token:config.github_token ~owner:config.github_owner
          ~repo:config.github_repo
      in
      let net = Eio.Stdenv.net env in
      (match Github.check_repo_access ~net github with
      | Ok () -> ()
      | Error err ->
          Printf.eprintf "Error: cannot access GitHub repo %s/%s: %s\n"
            config.github_owner config.github_repo (Github.show_error err);
          Stdlib.exit 1);
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
      let process_mgr = Eio.Stdenv.process_mgr env in
      let clock = Eio.Stdenv.clock env in
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
        Startup_reconciler.recover_worktrees ~process_mgr
          ~repo_root:config.repo_root ~patches:gameplan.Gameplan.patches
      in
      let reconciliation_fiber () =
        let startup =
          Startup_reconciler.reconcile ~net ~github
            ~patches:gameplan.Gameplan.patches ~repo_root:config.repo_root
            ~process_mgr ~agents:pre_agents
            ~pre_recovered_worktrees:pre_worktrees ()
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
      let review_backends = repo_config.Repo_config.review_backends in
      let findings_registry = Findings_registry.create () in
      let common_fibers =
        [
          reconciliation_fiber;
          (fun () ->
            poller_fiber ~runtime ~clock ~net ~process_mgr ~github ~config
              ~project_name ~pr_registry ~branch_of ~event_log ~review_backends
              ~findings_registry);
          (fun () ->
            persistence_fiber ~runtime ~clock ~project_name ~transcripts);
        ]
      in
      if config.headless then
        Eio.Fiber.all
          ((fun () -> headless_fiber ~runtime ~clock ~stdout)
          :: (fun () ->
            runner_fiber ~runtime ~env ~config ~pick_backend ~project_name
              ~pr_registry ~findings_registry ~review_backends ~transcripts
              ~github ~net ~event_log ())
          :: common_fibers)
      else
        let list_selected = ref 0 in
        let detail_scroll = ref 0 in
        let detail_follow = ref false in
        let timeline_scroll = ref 0 in
        let detail_scrolls : (Patch_id.t, int * bool) Hashtbl.t =
          Hashtbl.create 16
        in
        let view_mode = ref Tui.List_view in
        let sorted_patch_ids = ref [] in
        let input_mode = ref Tui_input.Normal in
        let prompt_line = ref None in
        let show_help = ref false in
        let status_msg : Tui.status_msg option ref = ref None in
        let patches_start_row = ref 0 in
        let patches_scroll_offset = ref 0 in
        let patches_visible_count = ref 0 in
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
                   tui_fiber ~runtime ~clock ~stdout ~list_selected
                     ~detail_scroll ~detail_follow ~timeline_scroll ~view_mode
                     ~show_help ~status_msg ~transcripts ~sorted_patch_ids
                     ~input_mode ~prompt_line ~patches_start_row
                     ~patches_scroll_offset ~patches_visible_count
                     ~backend_name:(backend_name (fst default_backend))
                     ~resolve_routing)
                :: (fun () ->
                  input_fiber ~runtime ~process_mgr ~net ~github ~list_selected
                    ~detail_scroll ~detail_follow ~timeline_scroll
                    ~detail_scrolls ~view_mode ~pr_registry ~project_name
                    ~show_help ~status_msg ~sorted_patch_ids ~input_mode
                    ~prompt_line ~patches_start_row ~patches_scroll_offset
                    ~patches_visible_count ~owner:config.github_owner
                    ~repo:config.github_repo ~resolve_routing)
                :: (fun () ->
                  runner_fiber ~runtime ~env ~config ~pick_backend ~project_name
                    ~pr_registry ~findings_registry ~review_backends
                    ~transcripts ~github ~net ~event_log ~status_msg ())
                :: common_fibers)
            with Quit_tui -> ())

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

let main_cmd =
  let open Cmdliner in
  let run_cmd project gameplan_path github_token backend model main_branch
      poll_interval repo_root max_concurrency headless upload_debug no_lock =
    if upload_debug then (
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
      $ max_concurrency_arg $ headless_arg $ upload_debug_arg $ no_lock_arg)
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
