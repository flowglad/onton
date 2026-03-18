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

(** Infer GitHub owner/repo from [git remote get-url origin] in [repo_root].
    Parses both HTTPS and SSH remote URLs. *)
let infer_owner_repo ~repo_root =
  let buf = Buffer.create 128 in
  try
    let ic =
      Unix.open_process_in
        (Printf.sprintf "git -C %s remote get-url origin 2>/dev/null"
           (Filename.quote repo_root))
    in
    (try
       while true do
         Buffer.add_char buf (input_char ic)
       done
     with End_of_file -> ());
    (match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> raise Exit);
    let url = Base.String.strip (Buffer.contents buf) in
    let re =
      Re.Pcre.re {|github\.com[:/]([^/]+)/([^/\s]+?)(?:\.git)?/?$|}
      |> Re.compile
    in
    match Re.exec_opt re url with
    | Some g -> Some (Re.Group.get g 1, Re.Group.get g 2)
    | None -> None
  with _ -> None

(** Resolve GitHub token: check GITHUB_TOKEN env var, then try [gh auth token].
*)
let infer_github_token () =
  match Stdlib.Sys.getenv_opt "GITHUB_TOKEN" with
  | Some t when not (Base.String.is_empty (Base.String.strip t)) ->
      Base.String.strip t
  | _ -> (
      let buf = Buffer.create 128 in
      try
        let ic = Unix.open_process_in "gh auth token 2>/dev/null" in
        (try
           while true do
             Buffer.add_char buf (input_char ic)
           done
         with End_of_file -> ());
        (match Unix.close_process_in ic with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> raise Exit);
        let t = Base.String.strip (Buffer.contents buf) in
        if Base.String.is_empty t then "" else t
      with _ -> "")

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

(** Set or unset draft status on a PR. [draft:true] converts to draft,
    [draft:false] marks ready for review. Errors are logged but not fatal. *)
let set_pr_draft ~process_mgr ~token ~owner ~repo ~pr_number ~draft =
  let pr_str = Int.to_string (Pr_number.to_int pr_number) in
  let args =
    [ "gh"; "pr"; "ready"; pr_str; "--repo"; Printf.sprintf "%s/%s" owner repo ]
    @ if draft then [ "--undo" ] else []
  in
  let env =
    Array.append [| Printf.sprintf "GH_TOKEN=%s" token |] (Unix.environment ())
  in
  try Eio.Process.run ~env process_mgr args
  with exn ->
    Printf.eprintf "set_pr_draft failed (PR #%s, draft=%b): %s\n%!" pr_str draft
      (Printexc.to_string exn)

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
  | Activity_log.Stream_entry.Tool_use name -> Printf.sprintf "Tool: %s" name
  | Activity_log.Stream_entry.Text_chunk text -> text
  | Activity_log.Stream_entry.Finished reason ->
      Printf.sprintf "Finished (%s)" reason
  | Activity_log.Stream_entry.Stream_error msg ->
      Printf.sprintf "Stream error: %s" msg

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
    ~map_stream:(fun (s : Activity_log.Stream_entry.t) ->
      Tui.Event
        {
          patch_id =
            Some (Patch_id.to_string s.Activity_log.Stream_entry.patch_id);
          message = format_stream_kind s.Activity_log.Stream_entry.kind;
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

(** Terminal failure — forces [Given_up] so [complete] raises intervention. Used
    for non-retryable errors (patch not found, PR discovery failed). *)
let mark_session_failed runtime patch_id =
  Runtime.update_orchestrator runtime (fun orch ->
      let orch = Orchestrator.set_session_failed orch patch_id in
      let orch = Orchestrator.set_tried_fresh orch patch_id in
      Orchestrator.complete orch patch_id)

(** Compute the session mode for the fallback chain.

    The fallback chain is: continue existing session → fresh session → give up.
    - [Fresh_available] with [has_pr]: use [--continue] to resume worktree
      session.
    - [Fresh_available] without [has_pr], or [Tried_fresh]: start fresh (no
      --continue).
    - [Given_up]: the agent has exhausted its fallback chain — return
      [`Give_up]. *)
let session_mode (agent : Patch_agent.t) : [ `Continue | `Fresh | `Give_up ] =
  match agent.Patch_agent.session_fallback with
  | Patch_agent.Given_up -> `Give_up
  | Patch_agent.Tried_fresh -> `Fresh
  | Patch_agent.Fresh_available ->
      if agent.Patch_agent.has_pr then `Continue else `Fresh

(** Extract a PR number from text containing a GitHub PR URL. Scans for
    [github.com/owner/repo/pull/N] patterns. *)
let extract_pr_number_from_text ~owner ~repo text =
  let needle = Printf.sprintf "github.com/%s/%s/pull/" owner repo in
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec scan i =
    if i + needle_len >= text_len then None
    else if String.sub text i needle_len = needle then
      (* Extract digits after the needle *)
      let start = i + needle_len in
      let rec end_pos j =
        if j < text_len && text.[j] >= '0' && text.[j] <= '9' then
          end_pos (j + 1)
        else j
      in
      let stop = end_pos start in
      if stop > start then
        try
          Some
            (Pr_number.of_int
               (int_of_string (String.sub text start (stop - start))))
        with _ -> scan (i + 1)
      else scan (i + 1)
    else scan (i + 1)
  in
  scan 0

(** Log a stream entry to the activity log. *)
let log_stream_entry runtime ~patch_id kind =
  Runtime.update_activity_log runtime (fun log ->
      Activity_log.add_stream_entry log
        (Activity_log.Stream_entry.create ~timestamp:(Unix.gettimeofday ())
           ~patch_id ~kind))

(** Run a Claude process with streaming and handle the result. Returns [`Ok] on
    successful Claude exit (code 0), otherwise [`Failed]. The [on_pr_detected]
    callback is invoked if a PR number is found in Claude's text output.
    Implements the session fallback chain: resume → fresh → give up. On success,
    stores the session ID and clears fallback state. *)

let truncate s n = if String.length s <= n then s else String.sub s 0 n ^ "..."

let run_claude_and_handle ~runtime ~process_mgr ~fs ~project_name ~patch_id
    ~repo_root ~prompt ~(agent : Patch_agent.t) ~owner ~repo ~on_pr_detected
    ~transcripts =
  match session_mode agent with
  | `Give_up ->
      log_event runtime ~patch_id
        "session fallback exhausted (continue failed, fresh failed), needs \
         intervention";
      mark_session_failed runtime patch_id;
      `Failed
  | (`Continue | `Fresh) as mode -> (
      let continue, is_fresh =
        match mode with `Continue -> (true, false) | `Fresh -> (false, true)
      in
      let worktree_path =
        match agent.Patch_agent.worktree_path with
        | Some p -> p
        | None ->
            (* Try to find an existing worktree for the patch's branch before
               falling back to the default computed path. *)
            let found =
              match agent.Patch_agent.head_branch with
              | Some branch ->
                  Worktree.find_for_branch ~process_mgr ~repo_root branch
              | None -> None
            in
            let path =
              match found with
              | Some p -> p
              | None -> Worktree.worktree_dir ~project_name ~patch_id
            in
            (* Persist so we don't re-discover every time *)
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.set_worktree_path orch patch_id path);
            path
      in
      (* Ensure worktree exists — create if needed *)
      (if not (Stdlib.Sys.file_exists worktree_path) then
         match agent.Patch_agent.head_branch with
         | None ->
             log_event runtime ~patch_id
               "worktree not ready and branch unknown, waiting for poller";
             Runtime.update_orchestrator runtime (fun orch ->
                 Orchestrator.complete orch patch_id)
         | Some branch ->
             let base =
               match agent.Patch_agent.base_branch with
               | Some b -> Branch.to_string b
               | None -> "HEAD"
             in
             let patch =
               Types.Patch.
                 {
                   id = patch_id;
                   title = "";
                   description = "";
                   branch;
                   dependencies = [];
                   spec = "";
                   acceptance_criteria = [];
                   files = [];
                 }
             in
             log_event runtime ~patch_id
               (Printf.sprintf "creating worktree at %s" worktree_path);
             ignore
               (Worktree.create ~process_mgr ~repo_root ~project_name ~patch
                  ~base_ref:base);
             Runtime.update_orchestrator runtime (fun orch ->
                 Orchestrator.set_worktree_path orch patch_id worktree_path));
      if not (Stdlib.Sys.file_exists worktree_path) then (
        log_event runtime ~patch_id
          (Printf.sprintf "worktree still missing at %s" worktree_path);
        Runtime.update_orchestrator runtime (fun orch ->
            Orchestrator.complete orch patch_id);
        `Failed)
      else
        let cwd = Eio.Path.(fs / worktree_path) in
        let text_buf =
          let buf = Buffer.create 4096 in
          (match Hashtbl.find_opt transcripts patch_id with
          | Some prev when String.length prev > 0 ->
              Buffer.add_string buf prev;
              Buffer.add_char buf '\n'
          | Some _ | None -> ());
          (* Write the prompt being delivered so it appears in the transcript *)
          let now = Unix.gettimeofday () in
          let tm = Unix.localtime now in
          Buffer.add_string buf
            (Printf.sprintf
               "\n---\n**[%02d:%02d:%02d] Delivered to Claude%s:**\n\n"
               tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
               (if continue then " (--continue)" else ""));
          Buffer.add_string buf prompt;
          Buffer.add_string buf "\n\n---\n**Claude response:**\n\n";
          Hashtbl.replace transcripts patch_id (Buffer.contents buf);
          buf
        in
        let error_buf = Buffer.create 256 in
        let tool_count = ref 0 in
        let pr_found = ref false in
        let needle_len =
          String.length (Printf.sprintf "github.com/%s/%s/pull/" owner repo)
        in
        let on_event (event : Types.Stream_event.t) =
          match event with
          | Types.Stream_event.Text_delta text -> (
              let prev_len = Buffer.length text_buf in
              Buffer.add_string text_buf text;
              Hashtbl.replace transcripts patch_id (Buffer.contents text_buf);
              if not !pr_found then
                let offset = max 0 (prev_len - needle_len) in
                let tail =
                  Buffer.sub text_buf offset (Buffer.length text_buf - offset)
                in
                match extract_pr_number_from_text ~owner ~repo tail with
                | Some pr_number ->
                    pr_found := true;
                    log_stream_entry runtime ~patch_id
                      (Activity_log.Stream_entry.Text_chunk
                         (Printf.sprintf "PR #%d detected"
                            (Pr_number.to_int pr_number)));
                    on_pr_detected pr_number
                | None -> ())
          | Types.Stream_event.Tool_use { name; _ } ->
              tool_count := !tool_count + 1;
              Buffer.add_string text_buf
                (Printf.sprintf "\n\n---\n`[tool: %s]`\n\n" name);
              Hashtbl.replace transcripts patch_id (Buffer.contents text_buf);
              log_stream_entry runtime ~patch_id
                (Activity_log.Stream_entry.Tool_use name)
          | Types.Stream_event.Final_result { stop_reason; _ } ->
              let reason = Types.Stop_reason.show stop_reason in
              log_stream_entry runtime ~patch_id
                (Activity_log.Stream_entry.Finished reason)
          | Types.Stream_event.Error msg ->
              if Buffer.length error_buf > 0 then Buffer.add_char error_buf '\n';
              Buffer.add_string error_buf msg;
              log_stream_entry runtime ~patch_id
                (Activity_log.Stream_entry.Stream_error msg)
        in
        let result =
          try
            Ok
              (Claude_runner.run_streaming ~process_mgr ~cwd ~patch_id ~prompt
                 ~continue ~on_event)
          with exn -> Error (Printexc.to_string exn)
        in
        let open Run_classification in
        let outcome =
          Result.map
            (fun (r : Claude_runner.result) ->
              {
                exit_code = r.Claude_runner.exit_code;
                got_events = r.Claude_runner.got_events;
                stderr = r.Claude_runner.stderr;
                stream_errors = String.trim (Buffer.contents error_buf);
              })
            result
        in
        match classify ~continue outcome with
        | Process_error msg ->
            log_event runtime ~patch_id
              (Printf.sprintf "Claude process error: %s" msg);
            Runtime.update_orchestrator runtime (fun orch ->
                let orch =
                  Orchestrator.on_session_failure orch patch_id ~is_fresh
                in
                Orchestrator.complete orch patch_id);
            `Failed
        | No_session_to_resume ->
            log_event runtime ~patch_id
              "--continue produced no events (no session to resume), will \
               retry fresh";
            Runtime.update_orchestrator runtime (fun orch ->
                let orch =
                  Orchestrator.on_session_failure orch patch_id ~is_fresh:false
                in
                Orchestrator.complete orch patch_id);
            `Failed
        | Success { stream_errors } ->
            if String.length stream_errors > 0 then
              log_event runtime ~patch_id
                (Printf.sprintf "Claude exited 0 but had stream errors: %s"
                   (truncate stream_errors 500));
            let text_len = Buffer.length text_buf in
            let tools = !tool_count in
            if tools = 0 && text_len < 200 then
              log_event runtime ~patch_id
                (Printf.sprintf
                   "Claude exited 0 with no tool use and %d chars of text: %s"
                   text_len
                   (truncate (String.trim (Buffer.contents text_buf)) 200));
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.clear_session_fallback orch patch_id);
            `Ok
        | Session_failed { exit_code; detail } ->
            log_event runtime ~patch_id
              (Printf.sprintf
                 "Claude exited with code %d, marking session failed: %s"
                 exit_code detail);
            Runtime.update_orchestrator runtime (fun orch ->
                let orch =
                  Orchestrator.on_session_failure orch patch_id ~is_fresh
                in
                Orchestrator.complete orch patch_id);
            `Failed)

(** {1 Fibers} *)

exception Quit_tui
(** Raised by the input fiber to signal a clean exit. *)

(** TUI rendering fiber — redraws the terminal at ~10 fps.

    [list_selected], [detail_scroll], [timeline_scroll], and [view_mode] are
    shared mutable refs updated by the input fiber. *)
let tui_fiber ~runtime ~clock ~stdout ~list_selected ~detail_scroll
    ~detail_follow ~timeline_scroll ~view_mode ~show_help ~status_msg
    ~transcripts ~sorted_patch_ids ~input_line ~completion_hint
    ~patches_start_row ~patches_scroll_offset ~patches_visible_count =
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
    let views =
      Tui.views_of_orchestrator ~orchestrator:orch ~gameplan:gp ~activity
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
        ~show_help:!show_help ~transcript ?status_msg:!status_msg
        ?input_line:!input_line ?completion_hint:!completion_hint views
    in
    (* Write back the clamped scroll offset so delta-based input in
       input_fiber works from a real value, not a sentinel like max_value. *)
    detail_scroll := Tui.detail_scroll_offset frame;
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
    - Normal mode: single-key navigation (j/k, arrows, q to quit, enter for
      detail)
    - Text mode: entered via [:], accumulates a line buffer, dispatched on Enter

    Text-mode commands (parsed by {!Tui_input.parse_line}):
    - ["N> message"] — send human message to patch N
    - ["+123"] — register ad-hoc PR #123 for the selected patch
    - ["w /path"] — register existing worktree directory for the selected patch
    - ["-"] — remove the selected patch from orchestration *)
let input_fiber ~runtime ~list_selected ~detail_scroll ~detail_follow
    ~timeline_scroll ~detail_scrolls ~view_mode ~pr_registry ~project_name
    ~show_help ~status_msg ~sorted_patch_ids ~input_line ~completion_hint
    ~patches_start_row ~patches_scroll_offset ~patches_visible_count =
  let buf = Buffer.create 64 in
  let text_mode = ref false in
  let current_completions = ref [] in
  let recompute_completions () =
    let buffer = Buffer.contents buf in
    let patch_ids = Base.List.map !sorted_patch_ids ~f:Patch_id.to_string in
    current_completions := Completions.complete ~buffer ~patch_ids;
    completion_hint :=
      match !current_completions with
      | first :: _ ->
          let full = first.Completions.full in
          if Base.String.is_prefix full ~prefix:buffer then
            Some (Base.String.drop_prefix full (Base.String.length buffer))
          else None
      | [] -> None
  in
  let sync_input () =
    input_line := if !text_mode then Some (Buffer.contents buf) else None;
    if !text_mode then recompute_completions ()
    else (
      current_completions := [];
      completion_hint := None)
  in
  let history = Tui_input.History.create () in
  let saved_draft = ref "" in
  let eof_count = ref 0 in
  let last_click_time = ref 0.0 in
  let last_click_row = ref (-1) in
  let rec loop () =
    sync_input ();
    match Term.Key.read () with
    | None ->
        (* Transient EOF can happen if a child process (e.g. script/gh)
           briefly interferes with the terminal. Retry a few times before
           giving up. *)
        eof_count := !eof_count + 1;
        if !eof_count >= 10 then
          log_event runtime "input fiber: stdin closed (10 consecutive EOFs)"
        else (
          Eio.Fiber.yield ();
          loop ())
    | Some key -> (
        eof_count := 0;
        if !show_help then (
          show_help := false;
          loop ())
        else if !text_mode then
          match key with
          | Term.Key.Paste text ->
              (* Strip trailing newlines — pasted text is submitted via Enter *)
              let text =
                Base.String.rstrip text ~drop:(fun c ->
                    Char.equal c '\n' || Char.equal c '\r')
              in
              (* Replace internal newlines with spaces for single-line input *)
              let text = Base.String.tr text ~target:'\n' ~replacement:' ' in
              let text = Base.String.tr text ~target:'\r' ~replacement:' ' in
              Buffer.add_string buf text;
              loop ()
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
              (* In detail view, bare text (no N> prefix) is sent to the
                 currently viewed patch as a human message. *)
              let parsed =
                let p = Tui_input.parse_line line in
                match (p, !view_mode) with
                | None, Tui.Detail_view pid
                  when not (Base.String.is_empty (Base.String.strip line)) ->
                    Some (Tui_input.Send_message (pid, Base.String.strip line))
                | None, (Tui.List_view | Tui.Timeline_view | Tui.Detail_view _)
                | Some _, _ ->
                    p
              in
              (match parsed with
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
              | Some (Tui_input.Add_pr pr_number) ->
                  let patch_id =
                    Patch_id.of_string
                      (Int.to_string (Pr_number.to_int pr_number))
                  in
                  let already_exists =
                    Runtime.read runtime (fun snap ->
                        Base.Option.is_some
                          (Orchestrator.find_agent snap.Runtime.orchestrator
                             patch_id))
                  in
                  if already_exists then
                    log_event runtime ~patch_id
                      (Printf.sprintf "Ad-hoc PR #%d already registered"
                         (Pr_number.to_int pr_number))
                  else (
                    Pr_registry.register pr_registry ~patch_id ~pr_number;
                    Runtime.update_orchestrator runtime (fun orch ->
                        Orchestrator.add_agent orch ~patch_id ~pr_number);
                    log_event runtime ~patch_id
                      (Printf.sprintf "Ad-hoc PR #%d added"
                         (Pr_number.to_int pr_number)))
              | Some (Tui_input.Add_worktree path) -> (
                  let info_opt =
                    let pids = !sorted_patch_ids in
                    let count = Base.List.length pids in
                    if count = 0 then None
                    else
                      let idx =
                        Base.Int.max 0 (Base.Int.min !list_selected (count - 1))
                      in
                      let pid = Base.List.nth_exn pids idx in
                      let busy =
                        Runtime.read runtime (fun snap ->
                            (Orchestrator.agent snap.Runtime.orchestrator pid)
                              .Patch_agent.busy)
                      in
                      Some (pid, busy)
                  in
                  match info_opt with
                  | None ->
                      log_event runtime
                        "Cannot add worktree: no selectable patch"
                  | Some (patch_id, busy) -> (
                      if busy then
                        log_event runtime ~patch_id
                          "Warning: patch is currently running — changing \
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
                        let git_file = Stdlib.Filename.concat raw_path ".git" in
                        if
                          (not (Stdlib.Sys.file_exists git_file))
                          || Stdlib.Sys.is_directory git_file
                        then
                          failwith
                            ("Path is not a git worktree (no .git file): "
                           ^ raw_path);
                        let canonical_expected =
                          try Unix.realpath expected
                          with Unix.Unix_error (Unix.ENOENT, _, _) -> expected
                        in
                        if not (String.equal canonical_real canonical_expected)
                        then (
                          let parent = Stdlib.Filename.dirname expected in
                          (try Unix.mkdir parent 0o755 with
                          | Unix.Unix_error (Unix.ENOENT, _, _) ->
                              failwith
                                ("Cannot create parent directory: " ^ parent)
                          | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
                          (match Unix.lstat expected with
                          | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
                          | { Unix.st_kind = Unix.S_LNK; _ } ->
                              Unix.unlink expected
                          | {
                           Unix.st_kind =
                             ( Unix.S_REG | Unix.S_DIR | Unix.S_CHR | Unix.S_BLK
                             | Unix.S_FIFO | Unix.S_SOCK );
                           _;
                          } ->
                              failwith
                                (Printf.sprintf
                                   "Cannot overwrite non-symlink at %s" expected));
                          Unix.symlink canonical_real expected);
                        Runtime.update_orchestrator runtime (fun orch ->
                            let orch =
                              Orchestrator.clear_needs_intervention orch
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
                               "Worktree registered: symlinked %s → %s" expected
                               canonical_real)
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
                            (Printf.sprintf "Failed to add worktree: %s" msg)
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
                            (Printf.sprintf "Failed to add worktree: %s" msg)))
              | Some Tui_input.Remove_patch -> (
                  let info_opt =
                    let pids = !sorted_patch_ids in
                    let count = Base.List.length pids in
                    if count = 0 then None
                    else
                      let idx =
                        Base.Int.max 0 (Base.Int.min !list_selected (count - 1))
                      in
                      let pid = Base.List.nth_exn pids idx in
                      Runtime.read runtime (fun snap ->
                          let agent =
                            Orchestrator.agent snap.Runtime.orchestrator pid
                          in
                          Some
                            ( agent.Patch_agent.patch_id,
                              agent.Patch_agent.busy,
                              agent.Patch_agent.merged,
                              agent.Patch_agent.removed ))
                  in
                  match info_opt with
                  | None ->
                      log_event runtime
                        "Cannot remove patch: no selectable patch"
                  | Some (patch_id, _busy, true, _) ->
                      log_event runtime ~patch_id
                        "Patch is already merged — nothing to do"
                  | Some (patch_id, _busy, _, true) ->
                      log_event runtime ~patch_id
                        "Patch is already removed — nothing to do"
                  | Some (patch_id, busy, false, false) ->
                      if busy then
                        log_event runtime ~patch_id
                          "Warning: patch is currently running — it may create \
                           a GitHub PR before stopping";
                      Runtime.update_orchestrator runtime (fun orch ->
                          Orchestrator.mark_removed orch patch_id);
                      log_event runtime ~patch_id
                        "Patch removed from orchestration (dependents remain \
                         blocked)")
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
          | Term.Key.Ctrl 'z' ->
              Term.Raw.suspend ();
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
          | Term.Key.Tab ->
              let buffer = Buffer.contents buf in
              let accepted =
                Completions.accept_first ~buffer
                  ~completions:!current_completions
              in
              if not (String.equal accepted buffer) then (
                Buffer.clear buf;
                Buffer.add_string buf accepted);
              loop ()
          | Term.Key.Left | Term.Key.Right | Term.Key.Home | Term.Key.End
          | Term.Key.Page_up | Term.Key.Page_down | Term.Key.F _
          | Term.Key.Ctrl _ | Term.Key.Mouse _ | Term.Key.Unknown _ ->
              loop ()
        else if Term.Key.equal key (Term.Key.Ctrl 'z') then (
          Term.Raw.suspend ();
          loop ())
        else if Term.Key.equal key (Term.Key.Char ':') then (
          Buffer.clear buf;
          text_mode := true;
          loop ())
        else
          match key with
          | Term.Key.Paste text -> (
              (* In detail view, auto-enter text mode and buffer the paste *)
              match !view_mode with
              | Tui.Detail_view _ ->
                  Buffer.clear buf;
                  let text =
                    Base.String.rstrip text ~drop:(fun c ->
                        Char.equal c '\n' || Char.equal c '\r')
                  in
                  let text =
                    Base.String.tr text ~target:'\n' ~replacement:' '
                  in
                  let text =
                    Base.String.tr text ~target:'\r' ~replacement:' '
                  in
                  Buffer.add_string buf text;
                  text_mode := true;
                  loop ()
              | Tui.List_view | Tui.Timeline_view -> loop ())
          | Term.Key.Mouse ev -> (
              match (ev, !view_mode) with
              | ( Term.Click { button = Term.Left; row; press = true; _ },
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
              | ( Term.Click { button = Term.Left; row; press = true; _ },
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
              | Term.Scroll { dir; _ }, Tui.List_view ->
                  let count = Base.List.length !sorted_patch_ids in
                  let delta = match dir with Term.Up -> -1 | Term.Down -> 1 in
                  list_selected :=
                    Base.Int.max 0
                      (Base.Int.min (count - 1) (!list_selected + delta));
                  loop ()
              | Term.Scroll { dir; _ }, Tui.Detail_view _ ->
                  let delta = match dir with Term.Up -> -3 | Term.Down -> 3 in
                  detail_scroll := Base.Int.max 0 (!detail_scroll + delta);
                  loop ()
              | Term.Scroll { dir; _ }, Tui.Timeline_view ->
                  let delta = match dir with Term.Up -> -3 | Term.Down -> 3 in
                  timeline_scroll := Base.Int.max 0 (!timeline_scroll + delta);
                  loop ()
              | ( Term.Click { button = Term.Left | Term.Middle | Term.Right; _ },
                  (Tui.List_view | Tui.Detail_view _ | Tui.Timeline_view) ) ->
                  loop ())
          | Term.Key.Char _ | Term.Key.Enter | Term.Key.Tab | Term.Key.Backspace
          | Term.Key.Escape | Term.Key.Up | Term.Key.Down | Term.Key.Left
          | Term.Key.Right | Term.Key.Home | Term.Key.End | Term.Key.Page_up
          | Term.Key.Page_down | Term.Key.Delete | Term.Key.F _
          | Term.Key.Ctrl _ | Term.Key.Unknown _ -> (
              let cmd = Tui_input.of_key key in
              match cmd with
              | Tui_input.Quit -> raise Quit_tui
              | Tui_input.Move_up | Tui_input.Move_down | Tui_input.Page_up
              | Tui_input.Page_down ->
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
                  | Tui.Detail_view _ ->
                      let delta =
                        match cmd with
                        | Tui_input.Move_up -> -1
                        | Tui_input.Move_down -> 1
                        | Tui_input.Page_up -> -10
                        | Tui_input.Page_down -> 10
                        | Tui_input.Quit | Tui_input.Refresh | Tui_input.Help
                        | Tui_input.Select | Tui_input.Back | Tui_input.Timeline
                        | Tui_input.Noop | Tui_input.Send_message _
                        | Tui_input.Add_pr _ | Tui_input.Add_worktree _
                        | Tui_input.Remove_patch ->
                            0
                      in
                      detail_follow := false;
                      detail_scroll := Base.Int.max 0 (!detail_scroll + delta));
                  loop ()
              | Tui_input.Select -> (
                  match !view_mode with
                  | Tui.List_view ->
                      let pids = !sorted_patch_ids in
                      let count = Base.List.length pids in
                      if count > 0 then (
                        let idx =
                          Base.Int.max 0
                            (Base.Int.min !list_selected (count - 1))
                        in
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
                      text_mode := true;
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
              | Tui_input.Refresh | Tui_input.Noop | Tui_input.Send_message _
              | Tui_input.Add_pr _ | Tui_input.Add_worktree _
              | Tui_input.Remove_patch ->
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
let poller_fiber ~runtime ~clock ~net ~process_mgr ~github ~config ~pr_registry
    ~branch_of ~ci_checks_cache =
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
                let addressed_ids =
                  Runtime.read runtime (fun snap ->
                      match
                        Orchestrator.find_agent snap.Runtime.orchestrator
                          patch_id
                      with
                      | None -> Base.Set.empty (module Types.Comment_id)
                      | Some a -> a.Patch_agent.addressed_comment_ids)
                in
                let poll_result =
                  Poller.poll ~was_merged ~addressed_ids pr_state
                in
                let logs = ref [] in
                Runtime.update_orchestrator runtime (fun orch ->
                    let orch, log_entries =
                      Poll_applicator.apply orch patch_id poll_result
                    in
                    logs := log_entries;
                    (* CI cache side-effect — not pure *)
                    let failed =
                      let failure_conclusions =
                        [
                          "failure";
                          "error";
                          "action_required";
                          "timed_out";
                          "startup_failure";
                        ]
                      in
                      Base.List.filter poll_result.Poller.ci_checks
                        ~f:(fun (c : Ci_check.t) ->
                          Base.List.mem failure_conclusions
                            c.Ci_check.conclusion ~equal:Base.String.equal)
                    in
                    if not (Base.List.is_empty failed) then
                      Hashtbl.replace ci_checks_cache patch_id failed
                    else Hashtbl.remove ci_checks_cache patch_id;
                    (* head_branch and worktree discovery — side-effectful,
                       stays outside Poll_applicator *)
                    let orch =
                      match pr_state.Github.Pr_state.head_branch with
                      | Some b -> Orchestrator.set_head_branch orch patch_id b
                      | None -> orch
                    in
                    let orch =
                      let agent = Orchestrator.agent orch patch_id in
                      match
                        ( agent.Patch_agent.base_branch,
                          pr_state.Github.Pr_state.base_branch )
                      with
                      | None, Some b ->
                          Orchestrator.set_base_branch orch patch_id b
                      | _ -> orch
                    in
                    let orch =
                      let agent = Orchestrator.agent orch patch_id in
                      match
                        ( agent.Patch_agent.worktree_path,
                          agent.Patch_agent.head_branch )
                      with
                      | None, Some branch -> (
                          match
                            Worktree.find_for_branch ~process_mgr
                              ~repo_root:config.repo_root branch
                          with
                          | Some path ->
                              let orch =
                                Orchestrator.set_worktree_path orch patch_id
                                  path
                              in
                              if agent.Patch_agent.needs_intervention then
                                Orchestrator.clear_needs_intervention orch
                                  patch_id
                              else orch
                          | None -> orch)
                      | _ -> orch
                    in
                    orch);
                Base.List.iter !logs
                  ~f:(fun (entry : Poll_applicator.log_entry) ->
                    log_event runtime ~patch_id:entry.Poll_applicator.patch_id
                      entry.Poll_applicator.message);
                if pr_state.Github.Pr_state.ci_checks_truncated then
                  log_event runtime ~patch_id
                    "warning: CI check list was truncated (>100 checks); some \
                     failures may not appear in the prompt"));
    (* Reconcile *)
    let reconcile_logs = ref [] in
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
        let actions =
          Reconciler.reconcile ~graph:(Orchestrator.graph orch) ~main
            ~merged_pr_patches:merged_patches ~branch_of patch_views
        in
        let orch =
          Base.List.fold actions ~init:orch ~f:(fun orch action ->
              match action with
              | Reconciler.Mark_merged pid -> Orchestrator.mark_merged orch pid
              | Reconciler.Enqueue_rebase pid ->
                  reconcile_logs :=
                    ("rebase enqueued by reconciler", pid) :: !reconcile_logs;
                  Orchestrator.enqueue orch pid Operation_kind.Rebase
              | Reconciler.Start_operation _ -> orch)
        in
        { snap with Runtime.orchestrator = orch });
    Base.List.iter (Base.List.rev !reconcile_logs) ~f:(fun (msg, pid) ->
        log_event runtime ~patch_id:pid msg);
    Eio.Time.sleep clock config.poll_interval;
    loop ()
  in
  loop ()

(** Runner fiber — executes orchestrator actions by spawning Claude processes
    concurrently. *)
let runner_fiber ~runtime ~env ~config ~project_name ~pr_registry
    ~ci_checks_cache ~transcripts ?status_msg () =
  let main = config.main_branch in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let set_status ~level ~text ?expires_at () =
    match status_msg with
    | Some r -> r := Some { Tui.level; text; expires_at }
    | None -> ()
  in
  let semaphore = Eio.Semaphore.make config.max_concurrency in
  let with_claude_slot f =
    Eio.Semaphore.acquire semaphore;
    Fun.protect ~finally:(fun () -> Eio.Semaphore.release semaphore) f
  in
  let with_busy_guard ~patch_id f =
    Fun.protect
      ~finally:(fun () ->
        let still_busy =
          Runtime.read runtime (fun snap ->
              (Orchestrator.agent snap.Runtime.orchestrator patch_id)
                .Patch_agent.busy)
        in
        if still_busy then (
          log_event runtime ~patch_id
            "runner: fiber exiting with busy=true, forcing complete";
          Runtime.update_orchestrator runtime (fun orch ->
              Orchestrator.complete orch patch_id)))
      (fun () ->
        try f () with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn ->
            log_event runtime ~patch_id
              (Printf.sprintf "runner: unexpected action exception: %s"
                 (Printexc.to_string exn));
            mark_session_failed runtime patch_id)
  in
  let rec loop sw =
    let actions, gameplan, pre_fire_agents =
      Runtime.read runtime (fun snap ->
          let orch = snap.Runtime.orchestrator in
          let actions =
            Orchestrator.pending_actions orch
              ~patches:snap.Runtime.gameplan.Gameplan.patches
          in
          (* Capture agent state BEFORE fire clears pending_comments.
             Respond clears pending_comments as a postcondition, but the
             runner needs them to build the prompt. *)
          let agent_map =
            Base.List.filter_map actions ~f:(fun action ->
                match action with
                | Orchestrator.Respond (pid, _) | Orchestrator.Rebase (pid, _)
                  ->
                    Some (pid, Orchestrator.agent orch pid)
                | Orchestrator.Start _ -> None)
          in
          (actions, snap.Runtime.gameplan, agent_map))
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
                      with_busy_guard ~patch_id (fun () ->
                          let result =
                            with_claude_slot (fun () ->
                                let agent =
                                  Runtime.read runtime (fun snap ->
                                      Orchestrator.agent
                                        snap.Runtime.orchestrator patch_id)
                                in
                                if
                                  agent.Patch_agent.merged
                                  || agent.Patch_agent.needs_intervention
                                  || not agent.Patch_agent.busy
                                then (
                                  log_event runtime ~patch_id
                                    "runner: action stale after semaphore \
                                     wait, skipping";
                                  `Stale)
                                else
                                  let wt_path =
                                    match
                                      Worktree.find_for_branch ~process_mgr
                                        ~repo_root:config.repo_root
                                        patch.Patch.branch
                                    with
                                    | Some existing_path ->
                                        log_event runtime ~patch_id
                                          (Printf.sprintf
                                             "found existing worktree at %s"
                                             existing_path);
                                        existing_path
                                    | None ->
                                        let default_path =
                                          Worktree.worktree_dir ~project_name
                                            ~patch_id
                                        in
                                        if
                                          not
                                            (Stdlib.Sys.file_exists default_path)
                                        then (
                                          log_event runtime ~patch_id
                                            "creating worktree";
                                          ignore
                                            (Worktree.create ~process_mgr
                                               ~repo_root:config.repo_root
                                               ~project_name ~patch
                                               ~base_ref:
                                                 (Branch.to_string base_branch)));
                                        default_path
                                  in
                                  Runtime.update_orchestrator runtime
                                    (fun orch ->
                                      Orchestrator.set_worktree_path orch
                                        patch_id wt_path);
                                  let prompt =
                                    Prompt.render_patch_prompt ~project_name
                                      ?pr_number:agent.Patch_agent.pr_number
                                      patch gameplan
                                      ~base_branch:
                                        (Branch.to_string base_branch)
                                  in
                                  (* PR detection from stream text is a hint
                                     only — always confirmed via gh pr list
                                     after Claude finishes *)
                                  let on_pr_detected _pr_number = () in
                                  run_claude_and_handle ~runtime ~process_mgr
                                    ~fs ~project_name ~patch_id
                                    ~repo_root:config.repo_root ~prompt ~agent
                                    ~owner:config.github_owner
                                    ~repo:config.github_repo ~on_pr_detected
                                    ~transcripts)
                          in
                          match result with
                          | `Stale -> ()
                          | `Failed ->
                              let agent =
                                Runtime.read runtime (fun snap ->
                                    Orchestrator.agent snap.Runtime.orchestrator
                                      patch_id)
                              in
                              if agent.Patch_agent.needs_intervention then
                                set_status ~level:Tui.Error
                                  ~text:
                                    (Printf.sprintf
                                       "Patch %s: session failed — human \
                                        review needed"
                                       (Patch_id.to_string patch_id))
                                  ()
                          | `Ok ->
                              (* Always confirm via gh pr list *)
                              let rec discover remaining =
                                match
                                  discover_pr_number ~process_mgr
                                    ~token:config.github_token
                                    ~owner:config.github_owner
                                    ~repo:config.github_repo
                                    ~branch:patch.Patch.branch ~base_branch
                                with
                                | Ok pr_number ->
                                    log_event runtime ~patch_id
                                      (Printf.sprintf "PR #%d created"
                                         (Pr_number.to_int pr_number));
                                    Pr_registry.register pr_registry ~patch_id
                                      ~pr_number;
                                    if not (Branch.equal base_branch main) then
                                      set_pr_draft ~process_mgr
                                        ~token:config.github_token
                                        ~owner:config.github_owner
                                        ~repo:config.github_repo ~pr_number
                                        ~draft:true;
                                    Runtime.update_orchestrator runtime
                                      (fun orch ->
                                        let orch =
                                          Orchestrator.set_pr_number orch
                                            patch_id pr_number
                                        in
                                        Orchestrator.complete orch patch_id)
                                | Error _ when remaining > 0 ->
                                    Eio.Time.sleep clock 2.0;
                                    discover (remaining - 1)
                                | Error msg ->
                                    log_event runtime ~patch_id
                                      (Printf.sprintf "PR discovery failed: %s"
                                         msg);
                                    Runtime.update_orchestrator runtime
                                      (fun orch ->
                                        let orch =
                                          Orchestrator.on_pr_discovery_failure
                                            orch patch_id
                                        in
                                        Orchestrator.complete orch patch_id)
                              in
                              discover 2)))
          | Orchestrator.Rebase (patch_id, new_base) ->
              Some
                (fun () ->
                  with_busy_guard ~patch_id (fun () ->
                      let agent =
                        Runtime.read runtime (fun snap ->
                            Orchestrator.agent snap.Runtime.orchestrator
                              patch_id)
                      in
                      let wt_path =
                        match agent.Patch_agent.worktree_path with
                        | Some p -> p
                        | None -> Worktree.worktree_dir ~project_name ~patch_id
                      in
                      let rebase_result =
                        Worktree.rebase_onto ~process_mgr ~path:wt_path
                          ~target:new_base
                      in
                      (match rebase_result with
                      | Worktree.Ok ->
                          log_event runtime ~patch_id
                            (Printf.sprintf "runner: rebase onto %s succeeded"
                               (Branch.to_string new_base))
                      | Worktree.Noop ->
                          log_event runtime ~patch_id
                            "runner: rebase is a noop (already up-to-date)"
                      | Worktree.Conflict ->
                          log_event runtime ~patch_id
                            "runner: rebase conflict, enqueuing merge-conflict"
                      | Worktree.Error msg ->
                          log_event runtime ~patch_id
                            (Printf.sprintf "runner: rebase error: %s" msg));
                      Runtime.update_orchestrator runtime (fun orch ->
                          Orchestrator.apply_rebase_result orch patch_id
                            rebase_result new_base);
                      (* Update PR draft status after successful rebase *)
                      match rebase_result with
                      | Worktree.Ok | Worktree.Noop -> (
                          let agent =
                            Runtime.read runtime (fun snap ->
                                Orchestrator.agent snap.Runtime.orchestrator
                                  patch_id)
                          in
                          match agent.Patch_agent.pr_number with
                          | Some pr_number when Branch.equal new_base main ->
                              set_pr_draft ~process_mgr
                                ~token:config.github_token
                                ~owner:config.github_owner
                                ~repo:config.github_repo ~pr_number ~draft:false
                          | _ -> ())
                      | Worktree.Conflict | Worktree.Error _ -> ()))
          | Orchestrator.Respond (patch_id, kind) ->
              (* Use pre-fire agent state for pending_comments — fire/respond
                 clears them as a postcondition. *)
              let pre_fire_agent =
                Base.List.Assoc.find pre_fire_agents patch_id
                  ~equal:Patch_id.equal
              in
              Some
                (fun () ->
                  with_busy_guard ~patch_id (fun () ->
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
                                Base.Option.value_map
                                  agent.Patch_agent.base_branch
                                  ~default:(Branch.to_string main)
                                  ~f:Branch.to_string
                              in
                              let source_agent =
                                Base.Option.value pre_fire_agent ~default:agent
                              in
                              let pending_comments =
                                Base.List.map
                                  source_agent.Patch_agent.pending_comments
                                  ~f:(fun (pc : Patch_agent.pending_comment) ->
                                    pc.Patch_agent.comment)
                              in
                              let pr_number = agent.Patch_agent.pr_number in
                              log_event runtime ~patch_id
                                (match kind with
                                | Operation_kind.Review_comments
                                | Operation_kind.Human ->
                                    Printf.sprintf "delivering %s (%d comments)"
                                      (Operation_kind.to_label kind)
                                      (Base.List.length pending_comments)
                                | Operation_kind.Ci | Operation_kind.Rebase
                                | Operation_kind.Merge_conflict ->
                                    Printf.sprintf "delivering %s"
                                      (Operation_kind.to_label kind));
                              let prompt =
                                match kind with
                                | Operation_kind.Ci -> (
                                    match
                                      Hashtbl.find_opt ci_checks_cache patch_id
                                    with
                                    | Some checks
                                      when not (Base.List.is_empty checks) ->
                                        Prompt.render_ci_failure_prompt
                                          ~project_name ?pr_number checks
                                    | _ ->
                                        Prompt.render_ci_failure_unknown_prompt
                                          ~project_name ?pr_number ())
                                | Operation_kind.Review_comments ->
                                    Prompt.render_review_prompt ~project_name
                                      ?pr_number pending_comments
                                | Operation_kind.Merge_conflict ->
                                    Prompt.render_merge_conflict_prompt
                                      ~project_name ?pr_number ~base_branch:base
                                      ()
                                | Operation_kind.Human ->
                                    Prompt.render_human_message_prompt
                                      ~project_name
                                      (Base.List.map pending_comments
                                         ~f:(fun (c : Comment.t) ->
                                           c.Comment.body))
                                | Operation_kind.Rebase ->
                                    (* Invariant: Rebase is never routed
                                       through Respond *)
                                    assert false
                              in
                              let on_pr_detected _pr_number = () in
                              run_claude_and_handle ~runtime ~process_mgr ~fs
                                ~project_name ~patch_id
                                ~repo_root:config.repo_root ~prompt ~agent
                                ~owner:config.github_owner
                                ~repo:config.github_repo ~on_pr_detected
                                ~transcripts)
                      in
                      match result with
                      | `Stale -> ()
                      | `Failed ->
                          let agent =
                            Runtime.read runtime (fun snap ->
                                Orchestrator.agent snap.Runtime.orchestrator
                                  patch_id)
                          in
                          if agent.Patch_agent.needs_intervention then
                            set_status ~level:Tui.Error
                              ~text:
                                (Printf.sprintf
                                   "Patch %s: session failed — human review \
                                    needed"
                                   (Patch_id.to_string patch_id))
                              ()
                      | `Ok ->
                          (* Mark pending comment IDs as addressed so the
                             poller doesn't re-enqueue them next cycle. *)
                          let comment_ids =
                            match pre_fire_agent with
                            | Some a ->
                                Base.List.map a.Patch_agent.pending_comments
                                  ~f:(fun (pc : Patch_agent.pending_comment) ->
                                    pc.Patch_agent.comment.Comment.id)
                            | None -> []
                          in
                          Runtime.update_orchestrator runtime (fun orch ->
                              let orch =
                                Base.List.fold comment_ids ~init:orch
                                  ~f:(fun orch cid ->
                                    Orchestrator.add_addressed_comment_id orch
                                      patch_id cid)
                              in
                              let orch =
                                if
                                  Operation_kind.equal kind
                                    Operation_kind.Merge_conflict
                                then (
                                  set_status ~level:Tui.Info
                                    ~text:
                                      (Printf.sprintf
                                         "Patch %s: conflict resolved, \
                                          rebasing…"
                                         (Patch_id.to_string patch_id))
                                    ~expires_at:(Unix.gettimeofday () +. 10.0)
                                    ();
                                  Orchestrator.clear_has_conflict orch patch_id)
                                else orch
                              in
                              Orchestrator.complete orch patch_id))))
    in
    (* Spawn action fibers without waiting for completion. Each fiber is
       guarded by with_busy_guard (ensures complete on exit) and
       with_claude_slot (semaphore backpressure). The runner loop continues
       immediately to pick up newly-queued actions from the poller. *)
    Base.List.iter action_fibers ~f:(fun f ->
        Eio.Fiber.fork_daemon ~sw (fun () ->
            f ();
            `Stop_daemon));
    Eio.Time.sleep clock 1.0;
    loop sw
  in
  Eio.Switch.run @@ fun sw -> loop sw

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

(** Resolve CLI args into a config ready to run.
    - [--gameplan] provided: parse it, persist config + gameplan source, derive
      project name.
    - [PROJECT] only: load stored config + gameplan. CLI flags override stored
      values. *)
let resolve_config ~project ~gameplan_path ~github_token ~main_branch
    ~poll_interval ~repo_root ~max_concurrency ~headless =
  match (project, gameplan_path) with
  | None, None ->
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
            design_decisions = "";
            patches = [];
          }
      in
      Project_store.save_config ~project_name ~github_token:token
        ~github_owner:owner ~github_repo:repo
        ~main_branch:(Branch.to_string main_branch)
        ~poll_interval ~repo_root ~max_concurrency;
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
          existing_snapshot )
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
          let token, owner, repo =
            resolve_github_credentials ~github_token ~repo_root
          in
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
                  let token_from_stored =
                    merge_cli_stored github_token
                      stored.Project_store.github_token
                  in
                  let token, owner, repo =
                    resolve_github_credentials ~github_token:token_from_stored
                      ~repo_root:stored.Project_store.repo_root
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
      let github =
        Github.create ~token:config.github_token ~owner:config.github_owner
          ~repo:config.github_repo
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
      let process_mgr = Eio.Stdenv.process_mgr env in
      let clock = Eio.Stdenv.clock env in
      let net = Eio.Stdenv.net env in
      let stdout = Eio.Stdenv.stdout env in
      let ci_checks_cache : (Patch_id.t, Ci_check.t list) Hashtbl.t =
        Hashtbl.create 16
      in
      let reconciliation_fiber () =
        let all_agents =
          Runtime.read runtime (fun snap ->
              Orchestrator.all_agents snap.Runtime.orchestrator)
        in
        let startup =
          Startup_reconciler.reconcile ~process_mgr ~token:config.github_token
            ~owner:config.github_owner ~repo:config.github_repo
            ~patches:gameplan.Gameplan.patches ~repo_root:config.repo_root
            ~agents:all_agents ()
        in
        let errored_ids =
          Base.List.map startup.Startup_reconciler.errors
            ~f:(fun (patch_id, err) ->
              log_event runtime ~patch_id
                (Printf.sprintf "startup discovery error: %s" err);
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
                | Some agent when agent.Patch_agent.has_pr ->
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
              "reset stale busy agent from crashed session";
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.reset_busy orch patch_id));
        Base.List.iter startup.recovered_worktrees ~f:(fun wr ->
            log_event runtime ~patch_id:wr.worktree_patch_id
              (Printf.sprintf "recovered worktree at %s" wr.worktree_path));
        Base.List.iter startup.worktree_errors ~f:(fun err ->
            log_event runtime (Printf.sprintf "startup worktree error: %s" err))
      in
      let transcripts =
        let t = Hashtbl.create 16 in
        Runtime.read runtime (fun snap ->
            Base.Hashtbl.iteri snap.Runtime.transcripts ~f:(fun ~key ~data ->
                Hashtbl.replace t key data));
        t
      in
      let common_fibers =
        [
          reconciliation_fiber;
          (fun () ->
            poller_fiber ~runtime ~clock ~net ~process_mgr ~github ~config
              ~pr_registry ~branch_of ~ci_checks_cache);
          (fun () ->
            persistence_fiber ~runtime ~clock ~project_name ~transcripts);
        ]
      in
      if config.headless then
        Eio.Fiber.all
          ((fun () -> headless_fiber ~runtime ~clock ~stdout)
          :: (fun () ->
            runner_fiber ~runtime ~env ~config ~project_name ~pr_registry
              ~ci_checks_cache ~transcripts ())
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
        let input_line = ref None in
        let completion_hint = ref None in
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
            let snap = Runtime.read runtime (fun s -> s) in
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
                     ~input_line ~completion_hint ~patches_start_row
                     ~patches_scroll_offset ~patches_visible_count)
                :: (fun () ->
                  input_fiber ~runtime ~list_selected ~detail_scroll
                    ~detail_follow ~timeline_scroll ~detail_scrolls ~view_mode
                    ~pr_registry ~project_name ~show_help ~status_msg
                    ~sorted_patch_ids ~input_line ~completion_hint
                    ~patches_start_row ~patches_scroll_offset
                    ~patches_visible_count)
                :: (fun () ->
                  runner_fiber ~runtime ~env ~config ~project_name ~pr_registry
                    ~ci_checks_cache ~transcripts ~status_msg ())
                :: common_fibers)
            with Quit_tui -> ())

let run ~project ~gameplan_path ~github_token ~main_branch ~poll_interval
    ~repo_root ~max_concurrency ~headless =
  match
    resolve_config ~project ~gameplan_path ~github_token ~main_branch
      ~poll_interval ~repo_root ~max_concurrency ~headless
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

let repo_arg =
  let open Cmdliner in
  Arg.(
    value & opt string "."
    & info [ "repo" ] ~docv:"PATH"
        ~doc:"Path to the git repository (default: current directory).")

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
  let run_cmd project gameplan_path github_token main_branch poll_interval
      repo_root max_concurrency headless =
    run ~project ~gameplan_path ~github_token
      ~main_branch:(Branch.of_string (Base.String.strip main_branch))
      ~poll_interval ~repo_root ~max_concurrency ~headless
  in
  let term =
    Term.(
      const run_cmd $ project_arg $ gameplan_path_arg $ github_token_arg
      $ main_branch_arg $ poll_interval_arg $ repo_arg $ max_concurrency_arg
      $ headless_arg)
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
