open Onton
open Onton.Types

(** {1 Configuration} *)

type config = {
  project : string option;
  backend : string;
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

(** Infer GitHub owner/repo from [git remote get-url origin] in [repo_root].
    Parses both HTTPS and SSH remote URLs. Uses argv (no shell). *)
let infer_owner_repo ~repo_root =
  let buf = Buffer.create 128 in
  try
    let ic =
      Unix.open_process_args_in "git"
        [| "git"; "-C"; repo_root; "remote"; "get-url"; "origin" |]
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
    Uses argv (no shell). *)
let infer_github_token () =
  match Stdlib.Sys.getenv_opt "GITHUB_TOKEN" with
  | Some t when not (Base.String.is_empty (Base.String.strip t)) ->
      Base.String.strip t
  | _ -> (
      let buf = Buffer.create 128 in
      try
        let ic = Unix.open_process_args_in "gh" [| "gh"; "auth"; "token" |] in
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

(** Detect the default branch of a git repository. Tries: 1.
    [git symbolic-ref refs/remotes/origin/HEAD] (fast, local) 2.
    [git rev-parse --verify refs/heads/main] → "main" 3.
    [git rev-parse --verify refs/heads/master] → "master" 4. Fallback: "main" *)
let infer_default_branch ~repo_root =
  let run_git cmd =
    let buf = Buffer.create 128 in
    try
      let ic =
        Unix.open_process_in
          (Printf.sprintf "git -C %s %s 2>/dev/null" (Filename.quote repo_root)
             cmd)
      in
      (try
         while true do
           Buffer.add_char buf (input_char ic)
         done
       with End_of_file -> ());
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> Some (Base.String.strip (Buffer.contents buf))
      | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> None
    with _ -> None
  in
  match run_git "symbolic-ref refs/remotes/origin/HEAD" with
  | Some ref_path ->
      let prefix = "refs/remotes/origin/" in
      if Base.String.is_prefix ref_path ~prefix then
        Base.String.chop_prefix_exn ref_path ~prefix
      else ref_path
  | None -> (
      match run_git "rev-parse --verify refs/heads/main" with
      | Some _ -> "main"
      | None -> (
          match run_git "rev-parse --verify refs/heads/master" with
          | Some _ -> "master"
          | None -> "main"))

let known_backends = [ "claude"; "codex"; "opencode"; "pi"; "gemini" ]

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
    patch's branch name after Claude completes work. *)

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
        | Patch_controller.Set_pr_description { pr_number; _ } ->
            Printf.sprintf "set_pr_description (PR #%d)"
              (Pr_number.to_int pr_number)
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
          | Patch_controller.Set_pr_description
              { patch_id = _; pr_number; body } ->
              Github.update_pr_body ~net github ~pr_number ~body
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

(** Pluralize a count for inline rendering: [pluralize 1 "comment"] →
    ["1 comment"], [pluralize 2 "comment"] → ["2 comments"]. Pass [~plural] when
    the plural is irregular. *)
let pluralize ?plural n singular =
  let many = match plural with Some p -> p | None -> singular ^ "s" in
  Printf.sprintf "%d %s" n (if n = 1 then singular else many)

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

    The fallback chain is: resume existing session → fresh session → give up.
    - [Fresh_available] with [llm_session_id = Some id]: use [--resume <id>] to
      target the specific session.
    - [Fresh_available] without a stored session_id, or [Tried_fresh]: start
      fresh (no --resume).
    - [Given_up]: the agent has exhausted its fallback chain — return
      [`Give_up]. *)
let session_mode (agent : Patch_agent.t) :
    [ `Resume of string | `Fresh | `Give_up ] =
  match agent.Patch_agent.session_fallback with
  | Patch_agent.Given_up -> `Give_up
  | Patch_agent.Tried_fresh -> `Fresh
  | Patch_agent.Fresh_available -> (
      match agent.Patch_agent.llm_session_id with
      | Some id -> `Resume id
      | None -> `Fresh)

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

(** Resolve the worktree path for a patch. Checks the stored path first, then
    searches git worktrees by branch, then falls back to the default computed
    path. Persists the result so subsequent calls are instant. *)
let resolve_worktree_path ~process_mgr ~repo_root ~project_name ~patch_id
    ~(agent : Patch_agent.t) ?branch () =
  match agent.Patch_agent.worktree_path with
  | Some p -> p
  | None ->
      let search_branch =
        match branch with Some b -> b | None -> agent.Patch_agent.branch
      in
      let found =
        Worktree.find_for_branch ~process_mgr ~repo_root search_branch
      in
      let path =
        match found with
        | Some p -> p
        | None -> Worktree.worktree_dir ~project_name ~patch_id
      in
      path

(** Ensure a worktree exists for the given patch. Resolves the path, discovers
    existing worktrees for the branch, creates one if needed, and persists the
    path. Returns [Some path] on success or [None] if the branch is unknown and
    no worktree can be created yet. *)
let ensure_worktree ~runtime ~process_mgr ~fs ~repo_root ~project_name ~patch_id
    ~(agent : Patch_agent.t) ~(user_config : User_config.t) ~worktree_mutex
    ?branch ?base_ref () =
  let path =
    resolve_worktree_path ~process_mgr ~repo_root ~project_name ~patch_id ~agent
      ?branch ()
  in
  if Stdlib.Sys.file_exists path then (
    Runtime.update_orchestrator runtime (fun orch ->
        Orchestrator.set_worktree_path orch patch_id path);
    Some path)
  else
    let br =
      match branch with Some b -> b | None -> agent.Patch_agent.branch
    in
    match Worktree.find_for_branch ~process_mgr ~repo_root br with
    | Some existing ->
        log_event runtime ~patch_id
          (Printf.sprintf "Found existing worktree for branch at %s" existing);
        Runtime.update_orchestrator runtime (fun orch ->
            Orchestrator.set_worktree_path orch patch_id existing);
        Some existing
    | None ->
        if Worktree.is_checked_out_in_repo_root ~process_mgr ~repo_root br then (
          let main_root = Worktree.resolve_main_root ~process_mgr ~repo_root in
          log_event runtime ~patch_id
            (Printf.sprintf
               "Cannot create worktree — branch %s is checked out in the main \
                working tree (%s). Switch the main tree to another branch \
                (e.g. `git -C %s checkout <default-branch>`) and try again."
               (Branch.to_string br) main_root main_root);
          None)
        else
          let base =
            match base_ref with
            | Some b -> b
            | None -> (
                match agent.Patch_agent.base_branch with
                | Some b -> Branch.to_string b
                | None -> "HEAD")
          in
          log_event runtime ~patch_id
            (Printf.sprintf "Creating worktree at %s" path);
          (match
             Eio.Mutex.use_rw ~protect:true worktree_mutex (fun () ->
                 ignore
                   (Worktree.create ~process_mgr ~repo_root ~project_name
                      ~patch_id ~branch:br ~base_ref:base))
           with
          | () -> ()
          | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
          | exception exn ->
              log_event runtime ~patch_id
                (Printf.sprintf "Worktree creation failed — %s"
                   (Printexc.to_string exn)));
          if Stdlib.Sys.file_exists path then (
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.set_worktree_path orch patch_id path);
            (match user_config.User_config.on_worktree_create with
            | Some script -> (
                let env =
                  [
                    ("ONTON_WORKTREE_PATH", path);
                    ("ONTON_PATCH_ID", Patch_id.to_string patch_id);
                    ("ONTON_BRANCH", Branch.to_string br);
                  ]
                in
                let cwd = Eio.Path.(fs / path) in
                match User_config.run_hook ~process_mgr ~script ~cwd ~env with
                | Ok () ->
                    log_event runtime ~patch_id "Ran on_worktree_create hook"
                | Error msg ->
                    log_event runtime ~patch_id
                      (Printf.sprintf "Hook on_worktree_create failed — %s" msg)
                )
            | None -> ());
            Some path)
          else (
            log_event runtime ~patch_id
              (Printf.sprintf "Worktree still missing at %s" path);
            None)

let truncate s n = if String.length s <= n then s else String.sub s 0 n ^ "..."

let run_claude_and_handle ~runtime ~process_mgr ~fs ~project_name ~patch_id
    ~repo_root ~prompt ~(agent : Patch_agent.t) ~owner ~repo ~on_pr_detected
    ~transcripts ~user_config ~worktree_mutex ~backend ~event_log =
  match session_mode agent with
  | `Give_up ->
      log_event runtime ~patch_id
        "Session fallback exhausted — continue and fresh both failed, needs \
         intervention";
      Runtime.update_orchestrator runtime (fun orch ->
          Orchestrator.apply_session_result orch patch_id
            Orchestrator.Session_give_up);
      `Failed
  | (`Resume _ | `Fresh) as mode -> (
      let resume_session, is_fresh =
        match mode with `Resume id -> (Some id, false) | `Fresh -> (None, true)
      in
      match
        ensure_worktree ~runtime ~process_mgr ~fs ~repo_root ~project_name
          ~patch_id ~agent ~user_config ~worktree_mutex ()
      with
      | None ->
          Runtime.update_orchestrator runtime (fun orch ->
              Orchestrator.apply_session_result orch patch_id
                Orchestrator.Session_worktree_missing);
          `Failed
      | Some worktree_path ->
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
                 "\n---\n**[%02d:%02d:%02d] Delivered to %s%s:**\n\n"
                 tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
                 backend.Llm_backend.name
                 (match resume_session with
                 | Some id ->
                     Printf.sprintf " (--resume %s)"
                       (String.sub id 0 (min 8 (String.length id)))
                 | None -> ""));
            Buffer.add_string buf prompt;
            Buffer.add_string buf
              (Printf.sprintf "\n\n---\n**%s response:**\n\n"
                 backend.Llm_backend.name);
            Hashtbl.replace transcripts patch_id (Buffer.contents buf);
            buf
          in
          let error_buf = Buffer.create 256 in
          let tool_count = ref 0 in
          let pr_found = ref false in
          let needle_len =
            String.length (Printf.sprintf "github.com/%s/%s/pull/" owner repo)
          in
          let last_sync = ref (Unix.gettimeofday ()) in
          let sync_transcript () =
            Hashtbl.replace transcripts patch_id (Buffer.contents text_buf)
          in
          let maybe_sync_transcript () =
            let now = Unix.gettimeofday () in
            if now -. !last_sync >= 0.2 then (
              last_sync := now;
              sync_transcript ())
          in
          let captured_session_id = ref None in
          let on_event (event : Types.Stream_event.t) =
            match event with
            | Types.Stream_event.Text_delta text -> (
                let prev_len = Buffer.length text_buf in
                Buffer.add_string text_buf text;
                maybe_sync_transcript ();
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
            | Types.Stream_event.Tool_use { name; input } ->
                tool_count := !tool_count + 1;
                let summary =
                  try
                    let json = Yojson.Safe.from_string input in
                    let field key =
                      Yojson.Safe.Util.(member key json |> to_string_option)
                    in
                    let s =
                      match name with
                      | "Bash" -> field "command"
                      | "Read" | "Write" -> field "file_path"
                      | "Edit" -> field "file_path"
                      | "Glob" -> field "pattern"
                      | "Grep" -> field "pattern"
                      | _ -> None
                    in
                    match s with Some v -> truncate v 80 | None -> ""
                  with _ -> ""
                in
                let detail =
                  if summary <> "" then Printf.sprintf " %s" summary else ""
                in
                let sep =
                  let len = Buffer.length text_buf in
                  if len = 0 then ""
                  else if
                    len >= 2
                    && Char.equal (Buffer.nth text_buf (len - 1)) '\n'
                    && Char.equal (Buffer.nth text_buf (len - 2)) '\n'
                  then ""
                  else if Char.equal (Buffer.nth text_buf (len - 1)) '\n' then
                    "\n"
                  else "\n\n"
                in
                Buffer.add_string text_buf
                  (Printf.sprintf "%s[tool: %s]%s\n" sep name detail);
                sync_transcript ();
                log_stream_entry runtime ~patch_id
                  (Activity_log.Stream_entry.Tool_use (name, summary))
            | Types.Stream_event.Final_result { stop_reason; _ } ->
                sync_transcript ();
                let reason = Types.Stop_reason.to_display stop_reason in
                log_stream_entry runtime ~patch_id
                  (Activity_log.Stream_entry.Finished reason)
            | Types.Stream_event.Error msg ->
                if Buffer.length error_buf > 0 then
                  Buffer.add_char error_buf '\n';
                Buffer.add_string error_buf msg;
                log_stream_entry runtime ~patch_id
                  (Activity_log.Stream_entry.Stream_error msg)
            | Types.Stream_event.Session_init { session_id } ->
                captured_session_id := Some session_id
          in
          let result =
            try
              Ok
                (backend.Llm_backend.run_streaming ~cwd ~patch_id ~prompt
                   ~resume_session ~on_event)
            with exn -> Error (Printexc.to_string exn)
          in
          let open Run_classification in
          let outcome =
            Result.map
              (fun (r : Llm_backend.result) ->
                {
                  exit_code = r.Llm_backend.exit_code;
                  got_events = r.Llm_backend.got_events;
                  stderr = r.Llm_backend.stderr;
                  stream_errors = String.trim (Buffer.contents error_buf);
                  timed_out = r.Llm_backend.timed_out;
                })
              result
          in
          let session_result, user_result =
            match
              classify ~is_resume:(Option.is_some resume_session) outcome
            with
            | Process_error msg ->
                log_event runtime ~patch_id
                  (Printf.sprintf "Process error from %s — %s"
                     backend.Llm_backend.name msg);
                (Orchestrator.Session_process_error { is_fresh }, `Failed)
            | No_session_to_resume ->
                log_event runtime ~patch_id
                  "Resume produced no events — no session to resume, retrying \
                   fresh";
                (Orchestrator.Session_no_resume, `Failed)
            | Timed_out ->
                log_event runtime ~patch_id
                  (Printf.sprintf "Session timed out (%s) — marking failed"
                     backend.Llm_backend.name);
                (Orchestrator.Session_failed { is_fresh }, `Failed)
            | Success { stream_errors } ->
                if String.length stream_errors > 0 then
                  log_event runtime ~patch_id
                    (Printf.sprintf
                       "Session exited 0 (%s) with stream errors — %s"
                       backend.Llm_backend.name
                       (truncate stream_errors 500));
                let text_len = Buffer.length text_buf in
                let tools = !tool_count in
                if tools = 0 && text_len < 200 then
                  log_event runtime ~patch_id
                    (Printf.sprintf
                       "Session exited 0 (%s) with no tool use and %s of text \
                        — %s"
                       backend.Llm_backend.name
                       (pluralize text_len "char")
                       (truncate (String.trim (Buffer.contents text_buf)) 200));
                (Orchestrator.Session_ok, `Ok)
            | Session_failed { exit_code; detail } ->
                log_event runtime ~patch_id
                  (Printf.sprintf "Session failed (%s) — exit %d: %s"
                     backend.Llm_backend.name exit_code detail);
                (Orchestrator.Session_failed { is_fresh }, `Failed)
          in
          (* Supervisor-owned push: agent commits locally; we push every
             local commit to the remote at session end. force_push_with_lease
             is idempotent (Push_up_to_date when nothing new), and lease-safe
             against concurrent remote updates. Runs regardless of the LLM's
             session result so commits made before a partial failure still
             reach the remote. *)
          let branch = agent.Patch_agent.branch in
          let push_outcome =
            Worktree.force_push_with_lease ~process_mgr ~path:worktree_path
              ~branch
          in
          (match push_outcome with
          | Worktree.Push_ok ->
              log_event runtime ~patch_id "runner: pushed after session"
          | Worktree.Push_up_to_date ->
              log_event runtime ~patch_id
                "runner: push up-to-date after session (no new commits)"
          | Worktree.Push_rejected ->
              log_event runtime ~patch_id
                "runner: push rejected after session (lease)"
          | Worktree.Push_error msg ->
              log_event runtime ~patch_id
                (Printf.sprintf "runner: push error after session: %s" msg));
          (* Combine LLM session outcome with push outcome into a single
             session_result. A push failure when the LLM was otherwise
             healthy is recorded as Session_push_failed so the orchestrator
             treats it as a retryable failure without poisoning
             session_fallback. A pre-existing LLM failure takes precedence —
             the push outcome doesn't change it. *)
          let final_session_result, final_user_result =
            match session_result with
            | Orchestrator.Session_ok -> (
                match push_outcome with
                | Worktree.Push_ok | Worktree.Push_up_to_date ->
                    (session_result, user_result)
                | Worktree.Push_rejected | Worktree.Push_error _ ->
                    (Orchestrator.Session_push_failed, `Failed))
            | Orchestrator.Session_process_error _
            | Orchestrator.Session_no_resume | Orchestrator.Session_failed _
            | Orchestrator.Session_give_up
            | Orchestrator.Session_worktree_missing
            | Orchestrator.Session_push_failed ->
                (* Pre-existing LLM/setup failure — push outcome doesn't
                   change anything. *)
                (session_result, user_result)
          in
          let agent_before, agent_after =
            Runtime.update_orchestrator_returning runtime (fun orch ->
                let agent_before = Orchestrator.agent orch patch_id in
                let orch =
                  Orchestrator.apply_session_result orch patch_id
                    final_session_result
                in
                (* Store the captured session_id for future --resume calls *)
                let orch =
                  match !captured_session_id with
                  | Some _ ->
                      Orchestrator.set_llm_session_id orch patch_id
                        !captured_session_id
                  | None -> orch
                in
                let agent_after = Orchestrator.agent orch patch_id in
                (orch, (agent_before, agent_after)))
          in
          Event_log.log_complete event_log ~patch_id
            ~result:final_session_result ~agent_before ~agent_after;
          final_user_result)

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
    ~patches_scroll_offset ~patches_visible_count =
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
        ~intervention_reasons ()
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
        ~show_help:!show_help
        ~show_manage:
          (Tui_input.equal_input_mode !input_mode Tui_input.Manage_patch)
        ~transcript ?status_msg:!status_msg ?prompt_line:!prompt_line views
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
    ~patches_visible_count ~owner ~repo =
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
    match Term.Key.read () with
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
              match !view_mode with
              | Tui.Detail_view patch_id ->
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
              | Tui.List_view | Tui.Timeline_view -> ())
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
                            ~gameplan:snap.Runtime.gameplan ~activity:[] ())
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
                                    Orchestrator.add_agent orch ~patch_id
                                      ~branch ~pr_number);
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
                    Base.Int.max (-1)
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
                 | Tui.List_view | Tui.Timeline_view -> false ->
              (match !view_mode with
              | Tui.Detail_view patch_id -> (
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
              | Tui.List_view | Tui.Timeline_view -> ());
              loop ()
          | Term.Key.Char 'm'
            when match !view_mode with
                 | Tui.Detail_view _ -> true
                 | Tui.List_view | Tui.Timeline_view -> false ->
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
let poller_fiber ~runtime ~clock ~net ~process_mgr ~github ~config ~project_name
    ~pr_registry ~branch_of ~event_log =
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

(** Runner fiber — executes orchestrator actions by spawning Claude processes
    concurrently. *)
let runner_fiber ~runtime ~env ~config ~project_name ~pr_registry ~transcripts
    ~github ~net ~event_log ?status_msg () =
  let main = config.main_branch in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let fs = Eio.Stdenv.fs env in
  let session_timeout =
    1800.0
    (* 30 minutes *)
  in
  let backend =
    match config.backend with
    | "claude" ->
        Claude_backend.create ~process_mgr ~clock ~timeout:session_timeout
    | "codex" ->
        Codex_backend.create ~process_mgr ~clock ~timeout:session_timeout
    | "opencode" ->
        Opencode_backend.create ~process_mgr ~clock ~timeout:session_timeout
    | "pi" -> Pi_backend.create ~process_mgr ~clock ~timeout:session_timeout
    | "gemini" ->
        Gemini_backend.create ~process_mgr ~clock ~timeout:session_timeout
    | other ->
        invalid_arg
          (Printf.sprintf "Unsupported --backend=%S (expected %s)" other
             (String.concat ", " known_backends))
  in
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
  let worktree_mutex = Eio.Mutex.create () in
  let with_busy_guard ~patch_id f =
    Fun.protect
      ~finally:(fun () ->
        (* Check-and-complete must be atomic to avoid racing with another
           fiber that completes the same patch between read and update. *)
        let was_busy =
          Runtime.update_orchestrator_returning runtime (fun orch ->
              match Orchestrator.find_agent orch patch_id with
              | None -> (orch, false)
              | Some agent ->
                  if agent.Patch_agent.busy then
                    (Orchestrator.complete orch patch_id, true)
                  else (orch, false))
        in
        if was_busy then
          log_event runtime ~patch_id
            "Forced complete — runner fiber exited with busy=true")
      (fun () ->
        try f () with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn ->
            log_event runtime ~patch_id
              (Printf.sprintf "Unexpected action exception — %s"
                 (Printexc.to_string exn));
            mark_session_failed runtime patch_id)
  in
  let rec loop sw =
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
                                  || Patch_agent.needs_intervention agent
                                  || agent.Patch_agent.branch_blocked
                                  || not agent.Patch_agent.busy
                                then (
                                  log_event runtime ~patch_id
                                    "Skipping action — became stale during \
                                     semaphore wait";
                                  `Stale)
                                else
                                  match
                                    ensure_worktree ~runtime ~process_mgr ~fs
                                      ~repo_root:config.repo_root ~project_name
                                      ~patch_id ~agent
                                      ~user_config:config.user_config
                                      ~worktree_mutex ~branch:patch.Patch.branch
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
                                      let prompt =
                                        Prompt.render_patch_prompt ~project_name
                                          ?pr_number:agent.Patch_agent.pr_number
                                          patch gameplan
                                          ~base_branch:
                                            (Branch.to_string base_branch)
                                      in
                                      (* PR detection from stream text is a hint
                                     only — always confirmed via the GitHub
                                     REST API (Github.list_prs) after Claude
                                     finishes *)
                                      let on_pr_detected _pr_number = () in
                                      run_claude_and_handle ~runtime
                                        ~process_mgr ~fs ~project_name ~patch_id
                                        ~repo_root:config.repo_root ~prompt
                                        ~agent ~owner:config.github_owner
                                        ~repo:config.github_repo ~on_pr_detected
                                        ~transcripts
                                        ~user_config:config.user_config
                                        ~worktree_mutex ~backend ~event_log)
                          in
                          let start_outcome =
                            match result with
                            | `Stale -> Orchestrator.Start_stale
                            | `Failed -> Orchestrator.Start_failed
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
                                 gameplan-derived title and body. *)
                              let pr_title =
                                Printf.sprintf "[%s] Patch %s: %s" project_name
                                  (Patch_id.to_string patch.Patch.id)
                                  patch.Patch.title
                              in
                              let pr_body =
                                Prompt.render_pr_description ~project_name patch
                                  gameplan
                              in
                              (match
                                 Github.create_pull_request ~net github
                                   ~title:pr_title ~head:patch.Patch.branch
                                   ~base:base_branch ~body:pr_body ~draft:true
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
                                  | Github.Http_error { status = 422; _ } -> (
                                      (* PR already exists — discover it rather
                                         than treating this as failure *)
                                      match
                                        Github.list_prs ~net github
                                          ~branch:patch.Patch.branch
                                          ~base:(Some base_branch) ~state:`Open
                                          ()
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
                                            "PR creation failed (422) and \
                                             discovery found no open PRs";
                                          Runtime.update_orchestrator runtime
                                            (fun orch ->
                                              Orchestrator
                                              .on_pr_discovery_failure orch
                                                patch_id)
                                      | Error disc_err ->
                                          log_event runtime ~patch_id
                                            (Printf.sprintf
                                               "PR creation failed (422) and \
                                                discovery also failed — %s"
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
                      (* Fetch fresh remote refs before rebasing.
                         Fail closed: if fetch fails, don't rebase against
                         stale refs. *)
                      let rebase_result =
                        match
                          Worktree.fetch_origin ~process_mgr ~path:wt_path
                        with
                        | Result.Ok () ->
                            let remote_target =
                              Types.Branch.of_string
                                (Printf.sprintf "origin/%s"
                                   (Branch.to_string new_base))
                            in
                            Worktree.rebase_onto ~process_mgr ~path:wt_path
                              ~target:remote_target
                        | Result.Error msg ->
                            log_event runtime ~patch_id
                              (Printf.sprintf "Fetch failed before rebase — %s"
                                 msg);
                            Worktree.Error
                              (Printf.sprintf "fetch before rebase failed: %s"
                                 msg)
                      in
                      (match rebase_result with
                      | Worktree.Ok ->
                          log_event runtime ~patch_id
                            (Printf.sprintf "Rebased onto %s"
                               (Branch.to_string new_base))
                      | Worktree.Noop ->
                          log_event runtime ~patch_id
                            "Rebase noop — already up-to-date"
                      | Worktree.Conflict ->
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
                                ~path:wt_path ~branch
                            in
                            (match result with
                            | Worktree.Push_ok ->
                                log_event runtime ~patch_id
                                  "Force-pushed after rebase"
                            | Worktree.Push_up_to_date ->
                                log_event runtime ~patch_id
                                  "Push noop after rebase — already up-to-date"
                            | Worktree.Push_rejected ->
                                log_event runtime ~patch_id
                                  "Force-push rejected — lease violated"
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
                  (* For Review_comments, fetch fresh unaddressed comments
                     from GitHub before acquiring a Claude slot to avoid
                     blocking concurrency on GitHub API I/O. *)
                  let is_review =
                    Operation_kind.equal kind Operation_kind.Review_comments
                  in
                  let prefetched_comments =
                    if is_review then
                      match
                        Runtime.read runtime (fun snap ->
                            (Orchestrator.agent snap.Runtime.orchestrator
                               patch_id)
                              .Patch_agent.pr_number)
                      with
                      | Some pr_num -> (
                          log_event runtime ~patch_id
                            "Fetching fresh review comments from GitHub";
                          match Github.pr_state ~net github pr_num with
                          | Ok pr_state -> pr_state.Pr_state.comments
                          | Error _err ->
                              log_event runtime ~patch_id
                                "Failed to fetch fresh review comments";
                              [])
                      | None -> []
                    else []
                  in
                  with_busy_guard ~patch_id (fun () ->
                      let result =
                        with_claude_slot (fun () ->
                            let agent =
                              Runtime.read runtime (fun snap ->
                                  Orchestrator.agent snap.Runtime.orchestrator
                                    patch_id)
                            in
                            let delivery =
                              Patch_decision.respond_delivery ~agent ~kind
                                ~pre_fire_agent ~prefetched_comments
                                ~main_branch:(Branch.to_string main)
                            in
                            let render_base_changed_prefix base_change =
                              match base_change with
                              | Some bc ->
                                  log_event runtime ~patch_id
                                    (Printf.sprintf
                                       "Base branch changed from %s to %s — \
                                        notifying agent"
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
                                  (Printf.sprintf
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
                                let wt_path =
                                  match agent.Patch_agent.worktree_path with
                                  | Some p -> p
                                  | None ->
                                      Worktree.worktree_dir ~project_name
                                        ~patch_id
                                in
                                (* Helper: capture git context and deliver
                                   an enriched prompt to the agent. *)
                                let deliver_to_agent () =
                                  let pr_number = agent.Patch_agent.pr_number in
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
                                    ~rebase_in_progress:rebase_still_in_progress
                                    ~git_status ~git_diff;
                                  let patch =
                                    Base.List.find gameplan.Gameplan.patches
                                      ~f:(fun (p : Patch.t) ->
                                        Patch_id.equal p.Patch.id patch_id)
                                  in
                                  let prompt =
                                    let raw =
                                      Prompt.render_merge_conflict_prompt
                                        ~project_name ?pr_number ?patch
                                        ~gameplan ~base_branch:base ~git_status
                                        ~git_diff ()
                                    in
                                    if String.equal base_changed_prefix "" then
                                      raw
                                    else base_changed_prefix ^ "\n" ^ raw
                                  in
                                  let on_pr_detected _pr_number = () in
                                  let result =
                                    run_claude_and_handle ~runtime ~process_mgr
                                      ~fs ~project_name ~patch_id
                                      ~repo_root:config.repo_root ~prompt ~agent
                                      ~owner:config.github_owner
                                      ~repo:config.github_repo ~on_pr_detected
                                      ~transcripts
                                      ~user_config:config.user_config
                                      ~worktree_mutex ~backend ~event_log
                                  in
                                  (match result with
                                  | `Ok
                                    when not
                                           (String.equal base_changed_prefix "")
                                    ->
                                      Runtime.update_orchestrator runtime
                                        (fun orch ->
                                          Orchestrator.set_notified_base_branch
                                            orch patch_id
                                            (Branch.of_string base))
                                  | _ -> ());
                                  result
                                in
                                if
                                  Worktree.rebase_in_progress ~process_mgr
                                    ~path:wt_path
                                then (
                                  log_event runtime ~patch_id
                                    "Delivering merge-conflict — rebase \
                                     already in progress";
                                  deliver_to_agent ())
                                else
                                  (* Fetch fresh remote refs so rebase_onto
                                     sees the latest origin/<base>, not a
                                     stale local tracking ref. Fail closed:
                                     if fetch fails, don't rebase against
                                     stale refs. *)
                                  let rebase_result =
                                    match
                                      Worktree.fetch_origin ~process_mgr
                                        ~path:wt_path
                                    with
                                    | Result.Ok () ->
                                        let target =
                                          Types.Branch.of_string
                                            (Printf.sprintf "origin/%s" base)
                                        in
                                        Worktree.rebase_onto ~process_mgr
                                          ~path:wt_path ~target
                                    | Result.Error msg ->
                                        log_event runtime ~patch_id
                                          (Printf.sprintf
                                             "Fetch failed before \
                                              merge-conflict rebase — %s"
                                             msg);
                                        Worktree.Error
                                          (Printf.sprintf
                                             "fetch before rebase failed: %s"
                                             msg)
                                  in
                                  (match rebase_result with
                                  | Worktree.Ok ->
                                      log_event runtime ~patch_id
                                        (Printf.sprintf
                                           "Conflict rebase onto %s succeeded"
                                           base)
                                  | Worktree.Noop ->
                                      log_event runtime ~patch_id
                                        "Conflict rebase noop — local already \
                                         up-to-date, will push"
                                  | Worktree.Conflict ->
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
                                    ~patch_id ~result:rebase_result ~decision
                                    ~agent_before ~agent_after;
                                  let push_outcome =
                                    Base.List.find_map effects
                                      ~f:(fun Orchestrator.Push_branch ->
                                        let branch = agent.Patch_agent.branch in
                                        let result =
                                          Worktree.force_push_with_lease
                                            ~process_mgr ~path:wt_path ~branch
                                        in
                                        (match result with
                                        | Worktree.Push_ok ->
                                            log_event runtime ~patch_id
                                              "Force-pushed to resolve conflict"
                                        | Worktree.Push_up_to_date ->
                                            log_event runtime ~patch_id
                                              "Conflict push noop — already \
                                               up-to-date"
                                        | Worktree.Push_rejected ->
                                            log_event runtime ~patch_id
                                              "Conflict force-push rejected — \
                                               lease violated"
                                        | Worktree.Push_error msg ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "Conflict force-push failed — \
                                                  %s"
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
                                        "Re-enqueued conflict resolution after \
                                         push failure";
                                      `Retry_push
                                  | Orchestrator.Conflict_needs_agent ->
                                      deliver_to_agent ()
                                  | Orchestrator.Conflict_give_up -> `Failed)
                            | Patch_decision.Deliver
                                {
                                  payload =
                                    ( Patch_decision.Human_payload _
                                    | Patch_decision.Ci_payload _
                                    | Patch_decision.Review_payload _
                                    | Patch_decision.Pr_body_payload
                                    | Patch_decision
                                      .Implementation_notes_payload ) as payload;
                                  base_change;
                                } ->
                                let pr_number = agent.Patch_agent.pr_number in
                                let base_changed_prefix =
                                  render_base_changed_prefix base_change
                                in
                                log_event runtime ~patch_id
                                  (match payload with
                                  | Patch_decision.Review_payload { comments }
                                    ->
                                      Printf.sprintf "Delivering %s (%s)"
                                        (Operation_kind.to_label kind)
                                        (pluralize
                                           (Base.List.length comments)
                                           "comment")
                                  | Patch_decision.Human_payload { messages } ->
                                      Printf.sprintf "Delivering %s (%s)"
                                        (Operation_kind.to_label kind)
                                        (pluralize
                                           (Base.List.length messages)
                                           "message")
                                  | Patch_decision.Ci_payload _
                                  | Patch_decision.Pr_body_payload
                                  | Patch_decision.Implementation_notes_payload
                                  | Patch_decision.Merge_conflict_payload ->
                                      Printf.sprintf "Delivering %s"
                                        (Operation_kind.to_label kind));
                                let prompt =
                                  match payload with
                                  | Patch_decision.Ci_payload { failed_checks }
                                    ->
                                      if Base.List.is_empty failed_checks then
                                        Prompt.render_ci_failure_unknown_prompt
                                          ~project_name ?pr_number ()
                                      else
                                        Prompt.render_ci_failure_prompt
                                          ~project_name ?pr_number failed_checks
                                  | Patch_decision.Review_payload { comments }
                                    ->
                                      Prompt.render_review_prompt ~project_name
                                        ?pr_number comments
                                  | Patch_decision.Human_payload { messages } ->
                                      Prompt.render_human_message_prompt
                                        ~project_name messages
                                  | Patch_decision.Pr_body_payload ->
                                      let patch =
                                        Base.List.find_exn
                                          gameplan.Gameplan.patches
                                          ~f:(fun (p : Patch.t) ->
                                            Patch_id.equal p.Patch.id patch_id)
                                      in
                                      let pr_body =
                                        Prompt.render_pr_description
                                          ~project_name patch gameplan
                                      in
                                      let artifact_path =
                                        Project_store.pr_body_artifact_path
                                          ~project_name ~patch_id
                                      in
                                      Project_store.ensure_dir
                                        (Stdlib.Filename.dirname artifact_path);
                                      Prompt.render_pr_body_prompt ~project_name
                                        ~pr_number:
                                          (Base.Option.value_exn pr_number)
                                        ~pr_body ~artifact_path
                                  | Patch_decision.Implementation_notes_payload
                                    ->
                                      let patch =
                                        Base.List.find_exn
                                          gameplan.Gameplan.patches
                                          ~f:(fun (p : Patch.t) ->
                                            Patch_id.equal p.Patch.id patch_id)
                                      in
                                      let pr_body =
                                        Prompt.render_pr_description
                                          ~project_name patch gameplan
                                      in
                                      Prompt.render_implementation_notes_prompt
                                        ~project_name
                                        ~pr_number:
                                          (Base.Option.value_exn pr_number)
                                        ~pr_body
                                  | Patch_decision.Merge_conflict_payload ->
                                      (* Invariant: Merge_conflict is handled
                                         in the dedicated match arm above *)
                                      assert false
                                in
                                let prompt =
                                  if String.equal base_changed_prefix "" then
                                    prompt
                                  else base_changed_prefix ^ "\n" ^ prompt
                                in
                                let on_pr_detected _pr_number = () in
                                let base =
                                  Base.Option.value_map
                                    agent.Patch_agent.base_branch
                                    ~default:(Branch.to_string main)
                                    ~f:Branch.to_string
                                in
                                let result =
                                  run_claude_and_handle ~runtime ~process_mgr
                                    ~fs ~project_name ~patch_id
                                    ~repo_root:config.repo_root ~prompt ~agent
                                    ~owner:config.github_owner
                                    ~repo:config.github_repo ~on_pr_detected
                                    ~transcripts ~user_config:config.user_config
                                    ~worktree_mutex ~backend ~event_log
                                in
                                (match result with
                                | `Ok when Base.Option.is_some base_change ->
                                    Runtime.update_orchestrator runtime
                                      (fun orch ->
                                        Orchestrator.set_notified_base_branch
                                          orch patch_id (Branch.of_string base))
                                | _ -> ());
                                (* Pr_body: read the artifact the agent wrote
                                   and PATCH the PR body. Falls back silently
                                   to keep the gameplan-derived body if the
                                   artifact is missing or unreadable. The
                                   pr_body_delivered flag flips to true via
                                   apply_respond_outcome on Respond_ok
                                   regardless of whether the PATCH happened —
                                   that's the documented fallback. *)
                                (match payload with
                                | Patch_decision.Pr_body_payload
                                  when match result with
                                       | `Ok -> true
                                       | _ -> false -> (
                                    let artifact_path =
                                      Project_store.pr_body_artifact_path
                                        ~project_name ~patch_id
                                    in
                                    let body_opt =
                                      try
                                        if Stdlib.Sys.file_exists artifact_path
                                        then (
                                          let ic =
                                            Stdlib.open_in artifact_path
                                          in
                                          let len =
                                            Stdlib.in_channel_length ic
                                          in
                                          let s = Bytes.create len in
                                          Stdlib.really_input ic s 0 len;
                                          Stdlib.close_in ic;
                                          Some (Bytes.to_string s))
                                        else None
                                      with _ -> None
                                    in
                                    match body_opt with
                                    | None ->
                                        log_event runtime ~patch_id
                                          (Printf.sprintf
                                             "pr-body: artifact missing at %s; \
                                              keeping gameplan body"
                                             artifact_path)
                                    | Some body
                                      when String.length (String.trim body) = 0
                                      ->
                                        log_event runtime ~patch_id
                                          "pr-body: artifact empty; keeping \
                                           gameplan body"
                                    | Some body -> (
                                        (* pr_number is guaranteed Some here:
                                           value_exn above would have raised
                                           before the session started if it
                                           were None. *)
                                        let pr =
                                          Base.Option.value_exn pr_number
                                        in
                                        match
                                          Github.update_pr_body ~net github
                                            ~pr_number:pr ~body
                                        with
                                        | Ok () ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "pr-body: PATCHed PR #%d"
                                                 (Pr_number.to_int pr))
                                        | Error e ->
                                            log_event runtime ~patch_id
                                              (Printf.sprintf
                                                 "pr-body: PATCH failed — %s"
                                                 (Github.show_error e))))
                                | Patch_decision.Pr_body_payload
                                | Patch_decision.Human_payload _
                                | Patch_decision.Ci_payload _
                                | Patch_decision.Review_payload _
                                | Patch_decision.Implementation_notes_payload
                                | Patch_decision.Merge_conflict_payload ->
                                    ());
                                result)
                      in
                      let respond_outcome =
                        match result with
                        | `Stale -> Orchestrator.Respond_stale
                        | `Skip_empty -> Orchestrator.Respond_skip_empty
                        | `Failed -> Orchestrator.Respond_failed
                        | `Retry_push -> Orchestrator.Respond_retry_push
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
let normalize_repo_root rr =
  if Filename.is_relative rr then Filename.concat (Stdlib.Sys.getcwd ()) rr
  else rr

let resolve_config ~project ~gameplan_path ~github_token ~backend ~main_branch
    ~poll_interval ~(repo_root : string option) ~max_concurrency ~headless =
  let repo_root_for_fresh =
    normalize_repo_root (Base.Option.value repo_root ~default:".")
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
          }
      in
      let backend =
        if Base.String.is_empty backend then "claude" else backend
      in
      let main_branch = resolve_branch ~repo_root main_branch in
      Project_store.save_config ~project_name ~github_token:token
        ~github_owner:owner ~github_repo:repo ~backend
        ~main_branch:(Branch.to_string main_branch)
        ~poll_interval ~repo_root ~max_concurrency;
      let config =
        {
          project = Some project_name;
          backend;
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
          let backend =
            if Base.String.is_empty backend then "claude" else backend
          in
          let main_branch = resolve_branch ~repo_root main_branch in
          Project_store.save_config ~project_name ~github_token:token
            ~github_owner:owner ~github_repo:repo ~backend
            ~main_branch:(Branch.to_string main_branch)
            ~poll_interval ~repo_root ~max_concurrency;
          Project_store.save_gameplan_source ~project_name ~source_path:gp_path;
          let config =
            {
              project = Some project_name;
              backend;
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
                  let backend =
                    merge_cli_stored backend stored.Project_store.backend
                  in
                  let token_from_stored =
                    merge_cli_stored github_token
                      stored.Project_store.github_token
                  in
                  let repo_root =
                    match repo_root with
                    | Some rr -> normalize_repo_root rr
                    | None -> stored.Project_store.repo_root
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
                    ~backend ~main_branch:(Branch.to_string branch)
                    ~poll_interval:stored.Project_store.poll_interval ~repo_root
                    ~max_concurrency:stored.Project_store.max_concurrency;
                  let config =
                    {
                      project = Some proj;
                      backend;
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

let run_with_config (config : config) gameplan existing_snapshot =
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
      let common_fibers =
        [
          reconciliation_fiber;
          (fun () ->
            poller_fiber ~runtime ~clock ~net ~process_mgr ~github ~config
              ~project_name ~pr_registry ~branch_of ~event_log);
          (fun () ->
            persistence_fiber ~runtime ~clock ~project_name ~transcripts);
        ]
      in
      if config.headless then
        Eio.Fiber.all
          ((fun () -> headless_fiber ~runtime ~clock ~stdout)
          :: (fun () ->
            runner_fiber ~runtime ~env ~config ~project_name ~pr_registry
              ~transcripts ~github ~net ~event_log ())
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
                     ~patches_scroll_offset ~patches_visible_count)
                :: (fun () ->
                  input_fiber ~runtime ~process_mgr ~net ~github ~list_selected
                    ~detail_scroll ~detail_follow ~timeline_scroll
                    ~detail_scrolls ~view_mode ~pr_registry ~project_name
                    ~show_help ~status_msg ~sorted_patch_ids ~input_mode
                    ~prompt_line ~patches_start_row ~patches_scroll_offset
                    ~patches_visible_count ~owner:config.github_owner
                    ~repo:config.github_repo)
                :: (fun () ->
                  runner_fiber ~runtime ~env ~config ~project_name ~pr_registry
                    ~transcripts ~github ~net ~event_log ~status_msg ())
                :: common_fibers)
            with Quit_tui -> ())

let run ~project ~gameplan_path ~github_token ~backend
    ~(main_branch : Branch.t option) ~poll_interval ~(repo_root : string option)
    ~max_concurrency ~headless =
  match
    resolve_config ~project ~gameplan_path ~github_token ~backend ~main_branch
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

let backend_arg =
  let open Cmdliner in
  Arg.(
    value & opt string ""
    & info [ "backend" ] ~docv:"BACKEND"
        ~doc:"LLM backend to use: claude, codex, opencode, pi, or gemini.")

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
        ~doc:"Maximum number of concurrent Claude processes (default: 5)."
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

let main_cmd =
  let open Cmdliner in
  let run_cmd project gameplan_path github_token backend main_branch
      poll_interval repo_root max_concurrency headless upload_debug =
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
        ~main_branch ~poll_interval ~repo_root ~max_concurrency ~headless
  in
  let term =
    Term.(
      const run_cmd $ project_arg $ gameplan_path_arg $ github_token_arg
      $ backend_arg $ main_branch_arg $ poll_interval_arg $ repo_arg
      $ max_concurrency_arg $ headless_arg $ upload_debug_arg)
  in
  let info =
    Cmd.info "onton" ~version:Version.s
      ~doc:
        "Orchestrate parallel patch development with Claude.\n\n\
         Usage:\n\
        \  onton [PROJECT] --gameplan GAMEPLAN [OPTIONS]   Start a new project\n\
        \  onton PROJECT [OPTIONS]                         Resume a saved \
         project"
  in
  Cmd.v info term

let () = Stdlib.exit (Cmdliner.Cmd.eval main_cmd)
