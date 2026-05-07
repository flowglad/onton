open Base

let truncate s n =
  if String.length s <= n then s else String.sub s ~pos:0 ~len:n ^ "..."

let pluralize ?plural n singular =
  let many = match plural with Some p -> p | None -> singular ^ "s" in
  Printf.sprintf "%d %s" n (if n = 1 then singular else many)

let session_mode (agent : Patch_agent.t) :
    [ `Resume of string | `Fresh | `Give_up ] =
  match agent.Patch_agent.session_fallback with
  | Patch_agent.Given_up -> `Give_up
  | Patch_agent.Tried_fresh -> `Fresh
  | Patch_agent.Fresh_available -> (
      match agent.Patch_agent.llm_session_id with
      | Some id -> `Resume id
      | None -> `Fresh)

let extract_pr_number_from_text ?(at_end_of_stream = false) ~owner ~repo text =
  let needle = Printf.sprintf "github.com/%s/%s/pull/" owner repo in
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec scan i =
    (* [>] not [>=]: at [i = text_len - needle_len] the needle still fits
       exactly. Using [>=] would skip that final position; the digit-run
       check below correctly returns [None] when the needle ends at
       [text_len] (no digits possible), so [>] gives the same answer
       without the off-by-one. *)
    if i + needle_len > text_len then None
    else if String.equal (String.sub text ~pos:i ~len:needle_len) needle then
      let start = i + needle_len in
      let rec end_pos j =
        if j < text_len && Char.( >= ) text.[j] '0' && Char.( <= ) text.[j] '9'
        then end_pos (j + 1)
        else j
      in
      let stop = end_pos start in
      (* Mid-stream: a digit run that reaches the end of [text] may continue in
         the next chunk, so committing now would truncate the PR number (e.g.
         emit #12 when the full URL ends in #1234). Require a non-digit
         terminator unless the caller asserts no more text is coming. *)
      let has_terminator = stop < text_len || at_end_of_stream in
      if stop > start && has_terminator then
        try
          Some
            (Types.Pr_number.of_int
               (Stdlib.int_of_string
                  (String.sub text ~pos:start ~len:(stop - start))))
        with _ -> scan (i + 1)
      else scan (i + 1)
    else scan (i + 1)
  in
  scan 0

let%test_module "extract_pr_number_from_text" =
  (module struct
    let pr n = Some (Types.Pr_number.of_int n)

    let%test "complete url with terminator -> commits" =
      Option.equal Types.Pr_number.equal
        (extract_pr_number_from_text ~owner:"foo" ~repo:"bar"
           "see github.com/foo/bar/pull/1234 for details")
        (pr 1234)

    let%test "digit run at end-of-buffer -> waits (None)" =
      Option.is_none
        (extract_pr_number_from_text ~owner:"foo" ~repo:"bar"
           "see github.com/foo/bar/pull/12")

    let%test "digit run at end-of-buffer with ~at_end_of_stream -> commits" =
      Option.equal Types.Pr_number.equal
        (extract_pr_number_from_text ~at_end_of_stream:true ~owner:"foo"
           ~repo:"bar" "see github.com/foo/bar/pull/12")
        (pr 12)

    let%test "trailing newline counts as terminator" =
      Option.equal Types.Pr_number.equal
        (extract_pr_number_from_text ~owner:"foo" ~repo:"bar"
           "github.com/foo/bar/pull/42\nmore text")
        (pr 42)

    let%test "no url -> None" =
      Option.is_none
        (extract_pr_number_from_text ~owner:"foo" ~repo:"bar"
           "no relevant text here")

    let%test "url for different owner/repo -> None" =
      Option.is_none
        (extract_pr_number_from_text ~owner:"foo" ~repo:"bar"
           "github.com/other/repo/pull/12345 ")

    let%test "needle ending exactly at text_len -> None (no digits)" =
      (* The scan-termination guard uses [>] not [>=] so this position is
         attempted, but the digit-run check correctly returns None since
         there's no room for a digit after the needle. *)
      Option.is_none
        (extract_pr_number_from_text ~at_end_of_stream:true ~owner:"foo"
           ~repo:"bar" "github.com/foo/bar/pull/")

    let%test
        "left-to-right: with two URLs, the first wins (callers must window the \
         tail to favor the latest)" =
      Option.equal Types.Pr_number.equal
        (extract_pr_number_from_text ~at_end_of_stream:true ~owner:"foo"
           ~repo:"bar"
           "early stub github.com/foo/bar/pull/12 ... later real \
            github.com/foo/bar/pull/1234")
        (pr 12)
  end)

let run_with_backend ~session_mode_for_agent
    ~(kind : Types.Operation_kind.t option) ~runtime ~process_mgr ~clock ~fs
    ~project_name ~patch_id ~repo_root ~prompt ~(agent : Patch_agent.t) ~owner
    ~repo ~on_pr_detected ~transcripts ~user_config ~worktree_mutex ~hook_mutex
    ~backend_name ~run_backend ~complexity ~event_log =
  let log_event = Runtime_logging.log_event in
  let log_stream_entry = Runtime_logging.log_stream_entry in
  match session_mode_for_agent agent with
  | `Give_up ->
      log_event runtime ~patch_id
        "Session fallback exhausted — continue and fresh both failed, needs \
         intervention";
      Runtime.update_orchestrator runtime (fun orch ->
          Orchestrator.apply_session_result orch patch_id
            Orchestrator.Session_give_up);
      (`Failed, [])
  | (`Resume _ | `Fresh) as mode -> (
      let resume_session, is_fresh =
        match mode with `Resume id -> (Some id, false) | `Fresh -> (None, true)
      in
      match
        Worktree_setup.ensure_worktree ~runtime ~process_mgr ~clock ~fs
          ~repo_root ~project_name ~patch_id ~agent ~user_config ~worktree_mutex
          ~hook_mutex ()
      with
      | None ->
          Runtime.update_orchestrator runtime (fun orch ->
              Orchestrator.apply_session_result orch patch_id
                Orchestrator.Session_worktree_missing);
          (`Failed, [])
      | Some worktree_path ->
          let cwd = Eio.Path.(fs / worktree_path) in
          let text_buf =
            let buf = Buffer.create 4096 in
            (match Stdlib.Hashtbl.find_opt transcripts patch_id with
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
                 tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec backend_name
                 (match resume_session with
                 | Some id ->
                     Printf.sprintf " (--resume %s)"
                       (String.sub id ~pos:0 ~len:(min 8 (String.length id)))
                 | None -> ""));
            Buffer.add_string buf prompt;
            Buffer.add_string buf
              (Printf.sprintf "\n\n---\n**%s response:**\n\n" backend_name);
            Stdlib.Hashtbl.replace transcripts patch_id (Buffer.contents buf);
            buf
          in
          let error_buf = Buffer.create 256 in
          let tool_count = ref 0 in
          (* Accumulates (tool_name, status) for Tool_use events that report a
             non-"completed" status (OpenCode surfaces [pending]/[running] or
             error states; other backends do not populate [status] and so never
             contribute here). Propagated to the caller so artifact-backed
             phases like Pr_body can tell "agent chose not to write" apart
             from "a tool call was announced but never executed". *)
          let tool_failures = ref [] in
          let pr_found = ref false in
          let needle_len =
            String.length (Printf.sprintf "github.com/%s/%s/pull/" owner repo)
          in
          (* Lookback window for the per-chunk tail scan. We need to re-scan
             far enough back to include both the URL prefix and the digit run
             that may have started in a previous chunk; 32 digits covers any
             realistic PR number even if split across many tiny chunks. *)
          let pr_url_lookback = needle_len + 32 in
          let try_extract_pr ?(at_end_of_stream = false) text =
            if !pr_found then ()
            else
              match
                extract_pr_number_from_text ~at_end_of_stream ~owner ~repo text
              with
              | Some pr_number ->
                  pr_found := true;
                  log_stream_entry runtime ~patch_id
                    (Activity_log.Stream_entry.Text_chunk
                       (Printf.sprintf "PR #%d detected"
                          (Types.Pr_number.to_int pr_number)));
                  on_pr_detected pr_number
              | None -> ()
          in
          let last_sync = ref (Unix.gettimeofday ()) in
          let sync_transcript () =
            Stdlib.Hashtbl.replace transcripts patch_id
              (Buffer.contents text_buf)
          in
          let maybe_sync_transcript () =
            let now = Unix.gettimeofday () in
            if Float.( >= ) (now -. !last_sync) 0.2 then (
              last_sync := now;
              sync_transcript ())
          in
          let captured_session_id = ref None in
          let backend_accepted_turn = ref false in
          let mark_backend_accepted_turn () =
            if not !backend_accepted_turn then (
              backend_accepted_turn := true;
              match kind with
              | Some Types.Operation_kind.Human ->
                  Runtime.update_orchestrator runtime (fun orch ->
                      Orchestrator.mark_inflight_human_messages_delivered orch
                        patch_id)
              | Some Types.Operation_kind.Ci
              | Some Types.Operation_kind.Review_comments
              | Some Types.Operation_kind.Findings
              | Some Types.Operation_kind.Pr_body
              | Some Types.Operation_kind.Merge_conflict
              | Some Types.Operation_kind.Rebase
              | None ->
                  ())
          in
          let on_event (event : Types.Stream_event.t) =
            match event with
            (* Turn_started is the preferred signal; the arms below are
               fallbacks for backends that do not emit it. *)
            | Types.Stream_event.Turn_started -> mark_backend_accepted_turn ()
            | Types.Stream_event.Text_delta text ->
                mark_backend_accepted_turn ();
                let prev_len = Buffer.length text_buf in
                Buffer.add_string text_buf text;
                maybe_sync_transcript ();
                if not !pr_found then
                  (* Anchor the tail window to [prev_len], NOT [new_len].
                     We want the window to always cover the ENTIRE new chunk
                     plus up to [pr_url_lookback] bytes back into the prior
                     content (to catch a URL/digit run that spanned the
                     chunk boundary). LLM stream chunks are routinely longer
                     than [pr_url_lookback] (~62 bytes), so anchoring to
                     [new_len] would shrink the window to the last
                     [pr_url_lookback] bytes and miss URLs in the early part
                     of long chunks. *)
                  let offset = max 0 (prev_len - pr_url_lookback) in
                  let tail =
                    Buffer.To_string.sub text_buf ~pos:offset
                      ~len:(Buffer.length text_buf - offset)
                  in
                  try_extract_pr tail
            | Types.Stream_event.Tool_use { name; input; status } ->
                mark_backend_accepted_turn ();
                tool_count := !tool_count + 1;
                (* OpenCode emits pending → running → completed for a single
                   tool call. Track only the latest unresolved status per tool
                   name: clear on completed, replace on any other status.
                   Otherwise a normal pending → completed lifecycle would leave
                   a stale (name, "pending") entry that
                   [classify_pr_body_respond] would misread as a blocked Write. *)
                (match status with
                | Some s when String.equal s "completed" ->
                    tool_failures :=
                      List.filter !tool_failures ~f:(fun (n, _) ->
                          not (String.equal n name))
                | Some s ->
                    let without =
                      List.filter !tool_failures ~f:(fun (n, _) ->
                          not (String.equal n name))
                    in
                    tool_failures := (name, s) :: without
                | None -> ());
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
                  if not (String.is_empty summary) then
                    Printf.sprintf " %s" summary
                  else ""
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
                mark_backend_accepted_turn ();
                sync_transcript ();
                (* Final pass with end-of-stream semantics — catches PR URLs
                   whose digit run terminates exactly at the buffer end (no
                   trailing newline / next chunk to provide a non-digit
                   terminator).

                   Restricted to the same [pr_url_lookback] tail window the
                   per-chunk path uses: scanning the full buffer would
                   left-to-right match an earlier stub fragment (e.g. an
                   abandoned [.../pull/12] from a mid-stream digit-boundary
                   that the per-chunk path correctly returned [None] for),
                   reporting the wrong PR if the real URL appears later in
                   the same buffer. The per-chunk path already saw any URL
                   that had a non-digit terminator during streaming, so the
                   final pass only needs to cover what the tail window does. *)
                (if not !pr_found then
                   let full = Buffer.contents text_buf in
                   let len = String.length full in
                   let offset = max 0 (len - pr_url_lookback) in
                   let tail = String.sub full ~pos:offset ~len:(len - offset) in
                   try_extract_pr ~at_end_of_stream:true tail);
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
                (run_backend ~project_name ~cwd ~patch_id ~prompt
                   ~resume_session ~complexity ~on_event)
            with exn -> Error (Stdlib.Printexc.to_string exn)
          in
          let open Run_classification in
          let outcome =
            Result.map
              ~f:(fun (r : Llm_backend.result) ->
                {
                  exit_code = r.Llm_backend.exit_code;
                  got_events = r.Llm_backend.got_events;
                  saw_final_result = r.Llm_backend.saw_final_result;
                  stderr = r.Llm_backend.stderr;
                  stream_errors = String.strip (Buffer.contents error_buf);
                  timed_out = r.Llm_backend.timed_out;
                })
              result
          in
          (* classify routes Error outcomes to Process_error, so the
             empty-events arms below only ever see Ok. *)
          let log_empty_resume ~tail =
            match result with
            | Error _ -> ()
            | Ok r ->
                let render label s =
                  let s = String.strip s in
                  if String.is_empty s then label ^ "=empty"
                  else
                    Printf.sprintf "%s=%d chars: %s" label (String.length s)
                      (truncate s 500)
                in
                log_event runtime ~patch_id
                  (Printf.sprintf
                     "Resume exited %d (%s) with no parsed stream events%s — \
                      %s %s"
                     r.Llm_backend.exit_code backend_name tail
                     (render "stdout" r.Llm_backend.stdout)
                     (render "stderr" r.Llm_backend.stderr))
          in
          let session_result, user_result =
            match
              classify ~is_resume:(Option.is_some resume_session) outcome
            with
            | Process_error msg ->
                let detail =
                  Printf.sprintf "Process error from %s — %s" backend_name msg
                in
                log_event runtime ~patch_id detail;
                ( Orchestrator.Session_process_error
                    { is_fresh; detail = Some detail },
                  `Failed )
            | No_session_to_resume ->
                log_empty_resume ~tail:" — no session to resume, retrying fresh";
                (Orchestrator.Session_no_resume, `Failed)
            | Timed_out ->
                let detail =
                  Printf.sprintf "Session timed out (%s) — marking failed"
                    backend_name
                in
                log_event runtime ~patch_id detail;
                ( Orchestrator.Session_failed { is_fresh; detail = Some detail },
                  `Failed )
            | Success { stream_errors } ->
                (match (resume_session, result) with
                | Some _, Ok r when not r.Llm_backend.got_events ->
                    log_empty_resume ~tail:""
                | Some _, (Ok _ | Error _) | None, _ -> ());
                if String.length stream_errors > 0 then
                  log_event runtime ~patch_id
                    (Printf.sprintf
                       "Session exited 0 (%s) with stream errors — %s"
                       backend_name
                       (truncate stream_errors 500));
                let text_len = Buffer.length text_buf in
                let tools = !tool_count in
                if tools = 0 && text_len < 200 then
                  log_event runtime ~patch_id
                    (Printf.sprintf
                       "Session exited 0 (%s) with no tool use and %s of text \
                        — %s"
                       backend_name
                       (pluralize text_len "char")
                       (truncate (String.strip (Buffer.contents text_buf)) 200));
                (Orchestrator.Session_ok, `Ok)
            | Session_failed { exit_code; detail } ->
                let formatted =
                  Printf.sprintf "Session failed (%s) — exit %d: %s"
                    backend_name exit_code detail
                in
                log_event runtime ~patch_id formatted;
                ( Orchestrator.Session_failed
                    { is_fresh; detail = Some formatted },
                  `Failed )
          in
          (* Observability: if any tool_use events reported a non-"completed"
             status (OpenCode's sandbox/rejection/pending states), summarize
             them so the disconnect is visible in the activity log even when
             the session otherwise looks healthy. *)
          (match !tool_failures with
          | [] -> ()
          | failures ->
              let rendered =
                List.rev failures
                |> List.map ~f:(fun (n, s) -> Printf.sprintf "%s[%s]" n s)
                |> String.concat ~sep:", "
              in
              log_event runtime ~patch_id
                (Printf.sprintf
                   "Session ended with %d non-completed tool call(s) (%s): %s"
                   (List.length failures) backend_name rendered));
          (* Supervisor-owned push: agent commits locally; we push every
             local commit to the remote at session end. force_push_with_lease
             is idempotent (Push_up_to_date when nothing new), and lease-safe
             against concurrent remote updates. Runs regardless of the LLM's
             session result so commits made before a partial failure still
             reach the remote. *)
          let branch = agent.Patch_agent.branch in
          let base =
            match agent.Patch_agent.base_branch with
            | Some b -> b
            | None ->
                (* Invariant: a running session always has a base_branch set
                   by start/respond/rebase. Fall back to main just in case. *)
                Runtime.read runtime (fun snap ->
                    Orchestrator.main_branch snap.Runtime.orchestrator)
          in
          let push_outcome =
            Worktree.force_push_with_lease ~process_mgr ~path:worktree_path
              ~branch ~base
          in
          (match push_outcome with
          | Worktree.Push_ok ->
              log_event runtime ~patch_id "runner: pushed after session"
          | Worktree.Push_up_to_date ->
              log_event runtime ~patch_id
                "runner: push up-to-date after session (no new commits)"
          | Worktree.Push_no_commits ->
              log_event runtime ~patch_id
                "runner: session ended with no commits on branch — push \
                 skipped, PR creation deferred"
          | Worktree.Push_rejected ->
              log_event runtime ~patch_id
                "runner: push rejected after session (lease)"
          | Worktree.Push_worktree_missing ->
              log_event runtime ~patch_id
                (Printf.sprintf
                   "runner: worktree disappeared mid-session (%s) — local \
                    commits are lost; will reconstruct on next attempt"
                   worktree_path)
          | Worktree.Push_error msg ->
              log_event runtime ~patch_id
                (Printf.sprintf "runner: push error after session: %s" msg));
          (* Combine LLM session outcome with push outcome into a single
             session_result via the pure decision in
             [Orchestrator.combine_session_and_push]. user_result mirrors:
             same Ok/Failed disposition unless the combination promoted us
             to Session_push_failed (which is always Failed).

             Special case: when the agent is responding to a human message,
             the human may have asked a question or made a request that
             requires no code changes. Absence of new commits is therefore
             not a failure — override Session_no_commits to Session_ok so
             the no_commits_push_count counter does not march toward
             needs_intervention and the operation completes cleanly. *)
          let no_commits_is_ok =
            match kind with
            | Some Types.Operation_kind.Human -> true
            | Some Types.Operation_kind.Findings -> true
            | Some Types.Operation_kind.Ci
            | Some Types.Operation_kind.Review_comments
            | Some Types.Operation_kind.Pr_body
            | Some Types.Operation_kind.Merge_conflict
            | Some Types.Operation_kind.Rebase
            | None ->
                false
          in
          let final_session_result =
            let combined =
              Orchestrator.combine_session_and_push ~session:session_result
                ~push:push_outcome
            in
            match combined with
            | Orchestrator.Session_no_commits when no_commits_is_ok ->
                Orchestrator.Session_ok
            | Orchestrator.Session_ok | Orchestrator.Session_no_commits
            | Orchestrator.Session_process_error _
            | Orchestrator.Session_no_resume | Orchestrator.Session_failed _
            | Orchestrator.Session_give_up
            | Orchestrator.Session_worktree_missing
            | Orchestrator.Session_push_failed ->
                combined
          in
          let final_user_result =
            match final_session_result with
            | Orchestrator.Session_ok -> user_result
            | Orchestrator.Session_push_failed | Orchestrator.Session_no_commits
              ->
                (* LLM session ran fine but commits didn't ship (push failed
                   or the agent made no commits) — signal retry so the
                   Respond path uses Respond_retry_push (clean complete) and
                   the reconciler re-enqueues the operation naturally. After
                   2 consecutive no-commit sessions, needs_intervention fires
                   and the scheduler stops re-enqueueing. *)
                `Retry_push
            | Orchestrator.Session_process_error _
            | Orchestrator.Session_no_resume | Orchestrator.Session_failed _
            | Orchestrator.Session_give_up
            | Orchestrator.Session_worktree_missing ->
                `Failed
          in
          let agent_before, agent_after =
            Runtime.update_orchestrator_returning runtime (fun orch ->
                let agent_before = Orchestrator.agent orch patch_id in
                (* Store the captured session_id BEFORE applying the session
                   result. [apply_session_result] clears [llm_session_id] on
                   start-path fresh failure (via [on_session_failure]) and on
                   [Session_no_resume] / [Session_give_up]; doing the set
                   afterwards would overwrite that reset and break the
                   clean-retry path. *)
                let orch =
                  match !captured_session_id with
                  | Some _ ->
                      Orchestrator.set_llm_session_id orch patch_id
                        !captured_session_id
                  | None -> orch
                in
                let orch =
                  Orchestrator.apply_session_result orch patch_id
                    final_session_result
                in
                let agent_after = Orchestrator.agent orch patch_id in
                (orch, (agent_before, agent_after)))
          in
          Event_log.log_complete event_log ~patch_id
            ~result:final_session_result ~agent_before ~agent_after;
          (final_user_result, List.rev !tool_failures))

let run ~(kind : Types.Operation_kind.t option) ~runtime ~process_mgr ~clock ~fs
    ~project_name ~patch_id ~repo_root ~prompt ~(agent : Patch_agent.t) ~owner
    ~repo ~on_pr_detected ~transcripts ~user_config ~worktree_mutex ~hook_mutex
    ~backend ~complexity ~event_log =
  run_with_backend ~kind ~runtime ~process_mgr ~clock ~fs ~project_name
    ~patch_id ~repo_root ~prompt ~agent ~owner ~repo ~on_pr_detected
    ~transcripts ~user_config ~worktree_mutex ~hook_mutex
    ~session_mode_for_agent:session_mode ~backend_name:backend.Llm_backend.name
    ~run_backend:backend.Llm_backend.run_streaming ~complexity ~event_log

type long_lived_session =
  | Long_lived_session : {
      name : string;
      start : sw:Eio.Switch.t -> Llm_backend_long_lived.start_config -> 'handle;
      prompt_backend :
        'handle ->
        prompt:string ->
        timeout:float ->
        on_event:(Types.Stream_event.t -> unit) ->
        Llm_backend_long_lived.result;
      shutdown : 'handle -> unit;
      mutable handle : 'handle option;
      mutable pending_shutdown_handle : 'handle option;
      mutable failed : bool;
      mutable failure_reason : string option;
      provider : string;
      model : string;
      effort : string;
      mutable gameplan_prompt : string;
      mutable patch_prompt : string;
      timeout : float;
    }
      -> long_lived_session

let create_long_lived_session ~(backend : Llm_backend_long_lived.t) ~provider
    ~model ~effort ~gameplan_prompt ~patch_prompt =
  let (Llm_backend_long_lived.T
         { name; timeout; start; prompt = prompt_backend; shutdown; _ }) =
    backend
  in
  Long_lived_session
    {
      name;
      start;
      prompt_backend;
      shutdown;
      handle = None;
      pending_shutdown_handle = None;
      failed = false;
      failure_reason = None;
      provider;
      model;
      effort;
      gameplan_prompt;
      patch_prompt;
      timeout;
    }

let update_long_lived_session_prompts session ~gameplan_prompt ~patch_prompt =
  let (Long_lived_session session) = session in
  let changed =
    (not (String.equal session.gameplan_prompt gameplan_prompt))
    || not (String.equal session.patch_prompt patch_prompt)
  in
  if changed && Option.is_some session.handle then (
    let handle = session.handle in
    session.handle <- None;
    session.pending_shutdown_handle <- handle;
    session.failed <- true;
    session.failure_reason <-
      Some "long-lived backend prompt prefix changed after session start");
  session.gameplan_prompt <- gameplan_prompt;
  session.patch_prompt <- patch_prompt

let long_lived_session_failed = function
  | Long_lived_session session -> session.failed

let shutdown_long_lived_session = function
  | Long_lived_session session -> (
      let pending_shutdown_handle = session.pending_shutdown_handle in
      let handle = session.handle in
      session.pending_shutdown_handle <- None;
      session.handle <- None;
      (match pending_shutdown_handle with
      | None -> ()
      | Some handle -> session.shutdown handle);
      match handle with None -> () | Some handle -> session.shutdown handle)

let run_long_lived ~sw ~(kind : Types.Operation_kind.t option) ~runtime
    ~process_mgr ~clock ~fs ~project_name ~patch_id ~repo_root ~prompt
    ~(agent : Patch_agent.t) ~owner ~repo ~on_pr_detected ~transcripts
    ~user_config ~worktree_mutex ~hook_mutex ~session ~complexity ~event_log =
  let (Long_lived_session session) = session in
  let run_backend ~project_name ~cwd ~patch_id ~prompt ~resume_session:_
      ~complexity:_ ~on_event =
    let failed_result message =
      on_event (Types.Stream_event.Error message);
      {
        Llm_backend.exit_code = 1;
        stdout = "";
        stderr = message;
        got_events = true;
        saw_final_result = false;
        timed_out = false;
      }
    in
    if session.failed then
      failed_result
        (Option.value session.failure_reason
           ~default:
             "long-lived backend session already failed; waiting for teardown")
    else
      match
        match session.handle with
        | Some handle -> Ok handle
        | None -> (
            try
              let handle =
                session.start ~sw
                  {
                    project_name;
                    worktree = cwd;
                    patch_id;
                    provider = session.provider;
                    model = session.model;
                    effort = session.effort;
                    gameplan_prompt = session.gameplan_prompt;
                    patch_prompt = session.patch_prompt;
                  }
              in
              session.handle <- Some handle;
              Ok handle
            with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | exn ->
                session.failed <- true;
                let message =
                  Printf.sprintf "long-lived backend start raised: %s"
                    (Stdlib.Printexc.to_string exn)
                in
                session.failure_reason <- Some message;
                Error message)
      with
      | Error message -> failed_result message
      | Ok handle -> (
          try
            let result =
              session.prompt_backend handle ~prompt ~timeout:session.timeout
                ~on_event
            in
            if result.Llm_backend.exit_code <> 0 || result.Llm_backend.timed_out
            then (
              session.handle <- None;
              session.failed <- true;
              session.failure_reason <-
                Some "long-lived backend prompt failed or timed out";
              try session.shutdown handle with _ -> ());
            result
          with
          | Eio.Cancel.Cancelled _ as exn ->
              session.handle <- None;
              session.failed <- true;
              session.failure_reason <-
                Some "long-lived backend prompt cancelled";
              (try session.shutdown handle with _ -> ());
              raise exn
          | exn ->
              session.handle <- None;
              session.pending_shutdown_handle <- Some handle;
              session.failed <- true;
              let message =
                Printf.sprintf "long-lived backend prompt raised: %s"
                  (Stdlib.Printexc.to_string exn)
              in
              session.failure_reason <- Some message;
              failed_result message)
  in
  run_with_backend ~kind ~runtime ~process_mgr ~clock ~fs ~project_name
    ~patch_id ~repo_root ~prompt ~agent ~owner ~repo ~on_pr_detected
    ~transcripts ~user_config ~worktree_mutex ~hook_mutex
    ~session_mode_for_agent:(fun _ -> `Fresh)
    ~backend_name:session.name ~run_backend ~complexity ~event_log
