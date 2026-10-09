(* @archlint.module shell
   @archlint.domain orchestrator *)

open Base

type disposition = [ `Ok | `Failed | `Retry_push | `No_commits ]
type prompt = { context : worktree_path:string -> string; turn : string }

type run_result = {
  disposition : disposition;
  tool_failures : (string * string) list;
  turn_accepted : bool;
}

let truncate s n =
  if String.length s <= n then s else String.sub s ~pos:0 ~len:n ^ "..."

let pluralize ?plural n singular =
  let many = match plural with Some p -> p | None -> singular ^ "s" in
  Printf.sprintf "%d %s" n (if n = 1 then singular else many)

let safe_session_id s =
  (not (String.is_empty s))
  && (not (String.is_substring s ~substring:".."))
  && String.for_all s ~f:(fun c ->
      Char.is_alphanum c || Char.equal c '-' || Char.equal c '_'
      || Char.equal c '.')

let%test "safe_session_id accepts ordinary claude ids" =
  safe_session_id "abc-123_DEF.456"

let%test "safe_session_id rejects traversal and separators" =
  (not (safe_session_id "../abc"))
  && (not (safe_session_id "abc/def"))
  && (not (safe_session_id "abc\\def"))
  && not (safe_session_id "")

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

(* ppx_inline_test v0.17 emits an unused local module binding under OCaml 5.5.
   Keep warning 60 disabled only for this generated structure item. *)
[@@@warning "-60"]

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

[@@@warning "+60"]

module type ENV = sig
  include Run_env.S

  val owner : string
  val repo : string
  val transcripts : (Types.Patch_id.t, string) Stdlib.Hashtbl.t
  val transcript_updates : (Types.Patch_id.t, string) Stdlib.Hashtbl.t
  val event_log : Event_log.t
end

module Make (W : Worktree.S) (Env : ENV) = struct
  let create_prompt ~context ~turn = { context; turn }

  type nonrec run_result = run_result

  let session_mode = session_mode
  let extract_pr_number_from_text = extract_pr_number_from_text

  module WS = Worktree_setup.Make (W) (Env)

  let publish_transcript ~patch_id text =
    Stdlib.Hashtbl.replace Env.transcripts patch_id text;
    Stdlib.Hashtbl.replace Env.transcript_updates patch_id text

  let begin_transcript ~patch_id ~backend_name ~resume_session ~prompt =
    let buf = Buffer.create 4096 in
    Option.iter (Stdlib.Hashtbl.find_opt Env.transcripts patch_id)
      ~f:(fun previous -> Buffer.add_string buf previous);
    let tm = Unix.localtime (Unix.gettimeofday ()) in
    Buffer.add_string buf
      (Printf.sprintf
         "\n\
          ---\n\
          **[%02d:%02d:%02d] Delivered to %s%s:**\n\n\
          %s\n\n\
          ---\n\
          **%s response:**\n\n"
         tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec backend_name
         (Option.value_map resume_session ~default:"" ~f:(fun id ->
              " (--resume " ^ String.prefix id 8 ^ ")"))
         prompt backend_name);
    publish_transcript ~patch_id (Buffer.contents buf);
    buf

  let run_repair ~patch_id ~(agent : Patch_agent.t) ~backend ~complexity ~cwd
      ~context ~guidance ~turn ~read_head =
    (* A repair is another turn of the patch conversation. In particular, the
       ordinary fresh-session retry state must not discard this context. *)
    let resume_session = agent.llm_session_id in
    let session_uuid =
      Option.value_or_thunk resume_session ~default:Session_id.mint
    in
    let prompt = Branch_reconcile.recovery_prompt ~context ~guidance turn in
    let text_buf =
      begin_transcript ~patch_id ~backend_name:backend.Llm_backend.name
        ~resume_session ~prompt
    in
    let captured_session = ref resume_session in
    let gate = ref (Content_gate.create ()) in
    let streamed_text = ref false in
    let sync () = publish_transcript ~patch_id (Buffer.contents text_buf) in
    let on_event event =
      let log entry =
        Runtime_logging.log_stream_entry Env.runtime ~patch_id entry
      in
      (match event with
      | Types.Stream_event.Turn_started -> ()
      | Text_delta text ->
          streamed_text := true;
          Buffer.add_string text_buf text;
          log (Activity_log.Stream_entry.Text_chunk text)
      | Tool_use { name; input; status } ->
          Buffer.add_string text_buf
            (Printf.sprintf "\n[tool: %s%s] %s\n" name
               (Option.value_map status ~default:"" ~f:(fun status ->
                    " " ^ status))
               input);
          log (Activity_log.Stream_entry.Tool_use (name, input))
      | Final_result { text; stop_reason } ->
          if not !streamed_text then Buffer.add_string text_buf text;
          streamed_text := false;
          log
            (Activity_log.Stream_entry.Finished
               (Types.Stop_reason.to_display stop_reason))
      | Error error ->
          Buffer.add_string text_buf ("\n[error] " ^ error ^ "\n");
          log (Activity_log.Stream_entry.Stream_error error)
      | Session_init { session_id; _ } -> (
          match resume_session with
          | Some expected when not (String.equal expected session_id) ->
              failwith "repair backend changed the patch conversation identity"
          | Some _ | None -> captured_session := Some session_id));
      sync ();
      let next_gate, persist = Content_gate.should_persist !gate event in
      gate := next_gate;
      if persist then
        Option.iter !captured_session ~f:(fun session_id ->
            let snapshot_path = Project_store.snapshot_path Env.project_name in
            (match
               Runtime.update_persisting Env.runtime
                 ~persist:(Persistence.save_snapshot ~path:snapshot_path)
                 (fun snap ->
                   let orch =
                     Orchestrator.set_llm_session_id snap.Runtime.orchestrator
                       patch_id (Some session_id)
                   in
                   let orch =
                     Orchestrator.clear_session_fallback orch patch_id
                   in
                   let transcripts = Hashtbl.copy snap.transcripts in
                   Hashtbl.set transcripts ~key:patch_id
                     ~data:(Buffer.contents text_buf);
                   ({ snap with Runtime.orchestrator = orch; transcripts }, ()))
             with
            | Ok () -> ()
            | Error error ->
                failwith ("Cannot checkpoint repair conversation: " ^ error));
            match
              Persistence.record_session_id
                ~snapshot_path:(Project_store.snapshot_path Env.project_name)
                ~patch_id ~session_id
            with
            | Ok () -> ()
            | Error error ->
                Runtime_logging.log_event Env.runtime ~patch_id
                  ("Failed to persist repair conversation: " ^ error))
    in
    Exn.protect ~finally:sync ~f:(fun () ->
        Branch_repair_session.run ~backend ~context ~guidance ~on_event ~cwd
          ~project_name:Env.project_name ~patch_id ~complexity ~resume_session
          ~session_uuid ~turn ~read_head ~now:(fun () -> Eio.Time.now Env.clock))

  let publish_completion ~write_owner ~patch_id ~(agent : Patch_agent.t) ~path:_
      completion =
    let base, policy =
      Runtime.read Env.runtime (fun snap ->
          let orch = snap.Runtime.orchestrator in
          ( Types.Branch.to_string
              (Option.value agent.base_branch
                 ~default:(Orchestrator.main_branch orch)),
            if Orchestrator.is_integration_root orch patch_id then
              Branch_reconcile.Preserve_ancestry
            else Branch_reconcile.Rewrite ))
    in
    let event =
      Branch_reconcile.Request
        (Session_result.publication_intent completion ~base ~policy)
    in
    let persist =
      Persistence.save_snapshot
        ~path:(Project_store.snapshot_path Env.project_name)
    in
    let execute = WS.execute_reconciliation ~patch_id in
    let now () = Eio.Time.now Env.clock in
    let outcome =
      Branch_reconcile_runner.run_owned ~owner:write_owner ~persist ~now
        ~execute event
    in
    let outcome =
      match outcome with
      | Branch_reconcile_runner.Idle ->
          (* A settled receipt can outlive its remote ref while Start retries
             PR creation. Revalidate before reporting publication to the caller. *)
          Branch_reconcile_runner.run_owned ~owner:write_owner ~persist ~now
            ~execute Branch_reconcile.Reconfirm_publication
      | Branch_reconcile_runner.Waiting
      | Branch_reconcile_runner.Repair_needed _
      | Branch_reconcile_runner.Intervention _
      | Branch_reconcile_runner.Checkpoint_failed _ ->
          outcome
    in
    match outcome with
    | Branch_reconcile_runner.Idle ->
        Runtime.read Env.runtime (fun snap ->
            Branch_reconcile.publication_status
              (Orchestrator.agent snap.Runtime.orchestrator patch_id)
                .branch_reconcile)
    | Branch_reconcile_runner.Waiting | Branch_reconcile_runner.Repair_needed _
      ->
        `Pending
    | Branch_reconcile_runner.Intervention reason
    | Branch_reconcile_runner.Checkpoint_failed reason ->
        Runtime_logging.log_event Env.runtime ~patch_id
          ("Publication pending: " ^ reason);
        `Pending

  let run_owned ~write_owner ~(kind : Types.Operation_kind.t option)
      ~(delivery_mode : Patch_decision.delivery_mode) ~patch_id ~prompt
      ~(agent : Patch_agent.t) ~on_pr_detected ~(backend : Llm_backend.t)
      ~complexity =
    let backend_name = backend.name in
    let make_run_result ?(turn_accepted = false) disposition tool_failures =
      { disposition; tool_failures; turn_accepted }
    in
    let runtime = Env.runtime in
    let fs = Env.fs in
    let project_name = Env.project_name in
    let owner = Env.owner in
    let repo = Env.repo in
    let publish_transcript = publish_transcript ~patch_id in
    let log_event = Runtime_logging.log_event in
    let log_stream_entry = Runtime_logging.log_stream_entry in
    match
      Option.filter agent.session_completion
        ~f:
          (Session_result.resume_start ~delivery_mode
             ~guidance:agent.inflight_human_messages
             ~publication:agent.branch_reconcile)
    with
    | Some completion ->
        (* Completed implementation resumes its captured publication. Replacing
           that operation with provisioning would discard its fixed point and
           could publish the same completed turn twice. The owned publication
           executor performs any necessary checkout checks. *)
        let path =
          Option.value agent.worktree_path
            ~default:(Worktree.worktree_dir ~project_name ~patch_id)
        in
        let disposition =
          match
            publish_completion ~write_owner ~patch_id ~agent ~path completion
          with
          | `Published -> `Ok
          | `No_work -> `No_commits
          | `Pending -> `Retry_push
        in
        make_run_result ~turn_accepted:completion.turn_accepted disposition []
    | None -> (
        match session_mode agent with
        | `Give_up ->
            log_event runtime ~patch_id
              "Session fallback exhausted — continue and fresh both failed, \
               needs intervention";
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.apply_session_result orch patch_id
                  Orchestrator.Session_give_up);
            make_run_result `Failed []
        | (`Resume _ | `Fresh) as mode -> (
            let resume_session, is_fresh =
              match mode with
              | `Resume id -> (Some id, false)
              | `Fresh -> (None, true)
            in
            match WS.ensure_owned ~owner:write_owner () with
            | Worktree_setup.Unavailable _ -> make_run_result `Failed []
            | Worktree_setup.Path worktree_path ->
                let cwd = Eio.Path.(fs / worktree_path) in
                let pre_session_branch_sha =
                  let branch_str =
                    Types.Branch.to_string agent.Patch_agent.branch
                  in
                  W.read_branch_sha ~path:worktree_path
                    ~ref_name:("refs/heads/" ^ branch_str)
                in
                let base =
                  Option.value agent.Patch_agent.base_branch
                    ~default:
                      (Runtime.read runtime (fun snap ->
                           Orchestrator.main_branch snap.Runtime.orchestrator))
                in
                let initial_base_sha =
                  let base_name = Types.Branch.to_string base in
                  match
                    W.read_branch_sha ~path:worktree_path
                      ~ref_name:("refs/remotes/origin/" ^ base_name)
                  with
                  | Some _ as sha -> sha
                  | None ->
                      W.read_branch_sha ~path:worktree_path
                        ~ref_name:("refs/heads/" ^ base_name)
                in
                let wontdo_path =
                  Project_store.wontdo_artifact_path ~project_name ~patch_id
                in
                Project_store.ensure_dir (Stdlib.Filename.dirname wontdo_path);
                (* Each turn owns its signal; manual intervention must not replay an
               earlier opt-out file. Keep the previous message in the event log. *)
                (try Unix.unlink wontdo_path
                 with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
                let prompt =
                  let context =
                    match resume_session with
                    | None -> prompt.context ~worktree_path
                    | Some _ -> ""
                  in
                  let prompt =
                    Onton_core.Session_prompt.render ~resume_session
                      (Onton_core.Session_prompt.create ~context
                         ~turn:prompt.turn)
                  in
                  if
                    Patch_decision.session_prompt_requires_patch_instructions
                      ~delivery_mode ~kind
                  then
                    let mode_instructions =
                      Runtime.read runtime (fun snap ->
                          let orch = snap.Runtime.orchestrator in
                          if Orchestrator.is_feature_descendant orch patch_id
                          then
                            "\n\
                             Feature branch construction: this patch publishes \
                             a branch only. Do not create or modify a pull \
                             request or PR body. Commit locally; the \
                             supervisor pushes and integrates after branch \
                             HEAD checks pass.\n"
                          else if Orchestrator.is_integration_root orch patch_id
                          then
                            "\n\
                             Integration root: preserve all published history. \
                             Never rebase, reset, or force-push this branch. \
                             Commit implementation changes locally; the \
                             supervisor reconciles captured upstream revisions \
                             and uses normal pushes. Do not merge other \
                             branches.\n"
                          else "")
                    in
                    prompt ^ mode_instructions
                    ^ Printf.sprintf
                        "\n\n\
                         Before making your first commit, you may opt out of \
                         this patch. If you decide the work should not \
                         proceed, write your reason to `%s` and end your turn \
                         without committing. The supervisor will surface that \
                         message and stop this patch without pushing or \
                         opening a PR. This option is unavailable after any \
                         patch commit or PR exists."
                        wontdo_path
                  else prompt
                in
                (* Read once at session start so the per-event callback below can
             persist the session id to the crash-recovery sidecar without
             repeated env lookups.  When unset, the sidecar write is a no-op:
             nothing to recover from on restart anyway. *)
                let snapshot_path_opt =
                  match Stdlib.Sys.getenv_opt "ONTON_SNAPSHOT_PATH" with
                  | Some p when not (String.is_empty (String.strip p)) -> Some p
                  | _ -> None
                in
                let text_buf =
                  begin_transcript ~patch_id ~backend_name ~resume_session
                    ~prompt
                in
                let error_buf = Buffer.create 256 in
                let session_started_at = Unix.gettimeofday () in
                let session_uuid = Session_id.mint () in
                let session_sink =
                  Session_artifacts.create ~project_name ~patch_id ~session_uuid
                in
                Telemetry_dispatch.register_sink session_sink;
                Exn.protect
                  ~finally:(fun () ->
                    Telemetry_dispatch.unregister_sink
                      ~name:(Session_artifacts.sink_name ~session_uuid))
                  ~f:(fun () ->
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
                      String.length
                        (Printf.sprintf "github.com/%s/%s/pull/" owner repo)
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
                          extract_pr_number_from_text ~at_end_of_stream ~owner
                            ~repo text
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
                      publish_transcript (Buffer.contents text_buf)
                    in
                    let maybe_sync_transcript () =
                      let now = Unix.gettimeofday () in
                      if Float.( >= ) (now -. !last_sync) 0.2 then (
                        last_sync := now;
                        sync_transcript ())
                    in
                    let captured_session_id = ref None in
                    let captured_init = ref Failure_subkind.default_init in
                    let content_gate = ref (Content_gate.create ()) in
                    let maybe_persist_session_id () =
                      match (snapshot_path_opt, !captured_session_id) with
                      | Some snapshot_path, Some session_id -> (
                          match
                            Persistence.record_session_id ~snapshot_path
                              ~patch_id ~session_id
                          with
                          | Ok () -> ()
                          | Error msg ->
                              log_event runtime ~patch_id
                                (Printf.sprintf
                                   "Failed to record session_id sidecar for %s \
                                    — %s"
                                   session_id msg))
                      | None, _ | _, None -> ()
                    in
                    let backend_accepted_turn = ref false in
                    let mark_backend_accepted_turn () =
                      if not !backend_accepted_turn then (
                        backend_accepted_turn := true;
                        if
                          Patch_decision.human_acceptance_delivers_messages
                            ~agent ~delivery_mode ~kind
                        then
                          Runtime.update_orchestrator runtime (fun orch ->
                              Orchestrator
                              .mark_inflight_human_messages_delivered orch
                                patch_id)
                        else ())
                    in
                    let on_event (event : Types.Stream_event.t) =
                      let () =
                        match event with
                        (* Turn_started is the preferred signal; the arms below are
                 fallbacks for backends that do not emit it. *)
                        | Types.Stream_event.Turn_started ->
                            mark_backend_accepted_turn ()
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
                     Otherwise a normal pending → completed lifecycle would
                     leave a stale (name, "pending") entry that
                     [classify_pr_body_respond] would misread as a blocked
                     Write. *)
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
                                let field key = Json.string_field key json in
                                let s =
                                  match name with
                                  | "Bash" -> field "command"
                                  | "Read" | "Write" -> field "file_path"
                                  | "Edit" -> field "file_path"
                                  | "Glob" -> field "pattern"
                                  | "Grep" -> field "pattern"
                                  | _ -> None
                                in
                                match s with
                                | Some v -> truncate v 80
                                | None -> ""
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
                                && Char.equal
                                     (Buffer.nth text_buf (len - 1))
                                     '\n'
                                && Char.equal
                                     (Buffer.nth text_buf (len - 2))
                                     '\n'
                              then ""
                              else if
                                Char.equal (Buffer.nth text_buf (len - 1)) '\n'
                              then "\n"
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
                               let tail =
                                 String.sub full ~pos:offset ~len:(len - offset)
                               in
                               try_extract_pr ~at_end_of_stream:true tail);
                            let reason =
                              Types.Stop_reason.to_display stop_reason
                            in
                            log_stream_entry runtime ~patch_id
                              (Activity_log.Stream_entry.Finished reason)
                        | Types.Stream_event.Error msg ->
                            if Buffer.length error_buf > 0 then
                              Buffer.add_char error_buf '\n';
                            Buffer.add_string error_buf msg;
                            log_stream_entry runtime ~patch_id
                              (Activity_log.Stream_entry.Stream_error msg)
                        | Types.Stream_event.Session_init
                            {
                              session_id;
                              api_key_source;
                              model;
                              claude_code_version;
                              permission_mode = _;
                            } ->
                            captured_session_id := Some session_id;
                            captured_init :=
                              {
                                Failure_subkind.api_key_source;
                                model;
                                claude_code_version;
                              }
                      in
                      (* Persist the crash-recovery sidecar lazily: only after claude
               has *committed* a conversation turn to its .jsonl (first
               Final_result event).  Streamed chunks (Text_delta, Tool_use)
               can fire before the turn lands on disk — if the API errors
               mid-turn, the .jsonl stays at its 124-byte header and a
               sidecar pointing at it would poison every later --resume.
               Run this after the event dispatch so a same-batch Session_init
               has already updated [captured_session_id]. *)
                      let next_gate, persist =
                        Content_gate.should_persist !content_gate event
                      in
                      content_gate := next_gate;
                      if persist then maybe_persist_session_id ()
                    in
                    let cancelled = ref None in
                    let result =
                      try
                        Ok
                          (backend.run_streaming ~project_name ~cwd ~patch_id
                             ~prompt ~resume_session ~session_uuid ~complexity
                             ~on_event)
                      with
                      | Eio.Cancel.Cancelled _ as exn ->
                          cancelled := Some exn;
                          Error (Stdlib.Printexc.to_string exn)
                      | exn -> Error (Stdlib.Printexc.to_string exn)
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
                            stream_errors =
                              String.strip (Buffer.contents error_buf);
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
                              Printf.sprintf "%s=%d chars: %s" label
                                (String.length s) (truncate s 500)
                          in
                          log_event runtime ~patch_id
                            (Printf.sprintf
                               "Resume exited %d (%s) with no parsed stream \
                                events%s — %s %s"
                               r.Llm_backend.exit_code backend_name tail
                               (render "stdout" r.Llm_backend.stdout)
                               (render "stderr" r.Llm_backend.stderr))
                    in
                    let classification =
                      classify
                        ~is_resume:(Option.is_some resume_session)
                        outcome
                    in
                    let session_result, user_result =
                      match classification with
                      | Process_error msg ->
                          let detail =
                            Printf.sprintf "Process error from %s — %s"
                              backend_name msg
                          in
                          log_event runtime ~patch_id detail;
                          ( Orchestrator.Session_process_error
                              { is_fresh; detail = Some detail },
                            `Failed )
                      | No_session_to_resume ->
                          log_empty_resume
                            ~tail:" — no session to resume, retrying fresh";
                          (* Remove the stub .jsonl that claude refused to resume.
                   Otherwise it sits in the per-patch projects dir forever and
                   trips any future attempt that happens to target the same
                   session id.  Best-effort: a missing file or a transient
                   filesystem error must not interrupt the retry path. *)
                          (match resume_session with
                          | None -> ()
                          | Some session_id when safe_session_id session_id -> (
                              try
                                let path =
                                  Spawn_env.claude_session_jsonl_path
                                    ~project_name ~patch_id ~worktree_path
                                    ~session_id
                                in
                                Unix.unlink path
                              with _ -> ())
                          | Some session_id ->
                              log_event runtime ~patch_id
                                (Printf.sprintf
                                   "Skipping cleanup for unsafe Claude session \
                                    id %S"
                                   session_id));
                          (Orchestrator.Session_no_resume, `Failed)
                      | Timed_out ->
                          let detail =
                            Printf.sprintf
                              "Session timed out (%s) — preserving session for \
                               retry"
                              backend_name
                          in
                          log_event runtime ~patch_id detail;
                          ( Orchestrator.Session_timed_out
                              {
                                session_id =
                                  Option.first_some !captured_session_id
                                    resume_session;
                                detail = Some detail;
                              },
                            `Failed )
                      | Context_exhausted { stream_errors } ->
                          (* The model's context window overflowed (e.g. Codex "ran
                   out of room"). Resuming this thread would re-overflow, so
                   [Session_context_exhausted] clears the session id and the
                   next run starts fresh. The dedicated counter surfaces the
                   agent for intervention if a fresh session overflows too. *)
                          log_event runtime ~patch_id
                            (Printf.sprintf
                               "Session exited (%s) — context window \
                                exhausted; clearing session, next run starts \
                                fresh — %s"
                               backend_name
                               (truncate stream_errors 500));
                          (Orchestrator.Session_context_exhausted, `Failed)
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
                                 "Session exited 0 (%s) with no tool use and \
                                  %s of text — %s"
                                 backend_name
                                 (pluralize text_len "char")
                                 (truncate
                                    (String.strip (Buffer.contents text_buf))
                                    200));
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
                    let tail s =
                      let len = String.length s in
                      let pos = max 0 (len - 4096) in
                      String.sub s ~pos ~len:(len - pos)
                    in
                    let text_tail = tail (Buffer.contents text_buf) in
                    let stderr_tail =
                      match result with
                      | Ok r -> tail r.Llm_backend.stderr
                      | Error msg -> tail msg
                    in
                    let subkind =
                      Failure_subkind.classify ~classification
                        ~init:!captured_init ~text_tail ~stderr_tail
                    in
                    let meta =
                      let init = !captured_init in
                      let exit_code =
                        match result with
                        | Ok r -> r.Llm_backend.exit_code
                        | Error _ -> 1
                      in
                      Session_meta.create ~onton_session_uuid:session_uuid
                        ?claude_session_id:!captured_session_id
                        ~patch_id:(Types.Patch_id.to_string patch_id)
                        ~started_at:session_started_at
                        ~ended_at:(Unix.gettimeofday ()) ~exit_code ~subkind
                        ?api_key_source:init.api_key_source ?model:init.model
                        ?claude_code_version:init.claude_code_version ()
                    in
                    Telemetry_dispatch.emit
                      (Telemetry.Event.Spawn_finalized
                         {
                           patch_id;
                           session_uuid;
                           meta = Session_meta.yojson_of_t meta;
                         });
                    (* Observability: if any tool_use events reported a non-"completed"
             status (OpenCode's sandbox/rejection/pending states), summarize
             them so the disconnect is visible in the activity log even when
             the session otherwise looks healthy. *)
                    (match !tool_failures with
                    | [] -> ()
                    | failures ->
                        let rendered =
                          List.rev failures
                          |> List.map ~f:(fun (n, s) ->
                              Printf.sprintf "%s[%s]" n s)
                          |> String.concat ~sep:", "
                        in
                        log_event runtime ~patch_id
                          (Printf.sprintf
                             "Session ended with %d non-completed tool call(s) \
                              (%s): %s"
                             (List.length failures) backend_name rendered));
                    let apply_result_and_emit_complete final_session_result =
                      let agent_before, agent_after =
                        Runtime.update_orchestrator_returning runtime
                          (fun orch ->
                            let agent_before =
                              Orchestrator.agent orch patch_id
                            in
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
                            let agent_after =
                              Orchestrator.agent orch patch_id
                            in
                            (orch, (agent_before, agent_after)))
                      in
                      let subkind =
                        match final_session_result with
                        | Orchestrator.Session_wontdo _ ->
                            Failure_subkind.Other "wontdo"
                        | Orchestrator.Session_ok
                        | Orchestrator.Session_process_error _
                        | Orchestrator.Session_no_resume
                        | Orchestrator.Session_timed_out _
                        | Orchestrator.Session_failed _
                        | Orchestrator.Session_give_up
                        | Orchestrator.Session_no_commits
                        | Orchestrator.Session_context_exhausted ->
                            subkind
                      in
                      Telemetry_dispatch.emit
                        (Telemetry.Event.Complete
                           {
                             patch_id;
                             session_uuid = Some session_uuid;
                             subkind;
                             payload =
                               `Assoc
                                 [
                                   ( "result",
                                     `String
                                       (Orchestrator.show_session_result
                                          final_session_result) );
                                   ( "agent_before",
                                     Persistence.patch_agent_to_yojson
                                       agent_before );
                                   ( "agent_after",
                                     Persistence.patch_agent_to_yojson
                                       agent_after );
                                 ];
                           });
                      (agent_before, agent_after)
                    in
                    (* Publication must not erase the backend's outcome. Persist it
                   while the session still owns the checkout, before any push or
                   cancellation exit can obscure the local revision. *)
                    let completion : Session_result.completion =
                      {
                        session_uuid;
                        delivery_mode;
                        kind;
                        message_id = agent.current_message_id;
                        result = session_result;
                        guidance = agent.inflight_human_messages;
                        turn_accepted = !backend_accepted_turn;
                        head =
                          (match !cancelled with
                          | Some _ -> None
                          | None ->
                              W.read_branch_sha ~path:worktree_path
                                ~ref_name:
                                  ("refs/heads/"
                                  ^ Types.Branch.to_string agent.branch));
                      }
                    in
                    let checkpoint =
                      Project_store.snapshot_path Env.project_name
                    in
                    Project_store.ensure_dir
                      (Stdlib.Filename.dirname checkpoint);
                    (match
                       Eio.Cancel.protect (fun () ->
                           Runtime.update_persisting runtime
                             ~persist:
                               (Persistence.save_snapshot ~path:checkpoint)
                             (fun snap ->
                               let orchestrator =
                                 Orchestrator.record_session_completion
                                   snap.Runtime.orchestrator patch_id completion
                               in
                               let orchestrator =
                                 match !cancelled with
                                 | None -> orchestrator
                                 | Some _ ->
                                     let base =
                                       Option.value agent.base_branch
                                         ~default:
                                           (Orchestrator.main_branch
                                              orchestrator)
                                     in
                                     let policy =
                                       if
                                         Orchestrator.is_integration_root
                                           orchestrator patch_id
                                       then Branch_reconcile.Preserve_ancestry
                                       else Branch_reconcile.Rewrite
                                     in
                                     fst
                                       (Orchestrator.reconcile_branch
                                          orchestrator patch_id
                                          (Branch_reconcile.Request
                                             {
                                               base =
                                                 Types.Branch.to_string base;
                                               policy;
                                               purpose =
                                                 Publish_session session_uuid;
                                             }))
                               in
                               ({ snap with Runtime.orchestrator }, ())))
                     with
                    | Ok () -> ()
                    | Error message ->
                        failwith
                          ("session completion checkpoint failed: " ^ message));
                    (match !cancelled with
                    | None -> ()
                    | Some exn ->
                        ignore (apply_result_and_emit_complete session_result);
                        raise exn);
                    let wontdo_content =
                      try
                        let ic = Stdlib.open_in_bin wontdo_path in
                        Stdlib.Fun.protect
                          ~finally:(fun () -> Stdlib.close_in_noerr ic)
                          (fun () -> Some (Stdlib.In_channel.input_all ic))
                      with Sys_error _ -> None
                    in
                    let final_head =
                      W.read_branch_sha ~path:worktree_path
                        ~ref_name:
                          ("refs/heads/" ^ Types.Branch.to_string agent.branch)
                    in
                    let has_pr =
                      Runtime.read runtime (fun snap ->
                          Patch_agent.has_pr
                            (Orchestrator.agent snap.Runtime.orchestrator
                               patch_id))
                    in
                    let initial_head_in_base =
                      match (pre_session_branch_sha, initial_base_sha) with
                      | Some initial, Some base ->
                          String.equal initial base
                          || W.is_ancestor ~path:worktree_path ~ancestor:initial
                               ~descendant:base
                      | _ -> false
                    in
                    match
                      Patch_decision.wontdo_message
                        ~initial_head:pre_session_branch_sha ~final_head
                        ~base_sha:initial_base_sha ~initial_head_in_base ~has_pr
                        ~content:wontdo_content
                    with
                    | Some message ->
                        log_event runtime ~patch_id
                          ("Patch opted out — " ^ message);
                        ignore
                          (apply_result_and_emit_complete
                             (Orchestrator.Session_wontdo message));
                        make_run_result ~turn_accepted:!backend_accepted_turn
                          `Failed (List.rev !tool_failures)
                    | None ->
                        let publication =
                          publish_completion ~write_owner ~patch_id ~agent
                            ~path:worktree_path completion
                        in
                        let push_local_sha = completion.head in
                        let branch_changed =
                          match (pre_session_branch_sha, push_local_sha) with
                          | Some before, Some after ->
                              not (String.equal before after)
                          | None, _ | _, None -> true
                        in
                        let no_commits_is_ok =
                          Patch_decision.session_no_commits_is_ok ~agent
                            ~delivery_mode ~kind
                        in
                        let final_session_result =
                          let combined =
                            Session_result.after_local_work ~delivery_mode
                              ~branch_changed
                              ~no_work:
                                (match publication with
                                | `No_work -> true
                                | `Published | `Pending -> false)
                              session_result
                          in
                          match combined with
                          | Orchestrator.Session_no_commits
                            when no_commits_is_ok ->
                              Orchestrator.Session_ok
                          | Orchestrator.Session_ok
                          | Orchestrator.Session_no_commits
                          | Orchestrator.Session_process_error _
                          | Orchestrator.Session_no_resume
                          | Orchestrator.Session_timed_out _
                          | Orchestrator.Session_failed _
                          | Orchestrator.Session_wontdo _
                          | Orchestrator.Session_give_up
                          | Orchestrator.Session_context_exhausted ->
                              combined
                        in
                        let final_user_result =
                          match final_session_result with
                          | Orchestrator.Session_ok -> (
                              match publication with
                              | `Pending -> `Retry_push
                              | `Published | `No_work -> user_result)
                          | Orchestrator.Session_no_commits -> `No_commits
                          | Orchestrator.Session_process_error _
                          | Orchestrator.Session_no_resume
                          | Orchestrator.Session_timed_out _
                          | Orchestrator.Session_failed _
                          | Orchestrator.Session_wontdo _
                          | Orchestrator.Session_give_up
                          | Orchestrator.Session_context_exhausted ->
                              `Failed
                        in
                        if
                          (not branch_changed)
                          && Orchestrator.equal_session_result session_result
                               Orchestrator.Session_ok
                        then
                          log_event runtime ~patch_id
                            (if
                               Patch_decision.equal_delivery_mode delivery_mode
                                 Start
                               && Orchestrator.equal_session_result
                                    final_session_result Orchestrator.Session_ok
                             then
                               "runner: reusing published commits for PR \
                                creation"
                             else "runner: session made no new commit");
                        ignore
                          (apply_result_and_emit_complete final_session_result);
                        make_run_result ~turn_accepted:!backend_accepted_turn
                          final_user_result (List.rev !tool_failures))))

  let run ~kind ~delivery_mode ~patch_id ~prompt ~agent ~on_pr_detected ~backend
      ~complexity =
    Runtime.with_patch_ownership Env.runtime ~patch_id (fun write_owner ->
        run_owned ~write_owner ~kind ~delivery_mode ~patch_id ~prompt ~agent
          ~on_pr_detected ~backend ~complexity)
end
