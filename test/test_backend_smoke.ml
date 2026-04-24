open Base
open Onton

(** Smoke integration tests for all LLM backends.

    Each test spawns [printf] to emit representative NDJSON on stdout, feeds it
    through [Llm_backend.spawn_and_stream] with the backend's parser, and
    asserts the expected [Stream_event.t] list is collected. This exercises the
    full subprocess → line-reading → parsing → callback pipeline without
    requiring any LLM CLI to be installed. *)

let smoke ?setsid_exec ~process_mgr ~clock ~cwd ~ndjson ~process_line () =
  let events = ref [] in
  let on_event ev = events := ev :: !events in
  let payload = String.concat ~sep:"\n" ndjson in
  let args = [ "printf"; "%s"; payload ] in
  let result =
    Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout:60.0 ~cwd
      ~setsid_exec ~args ~process_line ~on_event
  in
  (result, List.rev !events)

let failures = ref 0

let assert_smoke ~name ~result ~got ~expected =
  let open Llm_backend in
  (* Accept either a clean exit or any status when Final_result was seen:
     after [saw_final_result] we SIGTERM the child, so a short-lived process
     like [printf] may race and exit 143 instead of 0. Both are successful
     runs from onton's perspective. *)
  let exit_ok = result.exit_code = 0 || result.saw_final_result in
  if not exit_ok then (
    Stdio.printf "FAIL: %s exit_code=%d saw_final_result=%b\n" name
      result.exit_code result.saw_final_result;
    Int.incr failures)
  else if not result.got_events then (
    Stdio.printf "FAIL: %s got_events=false\n" name;
    Int.incr failures)
  else if not (List.equal Types.Stream_event.equal got expected) then (
    Stdio.printf "FAIL: %s event mismatch\n" name;
    Stdio.printf "  expected: %s\n"
      (List.map expected ~f:Types.Stream_event.show |> String.concat ~sep:"; ");
    Stdio.printf "  got:      %s\n"
      (List.map got ~f:Types.Stream_event.show |> String.concat ~sep:"; ");
    Int.incr failures)
  else Stdio.printf "%s: passed\n" name

let process_line_strip parse line =
  let trimmed = String.strip line in
  if String.is_empty trimmed then [] else parse trimmed

let process_line_claude line =
  let trimmed = Claude_runner.strip_ansi (String.strip line) in
  if String.is_empty trimmed then []
  else
    match Claude_runner.parse_stream_event trimmed with
    | Some ev -> [ ev ]
    | None -> []

let () =
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let cwd = Eio.Stdenv.cwd env in
  (* --- Codex --- *)
  let result, got =
    smoke ~process_mgr ~clock ~cwd
      ~ndjson:
        [
          {|{"type":"item.completed","item":{"type":"agent_message","content":[{"type":"output_text","text":"hello"}]}}|};
          {|{"type":"turn.completed"}|};
        ]
      ~process_line:(process_line_strip Codex_backend.parse_event)
      ()
  in
  assert_smoke ~name:"codex" ~result ~got
    ~expected:
      [
        Types.Stream_event.Text_delta "hello";
        Types.Stream_event.Final_result
          { text = ""; stop_reason = Types.Stop_reason.End_turn };
      ];
  (* --- Claude --- *)
  let result, got =
    smoke ~process_mgr ~clock ~cwd
      ~ndjson:
        [
          {|{"type":"content_block_delta","delta":{"type":"text_delta","text":"hello"}}|};
          {|{"type":"result","result":"done","stop_reason":"end_turn"}|};
        ]
      ~process_line:process_line_claude ()
  in
  assert_smoke ~name:"claude" ~result ~got
    ~expected:
      [
        Types.Stream_event.Text_delta "hello";
        Types.Stream_event.Final_result
          { text = "done"; stop_reason = Types.Stop_reason.End_turn };
      ];
  (* --- Pi --- *)
  let result, got =
    smoke ~process_mgr ~clock ~cwd
      ~ndjson:
        [
          {|{"type":"message_update","assistantMessageEvent":{"type":"text_delta","delta":"hello"}}|};
          {|{"type":"agent_end","messages":[]}|};
        ]
      ~process_line:(process_line_strip Pi_backend.parse_event)
      ()
  in
  assert_smoke ~name:"pi" ~result ~got
    ~expected:
      [
        Types.Stream_event.Text_delta "hello";
        Types.Stream_event.Final_result
          { text = ""; stop_reason = Types.Stop_reason.End_turn };
      ];
  (* --- OpenCode --- *)
  let result, got =
    smoke ~process_mgr ~clock ~cwd
      ~ndjson:
        [
          {|{"type":"text","part":{"type":"text","text":"hello"}}|};
          {|{"type":"step_finish","part":{"type":"step-finish","reason":"stop"}}|};
        ]
      ~process_line:(process_line_strip Opencode_backend.parse_event)
      ()
  in
  assert_smoke ~name:"opencode" ~result ~got
    ~expected:
      [
        Types.Stream_event.Text_delta "hello";
        Types.Stream_event.Final_result
          { text = ""; stop_reason = Types.Stop_reason.End_turn };
      ];
  (* --- OpenCode: tool_use status survives the full subprocess pipeline.
     Regression guard for the silent-disconnect bug where a pending Write was
     mistaken for a completed one because the parser discarded state.status. *)
  let result, got =
    smoke ~process_mgr ~clock ~cwd
      ~ndjson:
        [
          {|{"type":"tool_use","part":{"type":"tool","tool":"write","state":{"status":"pending","input":{"filePath":"/tmp/x","content":"y"}}}}|};
          {|{"type":"step_finish","part":{"type":"step-finish","reason":"stop"}}|};
        ]
      ~process_line:(process_line_strip Opencode_backend.parse_event)
      ()
  in
  assert_smoke ~name:"opencode tool_use pending" ~result ~got
    ~expected:
      [
        Types.Stream_event.Tool_use
          {
            name = "Write";
            input = {|{"file_path":"/tmp/x","content":"y"}|};
            status = Some "pending";
          };
        Types.Stream_event.Final_result
          { text = ""; stop_reason = Types.Stop_reason.End_turn };
      ];
  (* --- Early exit on Final_result: the subprocess emits a terminal event
     and then sleeps. spawn_and_stream must return without waiting the full
     sleep, [saw_final_result] must be true, and [timed_out] must be false. *)
  let early_exit_test () =
    let payload =
      {|{"type":"result","result":"done","stop_reason":"end_turn"}|}
    in
    let events = ref [] in
    let on_event ev = events := ev :: !events in
    (* exec replaces sh with sleep, so the signal lands directly on
       sleep rather than on a shell waiting for its child. Otherwise
       SIGTERM would target sh and leave sleep orphaned until kernel
       reparenting, which slows teardown on loaded machines. *)
    let args =
      [ "sh"; "-c"; Printf.sprintf "printf '%s\\n'; exec sleep 30" payload ]
    in
    let started = Unix.gettimeofday () in
    let result =
      Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout:30.0 ~cwd
        ~setsid_exec:None ~args ~process_line:process_line_claude ~on_event
    in
    let elapsed = Unix.gettimeofday () -. started in
    if not result.Llm_backend.saw_final_result then (
      Stdio.printf "FAIL: early-exit saw_final_result=false\n";
      Int.incr failures)
    else if result.Llm_backend.timed_out then (
      Stdio.printf "FAIL: early-exit timed_out=true\n";
      Int.incr failures)
    else if Float.(elapsed > 5.0) then (
      Stdio.printf "FAIL: early-exit took %.2fs (expected < 5s)\n" elapsed;
      Int.incr failures)
    else Stdio.printf "early exit on Final_result: passed (%.2fs)\n" elapsed
  in
  early_exit_test ();
  (* --- Timeout regression: a subprocess that never emits Final_result
     still times out on schedule. *)
  let timeout_test () =
    let events = ref [] in
    let on_event ev = events := ev :: !events in
    let args = [ "sh"; "-c"; "sleep 30" ] in
    let started = Unix.gettimeofday () in
    let result =
      Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout:1.0 ~cwd
        ~setsid_exec:None ~args ~process_line:process_line_claude ~on_event
    in
    let elapsed = Unix.gettimeofday () -. started in
    if not result.Llm_backend.timed_out then (
      Stdio.printf "FAIL: timeout test timed_out=false\n";
      Int.incr failures)
    else if result.Llm_backend.saw_final_result then (
      Stdio.printf "FAIL: timeout test saw_final_result=true\n";
      Int.incr failures)
    else if Float.(elapsed > 5.0) then (
      Stdio.printf "FAIL: timeout test took %.2fs (expected < 5s)\n" elapsed;
      Int.incr failures)
    else Stdio.printf "timeout regression: passed (%.2fs)\n" elapsed
  in
  timeout_test ();
  (* --- Grandchild reap: with the setsid shim, killing the child on
     Final_result takes the whole process group with it. Without the shim
     a backgrounded [sleep] would reparent to PID 1 and outlive us. The
     subprocess embeds its grandchild's pid in a text_delta so we can
     probe for it after spawn_and_stream returns. *)
  let grandchild_reap_test () =
    match Stdlib.Sys.getenv_opt "ONTON_SETSID_EXEC" with
    | None | Some "" ->
        Stdio.printf
          "grandchild reap: SKIPPED (ONTON_SETSID_EXEC not set — dune run only)\n"
    | Some shim -> (
        let grand_pid_ref = ref None in
        let on_event ev =
          match ev with
          | Types.Stream_event.Text_delta t -> (
              match String.chop_prefix t ~prefix:"GRAND:" with
              | Some s -> (
                  match Int.of_string_opt (String.strip s) with
                  | Some pid -> grand_pid_ref := Some pid
                  | None -> ())
              | None -> ())
          | Types.Stream_event.Tool_use _ | Types.Stream_event.Final_result _
          | Types.Stream_event.Error _ | Types.Stream_event.Session_init _ ->
              ()
        in
        let script =
          {|sleep 30 &
GRAND=$!
printf '{"type":"content_block_delta","delta":{"type":"text_delta","text":"GRAND:%d"}}\n' "$GRAND"
printf '{"type":"result","result":"done","stop_reason":"end_turn"}\n'
# Don't wait — exit immediately so sleep is orphaned unless the shim's
# setsid lets the parent's SIGKILL sweep it up.
exit 0|}
        in
        let args = [ "sh"; "-c"; script ] in
        let result =
          Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout:30.0 ~cwd
            ~setsid_exec:(Some shim) ~args ~process_line:process_line_claude
            ~on_event
        in
        if not result.Llm_backend.saw_final_result then (
          Stdio.printf "FAIL: grandchild reap: Final_result not seen\n";
          Int.incr failures)
        else
          match !grand_pid_ref with
          | None ->
              Stdio.printf
                "FAIL: grandchild reap: grandchild pid not captured\n";
              Int.incr failures
          | Some pid ->
              (* Give the kernel a beat to deliver SIGKILL and reap. *)
              Unix.sleepf 0.2;
              let alive =
                try
                  Unix.kill pid 0;
                  true
                with Unix.Unix_error (Unix.ESRCH, _, _) -> false
              in
              if alive then (
                Stdio.printf
                  "FAIL: grandchild reap: pid %d still alive after \
                   spawn_and_stream returned\n"
                  pid;
                (try Unix.kill pid Stdlib.Sys.sigkill with _ -> ());
                Int.incr failures)
              else Stdio.printf "grandchild reap: passed (pid %d reaped)\n" pid)
  in
  grandchild_reap_test ();
  if !failures > 0 then (
    Stdio.printf "%d backend smoke test(s) failed\n" !failures;
    Stdlib.exit 1)
  else Stdio.printf "all backend smoke tests passed\n"
