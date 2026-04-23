open Base
open Onton

(** Smoke integration tests for all LLM backends.

    Each test spawns [printf] to emit representative NDJSON on stdout, feeds it
    through [Llm_backend.spawn_and_stream] with the backend's parser, and
    asserts the expected [Stream_event.t] list is collected. This exercises the
    full subprocess → line-reading → parsing → callback pipeline without
    requiring any LLM CLI to be installed. *)

let smoke ~process_mgr ~clock ~cwd ~ndjson ~process_line =
  let events = ref [] in
  let on_event ev = events := ev :: !events in
  let payload = String.concat ~sep:"\n" ndjson in
  let args = [ "printf"; "%s"; payload ] in
  let result =
    Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout:60.0 ~cwd ~args
      ~process_line ~on_event
  in
  (result, List.rev !events)

let failures = ref 0

let assert_smoke ~name ~result ~got ~expected =
  let open Llm_backend in
  if result.exit_code <> 0 then (
    Stdio.printf "FAIL: %s exit_code=%d\n" name result.exit_code;
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
      ~process_line:process_line_claude
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
  if !failures > 0 then (
    Stdio.printf "%d backend smoke test(s) failed\n" !failures;
    Stdlib.exit 1)
  else Stdio.printf "all backend smoke tests passed\n"
