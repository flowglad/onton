open Base
open Onton_core

let gen_line_safe_char =
  QCheck2.Gen.map
    (fun c -> if Char.( = ) c '\n' || Char.( = ) c '\r' then 'x' else c)
    QCheck2.Gen.printable

let gen_line_safe_string =
  QCheck2.Gen.string_size ~gen:gen_line_safe_char (QCheck2.Gen.int_range 0 128)

let gen_int = QCheck2.Gen.int_range 0 1_000_000

let gen_usage =
  let open QCheck2.Gen in
  let* input_tokens = gen_int in
  let* output_tokens = gen_int in
  let* cache_read_tokens = gen_int in
  let* cache_write_tokens = gen_int in
  return
    Patch_agent_rpc.
      { input_tokens; output_tokens; cache_read_tokens; cache_write_tokens }

let gen_command =
  let open QCheck2.Gen in
  oneof_weighted
    [
      ( 3,
        let* request_id = gen_line_safe_string in
        let* content = gen_line_safe_string in
        return (Patch_agent_rpc.Prompt { request_id; content }) );
      ( 1,
        let* request_id = gen_line_safe_string in
        return (Patch_agent_rpc.Abort { request_id }) );
      ( 1,
        let* request_id = gen_line_safe_string in
        return (Patch_agent_rpc.Status { request_id }) );
      ( 1,
        let* request_id = gen_line_safe_string in
        return (Patch_agent_rpc.Shutdown { request_id }) );
    ]

let gen_event =
  let open QCheck2.Gen in
  oneof_weighted
    [
      ( 1,
        let* session_id = gen_line_safe_string in
        let* model_id = gen_line_safe_string in
        let* provider = gen_line_safe_string in
        return (Patch_agent_rpc.Session_init { session_id; model_id; provider })
      );
      ( 1,
        let* turn_index = gen_int in
        return (Patch_agent_rpc.Turn_started { turn_index }) );
      ( 2,
        let* delta = gen_line_safe_string in
        return (Patch_agent_rpc.Text_delta { delta }) );
      ( 2,
        let* name = gen_line_safe_string in
        let* input = gen_line_safe_string in
        let* call_id = gen_line_safe_string in
        return (Patch_agent_rpc.Tool_call { name; input; call_id }) );
      ( 2,
        let* stop_reason = gen_line_safe_string in
        let* final_text = gen_line_safe_string in
        let* usage = QCheck2.Gen.option gen_usage in
        return (Patch_agent_rpc.Done { stop_reason; final_text; usage }) );
      ( 1,
        let* code = gen_line_safe_string in
        let* message = gen_line_safe_string in
        return (Patch_agent_rpc.Error { code; message }) );
    ]

let command_to_json = function
  | Patch_agent_rpc.Prompt { request_id; content } ->
      `Assoc
        [
          ("type", `String "prompt");
          ("request_id", `String request_id);
          ("content", `String content);
        ]
  | Abort { request_id } ->
      `Assoc [ ("type", `String "abort"); ("request_id", `String request_id) ]
  | Status { request_id } ->
      `Assoc [ ("type", `String "status"); ("request_id", `String request_id) ]
  | Shutdown { request_id } ->
      `Assoc
        [ ("type", `String "shutdown"); ("request_id", `String request_id) ]

let usage_to_json (usage : Patch_agent_rpc.usage) =
  `Assoc
    [
      ("input_tokens", `Int usage.input_tokens);
      ("output_tokens", `Int usage.output_tokens);
      ("cache_read_tokens", `Int usage.cache_read_tokens);
      ("cache_write_tokens", `Int usage.cache_write_tokens);
    ]

let event_to_json = function
  | Patch_agent_rpc.Session_init { session_id; model_id; provider } ->
      `Assoc
        [
          ("type", `String "session_init");
          ("session_id", `String session_id);
          ("model_id", `String model_id);
          ("provider", `String provider);
        ]
  | Turn_started { turn_index } ->
      `Assoc
        [ ("type", `String "turn_started"); ("turn_index", `Int turn_index) ]
  | Text_delta { delta } ->
      `Assoc [ ("type", `String "text_delta"); ("delta", `String delta) ]
  | Tool_call { name; input; call_id } ->
      `Assoc
        [
          ("type", `String "tool_call");
          ("name", `String name);
          ("input", `String input);
          ("call_id", `String call_id);
        ]
  | Done { stop_reason; final_text; usage } ->
      let fields =
        [
          ("type", `String "done");
          ("stop_reason", `String stop_reason);
          ("final_text", `String final_text);
        ]
      in
      let fields =
        match usage with
        | None -> fields
        | Some usage -> fields @ [ ("usage", usage_to_json usage) ]
      in
      `Assoc fields
  | Error { code; message } ->
      `Assoc
        [
          ("type", `String "error");
          ("code", `String code);
          ("message", `String message);
        ]

let prop_commands_round_trip =
  QCheck2.Test.make ~name:"Patch_agent_rpc > round-trips every command"
    ~count:500 gen_command (fun command ->
      let line = Patch_agent_rpc.serialize_command command in
      String.is_suffix line ~suffix:"\n"
      && Int.equal (String.count line ~f:(Char.( = ) '\n')) 1
      && (not (String.exists (String.drop_suffix line 1) ~f:(Char.( = ) '\n')))
      &&
      match Yojson.Safe.from_string (String.drop_suffix line 1) with
      | exception _ -> false
      | json -> Yojson.Safe.equal json (command_to_json command))

let prop_events_round_trip =
  QCheck2.Test.make ~name:"Patch_agent_rpc > round-trips every event" ~count:500
    gen_event (fun event ->
      let line = Yojson.Safe.to_string (event_to_json event) in
      match Patch_agent_rpc.parse_event line with
      | Result.Ok parsed -> Patch_agent_rpc.equal_event parsed event
      | Result.Error _ -> false)

let prop_rejects_embedded_lf =
  QCheck2.Test.make
    ~name:"Patch_agent_rpc > rejects input containing embedded LF"
    QCheck2.Gen.unit (fun () ->
      let line =
        {|{"type":"session_init","session_id":"abc",
"model_id":"m","provider":"p"}|}
      in
      match Patch_agent_rpc.parse_event line with
      | Result.Error _ -> true
      | Result.Ok _ -> false)

let test_rejects_unknown_event_type =
  QCheck2.Test.make
    ~name:"Patch_agent_rpc > rejects input with unknown event type"
    QCheck2.Gen.unit (fun () ->
      match Patch_agent_rpc.parse_event {|{"type":"mystery"}|} with
      | Result.Error msg ->
          String.is_substring msg ~substring:"unknown event type"
      | Result.Ok _ -> false)

let test_rejects_invalid_json =
  QCheck2.Test.make
    ~name:"Patch_agent_rpc > rejects input that is not valid JSON"
    QCheck2.Gen.unit (fun () ->
      match Patch_agent_rpc.parse_event "not json" with
      | Result.Error msg -> String.is_substring msg ~substring:"invalid JSON"
      | Result.Ok _ -> false)

let () =
  let suite =
    [
      prop_commands_round_trip;
      prop_events_round_trip;
      prop_rejects_embedded_lf;
      test_rejects_unknown_event_type;
      test_rejects_invalid_json;
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true suite in
  if exit_code <> 0 then Stdlib.exit exit_code
