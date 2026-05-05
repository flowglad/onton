open Base

(** Pure NDJSON event parser for [codex exec --json] output.

    Decodes one JSON line at a time, threading [Codex_cost.cost_state] through
    [turn.completed] events so that pricing decisions are made co-temporally
    with the events the parser emits. The effectful streaming driver lives in
    [Codex_backend.run_streaming]; everything testable lives here. *)

let parse_event_with_cost_tracking ~model ~budget_cap_nano_usd ~cost_state line
    =
  match Yojson.Safe.from_string line with
  | json -> (
      let open Yojson.Safe.Util in
      let typ = member "type" json |> to_string_option in
      match typ with
      | Some "item.completed" -> (
          let item = member "item" json in
          let item_type = member "type" item |> to_string_option in
          match item_type with
          | Some "agent_message" ->
              let text =
                match member "text" item |> to_string_option with
                | Some t -> t
                | None ->
                    let parts =
                      match member "content" item with
                      | `List l ->
                          List.filter_map l ~f:(fun block ->
                              match member "type" block |> to_string_option with
                              | Some "output_text" ->
                                  member "text" block |> to_string_option
                              | _ -> None)
                      | _ -> []
                    in
                    String.concat ~sep:"" parts
              in
              let events =
                if String.is_empty text then []
                else [ Types.Stream_event.Text_delta text ]
              in
              (events, cost_state)
          | Some "command_execution" -> ([], cost_state)
          | _ -> ([], cost_state))
      | Some "item.started" -> (
          let item = member "item" json in
          let item_type = member "type" item |> to_string_option in
          match item_type with
          | Some "command_execution" -> (
              match member "command" item |> to_string_option with
              | Some cmd when not (String.is_empty cmd) ->
                  let input =
                    Yojson.Safe.to_string (`Assoc [ ("command", `String cmd) ])
                  in
                  ( [
                      Types.Stream_event.Tool_use
                        { name = "Bash"; input; status = None };
                    ],
                    cost_state )
              | _ -> ([], cost_state))
          | _ -> ([], cost_state))
      | Some "thread.started" -> (
          match member "thread_id" json |> to_string_option with
          | Some id when not (String.is_empty id) ->
              ( [ Types.Stream_event.Session_init { session_id = id } ],
                cost_state )
          | _ -> ([], cost_state))
      | Some "turn.started" -> ([ Types.Stream_event.Turn_started ], cost_state)
      | Some "turn.completed" ->
          let { Codex_cost.events; state } =
            Codex_cost.on_turn_completed ~model ~budget_cap_nano_usd
              ~state:cost_state json
          in
          (events, state)
      | Some "error" ->
          let msg =
            member "message" json |> to_string_option
            |> Option.value ~default:"unknown codex error"
          in
          ([ Types.Stream_event.Error msg ], cost_state)
      | _ -> ([], cost_state))
  | exception Yojson.Json_error _ -> ([], cost_state)
  | exception Yojson.Safe.Util.Type_error _ -> ([], cost_state)

let build_args ~model ~cwd_path ~prompt ~resume_session =
  let model_args =
    match model with
    | Some m when not (String.is_empty m) -> [ "-m"; m ]
    | _ -> []
  in
  let global = [ "codex"; "-C"; cwd_path ] in
  let trailing = [ "--dangerously-bypass-approvals-and-sandbox" ] in
  match resume_session with
  | Some session_id ->
      global
      @ [ "exec"; "resume"; session_id; prompt; "--json" ]
      @ model_args @ trailing
  | None -> global @ [ "exec"; prompt; "--json" ] @ model_args @ trailing

let parse_event (line : string) : Types.Stream_event.t list =
  fst
    (parse_event_with_cost_tracking ~model:None ~budget_cap_nano_usd:None
       ~cost_state:Codex_cost.initial_cost_state line)

let auto_model ~complexity =
  match complexity with
  | Some 1 -> Some "gpt-5.4-mini"
  | Some 2 -> Some "gpt-5.4"
  | Some 3 -> Some "gpt-5.5"
  | Some _ | None -> Some "gpt-5.5"

let%test "build_args fresh (no resume, no model)" =
  let args =
    build_args ~model:None ~cwd_path:"/tmp/work" ~prompt:"do stuff"
      ~resume_session:None
  in
  List.equal String.equal args
    [
      "codex";
      "-C";
      "/tmp/work";
      "exec";
      "do stuff";
      "--json";
      "--dangerously-bypass-approvals-and-sandbox";
    ]

let%test "build_args fresh with model" =
  let args =
    build_args ~model:(Some "gpt-5-mini") ~cwd_path:"/tmp/work"
      ~prompt:"do stuff" ~resume_session:None
  in
  List.equal String.equal args
    [
      "codex";
      "-C";
      "/tmp/work";
      "exec";
      "do stuff";
      "--json";
      "-m";
      "gpt-5-mini";
      "--dangerously-bypass-approvals-and-sandbox";
    ]

let%test "build_args with resume session passes prompt and bypass flag" =
  let args =
    build_args ~model:None ~cwd_path:"/tmp/work" ~prompt:"do stuff"
      ~resume_session:(Some "sess-1")
  in
  List.equal String.equal args
    [
      "codex";
      "-C";
      "/tmp/work";
      "exec";
      "resume";
      "sess-1";
      "do stuff";
      "--json";
      "--dangerously-bypass-approvals-and-sandbox";
    ]

let%test "build_args with resume session and model" =
  let args =
    build_args ~model:(Some "gpt-5-mini") ~cwd_path:"/tmp/work"
      ~prompt:"do stuff" ~resume_session:(Some "sess-1")
  in
  List.equal String.equal args
    [
      "codex";
      "-C";
      "/tmp/work";
      "exec";
      "resume";
      "sess-1";
      "do stuff";
      "--json";
      "-m";
      "gpt-5-mini";
      "--dangerously-bypass-approvals-and-sandbox";
    ]

let%test "parse_event thread.started emits Session_init" =
  let line =
    {|{"type":"thread.started","thread_id":"019d91e0-5731-7182-ac68-19922a243e95"}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Session_init
        { session_id = "019d91e0-5731-7182-ac68-19922a243e95" };
    ]

let%test "parse_event thread.started without thread_id is ignored" =
  let line = {|{"type":"thread.started"}|} in
  List.is_empty (parse_event line)

let%test "parse_event turn.started emits Turn_started" =
  let line = {|{"type":"turn.started"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Turn_started ]

let%test "parse_event agent_message (content-array schema)" =
  let line =
    {|{"type":"item.completed","item":{"type":"agent_message","content":[{"type":"output_text","text":"hello"}]}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event agent_message (modern text-field schema)" =
  let line =
    {|{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"hello"}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event command_execution started encodes input as JSON" =
  let line =
    {|{"type":"item.started","item":{"type":"command_execution","command":"ls -la"}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        { name = "Bash"; input = {|{"command":"ls -la"}|}; status = None };
    ]

let%test "parse_event command_execution input survives JSON parse + extract" =
  let line =
    {|{"type":"item.started","item":{"type":"command_execution","command":"echo \"hi\" && ls"}}|}
  in
  let expected =
    Yojson.Safe.to_string (`Assoc [ ("command", `String {|echo "hi" && ls|}) ])
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        { name = "Bash"; input = expected; status = None };
    ]

let%test "parse_event command_execution completed is ignored" =
  let line =
    {|{"type":"item.completed","item":{"type":"command_execution","command":"ls -la"}}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event turn.completed" =
  let line = {|{"type":"turn.completed"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Final_result
        { text = ""; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "cost tracker emits Error when cumulative exceeds cap" =
  let line =
    {|{"type":"turn.completed","usage":{"input_tokens":1000,"output_tokens":2000,"output_tokens_details":{"reasoning_tokens":500}}}|}
  in
  let events, cost_state =
    parse_event_with_cost_tracking ~model:(Some "gpt-5.5")
      ~budget_cap_nano_usd:(Some 50_000_000L)
      ~cost_state:Codex_cost.initial_cost_state line
  in
  Int64.(cost_state.cumulative_nano_usd > 50_000_000L)
  && List.equal Types.Stream_event.equal events
       [
         Types.Stream_event.Error
           "Codex budget cap exceeded: cumulative cost $0.0650 > cap $0.0500";
         Types.Stream_event.Final_result
           { text = ""; stop_reason = Types.Stop_reason.End_turn };
       ]

let%test "cost tracker handles usage without output_tokens_details" =
  let line =
    {|{"type":"turn.completed","usage":{"input_tokens":1000,"output_tokens":2000}}|}
  in
  let events, _ =
    parse_event_with_cost_tracking ~model:(Some "gpt-5.5")
      ~budget_cap_nano_usd:None ~cost_state:Codex_cost.initial_cost_state line
  in
  List.equal Types.Stream_event.equal events
    [
      Types.Stream_event.Final_result
        { text = ""; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "parse_event error" =
  let line = {|{"type":"error","message":"rate limited"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Error "rate limited" ]

let%test "parse_event invalid json" =
  List.is_empty (parse_event "not json at all")

let%test "parse_event unknown type" =
  List.is_empty (parse_event {|{"type":"ping"}|})
