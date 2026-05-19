open Base

(** Pure stream-json event parser, ANSI scrubber, and CLI-arg builders for
    [claude]. The effectful streaming driver lives in [Claude_runner]; the pure
    side is fully testable without subprocesses. *)

(** Compiled regex for ANSI escape sequences, carriage returns, and stray
    control characters. Defense-in-depth so any incidental control bytes in
    Claude's stream-json output don't break JSON parsing. Matches:
    - CSI sequences (ESC\[ ... letter)
    - OSC sequences (ESC\] ... BEL/ST)
    - Bare \r
    - C0 control characters (0x00–0x1F except \n and \t). *)
let ansi_re =
  let esc = Re.char '\x1b' in
  let csi =
    Re.seq [ esc; Re.set "[("; Re.rep (Re.set "0123456789;?"); Re.rg 'A' 'z' ]
  in
  let osc =
    Re.seq
      [
        esc;
        Re.set "]>";
        Re.rep (Re.compl [ Re.char '\x07'; Re.char '\x1b' ]);
        Re.opt (Re.alt [ Re.char '\x07'; Re.seq [ esc; Re.char '\\' ] ]);
      ]
  in
  let cr = Re.char '\r' in
  let c0 =
    Re.alt
      [
        Re.rg '\x00' '\x08';
        Re.rg '\x0b' '\x0c';
        Re.rg '\x0e' '\x1a';
        Re.rg '\x1c' '\x1f';
      ]
  in
  Re.compile (Re.alt [ csi; osc; cr; c0 ])

let strip_ansi s = Re.replace_string ansi_re ~by:"" s

let auto_model ~complexity =
  match complexity with
  | Some 1 -> Some "haiku"
  | Some 2 -> Some "sonnet"
  | Some 3 -> Some "opus"
  | Some _ | None -> Some "opus"

let model_args = function
  | Some m when not (String.is_empty m) -> [ "--model"; m ]
  | _ -> []

let max_turns_for ~complexity =
  match complexity with Some 1 -> 50 | Some 2 -> 100 | _ -> 200

let budget_cap_args ~getenv_opt ~warn () =
  match
    getenv_opt "ONTON_BUDGET_CAP_USD"
    |> Option.map ~f:(fun value -> String.strip value)
  with
  | None | Some "" -> []
  | Some raw -> (
      match Float.of_string_opt raw with
      | Some cap when Float.(cap > 0.) -> [ "--max-budget-usd"; raw ]
      | Some _ -> []
      | None ->
          warn
            (Printf.sprintf
               "warning: ignoring invalid ONTON_BUDGET_CAP_USD=%S; expected a \
                positive numeric USD cap"
               raw);
          [])

let bare_args = [ "--bare" ]

let build_args ~getenv_opt ~warn ~model ~complexity ~prompt ~resume_session =
  let base = [ "claude" ] in
  let prompt_args = [ "-p"; prompt; "--output-format"; "text" ] in
  let session_args =
    match resume_session with Some id -> [ "--resume"; id ] | None -> []
  in
  let flags =
    [
      "--dangerously-skip-permissions";
      "--max-turns";
      Int.to_string (max_turns_for ~complexity);
      "--exclude-dynamic-system-prompt-sections";
    ]
    @ budget_cap_args ~getenv_opt ~warn ()
    @ bare_args
  in
  base @ model_args model @ prompt_args @ session_args @ flags

let build_stream_args ~getenv_opt ~warn ~model ~complexity ~prompt
    ~minted_session_id ~resume_session =
  (match (minted_session_id, resume_session) with
  | Some _, Some _ ->
      invalid_arg
        "Claude_event_parser.build_stream_args: minted_session_id and \
         resume_session are mutually exclusive"
  | _ -> ());
  let base = [ "claude" ] in
  let prompt_args =
    [ "-p"; prompt; "--output-format"; "stream-json"; "--verbose" ]
  in
  let minted_session_args =
    match minted_session_id with
    | Some id -> [ "--session-id"; id ]
    | None -> []
  in
  let session_args =
    match resume_session with Some id -> [ "--resume"; id ] | None -> []
  in
  let flags =
    [
      "--dangerously-skip-permissions";
      "--max-turns";
      Int.to_string (max_turns_for ~complexity);
      "--exclude-dynamic-system-prompt-sections";
    ]
    @ budget_cap_args ~getenv_opt ~warn ()
    @ bare_args
  in
  base @ model_args model @ minted_session_args @ prompt_args @ session_args
  @ flags

(** Find the first '\{' in [s] and return the substring starting there. Defense
    against any leading garbage in a stream-json line. *)
let find_json_start s =
  match String.lfindi s ~f:(fun _ c -> Char.equal c '{') with
  | Some 0 | None -> s
  | Some n -> String.drop_prefix s n

let session_init_of_json json =
  let open Yojson.Safe.Util in
  match member "session_id" json |> to_string_option with
  | None -> []
  | Some session_id ->
      [
        Types.Stream_event.Session_init
          {
            session_id;
            api_key_source = member "apiKeySource" json |> to_string_option;
            model = member "model" json |> to_string_option;
            claude_code_version =
              member "claude_code_version" json |> to_string_option;
            permission_mode = member "permissionMode" json |> to_string_option;
          };
      ]

let parse_stream_events (line : string) : Types.Stream_event.t list =
  try
    let json = Yojson.Safe.from_string (find_json_start line) in
    let open Yojson.Safe.Util in
    let typ = member "type" json |> to_string_option in
    match typ with
    | Some "content_block_delta" -> (
        let delta = member "delta" json in
        let delta_type = member "type" delta |> to_string_option in
        match delta_type with
        | Some "text_delta" ->
            let text =
              member "text" delta |> to_string_option
              |> Option.value ~default:""
            in
            [ Types.Stream_event.Text_delta text ]
        | _ -> [])
    | Some "content_block_start" -> (
        let content_block = member "content_block" json in
        let block_type = member "type" content_block |> to_string_option in
        match block_type with
        | Some "tool_use" ->
            let name =
              member "name" content_block
              |> to_string_option |> Option.value ~default:""
            in
            [ Types.Stream_event.Tool_use { name; input = ""; status = None } ]
        | _ -> [])
    | Some "assistant" ->
        let content =
          match member "message" json |> member "content" with
          | `List l -> l
          | _ -> []
        in
        List.filter_map content ~f:(fun block ->
            let block_type = member "type" block |> to_string_option in
            match block_type with
            | Some "tool_use" ->
                let name =
                  member "name" block |> to_string_option
                  |> Option.value ~default:""
                in
                let input =
                  match member "input" block with
                  | `Null -> ""
                  | v -> Yojson.Safe.to_string v
                in
                Some
                  (Types.Stream_event.Tool_use { name; input; status = None })
            | Some "text" ->
                let text =
                  member "text" block |> to_string_option
                  |> Option.value ~default:""
                in
                Some (Types.Stream_event.Text_delta text)
            | _ -> None)
    | Some "message_delta" -> (
        let delta = member "delta" json in
        let raw_reason =
          member "stop_reason" delta |> to_string_option
          |> Option.value ~default:""
        in
        match Types.Stop_reason.of_string raw_reason with
        | None -> []
        | Some stop_reason ->
            [ Types.Stream_event.Final_result { text = ""; stop_reason } ])
    | Some "result" ->
        let is_error =
          member "is_error" json |> to_bool_option
          |> Option.value ~default:false
        in
        let session_init = session_init_of_json json in
        if is_error then
          let errors =
            match member "errors" json with
            | `List items ->
                List.filter_map items ~f:(fun item -> to_string_option item)
            | _ -> []
          in
          let result_text =
            member "result" json |> to_string_option
            |> Option.map ~f:String.strip
          in
          let msg =
            match errors with
            | _ :: _ as errs -> String.concat ~sep:"; " errs
            | [] -> (
                match result_text with
                | Some text when not (String.is_empty text) -> text
                | _ -> "unknown error")
          in
          session_init @ [ Types.Stream_event.Error msg ]
        else
          let text =
            member "result" json |> to_string_option |> Option.value ~default:""
          in
          let raw_reason =
            member "stop_reason" json |> to_string_option
            |> Option.value ~default:"end_turn"
          in
          let stop_reason =
            Types.Stop_reason.of_string raw_reason
            |> Option.value ~default:Types.Stop_reason.End_turn
          in
          session_init
          @ [ Types.Stream_event.Final_result { text; stop_reason } ]
    | Some "error" ->
        let err =
          member "error" json |> member "message" |> to_string_option
          |> Option.value ~default:"unknown error"
        in
        [ Types.Stream_event.Error err ]
    | Some "system" -> (
        let subtype = member "subtype" json |> to_string_option in
        match subtype with
        | Some "init" -> session_init_of_json json
        | _ -> [])
    | _ -> []
  with
  | Yojson.Json_error _ -> []
  | Yojson.Safe.Util.Type_error _ -> []

let parse_stream_event (line : string) : Types.Stream_event.t option =
  match parse_stream_events line with [] -> None | e :: _ -> Some e

(* ─────────────────────────────────────────────────────────────────────────
   Tests
   ───────────────────────────────────────────────────────────────────────── *)

let no_env _ = None
let ignore_warn _ = ()

let%test "auto_model: complexity 1 -> haiku" =
  Option.equal String.equal (auto_model ~complexity:(Some 1)) (Some "haiku")

let%test "auto_model: complexity 2 -> sonnet" =
  Option.equal String.equal (auto_model ~complexity:(Some 2)) (Some "sonnet")

let%test "auto_model: complexity 3 -> opus" =
  Option.equal String.equal (auto_model ~complexity:(Some 3)) (Some "opus")

let%test "auto_model: None -> opus (conservative)" =
  Option.equal String.equal (auto_model ~complexity:None) (Some "opus")

let%test "auto_model: out-of-range -> opus (conservative)" =
  Option.equal String.equal (auto_model ~complexity:(Some 7)) (Some "opus")

let%test "max_turns_for: complexity 1 -> 50" =
  Int.equal (max_turns_for ~complexity:(Some 1)) 50

let%test "max_turns_for: complexity 2 -> 100" =
  Int.equal (max_turns_for ~complexity:(Some 2)) 100

let%test "max_turns_for: complexity 3 -> 200" =
  Int.equal (max_turns_for ~complexity:(Some 3)) 200

let%test "max_turns_for: None -> 200" =
  Int.equal (max_turns_for ~complexity:None) 200

let%test "build_args fresh (no resume, with model)" =
  let args =
    build_args ~getenv_opt:no_env ~warn:ignore_warn ~model:(Some "sonnet")
      ~complexity:(Some 2) ~prompt:"do stuff" ~resume_session:None
  in
  List.equal String.equal args
    [
      "claude";
      "--model";
      "sonnet";
      "-p";
      "do stuff";
      "--output-format";
      "text";
      "--dangerously-skip-permissions";
      "--max-turns";
      "100";
      "--exclude-dynamic-system-prompt-sections";
      "--bare";
    ]

let%test "build_args fresh (no resume, no model)" =
  let args =
    build_args ~getenv_opt:no_env ~warn:ignore_warn ~model:None
      ~complexity:(Some 1) ~prompt:"do stuff" ~resume_session:None
  in
  List.equal String.equal args
    [
      "claude";
      "-p";
      "do stuff";
      "--output-format";
      "text";
      "--dangerously-skip-permissions";
      "--max-turns";
      "50";
      "--exclude-dynamic-system-prompt-sections";
      "--bare";
    ]

let%test "build_args with resume session" =
  let args =
    build_args ~getenv_opt:no_env ~warn:ignore_warn ~model:(Some "opus")
      ~complexity:(Some 3) ~prompt:"do stuff" ~resume_session:(Some "abc-123")
  in
  List.equal String.equal args
    [
      "claude";
      "--model";
      "opus";
      "-p";
      "do stuff";
      "--output-format";
      "text";
      "--resume";
      "abc-123";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
      "--exclude-dynamic-system-prompt-sections";
      "--bare";
    ]

let%test "build_args includes --exclude-dynamic-system-prompt-sections" =
  let args =
    build_args ~getenv_opt:no_env ~warn:ignore_warn ~model:None ~complexity:None
      ~prompt:"do stuff" ~resume_session:None
  in
  List.mem args "--exclude-dynamic-system-prompt-sections" ~equal:String.equal

let%test "build_args includes --bare" =
  List.mem
    (build_args ~getenv_opt:no_env ~warn:ignore_warn ~model:None
       ~complexity:None ~prompt:"do stuff" ~resume_session:None)
    "--bare" ~equal:String.equal

let%test "build_stream_args fresh (no resume, with model)" =
  let args =
    build_stream_args ~getenv_opt:no_env ~warn:ignore_warn
      ~model:(Some "sonnet") ~complexity:(Some 2) ~prompt:"do stuff"
      ~minted_session_id:None ~resume_session:None
  in
  List.equal String.equal args
    [
      "claude";
      "--model";
      "sonnet";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--verbose";
      "--dangerously-skip-permissions";
      "--max-turns";
      "100";
      "--exclude-dynamic-system-prompt-sections";
      "--bare";
    ]

let%test "build_stream_args fresh (no resume, no model)" =
  let args =
    build_stream_args ~getenv_opt:no_env ~warn:ignore_warn ~model:None
      ~complexity:None ~prompt:"do stuff" ~minted_session_id:None
      ~resume_session:None
  in
  List.equal String.equal args
    [
      "claude";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--verbose";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
      "--exclude-dynamic-system-prompt-sections";
      "--bare";
    ]

let%test "build_stream_args with resume session" =
  let args =
    build_stream_args ~getenv_opt:no_env ~warn:ignore_warn ~model:(Some "opus")
      ~complexity:(Some 1) ~prompt:"do stuff" ~minted_session_id:None
      ~resume_session:(Some "abc-123")
  in
  List.equal String.equal args
    [
      "claude";
      "--model";
      "opus";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--verbose";
      "--resume";
      "abc-123";
      "--dangerously-skip-permissions";
      "--max-turns";
      "50";
      "--exclude-dynamic-system-prompt-sections";
      "--bare";
    ]

let%test "build_stream_args includes --exclude-dynamic-system-prompt-sections" =
  let args =
    build_stream_args ~getenv_opt:no_env ~warn:ignore_warn ~model:None
      ~complexity:None ~prompt:"do stuff" ~minted_session_id:None
      ~resume_session:None
  in
  List.mem args "--exclude-dynamic-system-prompt-sections" ~equal:String.equal

let%test "build_stream_args emits --session-id when minted_session_id is Some" =
  let args =
    build_stream_args ~getenv_opt:no_env ~warn:ignore_warn
      ~model:(Some "sonnet") ~complexity:None ~prompt:"do stuff"
      ~minted_session_id:(Some "123e4567-e89b-42d3-a456-426614174000")
      ~resume_session:None
  in
  List.equal String.equal args
    [
      "claude";
      "--model";
      "sonnet";
      "--session-id";
      "123e4567-e89b-42d3-a456-426614174000";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--verbose";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
      "--exclude-dynamic-system-prompt-sections";
      "--bare";
    ]

let%test "build_stream_args rejects session-id plus resume together" =
  match
    build_stream_args ~getenv_opt:no_env ~warn:ignore_warn ~model:None
      ~complexity:None ~prompt:"do stuff" ~minted_session_id:(Some "minted")
      ~resume_session:(Some "resume")
  with
  | _ -> false
  | exception Invalid_argument _ -> true

let%test "build_stream_args includes --bare" =
  List.mem
    (build_stream_args ~getenv_opt:no_env ~warn:ignore_warn ~model:None
       ~complexity:None ~prompt:"do stuff" ~minted_session_id:None
       ~resume_session:None)
    "--bare" ~equal:String.equal

let%test
    "build_stream_args emits --max-budget-usd when ONTON_BUDGET_CAP_USD set" =
  let getenv_opt = function "ONTON_BUDGET_CAP_USD" -> Some "10" | _ -> None in
  let args =
    build_stream_args ~getenv_opt ~warn:ignore_warn ~model:(Some "sonnet")
      ~complexity:None ~prompt:"do stuff" ~minted_session_id:None
      ~resume_session:None
  in
  List.equal String.equal args
    [
      "claude";
      "--model";
      "sonnet";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--verbose";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
      "--exclude-dynamic-system-prompt-sections";
      "--max-budget-usd";
      "10";
      "--bare";
    ]

let%test "budget_cap_args omits flag and warns on invalid cap" =
  let getenv_opt = function
    | "ONTON_BUDGET_CAP_USD" -> Some "not-a-number"
    | _ -> None
  in
  let warnings = ref [] in
  let warn msg = warnings := msg :: !warnings in
  let args = budget_cap_args ~getenv_opt ~warn () in
  List.is_empty args
  && List.equal String.equal !warnings
       [
         "warning: ignoring invalid ONTON_BUDGET_CAP_USD=\"not-a-number\"; \
          expected a positive numeric USD cap";
       ]

let%test "strip_ansi removes escape sequences" =
  String.equal (strip_ansi "\027[31mhello\027[0m") "hello"

let%test "strip_ansi passes clean text through" =
  String.equal (strip_ansi "hello world") "hello world"

let%test "strip_ansi removes backspaces" =
  String.equal (strip_ansi "\x08\x08hello") "hello"

let%test "strip_ansi removes NUL and other C0 chars" =
  String.equal (strip_ansi "\x04\x00\x01hello") "hello"

let%test "parse_stream_event text_delta" =
  let line =
    {|{"type":"content_block_delta","delta":{"type":"text_delta","text":"hello"}}|}
  in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some (Types.Stream_event.Text_delta "hello"))

let%test "parse_stream_event tool_use" =
  let line =
    {|{"type":"content_block_start","content_block":{"type":"tool_use","name":"write_file"}}|}
  in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some
       (Types.Stream_event.Tool_use
          { name = "write_file"; input = ""; status = None }))

let%test "parse_stream_event error" =
  let line = {|{"type":"error","error":{"message":"rate limited"}}|} in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some (Types.Stream_event.Error "rate limited"))

let%test "parse_stream_event result" =
  let line = {|{"type":"result","result":"done","stop_reason":"end_turn"}|} in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some
       (Types.Stream_event.Final_result
          { text = "done"; stop_reason = Types.Stop_reason.End_turn }))

let%test "parse_stream_event message_delta with stop_reason" =
  let line = {|{"type":"message_delta","delta":{"stop_reason":"end_turn"}}|} in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some
       (Types.Stream_event.Final_result
          { text = ""; stop_reason = Types.Stop_reason.End_turn }))

let%test "parse_stream_event error result" =
  let line =
    {|{"type":"result","subtype":"error_during_execution","is_error":true,"errors":["bad session ID"]}|}
  in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some (Types.Stream_event.Error "bad session ID"))

let%test "parse_stream_event result with empty errors uses result text" =
  let line =
    {|{"type":"result","is_error":true,"errors":[],"result":"auth unavailable"}|}
  in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some (Types.Stream_event.Error "auth unavailable"))

let%test
    "parse_stream_event result with non-empty errors prefers errors array" =
  let line =
    {|{"type":"result","is_error":true,"errors":["first","second"],"result":"ignored"}|}
  in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some (Types.Stream_event.Error "first; second"))

let%test
    "parse_stream_event result with both absent falls back to unknown error" =
  let line = {|{"type":"result","is_error":true,"errors":[]}|} in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some (Types.Stream_event.Error "unknown error"))

let%test "parse_stream_event invalid json returns None" =
  Option.is_none (parse_stream_event "not json at all")

let%test "parse_stream_event unknown type returns None" =
  Option.is_none (parse_stream_event {|{"type":"ping"}|})

let%test "parse_stream_events extracts session_id from system/init" =
  let line =
    {|{"type":"system","subtype":"init","session_id":"d3c2d71e-0399-4ed3-810a-9c1edce7dd00","tools":[]}|}
  in
  List.equal Types.Stream_event.equal (parse_stream_events line)
    [
      Types.Stream_event.Session_init
        {
          session_id = "d3c2d71e-0399-4ed3-810a-9c1edce7dd00";
          api_key_source = None;
          model = None;
          claude_code_version = None;
          permission_mode = None;
        };
    ]

let%test
    "parse_stream_events Session_init carries \
     apiKeySource/model/version/permissionMode" =
  let line =
    {|{"type":"system","subtype":"init","session_id":"abc-123","apiKeySource":"project","model":"sonnet","claude_code_version":"2.1.144","permissionMode":"bypassPermissions"}|}
  in
  List.equal Types.Stream_event.equal (parse_stream_events line)
    [
      Types.Stream_event.Session_init
        {
          session_id = "abc-123";
          api_key_source = Some "project";
          model = Some "sonnet";
          claude_code_version = Some "2.1.144";
          permission_mode = Some "bypassPermissions";
        };
    ]

let%test "parse_stream_events ignores system events without init subtype" =
  let line = {|{"type":"system","subtype":"heartbeat"}|} in
  List.is_empty (parse_stream_events line)

let%test "parse_stream_events ignores system/init without session_id" =
  let line = {|{"type":"system","subtype":"init"}|} in
  List.is_empty (parse_stream_events line)

let%test "parse_stream_events tolerates leading garbage before JSON" =
  let line =
    "^D\x08\x08{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"abc-123\",\"tools\":[]}"
  in
  List.equal Types.Stream_event.equal (parse_stream_events line)
    [
      Types.Stream_event.Session_init
        {
          session_id = "abc-123";
          api_key_source = None;
          model = None;
          claude_code_version = None;
          permission_mode = None;
        };
    ]

let%test "parse_stream_events extracts session_id from result event" =
  let line =
    {|{"type":"result","result":"done","stop_reason":"end_turn","session_id":"def-456"}|}
  in
  List.equal Types.Stream_event.equal (parse_stream_events line)
    [
      Types.Stream_event.Session_init
        {
          session_id = "def-456";
          api_key_source = None;
          model = None;
          claude_code_version = None;
          permission_mode = None;
        };
      Types.Stream_event.Final_result
        { text = "done"; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "parse_stream_events result without session_id omits Session_init" =
  let line = {|{"type":"result","result":"done","stop_reason":"end_turn"}|} in
  List.equal Types.Stream_event.equal (parse_stream_events line)
    [
      Types.Stream_event.Final_result
        { text = "done"; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "find_json_start strips leading junk" =
  String.equal (find_json_start "^D\x08\x08{\"type\":\"x\"}") "{\"type\":\"x\"}"

let%test "find_json_start passthrough when clean" =
  String.equal (find_json_start "{\"type\":\"x\"}") "{\"type\":\"x\"}"

let%test "find_json_start returns input when no brace" =
  String.equal (find_json_start "no json here") "no json here"
