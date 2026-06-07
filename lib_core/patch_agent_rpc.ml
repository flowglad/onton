(* @archlint.module core
   @archlint.domain patch-agent-rpc *)

open Base

type command =
  | Prompt of { request_id : string; content : string }
  | Abort of { request_id : string }
  | Status of { request_id : string }
  | Shutdown of { request_id : string }
[@@deriving show, eq, sexp_of, compare]

type usage = {
  input_tokens : int;
  output_tokens : int;
  cache_read_tokens : int;
  cache_write_tokens : int;
}
[@@deriving show, eq, sexp_of, compare]

type event =
  | Session_init of {
      session_id : string;
      model_id : string;
      provider : string;
    }
  | Turn_started of { turn_index : int }
  | Text_delta of { delta : string }
  | Tool_call of { name : string; input : string; call_id : string }
  | Done of { stop_reason : string; final_text : string; usage : usage option }
  | Error of { code : string; message : string }
[@@deriving show, eq, sexp_of, compare]

let has_raw_line_break s =
  String.exists s ~f:(function '\n' | '\r' -> true | _ -> false)

let reject_raw_line_breaks ~context s =
  if has_raw_line_break s then
    Result.Error
      (Printf.sprintf "%s must not contain raw CR/LF characters" context)
  else Result.Ok ()

let command_to_yojson = function
  | Prompt { request_id; content } ->
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

let serialize_command command =
  let payload = Yojson.Safe.to_string (command_to_yojson command) in
  payload ^ "\n"

let member name json =
  match json with
  | `Assoc fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None

let require_string field json =
  match member field json with
  | Some (`String s) -> Result.Ok s
  | Some `Null ->
      Result.Error (Printf.sprintf "missing required field %S" field)
  | Some _ -> Result.Error (Printf.sprintf "field %S must be a string" field)
  | None -> Result.Error (Printf.sprintf "missing required field %S" field)

let require_int field json =
  match member field json with
  | Some (`Int n) -> Result.Ok n
  | Some `Null ->
      Result.Error (Printf.sprintf "missing required field %S" field)
  | Some _ -> Result.Error (Printf.sprintf "field %S must be an integer" field)
  | None -> Result.Error (Printf.sprintf "missing required field %S" field)

let parse_usage json =
  let ( let* ) = Result.( >>= ) in
  match json with
  | `Assoc _ ->
      let* input_tokens = require_int "input_tokens" json in
      let* output_tokens = require_int "output_tokens" json in
      let* cache_read_tokens = require_int "cache_read_tokens" json in
      let* cache_write_tokens = require_int "cache_write_tokens" json in
      Result.Ok
        { input_tokens; output_tokens; cache_read_tokens; cache_write_tokens }
  | _ -> Result.Error "field \"usage\" must be an object"

let parse_event_json json =
  let ( let* ) = Result.( >>= ) in
  match json with
  | `Assoc _ -> (
      let* kind = require_string "type" json in
      match kind with
      | "session_init" ->
          let* session_id = require_string "session_id" json in
          let* model_id = require_string "model_id" json in
          let* provider = require_string "provider" json in
          Result.Ok (Session_init { session_id; model_id; provider })
      | "turn_started" ->
          let* turn_index = require_int "turn_index" json in
          Result.Ok (Turn_started { turn_index })
      | "text_delta" ->
          let* delta = require_string "delta" json in
          Result.Ok (Text_delta { delta })
      | "tool_call" ->
          let* name = require_string "name" json in
          let* input = require_string "input" json in
          let* call_id = require_string "call_id" json in
          Result.Ok (Tool_call { name; input; call_id })
      | "done" ->
          let* stop_reason = require_string "stop_reason" json in
          let* final_text = require_string "final_text" json in
          let* usage =
            match member "usage" json with
            | None -> Result.Ok None
            | Some usage_json ->
                Result.map (parse_usage usage_json) ~f:Option.some
          in
          Result.Ok (Done { stop_reason; final_text; usage })
      | "error" ->
          let* code = require_string "code" json in
          let* message = require_string "message" json in
          Result.Ok (Error { code; message } : event)
      | _ -> Result.Error (Printf.sprintf "unknown event type %S" kind))
  | _ -> Result.Error "event must be a JSON object"

let parse_event line =
  let ( let* ) = Result.( >>= ) in
  let* () = reject_raw_line_breaks ~context:"event line" line in
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error msg ->
      Result.Error (Printf.sprintf "invalid JSON: %s" msg)
  | json -> parse_event_json json
