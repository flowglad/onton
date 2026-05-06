open Base

let warn_unknown_stop_reason raw =
  Stdlib.prerr_endline
    (Printf.sprintf
       "warning: patch-agent reported unsupported stop_reason %S; defaulting \
        to end_turn"
       raw)

let parse_stop_reason (raw : string) : Types.Stop_reason.t =
  match raw with
  | "stop" -> Types.Stop_reason.End_turn
  | "tool_use" -> Types.Stop_reason.Tool_use
  | "max_tokens" -> Types.Stop_reason.Max_tokens
  | "error" -> Types.Stop_reason.End_turn
  | other -> (
      match Types.Stop_reason.of_string other with
      | Some stop_reason -> stop_reason
      | None ->
          warn_unknown_stop_reason other;
          Types.Stop_reason.End_turn)

let map_event (event : Patch_agent_rpc.event) : Types.Stream_event.t =
  match event with
  | Session_init { session_id; _ } ->
      Types.Stream_event.Session_init { session_id }
  | Turn_started _ -> Types.Stream_event.Turn_started
  | Text_delta { delta } -> Types.Stream_event.Text_delta delta
  | Tool_call { name; input; _ } ->
      Types.Stream_event.Tool_use { name; input; status = None }
  | Done { stop_reason; final_text; _ } ->
      Types.Stream_event.Final_result
        { text = final_text; stop_reason = parse_stop_reason stop_reason }
  | Error { code; message } ->
      let text =
        match String.strip code with
        | "" -> message
        | normalized_code ->
            Printf.sprintf "patch-agent %s: %s" normalized_code message
      in
      Types.Stream_event.Error text
