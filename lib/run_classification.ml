open Base

(** Pure classification of Claude runner outcomes.

    Extracts the decision logic from [run_claude_and_handle] so it can be tested
    without Eio or Runtime dependencies. *)

type run_outcome = {
  exit_code : int;
  got_events : bool;
  stderr : string;
  stream_errors : string;
}
[@@deriving show, eq]

type classification =
  | Process_error of string
  | No_session_to_resume
  | Success of { stream_errors : string }
  | Session_failed of { detail : string }
[@@deriving show, eq]

let truncate s n = if String.length s <= n then s else String.prefix s n ^ "..."

let classify ~continue result =
  match result with
  | Error msg -> Process_error msg
  | Ok r when (not r.got_events) && continue -> No_session_to_resume
  | Ok r when r.exit_code = 0 -> Success { stream_errors = r.stream_errors }
  | Ok r ->
      let stderr = String.strip r.stderr in
      let stream_errors = String.strip r.stream_errors in
      let detail =
        match (stderr, stream_errors) with
        | "", "" -> "(no error details)"
        | "", e | e, "" -> e
        | s, e -> s ^ " | stream: " ^ e
      in
      Session_failed { detail = truncate detail 500 }
