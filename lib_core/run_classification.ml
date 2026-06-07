(* @archlint.module core
   @archlint.domain run-classification *)

open Base

(** Pure classification of Claude runner outcomes.

    Extracts the decision logic from [run_claude_and_handle] so it can be tested
    without Eio or Runtime dependencies. *)

type run_outcome = {
  exit_code : int;
  got_events : bool;
  saw_final_result : bool;
  stderr : string;
  stream_errors : string;
  timed_out : bool;
}
[@@deriving show, eq]

type classification =
  | Process_error of string
  | No_session_to_resume
  | Timed_out
  | Context_exhausted of { stream_errors : string }
  | Success of { stream_errors : string }
  | Session_failed of { exit_code : int; detail : string }
[@@deriving show, eq]

let truncate s n = if String.length s <= n then s else String.prefix s n ^ "..."

(* The model's context window overflowed. Matched on the error stream only
   (never agent text), so legitimate output mentioning these phrases cannot
   trip it. Codex surfaces this as an [error]/[turn.failed] event carrying
   "Codex ran out of room in the model's context window..."; Claude's
   [model_context_window_exceeded] / "prompt is too long" land here too when
   its own auto-compaction fails. The phrasing is intentionally broad to cover
   both backends' wording and the OpenAI [context_length_exceeded] code. *)
let is_context_exhausted stream_errors =
  let h = String.lowercase stream_errors in
  let contains needle = String.is_substring h ~substring:needle in
  contains "ran out of room" || contains "context window"
  || contains "context_length_exceeded"
  || contains "model_context_window_exceeded"

let classify ~is_resume result =
  match result with
  | Error msg -> Process_error msg
  | Ok r when r.timed_out -> Timed_out
  | Ok r when is_context_exhausted r.stream_errors ->
      Context_exhausted { stream_errors = r.stream_errors }
  | Ok r when r.saw_final_result -> Success { stream_errors = r.stream_errors }
  | Ok r when r.exit_code = 0 -> Success { stream_errors = r.stream_errors }
  | Ok r when (not r.got_events) && is_resume -> No_session_to_resume
  | Ok r ->
      let stderr = String.strip r.stderr in
      let stream_errors = String.strip r.stream_errors in
      let detail =
        match (stderr, stream_errors) with
        | "", "" -> "(no error details)"
        | "", e | e, "" -> e
        | s, e -> s ^ " | stream: " ^ e
      in
      Session_failed { exit_code = r.exit_code; detail = truncate detail 500 }

let%test_module "context exhaustion classification" =
  (module struct
    let outcome ?(exit_code = 0) ?(saw_final_result = false)
        ?(timed_out = false) stream_errors =
      {
        exit_code;
        got_events = true;
        saw_final_result;
        stderr = "";
        stream_errors;
        timed_out;
      }

    let is_ctx = function
      | Context_exhausted _ -> true
      | Process_error _ | No_session_to_resume | Timed_out | Success _
      | Session_failed _ ->
          false

    let is_success = function
      | Success _ -> true
      | Process_error _ | No_session_to_resume | Timed_out | Context_exhausted _
      | Session_failed _ ->
          false

    (* Spec: an outcome whose error stream reports context overflow is
       Context_exhausted regardless of exit code or a final-result event. *)
    let%test "codex overflow phrase, exit 0 -> Context_exhausted" =
      is_ctx
        (classify ~is_resume:true
           (Ok
              (outcome
                 "Codex ran out of room in the model's context window. Start a \
                  new thread or clear earlier history before retrying.")))

    let%test "overflow + saw_final_result -> Context_exhausted (precedence)" =
      is_ctx
        (classify ~is_resume:false
           (Ok (outcome ~saw_final_result:true "model_context_window_exceeded")))

    let%test "openai context_length_exceeded -> Context_exhausted" =
      is_ctx
        (classify ~is_resume:false (Ok (outcome "context_length_exceeded")))

    (* Timeout still wins: a timed-out session is Timed_out even if its partial
       error stream happens to mention the context window. *)
    let%test "timed_out + overflow phrase -> Timed_out" =
      match
        classify ~is_resume:true
          (Ok (outcome ~timed_out:true "ran out of room in the context window"))
      with
      | Timed_out -> true
      | Process_error _ | No_session_to_resume | Context_exhausted _ | Success _
      | Session_failed _ ->
          false

    (* No false positive: a benign error stream with exit 0 stays Success. *)
    let%test "benign stream error, exit 0 -> Success" =
      is_success
        (classify ~is_resume:false
           (Ok (outcome "tool execution failed: ENOENT")))

    let%test "is_context_exhausted negative on empty" =
      not (is_context_exhausted "")
  end)
