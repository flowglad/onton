(* @archlint.module interface
   @archlint.domain run-classification *)

open Base

(** Pure classification of Claude runner outcomes.

    Maps a runner result to a decision value without any side effects. Designed
    for property-based testing. *)

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

val is_context_exhausted : string -> bool
(** [true] when the error stream reports a context-window overflow ("ran out of
    room", "context window", "context_length_exceeded",
    "model_context_window_exceeded"). Matched on the error stream only, so
    legitimate agent output mentioning these phrases cannot trip it. *)

val classify :
  is_resume:bool -> (run_outcome, string) Result.t -> classification
(** Classify an LLM runner result into a pure decision value. [is_resume]
    indicates whether this was a --resume session (explicit session ID).
    [Context_exhausted] takes precedence over [Success] (it can be reported on
    an otherwise-clean exit-0 stream) but not over [Timed_out]. *)
