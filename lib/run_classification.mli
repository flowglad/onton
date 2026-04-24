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
  | Success of { stream_errors : string }
  | Session_failed of { exit_code : int; detail : string }
[@@deriving show, eq]

val classify :
  is_resume:bool -> (run_outcome, string) Result.t -> classification
(** Classify a Claude runner result into a pure decision value. [is_resume]
    indicates whether this was a --resume session (explicit session ID). *)
