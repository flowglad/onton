(* @archlint.module core
   @archlint.domain worker-protocol *)

(** Protocol v1 pure framing and single-process admission. Only [decode] can
    construct a host message. The wrapper authenticates its local pipe and
    establishes exclusive process ownership before [bind]. *)
type decode_error = Invalid of string | Incompatible of string

type identity = { run_id : string; patch_id : string }

type operation = {
  operation_id : string;
  attempt_id : string;
  assignment_seq : int64;
}

type operation_kind =
  | Session of { phase : string; kind : string option; turn : string }
  | Reconcile

type recovery_mode = Retained_workspace | Lost_workspace

type host_message = private
  | Bind of {
      identity : identity;
      worker_id : string;
      workspace_id : string;
      context : string;
      capsule : string;
      state_version : int64;
      attention_id : string option;
    }
  | Execute of {
      identity : identity;
      operation : operation;
      expected_state_version : int64;
      work : operation_kind;
      guidance : string;
      recovery_mode : recovery_mode;
      renewal_id : string option;
      renewed_intent : bool;
    }
  | Cancel of { identity : identity; operation : operation }
  | Checkpoint_ack of {
      identity : identity;
      operation : operation;
      proposal_id : string;
      state_version : int64;
      capsule : string;
      accepted : bool;
    }
  | Result_ack of {
      identity : identity;
      operation : operation;
      state_version : int64;
    }
  | Shutdown of identity

type result_status =
  | Completed_ok
  | Completed_no_commits
  | Failed
  | Cancelled
  | Retryable
  | Attention
  | Merged_observed

type worker_message =
  | Ready of { identity : identity; worker_id : string; state_version : int64 }
  | Accepted of { identity : identity; operation : operation; replay : bool }
  | Observation of { identity : identity; operation : operation; text : string }
  | Checkpoint of {
      identity : identity;
      operation : operation;
      proposal_id : string;
      previous_state_version : int64;
      state_version : int64;
      capsule : string;
    }
  | Result of {
      identity : identity;
      operation : operation;
      state_version : int64;
      status : result_status;
      diagnostics : string;
      renewal_id : string option;
    }
  | Protocol_error of { identity : identity option; reason : string }

val decode : string -> (host_message, decode_error) result
(** One complete line, including its newline, is at most 1 MiB. *)

val encode : worker_message -> string

type t

val initial : unit -> t
val state_version : t -> int64 option
val is_stopped : t -> bool

type event =
  | Host of host_message
  | Propose_checkpoint of { proposal_id : string; capsule : string }
  | Finish of {
      status : result_status;
      diagnostics : string;
      renewal_id : string option;
    }
  | Cleanup_complete
  | Host_lost

type output =
  | Send of worker_message
  | Begin of host_message
  | Cancel_active
  | Stop

val step : t -> event -> t * output list
