(* @archlint.module core
   @archlint.domain worker-protocol *)

(** Protocol v2 pure framing and durable admission. Only [decode] can construct
    a host message. The wrapper authenticates its local pipe and establishes
    exclusive process ownership before [bind]. *)
type decode_error = Invalid of string | Incompatible of string

type identity = { run_id : string; patch_id : string }

type operation = {
  operation_id : string;
  attempt_id : string;
  assignment_seq : int64;
}

type phase = Start | Respond

type operation_kind =
  | Session of {
      phase : phase;
      kind : Types.Operation_kind.t option;
      turn : string;
    }
  | Reconcile

type recovery_mode = Retained_workspace | Lost_workspace
type renewal = Ordinary | Renewed of string

type execute_request = {
  identity : identity;
  operation : operation;
  expected_state_version : int64;
  work : operation_kind;
  guidance : string;
  recovery_mode : recovery_mode;
  renewal : renewal;
}

type admission
(** Decoded wire receipt; bind validates its admission history before readiness.
*)

type host_message = private
  | Bind of {
      identity : identity;
      worker_id : string;
      workspace_id : string;
      context : string;
      capsule : string;
      state_version : int64;
      attention_id : string option;
      admission : admission;
    }
  | Execute of execute_request
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

val recovery_pending : t -> bool
(** True only for a restored unfinished predecessor needing observation. *)

type event =
  | Host of host_message
  | Report_observation of string
  | Propose_checkpoint of { proposal_id : string; capsule : string }
  | Finish of {
      status : result_status;
      diagnostics : string;
      renewal_id : string option;
    }
  | Cleanup_complete
  | Recover_unfinished
      (** Explicitly dispatch observation/repair of an interrupted assignment.
          The adapter must not rerun its original work. Recovery must checkpoint
          observed state and finish cleanup before producing a terminal result.
      *)
  | Host_lost

type output =
  | Send of worker_message
  | Begin of host_message
  | Recover of host_message
  | Cancel_active
  | Stop

val step : t -> event -> t * output list

val admission_receipt : t -> Yojson.Safe.t option
(** Patch-scoped admission facts for the required bind [admission] field.
    Persist admission before effects, terminal results before delivery, and the
    post-ACK receipt before delivering Result ACK to a worker. Pair this with
    the same committed capsule, state version and attention ID. Never replace a
    process until its descendants have been fenced. *)
