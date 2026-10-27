(* @archlint.module interface
   @archlint.domain resource-protocol *)

(* Strict public resource-v1 wire contract. No provider authority is carried by these values. *)
type operation =
  | Open
  | Inspect
  | Takeover
  | Launch
  | Observe
  | Stop
  | Replace
  | State_read
  | State_write
  | Write_status
  | Close
  | Log

type request = private {
  operation : operation;
  bytes : string;
  request_id : string;
  run_id : string;
}

val make_request : string -> (request, string) result
val encode_request : request -> (string, string) result

type status = Ok | Rejected | Pending | Attention

type reason =
  | None_reason
  | Invalid_request
  | Not_found
  | Not_owner
  | Stale_guard
  | Changed_request
  | Closed
  | Closing
  | Chain_busy
  | Predecessor_live
  | Launch_uncertain
  | Observation_exhausted
  | State_version
  | Storage_uncertain
  | Corrupt_object
  | Cleanup_pending
  | Invariant_violation

type response = private {
  status : status;
  reason : reason;
  run_id : string option;
  revision : int64 option;
  owner_epoch : int64 option;
  resource_id : string option;
  state_version : int64 option;
  receipt_state_version : int64 option;
  payload : string option;
}

val decode_response : string -> (response, string) result

(* Only a matching, zero-exit stop (or successful guarded replacement) is
    helper-provided retirement evidence. Observe-ok alone is never proof. *)
val retirement_evidence :
  operation -> process_exit:int -> matching:bool -> response -> bool

val op_name : operation -> string
val max_envelope : int
