(* @archlint.module interface
   @archlint.domain worker-protocol-v3 *)

type execution_context
(** Opt-in, pure v3 boundary. The host stores [admission_receipt] beside the
    exact worker capsule; neither a receipt nor a prospective transition proves
    a storage commit or predecessor retirement. V2 remains [Worker_protocol]. *)

val decode_context : string -> (execution_context, string) result
val context_bytes : execution_context -> string
val selected_base : execution_context -> string
val execution_mode : execution_context -> string
val assigned_patch : execution_context -> string
val root_role : execution_context -> string
val message_generation : execution_context -> int64
val input_version : execution_context -> int64

type work =
  | Session of Worker_protocol.phase * Types.Operation_kind.t option * string
  | Reconcile
  | Integrate_revision of { contributor : string; revision : string }

type input = {
  input_id : string;
  version : int64;
  guidance : string;
  selected_base : string;
}

type execute = {
  request : Worker_protocol.execute_request;
  work : work;
  context : execution_context;
  input_version : int64;
}

type host_message =
  | Bind of Worker_protocol.host_message * execution_context
  | Execute of execute
  | Input of Worker_protocol.identity * input
  | Legacy_control of Worker_protocol.host_message

type worker_message = Worker_protocol.worker_message

val decode_host : string -> (host_message, string) result
val decode_worker : string -> (worker_message, string) result
val encode_host : host_message -> (string, string) result
val encode_worker : worker_message -> (string, string) result

type t

val initial : unit -> t
val admission_receipt : t -> Yojson.Safe.t option
val is_stopped : t -> bool
val recovery_pending : t -> bool

(* Latest checkpointed input base, otherwise bound base; never recovered from
    a historical Execute or an uncommitted input. *)
val desired_base : t -> string option
val state_version : t -> int64 option

type event =
  | Host of host_message
  | Worker_event of Worker_protocol.event
  | Input_committed of string * int64
  | Restore_input of input
  | Recover_unfinished
  | Host_lost

type output =
  | Send of worker_message
  | Begin of execute
  | Recover of execute
  | Apply_input of input
  | Cancel_active
  | Stop
  | Reject of string

(* [step] delegates assignment, checkpoint, terminal and ACK transitions to
    [Worker_protocol.step]. Bind fixes run/patch/config/plan/mode/role scope.
    New executions carry current coordinator evidence, a non-regressing message
    generation, and the desired base and version from the latest committed input
    (or Bind when no input has committed). Context and envelope input versions
    must agree. Exact historical assignments retain their original contexts for
    replay and recovery, even after retargeting.
    [Begin] still requires a committed admission receipt and exact capsule before
    the caller performs any effect. *)
val step : t -> event -> t * output list

type note =
  | Missing
  | Empty
  | Oversized
  | Unreadable
  | Present of {
      assignment : Worker_protocol.operation;
      publication : string;
      bytes : string;
    }

val note :
  assignment:Worker_protocol.operation ->
  publication:string ->
  (string, string) result ->
  note

val accept_note :
  assignment:Worker_protocol.operation ->
  publication:string ->
  note ->
  string option
