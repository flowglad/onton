open Types

type t
(** Coordinator-owned durable intent. This type contains no worker owner
    capsule; a caller must atomically persist the returned state before acting
    on effects. *)

type control = {
  id : string;
  run_id : string;
  config_id : string;
  payload : string;
  kind : string;
  patch_id : string option;
}

val decode_control : string -> (control, string) result

type assignment = {
  id : string;
  operation : string;
  patch : Patch_id.t;
  resource : string;
  request : string;
  reserved : bool;
  retired : bool;
  result_committed : bool;
  cleanup : bool;
}

type operation = {
  id : string;
  patch : Patch_id.t;
  used : int;
  budget : int;
  retry_after : int64;
  attention : bool;
  unfinished : bool;
  maintenance : bool;
}

type capsule = {
  assignment : string;
  version : int64;
  bytes : string;
  receipt : string;
}
(** Opaque exact owner artifact; coordinator cannot mutate reconciliation bytes.
*)

type lifecycle =
  | Running
  | Draining of string
  | Sleeping of string
  | Cancelling of string

type event =
  | Admit of operation
  | Assign of assignment * int64
  | Result_committed of string * string
  | Cleanup_confirmed of string * string
  | Stop_confirmed of string * string
  | Resource_stopped of string * string
  | Owner_receipt of capsule
  | Resource_request of string * string
  | Bind_resource of string * Patch_id.t
  | Terminal_patch of Patch_id.t
      (** Host-attested merged/receipt-credited completion only, never idle,
          blocked, retryable, no-commits or intervention. *)
  | Loss of string * int64
  | Retry of string * int64
  | Control of control
  | Input of string * string * int64 * string
  | Input_consumed of string * string * int64
      (** Checkpoint-attested assignment, input ID and input version. *)
  | Sleep_ready of string
  | Cancel_cleanup_confirmed

type outcome =
  | Accepted
  | Rejected of string
  | Already_applied
  | Dispatch of string
  | Recover of string
  | Maintain of Patch_id.t
  | Attention of string
  | Retire of string
  | Ready_to_sleep of string
  | Cancel_complete

val create :
  run_id:string ->
  config_id:string ->
  plan_id:string ->
  plan_bytes:string ->
  config_bytes:string ->
  capacity:int ->
  budget_bytes:int ->
  t

val plan_bytes : t -> string
val config_bytes : t -> string

(* Versioned exact run-state payload. Restore validates reservations, uniqueness,
    inventory, operation bindings and emergency headroom; no history is evicted. *)
val encode : t -> string
val decode : string -> (t, string) result
val step : t -> event -> t * outcome list

(* Pure prospective transition only. The host must commit the exact resulting
    state, capsule and protocol receipt together before an ACK or Dispatch.
    [Owner_receipt] and [Input_consumed] are attestations from the validated
    protocol/commit boundary, not proof created by calling [step].
    [Resource_stopped] requires matching successful helper stop evidence;
    generic observe-ok/RUNNING and stream EOF must never produce this event. *)
val operations : t -> operation list
val assignments : t -> assignment list
val resources : t -> (string * string) list
val resource_owners : t -> (string * Patch_id.t) list
val terminal_patches : t -> Patch_id.t list
val retired_resources : t -> string list
val capsules : t -> capsule list
val controls : t -> control list
val inputs : t -> (string * string * int64 * string * bool) list
val lifecycle : t -> lifecycle
val sleep_ready : t -> bool
val cancellation_complete : t -> bool
val reserved_count : t -> int
val can_recover : t -> operation -> now:int64 -> bool

type context_input = {
  run_id : string;
  config_id : string;
  plan_id : string;
  message_id : string;
  generation : int64;
  patch_id : Patch_id.t;
  dependencies : (Patch_id.t * bool * string) list;
  selected_base : Branch.t;
  mode : string;
  root : bool;
  evidence_id : string;
  guidance_ids : string list;
  input_version : int64;
}
(** Strict context projection: admission, message scope, selected base, mode,
    evidence and guidance are supplied from the current scheduling view. *)

val execution_context :
  t ->
  Scheduler_decision.input ->
  context_input ->
  (Worker_protocol_v3.execution_context, string) result
