(* @archlint.module interface
   @archlint.domain coordinator-state *)

type t
(** One serial coordinator epoch. No prospective frame is exposed before exact
    confirmed storage readback of the whole run envelope. *)

type error = Store of Coordinator_store.error | Invalid of string

type transition =
  | Applied of Coordinator_state.outcome list
  | Rejected of string

val restore :
  store:Coordinator_store.t ->
  next_write_id:(unit -> string) ->
  (t, error) result

val state : t -> Coordinator_state.t
val envelope : t -> Coordinator_store.envelope
val stream : t -> string -> Worker_stream.t option

val submit : t -> Coordinator_state.event -> (transition, error) result
(** Coordinator intent only. Dispatch reserves capacity; it does not authorize
    sending Execute. Worker evidence cannot enter through this API. *)

val command :
  t ->
  assignment:string ->
  frame:string ->
  (Coordinator_state.outcome list * string list, error) result
(** Persist exact Bind/Execute/Input/control and derived admission before send.
    Execute operation/attempt must match the committed reservation; exact
    execution bytes are frozen with the derived receipt before sending. ACK
    commands cannot enter here. *)

val receive :
  t ->
  assignment:string ->
  frame:string ->
  gameplan:Types.Gameplan.t ->
  main_branch:Types.Branch.t ->
  now:int64 ->
  (Coordinator_state.outcome list * string list, error) result
(** Validate inbound v3 frames and exact capsules. Checkpoint ACK follows the
    paired receipt/capsule commit. Result adoption and post-ACK cleanup are two
    confirmed barriers; Result attests completed worker cleanup. Returned frames
    alone may be sent. An error never grants an ACK, even after a partial
    commit. *)

val recover :
  t ->
  predecessor:string ->
  assignment:string ->
  worker_id:string ->
  workspace_id:string ->
  (Coordinator_state.outcome list * string list, error) result
(** Reconstruct Bind from retained committed state only, after proven
    retirement. Unfinished replacement preserves operation/attempt; acknowledged
    idle recovery requires a fresh operation. No caller-supplied receipt enters
    this path. *)

val retired :
  t ->
  resource_id:string ->
  stop_request:Resource_protocol.request ->
  response:Resource_protocol.response ->
  (transition, error) result
