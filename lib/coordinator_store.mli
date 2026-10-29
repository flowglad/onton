(* @archlint.module interface
   @archlint.domain coordinator-state *)

type envelope = private {
  version : int64;
  write_id : string;
  state : Coordinator_state.t;
  streams : (string * Worker_stream.t) list;
}

type receipt = private { write_id : string; state_version : int64 }

type error =
  | Invalid of string
  | Helper of Resource_client.error
  | Refused of Resource_protocol.response
  | Uncertain of string
  | Conflict of string

type t

val create :
  client:Resource_client.t ->
  run_id:string ->
  coordinator_id:string ->
  owner_epoch:int64 ->
  revision:int64 ->
  next_id:(unit -> string) ->
  t

val authority : t -> string * string * int64 * int64
val state : envelope -> Coordinator_state.t
val version : envelope -> int64
val write_id : envelope -> string
val initial : Coordinator_state.t -> envelope
val with_state : envelope -> Coordinator_state.t -> envelope
val stream : envelope -> string -> Worker_stream.t option
val with_stream : envelope -> string -> Worker_stream.t -> envelope
val committed : envelope -> receipt -> envelope
val encode : envelope -> string
val load : t -> (envelope, error) result

val commit :
  ?emergency:bool ->
  t ->
  expected_version:int64 ->
  write_id:string ->
  envelope ->
  (receipt, error) result
(** A failed/uncertain commit does not publish memory. A same-owner exact retry
    retains the original immutable request. Takeover reads only current bytes.
*)
