(* @archlint.module interface
   @archlint.domain coordinator-state *)

type t
(** A single serial coordinator epoch. Prospective decisions cannot be used as
    effects until the complete run envelope has been confirmed. *)

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

val submit : t -> Coordinator_state.event -> (transition, error) result
(** Coordinator-owned transitions only. Worker attestations require the
    explicitly bound entrypoints below. *)

val checkpoint :
  t ->
  assignment:string ->
  version:int64 ->
  capsule:string ->
  receipt:string ->
  gameplan:Types.Gameplan.t ->
  main_branch:Types.Branch.t ->
  context:string ->
  attention_id:string option ->
  (transition, error) result

val retired :
  t ->
  resource_id:string ->
  stop_request:Resource_protocol.request ->
  response:Resource_protocol.response ->
  (transition, error) result

val result :
  t -> assignment:string -> frame:string -> (transition, error) result

val cleanup :
  t -> assignment:string -> request:string -> (transition, error) result
