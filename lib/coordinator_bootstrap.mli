(* @archlint.module interface
   @archlint.domain coordinator-state *)

type authority = { owner_epoch : int64; revision : int64 }

type ready = {
  store : Coordinator_store.t;
  envelope : Coordinator_store.envelope;
  authority : authority;
}

val start :
  client:Resource_client.t ->
  run_id:string ->
  coordinator_id:string ->
  config_id:string ->
  plan_id:string ->
  plan_bytes:string ->
  config_bytes:string ->
  capacity:int ->
  fresh:bool ->
  takeover_guards:(int64 * int64) option ->
  next_id:(unit -> string) ->
  (ready, string) result
(** [fresh] authorizes creation only after the public helper confirms [open] and
    the guarded current-state read reports [not_found]. Existing runs must load
    exact matching identity/bytes; no legacy snapshot or empty-state fallback is
    permitted. A successor supplies the captured owner epoch and revision for a
    guarded [takeover]; missing guards fail closed. The helper authenticates the
    task identity and its deployed descriptor independently of caller-supplied
    request fields. *)
