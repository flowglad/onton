(* @archlint.module interface
   @archlint.domain coordinator *)

type env = {
  store : Coordinator_store.t;
  client : Resource_client.t;
  next_write_id : unit -> string;
  next_event : unit -> Coordinator_state.event option;
  stop_request :
    resource_id:string -> (Resource_protocol.request, string) result;
  outcome : Coordinator_state.outcome -> unit;
  blocked : string -> unit;
}
(** A serial, opt-in host loop. The caller supplies current helper authority and
    events; ordinary local execution never enters this module. *)

val run : env -> unit

val submit :
  Coordinator_host.t ->
  Coordinator_state.event ->
  (Coordinator_host.transition, Coordinator_host.error) result
(** Commit an intent before its outcomes become visible to callers. *)
