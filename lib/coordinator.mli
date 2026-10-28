(* @archlint.module interface
   @archlint.domain coordinator *)

type connection = {
  run_id : string;
  coordinator_id : string;
  owner_epoch : int64;
  revision : int64;
  request_id : string;
  resource_id : string;
}

type event =
  | Intent of Coordinator_state.event
  | Control_line of string * (string -> unit)
      (** The authorized socket queues the exact bytes and waits for this
          serialized commit response; no concurrent host writes. *)
  | Close_run of Resource_protocol.request
      (** Terminal cancellation only; commit the exact close request first, then
          accept only matching successful helper cleanup evidence. *)
  | Attach of string * connection
  | Command of string * string
  | Read of string
  | Recover of {
      predecessor : string;
      assignment : string;
      worker_id : string;
      workspace_id : string;
    }

type env = {
  store : Coordinator_store.t;
  client : Resource_client.t;
  next_write_id : unit -> string;
  next_event : unit -> event option;
  now : unit -> int64;
  stream_timeout : float;
  attached : resource_id:string -> readable:(unit -> bool) -> unit;
  gameplan : Types.Gameplan.t;
  main_branch : Types.Branch.t;
  stop_request :
    resource_id:string -> (Resource_protocol.request, string) result;
  outcome : Coordinator_state.outcome -> unit;
  blocked : string -> unit;
}
(** A serial, opt-in host loop. The caller supplies current helper authority and
    events; ordinary local execution never enters this module. Attach consumes a
    resource stream once. Command/Read run the durable v3 host, not caller
    attestations. [attached] supplies nonblocking readiness; schedule Read only
    for a readable attachment so quiet workers cannot block control selection.
    All descriptors close on failure/cancellation; EOF/send failure does not
    prove retirement. *)

val run : env -> unit

val submit :
  Coordinator_host.t ->
  Coordinator_state.event ->
  (Coordinator_host.transition, Coordinator_host.error) result
(** Commit an intent before its outcomes become visible to callers. *)
