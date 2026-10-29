(* @archlint.module interface
   @archlint.domain coordinator-state *)
type result = Ready | Pending | Attention | Blocked of string

val provision :
  host:Coordinator_host.t ->
  client:Resource_client.t ->
  request:Resource_protocol.request ->
  assignment:Coordinator_state.assignment ->
  now:int64 ->
  result
(** Commit exact launch/replace bytes, patch binding, reservation and attempt
    before invoking the public helper. Replay uses the original request only. A
    replacement requires a previously bound, proven-retired predecessor.
    Ambiguity holds the reservation; it never authorizes another launch ID. *)
