(* @archlint.module interface
   @archlint.domain coordinator *)

val handle : Coordinator_host.t -> string -> string
(** Decode a v3 run-scoped control and return accepted, already_applied,
    rejected or not_accepted. The confirmed host commit is the only ACK
    authority; callers must serialize access to the host with worker events. *)

val run :
  net:_ Eio.Net.t -> path:string -> submit:(string -> string) -> unit -> unit
(** Owner-only local socket. [submit] must serialize with the coordinator event
    loop (it must not concurrently mutate a host). No legacy Runtime writes. *)
