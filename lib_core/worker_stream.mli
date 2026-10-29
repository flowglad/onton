(* @archlint.module core
   @archlint.domain worker-stream *)

(** Host-side v3 validation. Only decoded inbound frames advance worker facts.
    The returned state is prospective: persist it with coordinator intent before
    sending any returned frame. The terminal and post-ACK states are separate
    barriers. No caller can supply a receipt or assert cleanup. *)

type t

val initial : t
val command : t -> string -> (t * string list, string) result
val receive : t -> string -> (t * string list, string) result
val acknowledge_result : t -> (t * string list, string) result

val rebind :
  t ->
  worker_id:string ->
  workspace_id:string ->
  (t * string list, string) result
(** Derive a replacement Bind solely from committed capsule/receipt history. The
    effect handler must first prove predecessor retirement. *)

val capsule : t -> string option
val receipt : t -> string option
val identity : t -> Worker_protocol.identity option
val binding : t -> Worker_protocol_v3.host_message option
val operation : t -> Worker_protocol.operation option
val ready : t -> bool
val input : t -> Worker_protocol_v3.input option
val delivered_input : t -> Worker_protocol_v3.input -> bool
val terminal : t -> Worker_protocol.worker_message option
val acknowledged : t -> bool
val encode : t -> string

val decode : string -> (t, string) result
(** Exact validated transcript replay, bounded by the enclosing run envelope.
    Decoding never sends frames or grants storage authority. *)
