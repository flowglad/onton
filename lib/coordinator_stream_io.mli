(* @archlint.module interface
   @archlint.domain coordinator-stream-io *)

type t

val create :
  mono_clock:Eio.Time.Mono.ty Eio.Time.Mono.t -> Resource_client.attachment -> t

val ready : t -> bool
(** Nonblocking readiness, including an already buffered complete frame. *)

val read : t -> timeout:float -> (string, string) result

val send : t -> timeout:float -> string list -> (unit, string) result
(** Bounded nonblocking raw-stream I/O. Each item must be one complete,
    newline-terminated protocol frame. Partial frames never enter the protocol;
    partial sends are uncertain and must not be retried on another attachment.
*)
