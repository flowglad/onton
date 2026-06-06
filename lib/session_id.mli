(* @archlint.module interface
   @archlint.domain session-id *)

val mint : unit -> string
(** Mint a UUID v4 string suitable for caller-provided Claude session IDs. *)
