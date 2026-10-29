(* @archlint.module interface
   @archlint.domain coordinator-state *)
val encode : string -> string
(** Lossless byte encoding with a fixed 64 KiB backward window. *)

val decode : max_bytes:int -> string -> (string, string) result
(** Total decoder. Rejects malformed tokens, invalid backward references,
    trailing bytes and declared output above the caller's limit before
    allocating output. *)
