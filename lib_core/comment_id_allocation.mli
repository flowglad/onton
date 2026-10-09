(* @archlint.module interface
   @archlint.domain types *)

val seed_floor : current:int -> observed:int list -> int
(** Lowest allocation boundary after restoring observed comment IDs. Positive
    server IDs cannot advance the synthetic counter. Combining snapshots is
    commutative and idempotent; the result never exceeds zero or [current]. *)
