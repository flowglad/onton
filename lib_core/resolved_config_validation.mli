(* @archlint.module interface
   @archlint.domain resolved-config *)

val automerge_timeout_error : float -> string option
(** Return the validation error for an automerge idle window, or [None] when the
    value is finite and greater than zero. Total for every float. *)
