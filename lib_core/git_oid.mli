(* @archlint.module interface
   @archlint.domain git-oid *)

val valid : string -> bool
(** A complete lowercase SHA-1 or SHA-256 Git object identifier. Total over
    arbitrary strings; abbreviated, uppercase and malformed input is rejected.
*)
