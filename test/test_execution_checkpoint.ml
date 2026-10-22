(* @archlint.module test
   @archlint.domain worker-checkpoint *)

(* Patch 3 replaces this scaffold with a fake host ACK gate around the real
   checkpoint-aware setup and session entry points. Delay, reject and drop each
   proposal, then observe that dependent setup, repair, session identity and
   terminal effects wait for the exact ACK without a Runtime-lock deadlock.
   Exercise the existing Make wrappers separately to preserve local behavior. *)
let () = print_endline "PENDING patch 3: execution checkpoint barrier"
