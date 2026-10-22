(* @archlint.module test
   @archlint.domain worker-protocol *)

(* Patch 2 replaces this scaffold with pure-only properties. Generate arbitrary
   JSONL frames for total decoding, including malformed and oversized input,
   then interleave assignments, exact duplicate replays, acknowledgements,
   terminal results and stale cancels. Observe that only the next bound
   operation can advance and no rejected input emits execution effects. *)
let () = print_endline "PENDING patch 2: worker protocol properties"
