(* @archlint.module test
   @archlint.domain worker-transport *)

(* Patch 6 replaces this scaffold with a subprocess host harness against the
   actual onton-worker executable. Send versioned JSONL assignments and ACKs
   across one run/patch process, then exercise malformed frames, conflicting
   identities, stale ACK/result/cancel, timeout/EOF and cleanup. Observe exact
   checkpoint gating and two sequential operations only after terminal ACK.
   The worker-protocol-smoke alias gains the binary dependency in patch 6. *)
let () = print_endline "PENDING patch 6: public worker protocol"
