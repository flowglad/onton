(* @archlint.module test
   @archlint.domain branch-reconcile *)

(* Patch 5 replaces this scaffold with real Git fixtures. Publish to a bare
   origin, destroy the original clone and its object database, then recover in
   a separate clone. Observe adoption of the remote patch tip, recreation only
   from the confirmed current base when allowed, and no mutation on failed or
   malformed probes. Include lost push ACK, disappeared published branch,
   reappearance, merge and renewed-intent cases; never carry old refs or a
   conversation archive into the replacement. Use the existing Eio fixture
   conventions and a bounded child process. *)
let () = print_endline "PENDING patch 5: fresh-clone Git recovery"
