(* @archlint.module test
   @archlint.domain invariants *)

open Onton_core

let empty_state_has_no_invariant_violations =
  QCheck2.Test.make ~name:"empty state has no invariant violations" ~count:1
    QCheck2.Gen.unit (fun () -> Invariants.check_invariants State.empty = [])

let () = QCheck2.Test.check_exn empty_state_has_no_invariant_violations
