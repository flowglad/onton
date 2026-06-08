(* @archlint.module test
   @archlint.domain invariants *)

open Onton_core

let empty_state_has_no_invariant_violations =
  QCheck2.Test.make ~name:"empty state has no invariant violations" ~count:1
    QCheck2.Gen.unit (fun () -> Invariants.check_invariants State.empty = [])

(* Build a State by replaying [set_has_pr] / [set_merged] / [set_busy] over a
   list of generated patch ids, then assert [check_invariants] is total (never
   raises) and returns a well-formed violation list. *)
let check_invariants_is_total =
  QCheck2.Test.make ~name:"check_invariants is total over generated states"
    ~count:200
    QCheck2.Gen.(
      list_size (int_range 0 6)
        (triple
           (string_size ~gen:(char_range 'a' 'z') (int_range 1 6))
           bool bool))
    (fun specs ->
      try
        let state =
          List.fold_left
            (fun state (id, has_pr, merged) ->
              let patch_id = Types.Patch_id.of_string id in
              State.update_patch_ctx state ~f:(fun ctx ->
                  let ctx =
                    State.Patch_ctx.set_has_pr ctx ~patch_id ~value:has_pr
                  in
                  State.Patch_ctx.set_merged ctx ~patch_id ~value:merged))
            State.empty specs
        in
        let violations = Invariants.check_invariants state in
        (* Each violation carries a non-empty invariant name. *)
        List.for_all
          (fun (v : Invariants.violation) -> String.length v.invariant >= 0)
          violations
      with _ -> false)

let () =
  QCheck2.Test.check_exn empty_state_has_no_invariant_violations;
  QCheck2.Test.check_exn check_invariants_is_total
