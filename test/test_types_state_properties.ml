(* @archlint.module stateTest
   @archlint.domain types *)

open Onton_core
module C = Types.Comment_id

type operation = Issue | Restore of int list

let operation =
  QCheck2.Gen.(
    oneof
      [
        return Issue;
        map
          (fun offsets -> Restore offsets)
          (list_size (int_range 0 12) (int_range (-20) 20));
      ])

let synthetic_ids_survive_restore_interleavings =
  QCheck2.Test.make ~count:500
    ~name:
      "synthetic comment IDs remain unique and monotone across restore \
       interleavings"
    QCheck2.Gen.(list_size (int_range 0 80) operation)
    (fun operations ->
      try
        let initial = C.to_int (C.next_synthetic ()) in
        let _, _, valid =
          List.fold_left
            (fun (minimum, issued, valid) operation ->
              match operation with
              | Issue ->
                  let actual = C.to_int (C.next_synthetic ()) in
                  ( actual,
                    actual :: issued,
                    valid
                    && actual = minimum - 1
                    && actual < 0
                    && not (List.mem actual issued) )
              | Restore offsets ->
                  let ids = List.map (fun offset -> minimum + offset) offsets in
                  C.seed_synthetic_counter (List.map C.of_int (42 :: ids));
                  let expected_minimum = List.fold_left min minimum ids in
                  (* Re-importing the same snapshot cannot advance or regress
                   the allocation boundary a second time. *)
                  C.seed_synthetic_counter (List.map C.of_int (List.rev ids));
                  ( expected_minimum,
                    issued,
                    valid
                    && Comment_id_allocation.seed_floor ~current:minimum
                         ~observed:ids
                       = expected_minimum ))
            (initial, [ initial ], initial < 0)
            (operations @ [ Issue ])
        in
        valid
      with _ -> false)

let () = QCheck2.Test.check_exn synthetic_ids_survive_restore_interleavings
