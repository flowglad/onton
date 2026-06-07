(* @archlint.module stateTest
   @archlint.domain anchor *)

open Onton_core

let valid_sha = String.make 40 'a'

let anchor_make_is_total_across_observation_sequences =
  QCheck2.Test.make ~name:"anchor creation is stable across observation order"
    ~count:200
    QCheck2.Gen.(list_size (int_range 0 50) bool)
    (fun observations ->
      List.for_all
        (fun observed_at_remote ->
          match
            Anchor.make
              ~base:(Types.Branch.of_string "main")
              ~sha:valid_sha ~observed_at_remote
          with
          | None -> false
          | Some anchor ->
              String.equal (Anchor.sha anchor) valid_sha
              && Bool.equal (Anchor.is_remote anchor) observed_at_remote)
        observations)

let () =
  QCheck2.Test.check_exn anchor_make_is_total_across_observation_sequences
