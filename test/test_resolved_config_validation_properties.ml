(* @archlint.module test
   @archlint.domain resolved-config *)

open Onton_core

let validation_is_total =
  QCheck2.Test.make ~name:"automerge timeout validation is total" ~count:1000
    QCheck2.Gen.float (fun timeout ->
      try
        ignore (Resolved_config_validation.automerge_timeout_error timeout);
        true
      with _ -> false)

let validation_matches_contract =
  QCheck2.Test.make
    ~name:"automerge timeout validation accepts exactly positive finite values"
    ~count:1000 QCheck2.Gen.float (fun timeout ->
      let expected = Float.is_finite timeout && Float.compare timeout 0. > 0 in
      Bool.equal expected
        (Option.is_none
           (Resolved_config_validation.automerge_timeout_error timeout)))

let boundary_values_match_contract =
  QCheck2.Test.make ~name:"automerge timeout validation handles boundaries"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let invalid =
        [ 0.; -0.; -1.; Float.nan; Float.infinity; Float.neg_infinity ]
      in
      let near_zero = Float.next_after 0. Float.infinity in
      List.for_all
        (fun timeout ->
          Option.is_some
            (Resolved_config_validation.automerge_timeout_error timeout))
        invalid
      && Option.is_none
           (Resolved_config_validation.automerge_timeout_error near_zero))

let repeated_validation_matches_contract =
  QCheck2.Test.make
    ~name:"repeated automerge timeout validation preserves the contract"
    ~count:300
    QCheck2.Gen.(list_size (int_range 1 40) float)
    (fun timeouts ->
      let validate = Resolved_config_validation.automerge_timeout_error in
      List.for_all
        (fun timeout ->
          let first = validate timeout in
          let second = validate timeout in
          let should_error =
            (not (Float.is_finite timeout)) || Float.compare timeout 0. <= 0
          in
          Option.equal String.equal first second
          && Bool.equal should_error (Option.is_some first))
        timeouts)

let () =
  QCheck2.Test.check_exn validation_is_total;
  QCheck2.Test.check_exn validation_matches_contract;
  QCheck2.Test.check_exn boundary_values_match_contract;
  QCheck2.Test.check_exn repeated_validation_matches_contract
