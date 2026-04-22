open Base
open Onton.Types

let () =
  let open QCheck2 in
  let open Onton_test_support.Test_generators in
  let tests =
    [
      (* merge_ready is passed through from PR state *)
      Test.make ~name:"merge_ready passed through" ~count:500 gen_pr_state
        (fun pr ->
          let result = Onton.Poller.poll ~was_merged:false pr in
          Bool.equal result.Onton.Poller.merge_ready
            (Onton.Pr_state.merge_ready pr));
      Test.make ~name:"is_draft passed through" ~count:500 gen_pr_state
        (fun pr ->
          let result = Onton.Poller.poll ~was_merged:false pr in
          Bool.equal result.Onton.Poller.is_draft (Onton.Pr_state.is_draft pr));
      (* checks_passing is passed through from PR state *)
      Test.make ~name:"checks_passing passed through" ~count:500 gen_pr_state
        (fun pr ->
          let result = Onton.Poller.poll ~was_merged:false pr in
          Bool.equal result.Onton.Poller.checks_passing
            (Onton.Pr_state.checks_passing pr));
      (* ci_checks are passed through from PR state *)
      Test.make ~name:"ci_checks passed through" ~count:500 gen_pr_state
        (fun pr ->
          let result = Onton.Poller.poll ~was_merged:false pr in
          List.equal Ci_check.equal result.Onton.Poller.ci_checks
            pr.Onton.Pr_state.ci_checks);
      (* -- derive_check_status semantics ------------------------------- *)
      (* Failing iff any conclusion is in failure_conclusions *)
      Test.make ~name:"derive_check_status: Failing iff any is_failure"
        ~count:1000
        Gen.(list_size (int_range 0 8) gen_ci_check)
        (fun checks ->
          let status = Onton.Pr_state.derive_check_status checks in
          let any_failure = List.exists checks ~f:Ci_check.is_failure in
          Bool.equal
            (Onton.Pr_state.equal_check_status status Onton.Pr_state.Failing)
            any_failure);
      (* Passing iff non-empty and every conclusion is in success_conclusions *)
      Test.make ~name:"derive_check_status: Passing iff non-empty all_success"
        ~count:1000
        Gen.(list_size (int_range 0 8) gen_ci_check)
        (fun checks ->
          let status = Onton.Pr_state.derive_check_status checks in
          let all_success =
            (not (List.is_empty checks))
            && List.for_all checks ~f:Ci_check.is_success
          in
          Bool.equal
            (Onton.Pr_state.equal_check_status status Onton.Pr_state.Passing)
            all_success);
      (* Cancelled-only lists must NOT be Failing (regression for the
         orchestrator needs-intervention loop seen in production). *)
      Test.make ~name:"derive_check_status: cancelled-only is not Failing"
        ~count:500
        Gen.(
          list_size (int_range 1 6)
            (map
               (fun (name, details_url, description) ->
                 Ci_check.
                   {
                     name;
                     conclusion = "cancelled";
                     details_url;
                     description;
                     started_at = None;
                     id = None;
                   })
               (triple
                  (string_size ~gen:(char_range 'a' 'z') (int_range 3 10))
                  (option (pure "https://ci.example.com/x"))
                  (option (string_size ~gen:printable (int_range 0 20))))))
        (fun checks ->
          let status = Onton.Pr_state.derive_check_status checks in
          not (Onton.Pr_state.equal_check_status status Onton.Pr_state.Failing));
      (* Empty list -> Pending (never Passing, never Failing) *)
      Test.make ~name:"derive_check_status: empty is Pending" ~count:1
        Gen.(pure ())
        (fun () ->
          Onton.Pr_state.equal_check_status
            (Onton.Pr_state.derive_check_status [])
            Onton.Pr_state.Pending);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t)
