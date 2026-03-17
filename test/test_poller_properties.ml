open Base
open Onton.Types

let gen_addressed_ids =
  QCheck2.Gen.(
    map
      (fun ids ->
        Set.of_list (module Comment_id) (List.map ids ~f:Comment_id.of_int))
      (list_small (int_range 1 10_000)))

let () =
  let open QCheck2 in
  let tests =
    [
      (* mergeable is passed through from PR state *)
      Test.make ~name:"mergeable passed through" ~count:500
        (Gen.pair Onton_test_support.Test_generators.gen_pr_state
           gen_addressed_ids) (fun (pr, addressed_ids) ->
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          Bool.equal result.Onton.Poller.mergeable (Onton.Github.mergeable pr));
      (* checks_passing is passed through from PR state *)
      Test.make ~name:"checks_passing passed through" ~count:500
        (Gen.pair Onton_test_support.Test_generators.gen_pr_state
           gen_addressed_ids) (fun (pr, addressed_ids) ->
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          Bool.equal result.Onton.Poller.checks_passing
            (Onton.Github.checks_passing pr));
      (* ci_checks are passed through from PR state *)
      Test.make ~name:"ci_checks passed through" ~count:500
        (Gen.pair Onton_test_support.Test_generators.gen_pr_state
           gen_addressed_ids) (fun (pr, addressed_ids) ->
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          List.equal Ci_check.equal result.Onton.Poller.ci_checks
            pr.Onton.Github.Pr_state.ci_checks);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t)
