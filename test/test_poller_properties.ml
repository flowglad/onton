open Base
open Onton.Types

let () =
  let open QCheck2 in
  let tests =
    [
      (* mergeable is passed through from PR state *)
      Test.make ~name:"mergeable passed through" ~count:500
        Onton_test_support.Test_generators.gen_pr_state (fun pr ->
          let result = Onton.Poller.poll ~was_merged:false pr in
          Bool.equal result.Onton.Poller.mergeable (Onton.Pr_state.mergeable pr));
      (* merge_ready is passed through from PR state *)
      Test.make ~name:"merge_ready passed through" ~count:500
        Onton_test_support.Test_generators.gen_pr_state (fun pr ->
          let result = Onton.Poller.poll ~was_merged:false pr in
          Bool.equal result.Onton.Poller.merge_ready
            (Onton.Pr_state.merge_ready pr));
      (* checks_passing is passed through from PR state *)
      Test.make ~name:"checks_passing passed through" ~count:500
        Onton_test_support.Test_generators.gen_pr_state (fun pr ->
          let result = Onton.Poller.poll ~was_merged:false pr in
          Bool.equal result.Onton.Poller.checks_passing
            (Onton.Pr_state.checks_passing pr));
      (* ci_checks are passed through from PR state *)
      Test.make ~name:"ci_checks passed through" ~count:500
        Onton_test_support.Test_generators.gen_pr_state (fun pr ->
          let result = Onton.Poller.poll ~was_merged:false pr in
          List.equal Ci_check.equal result.Onton.Poller.ci_checks
            pr.Onton.Pr_state.ci_checks);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t)
