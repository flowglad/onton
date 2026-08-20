(* @archlint.module test
   @archlint.domain sourcehut-builds *)

open Onton_core

let arbitrary_string = QCheck2.Gen.string_size (QCheck2.Gen.int_bound 500)

let totality =
  QCheck2.Test.make ~name:"sourcehut build decoders are total" ~count:1000
    arbitrary_string (fun body ->
      ignore (Sourcehut_builds.jobs_of_response body);
      ignore (Sourcehut_builds.job_of_response body);
      ignore (Sourcehut_builds.submit_id_of_response body);
      true)

let known_status_mappings =
  QCheck2.Test.make
    ~name:"sourcehut known statuses map to their exact CI conclusions"
    ~count:500
    QCheck2.Gen.(
      oneof_list
        [
          ("SUCCESS", "success");
          ("FAILED", "failure");
          ("TIMEOUT", "timed_out");
          ("CANCELLED", "cancelled");
          ("RUNNING", "in_progress");
          ("QUEUED", "queued");
          ("PENDING", "pending");
        ])
    (fun (status, expected) ->
      String.equal (Sourcehut_builds.conclusion_of_status status) expected)

let unknown_statuses_are_pending =
  QCheck2.Test.make ~name:"sourcehut unknown statuses map to pending" ~count:500
    arbitrary_string (fun suffix ->
      String.equal
        (Sourcehut_builds.conclusion_of_status ("UNKNOWN-" ^ suffix))
        "pending")

let build_decisions_are_total =
  QCheck2.Test.make
    ~name:"sourcehut build decisions are total over generated jobs" ~count:500
    QCheck2.Gen.(pair (int_bound 1_000_000) arbitrary_string)
    (fun (id, status) ->
      let job : Sourcehut_builds.job =
        Sourcehut_builds.
          {
            id;
            status;
            note = "https://git.sr.ht/~alice/demo/commit/abcdef012345";
            tags = [ "demo"; "commits"; "patch-1"; "build.yml" ];
            created = None;
            owner = "~alice";
            manifest = "build.yml";
            visibility = "PUBLIC";
            logs = [ status ];
          }
      in
      let checks =
        Sourcehut_builds.checks_for_commit ~owner:"alice" ~repo:"demo"
          ~branch:(Types.Branch.of_string "patch-1")
          ~sha:"abcdef012345" [ job ]
      in
      let source = Sourcehut_builds.log_source job in
      List.length checks = 1
      && source.Ci_log_digest.log = Some status
      && List.is_empty source.Ci_log_digest.annotations)

let fixture =
  {|{"data":{"jobs":{"results":[{"id":42,"status":"FAILED","note":"[abcdef0][0]\n\n[0]: https://git.sr.ht/~alice/demo/commit/abcdef012345","tags":["demo","commits","patch-1","not-the-manifest"],"created":"2026-08-20T12:00:00Z","manifest":"build.yml","owner":{"canonicalName":"~alice"}}],"cursor":null}}}|}

let exact_commit_join =
  QCheck2.Test.make ~name:"sourcehut jobs join to the exact repo branch and SHA"
    ~count:1 QCheck2.Gen.unit (fun () ->
      match Sourcehut_builds.jobs_of_response fixture with
      | Error _ -> false
      | Ok (jobs, _) -> (
          let checks =
            Sourcehut_builds.checks_for_commit ~owner:"alice" ~repo:"demo"
              ~branch:(Types.Branch.of_string "patch-1")
              ~sha:"abcdef012345" jobs
          in
          match checks with
          | [ check ] ->
              check.Types.Ci_check.id = Some 42
              && String.equal check.Types.Ci_check.name
                   "builds.sr.ht / build.yml"
              && String.equal check.Types.Ci_check.conclusion "failure"
              && List.is_empty
                   (Sourcehut_builds.checks_for_commit ~owner:"alice"
                      ~repo:"demo"
                      ~branch:(Types.Branch.of_string "patch-1")
                      ~sha:"abcdef012346" jobs)
          | _ -> false))

let rerun_supersedes_original =
  QCheck2.Test.make
    ~name:"the newest SourceHut job supersedes an older run of one manifest"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let job id status : Sourcehut_builds.job =
        Sourcehut_builds.
          {
            id;
            status;
            note =
              "[abcdef0][0]\n\n\
               [0]: https://git.sr.ht/~alice/demo/commit/abcdef012345";
            tags = [ "demo"; "commits"; "patch-1"; "build.yml" ];
            created = None;
            owner = "~alice";
            manifest = "build.yml";
            visibility = "PUBLIC";
            logs = [];
          }
      in
      match
        Sourcehut_builds.checks_for_commit ~owner:"alice" ~repo:"demo"
          ~branch:(Types.Branch.of_string "patch-1")
          ~sha:"abcdef012345"
          [ job 41 "FAILED"; job 42 "SUCCESS" ]
      with
      | [ check ] ->
          check.Types.Ci_check.id = Some 42
          && String.equal check.Types.Ci_check.conclusion "success"
      | _ -> false)

let tag_order_cannot_alias_manifests =
  QCheck2.Test.make
    ~name:"sourcehut manifest identity is independent of tag ordering" ~count:1
    QCheck2.Gen.unit (fun () ->
      let job id manifest status : Sourcehut_builds.job =
        Sourcehut_builds.
          {
            id;
            status;
            note =
              "[abcdef0][0]\n\n\
               [0]: https://git.sr.ht/~alice/demo/commit/abcdef012345";
            tags = [ "shared-last"; "patch-1"; "commits"; "demo" ];
            created = None;
            owner = "~alice";
            manifest;
            visibility = "PUBLIC";
            logs = [];
          }
      in
      let checks =
        Sourcehut_builds.checks_for_commit ~owner:"alice" ~repo:"demo"
          ~branch:(Types.Branch.of_string "patch-1")
          ~sha:"abcdef012345"
          [ job 42 "linux.yml" "SUCCESS"; job 43 "macos.yml" "FAILED" ]
      in
      List.length checks = 2
      && Stdlib.List.exists
           (fun check ->
             String.equal check.Types.Ci_check.name "builds.sr.ht / linux.yml")
           checks
      && Stdlib.List.exists
           (fun check ->
             String.equal check.Types.Ci_check.name "builds.sr.ht / macos.yml"
             && String.equal check.Types.Ci_check.conclusion "failure")
           checks)

let () =
  QCheck_base_runner.run_tests_main
    [
      totality;
      known_status_mappings;
      unknown_statuses_are_pending;
      build_decisions_are_total;
      exact_commit_join;
      rerun_supersedes_original;
      tag_order_cannot_alias_manifests;
    ]
