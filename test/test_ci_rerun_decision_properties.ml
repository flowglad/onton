(* @archlint.module test
   @archlint.domain ci-rerun *)

open Base
open Onton_core
open Onton_core.Types

let gen_text = QCheck2.Gen.string_size (QCheck2.Gen.int_range 0 80)

let gen_details_url =
  let open QCheck2.Gen in
  oneof_weighted
    [
      (2, return None);
      ( 5,
        map
          (fun id ->
            Some
              (Printf.sprintf "https://github.com/o/r/actions/runs/%d/job/123"
                 id))
          (int_range 0 12) );
      (3, map (fun value -> Some value) gen_text);
    ]

let gen_check =
  let open QCheck2.Gen in
  let* name = gen_text in
  let* details_url = gen_details_url in
  return
    Ci_check.
      {
        name;
        conclusion = "failure";
        details_url;
        description = None;
        started_at = None;
        id = None;
      }

let gen_checks = QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 40) gen_check

let run_id (check : Ci_check.t) =
  Option.bind check.Ci_check.details_url
    ~f:Ci_rerun_decision.workflow_run_id_from_url

let prop_url_parser_total =
  QCheck2.Test.make ~name:"workflow run URL parser is total" ~count:2_000
    gen_text (fun value ->
      let _ = Ci_rerun_decision.workflow_run_id_from_url value in
      true)

let prop_status_parser_total =
  QCheck2.Test.make ~name:"workflow status response parser is total"
    ~count:2_000 gen_text (fun value ->
      let _ = Ci_rerun_decision.workflow_status_of_response value in
      true)

let prop_dedupe_total =
  QCheck2.Test.make ~name:"workflow check dedupe is total" ~count:1_000
    gen_checks (fun checks ->
      let _ = Ci_rerun_decision.unique_workflow_checks checks in
      true)

let prop_dedupe_idempotent =
  QCheck2.Test.make ~name:"workflow check dedupe is idempotent" ~count:1_000
    gen_checks (fun checks ->
      let once = Ci_rerun_decision.unique_workflow_checks checks in
      let twice = Ci_rerun_decision.unique_workflow_checks once in
      List.equal Ci_check.equal once twice)

let gen_interleaved_run_ids =
  QCheck2.Gen.list_size
    (QCheck2.Gen.int_range 0 80)
    QCheck2.Gen.(option (int_range 0 8))

let check_of_run_id index run_id =
  Ci_check.
    {
      name = Printf.sprintf "check-%d" index;
      conclusion = "failure";
      details_url =
        Option.map run_id ~f:(fun id ->
            Printf.sprintf "https://github.com/o/r/actions/runs/%d/job/%d" id
              index);
      description = None;
      started_at = None;
      id = None;
    }

let prop_dedupe_interleavings =
  QCheck2.Test.make
    ~name:"dedupe preserves first checks and order across interleavings"
    ~count:1_000 gen_interleaved_run_ids (fun run_ids ->
      let checks = List.mapi run_ids ~f:check_of_run_id in
      let expected =
        List.filter_mapi checks ~f:(fun index check ->
            match run_id check with
            | None -> Some check
            | Some id ->
                let appeared_earlier =
                  List.take checks index
                  |> List.exists ~f:(fun earlier ->
                      Option.equal Int.equal (run_id earlier) (Some id))
                in
                if appeared_earlier then None else Some check)
      in
      List.equal Ci_check.equal
        (Ci_rerun_decision.unique_workflow_checks checks)
        expected)

let prop_identifiable_runs_unique =
  QCheck2.Test.make
    ~name:"at most one rerun request is planned for each workflow run"
    ~count:1_000 gen_checks (fun checks ->
      let ids =
        Ci_rerun_decision.unique_workflow_checks checks
        |> List.filter_map ~f:run_id
      in
      let unique_ids = Set.of_list (module Int) ids in
      List.length ids = Set.length unique_ids)

let prop_unidentifiable_checks_retained =
  QCheck2.Test.make
    ~name:"checks without workflow identities are retained for error reporting"
    ~count:1_000 gen_checks (fun checks ->
      let count_unidentified values =
        List.count values ~f:(fun check -> Option.is_none (run_id check))
      in
      Int.equal
        (count_unidentified checks)
        (count_unidentified (Ci_rerun_decision.unique_workflow_checks checks)))

let prop_status_boundaries =
  QCheck2.Test.make ~name:"only a completed workflow status permits a rerun"
    ~count:1 QCheck2.Gen.unit (fun () ->
      Ci_rerun_decision.equal_workflow_status
        (Ci_rerun_decision.workflow_status_of_response
           {|{"status":"completed"}|})
        Ci_rerun_decision.Completed
      && Ci_rerun_decision.equal_workflow_status
           (Ci_rerun_decision.workflow_status_of_response
              {|{"status":"in_progress"}|})
           Ci_rerun_decision.Pending
      && Ci_rerun_decision.equal_workflow_status
           (Ci_rerun_decision.workflow_status_of_response {|{"status":null}|})
           Ci_rerun_decision.Malformed
      && Ci_rerun_decision.equal_workflow_status
           (Ci_rerun_decision.workflow_status_of_response "not-json")
           Ci_rerun_decision.Malformed)

let prop_url_boundaries =
  QCheck2.Test.make ~name:"workflow run URL boundaries" ~count:1
    QCheck2.Gen.unit (fun () ->
      Option.equal Int.equal
        (Ci_rerun_decision.workflow_run_id_from_url
           "https://github.com/o/r/actions/runs/42/job/7")
        (Some 42)
      && Option.equal Int.equal
           (Ci_rerun_decision.workflow_run_id_from_url
              "https://github.com/o/r/actions/runs/42?attempt=2")
           (Some 42)
      && Option.equal Int.equal
           (Ci_rerun_decision.workflow_run_id_from_url
              "https://github.com/o/r/actions/runs/42#details")
           (Some 42)
      && Option.equal Int.equal
           (Ci_rerun_decision.workflow_run_id_from_url
              "https://github.com/o/r/actions/runs/42")
           (Some 42)
      && Option.is_none
           (Ci_rerun_decision.workflow_run_id_from_url
              "https://github.com/o/r/actions/runs/")
      && Option.is_none
           (Ci_rerun_decision.workflow_run_id_from_url
              "https://github.com/o/r/actions/runs/not-a-number/job/7")
      && Option.is_none
           (Ci_rerun_decision.workflow_run_id_from_url
              "https://github.com/o/r/actions/runs/42x")
      && Option.is_none
           (Ci_rerun_decision.workflow_run_id_from_url
              ("https://github.com/o/r/actions/runs/" ^ String.make 128 '9')))

let () =
  QCheck_base_runner.run_tests_main
    [
      prop_url_parser_total;
      prop_status_parser_total;
      prop_dedupe_total;
      prop_dedupe_idempotent;
      prop_dedupe_interleavings;
      prop_identifiable_runs_unique;
      prop_unidentifiable_checks_retained;
      prop_status_boundaries;
      prop_url_boundaries;
    ]
