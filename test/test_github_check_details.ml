(* @archlint.module test
   @archlint.domain github *)

open Base
open Onton_core

let assert_ok_annotations body ~f =
  match Onton.Github.parse_check_annotations_response body with
  | Ok annotations -> f annotations
  | Error err ->
      failwith
        (Printf.sprintf "unexpected parse error: %s"
           (Onton.Github.show_error err))

let test_field_mapping_and_nulls () =
  let body =
    {|
[
  {
    "path": "lib/foo.ml",
    "start_line": 42,
    "annotation_level": "failure",
    "message": "expected int",
    "end_line": 42,
    "raw_details": "extra detail"
  },
  {
    "path": null,
    "start_line": null,
    "annotation_level": "warning",
    "message": "ignored shape is still mapped",
    "unexpected": {"nested": true}
  }
]
|}
  in
  assert_ok_annotations body ~f:(function
    | [ first; second ] ->
        assert (
          Option.equal String.equal first.Ci_log_digest.path (Some "lib/foo.ml"));
        assert (Option.equal Int.equal first.Ci_log_digest.line (Some 42));
        assert (String.equal first.Ci_log_digest.level "failure");
        assert (String.equal first.Ci_log_digest.message "expected int");
        assert (Option.is_none second.Ci_log_digest.path);
        assert (Option.is_none second.Ci_log_digest.line);
        assert (String.equal second.Ci_log_digest.level "warning");
        assert (
          String.equal second.Ci_log_digest.message
            "ignored shape is still mapped")
    | annotations ->
        failwith
          (Printf.sprintf "unexpected annotation count: %d"
             (List.length annotations)))

let test_empty_array () =
  assert_ok_annotations "[]" ~f:(function
    | [] -> ()
    | _ -> failwith "expected empty annotation list")

let test_malformed_json () =
  match Onton.Github.parse_check_annotations_response "{not json" with
  | Error (Onton.Github.Json_parse_error _) -> ()
  | Ok _ -> failwith "expected Json_parse_error"
  | Error
      (( Onton.Github.Http_error _ | Onton.Github.Graphql_error _
       | Onton.Github.Timeout _ | Onton.Github.Transport_error _ ) as err) ->
      failwith
        (Printf.sprintf "expected Json_parse_error, got %s"
           (Onton.Github.show_error err))

let check_run ?(suite = 1) ~id ~name conclusion =
  Printf.sprintf
    {|{"__typename":"CheckRun","databaseId":%d,"name":%S,
       "checkSuite":{"databaseId":%d},"conclusion":%S}|}
    id name suite conclusion

let rollup ~truncated nodes =
  Printf.sprintf
    {|{"state":"FAILURE","contexts":{"pageInfo":{"hasNextPage":%b},
       "nodes":[%s]}}|}
    truncated
    (String.concat nodes ~sep:",")

let pr_response ~truncated nodes =
  Printf.sprintf
    {|{"data":{"repository":{"mergeQueue":{"id":"queue"},
       "pullRequest":{"id":"pr","state":"OPEN","mergeable":"MERGEABLE",
         "mergeStateStatus":"CLEAN","baseRefName":"main",
         "headRefName":"feature","headRefOid":"head",
         "mergeQueueEntry":{"id":"entry","state":"QUEUED","position":1},
         "commits":{"nodes":[{"commit":{"statusCheckRollup":%s}}]}}}}}|}
    (rollup ~truncated nodes)

let parse_pr ~truncated nodes =
  match
    Onton.Github.parse_response ~owner:"flowglad" (pr_response ~truncated nodes)
  with
  | Ok st -> st
  | Error err -> failwith (Onton.Github.show_error err)

let polled_orchestrator st =
  let main = Types.Branch.of_string "main" in
  let pid = Types.Patch_id.of_string "1" in
  let orch = Onton.Orchestrator.create ~patches:[] ~main_branch:main in
  let orch =
    Onton.Orchestrator.add_agent orch ~patch_id:pid
      ~branch:(Types.Branch.of_string "feature")
      ~base_branch:main ~pr_number:(Types.Pr_number.of_int 1)
  in
  let orch, _, _ =
    Onton.Patch_controller.apply_poll_result orch pid
      {
        Onton.Patch_controller.poll_result = Poller.poll ~was_merged:false st;
        base_branch = Some main;
        native_stack = false;
        branch_in_root = false;
        worktree_path = None;
      }
  in
  orch

let queued_agent st =
  Onton.Orchestrator.agent (polled_orchestrator st)
    (Types.Patch_id.of_string "1")

let test_replaced_cancellation_allows_automerge_enqueue () =
  let st =
    parse_pr ~truncated:false
      [
        check_run ~id:1 ~name:"build" "CANCELLED";
        check_run ~id:2 ~name:"build" "SUCCESS";
      ]
  in
  let st = { st with Pr_state.merge_queue_entry = None } in
  let orch =
    Onton.Orchestrator.set_automerge_enabled (polled_orchestrator st)
      (Types.Patch_id.of_string "1")
      true
  in
  let orch, _ = Onton.Patch_controller.reconcile_automerge orch ~now:0.0 in
  let _, decisions =
    Onton.Patch_controller.reconcile_automerge orch ~now:121.0
  in
  match decisions with
  | [ Onton.Patch_controller.Github_merge { action; _ } ] ->
      assert (
        Onton.Patch_controller.equal_merge_action action
          Onton.Patch_controller.Enqueue)
  | [ Onton.Patch_controller.Git_integrate _ ] | [] | _ :: _ :: _ ->
      failwith "expected an automerge enqueue after the idle window"

let test_replaced_cancellation_keeps_pr_queued () =
  let st =
    parse_pr ~truncated:false
      [
        check_run ~id:2 ~name:"build" "SUCCESS";
        check_run ~id:1 ~name:"build" "CANCELLED";
      ]
  in
  assert (Pr_state.checks_passing st && Pr_state.merge_ready st);
  assert (List.length st.Pr_state.ci_checks = 1);
  assert (
    List.is_empty
      (Onton.Patch_controller.dequeue_merge_queue_reasons (queued_agent st)
         ~main_branch:(Types.Branch.of_string "main")
         ~entry_id:"entry"))

let test_replacement_on_later_page () =
  let first =
    check_run ~id:1 ~name:"build" "CANCELLED"
    :: List.init 99 ~f:(fun i ->
        check_run ~id:(10 + i) ~name:(Printf.sprintf "test-%d" i) "SUCCESS")
  in
  let st = parse_pr ~truncated:true first in
  assert (not (Pr_state.merge_ready st));
  let page =
    Printf.sprintf
      {|{"data":{"repository":{"object":{"statusCheckRollup":%s}}}}|}
      (rollup ~truncated:false [ check_run ~id:200 ~name:"build" "SUCCESS" ])
  in
  let second =
    match Onton.Github.parse_contexts_page page with
    | Ok (checks, false, _) -> checks
    | Ok _ -> failwith "unexpected pagination state"
    | Error err -> failwith (Onton.Github.show_error err)
  in
  let st =
    Pr_state.with_resolved_checks st ~all_checks:(st.Pr_state.ci_checks @ second)
  in
  assert (Pr_state.checks_passing st && Pr_state.merge_ready st);
  assert (List.length st.Pr_state.ci_checks = 100);
  assert (
    not
      (Onton.Patch_controller.should_dequeue_merge_queue (queued_agent st)
         ~main_branch:(Types.Branch.of_string "main")
         ~entry_id:"entry"))

let test_current_or_unidentified_checks_still_block () =
  List.iter
    [
      [
        check_run ~id:1 ~name:"build" "SUCCESS";
        check_run ~id:2 ~name:"build" "FAILURE";
      ];
      [
        check_run ~id:1 ~name:"build" "SUCCESS";
        check_run ~id:2 ~name:"build" "PENDING";
      ];
      [
        check_run ~id:1 ~name:"build" "CANCELLED";
        check_run ~id:2 ~name:"test" "SUCCESS";
      ];
      [
        check_run ~id:1 ~name:"build" "CANCELLED";
        check_run ~suite:2 ~id:2 ~name:"build" "SUCCESS";
      ];
      [
        check_run ~suite:1 ~id:1 ~name:"build" "FAILURE";
        check_run ~suite:2 ~id:2 ~name:"build" "SUCCESS";
      ];
      [
        {|{"__typename":"CheckRun","name":"build","databaseId":1,
          "checkSuite":{},"conclusion":"FAILURE"}|};
        check_run ~id:2 ~name:"build" "SUCCESS";
      ];
      [
        check_run ~suite:0 ~id:1 ~name:"build" "FAILURE";
        check_run ~suite:0 ~id:2 ~name:"build" "SUCCESS";
      ];
      [
        {|{"__typename":"CheckRun","name":"build","databaseId":1,
          "conclusion":"CANCELLED"}|};
        check_run ~id:2 ~name:"build" "SUCCESS";
      ];
    ]
    ~f:(fun nodes ->
      let st = parse_pr ~truncated:false nodes in
      assert (not (Pr_state.merge_ready st));
      assert (
        Onton.Patch_controller.should_dequeue_merge_queue (queued_agent st)
          ~main_branch:(Types.Branch.of_string "main")
          ~entry_id:"entry"))

let test_merge_group_feedback_uses_current_runs () =
  let nodes =
    [
      check_run ~id:1 ~name:"build" "FAILURE";
      check_run ~id:2 ~name:"build" "SUCCESS";
      check_run ~id:3 ~name:"test" "FAILURE";
    ]
  in
  let body =
    Printf.sprintf
      {|{"data":{"repository":{"pullRequest":{"timelineItems":{"nodes":[
       {"beforeCommit":{"oid":"merge-head","statusCheckRollup":%s}}
       ]}}}}}|}
      (rollup ~truncated:false nodes)
  in
  match Onton.Github.parse_merge_queue_removal_response body with
  | Ok [ check ] ->
      assert (String.equal check.Types.Ci_check.name "test");
      assert (Option.equal Int.equal check.Types.Ci_check.id (Some 3))
  | Ok _ -> failwith "expected only the current failing check"
  | Error err -> failwith (Onton.Github.show_error err)

let test_same_named_workflows_preserve_failure_feedback () =
  let failed = check_run ~suite:1 ~id:1 ~name:"build" "FAILURE" in
  let passed = check_run ~suite:2 ~id:2 ~name:"build" "SUCCESS" in
  let st = parse_pr ~truncated:false [ failed; passed ] in
  assert (Pr_state.equal_check_status st.Pr_state.check_status Pr_state.Failing);
  assert (not (Pr_state.checks_passing st || Pr_state.merge_ready st));
  let result = Poller.poll ~was_merged:false st in
  assert (List.exists result.Poller.ci_checks ~f:Types.Ci_check.is_failure);
  assert (List.length result.Poller.ci_checks = 2);
  let body =
    Printf.sprintf
      {|{"data":{"repository":{"pullRequest":{"timelineItems":{"nodes":[
       {"beforeCommit":{"oid":"merge-head","statusCheckRollup":%s}}
       ]}}}}}|}
      (rollup ~truncated:false [ failed; passed ])
  in
  match Onton.Github.parse_merge_queue_removal_response body with
  | Ok [ check ] ->
      assert (Option.equal Int.equal check.Types.Ci_check.id (Some 1));
      assert (
        Option.equal Int.equal check.Types.Ci_check.check_suite_id (Some 1))
  | Ok _ -> failwith "expected the independent workflow's failure"
  | Error err -> failwith (Onton.Github.show_error err)

let test_private_producer_checks_preserve_pr_and_failure_feedback () =
  let st =
    parse_pr ~truncated:false
      [
        check_run ~suite:10 ~id:1 ~name:"Flowglad Review" "SUCCESS";
        check_run ~suite:20 ~id:2 ~name:"build" "FAILURE";
        check_run ~suite:20 ~id:3 ~name:"build" "SUCCESS";
        check_run ~suite:30 ~id:4 ~name:"test" "FAILURE";
      ]
  in
  assert (
    Option.equal Types.Branch.equal st.Pr_state.head_branch
      (Some (Types.Branch.of_string "feature")));
  assert (not (Pr_state.merge_ready st));
  let result = Poller.poll ~was_merged:false st in
  assert (List.length result.Poller.ci_checks = 3);
  match List.filter result.Poller.ci_checks ~f:Types.Ci_check.is_failure with
  | [ check ] ->
      assert (String.equal check.Types.Ci_check.name "test");
      assert (Option.equal Int.equal check.Types.Ci_check.id (Some 4))
  | _ -> failwith "expected actionable feedback from the current failing run"

let () =
  test_field_mapping_and_nulls ();
  test_empty_array ();
  test_malformed_json ();
  test_replaced_cancellation_keeps_pr_queued ();
  test_replaced_cancellation_allows_automerge_enqueue ();
  test_replacement_on_later_page ();
  test_current_or_unidentified_checks_still_block ();
  test_merge_group_feedback_uses_current_runs ();
  test_same_named_workflows_preserve_failure_feedback ();
  test_private_producer_checks_preserve_pr_and_failure_feedback ();
  Stdlib.print_endline "test_github_check_details: OK"
