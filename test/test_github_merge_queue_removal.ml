(* @archlint.module test
   @archlint.domain review-backend *)

(* Tests for [parse_merge_queue_removal_response].

   GitHub runs merge-queue CI on the ephemeral merge-group commit, not the PR
   head, so a PR kicked from the queue shows all-green PR-head checks while the
   real failures live on [RemovedFromMergeQueueEvent.beforeCommit]. This parser
   reaches that commit's rollup and returns only its failing checks, so the
   patch agent gets the real check names/urls/ids instead of the synthetic
   "GitHub merge queue" placeholder. See [lib/github.ml]. *)

open Base
module Ci_check = Onton_core.Types.Ci_check

let parse = Onton.Github.parse_merge_queue_removal_response

let removal_body ~nodes =
  Printf.sprintf
    {|{ "data": { "repository": { "pullRequest": {
      "timelineItems": { "nodes": %s }
    } } } }|}
    nodes

(* One removal event whose merge-group commit ran a mix of pass/fail checks:
   one passing CheckRun, one failing CheckRun, one failing legacy StatusContext.
   Only the two failures must survive. *)
let mixed_event =
  {|[{
    "createdAt": "2026-06-24T20:55:00Z",
    "reason": "The merge commit failed required status checks.",
    "beforeCommit": {
      "oid": "mergegroupsha123",
      "statusCheckRollup": {
        "state": "FAILURE",
        "contexts": {
          "pageInfo": { "hasNextPage": false },
          "nodes": [
            { "__typename": "CheckRun", "databaseId": 111, "name": "Lint",
              "conclusion": "SUCCESS", "detailsUrl": "https://gh/checks/111",
              "text": null, "startedAt": null },
            { "__typename": "CheckRun", "databaseId": 222,
              "name": "integration-tests", "conclusion": "FAILURE",
              "detailsUrl": "https://gh/checks/222", "text": "assertion failed",
              "startedAt": null },
            { "__typename": "StatusContext", "context": "legacy/ci",
              "state": "ERROR", "targetUrl": "https://gh/status/legacy",
              "description": "legacy job errored", "createdAt": null }
          ]
        }
      }
    }
  }]|}

let test_mixed_returns_only_failures () =
  match parse (removal_body ~nodes:mixed_event) with
  | Error e ->
      Stdlib.Printf.eprintf "  FAIL: mixed fixture errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1
  | Ok checks ->
      assert (List.length checks = 2);
      let find name =
        List.find checks ~f:(fun (c : Ci_check.t) ->
            String.equal c.Ci_check.name name)
      in
      (match find "integration-tests" with
      | Some c ->
          (* CheckRun: real conclusion, details_url, and databaseId (dedup key). *)
          assert (String.equal c.Ci_check.conclusion "failure");
          assert (
            match c.Ci_check.details_url with
            | Some "https://gh/checks/222" -> true
            | _ -> false);
          assert (match c.Ci_check.id with Some 222 -> true | _ -> false)
      | None ->
          Stdlib.Printf.eprintf "  FAIL: failing CheckRun not returned\n";
          Stdlib.exit 1);
      (match find "legacy/ci" with
      | Some c ->
          (* StatusContext: ERROR is a failure; no stable numeric id. *)
          assert (String.equal c.Ci_check.conclusion "error");
          assert (Option.is_none c.Ci_check.id)
      | None ->
          Stdlib.Printf.eprintf "  FAIL: failing StatusContext not returned\n";
          Stdlib.exit 1);
      (* The passing check must be filtered out. *)
      assert (Option.is_none (find "Lint"));
      Stdlib.print_endline "  mixed rollup → only failures: OK"

let truncated_event =
  {|[{
    "createdAt": "2026-06-24T20:55:00Z",
    "reason": "The merge commit failed required status checks.",
    "beforeCommit": {
      "oid": "mergegroupsha123",
      "statusCheckRollup": {
        "state": "FAILURE",
        "contexts": {
          "pageInfo": { "hasNextPage": true },
          "nodes": [
            { "__typename": "CheckRun", "databaseId": 222,
              "name": "integration-tests", "conclusion": "FAILURE",
              "detailsUrl": "https://gh/checks/222", "text": "assertion failed",
              "startedAt": null }
          ]
        }
      }
    }
  }]|}

let test_truncated_rollup_is_ok_empty () =
  match parse (removal_body ~nodes:truncated_event) with
  | Ok [] ->
      Stdlib.print_endline "  truncated rollup → Ok [] keeps placeholder: OK"
  | Ok _ ->
      Stdlib.Printf.eprintf "  FAIL: truncated rollup returned partial checks\n";
      Stdlib.exit 1
  | Error e ->
      Stdlib.Printf.eprintf "  FAIL: truncated rollup errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1

let test_empty_timeline_is_ok_empty () =
  match parse (removal_body ~nodes:"[]") with
  | Ok [] -> Stdlib.print_endline "  empty timeline → Ok []: OK"
  | Ok _ ->
      Stdlib.Printf.eprintf "  FAIL: empty timeline returned checks\n";
      Stdlib.exit 1
  | Error e ->
      Stdlib.Printf.eprintf "  FAIL: empty timeline errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1

let test_null_before_commit_is_ok_empty () =
  let body =
    removal_body
      ~nodes:
        {|[{ "createdAt": "2026-06-24T20:55:00Z", "reason": "x",
             "beforeCommit": null }]|}
  in
  match parse body with
  | Ok [] -> Stdlib.print_endline "  null beforeCommit → Ok []: OK"
  | Ok _ ->
      Stdlib.Printf.eprintf "  FAIL: null beforeCommit returned checks\n";
      Stdlib.exit 1
  | Error e ->
      Stdlib.Printf.eprintf "  FAIL: null beforeCommit errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1

let test_missing_pull_request_is_ok_empty () =
  match parse {|{ "data": { "repository": { "pullRequest": null } } }|} with
  | Ok [] -> Stdlib.print_endline "  missing pullRequest → Ok []: OK"
  | Ok _ ->
      Stdlib.Printf.eprintf "  FAIL: missing pullRequest returned checks\n";
      Stdlib.exit 1
  | Error e ->
      Stdlib.Printf.eprintf "  FAIL: missing pullRequest errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1

let test_graphql_errors_propagate () =
  match parse {|{ "errors": [ { "message": "Something broke" } ] }|} with
  | Error _ -> Stdlib.print_endline "  graphql errors → Error: OK"
  | Ok _ ->
      Stdlib.Printf.eprintf "  FAIL: graphql errors did not propagate\n";
      Stdlib.exit 1

let () =
  test_mixed_returns_only_failures ();
  test_truncated_rollup_is_ok_empty ();
  test_empty_timeline_is_ok_empty ();
  test_null_before_commit_is_ok_empty ();
  test_missing_pull_request_is_ok_empty ();
  test_graphql_errors_propagate ();
  Stdlib.print_endline "test_github_merge_queue_removal: OK"
