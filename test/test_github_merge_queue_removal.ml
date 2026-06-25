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

(* -- parse_merge_queue_removal_pagination -- *)

let paginate = Onton.Github.parse_merge_queue_removal_pagination

(* A truncated merge-group rollup must surface its beforeCommit OID so the I/O
   layer knows the [Ok []] from [parse] is hiding more pages. *)
let test_pagination_detects_truncated_oid () =
  match paginate (removal_body ~nodes:truncated_event) with
  | Some "mergegroupsha123" ->
      Stdlib.print_endline "  truncated rollup → Some oid: OK"
  | _ ->
      Stdlib.Printf.eprintf "  FAIL: truncated rollup did not surface oid\n";
      Stdlib.exit 1

(* A complete rollup (hasNextPage:false) needs no pagination → None. *)
let test_pagination_none_when_complete () =
  match paginate (removal_body ~nodes:mixed_event) with
  | None -> Stdlib.print_endline "  complete rollup → None: OK"
  | Some _ ->
      Stdlib.Printf.eprintf "  FAIL: complete rollup asked for pagination\n";
      Stdlib.exit 1

let test_pagination_none_when_empty () =
  match paginate (removal_body ~nodes:"[]") with
  | None -> Stdlib.print_endline "  empty timeline → None: OK"
  | Some _ ->
      Stdlib.Printf.eprintf "  FAIL: empty timeline asked for pagination\n";
      Stdlib.exit 1

(* -- parse_contexts_page -- *)

let parse_page = Onton.Github.parse_contexts_page

let contexts_page_body ~has_next ~cursor ~nodes =
  Printf.sprintf
    {|{ "data": { "repository": { "object": {
      "statusCheckRollup": { "contexts": {
        "pageInfo": { "hasNextPage": %b, "endCursor": %s },
        "nodes": %s
      } } } } } }|}
    has_next cursor nodes

let test_contexts_page_parses_checks_and_cursor () =
  let nodes =
    {|[
      { "__typename": "CheckRun", "databaseId": 1, "name": "build",
        "conclusion": "SUCCESS", "detailsUrl": null, "text": null,
        "startedAt": null },
      { "__typename": "StatusContext", "context": "legacy",
        "state": "FAILURE", "targetUrl": null, "description": null,
        "createdAt": null }
    ]|}
  in
  match
    parse_page (contexts_page_body ~has_next:true ~cursor:{|"CURSOR2"|} ~nodes)
  with
  | Ok (checks, true, Some "CURSOR2") when List.length checks = 2 ->
      Stdlib.print_endline "  contexts page → checks + next cursor: OK"
  | Ok _ ->
      Stdlib.Printf.eprintf "  FAIL: contexts page parsed wrong shape\n";
      Stdlib.exit 1
  | Error e ->
      Stdlib.Printf.eprintf "  FAIL: contexts page errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1

(* A missing object (gc'd commit / no rollup) is a non-error terminal page. *)
let test_contexts_page_missing_object_is_terminal () =
  match parse_page {|{ "data": { "repository": { "object": null } } }|} with
  | Ok ([], false, None) ->
      Stdlib.print_endline "  missing object → empty terminal page: OK"
  | Ok _ ->
      Stdlib.Printf.eprintf
        "  FAIL: missing object returned non-terminal page\n";
      Stdlib.exit 1
  | Error e ->
      Stdlib.Printf.eprintf "  FAIL: missing object errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1

let test_contexts_page_graphql_errors_propagate () =
  match parse_page {|{ "errors": [ { "message": "boom" } ] }|} with
  | Error _ -> Stdlib.print_endline "  contexts page graphql errors → Error: OK"
  | Ok _ ->
      Stdlib.Printf.eprintf "  FAIL: contexts page swallowed graphql errors\n";
      Stdlib.exit 1

let () =
  test_mixed_returns_only_failures ();
  test_truncated_rollup_is_ok_empty ();
  test_empty_timeline_is_ok_empty ();
  test_null_before_commit_is_ok_empty ();
  test_missing_pull_request_is_ok_empty ();
  test_graphql_errors_propagate ();
  test_pagination_detects_truncated_oid ();
  test_pagination_none_when_complete ();
  test_pagination_none_when_empty ();
  test_contexts_page_parses_checks_and_cursor ();
  test_contexts_page_missing_object_is_terminal ();
  test_contexts_page_graphql_errors_propagate ();
  Stdlib.print_endline "test_github_merge_queue_removal: OK"
