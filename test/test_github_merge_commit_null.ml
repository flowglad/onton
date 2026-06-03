(* Regression test for the null-[mergeCommit] poll crash.

   Every open/draft/unmerged PR comes back from GitHub with
   [mergeCommit: null]. Reading [mergeCommit |> member "oid"] on that null
   raised [Yojson.Safe.Util.Type_error], which the parser's catch-all turned
   into [Json_parse_error] — failing the *entire* poll on every cycle and
   blinding the orchestrator to the PR's state. See [lib/github.ml]. *)

let pr_json ~merge_commit =
  Printf.sprintf
    {|{
      "data": {
        "repository": {
          "pullRequest": {
            "state": "OPEN",
            "mergeable": "MERGEABLE",
            "isDraft": true,
            "mergeStateStatus": "BLOCKED",
            "commits": { "nodes": [] },
            "reviewThreads": { "nodes": [] },
            "headRefName": "feature-branch",
            "headRefOid": "abc123",
            "mergeCommit": %s,
            "baseRefName": "main",
            "headRepositoryOwner": { "login": "flowglad" }
          }
        }
      }
    }|}
    merge_commit

let parse s =
  Onton.Github.parse_response_json ~owner:"flowglad" (Yojson.Safe.from_string s)

let pending_patch_2_parse_merge_queue_and_entry () =
  let body =
    {|{
      "data": {
        "repository": {
          "mergeQueue": { "id": "MQ_main" },
          "pullRequest": {
            "id": "PR_node_123",
            "state": "OPEN",
            "mergeable": "MERGEABLE",
            "isDraft": false,
            "mergeStateStatus": "CLEAN",
            "commits": { "nodes": [] },
            "reviewThreads": { "nodes": [] },
            "headRefName": "feature-branch",
            "headRefOid": "abc123",
            "baseRefName": "main",
            "mergeCommit": null,
            "mergeQueueEntry": {
              "id": "MQE_node_456",
              "state": "SOME_FUTURE_STATE",
              "position": 7
            },
            "headRepositoryOwner": { "login": "flowglad" }
          }
        }
      }
    }|}
  in
  match parse body with
  | Ok st -> (
      assert (st.Onton_core.Pr_state.node_id = Some "PR_node_123");
      assert st.Onton_core.Pr_state.merge_queue_required;
      match st.Onton_core.Pr_state.merge_queue_entry with
      | Some entry ->
          assert (String.equal entry.Onton_core.Pr_state.id "MQE_node_456");
          assert (
            Onton_core.Pr_state.equal_merge_queue_entry_state
              entry.Onton_core.Pr_state.state Onton_core.Pr_state.Mq_locked);
          assert (entry.Onton_core.Pr_state.position = 7);
          Stdlib.print_endline
            "  merge queue GraphQL fields: OK (required + entry)"
      | None ->
          Stdlib.print_endline "  FAIL: missing parsed merge queue entry";
          Stdlib.exit 1)
  | Error e ->
      Printf.eprintf "  FAIL: merge queue fixture errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1

let pending_patch_3_enqueue_and_dequeue_parsing () =
  (* PENDING: Patch 3 - enqueue and dequeue mutation response parsing. *)
  ()

let pending_patch_3_merge_queue_405_detection () =
  (* PENDING: Patch 3 - is_merge_queue_required_error detects the 405 merge-queue body. *)
  ()

let () =
  pending_patch_2_parse_merge_queue_and_entry ();
  pending_patch_3_enqueue_and_dequeue_parsing ();
  pending_patch_3_merge_queue_405_detection ();
  (* Open PR: mergeCommit is null. Must parse Ok with merge_commit_sha = None,
     not crash the poll. *)
  (match parse (pr_json ~merge_commit:"null") with
  | Ok st ->
      assert (Option.is_none st.Onton_core.Pr_state.merge_commit_sha);
      Stdlib.print_endline "  open PR (mergeCommit:null): OK (None)"
  | Error e ->
      Printf.eprintf "  FAIL: open PR poll errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1);

  (* Merged PR: mergeCommit present. Must still read the oid through. *)
  (match parse (pr_json ~merge_commit:{|{ "oid": "deadbeef" }|}) with
  | Ok st ->
      assert (
        match st.Onton_core.Pr_state.merge_commit_sha with
        | Some "deadbeef" -> true
        | _ -> false);
      Stdlib.print_endline "  merged PR (mergeCommit.oid): OK (deadbeef)"
  | Error e ->
      Printf.eprintf "  FAIL: merged PR poll errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1);

  Stdlib.print_endline "test_github_merge_commit_null: OK"
