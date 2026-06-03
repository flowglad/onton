(* Regression test for the null-[mergeCommit] poll crash.

   Every open/draft/unmerged PR comes back from GitHub with
   [mergeCommit: null]. Reading [mergeCommit |> member "oid"] on that null
   raised [Yojson.Safe.Util.Type_error], which the parser's catch-all turned
   into [Json_parse_error] — failing the *entire* poll on every cycle and
   blinding the orchestrator to the PR's state. See [lib/github.ml]. *)

let pr_json ?(merge_queue_entry = "null") ~merge_commit () =
  Printf.sprintf
    {|{
      "data": {
        "repository": {
          "pullRequest": {
            "id": "PR_node_1",
            "state": "OPEN",
            "mergeable": "MERGEABLE",
            "isDraft": true,
            "mergeStateStatus": "BLOCKED",
            "commits": { "nodes": [] },
            "reviewThreads": { "nodes": [] },
            "headRefName": "feature-branch",
            "headRefOid": "abc123",
            "mergeQueueEntry": %s,
            "mergeCommit": %s,
            "baseRefName": "main",
            "headRepositoryOwner": { "login": "flowglad" }
          }
        }
      }
    }|}
    merge_queue_entry merge_commit

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
              entry.Onton_core.Pr_state.state Onton_core.Pr_state.Mq_queued);
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
  let enqueue_body =
    {|{
      "data": {
        "enqueuePullRequest": {
          "mergeQueueEntry": {
            "id": "MQE_123",
            "state": "AWAITING_CHECKS",
            "position": 7
          }
        }
      }
    }|}
  in
  (match Onton.Github.parse_enqueue_response enqueue_body with
  | Ok entry ->
      assert (String.equal entry.Onton_core.Pr_state.id "MQE_123");
      assert (
        Onton_core.Pr_state.equal_merge_queue_entry_state
          entry.Onton_core.Pr_state.state Onton_core.Pr_state.Mq_awaiting_checks);
      assert (entry.Onton_core.Pr_state.position = 7)
  | Error e ->
      Printf.eprintf "  FAIL: enqueue response parse errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1);
  (match
     Onton.Github.parse_enqueue_response
       {|{"data":{"enqueuePullRequest":{"mergeQueueEntry":{"id":"MQE_999","state":"FUTURE_STATE","position":2}}}}|}
   with
  | Ok entry ->
      assert (
        Onton_core.Pr_state.equal_merge_queue_entry_state
          entry.Onton_core.Pr_state.state Onton_core.Pr_state.Mq_queued)
  | Error e ->
      Printf.eprintf "  FAIL: future enqueue state parse errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1);
  match
    Onton.Github.parse_dequeue_response
      {|{"data":{"dequeuePullRequest":{"clientMutationId":null}}}|}
  with
  | Ok () -> ()
  | Error e ->
      Printf.eprintf "  FAIL: dequeue response parse errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1

let pending_patch_3_merge_queue_405_detection () =
  let merge_queue_err =
    Onton.Github.Http_error
      {
        meth = "PUT";
        path = "/repos/o/r/pulls/1/merge";
        status = 405;
        body = {|{"message":"Changes must be made through the merge queue"}|};
      }
  in
  let base_modified_err =
    Onton.Github.Http_error
      {
        meth = "PUT";
        path = "/repos/o/r/pulls/1/merge";
        status = 405;
        body = {|{"message":"Base branch was modified"}|};
      }
  in
  assert (Onton.Github.is_merge_queue_required_error merge_queue_err);
  assert (not (Onton.Github.is_method_not_allowed merge_queue_err));
  assert (not (Onton.Github.is_merge_queue_required_error base_modified_err))

let () =
  pending_patch_2_parse_merge_queue_and_entry ();
  pending_patch_3_enqueue_and_dequeue_parsing ();
  pending_patch_3_merge_queue_405_detection ();
  (* Open PR: mergeCommit is null. Must parse Ok with merge_commit_sha = None,
     not crash the poll. *)
  (match parse (pr_json ~merge_commit:"null" ()) with
  | Ok st ->
      assert (
        match st.Onton_core.Pr_state.node_id with
        | Some "PR_node_1" -> true
        | _ -> false);
      assert (Option.is_none st.Onton_core.Pr_state.merge_queue_entry);
      assert (Option.is_none st.Onton_core.Pr_state.merge_commit_sha);
      Stdlib.print_endline "  open PR (mergeCommit:null): OK (None)"
  | Error e ->
      Printf.eprintf "  FAIL: open PR poll errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1);

  (* Enqueued PR: mergeQueueEntry should propagate through normal pr_state
     parsing, not only through the dedicated enqueue-info query. *)
  (match
     parse
       (pr_json ~merge_commit:"null"
          ~merge_queue_entry:
            {|{ "id": "MQE_node_1", "state": "MERGEABLE", "position": 3 }|}
          ())
   with
  | Ok st -> (
      match st.Onton_core.Pr_state.merge_queue_entry with
      | Some entry ->
          assert (String.equal entry.Onton_core.Pr_state.id "MQE_node_1");
          assert (
            Onton_core.Pr_state.equal_merge_queue_entry_state
              entry.Onton_core.Pr_state.state Onton_core.Pr_state.Mq_mergeable);
          assert (entry.Onton_core.Pr_state.position = 3);
          Stdlib.print_endline
            "  enqueued PR (mergeQueueEntry): OK (MQE_node_1)"
      | None ->
          Printf.eprintf "  FAIL: enqueued PR missing merge_queue_entry\n";
          Stdlib.exit 1)
  | Error e ->
      Printf.eprintf "  FAIL: enqueued PR poll errored: %s\n"
        (Onton.Github.show_error e);
      Stdlib.exit 1);

  (* Merged PR: mergeCommit present. Must still read the oid through. *)
  (match parse (pr_json ~merge_commit:{|{ "oid": "deadbeef" }|} ()) with
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
