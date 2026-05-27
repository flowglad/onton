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

let () =
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
