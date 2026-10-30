(* @archlint.module test
   @archlint.domain prompt-recovery *)

open Base
open Onton

(* Every built-in prompt check must be independent of developer overrides. *)
let () =
  let directory = Stdlib.Filename.temp_dir "onton-prompt-defaults-" "" in
  let previous = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" directory;
  Stdlib.at_exit (fun () ->
      (match previous with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None -> Unix.unsetenv "ONTON_DATA_DIR");
      ignore (Stdlib.Sys.command ("rm -rf " ^ Stdlib.Filename.quote directory)))

(** Tests for live worktree guidance in implementation and cleanup sessions. *)

let () =
  let question =
    "Can you explain why you won't implement Patch 5, if no dependency is \
     missing?"
  in
  List.iter [ false; true ] ~f:(fun has_existing_changes ->
      List.iter
        [ [ question ]; [ question; "Which branch are you on?" ] ]
        ~f:(fun messages ->
          let prompt =
            Prompt.render_start_delivery ~project_name:"" ~has_existing_changes
              (Onton_core.Patch_decision.Start_with_human { messages })
          in
          let expected =
            Prompt.render_human_message_prompt ~project_name:"" messages
          in
          if not (String.equal prompt expected) then
            failwith "human-guided start must deliver only human guidance");
      let initial =
        Prompt.render_start_delivery ~project_name:"" ~has_existing_changes
          Onton_core.Patch_decision.Start_initial
      in
      let expected =
        Prompt.render_turn_layer_start ~project_name:"" ~has_existing_changes ()
      in
      if not (String.equal initial expected) then
        failwith "ordinary start must preserve implementation guidance")

let assert_contains label haystack ~substring =
  if not (String.is_substring haystack ~substring) then (
    Stdlib.print_endline ("FAIL: " ^ label);
    Stdlib.print_endline ("  expected substring: " ^ substring);
    Stdlib.print_endline "  ----- prompt -----";
    Stdlib.print_endline haystack;
    Stdlib.print_endline "  ----- end prompt -----";
    Stdlib.exit 1)

let assert_not_contains label haystack ~substring =
  if String.is_substring haystack ~substring then (
    Stdlib.print_endline ("FAIL: " ^ label);
    Stdlib.print_endline ("  unexpected substring: " ^ substring);
    Stdlib.exit 1)

let () =
  let prompt =
    Prompt.render_uncommitted_changes_prompt ~project_name:""
      ~pr_number:(Onton_core.Types.Pr_number.of_int 434)
      ~git_status:" M lib/worker.ml\n?? scratch.txt" ()
  in
  assert_contains "dirty: explains blocked rebase" prompt
    ~substring:"worktree contains uncommitted changes";
  assert_contains "dirty: offers commit" prompt ~substring:"stage and commit";
  assert_contains "dirty: offers discard" prompt
    ~substring:"discard them completely";
  assert_contains "dirty: includes status" prompt ~substring:"M lib/worker.ml";
  assert_contains "dirty: includes PR context" prompt ~substring:"PR: #434";
  assert_contains "dirty: requires clean porcelain status" prompt
    ~substring:"git status --porcelain";
  assert_contains "dirty: leaves rebase to supervisor" prompt
    ~substring:"Do not rebase or push"

let () =
  let prompt =
    Prompt.render_turn_layer_uncommitted_changes ~project_name:""
      ~git_status:(String.make 4500 'Z') ()
  in
  assert_contains "dirty: long status is marked truncated" prompt
    ~substring:"[truncated]";
  if String.count prompt ~f:(Char.equal 'Z') <> 4000 then (
    Stdlib.print_endline "FAIL: dirty: status was not capped at 4000 bytes";
    Stdlib.exit 1)

let () =
  let clean = Prompt.render_turn_layer_start ~project_name:"" () in
  let dirty =
    Prompt.render_turn_layer_start ~project_name:"" ~has_existing_changes:true
      ()
  in
  if not (String.equal clean "Continue implementing until all tests pass.\n")
  then failwith "clean start text drifted";
  if not (String.is_prefix dirty ~prefix:clean) then
    failwith "dirty start must preserve clean start text";
  assert_not_contains "clean start has no existing-changes instruction" clean
    ~substring:"This worktree already has uncommitted changes";
  assert_contains "dirty start includes existing changes" dirty
    ~substring:"This worktree already has uncommitted changes";
  assert_contains "dirty start permits a separate commit" dirty
    ~substring:"commit them separately";
  assert_contains "dirty start permits inclusion in current commit" dirty
    ~substring:"include them in your current commit";
  assert_contains "dirty start permits reverting" dirty
    ~substring:"revert them if they are not needed";
  assert_contains "dirty start requires inspection" dirty
    ~substring:"Inspect each changed and untracked file before acting";
  assert_contains "dirty start preserves unrelated files" dirty
    ~substring:"Preserve unrelated files";
  assert_contains "dirty start forbids ignoring patch work" dirty
    ~substring:"Do not leave patch-related changes uncommitted"

let () =
  let open Onton_core.Types in
  let patch : Patch.t =
    Patch.
      {
        id = Patch_id.of_string "prompt-recovery";
        title = "Prompt recovery";
        description = "";
        branch = Branch.of_string "prompt-recovery";
        dependencies = [];
        spec = "";
        acceptance_criteria = [];
        files = [];
        classification = "";
        changes = [];
        test_stubs_introduced = [];
        test_stubs_implemented = [];
        complexity = None;
        precedents = [];
        required_context = [];
      }
  in
  let gameplan : Gameplan.t =
    Gameplan.
      {
        project_name = "";
        repo_owner = "";
        repo_name = "";
        problem_statement = "";
        architecture_design = None;
        solution_summary = "";
        final_state_spec = "";
        patches = [ patch ];
        operational_considerations = "";
        required_changes = "";
        ordering_constraints = [];
        current_state_analysis = "";
        explicit_opinions = "";
        acceptance_criteria = [];
        open_questions = [];
        functional_changes = [];
        context_resources = [];
        publication = None;
        reachability_traces = [];
      }
  in
  let render ?has_existing_changes () =
    Prompt.render_patch_prompt ~project_name:"" ?has_existing_changes patch
      gameplan ~base_branch:"main"
  in
  assert_not_contains "clean patch prompt omits existing changes" (render ())
    ~substring:"This worktree already has uncommitted changes";
  assert_contains "dirty patch prompt includes existing changes"
    (render ~has_existing_changes:true ())
    ~substring:"This worktree already has uncommitted changes"

let () = Stdlib.print_endline "All prompt-recovery tests passed."

let () =
  let open Onton_core in
  let comment : Types.Comment.t =
    Types.Comment.
      {
        id = Types.Comment_id.of_int 42;
        thread_id = Some "thread";
        body = "Please handle this edge case.";
        path = Some "lib/foo.ml";
        line = Some 7;
        commit_sha = None;
        original_commit_sha = None;
        outdated = false;
        last_reply_author = None;
      }
  in
  let finding =
    match
      Review_service.parse_finding
        (`Assoc
           [
             ("id", `String "service/finding-7");
             ("postingSha", `String "abc");
             ("path", `String "lib/foo.ml");
             ("startLine", `Int 7);
             ("endLine", `Int 7);
             ("severity", `String "should-fix");
             ("body", `String "Handle the boundary.");
             ("createdAt", `String "now");
             ("outcome", `Assoc [ ("kind", `String "outstanding") ]);
           ])
    with
    | Ok finding -> finding
    | Error error -> failwith error
  in
  let render response_transport =
    [
      ( Prompt.render_pr_body_prompt ~response_transport ~branch_only:false
          ~project_name:"" ~pr_number:(Types.Pr_number.of_int 1)
          ~pr_body:"Patch description" ~spec_suffix:""
          ~artifact_path:"/notes.md",
        "implementation_notes",
        "Write just the notes content" );
      ( Prompt.render_turn_layer_review ~response_transport ~project_name:""
          ~artifact_dir:"/responses" [ comment ],
        "review_response",
        "Write your response to" );
      ( Prompt.render_turn_layer_findings ~response_transport ~project_name:""
          ~artifact_dir:"/findings" [ finding ],
        "finding_wontfix",
        "write the reason (plain text) to" );
      ( Session_prompt.render_opt_out ~response_transport
          ~artifact_path:"/WONTDO.md",
        "opt_out",
        "write your reason to" );
    ]
  in
  List.iter (render Session_prompt.Pi_tools)
    ~f:(fun (text, kind, file_instruction) ->
      assert_contains "Pi responses name the tool" text
        ~substring:"onton_respond";
      assert_contains "Pi responses select the response kind" text
        ~substring:kind;
      assert_not_contains "Pi responses do not instruct file delivery" text
        ~substring:file_instruction;
      assert_not_contains "Pi responses do not require the Write tool" text
        ~substring:"Write tool");
  List.iter (render Session_prompt.File_artifacts)
    ~f:(fun (text, _, file_instruction) ->
      assert_contains "File backends retain their delivery contract" text
        ~substring:file_instruction;
      assert_not_contains "File backends do not require Pi tools" text
        ~substring:"onton_respond");
  let review, _, _ = List.nth_exn (render Session_prompt.Pi_tools) 1 in
  assert_contains "Pi preserves comment IDs" review ~substring:"comment_id=42";
  assert_contains "Pi preserves review bodies" review
    ~substring:comment.Types.Comment.body;
  let retry_review =
    Prompt.render_turn_layer_review ~response_transport:Session_prompt.Pi_tools
      ~project_name:"" ~viewer_login:"onton-bot" ~artifact_dir:"/responses"
      [
        comment;
        { comment with Types.Comment.last_reply_author = Some "onton-bot" };
      ]
  in
  assert_contains "Pi review limits mandatory replies" retry_review
    ~substring:"every comment without [response already posted]";
  assert_contains "Pi review permits only new retry content" retry_review
    ~substring:"respond only if you have something new to add";
  let findings, _, _ = List.nth_exn (render Session_prompt.Pi_tools) 2 in
  assert_contains "Pi preserves finding response names" findings
    ~substring:"service_finding-7.md";
  assert_contains "Pi preserves finding bodies" findings
    ~substring:finding.Review_service.body;
  let directory = Stdlib.Filename.temp_dir "onton-prompt-transport-" "" in
  let previous = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
  Exn.protect
    ~finally:(fun () ->
      (match previous with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None -> Unix.unsetenv "ONTON_DATA_DIR");
      ignore (Stdlib.Sys.command ("rm -rf " ^ Stdlib.Filename.quote directory)))
    ~f:(fun () ->
      Unix.putenv "ONTON_DATA_DIR" directory;
      let prompts =
        Stdlib.Filename.concat (Project_store.project_dir "") "prompts"
      in
      Project_store.ensure_dir prompts;
      let write name content =
        let channel =
          Stdlib.open_out_bin (Stdlib.Filename.concat prompts (name ^ ".md"))
        in
        Exn.protect
          ~finally:(fun () -> Stdlib.close_out channel)
          ~f:(fun () -> Stdlib.output_string channel content)
      in
      List.iter [ "pr_body"; "turn_review"; "turn_findings" ] ~f:(fun name ->
          write name "Existing file response override");
      List.iter
        (List.take (render Session_prompt.File_artifacts) 3)
        ~f:(fun (text, _, _) ->
          assert (String.equal text "Existing file response override"));
      List.iter
        (List.take (render Session_prompt.Pi_tools) 3)
        ~f:(fun (text, _, _) ->
          assert_contains "Pi ignores file protocol overrides" text
            ~substring:"onton_respond");
      List.iter [ "pi_pr_body"; "pi_turn_review"; "pi_turn_findings" ]
        ~f:(fun name -> write name "Explicit Pi tool response override");
      List.iter
        (List.take (render Session_prompt.Pi_tools) 3)
        ~f:(fun (text, _, _) ->
          assert (String.equal text "Explicit Pi tool response override")));
  Stdlib.print_endline
    "Backend-specific response prompt contracts and override isolation: OK"
