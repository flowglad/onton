(* @archlint.module test
   @archlint.domain prompt-recovery *)

open Base
open Onton

(** Tests for live worktree guidance in implementation and cleanup sessions. *)

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
