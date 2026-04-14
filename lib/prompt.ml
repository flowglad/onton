open Base
open Types

(* Prompt override system: loads template files from the project data
   directory's prompts/ subdirectory and performs single-pass variable
   substitution. Falls back to built-in defaults when no override exists. *)

let prompts_dir (project_name : string) : string =
  Stdlib.Filename.concat (Project_store.project_dir project_name) "prompts"

let substitute_variables (template : string) (vars : (string * string) list) :
    string =
  let var_map =
    Map.of_alist_reduce (module String) vars ~f:(fun _first last -> last)
  in
  let buf = Buffer.create (String.length template) in
  let len = String.length template in
  let rec scan i =
    if i >= len then ()
    else if
      i + 1 < len
      && Char.equal (String.get template i) '{'
      && Char.equal (String.get template (i + 1)) '{'
    then (
      match String.substr_index template ~pos:(i + 2) ~pattern:"}}" with
      | None ->
          Buffer.add_char buf '{';
          Buffer.add_char buf '{';
          scan (i + 2)
      | Some close_pos ->
          let raw_key =
            String.sub template ~pos:(i + 2) ~len:(close_pos - i - 2)
          in
          let key = String.strip raw_key in
          (match Map.find var_map key with
          | Some value -> Buffer.add_string buf value
          | None ->
              Buffer.add_string buf "{{";
              Buffer.add_string buf raw_key;
              Buffer.add_string buf "}}");
          scan (close_pos + 2))
    else (
      Buffer.add_char buf (String.get template i);
      scan (i + 1))
  in
  scan 0;
  Buffer.contents buf

let validate_name (kind : string) (value : string) : unit =
  if String.is_substring value ~substring:"/" || String.equal value ".." then
    Stdlib.invalid_arg
      (Printf.sprintf "load_override: invalid %s %S" kind value)

let load_override ~(project_name : string) (name : string) : string option =
  validate_name "prompt name" name;
  let path = Stdlib.Filename.concat (prompts_dir project_name) (name ^ ".md") in
  match Unix.openfile path [ Unix.O_RDONLY ] 0 with
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None
  | fd -> (
      match (Unix.fstat fd).Unix.st_kind with
      | Unix.S_REG ->
          let ic =
            match Unix.in_channel_of_descr fd with
            | exception exn ->
                Unix.close fd;
                raise exn
            | ic -> ic
          in
          Exn.protect
            ~finally:(fun () -> Stdlib.In_channel.close ic)
            ~f:(fun () -> Some (Stdlib.In_channel.input_all ic))
      | Unix.S_DIR | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
      | Unix.S_SOCK ->
          Unix.close fd;
          None
      | exception exn ->
          Unix.close fd;
          raise exn)

let render_with_override ~(project_name : string) ~(name : string)
    ~(vars : (string * string) list) ~(default : unit -> string) : string =
  match load_override ~project_name name with
  | Some template -> substitute_variables template vars
  | None -> default ()

let format_list items =
  List.map items ~f:(fun s -> "- " ^ s) |> String.concat ~sep:"\n"

let optional_section ~header content =
  if String.is_empty content then ""
  else "\n## " ^ header ^ "\n" ^ content ^ "\n"

let optional_list_section ~header items =
  match items with
  | [] -> ""
  | _ -> optional_section ~header (format_list items)

let render_patch_prompt ~(project_name : string) ?pr_number (patch : Patch.t)
    (gameplan : Gameplan.t) ~(base_branch : string) =
  let patch_id = Patch_id.to_string patch.Patch.id in
  let deps =
    match patch.Patch.dependencies with
    | [] -> "None"
    | ids ->
        List.map ids ~f:(fun id -> Patch_id.to_string id)
        |> String.concat ~sep:", "
        |> Printf.sprintf "Patches %s"
  in
  let branch = Branch.to_string patch.Patch.branch in
  let patches_list =
    List.map gameplan.Gameplan.patches ~f:(fun (p : Patch.t) ->
        Printf.sprintf "- Patch %s: %s"
          (Patch_id.to_string p.Patch.id)
          p.Patch.title)
    |> String.concat ~sep:"\n"
  in
  let pr_str =
    match pr_number with
    | Some n -> Printf.sprintf "#%d" (Pr_number.to_int n)
    | None -> "Not yet created"
  in
  let classification_note =
    if String.is_empty patch.Patch.classification then ""
    else Printf.sprintf " [%s]" patch.Patch.classification
  in
  let base_branch_note =
    if String.equal base_branch "main" then ""
    else
      Printf.sprintf
        "\n\
         **NOTE:** Your branch is based on `%s`, which is a dependency patch's \
         branch. Your worktree already contains that patch's changes. This is \
         expected — build on top of those changes. Your PR will target `%s`, \
         so the diff should only show YOUR patch's changes, not the \
         dependency's.\n"
        base_branch base_branch
  in
  let pr_instructions =
    let commit_block =
      {|
**Commit your work with `git commit` before ending the session.** Every code change you make must land in a commit — uncommitted changes are discarded by the supervisor's push, and no PR can be opened from an empty branch. Multiple commits are fine; commit whenever it makes sense.

**Do NOT run `git push` or `gh pr create` yourself.** The supervisor pushes your commits and opens/updates the PR.|}
    in
    match pr_number with
    | Some _ -> commit_block ^ "\n\nContinue implementing until all tests pass."
    | None ->
        commit_block
        ^ {|

The supervisor opens the draft PR after your first commit lands on the remote, with a gameplan-derived title and body.

Continue implementing until all tests pass.|}
  in
  let vars =
    [
      ("project_name", project_name);
      ("title", patch.Patch.title);
      ("classification_note", classification_note);
      ("problem_statement", gameplan.Gameplan.problem_statement);
      ("solution_summary", gameplan.Gameplan.solution_summary);
      ("dependencies", deps);
      ("branch", branch);
      ("base_branch", base_branch);
      ("patch_id", patch_id);
      ("patches_list", patches_list);
      ( "pr_number",
        match pr_number with
        | Some n -> Int.to_string (Pr_number.to_int n)
        | None -> "" );
      ("pr_str", pr_str);
      ("description", patch.Patch.description);
      ("spec", patch.Patch.spec);
      ("acceptance_criteria", format_list patch.Patch.acceptance_criteria);
      ("files", format_list patch.Patch.files);
      ( "final_state_spec_section",
        optional_section ~header:"Final State Specification (Non-negotiable)"
          gameplan.Gameplan.final_state_spec );
      ( "explicit_opinions_section",
        optional_section ~header:"Explicit Opinions (Non-negotiable)"
          gameplan.Gameplan.explicit_opinions );
      ( "current_state_section",
        optional_section ~header:"Current State Analysis"
          gameplan.Gameplan.current_state_analysis );
      ("changes_section", optional_list_section ~header:"Changes" patch.changes);
      ( "spec_section",
        if String.is_empty patch.spec then ""
        else
          "\n\
           ## Specification\n\n\
           The following Pantagruel specification defines the formal \
           invariants and post-conditions for this patch. After implementing, \
           verify your code satisfies every clause. Call out any clause you \
           cannot map to your implementation.\n\n\
           ### How to Read This Spec\n\n\
           | Syntax | Meaning |\n\
           |--------|----------|\n\
           | `Domain.` | Entity type declaration |\n\
           | `rule x: T => R.` | State mapping (function from T to R) |\n\
           | `~> Action @ params.` | State transition (modifies state, no \
           return) |\n\
           | `~> Action @ params, guard.` | Action with precondition |\n\
           | `rule' x` | Post-state value of rule (after action executes) |\n\
           | `all x: T \\| P.` | For all x of type T, P must hold (invariant) |\n\
           | `some x: T \\| P.` | There exists x of type T where P holds |\n\
           | `p -> q` | Implication: if p then q |\n\
           | `~p` | Negation |\n\
           | `x in Domain` | Membership test |\n\
           | `Context ~> Action` | Action operates within write-permission \
           boundary |\n\
           | `{Context} rule` | Rule belongs to context (only modifiable by \
           that context's actions) |\n\
           | `initially P.` | Constraint on initial state only |\n\
           | `---` | Separator between declarations (above) and propositions \
           (below) |\n\
           | `where` | Introduces a new chapter (progressive disclosure) |\n\n\
           **Mapping spec to code:**\n\
           - **Rules** → fields, columns, computed properties, or lookups\n\
           - **Primed rules** (`f'`) → what your code must do (post-conditions)\n\
           - **Guards on actions** → what your code must check (preconditions)\n\
           - **Invariants** (non-primed propositions) → safety properties that \
           must hold before and after every operation\n\
           - **`all`/`some` quantifiers** → iteration or existence checks over \
           collections\n\n\
           ### Spec\n\
           ```\n" ^ patch.spec ^ "\n```\n" );
      ( "acceptance_criteria_section",
        optional_list_section ~header:"Acceptance Criteria"
          patch.acceptance_criteria );
      ( "files_section",
        optional_list_section ~header:"Files to Modify" patch.files );
      ( "test_stubs_introduced_section",
        optional_list_section ~header:"Test Stubs Introduced"
          patch.test_stubs_introduced );
      ( "test_stubs_implemented_section",
        optional_list_section ~header:"Test Stubs Implemented"
          patch.test_stubs_implemented );
      ("base_branch_note", base_branch_note);
      ("pr_instructions", pr_instructions);
    ]
  in
  render_with_override ~project_name ~name:"patch" ~vars ~default:(fun () ->
      substitute_variables
        {|# [{{project_name}}] Patch {{patch_id}}{{classification_note}}: {{title}}

## Problem Statement
{{problem_statement}}

## Solution Summary
{{solution_summary}}
{{final_state_spec_section}}{{explicit_opinions_section}}{{current_state_section}}
## Dependencies
{{dependencies}}

## Your Task

{{description}}
{{changes_section}}{{files_section}}{{test_stubs_introduced_section}}{{test_stubs_implemented_section}}{{spec_section}}{{acceptance_criteria_section}}
## Git Instructions
- Branch: {{branch}}
- Base branch: {{base_branch}}
- PR: {{pr_str}}
{{base_branch_note}}{{pr_instructions}}
## Patches in Gameplan
{{patches_list}}|}
        vars)

let resolve_pr_body_source ~(artifact : string option) ~(fallback : string) :
    string =
  match artifact with
  | Some body when String.length (String.strip body) > 0 -> body
  | Some _ | None -> fallback

let%test "resolve_pr_body_source: None artifact returns fallback" =
  String.equal
    (resolve_pr_body_source ~artifact:None ~fallback:"gameplan body")
    "gameplan body"

let%test "resolve_pr_body_source: empty artifact returns fallback" =
  String.equal
    (resolve_pr_body_source ~artifact:(Some "") ~fallback:"gameplan body")
    "gameplan body"

let%test "resolve_pr_body_source: whitespace-only artifact returns fallback" =
  String.equal
    (resolve_pr_body_source ~artifact:(Some "  \n\t  ")
       ~fallback:"gameplan body")
    "gameplan body"

let%test "resolve_pr_body_source: non-empty artifact wins" =
  String.equal
    (resolve_pr_body_source ~artifact:(Some "agent body") ~fallback:"gameplan")
    "agent body"

let render_spec_suffix (patch : Patch.t) (gameplan : Gameplan.t) : string =
  let gp =
    let ds = gameplan.Gameplan.final_state_spec in
    if String.is_empty ds then ""
    else "\n## Gameplan Specification\n\n```\n" ^ ds ^ "\n```\n"
  in
  let ps =
    if String.is_empty patch.Patch.spec then ""
    else "\n## Patch Specification\n\n```\n" ^ patch.spec ^ "\n```\n"
  in
  gp ^ ps

let%test "render_spec_suffix: both empty" =
  let patch =
    {
      Patch.id = Patch_id.of_string "1";
      title = "";
      description = "";
      spec = "";
      changes = [];
      acceptance_criteria = [];
      files = [];
      dependencies = [];
      branch = Branch.of_string "b";
      classification = "";
      test_stubs_introduced = [];
      test_stubs_implemented = [];
    }
  in
  let gameplan =
    {
      Gameplan.project_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }
  in
  String.equal (render_spec_suffix patch gameplan) ""

let%test "render_spec_suffix: gameplan spec only" =
  let patch =
    {
      Patch.id = Patch_id.of_string "1";
      title = "";
      description = "";
      spec = "";
      changes = [];
      acceptance_criteria = [];
      files = [];
      dependencies = [];
      branch = Branch.of_string "b";
      classification = "";
      test_stubs_introduced = [];
      test_stubs_implemented = [];
    }
  in
  let gameplan =
    {
      Gameplan.project_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "module FOO.\nsome spec";
      patches = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }
  in
  let result = render_spec_suffix patch gameplan in
  String.is_substring result ~substring:"## Gameplan Specification"
  && String.is_substring result ~substring:"module FOO."
  && not (String.is_substring result ~substring:"## Patch Specification")

let%test "render_spec_suffix: patch spec only" =
  let patch =
    {
      Patch.id = Patch_id.of_string "1";
      title = "";
      description = "";
      spec = "module BAR.\npatch spec";
      changes = [];
      acceptance_criteria = [];
      files = [];
      dependencies = [];
      branch = Branch.of_string "b";
      classification = "";
      test_stubs_introduced = [];
      test_stubs_implemented = [];
    }
  in
  let gameplan =
    {
      Gameplan.project_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }
  in
  let result = render_spec_suffix patch gameplan in
  String.is_substring result ~substring:"## Patch Specification"
  && String.is_substring result ~substring:"module BAR."
  && not (String.is_substring result ~substring:"## Gameplan Specification")

let%test "render_spec_suffix: both present" =
  let patch =
    {
      Patch.id = Patch_id.of_string "1";
      title = "";
      description = "";
      spec = "module BAR.\npatch spec";
      changes = [];
      acceptance_criteria = [];
      files = [];
      dependencies = [];
      branch = Branch.of_string "b";
      classification = "";
      test_stubs_introduced = [];
      test_stubs_implemented = [];
    }
  in
  let gameplan =
    {
      Gameplan.project_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "module FOO.\ngameplan spec";
      patches = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }
  in
  let result = render_spec_suffix patch gameplan in
  String.is_substring result ~substring:"## Gameplan Specification"
  && String.is_substring result ~substring:"## Patch Specification"

let render_pr_description ~(project_name : string) (patch : Patch.t)
    (gameplan : Gameplan.t) =
  let patch_id = Patch_id.to_string patch.Patch.id in
  let deps =
    match patch.Patch.dependencies with
    | [] -> "None"
    | ids ->
        List.map ids ~f:(fun id -> Patch_id.to_string id)
        |> String.concat ~sep:", "
        |> Printf.sprintf "Patches %s"
  in
  let vars =
    [
      ("project_name", project_name);
      ("patch_id", patch_id);
      ("title", patch.Patch.title);
      ("description", patch.Patch.description);
      ("problem_statement", gameplan.Gameplan.problem_statement);
      ("solution_summary", gameplan.Gameplan.solution_summary);
      ("dependencies", deps);
      ("changes_section", optional_list_section ~header:"Changes" patch.changes);
      ("gameplan_spec_section", "");
      ("patch_spec_section", "");
      ( "acceptance_criteria_section",
        optional_list_section ~header:"Acceptance Criteria"
          patch.acceptance_criteria );
      ( "files_section",
        optional_list_section ~header:"Files to Modify" patch.files );
    ]
  in
  render_with_override ~project_name ~name:"pr_description" ~vars
    ~default:(fun () ->
      substitute_variables
        {|## Patch {{patch_id}}: {{title}}

{{description}}
{{changes_section}}{{gameplan_spec_section}}{{patch_spec_section}}{{acceptance_criteria_section}}{{files_section}}|}
        vars)

let render_pr_body_prompt ~(project_name : string) ~(pr_number : Pr_number.t)
    ~(pr_body : string) ~(artifact_path : string) =
  let pr_num_str = Int.to_string (Pr_number.to_int pr_number) in
  let vars =
    [
      ("pr_number", pr_num_str);
      ("pr_body", pr_body);
      ("artifact_path", artifact_path);
    ]
  in
  render_with_override ~project_name ~name:"pr_body" ~vars ~default:(fun () ->
      substitute_variables
        {|You have just finished implementing this patch. PR #{{pr_number}} was opened with a generic, gameplan-derived body. Your task is to write a better one — describing what you actually built — and the supervisor will upload it.

The current PR body (gameplan-derived, will be replaced by what you write) is:

---
{{pr_body}}
---

**Write the full PR body to `{{artifact_path}}`.** This is an absolute path outside the worktree — write it with the Write tool. Do NOT run `gh`, `git`, or any forge command; the supervisor reads the file and PATCHes the PR.

What to include in the body:

- A short summary of what this patch does, in your own words.
- Key implementation decisions and trade-offs you made.
- Anything surprising or non-obvious about the approach.
- Deviations from the original plan (if any).
- Important context a reviewer should know.

Write the **full body** (not a delta). It will replace the existing description verbatim. If you genuinely have nothing to add over the gameplan-derived body, write that body verbatim — the supervisor PATCH is idempotent.

A separate, later phase will append `## Implementation Notes` to this body — do not include that section here.|}
        vars)

let render_implementation_notes_prompt ~(project_name : string)
    ~(pr_number : Pr_number.t) ~(artifact_path : string) =
  let pr_num_str = Int.to_string (Pr_number.to_int pr_number) in
  let vars = [ ("pr_number", pr_num_str); ("artifact_path", artifact_path) ] in
  render_with_override ~project_name ~name:"implementation_notes" ~vars
    ~default:(fun () ->
      substitute_variables
        {|You have just finished implementing this patch and a PR has been created. The supervisor opens a final phase where you write **just the implementation notes** — the "## Implementation Notes" section a reviewer cares about.

**Write the notes content (markdown, no header line) to `{{artifact_path}}`.** This is an absolute path outside the worktree — write it with the Write tool. The supervisor reads the file, prepends `## Implementation Notes`, and appends it to the PR body.

Do NOT run `gh`, `git`, or any forge command — the supervisor handles upload.

Focus on:

- Key implementation decisions and trade-offs you made.
- Anything surprising or non-obvious about the approach.
- Deviations from the original plan (if any).
- Important details a reviewer should know.

Keep it concise — a few bullet points usually suffices. If you have nothing material to add (the patch is straightforward), write a single line acknowledging that.|}
        vars)

let render_review_prompt ~(project_name : string) ?pr_number
    (comments : Comment.t list) =
  match comments with
  | [] -> "No review comments to address."
  | _ ->
      let pr_num_str =
        match pr_number with
        | Some n -> Int.to_string (Pr_number.to_int n)
        | None -> "{pr_number}"
      in
      let formatted =
        List.map comments ~f:(fun (c : Comment.t) ->
            let location =
              match (c.Comment.path, c.Comment.line) with
              | Some path, Some line ->
                  Printf.sprintf " on `%s` (line %d)" path line
              | Some path, None -> Printf.sprintf " on `%s`" path
              | None, _ -> ""
            in
            let db_id =
              Printf.sprintf " [comment_id=%d]" (Comment_id.to_int c.Comment.id)
            in
            let thread_ref =
              match c.Comment.thread_id with
              | Some tid -> Printf.sprintf " [thread_id=%s]" tid
              | None -> ""
            in
            Printf.sprintf "- **Comment**%s%s%s: %s" location db_id thread_ref
              c.Comment.body)
        |> String.concat ~sep:"\n\n"
      in
      let pr_ctx =
        match pr_number with
        | Some n -> Printf.sprintf "\n\nPR: #%d\n" (Pr_number.to_int n)
        | None -> ""
      in
      let vars =
        [
          ("project_name", project_name);
          ("comments", formatted);
          ("count", Int.to_string (List.length comments));
          ("pr_number", pr_num_str);
        ]
      in
      render_with_override ~project_name ~name:"review" ~vars
        ~default:(fun () ->
          Printf.sprintf
            "# Review Comments%s\n\n\
             The following review comments need to be addressed on your PR:\n\n\
             %s\n\n\
             For each comment:\n\
             1. Implement the requested change, OR explain why the current \
             approach is correct.\n\
             2. Reply to the comment thread:\n\
            \   `gh api \
             repos/{owner}/{repo}/pulls/%s/comments/{comment_id}/replies -f \
             body=\"your response\"`\n\
            \   (`{owner}/{repo}` is resolved automatically by `gh` when run \
             inside the repo)\n\
             3. Resolve the thread using the thread_id:\n\
            \   `gh api graphql -f query='mutation { \
             resolveReviewThread(input: {threadId: \"{thread_id}\"}) { thread \
             { isResolved } } }'`\n\n\
             After addressing all comments, commit your changes. The \
             supervisor will push them for you — do not run `git push`."
            pr_ctx formatted pr_num_str)

let render_ci_failure_prompt ~(project_name : string) ?pr_number
    (checks : Ci_check.t list) =
  match checks with
  | [] -> "No CI failures."
  | _ ->
      let formatted =
        List.map checks ~f:(fun (c : Ci_check.t) ->
            let url =
              match c.Ci_check.details_url with
              | Some u -> Printf.sprintf " (%s)" u
              | None -> ""
            in
            let desc =
              match c.Ci_check.description with
              | Some d -> Printf.sprintf "\n  %s" d
              | None -> ""
            in
            Printf.sprintf "- **%s**: %s%s%s" c.Ci_check.name
              c.Ci_check.conclusion url desc)
        |> String.concat ~sep:"\n"
      in
      let pr_ctx =
        match pr_number with
        | Some n -> Printf.sprintf "\n\nPR: #%d\n" (Pr_number.to_int n)
        | None -> ""
      in
      let vars =
        [
          ("project_name", project_name);
          ("checks", formatted);
          ("count", Int.to_string (List.length checks));
          ( "pr_number",
            match pr_number with
            | Some n -> Int.to_string (Pr_number.to_int n)
            | None -> "" );
        ]
      in
      render_with_override ~project_name ~name:"ci_failure" ~vars
        ~default:(fun () ->
          Printf.sprintf
            "# CI Failures%s\n\n\
             The following CI checks failed:\n\n\
             %s\n\n\
             After making your changes, commit them. The supervisor will push \
             them for you — do not run `git push`."
            pr_ctx formatted)

let render_ci_failure_unknown_prompt ~(project_name : string) ?pr_number () =
  let pr_ctx =
    match pr_number with
    | Some n -> Printf.sprintf "\n\nPR: #%d\n" (Pr_number.to_int n)
    | None -> ""
  in
  let vars =
    [
      ("project_name", project_name);
      ( "pr_number",
        match pr_number with
        | Some n -> Int.to_string (Pr_number.to_int n)
        | None -> "" );
    ]
  in
  render_with_override ~project_name ~name:"ci_failure_unknown" ~vars
    ~default:(fun () ->
      Printf.sprintf
        "# CI Failures%s\n\n\
         One or more CI checks failed. Please investigate the failures and fix \
         them.\n\n\
         Run the CI checks locally or check the PR status for details.\n\n\
         After making your changes, commit them. The supervisor will push them \
         for you — do not run `git push`."
        pr_ctx)

let render_merge_conflict_prompt ~(project_name : string) ?pr_number ?patch
    ?gameplan ~(base_branch : string) ?(git_status = "") ?(git_diff = "") () =
  let pr_ctx =
    match pr_number with
    | Some n -> Printf.sprintf "\n\nPR: #%d\n" (Pr_number.to_int n)
    | None -> ""
  in
  let status_section =
    if String.is_empty git_status then ""
    else Printf.sprintf {|

## Current rebase state

```
%s
```|} git_status
  in
  let diff_section =
    if String.is_empty git_diff then ""
    else Printf.sprintf {|

## Conflict markers

```diff
%s
```|} git_diff
  in
  let task_context =
    match (patch, gameplan) with
    | Some (patch : Patch.t), Some (gp : Gameplan.t) ->
        let patch_id = Patch_id.to_string patch.Patch.id in
        let desc_section =
          if String.is_empty patch.Patch.description then ""
          else "\n\n### Your Task\n\n" ^ patch.Patch.description
        in
        let changes_section =
          optional_list_section ~header:"Changes" patch.Patch.changes
        in
        let ac_section =
          optional_list_section ~header:"Acceptance Criteria"
            patch.Patch.acceptance_criteria
        in
        Printf.sprintf
          {|

## Task Context

**Patch %s: %s**

### Problem Statement
%s

### Solution Summary
%s%s%s%s|}
          patch_id patch.Patch.title gp.Gameplan.problem_statement
          gp.Gameplan.solution_summary desc_section changes_section ac_section
    | _ -> ""
  in
  let vars =
    [
      ("project_name", project_name);
      ("base_branch", base_branch);
      ( "pr_number",
        match pr_number with
        | Some n -> Int.to_string (Pr_number.to_int n)
        | None -> "" );
      ("git_status", git_status);
      ("git_diff", git_diff);
      ("task_context", task_context);
    ]
  in
  render_with_override ~project_name ~name:"merge_conflict" ~vars
    ~default:(fun () ->
      Printf.sprintf
        {|# Merge Conflict%s

A rebase onto `%s` is already in progress but hit conflicts.

Resolve each conflicted file, then stage and continue:

```
git add <resolved files>
git rebase --continue
```

If the rebase continues and hits further conflicts, repeat the process.

Do NOT run `git rebase origin/%s` — the rebase is already set up with the
correct --onto range. Starting a new rebase would re-introduce dependency
commits that have already been stripped.

After resolving all conflicts and completing the rebase, the supervisor will push the rebased commits for you — do not run `git push`.%s%s%s|}
        pr_ctx base_branch base_branch status_section diff_section task_context)

let render_human_message_prompt ~(project_name : string)
    (messages : string list) =
  match messages with
  | [] -> "No messages."
  | _ ->
      let formatted_flat = String.concat ~sep:"\n" messages in
      let formatted_numbered =
        List.mapi messages ~f:(fun i msg -> Printf.sprintf "%d. %s" (i + 1) msg)
        |> String.concat ~sep:"\n"
      in
      let vars =
        [
          ("project_name", project_name);
          ("messages", formatted_flat);
          ("messages_numbered", formatted_numbered);
          ("count", Int.to_string (List.length messages));
        ]
      in
      render_with_override ~project_name ~name:"human_message" ~vars
        ~default:(fun () ->
          if Int.equal (List.length messages) 1 then
            Printf.sprintf "# Message from Human\n\n%s" formatted_flat
          else Printf.sprintf "# Messages from Human\n\n%s" formatted_numbered)

let render_base_branch_changed ~old_base ~new_base =
  Printf.sprintf
    "**NOTICE: Your base branch has changed from `%s` to `%s`.** Your PR now \
     targets `%s`. The orchestrator has already rebased your branch and \
     updated the PR target. Do NOT change the PR base branch. Your diff should \
     show only your patch's changes relative to `%s`.\n"
    old_base new_base new_base new_base

let%test "patch prompt includes title and deps" =
  let patch : Patch.t =
    Patch.
      {
        id = Patch_id.of_string "5";
        title = "Prompt renderer";
        description = "";
        branch = Branch.of_string "onton-port/patch-5";
        dependencies = [ Patch_id.of_string "1" ];
        spec = "";
        acceptance_criteria = [];
        files = [];
        classification = "";
        changes = [];
        test_stubs_introduced = [];
        test_stubs_implemented = [];
      }
  in
  let gameplan : Gameplan.t =
    Gameplan.
      {
        project_name = "onton-port";
        problem_statement = "Port Anton to OCaml.";
        solution_summary = "Use Eio for concurrency.";
        final_state_spec = "";
        current_state_analysis = "";
        explicit_opinions = "";
        acceptance_criteria = [];
        open_questions = [];
        patches =
          [
            {
              Patch.id = Patch_id.of_string "1";
              title = "Core types";
              description = "";
              branch = Branch.of_string "onton-port/patch-1";
              dependencies = [];
              spec = "";
              acceptance_criteria = [];
              files = [];
              classification = "";
              changes = [];
              test_stubs_introduced = [];
              test_stubs_implemented = [];
            };
            patch;
          ];
      }
  in
  let result =
    render_patch_prompt ~project_name:"onton-port" patch gameplan
      ~base_branch:"onton-port/patch-1"
  in
  String.is_substring result
    ~substring:"# [onton-port] Patch 5: Prompt renderer"
  && String.is_substring result ~substring:"Patches 1"
  && String.is_substring result ~substring:"Patch 1: Core types"
  && String.is_substring result ~substring:"Patch 5: Prompt renderer"

let%test "substitute_variables replaces placeholders" =
  let result =
    substitute_variables "Hello {{name}}, welcome to {{project}}!"
      [ ("name", "Alice"); ("project", "onton") ]
  in
  String.equal result "Hello Alice, welcome to onton!"

let%test "substitute_variables leaves unknown placeholders" =
  let result =
    substitute_variables "Hello {{name}}, {{unknown}} here" [ ("name", "Bob") ]
  in
  String.equal result "Hello Bob, {{unknown}} here"

let%test "substitute_variables does not expand values" =
  let result =
    substitute_variables "A: {{a}}, B: {{b}}"
      [ ("a", "{{b}}"); ("b", "expanded") ]
  in
  String.equal result "A: {{b}}, B: expanded"

let%test "load_override returns None for missing file" =
  Option.is_none
    (load_override ~project_name:"nonexistent_project" "nonexistent_prompt_name")

let%test "load_override rejects traversal in prompt name" =
  match load_override ~project_name:"test" "../secret" with
  | _ -> false
  | exception Stdlib.Invalid_argument _ -> true

let%test "review prompt formats comments" =
  let comments : Comment.t list =
    [
      Comment.
        {
          id = Comment_id.of_int 1;
          thread_id = Some "PRRT_thread1";
          body = "Fix this function.";
          path = Some "lib/foo.ml";
          line = Some 42;
        };
      Comment.
        {
          id = Comment_id.of_int 2;
          thread_id = None;
          body = "General feedback.";
          path = None;
          line = None;
        };
    ]
  in
  let result = render_review_prompt ~project_name:"test" comments in
  String.is_substring result ~substring:"# Review Comments"
  && String.is_substring result ~substring:"on `lib/foo.ml` (line 42)"
  && String.is_substring result ~substring:"[comment_id=1]"
  && String.is_substring result ~substring:"[thread_id=PRRT_thread1]"
  && String.is_substring result ~substring:"Fix this function."
  && String.is_substring result ~substring:"General feedback."
  && String.is_substring result ~substring:"resolveReviewThread"
  && String.is_substring result ~substring:"gh api repos/"
