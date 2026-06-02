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

let format_precedents (ps : Precedent.t list) : string =
  if List.is_empty ps then ""
  else
    let body =
      List.map ps ~f:(fun p ->
          let url_part =
            match p.Precedent.url with
            | None | Some "" -> ""
            | Some u -> Printf.sprintf " — %s" u
          in
          let why =
            if String.is_empty p.why_applicable then ""
            else " — " ^ p.why_applicable
          in
          Printf.sprintf "- **[%s] %s**%s%s" p.kind p.name url_part why)
      |> String.concat ~sep:"\n"
    in
    "\n\
     ## Established Precedents\n\n\
     This patch should adopt the following proven techniques rather than \
     rolling its own. Read the references when you need detail on the API \
     shape or invariants they impose. Each entry names a library, algorithm, \
     pattern, paper, RFC, doc, or blog post; the trailing sentence explains \
     how it applies to this specific patch.\n\n" ^ body ^ "\n"

let format_context_resources (resources : Context_resource.t list) : string =
  if List.is_empty resources then ""
  else
    let body =
      List.map resources ~f:(fun r ->
          let paths =
            match r.Context_resource.paths with
            | [] -> "Paths/references: none listed"
            | paths -> "Paths/references: " ^ String.concat paths ~sep:", "
          in
          let why =
            if String.is_empty r.why then "Why authoritative: not specified"
            else "Why authoritative: " ^ r.why
          in
          Printf.sprintf "- **%s** (`%s`)\n  - %s\n  - %s" r.id r.kind paths why)
      |> String.concat ~sep:"\n"
    in
    "\n\
     ## Required Context Before Editing\n\n\
     Read these authoritative resources before making code changes. Trust this \
     named context over stale or ambiguous patch prose. Search and read the \
     listed paths/references before choosing an implementation approach, and \
     avoid starting a parallel implementation until you have checked them.\n\n"
    ^ body ^ "\n"

let agents_md_section = function
  | Some content when not (String.is_empty (String.strip content)) ->
      "## Project Conventions (AGENTS.md)\n\n" ^ String.rstrip content ^ "\n\n"
  | Some _ | None -> ""

(* === Three-layer prompt structure ===

   Every layered prompt is composed as

       gameplan_layer ^ patch_layer ^ turn_layer

   so prefix-cache hits accumulate at the layer boundaries:

   - {!render_gameplan_layer} produces a string that is byte-identical
     across every layered prompt in a single gameplan run (cache reuse
     across patches and turn kinds).
   - {!render_patch_layer} produces a string that is byte-identical
     across every layered prompt for one patch agent during a period
     where [pr_number] and [base_branch] are stable (cache reuse across
     follow-up turns of the same patch).
   - The per-kind [render_turn_layer_*] helpers produce strictly
     turn-dynamic content that lives at the tail.

   Project-level overrides are now per-layer:
   [prompts/gameplan.md], [prompts/patch.md], and
   [prompts/turn_<kind>.md] (e.g. [prompts/turn_ci.md]). Each layer
   renders independently; overriding one layer leaves the others' cache
   structure intact. The legacy whole-prompt override names
   ([prompts/patch.md] used to mean the entire Start prompt;
   [prompts/review.md], [prompts/ci_failure.md], etc.) are no longer
   honoured. *)

(* The gameplan-reference section points agents at the read-only copy
   published by [Project_store.publish_gameplan_artifact] at startup. The
   path is a pure function of the project name (no filesystem probe here),
   so the rendered gameplan layer stays byte-identical across the run. *)
let gameplan_reference_section ~(project_name : string) : string =
  Printf.sprintf
    "\n\
     ## Full Gameplan Reference\n\n\
     A read-only copy of the complete gameplan JSON — every patch's full \
     description, spec, acceptance criteria, and the functional-change \
     ownership map — is saved at:\n\n\
     `%s`\n\n\
     Everything your patch needs is already in this prompt, so most sessions \
     never read it. Consult it only when you genuinely need cross-patch \
     context — for example, to check a sibling patch's scope before deciding \
     whether a change belongs to you. Do not edit the file, and do not take on \
     work owned by sibling patches.\n"
    (Project_store.gameplan_artifact_path project_name)

let render_gameplan_layer ~(project_name : string) (gameplan : Gameplan.t) :
    string =
  let patches_list =
    List.map gameplan.Gameplan.patches ~f:(fun (p : Patch.t) ->
        Printf.sprintf "- Patch %s: %s"
          (Patch_id.to_string p.Patch.id)
          p.Patch.title)
    |> String.concat ~sep:"\n"
  in
  let vars =
    [
      ("project_name", project_name);
      ("problem_statement", gameplan.Gameplan.problem_statement);
      ("solution_summary", gameplan.Gameplan.solution_summary);
      ( "final_state_spec_section",
        optional_section ~header:"Final State Specification (Non-negotiable)"
          gameplan.Gameplan.final_state_spec );
      ( "explicit_opinions_section",
        optional_section ~header:"Explicit Opinions (Non-negotiable)"
          gameplan.Gameplan.explicit_opinions );
      ( "current_state_section",
        optional_section ~header:"Current State Analysis"
          gameplan.Gameplan.current_state_analysis );
      ("patches_list", patches_list);
      ("gameplan_reference_section", gameplan_reference_section ~project_name);
    ]
  in
  render_with_override ~project_name ~name:"gameplan" ~vars ~default:(fun () ->
      substitute_variables
        {|# [{{project_name}}]

## Problem Statement
{{problem_statement}}

## Solution Summary
{{solution_summary}}
{{final_state_spec_section}}{{explicit_opinions_section}}{{current_state_section}}
## Patches in Gameplan
{{patches_list}}
{{gameplan_reference_section}}
|}
        vars)

let format_functional_changes_section (fcs : Functional_change.t list) : string
    =
  if List.is_empty fcs then ""
  else
    let body =
      List.map fcs ~f:(fun (fc : Functional_change.t) ->
          Printf.sprintf "- **%s** — %s" fc.id fc.description)
      |> String.concat ~sep:"\n"
    in
    "\n\
     ## Functional Changes You Own\n\n\
     These are the user-visible / behavioural changes assigned to this patch. \
     Each is your responsibility — do not defer them to another patch, and do \
     not stop until every one is delivered. The gameplan's enumeration is \
     exhaustive and each change is owned by exactly one patch, so if a change \
     appears here, no sibling patch will pick it up.\n\n" ^ body ^ "\n"

(* The dependency-notes section points the agent at the implementation notes
   each ancestor patch's agent records via its Pr_body session
   ([Project_store.pr_body_artifact_path]). Start is gated on every unmerged
   dep having delivered its notes (deps-notes-ready in the spec, enforced by
   [Patch_controller.plan_action_for_patch]), so the files exist by the time
   this layer is first read — except for an ancestor that merged before its
   notes step ran. Paths are pure functions of the project name and patch id —
   no filesystem probe here — so the rendered patch layer stays byte-identical
   across a patch's sessions. *)
let dependency_notes_section ~(project_name : string) (ancestors : Patch.t list)
    : string =
  if List.is_empty ancestors then ""
  else
    let entries =
      List.map ancestors ~f:(fun (p : Patch.t) ->
          Printf.sprintf "- Patch %s: %s — `%s`"
            (Patch_id.to_string p.Patch.id)
            p.Patch.title
            (Project_store.pr_body_artifact_path ~project_name
               ~patch_id:p.Patch.id))
      |> String.concat ~sep:"\n"
    in
    Printf.sprintf
      "\n\
       ## Dependency Implementation Notes\n\n\
       Your worktree already contains the changes from these ancestor patches. \
       Each patch's agent records implementation notes — key decisions, \
       deviations from plan, gotchas — at:\n\n\
       %s\n\n\
       Like the gameplan reference, these are read-only, on-demand context: \
       consult a file only when you need to understand a decision your patch \
       builds on. Each ancestor's notes are recorded before a dependent patch \
       starts; in the rare case a file is missing, that ancestor merged before \
       its notes step ran. Do not edit these files.\n"
      entries

let render_patch_layer ~(project_name : string) (patch : Patch.t) ?pr_number
    ?(functional_changes = []) ?(context_resources = []) ?(ancestors = [])
    ~(base_branch : string) () : string =
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
        "**NOTE:** Your branch is based on `%s`, which is a dependency patch's \
         branch. Your worktree already contains that patch's changes. This is \
         expected — build on top of those changes. Your PR will target `%s`, \
         so the diff should only show YOUR patch's changes, not the \
         dependency's.\n\n"
        base_branch base_branch
  in
  let pr_instructions =
    (* Commit subjects are not rewritten or rejected downstream. The rebase
       subject filter in [Worktree.rebase_onto] treats this prompt convention as
       a best-effort agent contract: malformed subjects simply will not match
       [Worktree.is_ancestor_patch_subject], so rebase falls back to the other
       ancestry / patch-id paths. *)
    let commit_block =
      Printf.sprintf
        {|
**Commit your work with `git commit` before ending the session.** Every code change you make must land in a commit — uncommitted changes are discarded by the supervisor's push, and no PR can be opened from an empty branch. Multiple commits are fine; commit whenever it makes sense.

**Prefix every commit subject with `[%s] Patch %s: `** (the same project name and patch number used by this branch and its PR title), e.g. `[%s] Patch %s: <short summary>`. This lets the supervisor recognize your commits as belonging to this patch when rebasing onto dependency branches later.

**Do NOT run `git push` or `gh pr create` yourself.** The supervisor pushes your commits and opens/updates the PR.|}
        project_name patch_id project_name patch_id
    in
    match pr_number with
    | Some _ -> commit_block
    | None ->
        commit_block
        ^ {|

The supervisor opens the draft PR after your first commit lands on the remote, with a gameplan-derived title and body.|}
  in
  let vars =
    [
      ("project_name", project_name);
      ("title", patch.Patch.title);
      ("classification_note", classification_note);
      ("dependencies", deps);
      ("branch", branch);
      ("base_branch", base_branch);
      ("patch_id", patch_id);
      ( "pr_number",
        match pr_number with
        | Some n -> Int.to_string (Pr_number.to_int n)
        | None -> "" );
      ("pr_str", pr_str);
      ("description", patch.Patch.description);
      ("spec", patch.Patch.spec);
      ("acceptance_criteria", format_list patch.Patch.acceptance_criteria);
      ("files", format_list patch.Patch.files);
      ( "functional_changes_section",
        format_functional_changes_section functional_changes );
      ("context_resources_section", format_context_resources context_resources);
      ( "dependency_notes_section",
        dependency_notes_section ~project_name ancestors );
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
      ("precedents_section", format_precedents patch.Patch.precedents);
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
        {|## Patch {{patch_id}}{{classification_note}}: {{title}}

## Dependencies
{{dependencies}}
{{dependency_notes_section}}
## Your Task

{{base_branch_note}}{{description}}
{{functional_changes_section}}{{context_resources_section}}{{changes_section}}{{files_section}}{{precedents_section}}{{test_stubs_introduced_section}}{{test_stubs_implemented_section}}{{spec_section}}{{acceptance_criteria_section}}
## Git Instructions
- Branch: {{branch}}
- Base branch: {{base_branch}}
- PR: {{pr_str}}
{{pr_instructions}}

|}
        vars)

let owned_functional_changes (gameplan : Gameplan.t) (patch : Patch.t) :
    Functional_change.t list =
  List.filter gameplan.Gameplan.functional_changes
    ~f:(fun (fc : Functional_change.t) ->
      Patch_id.equal fc.Functional_change.owned_by patch.Patch.id)

let required_context_resources (gameplan : Gameplan.t) (patch : Patch.t) :
    Context_resource.t list =
  List.filter gameplan.Gameplan.context_resources
    ~f:(fun (r : Context_resource.t) ->
      List.mem patch.Patch.required_context r.id ~equal:String.equal)

let ancestor_patches (gameplan : Gameplan.t) (patch : Patch.t) : Patch.t list =
  let graph = Graph.of_patches gameplan.Gameplan.patches in
  Graph.transitive_ancestors graph patch.Patch.id
  |> List.filter_map ~f:(fun id ->
      List.find gameplan.Gameplan.patches ~f:(fun (p : Patch.t) ->
          Patch_id.equal p.Patch.id id))

(* When all of [patch], [gameplan], [base_branch] are supplied, returns
   the gameplan+patch prefix; otherwise returns the empty string. Used by
   the layered turn-prompt composers to support callers that don't have a
   gameplan-defined patch in scope (e.g. ad-hoc PRs). *)
let layered_prefix ~project_name ?pr_number ?patch ?gameplan ?base_branch
    ?agents_md () =
  match (patch, gameplan, base_branch) with
  | Some p, Some g, Some b ->
      let functional_changes = owned_functional_changes g p in
      let context_resources = required_context_resources g p in
      let ancestors = ancestor_patches g p in
      render_gameplan_layer ~project_name g
      ^ (match agents_md with
        | Some content -> agents_md_section (Some content)
        | None -> "")
      ^ render_patch_layer ~project_name p ?pr_number ~functional_changes
          ~context_resources ~ancestors ~base_branch:b ()
  | _ -> ""

let render_turn_layer_start ~(project_name : string) : string =
  let vars = [ ("project_name", project_name) ] in
  render_with_override ~project_name ~name:"turn_start" ~vars
    ~default:(fun () -> "Continue implementing until all tests pass.\n")

let render_patch_prompt ~(project_name : string) ?agents_md ?pr_number
    (patch : Patch.t) (gameplan : Gameplan.t) ~(base_branch : string) =
  let functional_changes = owned_functional_changes gameplan patch in
  let context_resources = required_context_resources gameplan patch in
  let ancestors = ancestor_patches gameplan patch in
  render_gameplan_layer ~project_name gameplan
  ^ agents_md_section agents_md
  ^ render_patch_layer ~project_name patch ?pr_number ~functional_changes
      ~context_resources ~ancestors ~base_branch ()
  ^ render_turn_layer_start ~project_name

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
      complexity = None;
      precedents = [];
      required_context = [];
    }
  in
  let gameplan =
    {
      Gameplan.project_name = "";
      repo_owner = "";
      repo_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches = [];
      functional_changes = [];
      context_resources = [];
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
      complexity = None;
      precedents = [];
      required_context = [];
    }
  in
  let gameplan =
    {
      Gameplan.project_name = "";
      repo_owner = "";
      repo_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "module FOO.\nsome spec";
      patches = [];
      functional_changes = [];
      context_resources = [];
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
      complexity = None;
      precedents = [];
      required_context = [];
    }
  in
  let gameplan =
    {
      Gameplan.project_name = "";
      repo_owner = "";
      repo_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches = [];
      functional_changes = [];
      context_resources = [];
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
      complexity = None;
      precedents = [];
      required_context = [];
    }
  in
  let gameplan =
    {
      Gameplan.project_name = "";
      repo_owner = "";
      repo_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "module FOO.\ngameplan spec";
      patches = [];
      functional_changes = [];
      context_resources = [];
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
      ("precedents_section", format_precedents patch.Patch.precedents);
    ]
  in
  render_with_override ~project_name ~name:"pr_description" ~vars
    ~default:(fun () ->
      substitute_variables
        {|## Patch {{patch_id}}: {{title}}

{{description}}
{{changes_section}}{{gameplan_spec_section}}{{patch_spec_section}}{{acceptance_criteria_section}}{{files_section}}{{precedents_section}}|}
        vars)

let render_pr_body_prompt ~(project_name : string) ~(pr_number : Pr_number.t)
    ~(pr_body : string) ~(spec_suffix : string) ~(artifact_path : string) =
  let pr_num_str = Int.to_string (Pr_number.to_int pr_number) in
  let spec_context =
    if String.is_empty (String.strip spec_suffix) then ""
    else "\n\nThe specifications governing this patch:\n" ^ spec_suffix ^ "\n"
  in
  let vars =
    [
      ("pr_number", pr_num_str);
      ("pr_body", pr_body);
      ("spec_context", spec_context);
      ("artifact_path", artifact_path);
    ]
  in
  render_with_override ~project_name ~name:"pr_body" ~vars ~default:(fun () ->
      substitute_variables
        {|You have just finished implementing this patch. PR #{{pr_number}} was opened with a gameplan-derived body and specifications that will be kept as-is.

The current PR body is:

---
{{pr_body}}
---
{{spec_context}}
The supervisor will keep this description and specs on the PR. Your job is to write **additional notes** — anything a reviewer needs beyond the gameplan description. The supervisor will append your notes to the PR body under an `## Implementation Notes` header. Agents implementing patches that depend on this one are also pointed at these notes, so they double as a handoff to the work built on top of your changes.

**Write just the notes content (no header) to `{{artifact_path}}`.** This is an absolute path outside the worktree — write it with the Write tool. Do NOT run `gh`, `git`, or any forge command; the supervisor reads the file and PATCHes the PR.

What to include:

- Key implementation decisions and trade-offs you made.
- Anything surprising or non-obvious about the approach.
- Deviations from the original plan (if any).
- Important details a reviewer should know.
- Anything an agent building a dependent patch on top of yours should know: renames, new invariants, API changes, gotchas.
- A short `Spec/Evidence Mapping` when applicable: spec clause or acceptance criterion satisfied, code path implementing it, required context resource consulted, and test/static check proving it if available.

Do not repeat information already in the description or specs above — add only what's new.

Keep it concise — a few bullet points usually suffices. If you have nothing material to add (the patch is straightforward), write a single line acknowledging that.|}
        vars)

(* Private helper — truncates a Git SHA to 7 chars for prompt display. *)
let short_sha s = if String.length s <= 7 then s else String.sub s ~pos:0 ~len:7

let render_turn_layer_review ~(project_name : string) ?pr_number
    ?current_head_sha (comments : Comment.t list) : string =
  match comments with
  | [] -> "No review comments to address."
  | _ ->
      let pr_num_str =
        match pr_number with
        | Some n -> Int.to_string (Pr_number.to_int n)
        | None -> "{pr_number}"
      in
      (* [outdated] is GitHub's own [isOutdated] on the thread — the line
         the comment anchors to was changed by a later commit (or the file
         was deleted). Do not infer from [commit] vs [originalCommit]:
         GitHub advances [commit] to any later commit that still contains
         the anchored line, including commits that touched unrelated
         files, so that comparison produces false positives. *)
      let is_outdated (c : Comment.t) = c.Comment.outdated in
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
            (* [at=…] displays [original_commit_sha] — the SHA at which the
               reviewer's diff view was anchored when they wrote the comment.
               For an outdated comment, [commit_sha] holds the later commit
               GitHub re-anchored to, but we still show the original so the
               agent knows which diff the reviewer was looking at. *)
            let anchor =
              match c.Comment.original_commit_sha with
              | Some sha -> Printf.sprintf " [at=%s]" (short_sha sha)
              | None -> ""
            in
            let outdated = if is_outdated c then " [outdated]" else "" in
            Printf.sprintf "- **Comment**%s%s%s%s%s: %s" location db_id
              thread_ref anchor outdated c.Comment.body)
        |> String.concat ~sep:"\n\n"
      in
      (* Both [pr_ctx] and [sha_anchor] are splatted into the
         [Printf.sprintf "# Review Comments%s%s\n\n…"] format below. Each
         owns its leading `\n\n` separator and no trailing newline, so
         any subset of {pr_ctx, sha_anchor} renders with exactly one
         blank line between each non-empty block (instead of two, which
         is what happens if pr_ctx also ends in `\n`). *)
      let pr_ctx =
        match pr_number with
        | Some n -> Printf.sprintf "\n\nPR: #%d" (Pr_number.to_int n)
        | None -> ""
      in
      (* Comments without [original_commit_sha] are dropped here. If the batch
         mixes SHA-bearing and SHA-less comments, the preamble only lists the
         SHA-bearing ones and the SHA-less entries simply omit the [at=…]
         annotation — by design, since we'd be fabricating an anchor otherwise. *)
      let reviewed_at_shas =
        List.filter_map comments ~f:(fun (c : Comment.t) ->
            c.Comment.original_commit_sha)
        |> List.dedup_and_sort ~compare:String.compare
      in
      let sha_anchor =
        match (current_head_sha, reviewed_at_shas) with
        | Some head, (_ :: _ as shas) ->
            let anchored =
              match shas with
              | [ sha ] -> Printf.sprintf "commit `%s`" (short_sha sha)
              | many ->
                  "commits "
                  ^ (List.map many ~f:(fun s ->
                         Printf.sprintf "`%s`" (short_sha s))
                    |> String.concat ~sep:", ")
            in
            (* No trailing newline — the format-string literal supplies the
               \n\n separator before "The following review comments". *)
            Printf.sprintf
              "\n\n\
               Review anchored at %s. Current branch HEAD is `%s`.\n\
               If a comment is marked `[outdated]` or refers to code that no \
               longer exists at HEAD, reply acknowledging it's addressed in a \
               later commit and skip — do not re-do the change."
              anchored (short_sha head)
        | _ -> ""
      in
      let reviewed_at_sha_var =
        match reviewed_at_shas with
        | [ sha ] -> short_sha sha
        | _ :: _ as many ->
            List.map many ~f:short_sha |> String.concat ~sep:", "
        | [] -> ""
      in
      let current_head_sha_var =
        match current_head_sha with Some s -> short_sha s | None -> ""
      in
      let vars =
        [
          ("project_name", project_name);
          ("comments", formatted);
          ("count", Int.to_string (List.length comments));
          ("pr_number", pr_num_str);
          ("reviewed_at_sha", reviewed_at_sha_var);
          ("current_head_sha", current_head_sha_var);
          ("sha_anchor", sha_anchor);
        ]
      in
      render_with_override ~project_name ~name:"turn_review" ~vars
        ~default:(fun () ->
          Printf.sprintf
            "# Review Comments%s%s\n\n\
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
            pr_ctx sha_anchor formatted pr_num_str)

let render_review_prompt ~(project_name : string) ?agents_md ?pr_number
    ?current_head_sha ?patch ?gameplan ?base_branch (comments : Comment.t list)
    : string =
  layered_prefix ~project_name ?pr_number ?patch ?gameplan ?base_branch
    ?agents_md ()
  ^ render_turn_layer_review ~project_name ?pr_number ?current_head_sha comments

let render_turn_layer_findings ~(project_name : string) ?pr_number
    ?current_head_sha ~artifact_path (findings : Review_service.finding list) :
    string =
  let pr_num_str =
    match pr_number with
    | Some n -> Printf.sprintf "#%d" (Pr_number.to_int n)
    | None -> "for this PR"
  in
  let sha_anchor =
    match current_head_sha with
    | Some sha ->
        Printf.sprintf
          "\n\n\
           Note: review findings are anchored to specific commit SHAs (the \
           [posting_sha] field below). The current HEAD is %s. If a finding's \
           [posting_sha] differs from HEAD, the line numbers may have drifted \
           — re-locate the issue by content, not by line number, before \
           editing."
          sha (* sha intentionally not abbreviated — exact value matters *)
    | None -> ""
  in
  let formatted =
    match findings with
    | [] -> "No outstanding findings."
    | _ ->
        List.map findings ~f:(fun (f : Review_service.finding) ->
            let lines =
              if f.start_line = f.end_line then
                Printf.sprintf "%s:%d" f.path f.start_line
              else Printf.sprintf "%s:%d-%d" f.path f.start_line f.end_line
            in
            Printf.sprintf
              "## [%s] finding %s\nanchor: %s\nposting_sha: %s\n\n%s"
              (Review_service.severity_to_string f.severity)
              f.id lines f.posting_sha f.body)
        |> String.concat ~sep:"\n\n---\n\n"
  in
  let vars =
    [
      ("project_name", project_name);
      ("findings", formatted);
      ("count", Int.to_string (List.length findings));
      ( "pr_number",
        match pr_number with
        | Some n -> Int.to_string (Pr_number.to_int n)
        | None -> "" );
      ("pr_ref", pr_num_str);
      ("current_head_sha", Option.value current_head_sha ~default:"");
      ("sha_anchor", sha_anchor);
      ("artifact_path", artifact_path);
    ]
  in
  render_with_override ~project_name ~name:"turn_findings" ~vars
    ~default:(fun () ->
      Printf.sprintf
        "You are reviewing pull request %s. The findings below come from a \
         review service, not from GitHub review threads — there is no thread \
         to reply to. To resolve a finding, you must EITHER (a) make code \
         changes that address it, OR (b) explicitly mark it as wontfix in an \
         artifact file.%s\n\n\
         %s\n\n\
         ## Resolving findings\n\
         - For each finding you fix in code, do nothing extra: it will be \
         reported back as `addressed` after this session.\n\
         - For each finding you decide NOT to fix (out of scope, false \
         positive, intentional, etc.), write an entry to the artifact file at \
         `%s`. The supervisor reads it after this session and reports those \
         findings as `wontfix`.\n\
         - Artifact format: a JSON array of objects with required string field \
         `id` (the composite finding id shown above) and required string field \
         `reason`.\n\
         - If you create or update the artifact, use the Write tool with the \
         absolute path above (it is outside the worktree on purpose — do not \
         commit it).\n\n\
         After addressing the in-scope findings, commit your changes. The \
         supervisor will push them for you — do not run `git push`."
        pr_num_str sha_anchor formatted artifact_path)

let render_findings_prompt ~(project_name : string) ?agents_md ?pr_number
    ?current_head_sha ?patch ?gameplan ?base_branch ~artifact_path
    (findings : Review_service.finding list) : string =
  layered_prefix ~project_name ?pr_number ?patch ?gameplan ?base_branch
    ?agents_md ()
  ^ render_turn_layer_findings ~project_name ?pr_number ?current_head_sha
      ~artifact_path findings

let render_turn_layer_ci ~(project_name : string) ?pr_number
    (checks : Ci_check.t list) : string =
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
      render_with_override ~project_name ~name:"turn_ci" ~vars
        ~default:(fun () ->
          Printf.sprintf
            "# CI Failures%s\n\n\
             The following CI checks failed:\n\n\
             %s\n\n\
             After making your changes, commit them. The supervisor will push \
             them for you — do not run `git push`."
            pr_ctx formatted)

let render_ci_failure_prompt ~(project_name : string) ?agents_md ?pr_number
    ?patch ?gameplan ?base_branch (checks : Ci_check.t list) : string =
  layered_prefix ~project_name ?pr_number ?patch ?gameplan ?base_branch
    ?agents_md ()
  ^ render_turn_layer_ci ~project_name ?pr_number checks

let render_turn_layer_ci_unknown ~(project_name : string) ?pr_number () =
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
  render_with_override ~project_name ~name:"turn_ci_unknown" ~vars
    ~default:(fun () ->
      Printf.sprintf
        "# CI Failures%s\n\n\
         One or more CI checks failed. Please investigate the failures and fix \
         them.\n\n\
         Run the CI checks locally or check the PR status for details.\n\n\
         After making your changes, commit them. The supervisor will push them \
         for you — do not run `git push`."
        pr_ctx)

let render_ci_failure_unknown_prompt ~(project_name : string) ?agents_md
    ?pr_number ?patch ?gameplan ?base_branch () : string =
  layered_prefix ~project_name ?pr_number ?patch ?gameplan ?base_branch
    ?agents_md ()
  ^ render_turn_layer_ci_unknown ~project_name ?pr_number ()

let render_recovery_section (ci : Worktree.conflict_info) =
  let bullet (c : Worktree.unique_commit) =
    let short =
      if String.length c.sha >= 7 then String.sub c.sha ~pos:0 ~len:7 else c.sha
    in
    Printf.sprintf "  %s %s" short c.subject
  in
  (* unique_commits is git-log order (newest-first); the bullet list reads
     oldest-first so an agent reapplying with cherry-pick can scan top-to-bottom
     in commit-order. *)
  let commits_section =
    if List.is_empty ci.Worktree.unique_commits then ""
    else
      let commits_lines =
        List.rev ci.Worktree.unique_commits
        |> List.map ~f:bullet |> String.concat ~sep:"\n"
      in
      Printf.sprintf "\n\nCommits unique to this patch (oldest first):\n%s"
        commits_lines
  in
  let orig_head_block =
    if String.is_empty ci.Worktree.orig_head then ""
    else
      Printf.sprintf
        {|

If you need to discard your in-progress conflict resolution and start over
from your pre-rebase state, the supervisor captured your HEAD before the
rebase began:

    git reset --hard %s|}
        ci.Worktree.orig_head
  in
  match ci.Worktree.strategy with
  | Worktree.Onto ->
      Printf.sprintf
        {|

## Recovery (if rebase state is lost)

If `git status` no longer shows a rebase in progress (e.g. you ran
`git rebase --abort` or the worktree was reset), do NOT run
`git rebase %s` against your local tracking ref — it may be stale
and would re-pick already-merged dependency commits.

First refresh remote tracking refs, then restart with the same `--onto`
range the supervisor used:

    git fetch origin
    git rebase --onto %s %s%s%s|}
        ci.target ci.target ci.old_base commits_section orig_head_block
  | Worktree.Plain ->
      Printf.sprintf
        {|

## Recovery (if rebase state is lost)

If `git status` no longer shows a rebase in progress, refresh remote
tracking refs and restart with:

    git fetch origin
    git rebase %s

(No per-patch commit list could be isolated — the supervisor fell back
to a plain rebase against `%s` because no unique commits were
identified.)%s|}
        ci.target ci.target orig_head_block

let render_turn_layer_merge_conflict ~(project_name : string) ?pr_number
    ~(base_branch : string) ?(git_status = "") ?(git_diff = "") ?conflict_info
    () : string =
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
  let recovery_section =
    match conflict_info with
    | Some ci -> render_recovery_section ci
    | None -> ""
  in
  let old_base_var =
    match conflict_info with Some ci -> ci.Worktree.old_base | None -> ""
  in
  let target_branch_var =
    match conflict_info with Some ci -> ci.Worktree.target | None -> ""
  in
  let orig_head_var =
    match conflict_info with Some ci -> ci.Worktree.orig_head | None -> ""
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
      ("recovery_section", recovery_section);
      ("old_base", old_base_var);
      ("target_branch", target_branch_var);
      ("orig_head", orig_head_var);
    ]
  in
  render_with_override ~project_name ~name:"turn_merge_conflict" ~vars
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
        pr_ctx base_branch base_branch status_section diff_section
        recovery_section)

let render_merge_conflict_prompt ~(project_name : string) ?agents_md ?pr_number
    ?patch ?gameplan ~(base_branch : string) ?(git_status = "") ?(git_diff = "")
    ?conflict_info () : string =
  layered_prefix ~project_name ?pr_number ?patch ?gameplan
    ?base_branch:(Some base_branch) ?agents_md ()
  ^ render_turn_layer_merge_conflict ~project_name ?pr_number ~base_branch
      ~git_status ~git_diff ?conflict_info ()

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
        complexity = None;
        precedents = [];
        required_context = [];
      }
  in
  let gameplan : Gameplan.t =
    Gameplan.
      {
        project_name = "onton-port";
        repo_owner = "flowglad";
        repo_name = "onton";
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
              complexity = None;
              precedents = [];
              required_context = [];
            };
            patch;
          ];
        functional_changes = [];
        context_resources = [];
      }
  in
  let result =
    render_patch_prompt ~project_name:"onton-port" patch gameplan
      ~base_branch:"onton-port/patch-1"
  in
  String.is_substring result ~substring:"# [onton-port]\n"
  && String.is_substring result ~substring:"Patches 1"
  && String.is_substring result ~substring:"Patch 1: Core types"
  && String.is_substring result ~substring:"## Patch 5: Prompt renderer"

let prompt_prefix_through_patch_heading prompt =
  let marker = "## Patch " in
  let all = String.substr_index_all prompt ~pattern:marker ~may_overlap:false in
  match List.last all with
  | None -> None
  | Some idx -> Some (String.prefix prompt (idx + String.length marker))

let%test "patch prompt static prefix is byte-identical across patches" =
  let patch_1 : Patch.t =
    Patch.
      {
        id = Patch_id.of_string "1";
        title = "Restructure prompt";
        description = "Make shared content static.";
        branch = Branch.of_string "headless-cache-tuning/patch-1";
        dependencies = [];
        spec = "module P1.";
        acceptance_criteria = [ "Prefix is stable." ];
        files = [ "lib/prompt.ml" ];
        classification = "INFRA";
        changes = [ "Move shared sections above patch heading." ];
        test_stubs_introduced = [];
        test_stubs_implemented =
          [
            "Prompt > static prefix is byte-identical across patches in one \
             gameplan";
          ];
        complexity = None;
        precedents = [];
        required_context = [];
      }
  in
  let patch_2 : Patch.t =
    Patch.
      {
        id = Patch_id.of_string "2";
        title = "Add Claude flags";
        description = "Add spawn args.";
        branch = Branch.of_string "headless-cache-tuning/patch-2";
        dependencies = [ Patch_id.of_string "1" ];
        spec = "module P2.";
        acceptance_criteria = [ "Claude gets the new flag." ];
        files = [ "lib/claude_runner.ml" ];
        classification = "INFRA";
        changes = [ "Emit exclude-dynamic-system-prompt-sections." ];
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
        project_name = "onton";
        repo_owner = "flowglad";
        repo_name = "onton";
        problem_statement = "Prompt cache hit rate is low.\n\n## Patch notes";
        solution_summary = "Move shared prompt content into a stable prefix.";
        final_state_spec = "module HEADLESS_CACHE_TUNING.\n\n## Patch state.";
        current_state_analysis =
          "Patch-specific text currently appears first.\n\n## Patch drift.";
        explicit_opinions = "- Use env vars for flags.\n- ## Patch fallback.";
        acceptance_criteria = [];
        open_questions = [];
        patches = [ patch_1; patch_2 ];
        functional_changes = [];
        context_resources = [];
      }
  in
  let prompt_1 =
    render_patch_prompt ~project_name:"onton" patch_1 gameplan
      ~base_branch:"main"
  in
  let prompt_2 =
    render_patch_prompt ~project_name:"onton" patch_2 gameplan
      ~base_branch:"headless-cache-tuning/patch-1"
  in
  match
    ( prompt_prefix_through_patch_heading prompt_1,
      prompt_prefix_through_patch_heading prompt_2 )
  with
  | Some prefix_1, Some prefix_2 -> String.equal prefix_1 prefix_2
  | _ -> false

let%test "agents_md content appears in static prefix when Some" =
  let patch : Patch.t =
    Patch.
      {
        id = Patch_id.of_string "7";
        title = "Bare Claude";
        description = "Inject AGENTS.md into prompt prefix.";
        branch = Branch.of_string "headless-cache-tuning/patch-7";
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
        project_name = "onton";
        repo_owner = "flowglad";
        repo_name = "onton";
        problem_statement = "Prompt cache hit rate is low.";
        solution_summary = "Keep shared content in a stable prefix.";
        final_state_spec = "";
        current_state_analysis = "";
        explicit_opinions = "";
        acceptance_criteria = [];
        open_questions = [];
        patches = [ patch ];
        functional_changes = [];
        context_resources = [];
      }
  in
  let prompt =
    render_patch_prompt ~project_name:"onton"
      ~agents_md:"Follow AGENTS.md.\nNever use *_exn." patch gameplan
      ~base_branch:"main"
  in
  match prompt_prefix_through_patch_heading prompt with
  | None -> false
  | Some prefix ->
      String.is_substring prefix ~substring:"## Project Conventions (AGENTS.md)"
      && String.is_substring prefix ~substring:"Never use *_exn."

let%test "agents_md section is omitted when None" =
  let patch : Patch.t =
    Patch.
      {
        id = Patch_id.of_string "7";
        title = "Bare Claude";
        description = "Inject AGENTS.md into prompt prefix.";
        branch = Branch.of_string "headless-cache-tuning/patch-7";
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
        project_name = "onton";
        repo_owner = "flowglad";
        repo_name = "onton";
        problem_statement = "Prompt cache hit rate is low.";
        solution_summary = "Keep shared content in a stable prefix.";
        final_state_spec = "";
        current_state_analysis = "";
        explicit_opinions = "";
        acceptance_criteria = [];
        open_questions = [];
        patches = [ patch ];
        functional_changes = [];
        context_resources = [];
      }
  in
  let prompt =
    render_patch_prompt ~project_name:"onton" patch gameplan ~base_branch:"main"
  in
  not
    (String.is_substring prompt ~substring:"## Project Conventions (AGENTS.md)")

let%test "agents_md section normalizes trailing newline spacing" =
  String.equal
    (agents_md_section (Some "Rule one.\n"))
    "## Project Conventions (AGENTS.md)\n\nRule one.\n\n"

(* === Three-layer invariants ===

   The fixture below is reused by every layer-invariant test in this
   block. Two patches in the same gameplan, both with non-empty patch
   fields, so the helpers exercise the full templates. *)

let make_layer_test_fixture () =
  let patch_a : Patch.t =
    Patch.
      {
        id = Patch_id.of_string "1";
        title = "Add layer helpers";
        description = "Introduce render_*_layer helpers.";
        branch = Branch.of_string "feat/patch-1";
        dependencies = [];
        spec = "module L1.";
        acceptance_criteria = [ "Three helpers exist." ];
        files = [ "lib/prompt.ml" ];
        classification = "INFRA";
        changes = [ "Add helpers." ];
        test_stubs_introduced = [];
        test_stubs_implemented = [];
        complexity = None;
        precedents = [];
        required_context = [];
      }
  in
  let patch_b : Patch.t =
    Patch.
      {
        id = Patch_id.of_string "2";
        title = "Wire dispatch";
        description = "Pass patch + gameplan + base_branch to follow-ups.";
        branch = Branch.of_string "feat/patch-2";
        dependencies = [ Patch_id.of_string "1" ];
        spec = "module L2.";
        acceptance_criteria = [ "Follow-ups receive the layer args." ];
        files = [ "bin/main.ml" ];
        classification = "";
        changes = [ "Add Base.List.find for patch resolution." ];
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
        project_name = "onton";
        repo_owner = "flowglad";
        repo_name = "onton";
        problem_statement = "Prompts mix gameplan, patch, and turn content.";
        solution_summary = "Compose three layers in a fixed order.";
        final_state_spec = "module THREE_LAYERS.";
        current_state_analysis = "Today only the Start prompt is layered.";
        explicit_opinions = "- Caching pays off when prefixes repeat.";
        acceptance_criteria = [];
        open_questions = [];
        patches = [ patch_a; patch_b ];
        functional_changes =
          [
            {
              Functional_change.id = "FC-LAYER-1";
              description =
                "Patch A owns a behavior that must stay in the cache-stable \
                 layer.";
              owned_by = patch_a.Patch.id;
            };
          ];
        context_resources = [];
      }
  in
  (patch_a, patch_b, gameplan)

let%test "gameplan_layer is the prefix of render_patch_prompt for both patches"
    =
  let patch_a, patch_b, gameplan = make_layer_test_fixture () in
  let g_layer = render_gameplan_layer ~project_name:"onton" gameplan in
  let prompt_a =
    render_patch_prompt ~project_name:"onton" patch_a gameplan
      ~base_branch:"main"
  in
  let prompt_b =
    render_patch_prompt ~project_name:"onton" patch_b gameplan
      ~base_branch:"feat/patch-1"
  in
  String.is_prefix prompt_a ~prefix:g_layer
  && String.is_prefix prompt_b ~prefix:g_layer

let%test "gameplan layer points at the published gameplan artifact copy" =
  let _, _, gameplan = make_layer_test_fixture () in
  let g_layer = render_gameplan_layer ~project_name:"onton" gameplan in
  String.is_substring g_layer ~substring:"## Full Gameplan Reference"
  && String.is_substring g_layer
       ~substring:("`" ^ Project_store.gameplan_artifact_path "onton" ^ "`")
  (* The pointer is lazy-disclosure only: the layer must keep withholding
     sibling patch detail itself (titles only, per the patches list). *)
  && not (String.is_substring g_layer ~substring:"Pass patch + gameplan")

let%test "patch layer lists ancestor implementation notes for dependents" =
  let patch_a, patch_b, gameplan = make_layer_test_fixture () in
  let prompt_a =
    render_patch_prompt ~project_name:"onton" patch_a gameplan
      ~base_branch:"main"
  in
  let prompt_b =
    render_patch_prompt ~project_name:"onton" patch_b gameplan
      ~base_branch:"feat/patch-1"
  in
  let expected_entry =
    Printf.sprintf "- Patch 1: Add layer helpers — `%s`"
      (Project_store.pr_body_artifact_path ~project_name:"onton"
         ~patch_id:patch_a.Patch.id)
  in
  String.is_substring prompt_b ~substring:"## Dependency Implementation Notes"
  && String.is_substring prompt_b ~substring:expected_entry
  (* A patch with no ancestors gets no section at all. *)
  && not
       (String.is_substring prompt_a
          ~substring:"## Dependency Implementation Notes")

let%test "dependency notes cover transitive ancestors, not just direct deps" =
  let patch_a, patch_b, gameplan = make_layer_test_fixture () in
  let patch_c =
    {
      patch_b with
      Patch.id = Patch_id.of_string "3";
      title = "Stack further";
      branch = Branch.of_string "feat/patch-3";
      dependencies = [ Patch_id.of_string "2" ];
    }
  in
  let gameplan =
    { gameplan with Gameplan.patches = [ patch_a; patch_b; patch_c ] }
  in
  let ancestors = ancestor_patches gameplan patch_c in
  let prompt_c =
    render_patch_prompt ~project_name:"onton" patch_c gameplan
      ~base_branch:"feat/patch-2"
  in
  let path_of p =
    Project_store.pr_body_artifact_path ~project_name:"onton"
      ~patch_id:p.Patch.id
  in
  List.equal String.equal
    (List.map ancestors ~f:(fun p -> Patch_id.to_string p.Patch.id))
    [ "1"; "2" ]
  && String.is_substring prompt_c ~substring:(path_of patch_a)
  && String.is_substring prompt_c ~substring:(path_of patch_b)

let%test "pr body prompt tells the author about dependent patch readers" =
  let rendered =
    render_pr_body_prompt ~project_name:"onton" ~pr_number:(Pr_number.of_int 7)
      ~pr_body:"body" ~spec_suffix:"" ~artifact_path:"/tmp/pr-body.md"
  in
  String.is_substring rendered ~substring:"patches that depend on this one"

let%test
    "gameplan + patch layers are the prefix of every layered prompt for one \
     patch" =
  let patch_a, _, gameplan = make_layer_test_fixture () in
  let pr_number = Pr_number.of_int 7 in
  let g_layer = render_gameplan_layer ~project_name:"onton" gameplan in
  let functional_changes = owned_functional_changes gameplan patch_a in
  let p_layer =
    render_patch_layer ~project_name:"onton" patch_a ~pr_number
      ~functional_changes ~base_branch:"main" ()
  in
  let agents_md = "Follow AGENTS.md.\nNever use *_exn." in
  let prefix = g_layer ^ agents_md_section (Some agents_md) ^ p_layer in
  let start_prompt =
    render_patch_prompt ~project_name:"onton" ~agents_md ~pr_number patch_a
      gameplan ~base_branch:"main"
  in
  let ci_prompt =
    render_ci_failure_prompt ~project_name:"onton" ~agents_md ~pr_number
      ~patch:patch_a ~gameplan ~base_branch:"main"
      [
        Ci_check.
          {
            name = "build";
            conclusion = "FAILURE";
            details_url = None;
            description = None;
            started_at = None;
            id = None;
          };
      ]
  in
  let ci_unknown_prompt =
    render_ci_failure_unknown_prompt ~project_name:"onton" ~agents_md ~pr_number
      ~patch:patch_a ~gameplan ~base_branch:"main" ()
  in
  let review_prompt =
    render_review_prompt ~project_name:"onton" ~agents_md ~pr_number
      ~patch:patch_a ~gameplan ~base_branch:"main"
      [
        Comment.
          {
            id = Comment_id.of_int 100;
            thread_id = None;
            body = "Looks good.";
            path = None;
            line = None;
            commit_sha = None;
            original_commit_sha = None;
            outdated = false;
          };
      ]
  in
  let conflict_prompt =
    render_merge_conflict_prompt ~project_name:"onton" ~agents_md ~pr_number
      ~patch:patch_a ~gameplan ~base_branch:"main" ()
  in
  String.is_prefix start_prompt ~prefix
  && String.is_prefix ci_prompt ~prefix
  && String.is_prefix ci_unknown_prompt ~prefix
  && String.is_prefix review_prompt ~prefix
  && String.is_prefix conflict_prompt ~prefix

let%test
    "render_patch_layer surfaces precedents to the implementer when present" =
  let patch, _, _ = make_layer_test_fixture () in
  let patch_with_precedents : Patch.t =
    {
      patch with
      precedents =
        [
          {
            Precedent.kind = "library";
            name = "Bindlib";
            url = Some "https://github.com/rlepigre/ocaml-bindlib";
            why_applicable =
              "Use bind_mvar / unmbind so substitution composes via msubst.";
          };
          {
            Precedent.kind = "algorithm";
            name = "Tarjan 1972 strongly connected components";
            url = None;
            why_applicable = "Detect cycles in the dependency graph in O(V+E).";
          };
        ];
    }
  in
  let rendered =
    render_patch_layer ~project_name:"onton" patch_with_precedents
      ~base_branch:"main" ()
  in
  String.is_substring rendered ~substring:"## Established Precedents"
  && String.is_substring rendered ~substring:"**[library] Bindlib**"
  && String.is_substring rendered
       ~substring:"https://github.com/rlepigre/ocaml-bindlib"
  && String.is_substring rendered
       ~substring:"**[algorithm] Tarjan 1972 strongly connected components**"
  && String.is_substring rendered
       ~substring:"Detect cycles in the dependency graph"

let%test
    "render_patch_layer omits the Established Precedents section when empty" =
  let patch, _, _ = make_layer_test_fixture () in
  let rendered =
    render_patch_layer ~project_name:"onton" patch ~base_branch:"main" ()
  in
  not (String.is_substring rendered ~substring:"Established Precedents")

let%test
    "render_patch_prompt surfaces ONLY the owned functional changes for a patch"
    =
  let patch_a, patch_b, gameplan = make_layer_test_fixture () in
  let gameplan : Gameplan.t =
    {
      gameplan with
      functional_changes =
        [
          {
            Functional_change.id = "FC-1";
            description = "Behavior owned by patch A only";
            owned_by = patch_a.Patch.id;
          };
          {
            Functional_change.id = "FC-2";
            description = "Behavior owned by patch B only";
            owned_by = patch_b.Patch.id;
          };
        ];
    }
  in
  let prompt_a =
    render_patch_prompt ~project_name:"onton" patch_a gameplan
      ~base_branch:"main"
  in
  let prompt_b =
    render_patch_prompt ~project_name:"onton" patch_b gameplan
      ~base_branch:"feat/patch-1"
  in
  String.is_substring prompt_a ~substring:"## Functional Changes You Own"
  && String.is_substring prompt_a ~substring:"**FC-1**"
  && String.is_substring prompt_a ~substring:"Behavior owned by patch A only"
  && (not (String.is_substring prompt_a ~substring:"**FC-2**"))
  && String.is_substring prompt_b ~substring:"**FC-2**"
  && String.is_substring prompt_b ~substring:"Behavior owned by patch B only"
  && not (String.is_substring prompt_b ~substring:"**FC-1**")

let%test
    "render_patch_layer omits Functional Changes section when patch owns none" =
  let _, patch, gameplan = make_layer_test_fixture () in
  let functional_changes = owned_functional_changes gameplan patch in
  let rendered =
    render_patch_layer ~project_name:"onton" patch ~functional_changes
      ~base_branch:"main" ()
  in
  not (String.is_substring rendered ~substring:"Functional Changes You Own")

let%test
    "render_patch_prompt surfaces only required context resources for patch" =
  let patch_a, patch_b, gameplan = make_layer_test_fixture () in
  let patch_a = { patch_a with required_context = [ "ctx-a" ] } in
  let patch_b = { patch_b with required_context = [ "ctx-b" ] } in
  let gameplan : Gameplan.t =
    {
      gameplan with
      patches = [ patch_a; patch_b ];
      context_resources =
        [
          {
            Context_resource.id = "ctx-a";
            kind = "existing-implementation";
            paths = [ "lib/current.ml"; "test/test_current.ml" ];
            why = "This is the behavior Patch A must preserve.";
            consumed_by = [ patch_a.Patch.id ];
          };
          {
            Context_resource.id = "ctx-b";
            kind = "reference-doc";
            paths = [ "docs/reference.md" ];
            why = "This belongs only to Patch B.";
            consumed_by = [ patch_b.Patch.id ];
          };
        ];
    }
  in
  let prompt_a =
    render_patch_prompt ~project_name:"onton" patch_a gameplan
      ~base_branch:"main"
  in
  String.is_substring prompt_a ~substring:"## Required Context Before Editing"
  && String.is_substring prompt_a ~substring:"**ctx-a**"
  && String.is_substring prompt_a ~substring:"lib/current.ml"
  && String.is_substring prompt_a
       ~substring:"This is the behavior Patch A must preserve."
  && String.is_substring prompt_a
       ~substring:
         "Read these authoritative resources before making code changes"
  && String.is_substring prompt_a
       ~substring:"Trust this named context over stale or ambiguous patch prose"
  && String.is_substring prompt_a
       ~substring:"avoid starting a parallel implementation"
  && not (String.is_substring prompt_a ~substring:"**ctx-b**")

let%test
    "render_patch_layer omits Required Context section when no resources apply"
    =
  let patch, _, _ = make_layer_test_fixture () in
  let rendered =
    render_patch_layer ~project_name:"onton" patch ~base_branch:"main" ()
  in
  not
    (String.is_substring rendered ~substring:"Required Context Before Editing")

let%test "render_pr_description surfaces precedents when present" =
  let patch, _, gameplan = make_layer_test_fixture () in
  let patch_with_precedents : Patch.t =
    {
      patch with
      precedents =
        [
          {
            Precedent.kind = "library";
            name = "Bindlib";
            url = Some "https://github.com/rlepigre/ocaml-bindlib";
            why_applicable = "Use bind_mvar / unmbind for binders.";
          };
        ];
    }
  in
  let body =
    render_pr_description ~project_name:"onton" patch_with_precedents gameplan
  in
  String.is_substring body ~substring:"## Established Precedents"
  && String.is_substring body ~substring:"**[library] Bindlib**"
  && String.is_substring body ~substring:"Use bind_mvar / unmbind for binders."

let%test "render_pr_description omits Established Precedents when empty" =
  let patch, _, gameplan = make_layer_test_fixture () in
  let body = render_pr_description ~project_name:"onton" patch gameplan in
  not (String.is_substring body ~substring:"Established Precedents")

let%test "follow-up prompts without patch+gameplan emit only the turn layer" =
  let _, _, _gameplan = make_layer_test_fixture () in
  let ci_prompt =
    render_ci_failure_prompt ~project_name:"onton"
      [
        Ci_check.
          {
            name = "build";
            conclusion = "FAILURE";
            details_url = None;
            description = None;
            started_at = None;
            id = None;
          };
      ]
  in
  (* No layered prefix: the prompt does not start with the project heading. *)
  not (String.is_prefix ci_prompt ~prefix:"# [onton]")

let%test "human and pr_body prompts do not include the gameplan layer" =
  let human_prompt =
    render_human_message_prompt ~project_name:"onton" [ "Please rebase." ]
  in
  let pr_body_prompt =
    render_pr_body_prompt ~project_name:"onton" ~pr_number:(Pr_number.of_int 42)
      ~pr_body:"## Patch 1: Foo\nBody." ~spec_suffix:""
      ~artifact_path:"/tmp/notes.md"
  in
  (* Neither contains the gameplan project heading marker — these
     intentionally stay turn-only. The pr_body prompt may legitimately
     contain "## Patch ..." inside its embedded PR body, so we only
     check the project heading prefix. *)
  (not (String.is_substring human_prompt ~substring:"# [onton]"))
  && not (String.is_prefix pr_body_prompt ~prefix:"# [onton]")

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
          commit_sha = None;
          original_commit_sha = None;
          outdated = false;
        };
      Comment.
        {
          id = Comment_id.of_int 2;
          thread_id = None;
          body = "General feedback.";
          path = None;
          line = None;
          commit_sha = None;
          original_commit_sha = None;
          outdated = false;
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
  (* Back-compat: no SHAs → no preamble, no [at=…], no [outdated]. *)
  && (not (String.is_substring result ~substring:"Review anchored at"))
  && (not (String.is_substring result ~substring:"[at="))
  (* Safe substring check: sha_anchor is empty (no current_head_sha and no
     comment SHAs), so the preamble text — which itself references the
     literal `[outdated]` — is never emitted. If this test later adds SHAs,
     switch to a bullet-line-scoped assertion like the file-level test below. *)
  && not (String.is_substring result ~substring:"[outdated]")

let%expect_test "review prompt includes SHA preamble and per-bullet anchor" =
  let comments : Comment.t list =
    [
      Comment.
        {
          id = Comment_id.of_int 1;
          thread_id = Some "PRRT_thread1";
          body = "Still relevant.";
          path = Some "lib/foo.ml";
          line = Some 10;
          commit_sha = Some "47525fdaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
          original_commit_sha = Some "47525fdaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
          outdated = false;
        };
    ]
  in
  let result =
    render_review_prompt ~project_name:"test"
      ~current_head_sha:"da442c5bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" comments
  in
  Stdlib.print_endline result;
  [%expect
    {|
    # Review Comments

    Review anchored at commit `47525fd`. Current branch HEAD is `da442c5`.
    If a comment is marked `[outdated]` or refers to code that no longer exists at HEAD, reply acknowledging it's addressed in a later commit and skip — do not re-do the change.

    The following review comments need to be addressed on your PR:

    - **Comment** on `lib/foo.ml` (line 10) [comment_id=1] [thread_id=PRRT_thread1] [at=47525fd]: Still relevant.

    For each comment:
    1. Implement the requested change, OR explain why the current approach is correct.
    2. Reply to the comment thread:
       `gh api repos/{owner}/{repo}/pulls/{pr_number}/comments/{comment_id}/replies -f body="your response"`
       (`{owner}/{repo}` is resolved automatically by `gh` when run inside the repo)
    3. Resolve the thread using the thread_id:
       `gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "{thread_id}"}) { thread { isResolved } } }'`

    After addressing all comments, commit your changes. The supervisor will push them for you — do not run `git push`.
    |}]

let%expect_test "review prompt marks outdated comments" =
  let comments : Comment.t list =
    [
      Comment.
        {
          id = Comment_id.of_int 1;
          thread_id = Some "PRRT_outdated";
          body = "This line moved.";
          path = Some "lib/foo.ml";
          line = None;
          (* GitHub returns null line when the comment is outdated.
             [commit_sha] may or may not equal [original_commit_sha] —
             what matters is the authoritative [outdated] flag. *)
          commit_sha = Some "da442c5bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
          original_commit_sha = Some "47525fdaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
          outdated = true;
        };
    ]
  in
  let result =
    render_review_prompt ~project_name:"test"
      ~current_head_sha:"da442c5bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" comments
  in
  Stdlib.print_endline result;
  [%expect
    {|
    # Review Comments

    Review anchored at commit `47525fd`. Current branch HEAD is `da442c5`.
    If a comment is marked `[outdated]` or refers to code that no longer exists at HEAD, reply acknowledging it's addressed in a later commit and skip — do not re-do the change.

    The following review comments need to be addressed on your PR:

    - **Comment** on `lib/foo.ml` [comment_id=1] [thread_id=PRRT_outdated] [at=47525fd] [outdated]: This line moved.

    For each comment:
    1. Implement the requested change, OR explain why the current approach is correct.
    2. Reply to the comment thread:
       `gh api repos/{owner}/{repo}/pulls/{pr_number}/comments/{comment_id}/replies -f body="your response"`
       (`{owner}/{repo}` is resolved automatically by `gh` when run inside the repo)
    3. Resolve the thread using the thread_id:
       `gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "{thread_id}"}) { thread { isResolved } } }'`

    After addressing all comments, commit your changes. The supervisor will push them for you — do not run `git push`.
    |}]

let%expect_test
    "review prompt with both pr_number and sha_anchor has single-blank \
     separators" =
  let comments : Comment.t list =
    [
      Comment.
        {
          id = Comment_id.of_int 1;
          thread_id = Some "PRRT_thread1";
          body = "Still relevant.";
          path = Some "lib/foo.ml";
          line = Some 10;
          commit_sha = Some "47525fdaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
          original_commit_sha = Some "47525fdaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
          outdated = false;
        };
    ]
  in
  let result =
    render_review_prompt ~project_name:"test" ~pr_number:(Pr_number.of_int 42)
      ~current_head_sha:"da442c5bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" comments
  in
  Stdlib.print_endline result;
  (* Exactly one blank line between "# Review Comments" / "PR: #42" /
     the preamble / "The following" — not two. *)
  [%expect
    {|
    # Review Comments

    PR: #42

    Review anchored at commit `47525fd`. Current branch HEAD is `da442c5`.
    If a comment is marked `[outdated]` or refers to code that no longer exists at HEAD, reply acknowledging it's addressed in a later commit and skip — do not re-do the change.

    The following review comments need to be addressed on your PR:

    - **Comment** on `lib/foo.ml` (line 10) [comment_id=1] [thread_id=PRRT_thread1] [at=47525fd]: Still relevant.

    For each comment:
    1. Implement the requested change, OR explain why the current approach is correct.
    2. Reply to the comment thread:
       `gh api repos/{owner}/{repo}/pulls/42/comments/{comment_id}/replies -f body="your response"`
       (`{owner}/{repo}` is resolved automatically by `gh` when run inside the repo)
    3. Resolve the thread using the thread_id:
       `gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "{thread_id}"}) { thread { isResolved } } }'`

    After addressing all comments, commit your changes. The supervisor will push them for you — do not run `git push`.
    |}]

let%test "review prompt does not mark file-level comments as outdated" =
  let comments : Comment.t list =
    [
      Comment.
        {
          id = Comment_id.of_int 1;
          thread_id = Some "PRRT_filelevel";
          body = "File-level feedback.";
          path = Some "lib/foo.ml";
          line = None;
          (* File-level comments have null line by design, and the
             thread's isOutdated is false because the file still exists. *)
          commit_sha = Some "47525fdaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
          original_commit_sha = Some "47525fdaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
          outdated = false;
        };
    ]
  in
  let result =
    render_review_prompt ~project_name:"test"
      ~current_head_sha:"47525fdaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" comments
  in
  (* The preamble references "[outdated]" literally, so we can't just grep the
     whole result. Assert directly on the bullet line that contains the comment
     body — this stays correct even if the bullet format changes. *)
  let lines = String.split_lines result in
  match
    List.find lines ~f:(fun l ->
        String.is_substring l ~substring:"File-level feedback.")
  with
  | None -> false
  | Some line -> not (String.is_substring line ~substring:"[outdated]")
