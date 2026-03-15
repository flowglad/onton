open Base
open Types

let render_patch_prompt (patch : Patch.t) (gameplan : Gameplan.t)
    ~(base_branch : string) ~(project_name : string) =
  let deps =
    match patch.Patch.dependencies with
    | [] -> "None"
    | ids ->
        List.map ids ~f:(fun id -> Int.to_string (Patch_id.to_int id))
        |> String.concat ~sep:", "
        |> Printf.sprintf "Patches %s"
  in
  Printf.sprintf
    {|# [%s] %s

## Problem Statement
%s

## Solution Summary
%s

## Dependencies
%s

## Git Instructions
- Branch: %s
- Base branch: %s

## Patches in Gameplan
%s|}
    project_name patch.Patch.title gameplan.Gameplan.problem_statement
    gameplan.Gameplan.solution_summary deps
    (Branch.to_string patch.Patch.branch)
    base_branch
    (List.map gameplan.Gameplan.patches ~f:(fun (p : Patch.t) ->
         Printf.sprintf "- Patch %d: %s"
           (Patch_id.to_int p.Patch.id)
           p.Patch.title)
    |> String.concat ~sep:"\n")

let render_review_prompt (comments : Comment.t list) =
  match comments with
  | [] -> "No review comments to address."
  | _ ->
      let formatted =
        List.map comments ~f:(fun (c : Comment.t) ->
            let location =
              match (c.Comment.path, c.Comment.line) with
              | Some path, Some line -> Printf.sprintf "%s:%d" path line
              | Some path, None -> path
              | None, _ -> "(general)"
            in
            Printf.sprintf "### %s\n%s" location c.Comment.body)
        |> String.concat ~sep:"\n\n"
      in
      Printf.sprintf
        "# Review Comments\n\n\
         Please address the following review comments:\n\n\
         %s"
        formatted

let render_ci_failure_prompt (checks : Ci_check.t list) =
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
      Printf.sprintf "# CI Failures\n\nThe following CI checks failed:\n\n%s"
        formatted

let render_merge_conflict_prompt ~(base_branch : string) =
  Printf.sprintf
    {|# Merge Conflict

Your branch has conflicts with `%s`. Please rebase and resolve conflicts:

```
git fetch origin
git rebase origin/%s
```

Resolve any conflicts, then continue with `git rebase --continue`.|}
    base_branch base_branch

let render_human_message_prompt (messages : string list) =
  match messages with
  | [] -> "No messages."
  | [ msg ] -> Printf.sprintf "# Message from Human\n\n%s" msg
  | _ ->
      let formatted =
        List.mapi messages ~f:(fun i msg -> Printf.sprintf "%d. %s" (i + 1) msg)
        |> String.concat ~sep:"\n"
      in
      Printf.sprintf "# Messages from Human\n\n%s" formatted

[@@@warning "-40-42"]

let%expect_test "patch prompt includes title and deps" =
  let patch : Patch.t =
    Patch.
      {
        id = Patch_id.of_int 5;
        title = "Prompt renderer";
        branch = Branch.of_string "onton-port/patch-5";
        dependencies = [ Patch_id.of_int 1 ];
      }
  in
  let gameplan : Gameplan.t =
    Gameplan.
      {
        project_name = "onton-port";
        problem_statement = "Port Anton to OCaml.";
        solution_summary = "Use Eio for concurrency.";
        patches =
          [
            Patch.
              {
                id = Patch_id.of_int 1;
                title = "Core types";
                branch = Branch.of_string "onton-port/patch-1";
                dependencies = [];
              };
            patch;
          ];
      }
  in
  let result =
    render_patch_prompt patch gameplan ~base_branch:"onton-port/patch-1"
      ~project_name:"onton-port"
  in
  Stdio.print_string result;
  [%expect
    {|
    # [onton-port] Prompt renderer

    ## Problem Statement
    Port Anton to OCaml.

    ## Solution Summary
    Use Eio for concurrency.

    ## Dependencies
    Patches 1

    ## Git Instructions
    - Branch: onton-port/patch-5
    - Base branch: onton-port/patch-1

    ## Patches in Gameplan
    - Patch 1: Core types
    - Patch 5: Prompt renderer |}]

let%expect_test "review prompt formats comments" =
  let comments : Comment.t list =
    [
      Comment.
        {
          body = "Fix this function.";
          path = Some "lib/foo.ml";
          line = Some 42;
        };
      Comment.{ body = "General feedback."; path = None; line = None };
    ]
  in
  let result = render_review_prompt comments in
  Stdio.print_string result;
  [%expect
    {|
    # Review Comments

    Please address the following review comments:

    ### lib/foo.ml:42
    Fix this function.

    ### (general)
    General feedback. |}]
