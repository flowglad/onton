open Base
open Types

let render_patch_prompt (patch : Patch.t) (gameplan : Gameplan.t)
    ~(base_branch : string) =
  let project_name = gameplan.Gameplan.project_name in
  let deps =
    match patch.Patch.dependencies with
    | [] -> "None"
    | ids ->
        List.map ids ~f:(fun id -> Patch_id.to_string id)
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
         Printf.sprintf "- Patch %s: %s"
           (Patch_id.to_string p.Patch.id)
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

let%test "patch prompt includes title and deps" =
  let patch : Patch.t =
    Patch.
      {
        id = Patch_id.of_string "5";
        title = "Prompt renderer";
        branch = Branch.of_string "onton-port/patch-5";
        dependencies = [ Patch_id.of_string "1" ];
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
                id = Patch_id.of_string "1";
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
  in
  String.is_substring result ~substring:"# [onton-port] Prompt renderer"
  && String.is_substring result ~substring:"Patches 1"
  && String.is_substring result ~substring:"Patch 1: Core types"
  && String.is_substring result ~substring:"Patch 5: Prompt renderer"

let%test "review prompt formats comments" =
  let comments : Comment.t list =
    [
      Comment.
        {
          id = Comment_id.of_int 1;
          body = "Fix this function.";
          path = Some "lib/foo.ml";
          line = Some 42;
        };
      Comment.
        {
          id = Comment_id.of_int 2;
          body = "General feedback.";
          path = None;
          line = None;
        };
    ]
  in
  let result = render_review_prompt comments in
  String.is_substring result ~substring:"# Review Comments"
  && String.is_substring result ~substring:"### lib/foo.ml:42"
  && String.is_substring result ~substring:"Fix this function."
  && String.is_substring result ~substring:"### (general)"
  && String.is_substring result ~substring:"General feedback."
