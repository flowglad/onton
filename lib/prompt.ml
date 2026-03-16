open Base
open Types

(* Prompt override system: loads template files from .onton/prompts/ and
   substitutes \{\{variable\}\} placeholders. Falls back to built-in defaults
   when no override file exists. *)

let prompts_dir = ".onton/prompts"

let substitute_variables (template : string) (vars : (string * string) list) :
    string =
  List.fold vars ~init:template ~f:(fun acc (key, value) ->
      let pattern = "{{" ^ key ^ "}}" in
      String.substr_replace_all acc ~pattern ~with_:value)

let load_override (name : string) : string option =
  let path = prompts_dir ^ "/" ^ name ^ ".md" in
  match Stdlib.In_channel.with_open_text path Stdlib.In_channel.input_all with
  | contents -> Some contents
  | exception Stdlib.Sys_error _ -> None

let render_with_override ~(name : string) ~(vars : (string * string) list)
    ~(default : unit -> string) : string =
  match load_override name with
  | Some template -> substitute_variables template vars
  | None -> default ()

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
  let branch = Branch.to_string patch.Patch.branch in
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
      ("title", patch.Patch.title);
      ("problem_statement", gameplan.Gameplan.problem_statement);
      ("solution_summary", gameplan.Gameplan.solution_summary);
      ("dependencies", deps);
      ("branch", branch);
      ("base_branch", base_branch);
      ("patches_list", patches_list);
    ]
  in
  render_with_override ~name:"patch" ~vars ~default:(fun () ->
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
        gameplan.Gameplan.solution_summary deps branch base_branch patches_list)

let render_review_prompt (comments : Comment.t list) =
  let formatted =
    match comments with
    | [] -> ""
    | _ ->
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
  let vars =
    [ ("comments", formatted); ("count", Int.to_string (List.length comments)) ]
  in
  render_with_override ~name:"review" ~vars ~default:(fun () ->
      match comments with
      | [] -> "No review comments to address."
      | _ ->
          Printf.sprintf
            "# Review Comments\n\n\
             Please address the following review comments:\n\n\
             %s"
            formatted)

let render_ci_failure_prompt (checks : Ci_check.t list) =
  let formatted =
    match checks with
    | [] -> ""
    | _ ->
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
  let vars =
    [ ("checks", formatted); ("count", Int.to_string (List.length checks)) ]
  in
  render_with_override ~name:"ci_failure" ~vars ~default:(fun () ->
      match checks with
      | [] -> "No CI failures."
      | _ ->
          Printf.sprintf
            "# CI Failures\n\nThe following CI checks failed:\n\n%s" formatted)

let render_merge_conflict_prompt ~(base_branch : string) =
  let vars = [ ("base_branch", base_branch) ] in
  render_with_override ~name:"merge_conflict" ~vars ~default:(fun () ->
      Printf.sprintf
        {|# Merge Conflict

Your branch has conflicts with `%s`. Please rebase and resolve conflicts:

```
git fetch origin
git rebase origin/%s
```

Resolve any conflicts, then continue with `git rebase --continue`.|}
        base_branch base_branch)

let render_human_message_prompt (messages : string list) =
  let formatted =
    match messages with
    | [] -> ""
    | [ msg ] -> msg
    | _ ->
        List.mapi messages ~f:(fun i msg -> Printf.sprintf "%d. %s" (i + 1) msg)
        |> String.concat ~sep:"\n"
  in
  let vars =
    [ ("messages", formatted); ("count", Int.to_string (List.length messages)) ]
  in
  render_with_override ~name:"human_message" ~vars ~default:(fun () ->
      match messages with
      | [] -> "No messages."
      | [ msg ] -> Printf.sprintf "# Message from Human\n\n%s" msg
      | _ -> Printf.sprintf "# Messages from Human\n\n%s" formatted)

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

let%test "load_override returns None for missing file" =
  Option.is_none (load_override "nonexistent_prompt_name")

let%test "review prompt formats comments" =
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
  String.is_substring result ~substring:"# Review Comments"
  && String.is_substring result ~substring:"### lib/foo.ml:42"
  && String.is_substring result ~substring:"Fix this function."
  && String.is_substring result ~substring:"### (general)"
  && String.is_substring result ~substring:"General feedback."
