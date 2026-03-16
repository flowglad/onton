open Base
open Types

(* Prompt override system: loads template files from the project data
   directory's prompts/ subdirectory and performs single-pass variable
   substitution. Falls back to built-in defaults when no override exists. *)

let prompts_dir (project_name : string) : string =
  Stdlib.Filename.concat (Project_store.project_dir project_name) "prompts"

let substitute_variables (template : string) (vars : (string * string) list) :
    string =
  let var_map = Map.of_alist_exn (module String) vars in
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
  | fd ->
      let ic =
        match Unix.in_channel_of_descr fd with
        | exception exn ->
            Unix.close fd;
            raise exn
        | ic -> ic
      in
      Exn.protect
        ~finally:(fun () -> Stdlib.In_channel.close ic)
        ~f:(fun () ->
          match Stdlib.In_channel.input_all ic with
          | content -> Some content
          | exception (Sys_error msg as exn) ->
              if String.is_substring msg ~substring:"Is a directory" then None
              else raise exn)

let render_with_override ~(project_name : string) ~(name : string)
    ~(vars : (string * string) list) ~(default : unit -> string) : string =
  match load_override ~project_name name with
  | Some template -> substitute_variables template vars
  | None -> default ()

let render_patch_prompt ~(project_name : string) (patch : Patch.t)
    (gameplan : Gameplan.t) ~(base_branch : string) =
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
      ("patch_id", Patch_id.to_string patch.Patch.id);
      ("patches_list", patches_list);
    ]
  in
  render_with_override ~project_name ~name:"patch" ~vars ~default:(fun () ->
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

let render_review_prompt ~(project_name : string) (comments : Comment.t list) =
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
      let vars =
        [
          ("project_name", project_name);
          ("comments", formatted);
          ("count", Int.to_string (List.length comments));
        ]
      in
      render_with_override ~project_name ~name:"review" ~vars
        ~default:(fun () ->
          Printf.sprintf
            "# Review Comments\n\n\
             Please address the following review comments:\n\n\
             %s"
            formatted)

let render_ci_failure_prompt ~(project_name : string) (checks : Ci_check.t list)
    =
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
      let vars =
        [
          ("project_name", project_name);
          ("checks", formatted);
          ("count", Int.to_string (List.length checks));
        ]
      in
      render_with_override ~project_name ~name:"ci_failure" ~vars
        ~default:(fun () ->
          Printf.sprintf
            "# CI Failures\n\nThe following CI checks failed:\n\n%s" formatted)

let render_ci_failure_unknown_prompt ~(project_name : string) =
  let vars = [ ("project_name", project_name) ] in
  render_with_override ~project_name ~name:"ci_failure_unknown" ~vars
    ~default:(fun () ->
      "# CI Failures\n\n\
       One or more CI checks failed. Please investigate the failures and fix \
       them.\n\n\
       Run the CI checks locally or check the PR status for details.")

let render_merge_conflict_prompt ~(project_name : string)
    ~(base_branch : string) =
  let vars = [ ("project_name", project_name); ("base_branch", base_branch) ] in
  render_with_override ~project_name ~name:"merge_conflict" ~vars
    ~default:(fun () ->
      Printf.sprintf
        {|# Merge Conflict

Your branch has conflicts with `%s`. Please rebase and resolve conflicts:

```
git fetch origin
git rebase origin/%s
```

Resolve any conflicts, then continue with `git rebase --continue`.|}
        base_branch base_branch)

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
    render_patch_prompt ~project_name:"onton-port" patch gameplan
      ~base_branch:"onton-port/patch-1"
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
          body = "Fix this function.";
          path = Some "lib/foo.ml";
          line = Some 42;
        };
      Comment.{ body = "General feedback."; path = None; line = None };
    ]
  in
  let result = render_review_prompt ~project_name:"test" comments in
  String.is_substring result ~substring:"# Review Comments"
  && String.is_substring result ~substring:"### lib/foo.ml:42"
  && String.is_substring result ~substring:"Fix this function."
  && String.is_substring result ~substring:"### (general)"
  && String.is_substring result ~substring:"General feedback."
