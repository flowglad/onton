(* @archlint.module core
   @archlint.domain adhoc-branch *)

open Base

type add_target =
  | Pull_request of Types.Pr_number.t
  | Remote_branch of Types.Branch.t
[@@deriving show, eq]

type operation = Add of add_target | Remove_pr of Types.Pr_number.t
[@@deriving show, eq]

let operation_supported ~supports_pull_request_changes ~supports_branch_changes
    = function
  | Add (Pull_request _) -> supports_pull_request_changes
  | Add (Remote_branch _) -> supports_branch_changes
  | Remove_pr _ -> true

let forbidden_ref_char c =
  Char.to_int c <= 0x20
  || Char.to_int c = 0x7f
  || List.mem [ '~'; '^'; ':'; '?'; '*'; '['; '\\' ] c ~equal:Char.equal

let validate_remote_branch raw =
  let branch = String.strip raw in
  let components = String.split branch ~on:'/' in
  let invalid_reason =
    if String.is_empty branch then Some "branch name is empty"
    else if not (String.equal branch raw) then
      Some "branch name must not have surrounding whitespace"
    else if String.equal branch "@" || String.equal branch "HEAD" then
      Some "branch name must not be @ or HEAD"
    else if String.is_prefix branch ~prefix:"-" then
      Some "branch name must not begin with '-'"
    else if String.is_prefix branch ~prefix:"refs/" then
      Some "use the short branch name, without refs/"
    else if String.is_prefix branch ~prefix:"/" then
      Some "branch name must not begin with '/'"
    else if String.is_suffix branch ~suffix:"/" then
      Some "branch name must not end with '/'"
    else if String.is_suffix branch ~suffix:"." then
      Some "branch name must not end with '.'"
    else if String.is_substring branch ~substring:"//" then
      Some "branch name must not contain consecutive '/' characters"
    else if String.is_substring branch ~substring:".." then
      Some "branch name must not contain '..'"
    else if String.is_substring branch ~substring:"@{" then
      Some "branch name must not contain '@{'"
    else if String.exists branch ~f:forbidden_ref_char then
      Some "branch name contains a character forbidden by git"
    else if
      List.exists components ~f:(fun component ->
          String.is_empty component
          || String.is_prefix component ~prefix:"."
          || String.is_suffix component ~suffix:".lock")
    then Some "branch path components must not begin with '.' or end with .lock"
    else None
  in
  match invalid_reason with
  | None -> Ok (Types.Branch.of_string branch)
  | Some reason ->
      Error (Printf.sprintf "invalid remote branch %S: %s" raw reason)

let positive_pr_number raw =
  match Int.of_string_opt raw with
  | Some n when n > 0 -> Ok (Types.Pr_number.of_int n)
  | Some _ | None -> Error (Printf.sprintf "invalid PR number %S" raw)

let parse_add_value raw =
  let raw = String.strip raw in
  match String.chop_prefix raw ~prefix:"branch:" with
  | Some branch ->
      Result.map (validate_remote_branch branch) ~f:(fun branch ->
          Remote_branch branch)
  | None ->
      if (not (String.is_empty raw)) && String.for_all raw ~f:Char.is_digit then
        Result.map (positive_pr_number raw) ~f:(fun pr -> Pull_request pr)
      else
        Result.map (validate_remote_branch raw) ~f:(fun branch ->
            Remote_branch branch)

let looks_like_operation value =
  let length = String.length value in
  length >= 2
  &&
  match value.[0] with
  | '+' -> true
  | '-' -> String.drop_prefix value 1 |> String.for_all ~f:Char.is_digit
  | _ -> false

let parse_operation value =
  if not (looks_like_operation value) then
    Error
      (Printf.sprintf
         "unrecognized ad-hoc operation %S; expected +PR, +BRANCH, or -PR" value)
  else
    let payload = String.drop_prefix value 1 in
    if Char.equal value.[0] '+' then
      Result.map (parse_add_value payload) ~f:(fun target -> Add target)
    else Result.map (positive_pr_number payload) ~f:(fun pr -> Remove_pr pr)

let branch_patch_id ~change_id =
  Types.Patch_id.of_string
    ("branch-" ^ Int.to_string (Types.Pr_number.to_int change_id))
