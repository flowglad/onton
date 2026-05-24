open Base

type rejection =
  | Workflow_scope_missing
  | Branch_protection
  | Push_pattern_block
  | Lease_violation
  | Hook_failure of string
  | Unknown of string
  | Local_state_unsafe of { reason : string }
[@@deriving show, eq, sexp_of, compare]

(* GitHub's remote-rejected stderr is a mix of [remote: ...] lines and a
   trailing [ ! [remote rejected] ...] line. Recognizers below match against the
   whole stderr blob (case-insensitive on the substring) rather than parsing
   line-by-line — the fingerprints are stable across years of GitHub history
   and false positives would require the message to contain literally these
   phrases. *)

let contains_ci hay needle =
  let h = String.lowercase hay in
  let n = String.lowercase needle in
  String.is_substring h ~substring:n

let truncate_200 s =
  if String.length s <= 200 then s else String.sub s ~pos:0 ~len:200

(** Pick the first non-empty [remote: ...] line as the human-readable excerpt
    for hook/unknown failures. Falls back to the first non-empty line of the
    stderr, or the empty string. *)
let pick_remote_line stderr =
  let lines = String.split_lines stderr in
  let strip_remote_prefix line =
    match String.chop_prefix line ~prefix:"remote:" with
    | Some s -> String.strip s
    | None -> String.strip line
  in
  let remote_lines =
    List.filter_map lines ~f:(fun line ->
        let line = String.strip line in
        if
          String.is_prefix line ~prefix:"remote:"
          && not (String.is_empty (strip_remote_prefix line))
        then Some (strip_remote_prefix line)
        else None)
  in
  match remote_lines with
  | first :: _ -> first
  | [] -> (
      (* No remote: lines — fall back to the first non-empty line of stderr. *)
      match
        List.find lines ~f:(fun l -> not (String.is_empty (String.strip l)))
      with
      | Some l -> String.strip l
      | None -> "")

let classify ~stderr ~stdout:_ =
  (* Recognizer order matters: workflow scope is the most specific message.
     GH013 workflow-scope failures include "Repository rule violations found",
     so [Workflow_scope_missing] must stay before [Push_pattern_block]. It can
     also co-occur with the generic "remote rejected" trailer, so it must be
     checked before the catch-all hook-failure branch. *)
  if
    contains_ci stderr
      "refusing to allow an OAuth App to create or update workflow"
    || contains_ci stderr
         "refusing to allow a Personal Access Token to create or update \
          workflow"
    || contains_ci stderr "without `workflow` scope"
  then Workflow_scope_missing
  else if
    contains_ci stderr "GH006: Protected branch update failed"
    || contains_ci stderr "protected branch hook declined"
    || contains_ci stderr "Cannot force-push to a protected branch"
  then Branch_protection
  else if
    contains_ci stderr "push declined due to repository rule violations"
    || contains_ci stderr "Repository rule violations found"
  then Push_pattern_block
  else if
    contains_ci stderr "stale info"
    || contains_ci stderr "fetch first"
    || contains_ci stderr "non-fast-forward"
  then Lease_violation
  else
    let excerpt = truncate_200 (pick_remote_line stderr) in
    if String.is_empty excerpt then Unknown (truncate_200 (String.strip stderr))
    else if
      (* If we extracted a remote: line, treat as a hook failure (server-side
         policy spoke up but didn't match a named recognizer). *)
      contains_ci stderr "remote:"
    then Hook_failure excerpt
    else Unknown excerpt

let short_label = function
  | Workflow_scope_missing -> "workflow_scope_missing"
  | Branch_protection -> "branch_protection"
  | Push_pattern_block -> "push_pattern_block"
  | Lease_violation -> "lease_violation"
  | Hook_failure _ -> "hook_failure"
  | Unknown _ -> "unknown_rejection"
  | Local_state_unsafe _ -> "local_state_unsafe"

let detail_excerpt = function
  | Workflow_scope_missing | Branch_protection | Push_pattern_block
  | Lease_violation ->
      None
  | Hook_failure s | Unknown s ->
      if String.is_empty (String.strip s) then None else Some s
  | Local_state_unsafe { reason } ->
      if String.is_empty (String.strip reason) then None else Some reason

let is_permanent = function
  | Workflow_scope_missing | Branch_protection | Push_pattern_block
  | Hook_failure _ | Local_state_unsafe _ ->
      true
  | Lease_violation | Unknown _ -> false
