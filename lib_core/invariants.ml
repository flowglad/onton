open Base
open Types

type violation = { invariant : string; details : string }
[@@deriving show, eq, sexp_of]

let patch_ctx (state : State.t) = state.State.patch_ctx

let check_ci_failure_count_non_negative (state : State.t) =
  State.Patch_ctx.known_patch_ids (patch_ctx state)
  |> List.filter_map ~f:(fun patch_id ->
      let count =
        State.Patch_ctx.ci_failure_count (patch_ctx state) ~patch_id
      in
      if count < 0 then
        Some
          {
            invariant = "ci_failure_count_non_negative";
            details =
              Printf.sprintf "patch %s has ci_failure_count=%d"
                (Patch_id.to_string patch_id)
                count;
          }
      else None)

let check_resolved_not_pending (state : State.t) =
  let comments = state.State.comments in
  let resolved =
    State.Comments.all_resolved comments |> Set.of_list (module Comment)
  in
  State.Comments.all_pending comments
  |> List.filter_map ~f:(fun (comment, patch_id) ->
      if Set.mem resolved comment then
        Some
          {
            invariant = "resolved_not_pending";
            details =
              Printf.sprintf
                "comment %s is both resolved and pending for patch %s"
                (Comment.show comment)
                (Patch_id.to_string patch_id);
          }
      else None)

let check_busy_implies_has_session (state : State.t) =
  State.Patch_ctx.known_patch_ids (patch_ctx state)
  |> List.filter_map ~f:(fun patch_id ->
      let busy = State.Patch_ctx.is_busy (patch_ctx state) ~patch_id in
      let has_session =
        State.Patch_ctx.has_session (patch_ctx state) ~patch_id
      in
      if busy && not has_session then
        Some
          {
            invariant = "busy_implies_has_session";
            details =
              Printf.sprintf "patch %s is busy but has no session"
                (Patch_id.to_string patch_id);
          }
      else None)

let all_checks =
  [
    check_ci_failure_count_non_negative;
    check_busy_implies_has_session;
    check_resolved_not_pending;
  ]

let check_invariants (state : State.t) : violation list =
  List.concat_map all_checks ~f:(fun check -> check state)
