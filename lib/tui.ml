open Base
open Types

type display_status =
  | Merged
  | Needs_help
  | Approved
  | Rebasing
  | Running
  | Awaiting_review
  | Pending
[@@deriving show, eq, sexp_of, compare]

let label = function
  | Merged -> "merged"
  | Needs_help -> "needs-help"
  | Approved -> "approved"
  | Rebasing -> "rebasing"
  | Running -> "running"
  | Awaiting_review -> "awaiting-review"
  | Pending -> "pending"

let is_feedback (kind : Operation_kind.t) =
  match kind with
  | Operation_kind.Human | Operation_kind.Merge_conflict | Operation_kind.Ci
  | Operation_kind.Review_comments ->
      true
  | Operation_kind.Rebase -> false

let is_feedback_busy (ctx : State.Patch_ctx.t) ~patch_id ~kind =
  State.Patch_ctx.is_busy ctx ~patch_id && is_feedback kind

let approved ctx ~patch_id = State.Patch_ctx.is_approved ctx ~patch_id

let derive_display_status (ctx : State.Patch_ctx.t) ~patch_id
    ~(current_op : Operation_kind.t option) =
  if State.Patch_ctx.is_merged ctx ~patch_id then Merged
  else if State.Patch_ctx.needs_intervention ctx ~patch_id then Needs_help
  else if approved ctx ~patch_id then Approved
  else if State.Patch_ctx.is_busy ctx ~patch_id then
    match current_op with
    | Some kind when is_feedback kind -> Running
    | _ -> Rebasing
  else if State.Patch_ctx.has_pr ctx ~patch_id then Awaiting_review
  else Pending
