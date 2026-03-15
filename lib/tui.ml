open Base
open Types

type display_status =
  | Merged
  | Needs_help
  | Approved_idle
  | Approved_running
  | Fixing_ci
  | Addressing_review
  | Resolving_conflict
  | Responding_to_human
  | Rebasing
  | Starting
  | Ci_queued
  | Review_queued
  | Awaiting_ci
  | Awaiting_review
  | Pending
[@@deriving show, eq, sexp_of, compare]

type status_display = { label : string; color : string }
[@@deriving show, eq, sexp_of, compare]

let label = function
  | Merged -> "merged"
  | Needs_help -> "needs-help"
  | Approved_idle -> "approved"
  | Approved_running -> "approved (running)"
  | Fixing_ci -> "fixing-ci"
  | Addressing_review -> "addressing-review"
  | Resolving_conflict -> "resolving-conflict"
  | Responding_to_human -> "responding-to-human"
  | Rebasing -> "rebasing"
  | Starting -> "starting"
  | Ci_queued -> "ci-queued"
  | Review_queued -> "review-queued"
  | Awaiting_ci -> "awaiting-ci"
  | Awaiting_review -> "awaiting-review"
  | Pending -> "pending"

let color = function
  | Merged -> Term.Sgr.fg_green
  | Needs_help -> Term.Sgr.fg_red
  | Approved_idle -> Term.Sgr.fg_green
  | Approved_running -> Term.Sgr.fg_cyan
  | Fixing_ci -> Term.Sgr.fg_yellow
  | Addressing_review -> Term.Sgr.fg_yellow
  | Resolving_conflict -> Term.Sgr.fg_yellow
  | Responding_to_human -> Term.Sgr.fg_magenta
  | Rebasing -> Term.Sgr.fg_cyan
  | Starting -> Term.Sgr.fg_cyan
  | Ci_queued -> Term.Sgr.fg_yellow
  | Review_queued -> Term.Sgr.fg_yellow
  | Awaiting_ci -> Term.Sgr.fg_blue
  | Awaiting_review -> Term.Sgr.fg_blue
  | Pending -> Term.Sgr.fg_white

let to_status_display status = { label = label status; color = color status }

let styled_label status =
  let c = color status in
  Term.styled [ c ] (label status)

let derive_display_status (ctx : State.Patch_ctx.t) ~patch_id
    ~(current_op : Operation_kind.t option) =
  if State.Patch_ctx.is_merged ctx ~patch_id then Merged
  else if State.Patch_ctx.needs_intervention ctx ~patch_id then Needs_help
  else if State.Patch_ctx.is_approved ctx ~patch_id then
    if State.Patch_ctx.is_busy ctx ~patch_id then Approved_running
    else Approved_idle
  else if State.Patch_ctx.is_busy ctx ~patch_id then
    match current_op with
    | Some Ci -> Fixing_ci
    | Some Review_comments -> Addressing_review
    | Some Merge_conflict -> Resolving_conflict
    | Some Human -> Responding_to_human
    | Some Rebase -> Rebasing
    | None ->
        if State.Patch_ctx.has_pr ctx ~patch_id then Rebasing else Starting
  else if State.Patch_ctx.has_pr ctx ~patch_id then
    if State.Patch_ctx.is_queued ctx ~patch_id ~kind:Ci then Ci_queued
    else if State.Patch_ctx.is_queued ctx ~patch_id ~kind:Review_comments then
      Review_queued
    else if State.Patch_ctx.ci_failure_count ctx ~patch_id > 0 then Awaiting_ci
    else Awaiting_review
  else Pending
