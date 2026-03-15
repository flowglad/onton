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

type status_display [@@deriving show, eq, sexp_of, compare]

val label : display_status -> string
val color : display_status -> string
val to_status_display : display_status -> status_display
val styled_label : display_status -> string

val derive_display_status :
  State.Patch_ctx.t ->
  patch_id:Patch_id.t ->
  current_op:Operation_kind.t option ->
  display_status
