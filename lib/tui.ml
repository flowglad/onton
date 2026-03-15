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
  | Approved_running -> "approved-running"
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

(** Derive the display status for a patch from its current state context.

    [current_op] should be [Some op] when the patch is busy — it determines
    which specific "running" status to show. When [is_busy] is true but
    [current_op] is [None] (e.g. during startup before the first operation is
    tracked), we fall back to [Starting] or [Rebasing] based on whether a PR
    exists. *)
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

let%test "merged takes priority over everything" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_merged ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_needs_intervention ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal_display_status Merged
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "needs_help over approved" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_needs_intervention ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal_display_status Needs_help
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "approved idle" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal_display_status Approved_idle
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "approved running" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Approved_running
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Rebase))

let%test "approved running ignores current_op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Approved_running
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "busy with ci op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Fixing_ci
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Ci))

let%test "busy with review op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Addressing_review
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Review_comments))

let%test "busy with merge conflict op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Resolving_conflict
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Merge_conflict))

let%test "busy with human op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Responding_to_human
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Human))

let%test "busy with rebase op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Rebasing
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Rebase))

let%test "busy no op with pr falls back to rebasing" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Rebasing
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "busy no op without pr falls back to starting" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Starting
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "ci queued" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_queued ~patch_id:(Patch_id.of_string "1") ~kind:Ci
         ~value:true
  in
  equal_display_status Ci_queued
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "review queued" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_queued ~patch_id:(Patch_id.of_string "1")
         ~kind:Review_comments ~value:true
  in
  equal_display_status Review_queued
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "awaiting ci when failure count > 0" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_ci_failure_count ~patch_id:(Patch_id.of_string "1")
         ~count:2
  in
  equal_display_status Awaiting_ci
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "awaiting review default" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Awaiting_review
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None)

let%test "pending is default" =
  equal_display_status Pending
    (derive_display_status State.Patch_ctx.empty
       ~patch_id:(Patch_id.of_string "1") ~current_op:None)
