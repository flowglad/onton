open Base
open Types

(** Pure derivation of a patch's user-facing display status from its
    [State.Patch_ctx] entry plus the currently-running operation kind and the
    project's main branch.

    Used by the TUI to render the status column and by [Activity_log] to record
    transitions. The full effect of "rendering" stays in [Tui]; this module just
    decides which status applies. *)

type t =
  | Merged
  | Needs_help
  | Approved_idle
  | Approved_running
  | Fixing_ci
  | Addressing_review
  | Addressing_findings
  | Resolving_conflict
  | Responding_to_human
  | Writing_pr_body
  | Rebasing
  | Starting
  | Updating
  | Ci_queued
  | Review_queued
  | Findings_queued
  | Awaiting_feedback
  | Blocked_by_dep
  | Pending
[@@deriving show, eq, sexp_of, compare, yojson]

(** Migration-aware deserializer: the removed [Awaiting_ci]/[Awaiting_review]
    constructors were collapsed into [Awaiting_feedback], so map them to it when
    loading activity-log transitions persisted by older versions. Shadows the
    derived [t_of_yojson] so every consumer — including the derived
    [Activity_log.Transition_entry] decoder — picks up the migration. *)
let derived_t_of_yojson = t_of_yojson

let t_of_yojson json =
  match json with
  | `List [ `String ("Awaiting_ci" | "Awaiting_review") ]
  | `String ("Awaiting_ci" | "Awaiting_review") ->
      Awaiting_feedback
  | json -> derived_t_of_yojson json

(** A patch is "on main" if its tracked base branch equals the main branch, or
    if no base is tracked (the default before any rebase has happened). When
    [current_op] is [None] (e.g. during startup before the first operation is
    tracked), we fall back to [Starting] or [Rebasing] based on whether a PR
    exists. *)
let is_on_main ctx ~patch_id ~main_branch =
  match State.Patch_ctx.base_branch ctx ~patch_id with
  | Some b -> Branch.equal b main_branch
  | None -> true

let derive (ctx : State.Patch_ctx.t) ~patch_id
    ~(current_op : Operation_kind.t option) ~(main_branch : Branch.t) =
  if State.Patch_ctx.is_merged ctx ~patch_id then Merged
  else if State.Patch_ctx.needs_intervention ctx ~patch_id then Needs_help
  else if State.Patch_ctx.is_approved ctx ~patch_id then
    if State.Patch_ctx.is_busy ctx ~patch_id then Approved_running
    else Approved_idle
  else if State.Patch_ctx.is_busy ctx ~patch_id then
    match current_op with
    | Some Ci -> Fixing_ci
    | Some Review_comments -> Addressing_review
    | Some Findings -> Addressing_findings
    | Some Merge_conflict -> Resolving_conflict
    | Some Human -> Responding_to_human
    | Some Pr_body -> Writing_pr_body
    | Some Rebase -> Rebasing
    | None ->
        if State.Patch_ctx.has_pr ctx ~patch_id then Updating else Starting
  else if State.Patch_ctx.has_pr ctx ~patch_id then
    if not (is_on_main ctx ~patch_id ~main_branch) then Blocked_by_dep
    else if State.Patch_ctx.is_queued ctx ~patch_id ~kind:Ci then Ci_queued
    else if State.Patch_ctx.is_queued ctx ~patch_id ~kind:Review_comments then
      Review_queued
    else if State.Patch_ctx.is_queued ctx ~patch_id ~kind:Findings then
      Findings_queued
    else Awaiting_feedback
  else Pending

let%test "merged takes priority over everything" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_merged ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_needs_intervention ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal Merged
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "needs_help over approved" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_needs_intervention ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal Needs_help
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "approved idle" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal Approved_idle
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "approved running" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Approved_running
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:(Some Rebase)
       ~main_branch:(Branch.of_string "main"))

let%test "approved running ignores current_op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Approved_running
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "busy with ci op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Fixing_ci
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:(Some Ci)
       ~main_branch:(Branch.of_string "main"))

let%test "busy with review op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Addressing_review
    (derive ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Review_comments) ~main_branch:(Branch.of_string "main"))

let%test "busy with merge conflict op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Resolving_conflict
    (derive ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Merge_conflict) ~main_branch:(Branch.of_string "main"))

let%test "busy with human op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Responding_to_human
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:(Some Human)
       ~main_branch:(Branch.of_string "main"))

let%test "busy with rebase op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Rebasing
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:(Some Rebase)
       ~main_branch:(Branch.of_string "main"))

let%test "busy no op with pr falls back to updating" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Updating
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "busy no op without pr falls back to starting" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Starting
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "ci queued" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_queued ~patch_id:(Patch_id.of_string "1") ~kind:Ci
         ~value:true
  in
  equal Ci_queued
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "review queued" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_queued ~patch_id:(Patch_id.of_string "1")
         ~kind:Review_comments ~value:true
  in
  equal Review_queued
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "awaiting feedback with prior ci failures" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_ci_failure_count ~patch_id:(Patch_id.of_string "1")
         ~count:2
  in
  equal Awaiting_feedback
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "awaiting feedback default" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Awaiting_feedback
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "blocked by dep when base_branch is not main" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_base_branch ~patch_id:(Patch_id.of_string "1")
         ~branch:(Branch.of_string "feature/dep")
  in
  equal Blocked_by_dep
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "pending is default" =
  equal Pending
    (derive State.Patch_ctx.empty ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

(* Backward-compat: snapshots persisted before the collapse stored the removed
   [Awaiting_ci]/[Awaiting_review] constructors in their activity-log
   transitions. The compat [t_of_yojson] must map both legacy forms (canonical
   [`List [`String _]] and bare [`String _]) to [Awaiting_feedback] so those
   snapshots still load. *)
let%test "legacy Awaiting_ci decodes to Awaiting_feedback" =
  equal Awaiting_feedback (t_of_yojson (`List [ `String "Awaiting_ci" ]))
  && equal Awaiting_feedback (t_of_yojson (`String "Awaiting_ci"))

let%test "legacy Awaiting_review decodes to Awaiting_feedback" =
  equal Awaiting_feedback (t_of_yojson (`List [ `String "Awaiting_review" ]))
  && equal Awaiting_feedback (t_of_yojson (`String "Awaiting_review"))

let%test "current statuses round-trip through yojson" =
  List.for_all [ Awaiting_feedback; Ci_queued; Merged; Pending; Needs_help ]
    ~f:(fun s -> equal s (t_of_yojson (yojson_of_t s)))
