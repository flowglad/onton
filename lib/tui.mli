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
  | Updating
  | Ci_queued
  | Review_queued
  | Awaiting_ci
  | Awaiting_review
  | Pending
[@@deriving show, eq, sexp_of, compare, yojson]

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

(** {2 View mode} *)

type view_mode = List_view | Detail_view of Patch_id.t | Timeline_view
[@@deriving show, eq]

(** {2 Activity entry} *)

type activity_entry =
  | Transition of {
      patch_id : string;
      from_label : string;
      to_status : display_status;
      to_label : string;
      action : string;
    }
  | Event of { patch_id : string option; message : string }

(** {2 Patch view} *)

type patch_view = {
  patch_id : Patch_id.t;
  title : string;
  branch : Branch.t;
  status : display_status;
  queue_len : int;
  current_op : Operation_kind.t option;
  ci_failures : int;
  dep_count : int;
  has_pr : bool;
  has_conflict : bool;
  needs_intervention : bool;
  pending_comments : int;
  ci_checks : Ci_check.t list;
  recent_stream : activity_entry list;
  pr_number : Pr_number.t option;
  base_branch : Branch.t option;
}

(** {2 Frame rendering} *)

type frame

val views_of_orchestrator :
  orchestrator:Orchestrator.t ->
  gameplan:Gameplan.t ->
  activity:activity_entry list ->
  patch_view list

val render_frame :
  width:int ->
  height:int ->
  selected:int ->
  view_mode:view_mode ->
  activity:activity_entry list ->
  project_name:string ->
  ?transcript:string ->
  ?input_line:string ->
  patch_view list ->
  frame

val paint_frame : frame -> string
val frame_to_string : frame -> string
val enter_tui : unit -> string
val exit_tui : unit -> string
