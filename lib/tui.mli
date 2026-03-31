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
  | Adding_notes
  | Rebasing
  | Starting
  | Updating
  | Ci_queued
  | Review_queued
  | Awaiting_ci
  | Awaiting_review
  | Blocked_by_dep
  | Pending
[@@deriving show, eq, sexp_of, compare, yojson]

type status_display [@@deriving show, eq, sexp_of, compare]

val label : display_status -> string
val color : display_status -> string
val to_status_display : display_status -> status_display
val styled_label : display_status -> string

(** {2 Status messages} *)

type msg_level = Info | Warning | Error [@@deriving show, eq]

type status_msg = {
  level : msg_level;
  text : string;
  expires_at : float option;
}
[@@deriving show, eq]

val msg_expired : now:float -> status_msg -> bool

val derive_display_status :
  State.Patch_ctx.t ->
  patch_id:Patch_id.t ->
  current_op:Operation_kind.t option ->
  main_branch:Branch.t ->
  display_status

(** {2 Scroll state} *)

type scroll_state = { offset : int; total : int; visible : int }

val make_scroll : total:int -> visible:int -> scroll_state
val clamp_scroll : scroll_state -> int -> scroll_state
val scroll_max : scroll_state -> int
val scroll_indicators : scroll_state -> string * string

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
  dep_ids : (Patch_id.t * display_status) list;
  has_pr : bool;
  has_conflict : bool;
  needs_intervention : bool;
  human_messages : int;
  ci_checks : Ci_check.t list;
  recent_stream : activity_entry list;
  pr_number : Pr_number.t option;
  base_branch : Branch.t option;
  worktree_path : string option;
  intervention_reason : string option;
}

(** {2 Frame rendering} *)

type frame

val detail_at_bottom : frame -> bool
(** [true] when the detail view scroll is at or past the bottom of content. Used
    by the TUI loop to auto-follow new transcript content. *)

val detail_scroll_offset : frame -> int
(** The actual clamped scroll offset used for the detail view. Written back to
    the detail_scroll ref so delta-based input produces sensible values. *)

val patches_start_row : frame -> int
(** 1-indexed terminal row where the first patch line is rendered in list view.
    Returns 0 for non-list views. *)

val patches_scroll_offset : frame -> int
(** Scroll offset of the visible patch window in list view. The first visible
    patch row corresponds to patch index [patches_scroll_offset], not 0. *)

val patch_count : frame -> int
(** Number of actually rendered patch rows in the visible window (not total
    patches). Only meaningful in list view; 0 otherwise. *)

val views_of_orchestrator :
  orchestrator:Orchestrator.t ->
  gameplan:Gameplan.t ->
  activity:activity_entry list ->
  ?intervention_reasons:(Patch_id.t, string) Map.Poly.t ->
  unit ->
  patch_view list

val render_help_overlay : width:int -> height:int -> string list

val render_frame :
  width:int ->
  height:int ->
  selected:int ->
  scroll_offset:int ->
  view_mode:view_mode ->
  activity:activity_entry list ->
  project_name:string ->
  show_help:bool ->
  ?transcript:string ->
  ?status_msg:status_msg ->
  ?prompt_line:string ->
  patch_view list ->
  frame

val paint_frame : frame -> string
val frame_to_string : frame -> string
val enter_tui : unit -> string
val exit_tui : unit -> string
