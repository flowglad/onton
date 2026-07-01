(* @archlint.module interface
   @archlint.domain tui *)

open Base
open Types

(** Pure derivation of the patch display status lives in
    {!Onton_core.Display_status}. The transparent type re-export below equates
    [Tui.display_status] with [Display_status.t] so existing call sites that
    spell [Tui.Merged] etc. keep compiling unchanged. *)

type display_status = Display_status.t =
  | Merged
  | Needs_help
  | In_merge_queue
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
      timestamp : float;
    }
  | Event of { patch_id : string option; message : string; timestamp : float }

(** {2 Patch view} *)

type patch_view = {
  patch_id : Patch_id.t;
  title : string;
  branch : Branch.t;
  status : display_status;
  queue_len : int;
  current_op : Operation_kind.t option;
  current_op_state : Patch_agent.op_state;
  ci_failures : int;
  dep_ids : (Patch_id.t * display_status) list;
  has_pr : bool;
  has_conflict : bool;
  needs_intervention : bool;
  human_messages : int;
  ci_checks : Ci_check.t list;
  recent_stream : activity_entry list;
  pr_number : Pr_number.t option;
  merge_queue_entry : Pr_state.merge_queue_entry option;
  pr_missing : bool;
      (** [true] when [pr_number] is set but the remote no longer has the PR
          ([pr_status = Missing _]). Distinguishes "we lost it" from "we have
          it" so the TUI can flag the row distinctively. *)
  base_branch : Branch.t option;
  worktree_path : string option;
  intervention_reason : string option;
  automerge_enabled : bool;
  automerge_deadline : float option;
  automerge_failure_count : int;
  complexity : int option;
      (** Gameplan-author's complexity tier for this patch (1/2/3), or [None]
          when the gameplan didn't specify. *)
  backend : string;
      (** Backend name (e.g. ["claude"], ["codex"]) routed to for this patch
          given its complexity, the CLI flags, and the repo config. *)
  model : string option;
      (** Concrete model name the patch will run under (auto sentinel already
          resolved), or [None] when no [--model] flag was supplied and the
          backend uses its built-in default. *)
}

(** {2 Frame rendering} *)

type frame

val detail_at_bottom : frame -> bool
(** [true] when the detail view scroll is at or past the bottom of content. Used
    by the TUI loop to auto-follow new transcript content. *)

val detail_scroll_offset : frame -> int
(** The actual clamped scroll offset used for the detail view. Written back to
    the detail_scroll ref so delta-based input produces sensible values. *)

val checks_scroll_offset : frame -> int
(** The actual clamped scroll offset used for the CI checks overlay. Written
    back to the checks_scroll ref so delta-based input stays in range. When the
    overlay is not shown this round-trips the input value unchanged. *)

val patches_start_row : frame -> int
(** 1-indexed terminal row where the first patch line is rendered in list view.
    Returns 0 for non-list views. *)

val patches_scroll_offset : frame -> int
(** Scroll offset of the visible patch window in list view. The first visible
    patch row corresponds to patch index [patches_scroll_offset], not 0. *)

val patch_count : frame -> int
(** Number of actually rendered patch rows in the visible window (not total
    patches). Only meaningful in list view; 0 otherwise. *)

val human_intervention_reason : Onton_core.Patch_agent.t -> string option
(** The human-facing, actionable banner line for why a patch needs intervention,
    or [None] when it does not need operator attention. Translates the
    authoritative reason code from {!Onton_core.Patch_agent.intervention_reason}
    (which is driven by the agent's own failure counters) into operator-readable
    text, and also surfaces worktree branch collisions that intentionally block
    runner work. This is the source the detail banner trusts first, ahead of any
    scraped activity-log event. *)

val views_of_orchestrator :
  orchestrator:Orchestrator.t ->
  gameplan:Gameplan.t ->
  activity:activity_entry list ->
  resolve_routing:(complexity:int option -> Backend_routing.decision) ->
  ?intervention_reasons:(Patch_id.t, string) Map.Poly.t ->
  unit ->
  patch_view list

val render_help_overlay :
  width:int -> height:int -> version:string -> string list

val render_manage_overlay :
  width:int ->
  height:int ->
  automerge_enabled:bool ->
  needs_intervention:bool ->
  string list

val render_checks_overlay :
  width:int ->
  height:int ->
  scroll_offset:int ->
  patch_view ->
  string list * int
(** Full-screen scrollable list of a patch's CI checks. Returns the rendered
    lines and the clamped scroll offset. *)

type prompt_info = { prompt_text : string; cursor_col : int }

val render_frame :
  width:int ->
  height:int ->
  selected:int ->
  scroll_offset:int ->
  view_mode:view_mode ->
  activity:activity_entry list ->
  project_name:string ->
  backend_name:string ->
  version:string ->
  show_help:bool ->
  show_checks:bool ->
  checks_scroll:int ->
  show_manage:bool ->
  now:float ->
  ?transcript:string ->
  ?status_msg:status_msg ->
  ?prompt_line:prompt_info ->
  ?dep_select:int * Patch_id.t list ->
  patch_view list ->
  frame

val paint_frame : frame -> string
val frame_to_string : frame -> string
val enter_tui : unit -> string
val exit_tui : unit -> string
