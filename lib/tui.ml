open Base
open Types

(** {1 Color palette} *)

let c_accent = Term.Sgr.fg_256 75 (* soft blue — section headers *)
let c_muted = Term.Sgr.dim (* timestamps, secondary labels *)
let c_ok = Term.Sgr.fg_256 76 (* green — merged, passing *)
let c_warn = Term.Sgr.fg_256 178 (* amber — queued, rebasing *)
let c_alert = Term.Sgr.fg_256 196 (* red — error, needs-help *)
let c_running = Term.Sgr.fg_256 39 (* cyan — active work *)
let c_pending = Term.Sgr.dim (* pending patches *)

(** {1 Display status} *)

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
  | Updating -> "updating"
  | Ci_queued -> "ci-queued"
  | Review_queued -> "review-queued"
  | Awaiting_ci -> "awaiting-ci"
  | Awaiting_review -> "awaiting-review"
  | Pending -> "pending"

let color = function
  | Merged -> c_ok
  | Needs_help -> c_alert
  | Approved_idle -> c_ok
  | Approved_running -> c_running
  | Fixing_ci -> c_warn
  | Addressing_review -> c_warn
  | Resolving_conflict -> c_warn
  | Responding_to_human -> Term.Sgr.fg_magenta
  | Rebasing -> c_warn
  | Starting -> c_running
  | Updating -> c_running
  | Ci_queued -> c_warn
  | Review_queued -> c_warn
  | Awaiting_ci -> c_accent
  | Awaiting_review -> c_accent
  | Pending -> c_pending

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
        if State.Patch_ctx.has_pr ctx ~patch_id then Updating else Starting
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

let%test "busy no op with pr falls back to updating" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Updating
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

(** {1 Scroll state} *)

type scroll_state = { offset : int; total : int; visible : int }

let make_scroll ~total ~visible = { offset = 0; total; visible }
let scroll_max s = if s.total > s.visible then s.total - s.visible else 0

let clamp_scroll s delta =
  let new_offset = s.offset + delta in
  let clamped = Int.max 0 (Int.min new_offset (scroll_max s)) in
  { s with offset = clamped }

let scroll_indicators s =
  let max = scroll_max s in
  let offset = Int.max 0 (Int.min s.offset max) in
  let top = if offset > 0 then Printf.sprintf "↑ %d more" offset else "" in
  let remaining = max - offset in
  let bottom =
    if remaining > 0 then Printf.sprintf "↓ %d more" remaining else ""
  in
  (top, bottom)

let%test "scroll_max basic" =
  scroll_max { offset = 0; total = 50; visible = 20 } = 30

let%test "scroll_max when total <= visible" =
  scroll_max { offset = 0; total = 10; visible = 20 } = 0

let%test "clamp_scroll large negative clamps to 0" =
  let s = { offset = 5; total = 50; visible = 20 } in
  (clamp_scroll s (-100)).offset = 0

let%test "clamp_scroll large positive clamps to scroll_max" =
  let s = { offset = 5; total = 50; visible = 20 } in
  (clamp_scroll s 1000).offset = 30

let%test "scroll_indicators mid-scroll" =
  let s = { offset = 5; total = 28; visible = 20 } in
  let top, bottom = scroll_indicators s in
  String.equal top "↑ 5 more" && String.equal bottom "↓ 3 more"

let%test "scroll_indicators at top" =
  let s = { offset = 0; total = 30; visible = 20 } in
  let top, bottom = scroll_indicators s in
  String.is_empty top && String.equal bottom "↓ 10 more"

let%test "scroll_indicators at bottom" =
  let s = { offset = 10; total = 30; visible = 20 } in
  let top, bottom = scroll_indicators s in
  String.equal top "↑ 10 more" && String.is_empty bottom

let%test "scroll_indicators no scroll needed" =
  let s = { offset = 0; total = 10; visible = 20 } in
  let top, bottom = scroll_indicators s in
  String.is_empty top && String.is_empty bottom

(** {1 Styling} *)

let status_style = function
  | Merged -> [ c_ok; Term.Sgr.bold ]
  | Needs_help -> [ c_alert; Term.Sgr.bold ]
  | Approved_idle -> [ c_ok ]
  | Approved_running -> [ c_ok; Term.Sgr.bold ]
  | Fixing_ci | Addressing_review | Resolving_conflict | Responding_to_human ->
      [ c_running; Term.Sgr.bold ]
  | Rebasing -> [ c_warn ]
  | Starting | Updating -> [ c_running ]
  | Ci_queued | Review_queued -> [ c_warn ]
  | Awaiting_ci -> [ c_accent ]
  | Awaiting_review -> [ c_accent ]
  | Pending -> [ c_pending ]

let status_indicator = function
  | Merged -> "✓"
  | Needs_help -> "!"
  | Approved_idle -> "✓"
  | Approved_running -> "▶"
  | Fixing_ci | Addressing_review | Resolving_conflict | Responding_to_human ->
      "▶"
  | Rebasing -> "↻"
  | Starting | Updating -> "▶"
  | Ci_queued | Review_queued -> "◎"
  | Awaiting_ci | Awaiting_review -> "◎"
  | Pending -> "·"

(** {1 Status messages} *)

type msg_level = Info | Warning | Error [@@deriving show, eq]

type status_msg = {
  level : msg_level;
  text : string;
  expires_at : float option;
}
[@@deriving show, eq]

let msg_expired ~now msg =
  match msg.expires_at with Some t -> Float.( >= ) now t | None -> false

let sanitize_text s =
  let s = Term.strip_ansi s in
  String.map s ~f:(fun c ->
      let code = Char.to_int c in
      if code < 0x20 || code = 0x7F then ' ' else c)

let render_status_msg ~width = function
  | None -> ""
  | Some msg ->
      let safe_text = sanitize_text msg.text in
      let safe_w = Int.max 1 width in
      let raw =
        match msg.level with
        | Info -> Term.styled [ Term.Sgr.dim ] safe_text
        | Warning ->
            Term.styled [ Term.Sgr.fg_yellow ] (Printf.sprintf "⚠ %s" safe_text)
        | Error ->
            Term.styled
              [ Term.Sgr.fg_red; Term.Sgr.bold ]
              (Printf.sprintf "✗ %s" safe_text)
      in
      Term.fit_width safe_w raw

let%test "render_status_msg None is empty" =
  String.equal (render_status_msg ~width:80 None) ""

let%test "render_status_msg Error contains text" =
  let s =
    render_status_msg ~width:80
      (Some { level = Error; text = "oops"; expires_at = None })
  in
  String.is_substring s ~substring:"oops"

let%test "render_status_msg Error contains red ANSI" =
  let s =
    render_status_msg ~width:80
      (Some { level = Error; text = "oops"; expires_at = None })
  in
  String.is_substring s ~substring:"\027[31m"
  || String.is_substring s ~substring:"\027[1;31m"
  || String.is_substring s ~substring:"\027[31;1m"

let%test "render_status_msg Warning has prefix" =
  let s =
    render_status_msg ~width:80
      (Some { level = Warning; text = "watch out"; expires_at = None })
  in
  String.is_substring s ~substring:"⚠"
  && String.is_substring s ~substring:"watch out"

let%test "render_status_msg Info is styled dim" =
  let s =
    render_status_msg ~width:80
      (Some { level = Info; text = "hello"; expires_at = None })
  in
  String.is_substring s ~substring:"hello"
  && String.is_substring s ~substring:"\027[2m"

let%test "msg_expired true when past" =
  msg_expired ~now:100.0 { level = Info; text = "x"; expires_at = Some 50.0 }

let%test "msg_expired false when persistent" =
  not (msg_expired ~now:100.0 { level = Info; text = "x"; expires_at = None })

let%test "msg_expired true at exact expiry time" =
  msg_expired ~now:100.0 { level = Info; text = "x"; expires_at = Some 100.0 }

let%test "msg_expired false when not yet expired" =
  not
    (msg_expired ~now:100.0
       { level = Info; text = "x"; expires_at = Some 200.0 })

(** {1 View mode — list vs detail} *)

type view_mode = List_view | Detail_view of Patch_id.t | Timeline_view
[@@deriving show, eq]

(** Activity entry for rendering — breaks the Tui↔Activity_log cycle. *)
type activity_entry =
  | Transition of {
      patch_id : string;
      from_label : string;
      to_status : display_status;
      to_label : string;
      action : string;
    }
  | Event of { patch_id : string option; message : string }
[@@warning "-37"]

(** {1 Patch view — derived per-patch rendering data} *)

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
  worktree_path : string option;
}
[@@warning "-69"]

let patch_view_of_agent (agent : Patch_agent.t)
    ~(patches_by_id : Patch.t Map.M(Patch_id).t) ~(graph : Graph.t) =
  let patch_id = agent.patch_id in
  let patch_opt = Map.find patches_by_id patch_id in
  let title =
    match patch_opt with
    | Some p -> p.Patch.title
    | None -> (
        match agent.head_branch with
        | Some b -> Branch.to_string b
        | None -> Patch_id.to_string patch_id)
  in
  let branch =
    match patch_opt with
    | Some p -> p.Patch.branch
    | None -> Branch.of_string (Patch_id.to_string patch_id)
  in
  let current_op = Patch_agent.highest_priority agent in
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_merged ~patch_id ~value:agent.merged
    |> State.Patch_ctx.set_needs_intervention ~patch_id
         ~value:agent.needs_intervention
    |> State.Patch_ctx.set_busy ~patch_id ~value:agent.busy
    |> State.Patch_ctx.set_has_pr ~patch_id ~value:agent.has_pr
    |> State.Patch_ctx.set_approved ~patch_id
         ~value:(Patch_agent.is_approved agent)
    |> State.Patch_ctx.set_ci_failure_count ~patch_id
         ~count:agent.ci_failure_count
    |> fun ctx ->
    List.fold agent.queue ~init:ctx ~f:(fun acc kind ->
        State.Patch_ctx.set_queued acc ~patch_id ~kind ~value:true)
  in
  let status = derive_display_status ctx ~patch_id ~current_op in
  let dep_count = List.length (Graph.deps graph patch_id) in
  {
    patch_id;
    title;
    branch;
    status;
    queue_len = List.length agent.queue;
    current_op;
    ci_failures = agent.ci_failure_count;
    dep_count;
    has_pr = agent.has_pr;
    has_conflict = agent.has_conflict;
    needs_intervention = agent.needs_intervention;
    pending_comments = List.length agent.pending_comments;
    ci_checks = agent.ci_checks;
    recent_stream = [];
    pr_number = agent.pr_number;
    base_branch = agent.base_branch;
    worktree_path = agent.worktree_path;
  }

(** {1 Render helpers} *)

let styled_status status text = Term.styled (status_style status) text

let render_status_badge status =
  let ind = status_indicator status in
  let lbl = label status in
  styled_status status (Printf.sprintf "%s %s" ind lbl)

(** {1 Frame rendering} *)

type frame = { lines : string list; width : int; detail_at_bottom : bool }
[@@warning "-69"]

let render_header ~project_name ~width =
  let prefix = "── " in
  let suffix_pad = Int.max 0 (width - String.length prefix - String.length project_name - 1) in
  let rule_suffix = Term.hrule ~ch:"─" suffix_pad in
  let title_line =
    Term.styled [ c_accent ]
      (Printf.sprintf "%s%s %s" prefix project_name rule_suffix)
  in
  [ title_line ]

let short_op_name = function
  | Operation_kind.Ci -> "ci"
  | Operation_kind.Review_comments -> "review"
  | Operation_kind.Merge_conflict -> "conflict"
  | Operation_kind.Human -> "human"
  | Operation_kind.Rebase -> "rebase"

let render_patch_row ~width ~selected (pv : patch_view) =
  let ind = status_indicator pv.status in
  let styled_ind = styled_status pv.status (Printf.sprintf "[%s]" ind) in
  let pr_tag =
    match pv.pr_number with
    | Some n ->
        Term.styled [ c_muted ] (Printf.sprintf "PR#%d" (Pr_number.to_int n))
    | None -> ""
  in
  let branch_str = Term.styled [ c_muted ] (Branch.to_string pv.branch) in
  let status_suffix = styled_status pv.status (label pv.status) in
  let op_suffix =
    match pv.current_op with
    | Some op ->
        Term.styled [ c_muted ] (Printf.sprintf " (%s)" (short_op_name op))
    | None -> ""
  in
  let cursor = if selected then "▸" else " " in
  let row =
    Term.fit_width width
      (Printf.sprintf "%s %s %s  %s  %s%s" cursor styled_ind pr_tag branch_str
         status_suffix op_suffix)
  in
  if selected then Term.styled [ Term.Sgr.reverse ] row else row

(** Compute visible window: returns (offset, count) for scrolling. *)
let visible_window ~selected ~total ~max_visible =
  if total <= max_visible then (0, total)
  else
    let half = max_visible / 2 in
    let offset =
      if selected < half then 0
      else if selected > total - max_visible + half then total - max_visible
      else selected - half
    in
    (offset, max_visible)

let render_patches ~width ~selected ~max_visible (views : patch_view list) =
  let total = List.length views in
  if total = 0 then
    [
      Term.styled [ Term.Sgr.bold ] " Patches";
      Term.styled [ Term.Sgr.dim ]
        (Term.fit_width
           (Int.max 1 (width - 2))
           "  No patches. Press : then +N to add a PR (e.g. +123)");
    ]
  else
    let offset, count = visible_window ~selected ~total ~max_visible in
    let section_header = Term.styled [ c_accent ] " Patches" in
    let visible =
      List.sub views ~pos:offset ~len:(min count (total - offset))
    in
    let rows =
      List.mapi visible ~f:(fun i pv ->
          render_patch_row ~width ~selected:(offset + i = selected) pv)
    in
    let scroll_up =
      if offset > 0 then
        [ Term.styled [ Term.Sgr.dim ] (Printf.sprintf " ↑ %d more" offset) ]
      else []
    in
    let remaining = total - offset - count in
    let scroll_down =
      if remaining > 0 then
        [ Term.styled [ Term.Sgr.dim ] (Printf.sprintf " ↓ %d more" remaining) ]
      else []
    in
    (section_header :: scroll_up) @ rows @ scroll_down

let render_summary (views : patch_view list) =
  let count status =
    List.count views ~f:(fun v -> equal_display_status v.status status)
  in
  let merged = count Merged in
  let is_running status =
    match status with
    | Fixing_ci | Addressing_review | Resolving_conflict | Responding_to_human
    | Rebasing | Starting | Updating | Approved_running ->
        true
    | Merged | Needs_help | Approved_idle | Ci_queued | Review_queued
    | Awaiting_ci | Awaiting_review | Pending ->
        false
  in
  let running = List.count views ~f:(fun v -> is_running v.status) in
  let awaiting =
    List.count views ~f:(fun v ->
        match v.status with
        | Awaiting_ci | Awaiting_review | Ci_queued | Review_queued -> true
        | Merged | Needs_help | Approved_idle | Approved_running | Fixing_ci
        | Addressing_review | Resolving_conflict | Responding_to_human
        | Rebasing | Starting | Updating | Pending ->
            false)
  in
  let needs_help = count Needs_help in
  let parts =
    [
      (if merged > 0 then
         Some (Term.styled [ c_ok ] (Printf.sprintf "%d merged" merged))
       else None);
      (if running > 0 then
         Some (Term.styled [ c_running ] (Printf.sprintf "%d running" running))
       else None);
      (if awaiting > 0 then
         Some (Term.styled [ c_warn ] (Printf.sprintf "%d awaiting" awaiting))
       else None);
      (if needs_help > 0 then
         Some
           (Term.styled [ c_alert ]
              (Printf.sprintf "%d \xe2\x9a\xa0 needs help" needs_help))
       else None);
    ]
    |> List.filter_map ~f:Fn.id
  in
  " " ^ String.concat ~sep:"  " parts

let section_rule ~label:lbl ~width =
  let prefix = Printf.sprintf "── %s " lbl in
  let pad = Int.max 0 (width - String.length prefix) in
  Term.styled [ c_muted ] (prefix ^ Term.hrule ~ch:"─" pad)

let render_activity ~width (entries : activity_entry list) =
  if List.is_empty entries then []
  else
    let header = section_rule ~label:"Recent Activity" ~width in
    let lines =
      List.map entries ~f:(fun entry ->
          match entry with
          | Transition { patch_id; from_label; to_status; to_label; action } ->
              Printf.sprintf "  %s: %s → %s (%s)"
                (Term.styled [ c_muted ] patch_id)
                from_label
                (styled_status to_status to_label)
                (Term.styled [ c_muted ] action)
          | Event { patch_id; message } ->
              let prefix =
                match patch_id with
                | Some pid -> Term.styled [ c_muted ] (pid ^ ": ")
                | None -> "  "
              in
              Printf.sprintf "  %s%s" prefix
                (Term.styled [ c_muted ] message))
    in
    header :: lines

let render_detail (pv : patch_view) ~width ?(transcript = "") () =
  let header =
    Term.styled [ Term.Sgr.bold ]
      (Term.fit_width (Int.max 1 (width - 1)) (" " ^ pv.title))
  in
  let info_rule = section_rule ~label:"Info" ~width in
  let badge = render_status_badge pv.status in
  let col_w = 16 in
  let grid lbl value =
    let pad = String.make (Int.max 1 (col_w - String.length lbl)) ' ' in
    Printf.sprintf "  %s%s%s" lbl pad value
  in
  let lines =
    [
      header;
      info_rule;
      grid "Status" (Term.strip_ansi badge |> fun _ -> badge);
      grid "Patch ID" (Patch_id.to_string pv.patch_id);
      grid "Branch" (Branch.to_string pv.branch);
      grid "Base"
        (match pv.base_branch with
        | Some b -> Branch.to_string b
        | None -> Term.styled [ c_muted ] "(not set)");
      grid "Worktree"
        (match pv.worktree_path with
        | Some p -> p
        | None -> Term.styled [ c_muted ] "(none)");
      grid "PR"
        (match pv.pr_number with
        | Some n -> Printf.sprintf "#%d" (Pr_number.to_int n)
        | None -> if pv.has_pr then "yes" else "no");
      grid "Dependencies" (Printf.sprintf "%d" pv.dep_count);
      grid "CI failures"
        (if pv.ci_failures > 0 then
           Term.styled [ c_alert ] (Printf.sprintf "%d" pv.ci_failures)
         else "0");
      grid "Queue depth" (Printf.sprintf "%d" pv.queue_len);
      grid "Conflict"
        (if pv.has_conflict then Term.styled [ c_warn ] "yes" else "no");
      grid "Comments"
        (if pv.pending_comments > 0 then
           Printf.sprintf "%d pending" pv.pending_comments
         else "0 pending");
    ]
  in
  let op_line =
    match pv.current_op with
    | Some op ->
        [ grid "Current op" (Term.styled [ c_running ] (short_op_name op)) ]
    | None -> []
  in
  let intervention =
    if pv.needs_intervention then
      let banner_text = " NEEDS HUMAN ATTENTION " in
      let inner_w = Int.max (String.length banner_text) (width - 6) in
      let top =
        Printf.sprintf "  \xe2\x95\x94%s\xe2\x95\x97"
          (Term.repeat inner_w "\xe2\x95\x90")
      in
      let mid =
        Printf.sprintf "  \xe2\x95\x91%-*s\xe2\x95\x91" inner_w banner_text
      in
      let bot =
        Printf.sprintf "  \xe2\x95\x9a%s\xe2\x95\x9d"
          (Term.repeat inner_w "\xe2\x95\x90")
      in
      [
        "";
        Term.styled [ c_alert; Term.Sgr.bold ] top;
        Term.styled [ c_alert; Term.Sgr.bold ] mid;
        Term.styled [ c_alert; Term.Sgr.bold ] bot;
      ]
    else []
  in
  let ci_section =
    if List.is_empty pv.ci_checks then []
    else
      let failure_conclusions =
        [
          "failure"; "error"; "action_required"; "timed_out"; "startup_failure";
        ]
      in
      let ci_header = [ ""; section_rule ~label:"CI Checks" ~width ] in
      let ci_rows =
        List.map pv.ci_checks ~f:(fun (c : Ci_check.t) ->
            let icon =
              if String.equal c.conclusion "success" then
                Term.styled [ c_ok ] "\xe2\x9c\x93"
              else if
                List.mem failure_conclusions c.conclusion ~equal:String.equal
              then Term.styled [ c_alert ] "\xe2\x9c\x97"
              else Term.styled [ c_warn ] "?"
            in
            Printf.sprintf "    %s %s: %s" icon c.name c.conclusion)
      in
      ci_header @ ci_rows
  in
  let comment_section =
    if pv.pending_comments > 0 then
      [ ""; Term.styled [ c_muted ] (Printf.sprintf "  %d pending comments" pv.pending_comments) ]
    else []
  in
  let transcript_section =
    if String.is_empty transcript then []
    else
      let transcript_header =
        [ ""; section_rule ~label:"Transcript" ~width ]
      in
      let wrap_line max_w line =
        let stripped_len = String.length (Term.strip_ansi line) in
        if stripped_len <= max_w then [ line ]
        else
          let rec split acc pos =
            if pos >= String.length line then List.rev acc
            else
              let len = Int.min max_w (String.length line - pos) in
              split (String.sub line ~pos ~len :: acc) (pos + len)
          in
          split [] 0
      in
      let content_width = Int.max 1 (width - 4) in
      let rendered = Markdown_render.render_to_lines transcript in
      let transcript_lines =
        List.concat_map rendered ~f:(fun line ->
            wrap_line content_width line
            |> List.map ~f:(fun chunk -> "    " ^ chunk))
      in
      transcript_header @ transcript_lines
  in
  let info =
    lines @ op_line @ intervention @ ci_section @ comment_section
  in
  (info, transcript_section)

let render_timeline ~width ~selected ~max_visible
    (entries : activity_entry list) =
  let total = List.length entries in
  let offset, count = visible_window ~selected ~total ~max_visible in
  let header = section_rule ~label:"Timeline" ~width in
  let visible =
    List.sub entries ~pos:offset ~len:(min count (total - offset))
  in
  let rows =
    List.mapi visible ~f:(fun i entry ->
        let is_selected = offset + i = selected in
        let row =
          match entry with
          | Transition { patch_id; from_label; to_status; to_label; action } ->
              Printf.sprintf "  %s  #%s  %s \xe2\x86\x92 %s (%s)"
                (Term.styled [ c_muted ] "··:··:··")
                (Term.styled [ c_muted ] patch_id)
                (Term.styled [ c_muted ] from_label)
                (styled_status to_status to_label)
                (Term.styled [ c_muted ] action)
          | Event { patch_id; message } ->
              let pid_tag =
                match patch_id with
                | Some pid -> Printf.sprintf "  #%s" pid
                | None -> ""
              in
              Printf.sprintf "  %s%s  \xc2\xb7  %s"
                (Term.styled [ c_muted ] "··:··:··")
                (Term.styled [ c_muted ] pid_tag)
                (Term.styled [ c_muted ] message)
        in
        let row = Term.fit_width (Int.max 1 (width - 1)) row in
        if is_selected then Term.styled [ Term.Sgr.reverse ] row else row)
  in
  let scroll_up =
    if offset > 0 then
      [ Term.styled [ c_muted ] (Printf.sprintf " \xe2\x86\x91 %d more" offset) ]
    else []
  in
  let remaining = total - offset - count in
  let scroll_down =
    if remaining > 0 then
      [ Term.styled [ c_muted ] (Printf.sprintf " \xe2\x86\x93 %d more" remaining) ]
    else []
  in
  (header :: scroll_up) @ rows @ scroll_down

let render_footer ~width ~view_mode ?input_line ?completion_hint () =
  match input_line with
  | Some text ->
      let ghost =
        match completion_hint with
        | Some hint when not (String.is_empty hint) ->
            Term.styled [ Term.Sgr.italic; Term.Sgr.dim ] hint
        | Some _ | None -> ""
      in
      let prompt_str =
        Term.fit_width
          (Int.max 1 (width - 2))
          (Printf.sprintf ": %s%s" text ghost)
      in
      [ Term.hrule width; prompt_str ]
  | None ->
      let help =
        match view_mode with
        | List_view ->
            Term.styled [ c_muted ]
              " [j/k] navigate  [Enter] detail  [t] timeline  [h] help  \
               [:] command  [q] quit"
        | Detail_view _ ->
            Term.styled [ c_muted ]
              " [\xe2\x86\x91\xe2\x86\x93] scroll  [Esc] back  [t] timeline  [h] help  [q] quit"
        | Timeline_view ->
            Term.styled [ c_muted ]
              " [\xe2\x86\x91\xe2\x86\x93] scroll  [Esc] back  [t] list  [h] help  [q] quit"
      in
      [ Term.hrule width; help ]

let render_help_overlay ~width ~height =
  let pair_rows keys =
    let col_w =
      List.fold keys ~init:0 ~f:(fun m k -> Int.max m (String.length k))
    in
    let rec go acc = function
      | a :: b :: tl -> go (Printf.sprintf "    %-*s %s" col_w a b :: acc) tl
      | [ a ] -> List.rev (Printf.sprintf "    %s" a :: acc)
      | [] -> List.rev acc
    in
    go [] keys
  in
  let sections =
    [
      ( "List View",
        [
          "↑/k       Move up";
          "↓/j       Move down";
          "PgUp      Page up";
          "PgDn      Page down";
          "Enter     Open detail";
          "t         Toggle timeline";
          ":         Enter command mode";
          "r         Refresh";
          "q         Quit";
        ] );
      ( "Detail View",
        [
          "↑/k       Scroll up";
          "↓/j       Scroll down";
          "PgUp      Page up (10 lines)";
          "PgDn      Page down (10 lines)";
          "Enter     Enter command mode";
          "Esc/Bksp  Back to list";
          "t         Toggle timeline";
        ] );
      ( "Timeline View",
        [
          "↑/k       Scroll up";
          "↓/j       Scroll down";
          "PgUp      Page up";
          "PgDn      Page down";
          "Esc/Bksp  Back to list";
          "t         Back to list";
        ] );
      ( "Command Mode (:)",
        [
          "N> msg    Send message to patch N";
          "+123      Add PR #123 to selected patch";
          "w /path   Register worktree for patch";
          "-         Remove selected patch";
          "Esc       Cancel";
          "Enter     Execute command";
          "↑/↓       Browse command history";
        ] );
    ]
  in
  let dismiss = Term.styled [ Term.Sgr.dim ] "(any key to dismiss)" in
  let title =
    Term.styled
      [ Term.Sgr.bold; Term.Sgr.fg_cyan ]
      (Printf.sprintf " Keyboard Shortcuts  %s" dismiss)
  in
  let body =
    List.concat_map sections ~f:(fun (header, keys) ->
        Term.styled [ Term.Sgr.bold ] (Printf.sprintf "  %s" header)
        :: List.map (pair_rows keys) ~f:(fun row ->
            Term.styled [ Term.Sgr.dim ] row))
  in
  let content = title :: body in
  let overlay_h = Int.max 0 (Int.min (List.length content) (height - 1)) in
  let visible = List.sub content ~pos:0 ~len:overlay_h in
  let pad_line line = if width <= 0 then "" else Term.fit_width width line in
  List.map visible ~f:pad_line

(** {1 Public API} *)

let views_of_orchestrator ~(orchestrator : Orchestrator.t)
    ~(gameplan : Gameplan.t) ~(activity : activity_entry list) =
  let agents = Orchestrator.all_agents orchestrator in
  let graph = Orchestrator.graph orchestrator in
  let patches_by_id =
    List.fold gameplan.patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) -> Map.set acc ~key:p.Patch.id ~data:p)
  in
  let views =
    List.map agents ~f:(fun agent ->
        let pv = patch_view_of_agent agent ~patches_by_id ~graph in
        let pid_str = Patch_id.to_string agent.patch_id in
        let filtered =
          List.filter activity ~f:(fun entry ->
              match entry with
              | Transition { patch_id = pid; _ } -> String.equal pid pid_str
              | Event { patch_id = Some pid; _ } -> String.equal pid pid_str
              | Event { patch_id = None; _ } -> false)
        in
        { pv with recent_stream = List.take filtered 10 })
  in
  (* Sort by gameplan order (numeric patch IDs sort naturally). *)
  let order =
    List.mapi gameplan.patches ~f:(fun i (p : Patch.t) -> (p.Patch.id, i))
    |> Map.of_alist_reduce (module Patch_id) ~f:(fun a _ -> a)
  in
  List.sort views ~compare:(fun a b ->
      let idx_a = Map.find order a.patch_id |> Option.value ~default:999 in
      let idx_b = Map.find order b.patch_id |> Option.value ~default:999 in
      Int.compare idx_a idx_b)

let render_frame ~width ~height ~selected ~view_mode
    ~(activity : activity_entry list) ~project_name ~show_help
    ?(transcript = "") ?input_line ?completion_hint ?status_msg
    (views : patch_view list) =
  if show_help then
    let overlay = render_help_overlay ~width ~height in
    { lines = overlay; width; detail_at_bottom = false }
  else
    let header = render_header ~project_name ~width in
    let summary = [ render_summary views ] in
    let footer =
      render_footer ~width ~view_mode ?input_line ?completion_hint ()
    in
    let status_line =
      let rendered = render_status_msg ~width status_msg in
      if String.is_empty rendered then [] else [ rendered ]
    in
    match view_mode with
    | Detail_view patch_id ->
        let info, transcript_lines =
          match
            List.find views ~f:(fun pv -> Patch_id.equal pv.patch_id patch_id)
          with
          | Some pv -> render_detail pv ~width ~transcript ()
          | None -> ([ " (patch not found)" ], [])
        in
        (* Chrome: header(1) + blank + summary(1) + blank + info + blank
           + status(0-1) + footer(2) *)
        let fixed_lines =
          1 + 1 + 1 + 1 + List.length info + 1 + List.length status_line + 2
        in
        let max_transcript = Int.max 0 (height - fixed_lines) in
        let total_transcript = List.length transcript_lines in
        let max_scroll = Int.max 0 (total_transcript - max_transcript) in
        let scroll_offset = Int.min selected max_scroll in
        let at_bottom = selected >= max_scroll in
        let visible_transcript =
          List.sub transcript_lines ~pos:scroll_offset
            ~len:(Int.min max_transcript (total_transcript - scroll_offset))
        in
        let lines =
          header @ [ "" ] @ summary @ [ "" ] @ info @ visible_transcript
          @ [ "" ] @ status_line @ footer
        in
        { lines; width; detail_at_bottom = at_bottom }
    | Timeline_view ->
        (* Budget: header(1) + blank + summary(1) + blank + "Timeline" header(1)
         + scroll indicators(2) + blank + status(0-1) + footer(2) *)
        let reserved =
          1 + 1 + 1 + 1 + 1 + 2 + 1 + List.length status_line + 2
        in
        let max_rows = Int.max 0 (height - reserved) in
        let timeline =
          render_timeline ~width ~selected ~max_visible:max_rows activity
        in
        let lines =
          header @ [ "" ] @ summary @ [ "" ] @ timeline @ [ "" ] @ status_line
          @ footer
        in
        { lines; width; detail_at_bottom = false }
    | List_view ->
        let activity_lines = render_activity ~width activity in
        let activity_height =
          if List.is_empty activity_lines then 0
          else 1 + List.length activity_lines
        in
        (* Budget: header(1) + blank + summary(1) + blank + "Patches" header(1)
         + scroll indicators(2) + blank + status(0-1) + footer(2) +
         activity block *)
        let reserved =
          1 + 1 + 1 + 1 + 1 + 2 + 1 + List.length status_line + 2
          + activity_height
        in
        let max_patch_rows = Int.max 0 (height - reserved) in
        let patches =
          render_patches ~width ~selected ~max_visible:max_patch_rows views
        in
        let lines =
          header @ [ "" ] @ summary @ [ "" ] @ patches
          @ (if List.is_empty activity_lines then [] else "" :: activity_lines)
          @ [ "" ] @ status_line @ footer
        in
        { lines; width; detail_at_bottom = false }

let frame_to_string (frame : frame) = String.concat ~sep:"\n" frame.lines ^ "\n"
let detail_at_bottom frame = frame.detail_at_bottom

let paint_frame (frame : frame) =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf (Term.Cursor.move_to ~row:1 ~col:1);
  List.iter frame.lines ~f:(fun line ->
      Buffer.add_string buf line;
      Buffer.add_string buf Term.Clear.line_to_end;
      Buffer.add_string buf "\n");
  Buffer.add_string buf Term.Clear.to_end;
  Buffer.contents buf

let enter_tui () =
  Term.Cursor.hide ^ Term.Clear.screen
  ^ Term.Cursor.move_to ~row:1 ~col:1
  ^ "\027[?2004h" (* enable bracketed paste *)

let exit_tui () =
  "\027[?2004l" (* disable bracketed paste *) ^ Term.Clear.screen
  ^ Term.Cursor.move_to ~row:1 ~col:1
  ^ Term.Cursor.show
