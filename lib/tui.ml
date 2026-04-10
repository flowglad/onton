open Base
open Types

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
  | Adding_notes -> "adding-notes"
  | Rebasing -> "rebasing"
  | Starting -> "starting"
  | Updating -> "updating"
  | Ci_queued -> "ci-queued"
  | Review_queued -> "review-queued"
  | Awaiting_ci -> "awaiting-ci"
  | Awaiting_review -> "awaiting-review"
  | Blocked_by_dep -> "blocked-by-dep"
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
  | Adding_notes -> Term.Sgr.fg_cyan
  | Rebasing -> Term.Sgr.fg_cyan
  | Starting -> Term.Sgr.fg_cyan
  | Updating -> Term.Sgr.fg_cyan
  | Ci_queued -> Term.Sgr.fg_yellow
  | Review_queued -> Term.Sgr.fg_yellow
  | Awaiting_ci -> Term.Sgr.fg_blue
  | Awaiting_review -> Term.Sgr.fg_blue
  | Blocked_by_dep -> Term.Sgr.fg_blue
  | Pending -> Term.Sgr.fg_default

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
let is_on_main ctx ~patch_id ~main_branch =
  match State.Patch_ctx.base_branch ctx ~patch_id with
  | Some b -> Branch.equal b main_branch
  | None -> true

let derive_display_status (ctx : State.Patch_ctx.t) ~patch_id
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
    | Some Merge_conflict -> Resolving_conflict
    | Some Human -> Responding_to_human
    | Some Implementation_notes -> Adding_notes
    | Some Rebase -> Rebasing
    | None ->
        if State.Patch_ctx.has_pr ctx ~patch_id then Updating else Starting
  else if State.Patch_ctx.has_pr ctx ~patch_id then
    if not (is_on_main ctx ~patch_id ~main_branch) then Blocked_by_dep
    else if State.Patch_ctx.is_queued ctx ~patch_id ~kind:Ci then Ci_queued
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
       ~current_op:None ~main_branch:(Branch.of_string "main"))

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
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "approved idle" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal_display_status Approved_idle
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "approved running" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Approved_running
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Rebase) ~main_branch:(Branch.of_string "main"))

let%test "approved running ignores current_op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Approved_running
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "busy with ci op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Fixing_ci
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Ci) ~main_branch:(Branch.of_string "main"))

let%test "busy with review op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Addressing_review
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Review_comments) ~main_branch:(Branch.of_string "main"))

let%test "busy with merge conflict op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Resolving_conflict
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Merge_conflict) ~main_branch:(Branch.of_string "main"))

let%test "busy with human op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Responding_to_human
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Human) ~main_branch:(Branch.of_string "main"))

let%test "busy with rebase op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Rebasing
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Rebase) ~main_branch:(Branch.of_string "main"))

let%test "busy no op with pr falls back to updating" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Updating
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "busy no op without pr falls back to starting" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Starting
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "ci queued" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_queued ~patch_id:(Patch_id.of_string "1") ~kind:Ci
         ~value:true
  in
  equal_display_status Ci_queued
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "review queued" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_queued ~patch_id:(Patch_id.of_string "1")
         ~kind:Review_comments ~value:true
  in
  equal_display_status Review_queued
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "awaiting ci when failure count > 0" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_ci_failure_count ~patch_id:(Patch_id.of_string "1")
         ~count:2
  in
  equal_display_status Awaiting_ci
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "awaiting review default" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal_display_status Awaiting_review
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "blocked by dep when base_branch is not main" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_base_branch ~patch_id:(Patch_id.of_string "1")
         ~branch:(Branch.of_string "feature/dep")
  in
  equal_display_status Blocked_by_dep
    (derive_display_status ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

let%test "pending is default" =
  equal_display_status Pending
    (derive_display_status State.Patch_ctx.empty
       ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

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
  | Merged -> [ Term.Sgr.fg_green; Term.Sgr.bold ]
  | Needs_help -> [ Term.Sgr.fg_red; Term.Sgr.bold ]
  | Approved_idle -> [ Term.Sgr.fg_green ]
  | Approved_running -> [ Term.Sgr.fg_green; Term.Sgr.bold ]
  | Fixing_ci | Addressing_review | Resolving_conflict | Responding_to_human
  | Adding_notes ->
      [ Term.Sgr.fg_cyan; Term.Sgr.bold ]
  | Rebasing -> [ Term.Sgr.fg_yellow ]
  | Starting | Updating -> [ Term.Sgr.fg_cyan ]
  | Ci_queued | Review_queued -> [ Term.Sgr.fg_yellow ]
  | Awaiting_ci -> [ Term.Sgr.fg_blue ]
  | Awaiting_review -> [ Term.Sgr.fg_blue ]
  | Blocked_by_dep -> [ Term.Sgr.fg_blue ]
  | Pending -> [ Term.Sgr.dim ]

let status_indicator = function
  | Merged -> "✓"
  | Needs_help -> "!"
  | Approved_idle -> "✓"
  | Approved_running -> "▶"
  | Fixing_ci | Addressing_review | Resolving_conflict | Responding_to_human
  | Adding_notes ->
      "▶"
  | Rebasing -> "↻"
  | Starting | Updating -> "▶"
  | Ci_queued | Review_queued -> "◎"
  | Awaiting_ci | Awaiting_review -> "◎"
  | Blocked_by_dep -> "◎"
  | Pending -> "·"

(** {1 Status messages} *)

type prompt_info = { prompt_text : string; cursor_col : int }
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
[@@warning "-69"]

let patch_view_of_agent (agent : Patch_agent.t)
    ~(patches_by_id : Patch.t Map.M(Patch_id).t) ~(graph : Graph.t)
    ~(main_branch : Branch.t) ~(agents_by_id : Patch_agent.t Map.M(Patch_id).t)
    =
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
  let current_op = agent.Patch_agent.current_op in
  let ctx =
    ( State.Patch_ctx.empty
    |> State.Patch_ctx.set_merged ~patch_id ~value:agent.merged
    |> State.Patch_ctx.set_needs_intervention ~patch_id
         ~value:(Patch_agent.needs_intervention agent)
    |> State.Patch_ctx.set_busy ~patch_id ~value:agent.busy
    |> State.Patch_ctx.set_has_pr ~patch_id ~value:(Patch_agent.has_pr agent)
    |> State.Patch_ctx.set_approved ~patch_id
         ~value:(Patch_agent.is_approved agent ~main_branch)
    |> State.Patch_ctx.set_ci_failure_count ~patch_id
         ~count:agent.ci_failure_count
    |> fun ctx ->
      match agent.base_branch with
      | Some branch -> State.Patch_ctx.set_base_branch ctx ~patch_id ~branch
      | None -> ctx )
    |> fun ctx ->
    List.fold agent.queue ~init:ctx ~f:(fun acc kind ->
        State.Patch_ctx.set_queued acc ~patch_id ~kind ~value:true)
  in
  let status = derive_display_status ctx ~patch_id ~current_op ~main_branch in
  let dep_ids =
    List.map (Graph.deps graph patch_id) ~f:(fun dep_id ->
        let dep_status =
          match Map.find agents_by_id dep_id with
          | Some dep_agent ->
              let dep_op = dep_agent.Patch_agent.current_op in
              let dep_ctx =
                ( State.Patch_ctx.empty
                |> State.Patch_ctx.set_merged ~patch_id:dep_id
                     ~value:dep_agent.merged
                |> State.Patch_ctx.set_needs_intervention ~patch_id:dep_id
                     ~value:(Patch_agent.needs_intervention dep_agent)
                |> State.Patch_ctx.set_busy ~patch_id:dep_id
                     ~value:dep_agent.busy
                |> State.Patch_ctx.set_has_pr ~patch_id:dep_id
                     ~value:(Patch_agent.has_pr dep_agent)
                |> State.Patch_ctx.set_approved ~patch_id:dep_id
                     ~value:(Patch_agent.is_approved dep_agent ~main_branch)
                |> State.Patch_ctx.set_ci_failure_count ~patch_id:dep_id
                     ~count:dep_agent.ci_failure_count
                |> fun ctx ->
                  match dep_agent.base_branch with
                  | Some branch ->
                      State.Patch_ctx.set_base_branch ctx ~patch_id:dep_id
                        ~branch
                  | None -> ctx )
                |> fun ctx ->
                List.fold dep_agent.queue ~init:ctx ~f:(fun acc kind ->
                    State.Patch_ctx.set_queued acc ~patch_id:dep_id ~kind
                      ~value:true)
              in
              derive_display_status dep_ctx ~patch_id:dep_id ~current_op:dep_op
                ~main_branch
          | None -> Pending
        in
        (dep_id, dep_status))
  in
  {
    patch_id;
    title;
    branch;
    status;
    queue_len = List.length agent.queue;
    current_op;
    ci_failures = agent.ci_failure_count;
    dep_ids;
    has_pr = Patch_agent.has_pr agent;
    has_conflict = agent.has_conflict;
    needs_intervention = Patch_agent.needs_intervention agent;
    human_messages = List.length agent.human_messages;
    ci_checks = agent.ci_checks;
    recent_stream = [];
    pr_number = agent.pr_number;
    base_branch = agent.base_branch;
    worktree_path = agent.worktree_path;
    intervention_reason = None;
  }

(** {1 Render helpers} *)

let styled_status status text = Term.styled (status_style status) text

let render_status_badge status =
  let ind = status_indicator status in
  let lbl = label status in
  styled_status status (Printf.sprintf "%s %s" ind lbl)

(** {1 Frame rendering} *)

type frame = {
  lines : string list;
  width : int;
  detail_at_bottom : bool;
  detail_scroll_offset : int;
  patches_start_row : int;
  patches_scroll_offset : int;
  patch_count : int;
}
[@@warning "-69"]

let render_header ~project_name ~width =
  let title =
    Term.styled
      [ Term.Sgr.bold; Term.Sgr.fg_cyan ]
      (Printf.sprintf " %s " project_name)
  in
  let rule = Term.hrule width in
  [ title; rule ]

let short_op_name = function
  | Operation_kind.Ci -> "ci"
  | Operation_kind.Review_comments -> "review"
  | Operation_kind.Merge_conflict -> "conflict"
  | Operation_kind.Human -> "human"
  | Operation_kind.Implementation_notes -> "notes"
  | Operation_kind.Rebase -> "rebase"

let render_patch_row ~width ~selected (pv : patch_view) =
  let badge = render_status_badge pv.status in
  let pr_label =
    match pv.pr_number with
    | Some n -> Printf.sprintf "#%-4d " (Pr_number.to_int n)
    | None -> ""
  in
  let op_suffix =
    match pv.current_op with
    | Some op ->
        Term.styled [ Term.Sgr.dim ] (Printf.sprintf " [%s]" (short_op_name op))
    | None -> ""
  in
  let ci_info =
    if pv.ci_failures >= 3 then
      Term.styled [ Term.Sgr.fg_red ] (Printf.sprintf " CI×%d" pv.ci_failures)
    else if pv.ci_failures > 0 then
      Term.styled [ Term.Sgr.fg_yellow ]
        (Printf.sprintf " CI×%d" pv.ci_failures)
    else ""
  in
  let dep_info =
    if equal_display_status pv.status Blocked_by_dep then
      match pv.base_branch with
      | Some b ->
          Term.styled [ Term.Sgr.dim ]
            (Printf.sprintf " → %s" (Branch.to_string b))
      | None -> ""
    else ""
  in
  let cursor = if selected then "▸" else " " in
  let row =
    Term.fit_width width
      (Printf.sprintf "%s%s%s  %s%s%s%s" cursor pr_label badge pv.title
         op_suffix ci_info dep_info)
  in
  if selected then Term.styled [ Term.Sgr.bold ] row else row

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
    ( [
        Term.styled [ Term.Sgr.bold ] " Patches";
        Term.styled [ Term.Sgr.dim ]
          (Term.fit_width
             (Int.max 1 (width - 2))
             "  No patches. Press : then +N to add a PR (e.g. +123)");
      ],
      0,
      0,
      0 )
  else
    let offset, count = visible_window ~selected ~total ~max_visible in
    let section_header = Term.styled [ Term.Sgr.bold ] " Patches" in
    let vis_count = min count (total - offset) in
    let visible = List.sub views ~pos:offset ~len:vis_count in
    let rows =
      List.mapi visible ~f:(fun i pv ->
          render_patch_row ~width ~selected:(offset + i = selected) pv)
    in
    let s = { offset; total; visible = count } in
    let top, bottom = scroll_indicators s in
    let scroll_up =
      if String.is_empty top then []
      else [ Term.styled [ Term.Sgr.dim ] (" " ^ top) ]
    in
    let scroll_down =
      if String.is_empty bottom then []
      else [ Term.styled [ Term.Sgr.dim ] (" " ^ bottom) ]
    in
    let header_lines = 1 + List.length scroll_up in
    ( (section_header :: scroll_up) @ rows @ scroll_down,
      header_lines,
      offset,
      vis_count )

let render_summary (views : patch_view list) =
  let count status =
    List.count views ~f:(fun v -> equal_display_status v.status status)
  in
  let total = List.length views in
  let merged = count Merged in
  let is_running status =
    match status with
    | Fixing_ci | Addressing_review | Resolving_conflict | Responding_to_human
    | Adding_notes | Rebasing | Starting | Updating | Approved_running ->
        true
    | Merged | Needs_help | Approved_idle | Ci_queued | Review_queued
    | Awaiting_ci | Awaiting_review | Blocked_by_dep | Pending ->
        false
  in
  let running = List.count views ~f:(fun v -> is_running v.status) in
  let needs_help = count Needs_help in
  let parts =
    [
      Printf.sprintf "%d/%d merged" merged total;
      (if running > 0 then Printf.sprintf "%d running" running else "");
      (if needs_help > 0 then
         Term.styled [ Term.Sgr.fg_red ]
           (Printf.sprintf "%d need help" needs_help)
       else "");
    ]
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  Term.styled [ Term.Sgr.dim ] (" " ^ String.concat ~sep:" │ " parts)

let render_activity (entries : activity_entry list) =
  if List.is_empty entries then []
  else
    let header = Term.styled [ Term.Sgr.bold ] " Activity" in
    let lines =
      List.map entries ~f:(fun entry ->
          match entry with
          | Transition { patch_id; from_label; to_status; to_label; action } ->
              Printf.sprintf "  %s: %s → %s (%s)"
                (Term.styled [ Term.Sgr.dim ] patch_id)
                from_label
                (styled_status to_status to_label)
                (Term.styled [ Term.Sgr.dim ] action)
          | Event { patch_id; message } ->
              let prefix =
                match patch_id with
                | Some pid -> Term.styled [ Term.Sgr.dim ] (pid ^ ": ")
                | None -> ""
              in
              Printf.sprintf "  %s%s" prefix
                (Term.styled [ Term.Sgr.dim ] message))
    in
    header :: lines

(** Build the info rows for a detail view. Shared between [render_detail] and
    [detail_info_height] to keep them in sync. *)
let detail_info_rows (pv : patch_view) ~width =
  let fit_value prefix value =
    prefix ^ Term.fit_width (Int.max 1 (width - String.length prefix)) value
  in
  let header =
    Term.styled [ Term.Sgr.bold ]
      (Term.fit_width (Int.max 1 (width - 1)) (" " ^ pv.title))
  in
  let rule = Term.hrule width in
  let badge = render_status_badge pv.status in
  let lines =
    [
      header;
      rule;
      Printf.sprintf "  Status:      %s" badge;
      fit_value "  Patch ID:    " (Patch_id.to_string pv.patch_id);
      fit_value "  Branch:      " (Branch.to_string pv.branch);
      fit_value "  Base:        "
        (match pv.base_branch with
        | Some b -> Branch.to_string b
        | None -> "(not set)");
      fit_value "  Worktree:    "
        (match pv.worktree_path with Some p -> p | None -> "(none)");
      Printf.sprintf "  PR:          %s"
        (match pv.pr_number with
        | Some n -> Printf.sprintf "#%d" (Pr_number.to_int n)
        | None -> if pv.has_pr then "yes" else "no");
      Printf.sprintf "  Dependencies: %s"
        (match pv.dep_ids with
        | [] -> "none"
        | ids ->
            List.map ids ~f:(fun (id, st) ->
                styled_status st (Patch_id.to_string id))
            |> String.concat ~sep:", ");
      Printf.sprintf "  CI failures: %d" pv.ci_failures;
      Printf.sprintf "  Queue depth: %d" pv.queue_len;
      Printf.sprintf "  Conflict:    %s"
        (if pv.has_conflict then "yes" else "no");
      Printf.sprintf "  Messages:    %d queued" pv.human_messages;
    ]
  in
  let op_line =
    match pv.current_op with
    | Some op -> [ Printf.sprintf "  Current op:  %s" (short_op_name op) ]
    | None -> []
  in
  let intervention =
    if pv.needs_intervention then
      let reason_text =
        match pv.intervention_reason with
        | Some r -> sanitize_text r
        | None ->
            if pv.ci_failures >= 3 then
              Printf.sprintf "CI failed %d times in a row" pv.ci_failures
            else "Session failed unexpectedly"
      in
      let prefix = "  \xe2\x9a\xa0 Needs attention: " in
      let prefix_len = String.length prefix in
      let max_text = Int.max 10 (width - prefix_len) in
      let wrapped =
        if String.length reason_text <= max_text then [ reason_text ]
        else
          let rec wrap acc remaining =
            if String.length remaining <= max_text then
              List.rev (remaining :: acc)
            else
              let chunk = String.prefix remaining max_text in
              wrap (chunk :: acc) (String.drop_prefix remaining max_text)
          in
          wrap [] reason_text
      in
      let styled_lines =
        List.mapi wrapped ~f:(fun i line ->
            let text =
              if i = 0 then prefix ^ line else String.make prefix_len ' ' ^ line
            in
            Term.styled [ Term.Sgr.fg_red; Term.Sgr.bold ] text)
      in
      [ "" ] @ styled_lines
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
      (* Deduplicate by name, keeping the most recent result by started_at.
         ISO 8601 timestamps sort lexicographically. *)
      let deduped =
        let seen = Hashtbl.create (module String) in
        List.iter pv.ci_checks ~f:(fun (c : Ci_check.t) ->
            match Hashtbl.find seen c.name with
            | Some (prev : Ci_check.t) ->
                let newer =
                  match (c.started_at, prev.started_at) with
                  | Some a, Some b -> String.( >= ) a b
                  | Some _, None -> true
                  | None, Some _ -> false
                  | None, None -> false
                in
                if newer then Hashtbl.set seen ~key:c.name ~data:c
            | None -> Hashtbl.set seen ~key:c.name ~data:c);
        (* Preserve original order by filtering to first-seen names *)
        let emitted = Hashtbl.create (module String) in
        List.filter_map pv.ci_checks ~f:(fun (c : Ci_check.t) ->
            if Hashtbl.mem emitted c.name then None
            else (
              Hashtbl.set emitted ~key:c.name ~data:();
              Some (Hashtbl.find_exn seen c.name)))
      in
      let ci_header = [ ""; Term.styled [ Term.Sgr.bold ] "  CI Checks" ] in
      let ci_rows =
        List.map deduped ~f:(fun (c : Ci_check.t) ->
            let icon =
              if String.equal c.conclusion "success" then
                Term.styled [ Term.Sgr.fg_green ] "✓"
              else if
                List.mem failure_conclusions c.conclusion ~equal:String.equal
              then Term.styled [ Term.Sgr.fg_red ] "✗"
              else Term.styled [ Term.Sgr.fg_yellow ] "?"
            in
            Printf.sprintf "    %s %s: %s" icon c.name c.conclusion)
      in
      ci_header @ ci_rows
  in
  lines @ op_line @ intervention @ ci_section

let render_detail (pv : patch_view) ~width ~scroll ?(transcript = "") () =
  let info = detail_info_rows pv ~width in
  let transcript_content =
    if String.is_empty transcript then []
    else
      let wrap_line max_w line =
        (* Wrap on visible characters, not ANSI escapes. Use a simple
           byte-length heuristic — ANSI sequences add ~10 bytes per style
           but this is good enough for TUI display. *)
        let stripped_len = String.length (Term.strip_ansi line) in
        if stripped_len <= max_w then [ line ]
        else
          (* Fall back to raw split for long lines *)
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
      List.concat_map rendered ~f:(fun line ->
          wrap_line content_width line
          |> List.map ~f:(fun chunk -> "    " ^ chunk))
  in
  let total = List.length transcript_content in
  if total = 0 || scroll.visible = 0 then (info, [], false, 0)
  else
    (* Reserve 3 lines for header + possible top/bottom indicators *)
    let content_budget = Int.max 0 (scroll.visible - 3) in
    let s = { offset = scroll.offset; total; visible = content_budget } in
    let s = { s with offset = Int.max 0 (Int.min s.offset (scroll_max s)) } in
    let vis_count = Int.min s.visible (Int.max 0 (total - s.offset)) in
    let first = s.offset + 1 in
    let last = s.offset + vis_count in
    let transcript_hdr =
      if vis_count > 0 then
        Term.styled [ Term.Sgr.bold ]
          (Printf.sprintf "  ── Transcript (line %d–%d of %d) ──" first last
             total)
      else
        Term.styled [ Term.Sgr.bold ]
          (Printf.sprintf "  ── Transcript (%d lines) ──" total)
    in
    let remaining = scroll_max s - s.offset in
    let top_ind =
      if s.offset > 0 then
        [
          Term.styled [ Term.Sgr.dim ] (Printf.sprintf "  ↑ %d above" s.offset);
        ]
      else []
    in
    let bot_ind =
      if remaining > 0 then
        [
          Term.styled [ Term.Sgr.dim ] (Printf.sprintf "  ↓ %d below" remaining);
        ]
      else []
    in
    let visible_transcript =
      List.sub transcript_content ~pos:s.offset ~len:vis_count
    in
    let at_bottom = s.offset >= scroll_max s in
    let raw_section =
      (transcript_hdr :: top_ind) @ visible_transcript @ bot_ind
    in
    let section = List.take raw_section scroll.visible in
    (info, section, at_bottom, s.offset)

let render_timeline ~width ~scroll (entries : activity_entry list) =
  let total = List.length entries in
  let s = { offset = scroll.offset; total; visible = scroll.visible } in
  let s = { s with offset = Int.max 0 (Int.min s.offset (scroll_max s)) } in
  let vis_count = Int.min s.visible (Int.max 0 (total - s.offset)) in
  let header =
    if total = 0 then Term.styled [ Term.Sgr.bold ] " Timeline"
    else if vis_count = 0 then
      Term.styled [ Term.Sgr.bold ]
        (Printf.sprintf " ── Activity (%d entries) ──" total)
    else
      let first = s.offset + 1 in
      let last = s.offset + vis_count in
      Term.styled [ Term.Sgr.bold ]
        (Printf.sprintf " ── Activity (%d–%d of %d) ──" first last total)
  in
  let visible = List.sub entries ~pos:s.offset ~len:vis_count in
  let rows =
    List.map visible ~f:(fun entry ->
        let row =
          match entry with
          | Transition { patch_id; from_label; to_status; to_label; action } ->
              Printf.sprintf "  %s: %s → %s (%s)"
                (Term.styled [ Term.Sgr.dim ] patch_id)
                from_label
                (styled_status to_status to_label)
                (Term.styled [ Term.Sgr.dim ] action)
          | Event { patch_id; message } ->
              let prefix =
                match patch_id with
                | Some pid -> Term.styled [ Term.Sgr.dim ] (pid ^ ": ")
                | None -> "  "
              in
              Printf.sprintf "  %s%s" prefix
                (Term.styled [ Term.Sgr.dim ] message)
        in
        Term.fit_width (Int.max 1 (width - 1)) row)
  in
  let top, bottom = scroll_indicators s in
  let scroll_up =
    if String.is_empty top then []
    else [ Term.styled [ Term.Sgr.dim ] (" " ^ top) ]
  in
  let scroll_down =
    if String.is_empty bottom then []
    else [ Term.styled [ Term.Sgr.dim ] (" " ^ bottom) ]
  in
  (header :: scroll_up) @ rows @ scroll_down

let render_footer ~width ~view_mode ?prompt_line () =
  match prompt_line with
  | Some { prompt_text; cursor_col } ->
      let usable = Int.max 1 (width - 2) in
      let wrapped = Term.wrap_lines usable prompt_text in
      let cursor_line = cursor_col / usable in
      let cursor_within = cursor_col % usable in
      let with_cursor =
        List.mapi wrapped ~f:(fun i line ->
            if i <> cursor_line || cursor_within >= Term.visible_length line
            then line
            else
              let bp = Term.byte_offset_of_visible_col line cursor_within in
              let bp_next =
                Term.byte_offset_of_visible_col line (cursor_within + 1)
              in
              let before = String.sub line ~pos:0 ~len:bp in
              let ch = String.sub line ~pos:bp ~len:(bp_next - bp) in
              let after =
                String.sub line ~pos:bp_next ~len:(String.length line - bp_next)
              in
              before ^ Term.styled [ Term.Sgr.underline ] ch ^ after)
      in
      Term.hrule width :: with_cursor
  | None ->
      let help =
        match view_mode with
        | List_view ->
            Term.styled [ Term.Sgr.dim ]
              " q:quit  ↑/↓:navigate  enter:detail  +:add PR  w:worktree  \
               -:remove  h:help"
        | Detail_view _ ->
            Term.styled [ Term.Sgr.dim ]
              " q:quit  esc/backspace:back  enter:message  m:manage  \
               t:timeline  h:help"
        | Timeline_view ->
            Term.styled [ Term.Sgr.dim ]
              " q:quit  esc/backspace:back  ↑/↓:scroll  t:list  h:help"
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
          "+         Add PR (enter number)";
          "w         Register worktree (enter path)";
          "-/x       Remove selected patch";
          "t         Toggle timeline";
          "q         Quit";
        ] );
      ( "Detail View",
        [
          "↑/k       Scroll up";
          "↓/j       Scroll down";
          "PgUp      Page up (10 lines)";
          "PgDn      Page down (10 lines)";
          "Enter     Send message";
          "m         Manage patch";
          "o         Open PR in browser";
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
      ( "Prompts",
        [ "Enter     Confirm"; "Esc       Cancel"; "↑/↓       Browse history" ]
      );
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

let render_manage_overlay ~width ~height =
  let dismiss = Term.styled [ Term.Sgr.dim ] "(esc to cancel)" in
  let title =
    Term.styled
      [ Term.Sgr.bold; Term.Sgr.fg_yellow ]
      (Printf.sprintf " Manage Patch  %s" dismiss)
  in
  let items =
    [
      Term.styled [ Term.Sgr.dim ] "    m   Force mark as merged (break glass)";
    ]
  in
  let content = title :: "" :: items in
  let overlay_h = Int.max 0 (Int.min (List.length content) (height - 1)) in
  let visible = List.sub content ~pos:0 ~len:overlay_h in
  let pad_line line = if width <= 0 then "" else Term.fit_width width line in
  List.map visible ~f:pad_line

(** Number of info lines render_detail produces for a patch. Derived from
    [detail_info_rows] so the two cannot drift. Width only affects truncation,
    not line count, so we pass a dummy value. *)
let detail_info_height (pv : patch_view) =
  List.length (detail_info_rows pv ~width:80)

(** {1 Public API} *)

let views_of_orchestrator ~(orchestrator : Orchestrator.t)
    ~(gameplan : Gameplan.t) ~(activity : activity_entry list)
    ?(intervention_reasons = Map.Poly.empty) () =
  let agents = Orchestrator.all_agents orchestrator in
  let graph = Orchestrator.graph orchestrator in
  let patches_by_id =
    List.fold gameplan.patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) -> Map.set acc ~key:p.Patch.id ~data:p)
  in
  let agents_by_id =
    List.fold agents
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc (a : Patch_agent.t) -> Map.set acc ~key:a.patch_id ~data:a)
  in
  let views =
    List.map agents ~f:(fun agent ->
        let main_branch = Orchestrator.main_branch orchestrator in
        let pv =
          patch_view_of_agent agent ~patches_by_id ~graph ~main_branch
            ~agents_by_id
        in
        let pid_str = Patch_id.to_string agent.patch_id in
        let filtered =
          List.filter activity ~f:(fun entry ->
              match entry with
              | Transition { patch_id = pid; _ } -> String.equal pid pid_str
              | Event { patch_id = Some pid; _ } -> String.equal pid pid_str
              | Event { patch_id = None; _ } -> false)
        in
        let intervention_reason =
          if pv.needs_intervention then
            match Map.Poly.find intervention_reasons pv.patch_id with
            | Some _ as r -> r
            | None ->
                List.find_map filtered ~f:(function
                  | Event { message; _ } -> Some message
                  | Transition _ -> None)
          else None
        in
        { pv with recent_stream = List.take filtered 10; intervention_reason })
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

let render_frame ~width ~height ~selected ~scroll_offset ~view_mode
    ~(activity : activity_entry list) ~project_name ~show_help ~show_manage
    ?(transcript = "") ?status_msg ?prompt_line (views : patch_view list) =
  let no_patches =
    {
      lines = [];
      width;
      detail_at_bottom = false;
      detail_scroll_offset = 0;
      patches_start_row = 0;
      patches_scroll_offset = 0;
      patch_count = 0;
    }
  in
  if show_help then
    let overlay = render_help_overlay ~width ~height in
    { no_patches with lines = overlay }
  else if show_manage then
    let overlay = render_manage_overlay ~width ~height in
    { no_patches with lines = overlay; detail_scroll_offset = scroll_offset }
  else
    let header = render_header ~project_name ~width in
    let summary = [ render_summary views ] in
    let footer = render_footer ~width ~view_mode ?prompt_line () in
    let status_line =
      let rendered = render_status_msg ~width status_msg in
      if String.is_empty rendered then [] else [ rendered ]
    in
    match view_mode with
    | Detail_view patch_id -> (
        match
          List.find views ~f:(fun pv -> Patch_id.equal pv.patch_id patch_id)
        with
        | Some pv ->
            let info_h = detail_info_height pv in
            (* Chrome: header(2) + blank + summary(1) + blank + info + blank
               before footer + footer *)
            let fixed =
              2 + 1 + 1 + 1 + info_h + 1 + List.length status_line
              + List.length footer
            in
            let max_section = Int.max 0 (height - fixed) in
            let scroll =
              { offset = scroll_offset; total = 0; visible = max_section }
            in
            let info, transcript_section, at_bottom, clamped_offset =
              render_detail pv ~width ~scroll ~transcript ()
            in
            let lines =
              header @ [ "" ] @ summary @ [ "" ] @ info @ transcript_section
              @ [ "" ] @ status_line @ footer
            in
            {
              no_patches with
              lines;
              detail_at_bottom = at_bottom;
              detail_scroll_offset = clamped_offset;
            }
        | None ->
            let lines =
              header @ [ "" ] @ summary @ [ "" ] @ [ " (patch not found)" ]
              @ [ "" ] @ status_line @ footer
            in
            { no_patches with lines })
    | Timeline_view ->
        (* Budget: header(2) + blank + summary(1) + blank + "Timeline" header(1)
         + scroll indicators(2) + blank before footer + footer *)
        let reserved =
          2 + 1 + 1 + 1 + 1 + 2 + 1 + List.length status_line
          + List.length footer
        in
        let max_rows = Int.max 0 (height - reserved) in
        let scroll =
          { offset = scroll_offset; total = 0; visible = max_rows }
        in
        let timeline = render_timeline ~width ~scroll activity in
        let lines =
          header @ [ "" ] @ summary @ [ "" ] @ timeline @ [ "" ] @ status_line
          @ footer
        in
        { no_patches with lines }
    | List_view ->
        let activity_lines = render_activity activity in
        let activity_height =
          if List.is_empty activity_lines then 0
          else 1 + List.length activity_lines
        in
        (* Budget: header(2) + blank + summary(1) + blank + "Patches" header(1)
         + scroll indicators(2) + blank before footer + footer +
         activity block *)
        let reserved =
          2 + 1 + 1 + 1 + 1 + 2 + 1 + List.length status_line
          + List.length footer + activity_height
        in
        let max_patch_rows = Int.max 0 (height - reserved) in
        let patches, patch_header_lines, scroll_off, visible_rows =
          render_patches ~width ~selected ~max_visible:max_patch_rows views
        in
        let patches_start_row = 5 + patch_header_lines + 1 in
        let lines =
          header @ [ "" ] @ summary @ [ "" ] @ patches
          @ (if List.is_empty activity_lines then [] else "" :: activity_lines)
          @ [ "" ] @ status_line @ footer
        in
        {
          no_patches with
          lines;
          patches_start_row;
          patches_scroll_offset = scroll_off;
          patch_count = visible_rows;
        }

let frame_to_string (frame : frame) = String.concat ~sep:"\n" frame.lines ^ "\n"
let detail_at_bottom frame = frame.detail_at_bottom
let detail_scroll_offset frame = frame.detail_scroll_offset
let patches_start_row frame = frame.patches_start_row
let patches_scroll_offset frame = frame.patches_scroll_offset
let patch_count frame = frame.patch_count

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
  ^ "\027[?2004h" (* enable bracketed paste *) ^ Term.enable_mouse

let exit_tui () =
  Term.disable_mouse ^ "\027[?2004l" (* disable bracketed paste *)
  ^ Term.Clear.screen
  ^ Term.Cursor.move_to ~row:1 ~col:1
  ^ Term.Cursor.show
