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

(** {1 Styling} *)

let status_style = function
  | Merged -> [ Term.Sgr.fg_green; Term.Sgr.bold ]
  | Needs_help -> [ Term.Sgr.fg_red; Term.Sgr.bold ]
  | Approved_idle -> [ Term.Sgr.fg_green ]
  | Approved_running -> [ Term.Sgr.fg_green; Term.Sgr.bold ]
  | Fixing_ci | Addressing_review | Resolving_conflict | Responding_to_human ->
      [ Term.Sgr.fg_cyan; Term.Sgr.bold ]
  | Rebasing -> [ Term.Sgr.fg_yellow ]
  | Starting -> [ Term.Sgr.fg_cyan ]
  | Ci_queued | Review_queued -> [ Term.Sgr.fg_yellow ]
  | Awaiting_ci -> [ Term.Sgr.fg_blue ]
  | Awaiting_review -> [ Term.Sgr.fg_blue ]
  | Pending -> [ Term.Sgr.dim ]

let status_indicator = function
  | Merged -> "✓"
  | Needs_help -> "!"
  | Approved_idle -> "✓"
  | Approved_running -> "▶"
  | Fixing_ci | Addressing_review | Resolving_conflict | Responding_to_human ->
      "▶"
  | Rebasing -> "↻"
  | Starting -> "▶"
  | Ci_queued | Review_queued -> "◎"
  | Awaiting_ci | Awaiting_review -> "◎"
  | Pending -> "·"

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
}
[@@warning "-69"]

let patch_view_of_agent (agent : Patch_agent.t)
    ~(patches_by_id : Patch.t Map.M(Patch_id).t) ~(graph : Graph.t) =
  let patch_id = agent.patch_id in
  let patch_opt = Map.find patches_by_id patch_id in
  let title =
    match patch_opt with
    | Some p -> p.Patch.title
    | None -> Patch_id.to_string patch_id
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
  }

(** {1 Render helpers} *)

let styled_status status text = Term.styled (status_style status) text

let render_status_badge status =
  let ind = status_indicator status in
  let lbl = label status in
  styled_status status (Printf.sprintf "%s %s" ind lbl)

(** {1 Frame rendering} *)

type frame = { lines : string list; width : int } [@@warning "-69"]

let render_header ~project_name ~width =
  let title =
    Term.styled
      [ Term.Sgr.bold; Term.Sgr.fg_cyan ]
      (Printf.sprintf " %s " project_name)
  in
  let rule = Term.hrule width in
  [ title; rule ]

let render_patch_row ~width ~selected (pv : patch_view) =
  let badge = render_status_badge pv.status in
  let title_max = width - 30 in
  let title_display = Term.fit_width (max title_max 10) pv.title in
  let queue_info =
    if pv.queue_len > 0 then
      Term.styled [ Term.Sgr.dim ] (Printf.sprintf " q:%d" pv.queue_len)
    else ""
  in
  let ci_info =
    if pv.ci_failures > 0 then
      Term.styled [ Term.Sgr.fg_red ] (Printf.sprintf " ci:%d" pv.ci_failures)
    else ""
  in
  let op_info =
    match pv.current_op with
    | Some op ->
        Term.styled [ Term.Sgr.dim ]
          (Printf.sprintf " [%s]" (Operation_kind.show op))
    | None -> ""
  in
  let cursor = if selected then "▸" else " " in
  let row =
    Printf.sprintf "%s%s  %s%s%s%s" cursor badge title_display queue_info
      ci_info op_info
  in
  if selected then Term.styled [ Term.Sgr.bold; Term.Sgr.bg_256 236 ] row
  else row

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
  let offset, count = visible_window ~selected ~total ~max_visible in
  let section_header = Term.styled [ Term.Sgr.bold ] " Patches" in
  let visible = List.sub views ~pos:offset ~len:(min count (total - offset)) in
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
  let total = List.length views in
  let merged = count Merged in
  let is_running status =
    match status with
    | Fixing_ci | Addressing_review | Resolving_conflict | Responding_to_human
    | Rebasing | Starting | Approved_running ->
        true
    | Merged | Needs_help | Approved_idle | Ci_queued | Review_queued
    | Awaiting_ci | Awaiting_review | Pending ->
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

let render_detail (pv : patch_view) ~width =
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
      Printf.sprintf "  PR:          %s" (if pv.has_pr then "yes" else "no");
      Printf.sprintf "  Dependencies: %d" pv.dep_count;
      Printf.sprintf "  CI failures: %d" pv.ci_failures;
      Printf.sprintf "  Queue depth: %d" pv.queue_len;
      Printf.sprintf "  Conflict:    %s"
        (if pv.has_conflict then "yes" else "no");
      Printf.sprintf "  Comments:    %d pending" pv.pending_comments;
    ]
  in
  let op_line =
    match pv.current_op with
    | Some op -> [ Printf.sprintf "  Current op:  %s" (Operation_kind.show op) ]
    | None -> []
  in
  let intervention =
    if pv.needs_intervention then
      [
        "";
        Term.styled
          [ Term.Sgr.fg_red; Term.Sgr.bold ]
          "  ⚠ Needs manual intervention";
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
      let ci_header = [ ""; Term.styled [ Term.Sgr.bold ] "  CI Checks" ] in
      let ci_rows =
        List.map pv.ci_checks ~f:(fun (c : Ci_check.t) ->
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
  let stream_section =
    if List.is_empty pv.recent_stream then []
    else
      let stream_header =
        [ ""; Term.styled [ Term.Sgr.bold ] "  Recent Activity" ]
      in
      let stream_rows =
        List.map pv.recent_stream ~f:(fun entry ->
            match entry with
            | Event { message; _ } ->
                Printf.sprintf "    %s" (Term.styled [ Term.Sgr.dim ] message)
            | Transition { from_label; to_status; to_label; action; _ } ->
                Printf.sprintf "    %s → %s (%s)" from_label
                  (styled_status to_status to_label)
                  (Term.styled [ Term.Sgr.dim ] action))
      in
      stream_header @ stream_rows
  in
  lines @ op_line @ intervention @ ci_section @ stream_section

let render_timeline ~width ~selected ~max_visible
    (entries : activity_entry list) =
  let total = List.length entries in
  let offset, count = visible_window ~selected ~total ~max_visible in
  let header = Term.styled [ Term.Sgr.bold ] " Timeline" in
  let visible =
    List.sub entries ~pos:offset ~len:(min count (total - offset))
  in
  let rows =
    List.mapi visible ~f:(fun i entry ->
        let is_selected = offset + i = selected in
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
        let row = Term.fit_width (Int.max 1 (width - 1)) row in
        if is_selected then
          Term.styled [ Term.Sgr.bold; Term.Sgr.bg_256 236 ] row
        else row)
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
  (header :: scroll_up) @ rows @ scroll_down

let render_footer ~width ~view_mode =
  let help =
    match view_mode with
    | List_view ->
        Term.styled [ Term.Sgr.dim ]
          " q:quit  r:refresh  ↑/↓:navigate  enter:detail  t:timeline  h:help"
    | Detail_view _ ->
        Term.styled [ Term.Sgr.dim ]
          " q:quit  esc/backspace:back  r:refresh  t:timeline  h:help"
    | Timeline_view ->
        Term.styled [ Term.Sgr.dim ]
          " q:quit  esc/backspace:back  ↑/↓:scroll  t:list  h:help"
  in
  [ Term.hrule width; help ]

(** {1 Public API} *)

let views_of_orchestrator ~(orchestrator : Orchestrator.t)
    ~(gameplan : Gameplan.t) =
  let agents = Orchestrator.all_agents orchestrator in
  let graph = Orchestrator.graph orchestrator in
  let patches_by_id =
    List.fold gameplan.patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) -> Map.set acc ~key:p.Patch.id ~data:p)
  in
  List.map agents ~f:(fun agent ->
      patch_view_of_agent agent ~patches_by_id ~graph)

let render_frame ~width ~height ~selected ~view_mode
    ~(activity : activity_entry list) ~project_name (views : patch_view list) =
  let header = render_header ~project_name ~width in
  let summary = [ render_summary views ] in
  let footer = render_footer ~width ~view_mode in
  match view_mode with
  | Detail_view patch_id ->
      let detail =
        match
          List.find views ~f:(fun pv -> Patch_id.equal pv.patch_id patch_id)
        with
        | Some pv ->
            let patch_id_str = Patch_id.to_string patch_id in
            let filtered_activity =
              List.filter activity ~f:(fun entry ->
                  match entry with
                  | Transition { patch_id = pid; _ } ->
                      String.equal pid patch_id_str
                  | Event { patch_id = Some pid; _ } ->
                      String.equal pid patch_id_str
                  | Event { patch_id = None; _ } -> false)
            in
            let pv =
              { pv with recent_stream = List.take filtered_activity 10 }
            in
            render_detail pv ~width
        | None -> [ " (patch not found)" ]
      in
      (* Chrome: header(2) + blank + summary(1) + blank + blank before footer
         + footer(2) = 7 fixed lines *)
      let max_detail = Int.max 0 (height - 7) in
      let detail = List.take detail max_detail in
      let lines =
        header @ [ "" ] @ summary @ [ "" ] @ detail @ [ "" ] @ footer
      in
      { lines; width }
  | Timeline_view ->
      (* Budget: header(2) + blank + summary(1) + blank + "Timeline" header(1)
         + scroll indicators(2) + blank before footer + footer(2) = 11 *)
      let reserved = 2 + 1 + 1 + 1 + 1 + 2 + 1 + 2 in
      (* = 11 *)
      let max_rows = Int.max 0 (height - reserved) in
      let timeline =
        render_timeline ~width ~selected ~max_visible:max_rows activity
      in
      let lines =
        header @ [ "" ] @ summary @ [ "" ] @ timeline @ [ "" ] @ footer
      in
      { lines; width }
  | List_view ->
      let activity_lines = render_activity activity in
      let activity_height =
        if List.is_empty activity_lines then 0
        else 1 + List.length activity_lines
      in
      (* Budget: header(2) + blank + summary(1) + blank + "Patches" header(1)
         + scroll indicators(2) + blank before footer + footer(2) +
         activity block *)
      let reserved = 2 + 1 + 1 + 1 + 1 + 2 + 1 + 2 + activity_height in
      let max_patch_rows = Int.max 0 (height - reserved) in
      let patches =
        render_patches ~width ~selected ~max_visible:max_patch_rows views
      in
      let lines =
        header @ [ "" ] @ summary @ [ "" ] @ patches
        @ (if List.is_empty activity_lines then [] else "" :: activity_lines)
        @ [ "" ] @ footer
      in
      { lines; width }

let frame_to_string (frame : frame) = String.concat ~sep:"\n" frame.lines ^ "\n"

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
  Term.Cursor.hide ^ Term.Clear.screen ^ Term.Cursor.move_to ~row:1 ~col:1

let exit_tui () =
  Term.Clear.screen ^ Term.Cursor.move_to ~row:1 ~col:1 ^ Term.Cursor.show
