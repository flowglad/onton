(* @archlint.module shell
   @archlint.domain orchestrator *)

open Base
open Types

(** {1 Display status}

    The pure derivation lives in [Onton_core.Display_status]. The transparent
    type re-export below keeps existing call sites that spell
    [Tui.display_status] or [Tui.Merged] working unchanged. *)

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

type status_display = { label : string; color : string }
[@@deriving show, eq, sexp_of, compare]

let label = function
  | Merged -> "merged"
  | Needs_help -> "needs-help"
  | In_merge_queue -> "in-merge-queue"
  | Approved_idle -> "approved"
  | Approved_running -> "approved-running"
  | Fixing_ci -> "fixing-ci"
  | Addressing_review -> "addressing-review"
  | Addressing_findings -> "addressing-findings"
  | Resolving_conflict -> "resolving-conflict"
  | Responding_to_human -> "responding-to-human"
  | Writing_pr_body -> "writing-pr-body"
  | Rebasing -> "rebasing"
  | Starting -> "starting"
  | Updating -> "updating"
  | Ci_queued -> "ci-queued"
  | Review_queued -> "review-queued"
  | Findings_queued -> "findings-queued"
  | Awaiting_feedback -> "awaiting-feedback"
  | Blocked_by_dep -> "blocked-by-dep"
  | Pending -> "pending"

let color = function
  | Merged -> Term.Sgr.fg_green
  | Needs_help -> Term.Sgr.fg_red
  | In_merge_queue -> Term.Sgr.fg_cyan
  | Approved_idle -> Term.Sgr.fg_green
  | Approved_running -> Term.Sgr.fg_cyan
  | Fixing_ci -> Term.Sgr.fg_yellow
  | Addressing_review -> Term.Sgr.fg_yellow
  | Addressing_findings -> Term.Sgr.fg_yellow
  | Resolving_conflict -> Term.Sgr.fg_yellow
  | Responding_to_human -> Term.Sgr.fg_magenta
  | Writing_pr_body -> Term.Sgr.fg_cyan
  | Rebasing -> Term.Sgr.fg_cyan
  | Starting -> Term.Sgr.fg_cyan
  | Updating -> Term.Sgr.fg_cyan
  | Ci_queued -> Term.Sgr.fg_yellow
  | Review_queued -> Term.Sgr.fg_yellow
  | Findings_queued -> Term.Sgr.fg_yellow
  | Awaiting_feedback -> Term.Sgr.fg_blue
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
let derive_display_status = Display_status.derive

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
  | In_merge_queue -> [ Term.Sgr.fg_cyan; Term.Sgr.bold ]
  | Approved_idle -> [ Term.Sgr.fg_green ]
  | Approved_running -> [ Term.Sgr.fg_green; Term.Sgr.bold ]
  | Fixing_ci | Addressing_review | Addressing_findings | Resolving_conflict
  | Responding_to_human | Writing_pr_body ->
      [ Term.Sgr.fg_cyan; Term.Sgr.bold ]
  | Rebasing -> [ Term.Sgr.fg_yellow ]
  | Starting | Updating -> [ Term.Sgr.fg_cyan ]
  | Ci_queued | Review_queued | Findings_queued -> [ Term.Sgr.fg_yellow ]
  | Awaiting_feedback -> [ Term.Sgr.fg_blue ]
  | Blocked_by_dep -> [ Term.Sgr.fg_blue ]
  | Pending -> [ Term.Sgr.dim ]

let status_indicator = function
  | Merged -> "✓"
  | Needs_help -> "!"
  | In_merge_queue -> "⇥"
  | Approved_idle -> "✓"
  | Approved_running -> "▶"
  | Fixing_ci | Addressing_review | Addressing_findings | Resolving_conflict
  | Responding_to_human | Writing_pr_body ->
      "▶"
  | Rebasing -> "↻"
  | Starting | Updating -> "▶"
  | Ci_queued | Review_queued | Findings_queued -> "◎"
  | Awaiting_feedback -> "◎"
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
  (not (Lazy.force Term.color_enabled))
  || String.is_substring s ~substring:"\027[31m"
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
  && ((not (Lazy.force Term.color_enabled))
     || String.is_substring s ~substring:"\027[2m")

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
      timestamp : float;
    }
  | Event of { patch_id : string option; message : string; timestamp : float }
[@@warning "-37"]

(** {1 Patch view — derived per-patch rendering data} *)

type patch_view = {
  patch_id : Patch_id.t;
  title : string;
  branch : Branch.t;
  status : display_status;
  queue_len : int;
  current_op : Operation_kind.t option;
  current_op_state : Patch_agent.op_state;
      (** Carries the agent's queued/running sub-state for the current op. Used
          to render a "(queued)" suffix on busy statuses so a saturated Claude
          semaphore can be told apart from an actively running session. *)
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
      (** [true] when [pr_number] is set but the remote no longer has the PR.
          Distinguishes "we lost the PR" from "we have the PR" so the row label
          can render a distinctive marker. *)
  base_branch : Branch.t option;
  worktree_path : string option;
  intervention_reason : string option;
  automerge_enabled : bool;
  automerge_deadline : float option;
  automerge_failure_count : int;
  complexity : int option;
  backend : string;
  model : string option;
}
[@@warning "-69"]

(* Translate the authoritative [Patch_agent.intervention_reason] code into a
   human-facing, actionable banner line. The codes are the single source of
   truth for *why* a patch is stuck (see patch_agent.ml); this function is the
   single source of truth for how to *phrase* that to an operator. Returns
   [None] exactly when the agent does not need intervention.

   A reason code we don't recognise falls through to the raw code rather than a
   generic message: an unmapped real reason is still more actionable than a
   misleading one, and it flags that a new code needs a phrasing here. *)
let display_needs_intervention (agent : Patch_agent.t) =
  Patch_agent.needs_intervention agent || agent.Patch_agent.branch_blocked

let human_intervention_reason (agent : Patch_agent.t) =
  if agent.Patch_agent.branch_blocked then
    Some
      "Branch is checked out in the repo root - switch the repo root to \
       another branch before onton can manage this patch"
  else
    Option.map (Patch_agent.intervention_reason agent) ~f:(fun code ->
        match code with
        | "session_fallback=given_up" ->
            "Agent gave up after repeated session failures \xe2\x80\x94 needs \
             manual fix or restart"
        | "pr_missing" ->
            "PR is missing from the remote \xe2\x80\x94 recreate it or \
             investigate why it vanished"
        | "ci_failure_count>=3" ->
            Printf.sprintf
              "CI failed %d times in a row \xe2\x80\x94 fix the failing checks \
               below"
              agent.Patch_agent.ci_failure_count
        | "start_attempts_without_pr>=2" ->
            Printf.sprintf
              "Could not open a PR after %d attempts \xe2\x80\x94 open it \
               manually or check repo access"
              agent.Patch_agent.start_attempts_without_pr
        | "conflict_noop_count>=2" ->
            "Stuck resolving merge conflicts against base \xe2\x80\x94 resolve \
             them manually"
        | "no_commits_push_count>=2" ->
            "Sessions keep producing no commits to push \xe2\x80\x94 the task \
             may be done or mis-scoped"
        | "context_exhaustion_count>=2" ->
            "Context window exhausted repeatedly \xe2\x80\x94 split the patch \
             into smaller pieces"
        | "push_failure_count>=3" ->
            Printf.sprintf
              "git push failed %d times \xe2\x80\x94 check branch protection \
               or remote state"
              agent.Patch_agent.push_failure_count
        | "rebase_failure_count>=2" ->
            Printf.sprintf
              "git rebase failed %d times \xe2\x80\x94 check the activity log \
               for the fetch or worktree error"
              agent.Patch_agent.rebase_failure_count
        | "pr_body_artifact_miss_count>=2" ->
            "PR body delivery blocked repeatedly \xe2\x80\x94 check the PR \
             description requirements"
        | other -> other)

let patch_view_of_agent (agent : Patch_agent.t)
    ~(patches_by_id : Patch.t Map.M(Patch_id).t) ~(graph : Graph.t)
    ~(main_branch : Branch.t) ~(agents_by_id : Patch_agent.t Map.M(Patch_id).t)
    ~(resolve_routing : complexity:int option -> Backend_routing.decision) =
  let patch_id = agent.patch_id in
  let patch_opt = Map.find patches_by_id patch_id in
  let complexity = Option.bind patch_opt ~f:(fun p -> p.Patch.complexity) in
  let { Backend_routing.backend = backend_name; model } =
    resolve_routing ~complexity
  in
  let title =
    match patch_opt with
    | Some p -> p.Patch.title
    | None -> Branch.to_string agent.Patch_agent.branch
  in
  let branch =
    match patch_opt with
    | Some p -> p.Patch.branch
    | None -> agent.Patch_agent.branch
  in
  let current_op = agent.Patch_agent.current_op in
  let needs_intervention = display_needs_intervention agent in
  let ctx =
    ( State.Patch_ctx.empty
    |> State.Patch_ctx.set_merged ~patch_id ~value:agent.merged
    |> State.Patch_ctx.set_needs_intervention ~patch_id
         ~value:needs_intervention
    |> State.Patch_ctx.set_busy ~patch_id ~value:agent.busy
    |> State.Patch_ctx.set_has_pr ~patch_id ~value:(Patch_agent.has_pr agent)
    |> State.Patch_ctx.set_approved ~patch_id
         ~value:(Patch_agent.is_approved agent ~main_branch)
    |> State.Patch_ctx.set_enqueued ~patch_id
         ~value:(Option.is_some agent.merge_queue_entry)
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
              let dep_needs_intervention =
                display_needs_intervention dep_agent
              in
              let dep_ctx =
                ( State.Patch_ctx.empty
                |> State.Patch_ctx.set_merged ~patch_id:dep_id
                     ~value:dep_agent.merged
                |> State.Patch_ctx.set_needs_intervention ~patch_id:dep_id
                     ~value:dep_needs_intervention
                |> State.Patch_ctx.set_busy ~patch_id:dep_id
                     ~value:dep_agent.busy
                |> State.Patch_ctx.set_has_pr ~patch_id:dep_id
                     ~value:(Patch_agent.has_pr dep_agent)
                |> State.Patch_ctx.set_approved ~patch_id:dep_id
                     ~value:(Patch_agent.is_approved dep_agent ~main_branch)
                |> State.Patch_ctx.set_enqueued ~patch_id:dep_id
                     ~value:(Option.is_some dep_agent.merge_queue_entry)
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
    current_op_state = agent.Patch_agent.current_op_state;
    ci_failures = agent.ci_failure_count;
    dep_ids;
    has_pr = Patch_agent.has_pr agent;
    has_conflict = agent.has_conflict;
    needs_intervention;
    human_messages = List.length agent.human_messages;
    ci_checks = agent.ci_checks;
    recent_stream = [];
    pr_number = Patch_agent.pr_number agent;
    merge_queue_entry = agent.Patch_agent.merge_queue_entry;
    pr_missing = Patch_agent.is_pr_missing agent;
    base_branch = agent.base_branch;
    worktree_path = agent.worktree_path;
    intervention_reason = human_intervention_reason agent;
    automerge_enabled = agent.automerge_enabled;
    automerge_deadline = agent.automerge_deadline;
    automerge_failure_count = agent.automerge_failure_count;
    complexity;
    backend = backend_name;
    model;
  }

(** {1 Render helpers} *)

let styled_status status text = Term.styled (status_style status) text

let render_badge ~style ~indicator ~label =
  Term.styled style (Printf.sprintf "%s %s" indicator label)

let render_status_badge ?(queued = false) status =
  let lbl = label status ^ if queued then " (queued)" else "" in
  render_badge ~style:(status_style status) ~indicator:(status_indicator status)
    ~label:lbl

(* The primary status column owns "in merge queue" ([In_merge_queue]); the badge
   only conveys the GitHub sub-state + queue position as detail, rendered in the
   In_merge_queue style. It no longer borrows other statuses' colors. *)
let merge_queue_badge_label = function
  | Pr_state.Mq_queued -> "mq-queued"
  | Pr_state.Mq_awaiting_checks -> "mq-awaiting-checks"
  | Pr_state.Mq_mergeable -> "mq-mergeable"
  | Pr_state.Mq_unmergeable -> "mq-unmergeable"
  | Pr_state.Mq_locked -> "mq-locked"

let render_merge_queue_badge = function
  | None -> ""
  | Some (entry : Pr_state.merge_queue_entry) ->
      render_badge
        ~style:(status_style In_merge_queue)
        ~indicator:(status_indicator In_merge_queue)
        ~label:
          (Printf.sprintf "%s #%d"
             (merge_queue_badge_label entry.state)
             entry.position)

let is_running_status = function
  | Fixing_ci | Addressing_review | Addressing_findings | Resolving_conflict
  | Responding_to_human | Writing_pr_body | Rebasing | Starting | Updating
  | Approved_running ->
      true
  | Merged | Needs_help | In_merge_queue | Approved_idle | Ci_queued
  | Review_queued | Findings_queued | Awaiting_feedback | Blocked_by_dep
  | Pending ->
      false

(** {1 Frame rendering} *)

type frame = {
  lines : string list;
  width : int;
  detail_at_bottom : bool;
  detail_scroll_offset : int;
  checks_scroll_offset : int;
  patches_start_row : int;
  patches_scroll_offset : int;
  patch_count : int;
}
[@@warning "-69"]

(** Build the header row: project name on the left, backend on the right.

    Both segments are truncated when [width] is constrained. The backend segment
    is sacrificed first (collapsed to empty) so the project name is always
    visible — a saturated TUI still tells the user which session they are
    looking at. *)
let render_header ~project_name ~backend_name ~width =
  let title_raw = Printf.sprintf " %s " project_name in
  let backend_raw =
    if String.is_empty backend_name then ""
    else Printf.sprintf " %s " backend_name
  in
  let title_w = Term.visible_length title_raw in
  let backend_w = Term.visible_length backend_raw in
  let style_title s = Term.styled [ Term.Sgr.bold; Term.Sgr.fg_cyan ] s in
  let style_backend s = Term.styled [ Term.Sgr.dim ] s in
  let header_line =
    if width <= 0 then ""
    else if title_w + backend_w + 1 <= width then
      let pad = width - title_w - backend_w in
      style_title title_raw ^ String.make pad ' ' ^ style_backend backend_raw
    else if title_w <= width then
      (* Backend doesn't fit — keep the project name and pad to width. *)
      style_title title_raw ^ String.make (width - title_w) ' '
    else
      (* Project name itself overflows — truncate it. *)
      style_title (Term.fit_width width title_raw)
  in
  let rule = Term.hrule width in
  [ header_line; rule ]

let short_op_name = function
  | Operation_kind.Ci -> "ci"
  | Operation_kind.Review_comments -> "review"
  | Operation_kind.Findings -> "findings"
  | Operation_kind.Merge_conflict -> "conflict"
  | Operation_kind.Human -> "human"
  | Operation_kind.Pr_body -> "pr-body"
  | Operation_kind.Rebase -> "rebase"

let pv_queued (pv : patch_view) =
  is_running_status pv.status
  && Patch_agent.equal_op_state pv.current_op_state Patch_agent.Queued

(** Compact automerge indicator for the overview row. Empty when automerge is
    disabled; otherwise a short colored badge describing the wait state. The
    longer-form prose lives in [detail_info_rows]. *)
let automerge_inline_info ~now (pv : patch_view) =
  if not pv.automerge_enabled then ""
  else if pv.automerge_failure_count >= Patch_controller.automerge_max_failures
  then Term.styled [ Term.Sgr.fg_red ] " AM✗"
  else
    match pv.automerge_deadline with
    | None -> Term.styled [ Term.Sgr.dim ] " AM⋯"
    | Some d ->
        let remaining = d -. now in
        if Float.( > ) remaining 0.0 then
          Term.styled [ Term.Sgr.fg_green ]
            (Printf.sprintf " AM %.0fs" (Float.max 1.0 remaining))
        else Term.styled [ Term.Sgr.fg_green ] " AM↻"

let render_patch_row ~width ~selected ~now (pv : patch_view) =
  let badge = render_status_badge ~queued:(pv_queued pv) pv.status in
  let queue_badge = render_merge_queue_badge pv.merge_queue_entry in
  let queue_badge =
    if String.is_empty queue_badge then "" else " " ^ queue_badge
  in
  let patch_label =
    let id_str = Patch_id.to_string pv.patch_id in
    let id_short =
      if String.length id_str > 4 then String.sub id_str ~pos:0 ~len:4
      else id_str
    in
    Printf.sprintf "Patch %-4s" id_short
  in
  let pr_label =
    match pv.pr_number with
    | Some n when pv.pr_missing ->
        (* Vanished from remote — flag the number with a leading ! so the
           operator can see which PR was lost. *)
        Printf.sprintf "!%-5d" (Pr_number.to_int n)
    | Some n -> Printf.sprintf "#%-5d" (Pr_number.to_int n)
    | None -> "  --  "
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
  let am_info = automerge_inline_info ~now pv in
  let cursor = if selected then "▸" else " " in
  let row =
    Term.fit_width width
      (Printf.sprintf "%s%s %s %s%s  %s%s%s%s" cursor patch_label pr_label badge
         queue_badge pv.title ci_info dep_info am_info)
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

let render_patches ~width ~selected ~max_visible ~now (views : patch_view list)
    =
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
          render_patch_row ~width ~selected:(offset + i = selected) ~now pv)
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

let format_activity_ts ts =
  match try Some (Unix.localtime ts) with _ -> None with
  | None -> "--:--:--"
  | Some tm ->
      Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min
        tm.Unix.tm_sec

let render_activity (entries : activity_entry list) =
  if List.is_empty entries then []
  else
    let header = Term.styled [ Term.Sgr.bold ] " Activity" in
    let lines =
      List.map entries ~f:(fun entry ->
          match entry with
          | Transition
              { patch_id; from_label; to_status; to_label; action; timestamp }
            ->
              Printf.sprintf "  %s  %s: %s → %s (%s)"
                (Term.styled [ Term.Sgr.dim ] (format_activity_ts timestamp))
                (Term.styled [ Term.Sgr.dim ] patch_id)
                from_label
                (styled_status to_status to_label)
                (Term.styled [ Term.Sgr.dim ] action)
          | Event { patch_id; message; timestamp } ->
              let prefix =
                match patch_id with
                | Some pid -> Term.styled [ Term.Sgr.dim ] (pid ^ ": ")
                | None -> ""
              in
              Printf.sprintf "  %s  %s%s"
                (Term.styled [ Term.Sgr.dim ] (format_activity_ts timestamp))
                prefix
                (Term.styled [ Term.Sgr.dim ] message))
    in
    header :: lines

(** Deduplicate CI checks by name, keeping the most recent result by
    [started_at]. ISO 8601 timestamps sort lexicographically. Original
    first-seen order is preserved. Shared between the inline detail info rows
    and the scrollable checks overlay so both agree on what to show. *)
let dedup_ci_checks (checks : Ci_check.t list) =
  let seen = Hashtbl.create (module String) in
  List.iter checks ~f:(fun (c : Ci_check.t) ->
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
  let emitted = Hashtbl.create (module String) in
  List.filter_map checks ~f:(fun (c : Ci_check.t) ->
      if Hashtbl.mem emitted c.name then None
      else (
        Hashtbl.set emitted ~key:c.name ~data:();
        Some (Hashtbl.find_exn seen c.name)))

(** Render a single CI check as an icon + name + conclusion, with [indent]
    leading spaces. Shared between the inline detail rows and the checks
    overlay. *)
let render_ci_check_row ~indent (c : Ci_check.t) =
  let failure_conclusions = Patch_decision.failure_conclusions in
  let icon =
    if String.equal c.conclusion "success" then
      Term.styled [ Term.Sgr.fg_green ] "✓"
    else if List.mem failure_conclusions c.conclusion ~equal:String.equal then
      Term.styled [ Term.Sgr.fg_red ] "✗"
    else Term.styled [ Term.Sgr.fg_yellow ] "?"
  in
  Printf.sprintf "%s%s %s: %s" indent icon c.name c.conclusion

(** Maximum number of CI checks shown inline in the detail info section. Beyond
    this, an ellipsis row points at the scrollable checks overlay so the checks
    cannot crowd the transcript and metadata off the screen. Kept a fixed
    constant (not height-derived) so [detail_info_height] stays a pure function
    of the check count. *)
let max_inline_ci_checks = 8

(** Build the info rows for a detail view. Shared between [render_detail] and
    [detail_info_height] to keep them in sync. [~now] is the wall-clock time the
    frame is rendered against — threaded in rather than read from
    [Unix.gettimeofday] so the function stays pure and testable. *)
let detail_info_rows (pv : patch_view) ~width ~now =
  let fit_value prefix value =
    prefix ^ Term.fit_width (Int.max 1 (width - String.length prefix)) value
  in
  let header =
    Term.styled [ Term.Sgr.bold ]
      (Term.fit_width (Int.max 1 (width - 1)) (" " ^ pv.title))
  in
  let rule = Term.hrule width in
  let badge = render_status_badge ~queued:(pv_queued pv) pv.status in
  let queue_badge = render_merge_queue_badge pv.merge_queue_entry in
  let status_line =
    if String.is_empty queue_badge then Printf.sprintf "  Status:      %s" badge
    else Printf.sprintf "  Status:      %s %s" badge queue_badge
  in
  let lines =
    [
      header;
      rule;
      status_line;
      fit_value "  Patch ID:    " (Patch_id.to_string pv.patch_id);
      fit_value "  Branch:      " (Branch.to_string pv.branch);
      fit_value "  Base:        "
        (match pv.base_branch with
        | Some b -> Branch.to_string b
        | None -> "(not set)");
      fit_value "  Worktree:    "
        (match pv.worktree_path with Some p -> p | None -> "(none)");
      Printf.sprintf "  Complexity:  %s"
        (match pv.complexity with Some n -> Int.to_string n | None -> "—");
      fit_value "  Backend:     " pv.backend;
      fit_value "  Model:       "
        (match pv.model with Some m -> m | None -> "(backend default)");
      Printf.sprintf "  PR:          %s"
        (match pv.pr_number with
        | Some n when pv.pr_missing ->
            Printf.sprintf "#%d (vanished from remote)" (Pr_number.to_int n)
        | Some n -> Printf.sprintf "#%d" (Pr_number.to_int n)
        | None -> "no");
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
      Printf.sprintf "  Automerge:   %s"
        (if not pv.automerge_enabled then "disabled"
         else if
           pv.automerge_failure_count >= Patch_controller.automerge_max_failures
         then "enabled (failure cap reached — toggle off/on to retry)"
         else
           match pv.automerge_deadline with
           | None ->
               if pv.queue_len > 0 then "enabled (waiting: queue must drain)"
               else "enabled (waiting: needs approval and passing CI)"
           | Some d ->
               let remaining = d -. now in
               if Float.( > ) remaining 0.0 then
                 (* Floor at 1s so sub-second remainders don't render as
                    "fires in 0s" — transient but cosmetically odd. *)
                 Printf.sprintf "fires in %.0fs" (Float.max 1.0 remaining)
               else "firing...");
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
      let deduped = dedup_ci_checks pv.ci_checks in
      let total = List.length deduped in
      let shown = List.take deduped max_inline_ci_checks in
      let ci_header = [ ""; Term.styled [ Term.Sgr.bold ] "  CI Checks" ] in
      let ci_rows = List.map shown ~f:(render_ci_check_row ~indent:"    ") in
      let overflow =
        if total > max_inline_ci_checks then
          [
            Term.styled [ Term.Sgr.dim ]
              (Printf.sprintf "    … %d more — press c to view all"
                 (total - max_inline_ci_checks));
          ]
        else []
      in
      ci_header @ ci_rows @ overflow
  in
  lines @ op_line @ intervention @ ci_section

let render_detail (pv : patch_view) ~width ~scroll ~now ?(transcript = "") () =
  let info = detail_info_rows pv ~width ~now in
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
          | Transition { patch_id; from_label; to_status; to_label; action; _ }
            ->
              Printf.sprintf "  %s: %s → %s (%s)"
                (Term.styled [ Term.Sgr.dim ] patch_id)
                from_label
                (styled_status to_status to_label)
                (Term.styled [ Term.Sgr.dim ] action)
          | Event { patch_id; message; _ } ->
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
               -:remove  m:manage  o:open browser  h:help"
        | Detail_view _ ->
            Term.styled [ Term.Sgr.dim ]
              " q:quit  esc/backspace:back  enter:message  c:checks  m:manage  \
               o:open browser  t:timeline  h:help"
        | Timeline_view ->
            Term.styled [ Term.Sgr.dim ]
              " q:quit  esc/backspace:back  ↑/↓:scroll  t:list  h:help"
      in
      [ Term.hrule width; help ]

let render_help_overlay ~width ~height ~version =
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
          "m         Manage patch";
          "o         Open PR in browser";
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
          "c         View all CI checks";
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
      (Printf.sprintf " Keyboard Shortcuts  onton %s  %s" version dismiss)
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

let render_manage_overlay ~width ~height ~automerge_enabled ~needs_intervention
    =
  let dismiss = Term.styled [ Term.Sgr.dim ] "(esc to cancel)" in
  let title =
    Term.styled
      [ Term.Sgr.bold; Term.Sgr.fg_yellow ]
      (Printf.sprintf " Manage Patch  %s" dismiss)
  in
  let automerge_label =
    Printf.sprintf "    a   %s automerge (5-minute idle timer after approval)"
      (if automerge_enabled then "Disable" else "Enable")
  in
  let automerge_item =
    (* Highlight in green when automerge is currently on so the user can tell
       at a glance, not just by reading the verb. *)
    if automerge_enabled then
      Term.styled [ Term.Sgr.bold; Term.Sgr.fg_green ] automerge_label
    else Term.styled [ Term.Sgr.dim ] automerge_label
  in
  let bump_item =
    if needs_intervention then
      Some
        (Term.styled [ Term.Sgr.dim ]
           "    b   Bump (clear intervention, continue patch loop)")
    else None
  in
  let items =
    [
      Term.styled [ Term.Sgr.dim ] "    m   Force mark as merged (break glass)";
      automerge_item;
    ]
    @ Option.to_list bump_item
    @ [
        Term.styled [ Term.Sgr.dim ]
          "    p   Add patch to gameplan (description + dependencies)";
      ]
  in
  let content = title :: "" :: items in
  let overlay_h = Int.max 0 (Int.min (List.length content) (height - 1)) in
  let visible = List.sub content ~pos:0 ~len:overlay_h in
  let pad_line line = if width <= 0 then "" else Term.fit_width width line in
  List.map visible ~f:pad_line

(** Multi-select dependency picker for the add-patch flow. Lists every existing
    patch with a checkbox; [cursor] is the highlighted row and [chosen] the
    toggled-on ids. Rows are windowed to fit [height] around the cursor so long
    gameplans stay navigable. *)
let render_deps_overlay ~width ~height ~(views : patch_view list) ~cursor
    ~(chosen : Patch_id.t list) =
  let dismiss =
    Term.styled [ Term.Sgr.dim ]
      "(↑/↓ move · space toggle · enter confirm · esc cancel)"
  in
  let title =
    Term.styled
      [ Term.Sgr.bold; Term.Sgr.fg_yellow ]
      (Printf.sprintf " Select Dependencies  %s" dismiss)
  in
  let is_chosen pid = List.mem chosen pid ~equal:Patch_id.equal in
  let body =
    if List.is_empty views then
      [ Term.styled [ Term.Sgr.dim ] "    (no existing patches to depend on)" ]
    else
      (* Window the rows around the cursor. Reserve the title + a blank line. *)
      let total = List.length views in
      let visible = Int.max 1 (height - 2) in
      let cursor = Int.max 0 (Int.min cursor (total - 1)) in
      let start =
        if total <= visible then 0
        else Int.max 0 (Int.min (cursor - (visible / 2)) (total - visible))
      in
      let window =
        List.sub views ~pos:start ~len:(Int.min visible (total - start))
      in
      List.mapi window ~f:(fun i pv ->
          let idx = start + i in
          let mark = if is_chosen pv.patch_id then "[x]" else "[ ]" in
          let pointer = if idx = cursor then "▸" else " " in
          let label =
            Printf.sprintf "%s %s %s  %s" pointer mark
              (Patch_id.to_string pv.patch_id)
              pv.title
          in
          let line = if width <= 0 then label else Term.fit_width width label in
          if idx = cursor then Term.styled [ Term.Sgr.reverse ] line
          else if is_chosen pv.patch_id then
            Term.styled [ Term.Sgr.fg_green ] line
          else line)
  in
  let content = title :: "" :: body in
  let overlay_h = Int.max 0 (Int.min (List.length content) height) in
  let visible = List.sub content ~pos:0 ~len:overlay_h in
  let pad_line line = if width <= 0 then "" else Term.fit_width width line in
  List.map visible ~f:pad_line

(** Full-screen, scrollable list of a patch's CI checks. Opened by hotkey from
    the detail view when the inline section is capped. Returns the rendered
    lines and the clamped scroll offset (written back to [checks_scroll] so
    delta-based input stays in range, mirroring [render_detail]). Only one
    scroll offset is ever live at a time: while this overlay is up the detail
    transcript scroll is dormant. *)
let render_checks_overlay ~width ~height ~scroll_offset (pv : patch_view) =
  let deduped = dedup_ci_checks pv.ci_checks in
  let total = List.length deduped in
  let dismiss = Term.styled [ Term.Sgr.dim ] "(↑/↓ scroll · esc to close)" in
  (* Reserve the title plus a possible top and bottom indicator. *)
  let content_visible = Int.max 0 (height - 3) in
  let max_off =
    if total > content_visible then total - content_visible else 0
  in
  let offset = Int.max 0 (Int.min scroll_offset max_off) in
  let vis_count = Int.min content_visible (Int.max 0 (total - offset)) in
  let title =
    let label =
      if total = 0 then " CI Checks"
      else if vis_count = 0 then Printf.sprintf " CI Checks (– of %d)" total
      else
        Printf.sprintf " CI Checks (%d–%d of %d)" (offset + 1)
          (offset + vis_count) total
    in
    Term.styled
      [ Term.Sgr.bold; Term.Sgr.fg_cyan ]
      (Printf.sprintf "%s  %s" label dismiss)
  in
  let top_ind =
    if offset > 0 then
      [ Term.styled [ Term.Sgr.dim ] (Printf.sprintf "  ↑ %d above" offset) ]
    else []
  in
  let bot_ind =
    let below = max_off - offset in
    if below > 0 then
      [ Term.styled [ Term.Sgr.dim ] (Printf.sprintf "  ↓ %d below" below) ]
    else []
  in
  let rows =
    if total = 0 then
      [ Term.styled [ Term.Sgr.dim ] "  (no CI checks reported)" ]
    else
      List.sub deduped ~pos:offset ~len:vis_count
      |> List.map ~f:(render_ci_check_row ~indent:"  ")
  in
  let content = (title :: top_ind) @ rows @ bot_ind in
  let overlay_h = Int.max 0 (Int.min (List.length content) (height - 1)) in
  let visible = List.sub content ~pos:0 ~len:overlay_h in
  let pad_line line = if width <= 0 then "" else Term.fit_width width line in
  (List.map visible ~f:pad_line, offset)

(** Number of info lines render_detail produces for a patch. Derived from
    [detail_info_rows] so the two cannot drift. Width only affects truncation,
    not line count, and [~now] only affects the automerge countdown text (every
    possible value fits on one line — "fires in Ns", "firing...", "enabled
    (...)", "disabled"), so fixed dummy values here are safe.

    Invariant: automerge status strings must stay well under 70 chars so they
    cannot wrap at the dummy [~width:80] used here. If a future string is long
    enough to wrap, thread [~now] and the real terminal width through
    [detail_info_height] instead of passing placeholders. *)
let detail_info_height (pv : patch_view) =
  List.length (detail_info_rows pv ~width:80 ~now:0.0)

(** {1 Public API} *)

let views_of_orchestrator ~(orchestrator : Orchestrator.t)
    ~(gameplan : Gameplan.t) ~(activity : activity_entry list)
    ~(resolve_routing : complexity:int option -> Backend_routing.decision)
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
            ~agents_by_id ~resolve_routing
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
            (* Prefer the authoritative reason derived from the agent's own
               failure counters (set in [patch_view_of_agent]). The activity-log
               map and recent stream are only deep fallbacks: scraping the most
               recent event surfaces whatever happened last (e.g. "pushed after
               session"), which is rarely the reason the patch is stuck. *)
            match pv.intervention_reason with
            | Some _ as r -> r
            | None -> (
                match Map.Poly.find intervention_reasons pv.patch_id with
                | Some _ as r -> r
                | None ->
                    List.find_map filtered ~f:(function
                      | Event { message; _ } -> Some message
                      | Transition _ -> None))
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
    ~(activity : activity_entry list) ~project_name ~backend_name ~version
    ~show_help ~show_checks ~checks_scroll ~show_manage ~now ?(transcript = "")
    ?status_msg ?prompt_line ?dep_select (views : patch_view list) =
  let no_patches =
    {
      lines = [];
      width;
      detail_at_bottom = false;
      detail_scroll_offset = 0;
      checks_scroll_offset = checks_scroll;
      patches_start_row = 0;
      patches_scroll_offset = 0;
      patch_count = 0;
    }
  in
  (* The patch the overlays (manage, checks) act on: the focused patch in detail
     view, or the selected row in list view. *)
  let overlay_target_pv () =
    match view_mode with
    | Detail_view patch_id ->
        List.find views ~f:(fun pv -> Patch_id.equal pv.patch_id patch_id)
    | List_view ->
        let count = List.length views in
        if count = 0 then None
        else
          let idx = Int.max 0 (Int.min selected (count - 1)) in
          List.nth views idx
    | Timeline_view -> None
  in
  if show_help then
    let overlay = render_help_overlay ~width ~height ~version in
    { no_patches with lines = overlay }
  else if show_checks then
    match overlay_target_pv () with
    | Some pv ->
        let overlay, clamped =
          render_checks_overlay ~width ~height ~scroll_offset:checks_scroll pv
        in
        { no_patches with lines = overlay; checks_scroll_offset = clamped }
    | None ->
        (* No patch in focus — nothing to show. Render a dismissable notice
           rather than dereferencing an absent patch. *)
        let line =
          Term.styled
            [ Term.Sgr.bold; Term.Sgr.fg_cyan ]
            " CI Checks  (no patch selected — esc to close)"
        in
        let pad = if width <= 0 then "" else Term.fit_width width line in
        { no_patches with lines = [ pad ] }
  else if show_manage then
    let target_pv = overlay_target_pv () in
    let automerge_enabled =
      Option.value_map target_pv ~default:false ~f:(fun pv ->
          pv.automerge_enabled)
    in
    let needs_intervention =
      Option.value_map target_pv ~default:false ~f:(fun pv ->
          pv.needs_intervention)
    in
    let overlay =
      render_manage_overlay ~width ~height ~automerge_enabled
        ~needs_intervention
    in
    { no_patches with lines = overlay; detail_scroll_offset = scroll_offset }
  else
    match dep_select with
    | Some (cursor, chosen) ->
        let overlay =
          render_deps_overlay ~width ~height ~views ~cursor ~chosen
        in
        {
          no_patches with
          lines = overlay;
          detail_scroll_offset = scroll_offset;
        }
    | None -> (
        let header = render_header ~project_name ~backend_name ~width in
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
                (* Chrome: header(2) + blank + info + blank before footer + status +
               footer *)
                let fixed =
                  2 + 1 + info_h + 1 + List.length status_line
                  + List.length footer
                in
                let max_section = Int.max 0 (height - fixed) in
                let scroll =
                  { offset = scroll_offset; total = 0; visible = max_section }
                in
                let info, transcript_section, at_bottom, clamped_offset =
                  render_detail pv ~width ~scroll ~now ~transcript ()
                in
                let lines =
                  header @ [ "" ] @ info @ transcript_section @ [ "" ]
                  @ status_line @ footer
                in
                {
                  no_patches with
                  lines;
                  detail_at_bottom = at_bottom;
                  detail_scroll_offset = clamped_offset;
                }
            | None ->
                let lines =
                  header @ [ "" ] @ [ " (patch not found)" ] @ [ "" ]
                  @ status_line @ footer
                in
                { no_patches with lines })
        | Timeline_view ->
            (* Budget: header(2) + blank + "Timeline" header(1) + scroll
           indicators(2) + blank before footer + status + footer *)
            let reserved =
              2 + 1 + 1 + 2 + 1 + List.length status_line + List.length footer
            in
            let max_rows = Int.max 0 (height - reserved) in
            let scroll =
              { offset = scroll_offset; total = 0; visible = max_rows }
            in
            let timeline = render_timeline ~width ~scroll activity in
            let lines =
              header @ [ "" ] @ timeline @ [ "" ] @ status_line @ footer
            in
            { no_patches with lines }
        | List_view ->
            (* Patches get priority. Reserve only the non-patch, non-activity
           chrome: header(2) + blank after header + blank before footer +
           status + footer. Scroll indicators are reserved only when patches
           actually overflow, so the rows they would have taken go to
           activity when the list fits. *)
            let chrome_reserved =
              2 + 1 + 1 + List.length status_line + List.length footer
            in
            let patches_section_max = Int.max 0 (height - chrome_reserved) in
            let total_views = List.length views in
            let max_patch_rows =
              let without_indicators = Int.max 0 (patches_section_max - 1) in
              if total_views <= without_indicators then without_indicators
              else Int.max 0 (patches_section_max - 3)
            in
            let patches, patch_header_lines, scroll_off, visible_rows =
              render_patches ~width ~selected ~max_visible:max_patch_rows ~now
                views
            in
            let patches_start_row = 3 + patch_header_lines + 1 in
            let lines_before_activity =
              2 (* header *) + 1 (* blank *) + List.length patches
            in
            let lines_after_activity =
              1 (* blank before footer *) + List.length status_line
              + List.length footer
            in
            (* [activity_budget] is the total space available for the activity
           block, which when rendered is: 1 blank separator + render_activity
           output ("Activity" header + one line per entry). Need ≥ 3 to fit
           separator + header + 1 entry. The separator is folded into
           [activity_lines] itself so the budget accounting is local: the
           sole invariant is [List.length activity_lines ≤ activity_budget],
           and there is no separate prepend at the concat site that could
           drift from the budget. *)
            let activity_budget =
              height - lines_before_activity - lines_after_activity
            in
            let activity_lines =
              if List.is_empty activity || activity_budget < 3 then []
              else
                let raw = render_activity activity in
                let max_raw = activity_budget - 1 in
                "" :: List.take raw (Int.min (List.length raw) max_raw)
            in
            let lines =
              header @ [ "" ] @ patches @ activity_lines @ [ "" ] @ status_line
              @ footer
            in
            {
              no_patches with
              lines;
              patches_start_row;
              patches_scroll_offset = scroll_off;
              patch_count = visible_rows;
            })

let frame_to_string (frame : frame) = String.concat ~sep:"\n" frame.lines ^ "\n"
let detail_at_bottom frame = frame.detail_at_bottom
let detail_scroll_offset frame = frame.detail_scroll_offset
let checks_scroll_offset frame = frame.checks_scroll_offset
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
