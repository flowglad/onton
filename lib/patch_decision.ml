open Base
open Types

(** Pure decision functions for patch agents.

    Mirrors the Elixir [Anton.PatchDecision] module: given the current agent
    state, produce a decision value without performing I/O. The runner/poller
    calls these functions and acts on the results. *)

type disposition =
  | Skip  (** Patch is merged or removed — nothing to do. *)
  | Blocked  (** Patch needs intervention — waiting for human. *)
  | Busy  (** Patch is already executing — queue any new work. *)
  | Idle  (** Patch is idle with no queued work. *)
  | Ready_start  (** Patch is ready to start (no PR yet). *)
  | Ready_respond of Operation_kind.t
      (** Patch has queued feedback to address. *)
  | Ready_rebase  (** Patch has a queued rebase as highest priority. *)
[@@deriving show, eq, sexp_of, compare]

let disposition (a : Patch_agent.t) : disposition =
  if a.merged then Skip
  else if Patch_agent.needs_intervention a then Blocked
  else if a.busy then Busy
  else if not (Patch_agent.has_pr a) then Ready_start
  else
    match Patch_agent.highest_priority a with
    | None -> Idle
    | Some k -> (
        match k with
        | Operation_kind.Rebase -> Ready_rebase
        | Operation_kind.Human | Operation_kind.Merge_conflict
        | Operation_kind.Ci | Operation_kind.Review_comments
        | Operation_kind.Implementation_notes ->
            Ready_respond k)

type ci_decision =
  | Enqueue_ci  (** CI failure count below cap — enqueue Ci feedback. *)
  | Ci_already_queued  (** Ci already in queue — no action needed. *)
  | Ci_fix_in_progress
      (** Agent is already fixing CI — suppress until checks pass. *)
  | Cap_reached  (** CI failure count >= 3 — do not enqueue, flag. *)
[@@deriving show, eq, sexp_of, compare]

let on_ci_failure (a : Patch_agent.t) : ci_decision =
  if a.ci_failure_count >= 3 then Cap_reached
  else if
    a.busy
    && Option.equal Operation_kind.equal a.current_op (Some Operation_kind.Ci)
  then Ci_fix_in_progress
  else if List.mem a.queue Operation_kind.Ci ~equal:Operation_kind.equal then
    Ci_already_queued
  else Enqueue_ci

type human_decision =
  | Enqueue_human  (** Queue human feedback for processing. *)
  | Already_queued  (** Human feedback already in queue. *)
[@@deriving show, eq, sexp_of, compare]

let on_human_message (a : Patch_agent.t) : human_decision =
  if List.mem a.queue Operation_kind.Human ~equal:Operation_kind.equal then
    Already_queued
  else Enqueue_human

type conflict_decision =
  | Enqueue_conflict  (** Queue merge conflict resolution. *)
  | Already_conflicting  (** Conflict already tracked. *)
[@@deriving show, eq, sexp_of, compare]

let on_merge_conflict (a : Patch_agent.t) : conflict_decision =
  if a.has_conflict then Already_conflicting else Enqueue_conflict

type checks_passing_decision =
  | Reset_ci_failure_count
      (** CI checks now pass after prior failures — reset the counter. *)
  | No_ci_reset
      (** No reset needed (no prior failures, or checks not passing). *)
[@@deriving show, eq, sexp_of, compare]

let on_checks_passing (a : Patch_agent.t) ~(checks_passing : bool) :
    checks_passing_decision =
  if a.ci_failure_count > 0 && checks_passing then Reset_ci_failure_count
  else No_ci_reset

let should_clear_conflict (a : Patch_agent.t) : bool =
  not
    (List.mem a.queue Operation_kind.Merge_conflict ~equal:Operation_kind.equal
    || Option.equal Operation_kind.equal a.current_op
         (Some Operation_kind.Merge_conflict))

(** CI conclusion strings that count as failures. *)
let ci_failure_conclusions =
  [ "failure"; "error"; "action_required"; "timed_out"; "startup_failure" ]

let filter_failed_ci_checks (checks : Ci_check.t list) : Ci_check.t list =
  List.filter checks ~f:(fun (c : Ci_check.t) ->
      List.mem ci_failure_conclusions c.conclusion ~equal:String.equal)

let has_failed_ci_checks (checks : Ci_check.t list) : bool =
  List.exists checks ~f:(fun (c : Ci_check.t) ->
      List.mem ci_failure_conclusions c.conclusion ~equal:String.equal)

type ci_prompt_kind =
  | Known_failures of Ci_check.t list
      (** Non-empty list of checks with failure conclusions. *)
  | Unknown_failure  (** CI failed but no check matches failure conclusions. *)
[@@deriving show, eq, sexp_of, compare]

let ci_prompt_kind (checks : Ci_check.t list) : ci_prompt_kind =
  let failed = filter_failed_ci_checks checks in
  if List.is_empty failed then Unknown_failure else Known_failures failed

let is_stale (a : Patch_agent.t) : bool =
  a.merged || Patch_agent.needs_intervention a || a.branch_blocked || not a.busy

type delivery_decision =
  | Deliver  (** There is content to deliver to the agent. *)
  | Skip_empty  (** Nothing to deliver — skip this operation. *)
[@@deriving show, eq, sexp_of, compare]

let delivery_decision ~(kind : Operation_kind.t)
    ~(inflight_human_messages : string list) ~(review_comment_count : int)
    ~(ci_checks : Ci_check.t list) : delivery_decision =
  match kind with
  | Human ->
      if List.is_empty inflight_human_messages then Skip_empty else Deliver
  | Review_comments -> if review_comment_count = 0 then Skip_empty else Deliver
  | Ci -> if has_failed_ci_checks ci_checks then Deliver else Skip_empty
  | Merge_conflict | Implementation_notes | Rebase -> Deliver
