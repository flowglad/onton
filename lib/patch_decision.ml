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
        | Operation_kind.Pr_body ->
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

(** {2 Respond delivery — pre-session decisions for the runner} *)

let failure_conclusions = Ci_check.failure_conclusions

type base_change = { old_base : string; new_base : string }
[@@deriving show, eq, sexp_of, compare]

type delivery_payload =
  | Human_payload of { messages : string list }
  | Ci_payload of { failed_checks : Ci_check.t list }
  | Review_payload of { comments : Comment.t list }
  | Pr_body_payload
  | Merge_conflict_payload
[@@deriving show, eq, sexp_of, compare]

type respond_delivery =
  | Deliver of { payload : delivery_payload; base_change : base_change option }
  | Skip_empty
  | Respond_stale
[@@deriving show, eq, sexp_of, compare]

(** Keep only failing CI checks that haven't already been delivered to the
    agent. Checks without a stable [id] (StatusContext entries, legacy
    snapshots) bypass the dedup — they can't be keyed reliably, and the
    conservative choice is to deliver rather than silently drop. *)
let filter_undelivered_ci_failures (agent : Patch_agent.t) : Ci_check.t list =
  List.filter agent.ci_checks ~f:(fun (c : Ci_check.t) ->
      if not (Ci_check.is_failure c) then false
      else
        match c.id with
        | None -> true
        | Some id ->
            not (List.mem agent.delivered_ci_run_ids id ~equal:Int.equal))

let respond_delivery ~(agent : Patch_agent.t) ~(kind : Operation_kind.t)
    ~(pre_fire_agent : Patch_agent.t option)
    ~(prefetched_comments : Comment.t list) ~(main_branch : string) :
    respond_delivery =
  if
    agent.merged
    || Patch_agent.needs_intervention agent
    || agent.branch_blocked || not agent.busy
  then Respond_stale
  else
    let source = Option.value pre_fire_agent ~default:agent in
    (* Precompute the CI failure list once so emptiness and payload agree.
       Filtering against [delivered_ci_run_ids] is what prevents a second
       delivery of the same underlying run after an unrelated [generation]
       bump. *)
    let ci_undelivered = filter_undelivered_ci_failures agent in
    let is_empty =
      match kind with
      | Operation_kind.Review_comments -> List.is_empty prefetched_comments
      | Operation_kind.Human -> List.is_empty source.human_messages
      | Operation_kind.Ci ->
          (* Freshness is the caller's responsibility: it re-polls GitHub
             and skips delivery before calling us when the failure is
             already resolved. This is a belt-and-suspenders guard so the
             pure function never emits an empty [Ci_payload] in isolation
             (e.g. if a future caller forgets the freshness hop). Also
             catches the case where every fresh failure has already been
             delivered — no new information to send. *)
          List.is_empty ci_undelivered
      | Operation_kind.Merge_conflict | Operation_kind.Pr_body
      | Operation_kind.Rebase ->
          false
    in
    if is_empty then Skip_empty
    else
      let base_change =
        if Patch_agent.base_branch_changed agent then
          let old_base =
            Option.value_map agent.notified_base_branch ~default:main_branch
              ~f:Branch.to_string
          in
          let new_base =
            Option.value_map agent.base_branch ~default:main_branch
              ~f:Branch.to_string
          in
          Some { old_base; new_base }
        else None
      in
      let payload =
        match kind with
        | Operation_kind.Human ->
            Human_payload { messages = List.rev source.human_messages }
        | Operation_kind.Ci -> Ci_payload { failed_checks = ci_undelivered }
        | Operation_kind.Review_comments ->
            Review_payload { comments = prefetched_comments }
        | Operation_kind.Pr_body -> Pr_body_payload
        | Operation_kind.Merge_conflict -> Merge_conflict_payload
        | Operation_kind.Rebase ->
            (* Invariant: Rebase is never routed through Respond *)
            assert false
      in
      Deliver { payload; base_change }
