(* @archlint.module core
   @archlint.domain patch-agent *)

open Base
open Types
open Operation_kind

type session_fallback = Fresh_available | Tried_fresh | Given_up
[@@deriving show, eq, sexp_of, compare, yojson]

type op_state = Queued | Running
[@@deriving show, eq, sexp_of, compare, yojson]

type worktree_state = Unmaterialized | Materialized of string
[@@deriving show, eq, sexp_of, compare]

type pr_body_refresh = { version : int; pending : bool }
[@@deriving eq, sexp_of, compare]

type t = {
  branch_reconcile : Branch_reconcile.t;
  session_completion : Session_result.completion option;
  scheduler_recovery : Scheduler_recovery.t;
  scheduler_repair : Scheduler_recovery.ticket option;
  patch_id : Patch_id.t;
  branch : Branch.t;
  pr_status : Patch_pr_status.t;
  complexity : int option;
  has_session : bool;
  busy : bool;
  merged : bool;
  queue : Operation_kind.t list;
  satisfies : bool;
  changed : bool;
  base_branch : Branch.t option;
  notified_base_branch : Branch.t option;
  ci_failure_count : int;
  max_ci_failures : int;
  session_fallback : session_fallback;
  wontdo_reason : string option;
      (** A pre-commit opt-out explanation. Pauses automatic work until human
          guidance or an explicit intervention reset clears it. *)
  human_messages : string list;
  inflight_human_messages : string list;
  ci_checks : Ci_check.t list;
  merge_ready : bool;
  head_oid : string option;
  review_decision : string option;
  unresolved_comment_count : int;
  mergeability_unknown : bool;
      (** The provider has not resolved mergeability. Once CI, review, identity,
          and conflict preconditions hold, the merge endpoint decides the
          attempt; unknown mergeability does not suspend the idle timer. *)
  merge_queue_required : bool;
  merge_queue_entry : Pr_state.merge_queue_entry option;
  merge_queue_ejection : Merge_queue_decision.pending option;
  native_stack : bool;
  native_stack_absent_polls : int;
  merge_commit_sha : string option;
      (** Squash/merge commit SHA once this patch's PR is merged (GitHub
          [mergeCommit.oid]). Persisted, because merged agents are not
          re-polled; dependents' base-containment gate ancestry-checks it. *)
  base_contains_merged_siblings : bool;
      (** Poll-derived cache (like [merge_ready]): whether this patch's resolved
          base branch already contains the squash commit of every *merged*
          dependency of this patch. Recomputed each poll tick by [poller_fiber]
          via [git merge-base --is-ancestor]; fail-closed to [false] until
          known. Read by the reconciler ([detect_sibling_stale_bases]) and the
          Start/Rebase eligibility gate. *)
  is_draft : bool;
  pr_body_delivered : bool;
  pr_body_refresh : pr_body_refresh;
  pr_body_artifact_miss_count : int;
      (** Consecutive Pr_body sessions that ended with evidence the agent was
          blocked mid-write (see [Orchestrator.Respond_pr_body_miss]). At >=2
          contributes to [needs_intervention]. Reset by
          [reset_intervention_state]. *)
  review_unresolved_cycle_count : int;
      (** Consecutive Review_comments sessions that completed cleanly but did
          not reply-and-resolve every delivered comment (missing response file,
          failed reply/resolve call, or an unaddressable comment — see
          [Orchestrator.Respond_review_unresolved]). Without this cap the loop
          has no terminator: the agent cannot resolve threads itself, so a
          comment it never responds to re-enqueues a session every poll,
          forever. At [>= 2] contributes to [needs_intervention]. Reset by a
          fully-converged review cycle and by [reset_intervention_state]. *)
  start_attempts_without_pr : int;
  no_commits_push_count : int;
  context_exhaustion_count : int;
      (** Consecutive sessions that ended by exhausting the model's context
          window ([Run_classification.Context_exhausted]). Each one clears
          [llm_session_id] so the next session starts fresh (resuming the
          overflowed thread would re-overflow). At [>= 2] contributes to
          [needs_intervention]: a *fresh* session that still overflows means the
          task does not fit one context window and needs a human. Reset on a
          successful session. *)
  checks_passing : bool;
  current_op : Operation_kind.t option;
  current_op_state : op_state;
      (** Sub-state of [current_op]. [Queued] when the action fiber is alive but
          the actual work has not begun (waiting on the Claude semaphore, or in
          pre-work setup). [Running] once the work has started. Meaningful only
          when [busy] is true; reset to [Queued] on [complete]. Drives the TUI
          "(queued)" suffix so a saturated semaphore is distinguishable from an
          actively-running session. *)
  current_message_id : Message_id.t option;
  generation : int;
  worktree_path : string option;
  branch_blocked : bool;
  llm_session_id : string option;
  automerge_enabled : bool;
  automerge_deadline : float option;
      (** Unix timestamp at which the supervisor should merge the PR if the
          patch is still approved. [None] when automerge is disabled or the
          patch has not yet been observed in the approved state. *)
  automerge_inflight : bool;
      (** [true] between the moment [reconcile_automerge] claims a merge
          decision and the moment the merge call resolves (success or failure).
          Prevents a second overlapping tick from issuing the same merge call.
      *)
  review_requested_for_oid : string option;
  review_request_inflight : bool;
  automerge_failure_count : int;
      (** Consecutive merge-call failures. After [automerge_max_failures] the
          patch is no longer an automerge candidate until the user re-toggles
          automerge or a successful merge resets the counter. *)
  delivered_ci_run_ids : int list;
      (** CheckRun [databaseId]s that have already been delivered to the agent
          as CI feedback. Maintained sorted and deduplicated. Used so a single
          failing run is only delivered once, even if [generation] bumps or
          other state changes cause the Ci payload to be recomposed with the
          same underlying failures. Cleared on [clear_pr]. Checks without a
          stable id (StatusContext entries) bypass this dedup. *)
}
[@@deriving eq, sexp_of, compare]

let pp fmt t = Sexp.pp_hum fmt (sexp_of_t t)
let show t = Sexp.to_string_hum (sexp_of_t t)
let has_pr t = Patch_pr_status.has_pr t.pr_status

let forge_scope (t : t) : Forge_observation.scope =
  {
    pr_number = Patch_pr_status.pr_number t.pr_status;
    branch = Branch.to_string t.branch;
    generation = t.generation;
    head = t.head_oid;
    base_branch = Option.map t.base_branch ~f:Branch.to_string;
  }

let forge_revision_pair_confirmed t =
  Patch_pr_status.is_pr_present t.pr_status
  && Option.exists
       (Forge_observation.latest_fact
          (Branch_reconcile.forge_observations t.branch_reconcile))
       ~f:(fun fact ->
         Forge_observation.revisions_confirmed fact
         && Forge_observation.fact_matches_scope fact (forge_scope t))

let has_conflict t =
  Branch_reconcile.has_conflict t.branch_reconcile ~scope:(forge_scope t)

let branch_rebased_onto t =
  List.find_map (Branch_reconcile.integrations t.branch_reconcile)
    ~f:(fun receipt ->
      Option.map receipt.Branch_reconcile.capture.base_branch
        ~f:Branch.of_string)

let branch_published t =
  not (List.is_empty (Branch_reconcile.publications t.branch_reconcile))

let is_pr_present t = Patch_pr_status.is_pr_present t.pr_status
let is_pr_missing t = Patch_pr_status.is_missing t.pr_status
let pr_number t = Patch_pr_status.pr_number t.pr_status
let default_max_ci_failures = 3

(* Single source of truth for the needs-intervention reason. Returns the
   first triggering condition's short label, or [None] when the predicate
   is false. [needs_intervention] is then defined as
   [Option.is_some (intervention_reason t)], so the two functions cannot
   drift. The label strings are stable and intended to land verbatim in the
   event log so operators can grep for "why is this patch stuck?" by
   reason. *)
let intervention_reason_of_fields ~branch_reconcile ~merged ~has_pr
    ~is_pr_missing ~session_given_up ~wontdo_reason ~human_pending
    ~ci_failure_count ~max_ci_failures ~start_attempts_without_pr
    ~no_commits_push_count ~context_exhaustion_count
    ~pr_body_artifact_miss_count ~review_unresolved_cycle_count =
  let existing =
    if merged then None
    else if Option.is_some wontdo_reason then Some "wontdo"
    else if session_given_up then Some "session_fallback=given_up"
    else if is_pr_missing then Some "pr_missing"
      (* The Human exemption lets a newly-arrived human message be delivered
       even to an agent with a high ci_failure_count or other failure state.
       However, the exemption does NOT apply when session_fallback = Given_up:
       a Given_up agent cannot start any session, so the delivery attempt
       immediately fails at the Give_up check and complete_failed re-enqueues
       Human — creating an infinite loop. Override the exemption so the
       reconciler stops scheduling actions and the agent surfaces for
       manual intervention.

       [merged] is terminal — a merged agent never needs intervention, so
       short-circuit on it to keep the predicate self-consistent even for
       callers that don't pre-filter by [merged]. *)
    else if human_pending then None
    else if ci_failure_count >= max_ci_failures then
      Some (Printf.sprintf "ci_failure_count>=%d" max_ci_failures)
    else if (not has_pr) && start_attempts_without_pr >= 2 then
      Some "start_attempts_without_pr>=2"
    else if no_commits_push_count >= 2 then Some "no_commits_push_count>=2"
    else if context_exhaustion_count >= 2 then
      Some "context_exhaustion_count>=2"
    else if pr_body_artifact_miss_count >= 2 then
      Some "pr_body_artifact_miss_count>=2"
    else if review_unresolved_cycle_count >= 2 then
      Some "review_unresolved_cycle_count>=2"
    else None
  in
  match existing with
  | Some _ -> existing
  | None -> (
      match Branch_reconcile.phase branch_reconcile with
      | Some (Branch_reconcile.Intervention reason) when not merged ->
          Some reason
      | Some (Branch_reconcile.Intervention _) -> None
      | None
      | Some
          ( Branch_reconcile.Preparing | Branch_reconcile.Integrating
          | Branch_reconcile.Repairing _ | Branch_reconcile.Publishing
          | Branch_reconcile.Confirming | Branch_reconcile.Waiting _
          | Branch_reconcile.Recovering | Branch_reconcile.Settled ) ->
          None)

let intervention_reason t =
  (* Accepting a Human operation moves it out of [queue] and into
     [inflight_human_messages] before the runner gets a session slot. Keep the
     exemption active across that ownership transfer; otherwise a failure cap
     can make the just-accepted action stale, [complete_failed] restores the
     message, and reconciliation dispatches the same action forever. *)
  let human_pending =
    List.mem t.queue Operation_kind.Human ~equal:Operation_kind.equal
    || (not (List.is_empty t.inflight_human_messages))
    || Option.is_some t.scheduler_repair
  in
  let existing =
    intervention_reason_of_fields ~branch_reconcile:t.branch_reconcile
      ~merged:t.merged ~has_pr:(has_pr t) ~is_pr_missing:(is_pr_missing t)
      ~session_given_up:(equal_session_fallback t.session_fallback Given_up)
      ~wontdo_reason:t.wontdo_reason ~human_pending
      ~ci_failure_count:t.ci_failure_count ~max_ci_failures:t.max_ci_failures
      ~start_attempts_without_pr:t.start_attempts_without_pr
      ~no_commits_push_count:t.no_commits_push_count
      ~context_exhaustion_count:t.context_exhaustion_count
      ~pr_body_artifact_miss_count:t.pr_body_artifact_miss_count
      ~review_unresolved_cycle_count:t.review_unresolved_cycle_count
  in
  match existing with
  | Some _ -> existing
  | None ->
      if human_pending then None
      else Scheduler_recovery.attention t.scheduler_recovery

let needs_intervention t = Option.is_some (intervention_reason t)

let needs_intervention_of_fields ~branch_reconcile ~merged ~has_pr
    ~is_pr_missing ~session_given_up ~wontdo_reason ~human_pending
    ~ci_failure_count ~max_ci_failures ~start_attempts_without_pr
    ~no_commits_push_count ~context_exhaustion_count
    ~pr_body_artifact_miss_count ~review_unresolved_cycle_count =
  Option.is_some
    (intervention_reason_of_fields ~branch_reconcile ~merged ~has_pr
       ~is_pr_missing ~session_given_up ~wontdo_reason ~human_pending
       ~ci_failure_count ~max_ci_failures ~start_attempts_without_pr
       ~no_commits_push_count ~context_exhaustion_count
       ~pr_body_artifact_miss_count ~review_unresolved_cycle_count)

let create ~branch ?(max_ci_failures = default_max_ci_failures) patch_id =
  {
    branch_reconcile = Branch_reconcile.empty;
    session_completion = None;
    scheduler_recovery = Scheduler_recovery.empty;
    scheduler_repair = None;
    patch_id;
    branch;
    pr_status = Patch_pr_status.Absent;
    complexity = None;
    has_session = false;
    busy = false;
    merged = false;
    queue = [];
    satisfies = false;
    changed = false;
    base_branch = None;
    notified_base_branch = None;
    ci_failure_count = 0;
    max_ci_failures;
    session_fallback = Fresh_available;
    wontdo_reason = None;
    human_messages = [];
    inflight_human_messages = [];
    ci_checks = [];
    merge_ready = false;
    head_oid = None;
    review_decision = None;
    unresolved_comment_count = 0;
    mergeability_unknown = false;
    merge_queue_required = false;
    merge_queue_entry = None;
    merge_queue_ejection = None;
    native_stack = false;
    native_stack_absent_polls = 0;
    merge_commit_sha = None;
    (* Defaults to [true] ("no known missing sibling"): the poller recomputes
       this every tick before any fan-in start can become eligible, and a fresh
       agent has no merged deps yet. Fail-open at birth, recomputed-to-truth. *)
    base_contains_merged_siblings = true;
    is_draft = false;
    pr_body_delivered = false;
    pr_body_refresh = { version = 0; pending = false };
    pr_body_artifact_miss_count = 0;
    review_unresolved_cycle_count = 0;
    start_attempts_without_pr = 0;
    no_commits_push_count = 0;
    context_exhaustion_count = 0;
    checks_passing = false;
    current_op = None;
    current_op_state = Queued;
    current_message_id = None;
    generation = 0;
    worktree_path = None;
    branch_blocked = false;
    llm_session_id = None;
    automerge_enabled = false;
    automerge_deadline = None;
    automerge_inflight = false;
    review_requested_for_oid = None;
    review_request_inflight = false;
    automerge_failure_count = 0;
    delivered_ci_run_ids = [];
  }

let create_adhoc ~complexity ~patch_id ~branch ~pr_number ~max_ci_failures =
  {
    branch_reconcile = Branch_reconcile.empty;
    session_completion = None;
    scheduler_recovery = Scheduler_recovery.empty;
    scheduler_repair = None;
    patch_id;
    branch;
    pr_status = Patch_pr_status.Present pr_number;
    complexity;
    has_session = false;
    busy = false;
    merged = false;
    queue = [];
    satisfies = false;
    changed = false;
    base_branch = None;
    notified_base_branch = None;
    ci_failure_count = 0;
    max_ci_failures;
    session_fallback = Fresh_available;
    wontdo_reason = None;
    human_messages = [];
    inflight_human_messages = [];
    ci_checks = [];
    merge_ready = false;
    head_oid = None;
    review_decision = None;
    unresolved_comment_count = 0;
    mergeability_unknown = false;
    merge_queue_required = false;
    merge_queue_entry = None;
    merge_queue_ejection = None;
    native_stack = false;
    native_stack_absent_polls = 0;
    merge_commit_sha = None;
    (* Defaults to [true] ("no known missing sibling"): the poller recomputes
       this every tick before any fan-in start can become eligible, and a fresh
       agent has no merged deps yet. Fail-open at birth, recomputed-to-truth. *)
    base_contains_merged_siblings = true;
    is_draft = false;
    pr_body_delivered = true;
    pr_body_refresh = { version = 0; pending = false };
    pr_body_artifact_miss_count = 0;
    review_unresolved_cycle_count = 0;
    start_attempts_without_pr = 0;
    no_commits_push_count = 0;
    context_exhaustion_count = 0;
    checks_passing = false;
    current_op = None;
    current_op_state = Queued;
    current_message_id = None;
    generation = 0;
    worktree_path = None;
    branch_blocked = false;
    llm_session_id = None;
    automerge_enabled = false;
    automerge_deadline = None;
    automerge_inflight = false;
    review_requested_for_oid = None;
    review_request_inflight = false;
    automerge_failure_count = 0;
    delivered_ci_run_ids = [];
  }

let highest_priority t =
  let queue =
    if equal_session_fallback t.session_fallback Given_up then
      List.filter t.queue ~f:(Operation_kind.equal Operation_kind.Rebase)
    else t.queue
  in
  List.min_elt queue ~compare:(fun a b ->
      Int.compare (Priority.priority a) (Priority.priority b))

let enqueue t k =
  if
    Operation_kind.equal k Operation_kind.Rebase
    && Branch_reconcile.is_unsettled t.branch_reconcile
    || List.mem t.queue k ~equal:Operation_kind.equal
  then t
  else { t with queue = k :: t.queue }

let mark_merged t = { t with merged = true }

let add_human_message t msg =
  {
    t with
    human_messages = msg :: t.human_messages;
    scheduler_recovery = Scheduler_recovery.resume t.scheduler_recovery;
  }

let add_human_messages t msgs =
  { t with human_messages = List.append t.human_messages msgs }

let normalize_wontdo_reason reason =
  Option.bind reason ~f:(fun text ->
      let text = String.strip text in
      if String.is_empty text then None else Some text)

let reconciliation_hold_reason t =
  if t.merged then Some "patch_merged"
  else if Option.is_some t.wontdo_reason then Some "patch_wontdo"
  else None

let set_wontdo t reason =
  match normalize_wontdo_reason (Some reason) with
  | None -> t
  | Some _ as wontdo_reason -> { t with wontdo_reason }

let set_session_failed t =
  match t.session_fallback with
  | Fresh_available -> { t with session_fallback = Tried_fresh }
  | Tried_fresh | Given_up -> t

let set_tried_fresh t =
  match t.session_fallback with
  | Fresh_available -> { t with session_fallback = Tried_fresh }
  | Tried_fresh -> { t with session_fallback = Given_up }
  | Given_up -> t

let clear_session_fallback t = { t with session_fallback = Fresh_available }

(** Handle a Claude session failure. Pure decision logic:
    - Start path (no PR or published branch) + fresh failure: reset to
      Fresh_available for retry
    - Resume failure: escalate to Tried_fresh (will try fresh next)
    - Fresh failure (respond path): escalate one step via set_tried_fresh *)
let on_session_failure t ~is_fresh =
  if (not (has_pr t || branch_published t)) && is_fresh then
    (* Start path fresh failure: full reset for clean retry *)
    { t with session_fallback = Fresh_available; llm_session_id = None }
  else if is_fresh then set_tried_fresh t
  else set_session_failed t

let increment_no_commits_push_count t =
  { t with no_commits_push_count = t.no_commits_push_count + 1 }

let reset_no_commits_push_count t = { t with no_commits_push_count = 0 }

(* Context-window exhaustion: bump the counter and drop [llm_session_id] so the
   next session starts fresh — resuming the overflowed thread would re-overflow
   immediately. Deliberately leaves [session_fallback] untouched: exhaustion has
   its own intervention budget (the counter), orthogonal to the resume/fresh
   fallback ladder. *)
let on_context_exhausted t =
  {
    t with
    context_exhaustion_count = t.context_exhaustion_count + 1;
    llm_session_id = None;
  }

let reset_context_exhaustion_count t = { t with context_exhaustion_count = 0 }

let increment_pr_body_artifact_miss_count t =
  { t with pr_body_artifact_miss_count = t.pr_body_artifact_miss_count + 1 }

let reset_pr_body_artifact_miss_count t =
  { t with pr_body_artifact_miss_count = 0 }

let increment_review_unresolved_cycle_count t =
  { t with review_unresolved_cycle_count = t.review_unresolved_cycle_count + 1 }

let reset_review_unresolved_cycle_count t =
  { t with review_unresolved_cycle_count = 0 }

let set_base_branch t branch =
  let notified =
    if t.has_session then
      match t.notified_base_branch with None -> Some branch | some -> some
    else t.notified_base_branch
  in
  let branch_reconcile =
    match Branch_reconcile.operation t.branch_reconcile with
    | Some op
      when Branch_reconcile.is_unsettled t.branch_reconcile
           && not (Option.equal Branch.equal t.base_branch (Some branch)) ->
        fst
          (Branch_reconcile.step t.branch_reconcile
             (Branch_reconcile.Request
                { op.intent with base = Branch.to_string branch }))
    | Some _ | None -> t.branch_reconcile
  in
  {
    t with
    base_branch = Some branch;
    notified_base_branch = notified;
    branch_reconcile;
  }

let set_notified_base_branch t branch =
  { t with notified_base_branch = Some branch }

let base_branch_changed t =
  match (t.notified_base_branch, t.base_branch) with
  | Some old_base, Some new_base -> not (Branch.equal old_base new_base)
  | _ -> false

let set_merge_ready t v = { t with merge_ready = v }
let set_head_oid t head_oid = { t with head_oid }

let expected_remote_head_oid t =
  Option.map
    (Branch_reconcile.publication_observation_pending t.branch_reconcile)
    ~f:(fun publication ->
      Branch_reconcile.Commit.to_string publication.revision)

let observe_publication_head ?confirmed_remote_head t head_oid =
  let branch_reconcile =
    match
      ( Branch_reconcile.publication_observation_target t.branch_reconcile,
        Option.bind head_oid ~f:Branch_reconcile.Commit.make )
    with
    | Some publication, Some head ->
        fst
          (Branch_reconcile.step t.branch_reconcile
             (Branch_reconcile.Publication_observed
                {
                  publication;
                  head;
                  remote =
                    Option.bind confirmed_remote_head
                      ~f:Branch_reconcile.Commit.make;
                }))
    | None, _ | _, None -> t.branch_reconcile
  in
  { t with branch_reconcile }

let set_review_decision t review_decision = { t with review_decision }

let set_unresolved_comment_count t unresolved_comment_count =
  { t with unresolved_comment_count }

let set_mergeability_unknown t v = { t with mergeability_unknown = v }
let set_merge_queue_required t v = { t with merge_queue_required = v }
let set_merge_queue_entry t merge_queue_entry = { t with merge_queue_entry }

let set_merge_queue_ejection t merge_queue_ejection =
  { t with merge_queue_ejection }

let set_native_stack t observed =
  if observed then { t with native_stack = true; native_stack_absent_polls = 0 }
  else if (not t.native_stack) || t.native_stack_absent_polls >= 1 then
    { t with native_stack = false; native_stack_absent_polls = 0 }
  else { t with native_stack_absent_polls = 1 }

let in_merge_queue t = Option.is_some t.merge_queue_entry
let set_merge_commit_sha t sha = { t with merge_commit_sha = sha }

let set_base_contains_merged_siblings t v =
  { t with base_contains_merged_siblings = v }

let set_is_draft t v = { t with is_draft = v }
let set_pr_body_delivered t v = { t with pr_body_delivered = v }

let request_pr_body_refresh t =
  {
    t with
    pr_body_refresh =
      { version = t.pr_body_refresh.version + 1; pending = true };
  }

let acknowledge_pr_body_refresh t ~publication =
  if
    Patch_id.equal t.patch_id publication.patch_id
    && Patch_pr_status.equal t.pr_status publication.pr_status
    && Int.equal t.pr_body_refresh.version publication.pr_body_refresh.version
  then { t with pr_body_refresh = { t.pr_body_refresh with pending = false } }
  else t

let increment_start_attempts_without_pr t =
  { t with start_attempts_without_pr = t.start_attempts_without_pr + 1 }

(** Handle a successful Claude run where PR discovery failed by recording a
    durable attempt. The controller derives intervention from this fact. No-op
    when the agent already has a PR — reruns after the first session should not
    accumulate this counter. *)
let on_pr_discovery_failure t =
  if has_pr t then t else increment_start_attempts_without_pr t

let on_pre_session_failure t =
  if has_pr t || branch_published t then t
  else increment_start_attempts_without_pr t

let set_checks_passing t v = { t with checks_passing = v }
let set_worktree_path t path = { t with worktree_path = Some path }
let clear_worktree_path t = { t with worktree_path = None }

let worktree_state t =
  match t.worktree_path with
  | None -> Unmaterialized
  | Some path -> Materialized path

(* Shared known preconditions for merge attempts, including identity and
   retained conflict/publication evidence. The endpoint resolves unknown
   provider mergeability without weakening these preconditions. *)
let is_approved_modulo_merge_ready t ~main_branch =
  is_pr_present t
  && forge_revision_pair_confirmed t
  && Option.is_none t.merge_queue_ejection
  && (not t.busy)
  && (not (has_conflict t))
  && (not (Branch_reconcile.is_unsettled t.branch_reconcile))
  && Option.is_none (Branch_reconcile.unobserved_publication t.branch_reconcile)
  && (not (needs_intervention t))
  && (not t.is_draft) && (not t.branch_blocked)
  && Option.equal Branch.equal t.base_branch (Some main_branch)

let is_approved t ~main_branch =
  (t.merge_ready
  || t.mergeability_unknown && t.checks_passing
     && not (Pr_state.review_blocking t.review_decision))
  && is_approved_modulo_merge_ready t ~main_branch

let awaiting_manual_merge t ~main_branch =
  Option.is_none
    (Forge_observation.pending_request
       (Branch_reconcile.forge_observations t.branch_reconcile))
  && (not t.merged) && (not t.automerge_enabled) && is_approved t ~main_branch
  && Option.is_none t.merge_queue_entry
  && t.checks_passing && List.is_empty t.queue

let should_request_review t ~main_branch =
  has_pr t
  && forge_revision_pair_confirmed t
  && (not (has_conflict t))
  && (not (Branch_reconcile.is_unsettled t.branch_reconcile))
  && Option.is_none (Branch_reconcile.unobserved_publication t.branch_reconcile)
  && t.checks_passing
  && t.unresolved_comment_count = 0
  && (not t.is_draft) && (not t.busy)
  && (not (needs_intervention t))
  && Option.equal Branch.equal t.base_branch (Some main_branch)
  && Option.equal String.equal t.review_decision (Some "REVIEW_REQUIRED")
  && (match t.head_oid with
    | None -> false
    | Some head_oid ->
        not
          (Option.equal String.equal (Some head_oid) t.review_requested_for_oid))
  && not t.review_request_inflight

let increment_ci_failure_count t =
  { t with ci_failure_count = t.ci_failure_count + 1 }

let reset_ci_failure_count t = { t with ci_failure_count = 0 }

(* Config stamp, not a state transition: deliberately does not bump
   [generation], so restoring a snapshot under a different --max-ci-failures
   does not invalidate in-flight outbox messages. *)
let set_max_ci_failures t ~max_ci_failures = { t with max_ci_failures }

let set_ci_checks t checks =
  (* Completed repair attempts consume the budget for delivered failures.
     A failing CheckRun not yet delivered is new work, even when an older
     run exhausted its budget. Id-less statuses cannot establish new work. *)
  let has_new_failure =
    List.exists checks ~f:(fun (check : Ci_check.t) ->
        Ci_check.is_failure check
        &&
        match check.id with
        | None -> false
        | Some id -> not (List.mem t.delivered_ci_run_ids id ~equal:Int.equal))
  in
  {
    t with
    ci_checks = checks;
    ci_failure_count = (if has_new_failure then 0 else t.ci_failure_count);
  }

let record_delivered_ci_run_ids t ids =
  (* Maintain sorted + deduplicated list. Small enough (tens of entries per
     patch lifetime) that a list is fine; sorting keeps equality checks stable
     across serialization round-trips. *)
  let combined = List.rev_append ids t.delivered_ci_run_ids in
  let deduped =
    List.dedup_and_sort combined ~compare:(fun a b -> Int.compare a b)
  in
  { t with delivered_ci_run_ids = deduped }

let set_branch_blocked t = { t with branch_blocked = true }
let clear_branch_blocked t = { t with branch_blocked = false }
let set_current_message_id t current_message_id = { t with current_message_id }
let bump_generation t = { t with generation = t.generation + 1 }
let set_llm_session_id t llm_session_id = { t with llm_session_id }

let mark_inflight_human_messages_delivered t =
  if Option.equal Operation_kind.equal t.current_op (Some Operation_kind.Human)
  then { t with inflight_human_messages = [] }
  else t

let set_automerge_enabled t v =
  if Bool.equal t.automerge_enabled v then t
  else
    (* Toggling automerge always resets the failure counter and clears any
       inflight flag, so a previously-capped patch can retry and any stale
       inflight state (e.g. from a crashed supervisor) cannot permanently block
       reconciliation. Disabling additionally clears the pending deadline. *)
    let automerge_deadline = if v then t.automerge_deadline else None in
    {
      t with
      automerge_enabled = v;
      automerge_deadline;
      automerge_inflight = false;
      automerge_failure_count = 0;
    }

let set_automerge_deadline t deadline =
  { t with automerge_deadline = Some deadline }

let clear_automerge_deadline t = { t with automerge_deadline = None }
let set_automerge_inflight t v = { t with automerge_inflight = v }

let set_review_requested_for_oid t oid =
  { t with review_requested_for_oid = oid }

let set_review_request_inflight t v = { t with review_request_inflight = v }

let increment_automerge_failure_count t =
  { t with automerge_failure_count = t.automerge_failure_count + 1 }

let reset_automerge_failure_count t = { t with automerge_failure_count = 0 }

let begin_branch_reconciliation t =
  { t with busy = true; current_op = None; current_op_state = Queued }

let resume_current_message t ~op =
  {
    t with
    busy = true;
    has_session = true;
    current_op = op;
    current_op_state = Queued;
  }

let mark_running t =
  if not t.busy then t else { t with current_op_state = Running }

let reset_intervention_state t =
  let branch_reconcile, _ =
    Branch_reconcile.step t.branch_reconcile Branch_reconcile.Resume
  in
  {
    t with
    branch_reconcile;
    scheduler_recovery = Scheduler_recovery.empty;
    scheduler_repair = None;
    session_fallback = Fresh_available;
    wontdo_reason = None;
    ci_failure_count = 0;
    start_attempts_without_pr = 0;
    no_commits_push_count = 0;
    context_exhaustion_count = 0;
    pr_body_artifact_miss_count = 0;
    review_unresolved_cycle_count = 0;
  }

let reset_busy t = if not t.busy then t else { t with busy = false }

let restore ?(scheduler_recovery = Scheduler_recovery.empty)
    ?(scheduler_repair = None) ?(session_completion = None)
    ?(branch_reconcile = Branch_reconcile.empty) ~patch_id ~branch ~pr_status
    ?(complexity = None) ~has_session ~busy ~merged ~queue ~satisfies ~changed
    ~base_branch ~notified_base_branch ~ci_failure_count
    ?(max_ci_failures = default_max_ci_failures) ~session_fallback
    ?(wontdo_reason = None) ~human_messages ~inflight_human_messages ~ci_checks
    ~merge_ready ?(head_oid = None) ?(review_decision = None)
    ?(unresolved_comment_count = 0) ~mergeability_unknown ~merge_queue_required
    ~merge_queue_entry ?(merge_queue_ejection = None) ?(native_stack = false)
    ?(native_stack_absent_polls = 0) ~merge_commit_sha
    ~base_contains_merged_siblings ~is_draft ~pr_body_delivered
    ?(pr_body_refresh_pending = false) ?(pr_body_refresh_version = 0)
    ~pr_body_artifact_miss_count ?(review_unresolved_cycle_count = 0)
    ~start_attempts_without_pr ~no_commits_push_count ~context_exhaustion_count
    ~checks_passing ~current_op ~current_op_state ~current_message_id
    ~generation ~worktree_path ~branch_blocked ~llm_session_id
    ~automerge_enabled ~automerge_deadline ~automerge_inflight
    ?(review_requested_for_oid = None) ?(review_request_inflight = false)
    ~automerge_failure_count ~delivered_ci_run_ids () =
  let scheduler_repair =
    Option.filter scheduler_repair ~f:(fun ticket ->
        busy
        && Option.equal Operation_kind.equal current_op
             (Some Operation_kind.Human)
        && Option.is_some current_message_id
        && Scheduler_recovery.is_running scheduler_recovery ticket)
  in
  let scheduler_recovery =
    Scheduler_recovery.recover scheduler_recovery ~running:scheduler_repair
  in
  {
    branch_reconcile;
    session_completion;
    scheduler_recovery;
    scheduler_repair;
    patch_id;
    branch;
    pr_status;
    complexity;
    has_session;
    busy;
    merged;
    queue;
    satisfies;
    changed;
    base_branch;
    notified_base_branch;
    ci_failure_count;
    max_ci_failures;
    session_fallback;
    wontdo_reason = normalize_wontdo_reason wontdo_reason;
    human_messages;
    inflight_human_messages;
    ci_checks;
    merge_ready;
    head_oid;
    review_decision;
    unresolved_comment_count;
    mergeability_unknown;
    merge_queue_required;
    merge_queue_entry;
    merge_queue_ejection;
    native_stack;
    native_stack_absent_polls;
    merge_commit_sha;
    base_contains_merged_siblings;
    is_draft;
    pr_body_delivered;
    pr_body_refresh =
      {
        version = Int.max 0 pr_body_refresh_version;
        pending = pr_body_refresh_pending;
      };
    pr_body_artifact_miss_count;
    review_unresolved_cycle_count;
    start_attempts_without_pr;
    no_commits_push_count;
    context_exhaustion_count;
    checks_passing;
    current_op;
    current_op_state;
    current_message_id;
    generation;
    worktree_path;
    branch_blocked;
    llm_session_id;
    automerge_enabled;
    automerge_deadline;
    automerge_inflight;
    review_requested_for_oid;
    review_request_inflight;
    automerge_failure_count;
    delivered_ci_run_ids;
  }

let set_pr_number t pr_number =
  (* Dispatch on the pure classifier:
     - [Set_present_recover_same] (Missing N → Present N or Present N →
       Present N): preserve all world-state. The body is still delivered, CI
       runs already accounted, notified_base_branch still authoritative.
     - [Set_present_adopt_new] (Absent → Present, Missing M → Present N
       M≠N, Present M → Present N M≠N): reset PR-bootstrap lifecycle fields
       that the OLD setter cleared, plus the PR-keyed CI history that no
       longer corresponds to the new PR's check runs. Does NOT touch
       base_branch / notified_base_branch — those are owned by [start]
       (bootstrap) and the poller (renumbering). *)
  match Patch_pr_status.classify_set_present t.pr_status pr_number with
  | Patch_pr_status.Set_present_recover_same ->
      { t with pr_status = Patch_pr_status.set_present t.pr_status pr_number }
  | Patch_pr_status.Set_present_adopt_new ->
      {
        t with
        pr_status = Patch_pr_status.set_present t.pr_status pr_number;
        is_draft = true;
        merge_ready = false;
        head_oid = None;
        merge_queue_ejection = None;
        merge_queue_entry = None;
        merge_queue_required = false;
        review_decision = None;
        unresolved_comment_count = 0;
        mergeability_unknown = false;
        native_stack = false;
        native_stack_absent_polls = 0;
        pr_body_delivered = false;
        pr_body_refresh = { version = 0; pending = false };
        checks_passing = false;
        start_attempts_without_pr = 0;
        ci_checks = [];
        ci_failure_count = 0;
        delivered_ci_run_ids = [];
      }

let clear_pr t =
  (* Gameplan-recreate path: a gameplan patch's PR was closed and the next
     [Start] should open a fresh one. Tightens to Present-only via
     [Patch_pr_status.clear_for_recreate]. Calling this on Absent or Missing
     is a caller bug — see [mark_pr_missing] for the ad-hoc-vanished path. *)
  {
    t with
    pr_status = Patch_pr_status.clear_for_recreate t.pr_status;
    merge_queue_ejection = None;
    merge_queue_entry = None;
    merge_queue_required = false;
    is_draft = false;
    merge_ready = false;
    head_oid = None;
    review_decision = None;
    unresolved_comment_count = 0;
    mergeability_unknown = false;
    native_stack = false;
    native_stack_absent_polls = 0;
    checks_passing = false;
    ci_checks = [];
    ci_failure_count = 0;
    base_branch = None;
    notified_base_branch = None;
    delivered_ci_run_ids = [];
  }

let mark_pr_missing t =
  (* Minimal transition. Clears only the world-state assertions that are no
     longer authoritative (is_draft, merge_ready, checks_passing, ci_checks);
     preserves everything else so a Missing -> Present recovery via
     [set_pr_number] (same-number, [Set_present_recover_same]) is a near
     no-op.

     Queue is preserved: the planner gates Rebase/Respond on [is_pr_present]
     so PR-coupled entries cannot fire on a Missing agent. [busy] /
     [current_op] are preserved too — any inflight session runs to
     completion against the recorded number; its outcome is applied to the
     agent normally (the session-level retry counters track real failures
     even when GitHub returns 404). *)
  {
    t with
    pr_status = Patch_pr_status.mark_missing t.pr_status;
    is_draft = false;
    merge_ready = false;
    head_oid = None;
    review_decision = None;
    unresolved_comment_count = 0;
    mergeability_unknown = false;
    checks_passing = false;
    ci_checks = [];
  }

let start t ~base_branch =
  if has_pr t then invalid_arg "Patch_agent.start: patch already has a PR";
  if t.busy then invalid_arg "Patch_agent.start: patch is already busy";
  (* A no-PR patch is always dispatched through the Start action, even when a
     human message is queued. Treat that Start as the delivery vehicle for the
     queued Human turn: otherwise the runner renders the initial patch prompt
     again and the Human operation can never reach [respond], whose contract
     requires a PR. Moving the inbox to inflight gives this path the same
     accept/restore semantics as [respond Human]. *)
  let is_human_queued =
    List.mem t.queue Operation_kind.Human ~equal:Operation_kind.equal
  in
  let queue =
    if is_human_queued then
      List.filter t.queue ~f:(fun k ->
          not (Operation_kind.equal k Operation_kind.Human))
    else t.queue
  in
  {
    t with
    has_session = true;
    busy = true;
    current_op = (if is_human_queued then Some Operation_kind.Human else None);
    current_op_state = Queued;
    current_message_id = None;
    queue;
    human_messages = (if is_human_queued then [] else t.human_messages);
    inflight_human_messages =
      (if is_human_queued then
         List.append t.human_messages t.inflight_human_messages
       else t.inflight_human_messages);
    satisfies = true;
    base_branch = Some base_branch;
    notified_base_branch = Some base_branch;
    ci_checks = [];
  }

let rebase t ~base_branch =
  if not (is_pr_present t || branch_published t) then
    invalid_arg "Patch_agent.rebase: patch has no PR (or PR is missing)";
  if t.merged then invalid_arg "Patch_agent.rebase: patch is merged";

  if t.busy then invalid_arg "Patch_agent.rebase: patch is busy";
  if not (List.mem t.queue Operation_kind.Rebase ~equal:Operation_kind.equal)
  then invalid_arg "Patch_agent.rebase: Rebase not in queue";
  (match highest_priority t with
  | Some hp when Operation_kind.equal hp Operation_kind.Rebase -> ()
  | _ -> invalid_arg "Patch_agent.rebase: Rebase not highest priority");
  let queue =
    List.filter t.queue ~f:(fun j ->
        not (Operation_kind.equal j Operation_kind.Rebase))
  in
  {
    t with
    has_session = true;
    busy = true;
    current_op = Some Rebase;
    current_op_state = Queued;
    current_message_id = None;
    queue;
    base_branch = Some base_branch;
    notified_base_branch =
      (match t.notified_base_branch with
      | None -> Some base_branch
      | some -> some);
    merge_ready = false;
    mergeability_unknown = false;
    checks_passing = false;
  }

let respond t k =
  if not (is_pr_present t || branch_published t) then
    invalid_arg "Patch_agent.respond: patch has no PR (or PR is missing)";
  if t.merged then invalid_arg "Patch_agent.respond: patch is merged";

  if t.busy then invalid_arg "Patch_agent.respond: patch is busy";
  if needs_intervention t then
    invalid_arg "Patch_agent.respond: patch needs intervention";
  if Operation_kind.equal k Operation_kind.Rebase then
    invalid_arg "Patch_agent.respond: Rebase is not a feedback operation";
  if not (List.mem t.queue k ~equal:Operation_kind.equal) then
    invalid_arg "Patch_agent.respond: operation not in queue";
  (match highest_priority t with
  | Some hp when Operation_kind.equal hp k -> ()
  | _ -> invalid_arg "Patch_agent.respond: not highest priority");
  let queue =
    List.filter t.queue ~f:(fun j -> not (Operation_kind.equal j k))
  in
  let equal_k = Operation_kind.equal k in
  let is_human = equal_k Human in
  let is_ci = equal_k Ci in
  let is_review = equal_k Review_comments in
  let satisfies = if is_human then false else t.satisfies in
  (* Spec: changed' only when a valid pending comment exists. We set it
     unconditionally here because comment validity is resolved downstream
     by the agent session — a conservative simplification. *)
  let changed = if is_ci || is_review then true else t.changed in
  (* NB: [ci_failure_count] is NOT bumped here. The bump happens in
     [Orchestrator.apply_respond_outcome] on [Respond_ok] for Ci, so the
     counter only reflects CI fix attempts that actually delivered a payload
     (i.e. had at least one failure conclusion) and ran to completion. *)
  let scheduler_repair =
    if is_human then Scheduler_recovery.pending t.scheduler_recovery else None
  in
  let scheduler_recovery =
    Option.value_map scheduler_repair ~default:t.scheduler_recovery
      ~f:(Scheduler_recovery.claim t.scheduler_recovery)
  in
  {
    t with
    scheduler_repair;
    scheduler_recovery;
    has_session = true;
    busy = true;
    current_op = Some k;
    current_op_state = Queued;
    current_message_id = None;
    queue;
    satisfies;
    changed;
    human_messages = (if is_human then [] else t.human_messages);
    inflight_human_messages =
      (if is_human then t.human_messages else t.inflight_human_messages);
    notified_base_branch =
      (match t.notified_base_branch with None -> t.base_branch | some -> some);
    merge_ready = false;
    mergeability_unknown = false;
    checks_passing = false;
  }

let complete t =
  if not t.busy then t
  else
    let scheduler_recovery =
      Option.value_map t.scheduler_repair ~default:t.scheduler_recovery
        ~f:(fun ticket ->
          Scheduler_recovery.complete t.scheduler_recovery ticket ~success:false)
    in
    {
      t with
      scheduler_recovery;
      scheduler_repair = None;
      busy = false;
      current_op = None;
      current_op_state = Queued;
      current_message_id = None;
      inflight_human_messages = [];
    }

let scheduler_scope t : Scheduler_recovery.scope =
  {
    head = t.head_oid;
    base = Option.map t.base_branch ~f:Branch.to_string;
    pr = Option.map (pr_number t) ~f:Pr_number.to_int;
    publication = expected_remote_head_oid t;
    queue =
      (match t.merge_queue_ejection with
      | Some (Merge_queue_decision.Awaiting_removal { entry; _ }) ->
          Some entry.id
      | None | Some (Merge_queue_decision.Failed _) -> None);
  }

let scheduler_blocked t ~pr_base_update ~pr_draft_update =
  let module R = Scheduler_recovery in
  let forge =
    (is_pr_present t && not (forge_revision_pair_confirmed t))
    || Option.is_some (expected_remote_head_oid t)
  in
  (if forge then [ R.Forge_revision ] else [])
  @ (if
       (not forge) && (not t.checks_passing)
       && not (List.exists t.ci_checks ~f:Ci_check.is_failure)
     then [ R.Ci_evidence ]
     else [])
  @ (match t.merge_queue_ejection with
    | Some (Merge_queue_decision.Awaiting_removal _) -> [ R.Queue_removal ]
    | None | Some (Merge_queue_decision.Failed _) -> [])
  @ (if t.native_stack then [ R.Native_stack ] else [])
  @ (if pr_base_update || R.contains t.scheduler_recovery R.Pr_base then
       [ R.Pr_base ]
     else [])
  @ (if pr_draft_update || R.contains t.scheduler_recovery R.Pr_draft then
       [ R.Pr_draft ]
     else [])
  @
  if t.automerge_enabled && t.automerge_failure_count >= 3 then
    [ R.Merge_attempt ]
  else []

let observe_scheduler t ~now ~pr_base_update ~pr_draft_update =
  if
    t.busy || t.merged
    || Branch_reconcile.is_unsettled t.branch_reconcile
    || not (is_pr_present t || branch_published t)
  then t
  else
    let scheduler_recovery =
      Scheduler_recovery.observe ~ci_checks:t.ci_checks t.scheduler_recovery
        ~now ~scope:(scheduler_scope t)
        ~blocked:(scheduler_blocked t ~pr_base_update ~pr_draft_update)
    in
    let t = { t with scheduler_recovery } in
    if
      Option.is_some (Scheduler_recovery.pending scheduler_recovery)
      && (Option.is_none (Scheduler_recovery.attention scheduler_recovery)
         || not (List.is_empty t.human_messages))
    then enqueue t Operation_kind.Human
    else t

let scheduler_write_failed t ~now ~kind =
  {
    t with
    scheduler_recovery =
      Scheduler_recovery.failed t.scheduler_recovery ~now
        ~scope:(scheduler_scope t) ~kind;
  }

let finish_scheduler_repair ?now t ~success =
  match t.scheduler_repair with
  | None -> t
  | Some ticket ->
      let current = scheduler_scope t in
      let repaired_revision =
        success
        && Option.exists t.session_completion ~f:(fun completion ->
            completion.turn_accepted
            && Session_result.equal_delivery_mode completion.delivery_mode
                 Respond
            && Option.equal Operation_kind.equal completion.kind (Some Human)
            && Option.is_some t.current_message_id
            && Option.equal Message_id.equal completion.message_id
                 t.current_message_id
            && Option.is_some completion.head
            && (Option.equal String.equal current.head ticket.scope.head
               || Option.equal String.equal completion.head current.head)
            && (Option.equal String.equal current.publication
                  ticket.scope.publication
               || Option.equal String.equal completion.head current.publication
               || Option.is_none current.publication)
            && List.exists (Branch_reconcile.publications t.branch_reconcile)
                 ~f:(fun receipt ->
                   Option.equal String.equal completion.head
                     (Some
                        (Branch_reconcile.Commit.to_string
                           receipt.published_revision))
                   && Branch_reconcile.equal_purpose
                        receipt.published_intent.purpose
                        (Session_result.publication_intent completion
                           ~base:(Option.value current.base ~default:"")
                           ~policy:Branch_reconcile.Preserve_ancestry)
                          .purpose))
      in
      let same_context =
        Option.equal Int.equal ticket.scope.pr current.pr
        && Option.equal String.equal ticket.scope.base current.base
        && Option.equal String.equal ticket.scope.queue current.queue
      in
      let accepted =
        Scheduler_recovery.is_running t.scheduler_recovery ticket
        && same_context
        && ((not success)
           || Scheduler_recovery.equal_scope ticket.scope current
           || repaired_revision)
        (* A failed owned turn needs operator attention even if its work changed
           the revision. This records execution failure, not successful repair
           credit, and cannot finish a different PR/base/queue or newer ticket. *)
      in
      let merge_queue_ejection =
        if
          accepted && success
          && Option.equal String.equal ticket.scope.queue current.queue
          && Scheduler_recovery.equal_kind ticket.kind
               Scheduler_recovery.Queue_removal
        then Merge_queue_decision.accept_removal_repair t.merge_queue_ejection
        else t.merge_queue_ejection
      in
      {
        t with
        scheduler_recovery =
          (if accepted then
             Scheduler_recovery.complete ?now ~scope:current
               t.scheduler_recovery ticket ~success
           else if Scheduler_recovery.is_running t.scheduler_recovery ticket
           then
             (* Supersede only this obsolete execution. The next observation
                starts the new scope's wait using its actual clock. *)
             Scheduler_recovery.resolve t.scheduler_recovery ticket.kind
           else t.scheduler_recovery);
        scheduler_repair = None;
        merge_queue_ejection;
        automerge_failure_count =
          (if
             accepted && success
             && Scheduler_recovery.equal_kind ticket.kind
                  Scheduler_recovery.Merge_attempt
           then 0
           else t.automerge_failure_count);
      }

(* -- Tests for session failure recovery -- *)

let%test
    "on_session_failure: start path fresh resets to Fresh_available and clears \
     session" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = { t with busy = true; session_fallback = Tried_fresh } in
  let t = on_session_failure t ~is_fresh:true in
  equal_session_fallback t.session_fallback Fresh_available

let%test "on_session_failure: resume failure escalates to Tried_fresh" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = { t with busy = true; session_fallback = Fresh_available } in
  let t = on_session_failure t ~is_fresh:false in
  equal_session_fallback t.session_fallback Tried_fresh

let%test "on_session_failure: respond path fresh escalates to Tried_fresh" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = set_pr_number t (Pr_number.of_int 1) in
  let t = { t with busy = true; session_fallback = Fresh_available } in
  let t = on_session_failure t ~is_fresh:true in
  equal_session_fallback t.session_fallback Tried_fresh

let%test
    "on_session_failure: respond path second fresh failure escalates to \
     Given_up" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = set_pr_number t (Pr_number.of_int 1) in
  let t = { t with busy = true; session_fallback = Tried_fresh } in
  let t = on_session_failure t ~is_fresh:true in
  equal_session_fallback t.session_fallback Given_up

let%test
    "on_session_failure: start fresh failure + complete does not set \
     needs_intervention" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = { t with busy = true; session_fallback = Tried_fresh } in
  let t = on_session_failure t ~is_fresh:true in
  let t = complete t in
  not (needs_intervention t)

let%test "on_pr_discovery_failure increments attempts from zero" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = on_pr_discovery_failure t in
  t.start_attempts_without_pr = 1

let%test "on_pr_discovery_failure increments attempts again" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = on_pr_discovery_failure t in
  let t = on_pr_discovery_failure t in
  t.start_attempts_without_pr = 2

let%test "on_pr_discovery_failure is no-op when agent has a PR" =
  let t = create ~branch:(Branch.of_string "b1") (Patch_id.of_string "1") in
  let t = set_pr_number t (Pr_number.of_int 42) in
  let t = on_pr_discovery_failure t in
  t.start_attempts_without_pr = 0

let reconcile_branch t event =
  let previously_published = branch_published t in
  let branch_reconcile, commands =
    Branch_reconcile.step t.branch_reconcile event
  in
  let obligation_unpublished =
    Option.value_map (Branch_reconcile.operation t.branch_reconcile)
      ~default:false ~f:(fun op ->
        not
          (List.exists (Branch_reconcile.publications t.branch_reconcile)
             ~f:(fun receipt -> receipt.operation_id = op.id)))
  in
  let queue =
    if
      obligation_unpublished
      && (not (Branch_reconcile.is_unsettled branch_reconcile))
      && Option.equal Branch.equal
           (branch_rebased_onto { t with branch_reconcile })
           t.base_branch
    then
      List.filter t.queue ~f:(fun kind ->
          not (Operation_kind.equal kind Operation_kind.Rebase))
    else t.queue
  in
  let t = { t with branch_reconcile; queue } in
  let t =
    if (not previously_published) && branch_published t then
      { t with start_attempts_without_pr = 0 }
    else t
  in
  (t, commands)

let begin_forge_observation t ~request =
  let branch_reconcile, ticket =
    Branch_reconcile.begin_forge_observation t.branch_reconcile ~request
      ~scope:(forge_scope t)
  in
  ({ t with branch_reconcile }, ticket)

let accept_forge_observation ?confirmed_base t ~ticket ~confirmed_head
    observation =
  let branch_reconcile, result =
    Branch_reconcile.accept_forge_observation ?confirmed_base t.branch_reconcile
      ~ticket ~current:(forge_scope t) ~confirmed_head observation
  in
  ({ t with branch_reconcile }, result)

let defer_forge_revision_pair t =
  {
    t with
    merge_ready = false;
    mergeability_unknown = true;
    checks_passing = false;
    queue =
      List.filter t.queue ~f:(fun kind ->
          not
            (Operation_kind.equal kind Operation_kind.Merge_conflict
            || Operation_kind.equal kind Operation_kind.Ci));
  }

let record_session_completion t completion =
  { t with session_completion = Some completion }

let migrate_legacy_branch_state ?(revision = `Null) ~main_branch ~owner_missing
    ~anchors ~published t =
  let t =
    if
      published || (not owner_missing)
      || not (Branch_reconcile.equal t.branch_reconcile Branch_reconcile.empty)
    then t
    else
      let branch_reconcile =
        if t.has_session || has_pr t || Option.is_some t.worktree_path then
          fst
            (Branch_reconcile.step Branch_reconcile.empty
               (Branch_reconcile.Request
                  {
                    base =
                      Branch.to_string
                        (Option.value t.base_branch ~default:main_branch);
                    policy = Preserve_ancestry;
                    purpose = Publish_session "legacy-snapshot-migration";
                  }))
        else Branch_reconcile.empty
      in
      { t with branch_reconcile; no_commits_push_count = 0 }
  in
  {
    t with
    branch_reconcile =
      (let owner =
         Branch_reconcile.import_legacy_anchors ~revision t.branch_reconcile
           anchors
       in
       if published then
         Branch_reconcile.import_legacy_publication owner
           ~base:
             (Branch.to_string
                (Option.value t.base_branch ~default:main_branch))
       else owner);
  }

let resolve_scheduler t kind =
  {
    t with
    scheduler_recovery = Scheduler_recovery.resolve t.scheduler_recovery kind;
  }

let recover_scheduler_owner t ~owned =
  let running = if owned then t.scheduler_repair else None in
  let scheduler_recovery =
    Scheduler_recovery.recover t.scheduler_recovery ~running
  in
  let lost = (not owned) && Option.is_some t.scheduler_repair in
  let t =
    {
      t with
      scheduler_recovery;
      scheduler_repair = running;
      busy = (if lost then false else t.busy);
      current_op = (if lost then None else t.current_op);
      current_message_id = (if lost then None else t.current_message_id);
    }
  in
  if
    Option.is_some (Scheduler_recovery.pending scheduler_recovery)
    && Option.is_none (Scheduler_recovery.attention scheduler_recovery)
  then enqueue t Operation_kind.Human
  else t
