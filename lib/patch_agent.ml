open Base
open Types
open Operation_kind

type session_fallback = Fresh_available | Tried_fresh | Given_up
[@@deriving show, eq, sexp_of, compare, yojson]

type t = {
  patch_id : Patch_id.t;
  branch : Branch.t;
  pr_number : Pr_number.t option;
  has_session : bool;
  busy : bool;
  merged : bool;
  queue : Operation_kind.t list;
  satisfies : bool;
  changed : bool;
  has_conflict : bool;
  base_branch : Branch.t option;
  notified_base_branch : Branch.t option;
  ci_failure_count : int;
  session_fallback : session_fallback;
  human_messages : string list;
  inflight_human_messages : string list;
  ci_checks : Ci_check.t list;
  merge_ready : bool;
  is_draft : bool;
  pr_body_delivered : bool;
  start_attempts_without_pr : int;
  conflict_noop_count : int;
  no_commits_push_count : int;
  branch_rebased_onto : Branch.t option;
  checks_passing : bool;
  current_op : Operation_kind.t option;
  current_message_id : Message_id.t option;
  generation : int;
  worktree_path : string option;
  branch_blocked : bool;
  llm_session_id : string option;
}
[@@deriving eq, sexp_of, compare]

let pp fmt t = Sexp.pp_hum fmt (sexp_of_t t)
let show t = Sexp.to_string_hum (sexp_of_t t)
let has_pr t = Option.is_some t.pr_number

let needs_intervention t =
  let human_in_queue =
    List.mem t.queue Operation_kind.Human ~equal:Operation_kind.equal
  in
  (* The Human exemption lets a newly-arrived human message be delivered
     even to an agent with a high ci_failure_count or other failure state.
     However, the exemption does NOT apply when session_fallback = Given_up:
     a Given_up agent cannot start any session, so the delivery attempt
     immediately fails at the Give_up check and complete_failed re-enqueues
     Human — creating an infinite loop.  Override the exemption so the
     reconciler stops scheduling actions and the agent surfaces for
     manual intervention. *)
  let given_up = equal_session_fallback t.session_fallback Given_up in
  given_up
  || ((not human_in_queue)
     && (t.ci_failure_count >= 3
        || ((not (has_pr t)) && t.start_attempts_without_pr >= 2)
        || t.conflict_noop_count >= 2
        || t.no_commits_push_count >= 2))

let create ~branch patch_id =
  {
    patch_id;
    branch;
    pr_number = None;
    has_session = false;
    busy = false;
    merged = false;
    queue = [];
    satisfies = false;
    changed = false;
    has_conflict = false;
    base_branch = None;
    notified_base_branch = None;
    ci_failure_count = 0;
    session_fallback = Fresh_available;
    human_messages = [];
    inflight_human_messages = [];
    ci_checks = [];
    merge_ready = false;
    is_draft = false;
    pr_body_delivered = false;
    start_attempts_without_pr = 0;
    conflict_noop_count = 0;
    no_commits_push_count = 0;
    branch_rebased_onto = None;
    checks_passing = false;
    current_op = None;
    current_message_id = None;
    generation = 0;
    worktree_path = None;
    branch_blocked = false;
    llm_session_id = None;
  }

let create_adhoc ~patch_id ~branch ~pr_number =
  {
    patch_id;
    branch;
    pr_number = Some pr_number;
    has_session = false;
    busy = false;
    merged = false;
    queue = [];
    satisfies = false;
    changed = false;
    has_conflict = false;
    base_branch = None;
    notified_base_branch = None;
    ci_failure_count = 0;
    session_fallback = Fresh_available;
    human_messages = [];
    inflight_human_messages = [];
    ci_checks = [];
    merge_ready = false;
    is_draft = false;
    pr_body_delivered = true;
    start_attempts_without_pr = 0;
    conflict_noop_count = 0;
    no_commits_push_count = 0;
    branch_rebased_onto = None;
    checks_passing = false;
    current_op = None;
    current_message_id = None;
    generation = 0;
    worktree_path = None;
    branch_blocked = false;
    llm_session_id = None;
  }

let highest_priority t =
  List.min_elt t.queue ~compare:(fun a b ->
      Int.compare (Priority.priority a) (Priority.priority b))

let enqueue t k =
  if List.mem t.queue k ~equal:Operation_kind.equal then t
  else { t with queue = k :: t.queue }

let mark_merged t = { t with merged = true }

let add_human_message t msg =
  { t with human_messages = msg :: t.human_messages }

let add_human_messages t msgs =
  { t with human_messages = List.append t.human_messages msgs }

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
    - Start path (no PR) + fresh failure: reset to Fresh_available for retry
    - Resume failure: escalate to Tried_fresh (will try fresh next)
    - Fresh failure (respond path): escalate one step via set_tried_fresh *)
let on_session_failure t ~is_fresh =
  if (not (has_pr t)) && is_fresh then
    (* Start path fresh failure: full reset for clean retry *)
    { t with session_fallback = Fresh_available; llm_session_id = None }
  else if is_fresh then set_tried_fresh t
  else set_session_failed t

let set_has_conflict t = { t with has_conflict = true }
let clear_has_conflict t = { t with has_conflict = false }
let reset_conflict_noop_count t = { t with conflict_noop_count = 0 }

let increment_conflict_noop_count t =
  { t with conflict_noop_count = t.conflict_noop_count + 1 }

let increment_no_commits_push_count t =
  { t with no_commits_push_count = t.no_commits_push_count + 1 }

let reset_no_commits_push_count t = { t with no_commits_push_count = 0 }

let set_base_branch t branch =
  let notified =
    if t.has_session then
      match t.notified_base_branch with None -> Some branch | some -> some
    else t.notified_base_branch
  in
  { t with base_branch = Some branch; notified_base_branch = notified }

let set_notified_base_branch t branch =
  { t with notified_base_branch = Some branch }

let base_branch_changed t =
  match (t.notified_base_branch, t.base_branch) with
  | Some old_base, Some new_base -> not (Branch.equal old_base new_base)
  | _ -> false

let set_merge_ready t v = { t with merge_ready = v }
let set_is_draft t v = { t with is_draft = v }
let set_pr_body_delivered t v = { t with pr_body_delivered = v }

let increment_start_attempts_without_pr t =
  { t with start_attempts_without_pr = t.start_attempts_without_pr + 1 }

(** Handle a successful Claude run where PR discovery failed by recording a
    durable attempt. The controller derives intervention from this fact. No-op
    when the agent already has a PR — reruns after the first session should not
    accumulate this counter. *)
let on_pr_discovery_failure t =
  if has_pr t then t else increment_start_attempts_without_pr t

let on_pre_session_failure t =
  if has_pr t then t else increment_start_attempts_without_pr t

let set_checks_passing t v = { t with checks_passing = v }
let set_worktree_path t path = { t with worktree_path = Some path }

let is_approved t ~main_branch =
  has_pr t && t.merge_ready && (not t.busy)
  && (not (needs_intervention t))
  && (not t.is_draft) && (not t.branch_blocked)
  && Option.equal Branch.equal t.base_branch (Some main_branch)

let increment_ci_failure_count t =
  { t with ci_failure_count = t.ci_failure_count + 1 }

let reset_ci_failure_count t = { t with ci_failure_count = 0 }
let set_ci_checks t checks = { t with ci_checks = checks }
let set_branch_blocked t = { t with branch_blocked = true }
let clear_branch_blocked t = { t with branch_blocked = false }
let set_current_message_id t current_message_id = { t with current_message_id }
let bump_generation t = { t with generation = t.generation + 1 }
let set_llm_session_id t llm_session_id = { t with llm_session_id }

let resume_current_message t ~op =
  { t with busy = true; has_session = true; current_op = op }

let reset_intervention_state t =
  {
    t with
    session_fallback = Fresh_available;
    ci_failure_count = 0;
    start_attempts_without_pr = 0;
    conflict_noop_count = 0;
    no_commits_push_count = 0;
  }

let reset_busy t = if not t.busy then t else { t with busy = false }

let restore ~patch_id ~branch ~pr_number ~has_session ~busy ~merged ~queue
    ~satisfies ~changed ~has_conflict ~base_branch ~notified_base_branch
    ~ci_failure_count ~session_fallback ~human_messages ~inflight_human_messages
    ~ci_checks ~merge_ready ~is_draft ~pr_body_delivered
    ~start_attempts_without_pr ~conflict_noop_count ~no_commits_push_count
    ~branch_rebased_onto ~checks_passing ~current_op ~current_message_id
    ~generation ~worktree_path ~branch_blocked ~llm_session_id =
  {
    patch_id;
    branch;
    pr_number;
    has_session;
    busy;
    merged;
    queue;
    satisfies;
    changed;
    has_conflict;
    base_branch;
    notified_base_branch;
    ci_failure_count;
    session_fallback;
    human_messages;
    inflight_human_messages;
    ci_checks;
    merge_ready;
    is_draft;
    pr_body_delivered;
    start_attempts_without_pr;
    conflict_noop_count;
    no_commits_push_count;
    branch_rebased_onto;
    checks_passing;
    current_op;
    current_message_id;
    generation;
    worktree_path;
    branch_blocked;
    llm_session_id;
  }

let set_pr_number t pr_number =
  {
    t with
    pr_number = Some pr_number;
    is_draft = true;
    pr_body_delivered = false;
    start_attempts_without_pr = 0;
  }

let clear_pr t =
  {
    t with
    pr_number = None;
    is_draft = false;
    merge_ready = false;
    checks_passing = false;
    ci_checks = [];
    ci_failure_count = 0;
    base_branch = None;
    notified_base_branch = None;
  }

let start t ~base_branch =
  if has_pr t then invalid_arg "Patch_agent.start: patch already has a PR";
  if t.busy then invalid_arg "Patch_agent.start: patch is already busy";
  {
    t with
    has_session = true;
    busy = true;
    current_op = None;
    current_message_id = None;
    satisfies = true;
    base_branch = Some base_branch;
    notified_base_branch = Some base_branch;
    (* The initial Start plants the branch on [base_branch]; the local branch
       tip is literally this base's HEAD until the agent commits. Record it
       so the drift detector knows the branch is on the right base. *)
    branch_rebased_onto = Some base_branch;
    ci_checks = [];
  }

let set_branch_rebased_onto t branch =
  { t with branch_rebased_onto = Some branch }

let rebase t ~base_branch =
  if not (has_pr t) then invalid_arg "Patch_agent.rebase: patch has no PR";
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
    current_message_id = None;
    queue;
    base_branch = Some base_branch;
    notified_base_branch =
      (match t.notified_base_branch with
      | None -> Some base_branch
      | some -> some);
    merge_ready = false;
    checks_passing = false;
  }

let respond t k =
  if not (has_pr t) then invalid_arg "Patch_agent.respond: patch has no PR";
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
  {
    t with
    has_session = true;
    busy = true;
    current_op = Some k;
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
    checks_passing = false;
  }

let complete t =
  if not t.busy then t
  else
    {
      t with
      busy = false;
      current_op = None;
      current_message_id = None;
      inflight_human_messages = [];
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
