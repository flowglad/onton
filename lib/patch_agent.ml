open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Types
open Operation_kind

type pending_comment = { comment : Comment.t; valid : bool }
[@@deriving show, eq, sexp_of, compare, yojson]

type session_fallback = Fresh_available | Tried_fresh | Given_up
[@@deriving show, eq, sexp_of, compare, yojson]

type t = {
  patch_id : Patch_id.t;
  has_pr : bool;
  pr_number : Pr_number.t option;
  has_session : bool;
  busy : bool;
  merged : bool;
  needs_intervention : bool;
  queue : Operation_kind.t list;
  satisfies : bool;
  changed : bool;
  has_conflict : bool;
  base_branch : Branch.t option;
  ci_failure_count : int;
  session_fallback : session_fallback;
  pending_comments : pending_comment list;
  ci_checks : Ci_check.t list;
  addressed_comment_ids : Set.M(Comment_id).t;
      [@equal Set.equal]
      [@compare Set.compare_direct]
      [@sexp_of fun s -> Set.sexp_of_m__t (module Comment_id) s]
  removed : bool;
  mergeable : bool;
  merge_ready : bool;
  checks_passing : bool;
  no_unresolved_comments : bool;
  current_op : Operation_kind.t option;
  worktree_path : string option;
}
[@@deriving eq, sexp_of, compare]

let pp fmt t = Sexp.pp_hum fmt (sexp_of_t t)
let show t = Sexp.to_string_hum (sexp_of_t t)

let create patch_id =
  {
    patch_id;
    has_pr = false;
    pr_number = None;
    has_session = false;
    busy = false;
    merged = false;
    needs_intervention = false;
    queue = [];
    satisfies = false;
    changed = false;
    has_conflict = false;
    base_branch = None;
    ci_failure_count = 0;
    session_fallback = Fresh_available;
    pending_comments = [];
    ci_checks = [];
    addressed_comment_ids = Set.empty (module Comment_id);
    removed = false;
    mergeable = false;
    merge_ready = false;
    checks_passing = false;
    no_unresolved_comments = false;
    current_op = None;
    worktree_path = None;
  }

let create_adhoc ~patch_id ~pr_number =
  {
    patch_id;
    has_pr = true;
    pr_number = Some pr_number;
    has_session = false;
    busy = false;
    merged = false;
    needs_intervention = false;
    queue = [];
    satisfies = false;
    changed = false;
    has_conflict = false;
    base_branch = None;
    ci_failure_count = 0;
    session_fallback = Fresh_available;
    pending_comments = [];
    ci_checks = [];
    addressed_comment_ids = Set.empty (module Comment_id);
    removed = false;
    mergeable = false;
    merge_ready = false;
    checks_passing = false;
    no_unresolved_comments = false;
    current_op = None;
    worktree_path = None;
  }

let highest_priority t =
  List.min_elt t.queue ~compare:(fun a b ->
      Int.compare (Priority.priority a) (Priority.priority b))

let enqueue t k =
  if List.mem t.queue k ~equal:Operation_kind.equal then t
  else { t with queue = k :: t.queue }

let mark_merged t = { t with merged = true }
let mark_removed t = { t with removed = true }

let add_pending_comment t comment ~valid =
  let is_synthetic id = Comment_id.to_int id < 0 in
  let is_content_match (a : Comment.t) (b : Comment.t) =
    String.equal a.body b.body
    && Option.equal String.equal a.path b.path
    && Option.equal Int.equal a.line b.line
  in
  (* Migration: if a pending comment has a synthetic ID and the incoming
     comment has a real ID with matching content, upgrade the synthetic entry's
     ID in-place so subsequent polls match by ID directly.
     Assumption: the GraphQL query only fetches review-thread comments, which
     always have path/line set. If PR-level comments (path=None, line=None)
     are ever fetched, this could falsely match a human message. *)
  let migration_matched = ref false in
  let pending_comments =
    List.map t.pending_comments ~f:(fun pc ->
        if
          is_synthetic pc.comment.Comment.id
          && (not (is_synthetic comment.Comment.id))
          && is_content_match pc.comment comment
        then (
          migration_matched := true;
          {
            comment = { pc.comment with Comment.id = comment.Comment.id };
            valid;
          })
        else pc)
  in
  let t = { t with pending_comments } in
  let already_present =
    !migration_matched
    || List.exists t.pending_comments ~f:(fun pc ->
        if is_synthetic pc.comment.Comment.id then
          is_content_match pc.comment comment
        else Comment_id.equal pc.comment.Comment.id comment.Comment.id)
  in
  if already_present then t
  else { t with pending_comments = { comment; valid } :: t.pending_comments }

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
    - Respond path fresh failure: escalate to Given_up → needs_intervention *)
let on_session_failure t ~is_fresh =
  if (not t.has_pr) && is_fresh then
    (* Start path fresh failure: full reset for clean retry *)
    { t with session_fallback = Fresh_available }
  else
    let t = set_session_failed t in
    if is_fresh then set_tried_fresh t else t

(** Handle a successful Claude run where PR discovery failed. Reset fallback so
    the patch retries from scratch. *)
let on_pr_discovery_failure t = { t with session_fallback = Fresh_available }

let set_has_conflict t = { t with has_conflict = true }
let clear_has_conflict t = { t with has_conflict = false }
let set_base_branch t branch = { t with base_branch = Some branch }
let set_mergeable t v = { t with mergeable = v }
let set_merge_ready t v = { t with merge_ready = v }
let set_checks_passing t v = { t with checks_passing = v }
let set_no_unresolved_comments t v = { t with no_unresolved_comments = v }
let set_worktree_path t path = { t with worktree_path = Some path }

let is_approved t =
  t.has_pr && t.merge_ready && (not t.busy) && not t.needs_intervention

let increment_ci_failure_count t =
  { t with ci_failure_count = t.ci_failure_count + 1 }

let set_ci_checks t checks = { t with ci_checks = checks }

let add_addressed_comment_id t id =
  { t with addressed_comment_ids = Set.add t.addressed_comment_ids id }

let is_comment_addressed t id = Set.mem t.addressed_comment_ids id

let clear_needs_intervention t =
  { t with needs_intervention = false; session_fallback = Fresh_available }

let reset_busy t =
  if not t.busy then t
  else
    let needs_intervention =
      if List.mem t.queue Operation_kind.Human ~equal:Operation_kind.equal then
        false
      else
        t.ci_failure_count >= 3
        || equal_session_fallback t.session_fallback Given_up
    in
    { t with busy = false; current_op = None; needs_intervention }

let restore ~patch_id ~has_pr ~pr_number ~has_session ~busy ~merged
    ~needs_intervention ~queue ~satisfies ~changed ~has_conflict ~base_branch
    ~ci_failure_count ~session_fallback ~pending_comments ~ci_checks
    ~addressed_comment_ids ~removed ~mergeable ~merge_ready ~checks_passing
    ~no_unresolved_comments ~worktree_path =
  {
    patch_id;
    has_pr;
    pr_number;
    has_session;
    busy;
    merged;
    needs_intervention;
    queue;
    satisfies;
    changed;
    has_conflict;
    base_branch;
    ci_failure_count;
    session_fallback;
    pending_comments;
    ci_checks;
    addressed_comment_ids;
    removed;
    mergeable;
    merge_ready;
    checks_passing;
    no_unresolved_comments;
    current_op = None;
    worktree_path;
  }

let restore_pending_comment ~comment ~valid = { comment; valid }

let set_pr_number t pr_number =
  { t with pr_number = Some pr_number; has_pr = true }

let start t ~base_branch =
  if t.has_pr then invalid_arg "Patch_agent.start: patch already has a PR";
  if t.busy then invalid_arg "Patch_agent.start: patch is already busy";
  {
    t with
    has_session = true;
    busy = true;
    current_op = None;
    satisfies = true;
    base_branch = Some base_branch;
    session_fallback = Fresh_available;
    ci_checks = [];
    addressed_comment_ids = Set.empty (module Comment_id);
  }

let rebase t ~base_branch =
  if not t.has_pr then invalid_arg "Patch_agent.rebase: patch has no PR";
  if t.merged then invalid_arg "Patch_agent.rebase: patch is merged";
  if t.removed then invalid_arg "Patch_agent.rebase: patch is removed";
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
    busy = true;
    current_op = Some Rebase;
    queue;
    base_branch = Some base_branch;
    mergeable = false;
    merge_ready = false;
    checks_passing = false;
    no_unresolved_comments = false;
  }

let respond t k =
  if not t.has_pr then invalid_arg "Patch_agent.respond: patch has no PR";
  if t.merged then invalid_arg "Patch_agent.respond: patch is merged";
  if t.removed then invalid_arg "Patch_agent.respond: patch is removed";
  if t.busy then invalid_arg "Patch_agent.respond: patch is busy";
  if t.needs_intervention then
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
  let is_merge_conflict = equal_k Merge_conflict in
  let satisfies = if is_human then false else t.satisfies in
  let changed =
    if is_ci then true
    else if is_review then
      t.changed || List.exists t.pending_comments ~f:(fun c -> c.valid)
    else t.changed
  in
  let has_conflict = if is_merge_conflict then false else t.has_conflict in
  let pending_comments = if is_review then [] else t.pending_comments in
  {
    t with
    has_session = true;
    busy = true;
    current_op = Some k;
    queue;
    satisfies;
    changed;
    has_conflict;
    pending_comments;
    mergeable = false;
    merge_ready = false;
    checks_passing = false;
    no_unresolved_comments = false;
  }

let complete t =
  if not t.busy then invalid_arg "Patch_agent.complete: patch is not busy";
  let needs_intervention =
    if List.mem t.queue Operation_kind.Human ~equal:Operation_kind.equal then
      false
    else
      t.ci_failure_count >= 3
      || equal_session_fallback t.session_fallback Given_up
  in
  { t with busy = false; current_op = None; needs_intervention }

(* -- Tests for session failure recovery -- *)

let%test
    "on_session_failure: start path fresh resets to Fresh_available and clears \
     session" =
  let t = create (Patch_id.of_string "1") in
  let t = { t with busy = true; session_fallback = Tried_fresh } in
  let t = on_session_failure t ~is_fresh:true in
  equal_session_fallback t.session_fallback Fresh_available

let%test "on_session_failure: resume failure escalates to Tried_fresh" =
  let t = create (Patch_id.of_string "1") in
  let t = { t with busy = true; session_fallback = Fresh_available } in
  let t = on_session_failure t ~is_fresh:false in
  equal_session_fallback t.session_fallback Tried_fresh

let%test "on_session_failure: respond path fresh escalates to Given_up" =
  let t = create (Patch_id.of_string "1") in
  let t =
    { t with busy = true; has_pr = true; session_fallback = Tried_fresh }
  in
  let t = on_session_failure t ~is_fresh:true in
  equal_session_fallback t.session_fallback Given_up

let%test
    "on_session_failure: start fresh failure + complete does not set \
     needs_intervention" =
  let t = create (Patch_id.of_string "1") in
  let t = { t with busy = true; session_fallback = Tried_fresh } in
  let t = on_session_failure t ~is_fresh:true in
  let t = complete t in
  not t.needs_intervention

let%test "on_pr_discovery_failure resets fallback and clears session" =
  let t = create (Patch_id.of_string "1") in
  let t = { t with busy = true; session_fallback = Tried_fresh } in
  let t = on_pr_discovery_failure t in
  equal_session_fallback t.session_fallback Fresh_available
