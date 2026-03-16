open Base
open Types
open Operation_kind

type pending_comment = { comment : Comment.t; valid : bool }
[@@deriving show, eq, sexp_of, compare]

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
  session_failed : bool;
  pending_comments : pending_comment list;
  last_session_id : Session_id.t option;
  tried_fresh : bool;
}
[@@deriving show, eq, sexp_of, compare]

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
    session_failed = false;
    pending_comments = [];
    last_session_id = None;
    tried_fresh = false;
  }

let highest_priority t =
  List.min_elt t.queue ~compare:(fun a b ->
      Int.compare (Priority.priority a) (Priority.priority b))

let enqueue t k =
  if List.mem t.queue k ~equal:Operation_kind.equal then t
  else { t with queue = k :: t.queue }

let mark_merged t = { t with merged = true }

let add_pending_comment t comment ~valid =
  let already_present =
    List.exists t.pending_comments ~f:(fun pc ->
        Comment_id.equal pc.comment.Comment.id comment.Comment.id
        || Comment_id.to_int comment.Comment.id < 0
           && Comment_id.to_int pc.comment.Comment.id < 0
           && String.equal pc.comment.Comment.body comment.Comment.body
           && Option.equal String.equal pc.comment.Comment.path
                comment.Comment.path
           && Option.equal Int.equal pc.comment.Comment.line
                comment.Comment.line)
  in
  if already_present then t
  else { t with pending_comments = { comment; valid } :: t.pending_comments }

let set_session_failed t = { t with session_failed = true }
let set_last_session_id t id = { t with last_session_id = Some id }
let set_tried_fresh t = { t with tried_fresh = true }

let clear_session_fallback t =
  { t with tried_fresh = false; session_failed = false }

let set_has_conflict t = { t with has_conflict = true }

let increment_ci_failure_count t =
  { t with ci_failure_count = t.ci_failure_count + 1 }

let clear_needs_intervention t =
  {
    t with
    needs_intervention = false;
    tried_fresh = false;
    session_failed = false;
  }

let restore ~patch_id ~has_pr ~pr_number ~has_session ~busy ~merged
    ~needs_intervention ~queue ~satisfies ~changed ~has_conflict ~base_branch
    ~ci_failure_count ~session_failed ~pending_comments ~last_session_id
    ~tried_fresh =
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
    session_failed;
    pending_comments;
    last_session_id;
    tried_fresh;
  }

let restore_pending_comment ~comment ~valid = { comment; valid }
let set_pr_number t pr_number = { t with pr_number = Some pr_number }

let start t ~base_branch =
  if t.has_pr then invalid_arg "Patch_agent.start: patch already has a PR";
  {
    t with
    has_pr = true;
    has_session = true;
    busy = true;
    satisfies = true;
    base_branch = Some base_branch;
    tried_fresh = false;
  }

let respond t k =
  if not t.has_pr then invalid_arg "Patch_agent.respond: patch has no PR";
  if t.merged then invalid_arg "Patch_agent.respond: patch is merged";
  if t.busy then invalid_arg "Patch_agent.respond: patch is busy";
  if t.needs_intervention then
    invalid_arg "Patch_agent.respond: patch needs intervention";
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
    queue;
    satisfies;
    changed;
    has_conflict;
    pending_comments;
  }

let complete t =
  if not t.busy then invalid_arg "Patch_agent.complete: patch is not busy";
  let needs_intervention =
    if List.mem t.queue Operation_kind.Human ~equal:Operation_kind.equal then
      false
    else t.ci_failure_count >= 3 || t.session_failed
  in
  { t with busy = false; needs_intervention }
