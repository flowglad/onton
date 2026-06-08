(* @archlint.module test
   @archlint.domain pr-state *)

open Onton_core

let merge_ready_matches_component_predicates =
  QCheck2.Test.make ~name:"merge_ready_of is exactly its component gate"
    ~count:200
    QCheck2.Gen.(
      triple
        (oneof_list
           [ Pr_state.Mergeable; Pr_state.Conflicting; Pr_state.Unknown ])
        (oneof_list [ Pr_state.Passing; Pr_state.Failing; Pr_state.Pending ])
        (option string_small))
    (fun (merge_state, check_status, review_decision) ->
      Bool.equal
        (Pr_state.merge_ready_of ~merge_state ~check_status ~review_decision)
        (Pr_state.equal_merge_state merge_state Pr_state.Mergeable
        && Pr_state.equal_check_status check_status Pr_state.Passing
        && not (Pr_state.review_blocking review_decision)))

(* A baseline PR state with neutral fields. Each property generates inputs and
   overrides only the relevant fields, then asserts the predicate's exact
   semantics. *)
let base : Pr_state.t =
  {
    status = Pr_state.Open;
    is_draft = false;
    merge_state = Pr_state.Unknown;
    merge_ready = false;
    merge_ready_divergence = None;
    review_decision = None;
    check_status = Pr_state.Pending;
    ci_checks = [];
    ci_checks_truncated = false;
    comments = [];
    unresolved_comment_count = 0;
    findings = [];
    node_id = None;
    merge_queue_required = false;
    merge_queue_entry = None;
    head_branch = None;
    head_oid = None;
    merge_commit_sha = None;
    base_branch = None;
    is_fork = false;
  }

let gen_status =
  QCheck2.Gen.oneof_list [ Pr_state.Open; Pr_state.Merged; Pr_state.Closed ]

let gen_merge_state =
  QCheck2.Gen.oneof_list
    [ Pr_state.Mergeable; Pr_state.Conflicting; Pr_state.Unknown ]

let gen_check_status =
  QCheck2.Gen.oneof_list
    [ Pr_state.Passing; Pr_state.Failing; Pr_state.Pending ]

(* merged / closed: track the status field exactly. *)
let prop_merged_closed_track_status =
  QCheck2.Test.make ~name:"merged/closed reflect the status field" ~count:300
    gen_status (fun status ->
      let st = { base with Pr_state.status } in
      Bool.equal (Pr_state.merged st)
        (Pr_state.equal_pr_status status Pr_state.Merged)
      && Bool.equal (Pr_state.closed st)
           (Pr_state.equal_pr_status status Pr_state.Closed))

(* mergeable: Open AND merge_state = Mergeable. has_conflict: Conflicting. *)
let prop_mergeable_and_conflict =
  QCheck2.Test.make
    ~name:"mergeable iff Open & Mergeable; has_conflict iff Conflicting"
    ~count:300
    QCheck2.Gen.(pair gen_status gen_merge_state)
    (fun (status, merge_state) ->
      let st = { base with Pr_state.status; merge_state } in
      Bool.equal (Pr_state.mergeable st)
        (Pr_state.equal_pr_status status Pr_state.Open
        && Pr_state.equal_merge_state merge_state Pr_state.Mergeable)
      && Bool.equal (Pr_state.has_conflict st)
           (Pr_state.equal_merge_state merge_state Pr_state.Conflicting))

(* checks_passing / ci_failed: track check_status. *)
let prop_check_predicates_track_status =
  QCheck2.Test.make ~name:"checks_passing/ci_failed reflect check_status"
    ~count:300 gen_check_status (fun cs ->
      let st = { base with Pr_state.check_status = cs } in
      Bool.equal
        (Pr_state.checks_passing st)
        (Pr_state.equal_check_status cs Pr_state.Passing)
      && Bool.equal (Pr_state.ci_failed st)
           (Pr_state.equal_check_status cs Pr_state.Failing))

(* merge_ready: Open AND the merge_ready bit. *)
let prop_merge_ready_gated_on_open =
  QCheck2.Test.make ~name:"merge_ready iff Open & merge_ready bit set"
    ~count:300
    QCheck2.Gen.(pair gen_status bool)
    (fun (status, ready) ->
      let st = { base with Pr_state.status; merge_ready = ready } in
      Bool.equal (Pr_state.merge_ready st)
        (Pr_state.equal_pr_status status Pr_state.Open && ready))

(* is_draft / is_fork: identity over their bool fields. *)
let prop_draft_fork_identity =
  QCheck2.Test.make ~name:"is_draft/is_fork are field identities" ~count:300
    QCheck2.Gen.(pair bool bool)
    (fun (draft, fork) ->
      let st = { base with Pr_state.is_draft = draft; is_fork = fork } in
      Bool.equal (Pr_state.is_draft st) draft
      && Bool.equal (Pr_state.is_fork st) fork)

(* no_unresolved_comments: count = 0. *)
let prop_no_unresolved_comments =
  QCheck2.Test.make ~name:"no_unresolved_comments iff count = 0" ~count:300
    QCheck2.Gen.(int_range 0 50)
    (fun n ->
      let st = { base with Pr_state.unresolved_comment_count = n } in
      Bool.equal (Pr_state.no_unresolved_comments st) (n = 0))

(* requires_merge_queue: the merge_queue_required flag. enqueued: an entry is
   present. *)
let prop_merge_queue_predicates =
  QCheck2.Test.make ~name:"requires_merge_queue/enqueued reflect their fields"
    ~count:300
    QCheck2.Gen.(pair bool bool)
    (fun (required, has_entry) ->
      let entry =
        if has_entry then
          Some { Pr_state.id = "e"; state = Pr_state.Mq_queued; position = 0 }
        else None
      in
      let st =
        {
          base with
          Pr_state.merge_queue_required = required;
          merge_queue_entry = entry;
        }
      in
      Bool.equal (Pr_state.requires_merge_queue st) required
      && Bool.equal (Pr_state.enqueued st) has_entry)

(* derive_check_status: a non-empty list of "success" conclusions is Passing;
   any "failure" conclusion makes it Failing. *)
let gen_ci_check conclusion =
  {
    Types.Ci_check.name = "c";
    conclusion;
    details_url = None;
    description = None;
    started_at = None;
    id = None;
  }

let prop_derive_check_status =
  QCheck2.Test.make
    ~name:"derive_check_status: failure dominates, all-success passes"
    ~count:300
    QCheck2.Gen.(
      list_size (int_range 1 6) (oneof_list [ "success"; "failure" ]))
    (fun conclusions ->
      let checks = List.map (fun c -> gen_ci_check c) conclusions in
      let result = Pr_state.derive_check_status checks in
      let has_failure =
        List.exists (fun c -> String.equal c "failure") conclusions
      in
      if has_failure then Pr_state.equal_check_status result Pr_state.Failing
      else Pr_state.equal_check_status result Pr_state.Passing)

(* merge_ready_divergence_of: Some iff a status was reported and the verdicts
   disagree (github_ready = status="CLEAN"). *)
let prop_merge_ready_divergence =
  QCheck2.Test.make
    ~name:"merge_ready_divergence_of: Some iff status present & disagree"
    ~count:300
    QCheck2.Gen.(
      pair bool (option (oneof_list [ "CLEAN"; "BLOCKED"; "BEHIND" ])))
    (fun (merge_ready, github_merge_state_status) ->
      let result =
        Pr_state.merge_ready_divergence_of ~merge_ready
          ~github_merge_state_status
      in
      match github_merge_state_status with
      | None -> Option.is_none result
      | Some s -> (
          let github_ready = String.equal s "CLEAN" in
          if Bool.equal merge_ready github_ready then Option.is_none result
          else
            match result with
            | Some d -> Bool.equal d.Pr_state.derived_merge_ready merge_ready
            | None -> false))

let () =
  QCheck2.Test.check_exn merge_ready_matches_component_predicates;
  QCheck2.Test.check_exn prop_merged_closed_track_status;
  QCheck2.Test.check_exn prop_mergeable_and_conflict;
  QCheck2.Test.check_exn prop_check_predicates_track_status;
  QCheck2.Test.check_exn prop_merge_ready_gated_on_open;
  QCheck2.Test.check_exn prop_draft_fork_identity;
  QCheck2.Test.check_exn prop_no_unresolved_comments;
  QCheck2.Test.check_exn prop_merge_queue_predicates;
  QCheck2.Test.check_exn prop_derive_check_status;
  QCheck2.Test.check_exn prop_merge_ready_divergence
