(* @archlint.module test
   @archlint.domain merge-queue-decision *)

open Base
open Onton_core
open Onton_core.Types

let gen_string =
  QCheck2.Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 1 16))

let gen_operation_kind =
  QCheck2.Gen.oneof_list
    Operation_kind.
      [ Human; Ci; Review_comments; Findings; Pr_body; Merge_conflict; Rebase ]

let gen_queue = QCheck2.Gen.(list_size (int_range 0 5) gen_operation_kind)

let gen_ci_check =
  let open QCheck2.Gen in
  let conclusions =
    Ci_check.failure_conclusions @ Ci_check.success_conclusions
    @ [ "cancelled"; "queued"; "in_progress"; "unknown" ]
  in
  map5
    (fun name conclusion details_url description id ->
      Ci_check.
        { name; conclusion; details_url; description; started_at = None; id })
    gen_string (oneof_list conclusions) (option gen_string) (option gen_string)
    (option (int_range 1 999_999))

let gen_ci_checks = QCheck2.Gen.(list_size (int_range 0 8) gen_ci_check)

let gen_merge_queue_entry =
  let open QCheck2.Gen in
  let states =
    Pr_state.
      [ Mq_queued; Mq_awaiting_checks; Mq_mergeable; Mq_unmergeable; Mq_locked ]
  in
  map3
    (fun id state position -> Pr_state.{ id; state; position })
    gen_string (oneof_list states) (int_range 0 99)

let gen_previous_entry = QCheck2.Gen.option gen_merge_queue_entry

let gen_poll_result =
  let open QCheck2.Gen in
  let* queue = gen_queue in
  let* merged = bool in
  let* closed = bool in
  let* is_draft = bool in
  let* merge_ready = bool in
  let* merge_queue_required = bool in
  let* merge_queue_entry = gen_previous_entry in
  let* checks_passing = bool in
  let* ci_checks = gen_ci_checks in
  let merge_state =
    if merge_ready then Pr_state.Mergeable else Pr_state.Unknown
  in
  return
    Poller.
      {
        queue;
        merged;
        closed;
        is_draft;
        merge_state;
        merge_ready;
        review_decision = None;
        merge_queue_required;
        merge_queue_entry;
        checks_passing;
        ci_checks;
        merge_commit_sha = None;
      }

let failing_check =
  Ci_check.
    {
      name = "failing";
      conclusion = "failure";
      details_url = None;
      description = None;
      started_at = None;
      id = Some 1;
    }

let base_ejection_poll =
  Poller.
    {
      queue = [];
      merged = false;
      closed = false;
      is_draft = false;
      merge_state = Pr_state.Mergeable;
      merge_ready = true;
      review_decision = Some "APPROVED";
      merge_queue_required = true;
      merge_queue_entry = None;
      checks_passing = true;
      ci_checks = [];
      merge_commit_sha = None;
    }

let previous_entry =
  Pr_state.{ id = "MQE_1"; state = Mq_awaiting_checks; position = 1 }

let visible_failure (poll : Poller.t) =
  List.exists poll.ci_checks ~f:Ci_check.is_failure

let expected_ejection ~previous_entry (poll : Poller.t) =
  Option.is_some previous_entry
  && poll.merge_queue_required
  && Option.is_none poll.merge_queue_entry
  && (not poll.merged) && (not poll.closed) && poll.merge_ready
  && poll.checks_passing
  && not (visible_failure poll)

let expected_unmergeable (poll : Poller.t) =
  let has_unmergeable_entry =
    Option.exists poll.merge_queue_entry ~f:(fun entry ->
        Pr_state.equal_merge_queue_entry_state entry.state
          Pr_state.Mq_unmergeable)
  in
  poll.merge_queue_required && has_unmergeable_entry && (not poll.merged)
  && (not poll.closed) && poll.checks_passing
  && not (visible_failure poll)

let prop_totality =
  QCheck2.Test.make ~name:"MQD total over arbitrary prior entry + poll"
    ~count:2000
    QCheck2.Gen.(pair gen_previous_entry gen_poll_result)
    (fun (previous_entry, poll) ->
      try
        let _ = Merge_queue_decision.apply ~previous_entry poll in
        true
      with _ -> false)

let prop_exact_predicate =
  QCheck2.Test.make ~name:"MQD ejection iff every predicate term holds"
    ~count:2000
    QCheck2.Gen.(pair gen_previous_entry gen_poll_result)
    (fun (previous_entry, poll) ->
      Bool.equal
        (Merge_queue_decision.should_treat_ejection_as_ci_failure
           ~previous_entry poll)
        (expected_ejection ~previous_entry poll))

let prop_exact_unmergeable_predicate =
  QCheck2.Test.make ~name:"MQD unmergeable iff every predicate term holds"
    ~count:2000 gen_poll_result (fun poll ->
      Bool.equal
        (Merge_queue_decision.should_treat_unmergeable_as_ci_failure poll)
        (expected_unmergeable poll))

let boundary_cases =
  [
    ("no previous entry", None, base_ejection_poll, false);
    ( "queue not required",
      Some previous_entry,
      { base_ejection_poll with merge_queue_required = false },
      false );
    ( "still enqueued",
      Some previous_entry,
      { base_ejection_poll with merge_queue_entry = Some previous_entry },
      false );
    ( "merged",
      Some previous_entry,
      { base_ejection_poll with merged = true },
      false );
    ( "closed",
      Some previous_entry,
      { base_ejection_poll with closed = true },
      false );
    ( "not merge-ready",
      Some previous_entry,
      { base_ejection_poll with merge_ready = false },
      false );
    ( "checks not passing",
      Some previous_entry,
      { base_ejection_poll with checks_passing = false },
      false );
    ( "current failure already visible",
      Some previous_entry,
      { base_ejection_poll with ci_checks = [ failing_check ] },
      false );
    ("ejected", Some previous_entry, base_ejection_poll, true);
  ]

let unmergeable_poll =
  {
    base_ejection_poll with
    Poller.merge_queue_entry =
      Some Pr_state.{ id = "MQE_2"; state = Mq_unmergeable; position = 4 };
  }

let unmergeable_boundary_cases =
  [
    ( "queue not required",
      { unmergeable_poll with merge_queue_required = false },
      false );
    ("merged", { unmergeable_poll with merged = true }, false);
    ("closed", { unmergeable_poll with closed = true }, false);
    ( "checks not passing",
      { unmergeable_poll with checks_passing = false },
      false );
    ( "current failure already visible",
      { unmergeable_poll with ci_checks = [ failing_check ] },
      false );
    ("unmergeable", unmergeable_poll, true);
  ]

let prop_boundary_cases =
  QCheck2.Test.make ~name:"MQD boundary guard matrix" ~count:1 QCheck2.Gen.unit
    (fun () ->
      List.for_all boundary_cases
        ~f:(fun (_name, previous_entry, poll, expected) ->
          Bool.equal
            (Merge_queue_decision.should_treat_ejection_as_ci_failure
               ~previous_entry poll)
            expected))

let prop_unmergeable_boundary_cases =
  QCheck2.Test.make ~name:"MQD unmergeable boundary guard matrix" ~count:1
    QCheck2.Gen.unit (fun () ->
      List.for_all unmergeable_boundary_cases ~f:(fun (_name, poll, expected) ->
          Bool.equal
            (Merge_queue_decision.should_treat_unmergeable_as_ci_failure poll)
            expected))

let prop_apply_shape =
  QCheck2.Test.make ~name:"MQD apply rewrites only merge-queue failure cases"
    ~count:2000
    QCheck2.Gen.(pair gen_previous_entry gen_poll_result)
    (fun (previous_entry, poll) ->
      let result = Merge_queue_decision.apply ~previous_entry poll in
      if expected_ejection ~previous_entry poll || expected_unmergeable poll
      then
        Bool.equal result.merge_queue_ejected
          (expected_ejection ~previous_entry poll)
        && Bool.equal result.merge_queue_unmergeable (expected_unmergeable poll)
        && List.exists result.poll_result.queue ~f:(Operation_kind.equal Ci)
        && (not result.poll_result.merge_ready)
        && (not result.poll_result.checks_passing)
        && List.exists result.poll_result.ci_checks
             ~f:Ci_check.is_merge_queue_failure
      else
        (not result.merge_queue_ejected)
        && (not result.merge_queue_unmergeable)
        && Poller.equal result.poll_result poll)

let prop_idempotent =
  QCheck2.Test.make ~name:"MQD apply is idempotent for a fixed prior entry"
    ~count:1000
    QCheck2.Gen.(pair gen_previous_entry gen_poll_result)
    (fun (previous_entry, poll) ->
      let first = Merge_queue_decision.apply ~previous_entry poll in
      let second =
        Merge_queue_decision.apply ~previous_entry first.poll_result
      in
      (not second.merge_queue_ejected)
      && (not second.merge_queue_unmergeable)
      && Poller.equal second.poll_result first.poll_result)

let prop_interleaving_threaded_prior_state =
  QCheck2.Test.make
    ~name:
      "MQD interleaving: threaded merge-queue entry produces exactly local \
       ejection decisions"
    ~count:1000
    QCheck2.Gen.(
      pair gen_previous_entry (list_size (int_range 0 20) gen_poll_result))
    (fun (initial_previous_entry, polls) ->
      let _last_previous, ok =
        List.fold polls ~init:(initial_previous_entry, true)
          ~f:(fun (previous_entry, ok) poll ->
            let result = Merge_queue_decision.apply ~previous_entry poll in
            let expected =
              expected_ejection ~previous_entry poll
              || expected_unmergeable poll
            in
            let local_ok =
              let synthetic_failure =
                List.exists result.poll_result.ci_checks
                  ~f:Ci_check.is_merge_queue_failure
              in
              Bool.equal
                (result.merge_queue_ejected || result.merge_queue_unmergeable)
                expected
              &&
              if result.merge_queue_ejected || result.merge_queue_unmergeable
              then
                synthetic_failure
                && (not result.poll_result.checks_passing)
                && List.exists result.poll_result.queue
                     ~f:(Operation_kind.equal Ci)
              else true
            in
            (result.poll_result.merge_queue_entry, ok && local_ok))
      in
      ok)

let () =
  List.iter
    [
      prop_totality;
      prop_exact_predicate;
      prop_exact_unmergeable_predicate;
      prop_boundary_cases;
      prop_unmergeable_boundary_cases;
      prop_apply_shape;
      prop_idempotent;
      prop_interleaving_threaded_prior_state;
    ] ~f:(fun test -> QCheck2.Test.check_exn test)
