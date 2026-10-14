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
        {
          name;
          conclusion;
          details_url;
          description;
          started_at = None;
          app_id = None;
          check_suite_id = None;
          id;
        })
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
  let* base_branch = option (map Branch.of_string gen_string) in
  let* base_oid =
    match base_branch with
    | None -> return None
    | Some _ -> map (fun oid -> Some oid) gen_string
  in
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
        base_branch;
        base_oid;
        head_oid = None;
        review_decision = None;
        unresolved_comment_count = 0;
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
      app_id = None;
      check_suite_id = None;
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
      base_branch = None;
      base_oid = None;
      head_oid = None;
      review_decision = Some "APPROVED";
      unresolved_comment_count = 0;
      merge_queue_required = true;
      merge_queue_entry = None;
      checks_passing = true;
      ci_checks = [];
      merge_commit_sha = None;
    }

let previous_entry =
  Pr_state.{ id = "MQE_1"; state = Mq_awaiting_checks; position = 1 }

let expected_ejection ~previous_entry (poll : Poller.t) =
  Option.is_some previous_entry
  && Option.is_none poll.merge_queue_entry
  && (not poll.merged) && not poll.closed

let prop_totality =
  QCheck2.Test.make ~name:"MQD total over arbitrary prior entry + poll"
    ~count:2000
    QCheck2.Gen.(triple gen_previous_entry gen_poll_result bool)
    (fun (previous_entry, poll, ejection_confirmed) ->
      try
        let _ =
          Merge_queue_decision.apply ~previous_entry
            ?removal_checks:
              (if ejection_confirmed then Some [ failing_check ] else Some [])
            poll
        in
        true
      with _ -> false)

let prop_exact_predicate =
  QCheck2.Test.make ~name:"MQD ejection iff every predicate term holds"
    ~count:2000
    QCheck2.Gen.(pair gen_previous_entry gen_poll_result)
    (fun (previous_entry, poll) ->
      Bool.equal
        (Merge_queue_decision.queue_entry_disappeared ~previous_entry poll)
        (expected_ejection ~previous_entry poll))

let boundary_cases =
  [
    ("no previous entry", None, base_ejection_poll, false);
    ( "queue not required",
      Some previous_entry,
      { base_ejection_poll with merge_queue_required = false },
      true );
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
      true );
    ( "checks not passing",
      Some previous_entry,
      { base_ejection_poll with checks_passing = false },
      true );
    ( "current failure already visible",
      Some previous_entry,
      { base_ejection_poll with ci_checks = [ failing_check ] },
      true );
    ("ejected", Some previous_entry, base_ejection_poll, true);
  ]

let prop_boundary_cases =
  QCheck2.Test.make ~name:"MQD boundary guard matrix" ~count:1 QCheck2.Gen.unit
    (fun () ->
      List.for_all boundary_cases
        ~f:(fun (_name, previous_entry, poll, expected) ->
          Bool.equal
            (Merge_queue_decision.queue_entry_disappeared ~previous_entry poll)
            expected))

(* Synthesis fires iff the ejection predicate holds AND the caller confirmed it
   (the removal event carried a real merge-group failure). An unconfirmed
   ejection — a conflict — must leave the poll result untouched. *)
let prop_apply_shape =
  QCheck2.Test.make ~name:"MQD apply rewrites only confirmed ejections"
    ~count:2000
    QCheck2.Gen.(triple gen_previous_entry gen_poll_result bool)
    (fun (previous_entry, poll, ejection_confirmed) ->
      let result =
        Merge_queue_decision.apply ~previous_entry
          ?removal_checks:
            (if ejection_confirmed then Some [ failing_check ] else Some [])
          poll
      in
      if ejection_confirmed && expected_ejection ~previous_entry poll then
        result.merge_queue_ejected
        && List.exists result.poll_result.queue ~f:(Operation_kind.equal Ci)
        && (not result.poll_result.merge_ready)
        && (not result.poll_result.checks_passing)
        && List.exists result.poll_result.ci_checks
             ~f:Ci_check.is_merge_queue_failure
        && Option.equal Branch.equal result.poll_result.base_branch
             poll.base_branch
        && Option.equal String.equal result.poll_result.base_oid poll.base_oid
      else
        (not result.merge_queue_ejected) && Poller.equal result.poll_result poll)

let prop_idempotent =
  QCheck2.Test.make ~name:"MQD apply is idempotent for a fixed prior entry"
    ~count:1000
    QCheck2.Gen.(triple gen_previous_entry gen_poll_result bool)
    (fun (previous_entry, poll, ejection_confirmed) ->
      let first =
        Merge_queue_decision.apply ~previous_entry
          ?removal_checks:
            (if ejection_confirmed then Some [ failing_check ] else Some [])
          poll
      in
      let second =
        Merge_queue_decision.apply ~previous_entry
          ?removal_checks:
            (if ejection_confirmed then Some [ failing_check ] else Some [])
          first.poll_result
      in
      Bool.equal second.merge_queue_ejected first.merge_queue_ejected
      && Poller.equal second.poll_result first.poll_result)

let prop_interleaving_threaded_prior_state =
  QCheck2.Test.make
    ~name:
      "MQD interleaving: threaded merge-queue entry produces exactly local \
       ejection decisions"
    ~count:1000
    QCheck2.Gen.(
      pair gen_previous_entry
        (list_size (int_range 0 20) (pair gen_poll_result bool)))
    (fun (initial_previous_entry, polls) ->
      let _last_previous, ok =
        List.fold polls ~init:(initial_previous_entry, true)
          ~f:(fun (previous_entry, ok) (poll, ejection_confirmed) ->
            let result =
              Merge_queue_decision.apply ~previous_entry
                ?removal_checks:
                  (if ejection_confirmed then Some [ failing_check ]
                   else Some [])
                poll
            in
            let expected =
              ejection_confirmed && expected_ejection ~previous_entry poll
            in
            let local_ok =
              let synthetic_failure =
                List.exists result.poll_result.ci_checks
                  ~f:Ci_check.is_merge_queue_failure
              in
              Bool.equal result.merge_queue_ejected expected
              &&
              if result.merge_queue_ejected then
                synthetic_failure
                && (not result.poll_result.checks_passing)
                && List.exists result.poll_result.queue
                     ~f:(Operation_kind.equal Ci)
              else true
            in
            (result.poll_result.merge_queue_entry, ok && local_ok))
      in
      ok)

let prop_readiness_interleavings =
  QCheck2.Test.make
    ~name:"MQD pending removal and failure survive readiness interleavings"
    ~count:1000
    QCheck2.Gen.(list_size (int_range 1 40) (pair bool bool))
    (fun readiness ->
      let old_head = Some "old" in
      let first =
        Merge_queue_decision.apply ~previous_head_oid:old_head
          ~previous_entry:(Some previous_entry)
          {
            base_ejection_poll with
            head_oid = old_head;
            merge_ready = false;
            checks_passing = false;
          }
      in
      let pending, unresolved =
        List.fold readiness ~init:(first.pending, true)
          ~f:(fun (pending, ok) (merge_ready, checks_passing) ->
            let poll =
              {
                base_ejection_poll with
                head_oid = old_head;
                merge_ready;
                checks_passing;
              }
            in
            let retry =
              Merge_queue_decision.should_fetch ~pending ~previous_entry:None
                poll
            in
            let next =
              Merge_queue_decision.apply ~pending ~previous_entry:None poll
            in
            ( next.pending,
              ok && retry
              && Option.is_some next.pending
              && not next.merge_queue_ejected ))
      in
      let repaired =
        Merge_queue_decision.apply ~pending ~previous_entry:None
          ~removal_checks:[ failing_check ]
          { base_ejection_poll with head_oid = old_head; merge_ready = false }
      in
      let pending, retained =
        List.fold readiness ~init:(repaired.pending, true)
          ~f:(fun (pending, ok) (merge_ready, checks_passing) ->
            let poll =
              {
                base_ejection_poll with
                head_oid = old_head;
                merge_ready;
                checks_passing;
              }
            in
            let next =
              Merge_queue_decision.apply ~pending ~previous_entry:None poll
            in
            ( next.pending,
              ok && next.merge_queue_ejected
              && (not next.poll_result.merge_ready)
              && (not next.poll_result.checks_passing)
              && List.mem next.poll_result.ci_checks failing_check
                   ~equal:Ci_check.equal ))
      in
      let changed =
        Merge_queue_decision.apply ~pending ~previous_entry:None
          { base_ejection_poll with head_oid = Some "fixed" }
      in
      unresolved && retained
      && Option.is_none changed.pending
      && Option.equal Merge_queue_decision.equal_pending first.pending
           (Merge_queue_decision.repair_completed first.pending)
      && Option.is_none (Merge_queue_decision.repair_completed repaired.pending))

let prop_resolution =
  QCheck2.Test.make
    ~name:"MQD successful non-CI removal and terminal PR resolve pending"
    ~count:1000
    QCheck2.Gen.(pair bool bool)
    (fun (merged, closed) ->
      let first =
        Merge_queue_decision.apply ~previous_entry:(Some previous_entry)
          base_ejection_poll
      in
      let non_ci =
        Merge_queue_decision.apply ~pending:first.pending ~previous_entry:None
          ~removal_checks:[] base_ejection_poll
      in
      let terminal =
        Merge_queue_decision.apply ~pending:first.pending ~previous_entry:None
          { base_ejection_poll with merged; closed }
      in
      Option.is_none non_ci.pending
      && (not non_ci.merge_queue_ejected)
      && Bool.equal (Option.is_none terminal.pending) (merged || closed))

let prop_head_change =
  QCheck2.Test.make
    ~name:"MQD known head changes release unresolved and failed evidence"
    ~count:200
    QCheck2.Gen.(pair bool (option gen_string))
    (fun (confirmed, fresh) ->
      let initial =
        Merge_queue_decision.apply ~previous_entry:(Some previous_entry)
          ~previous_head_oid:(Some "old")
          ?removal_checks:(if confirmed then Some [ failing_check ] else None)
          { base_ejection_poll with head_oid = Some "old" }
      in
      let result =
        Merge_queue_decision.apply ~pending:initial.pending ~previous_entry:None
          { base_ejection_poll with head_oid = fresh }
      in
      match fresh with
      | Some head when not (String.equal head "old") ->
          Option.is_none result.pending && not result.merge_queue_ejected
      | _ ->
          Option.equal Merge_queue_decision.equal_pending initial.pending
            result.pending)

let prop_repair_evidence =
  QCheck2.Test.make
    ~name:
      "MQD repair checks require the recorded head, including legacy markers"
    ~count:100 QCheck2.Gen.bool (fun legacy ->
      let marker = Ci_check.merge_queue_failure () in
      let pending =
        if legacy then None
        else
          (Merge_queue_decision.apply ~previous_entry:(Some previous_entry)
             ~previous_head_oid:(Some "old") ~removal_checks:[ failing_check ]
             { base_ejection_poll with head_oid = Some "old" })
            .pending
      in
      List.for_all [ None; Some "old"; Some "new" ] ~f:(fun head_oid ->
          let markers, failures =
            Merge_queue_decision.repair_evidence ~pending
              ~previous_head_oid:(Some "old") ~head_oid ~checks:[ marker ]
          in
          if Option.equal String.equal head_oid (Some "old") then
            List.equal Ci_check.equal markers [ marker ]
            && List.equal Ci_check.equal failures
                 (if legacy then [] else [ failing_check ])
          else List.is_empty markers && List.is_empty failures))

let prop_failed_checks =
  QCheck2.Test.make ~name:"MQD exposes only confirmed removal failures"
    ~count:200 QCheck2.Gen.bool (fun confirmed ->
      let first =
        Merge_queue_decision.apply ~previous_entry:(Some previous_entry)
          ?removal_checks:(if confirmed then Some [ failing_check ] else None)
          base_ejection_poll
      in
      List.equal Ci_check.equal
        (Merge_queue_decision.failed_checks first.pending)
        (if confirmed then [ failing_check ] else [])
      && List.is_empty (Merge_queue_decision.failed_checks None))

let prop_removal_classification =
  QCheck2.Test.make
    ~name:"MQD classifies missing, non-failure and incomplete failure evidence"
    ~count:200 QCheck2.Gen.bool (fun uppercase ->
      let classify reason =
        let reason = if uppercase then String.uppercase reason else reason in
        Merge_queue_decision.removal_evidence
          ~body:
            (Printf.sprintf
               {|{"data":{"repository":{"pullRequest":{"timelineItems":{"nodes":[{"reason":%S}]}}}}}|}
               reason)
      in
      (match Merge_queue_decision.removal_evidence ~body:"{}" with
        | Missing -> true
        | Non_failure | Needs_checks -> false)
      && (match classify "The merge commit has conflicts." with
        | Non_failure -> true
        | Missing | Needs_checks -> false)
      && List.for_all
           [
             "failed_checks";
             "The merge commit failed required status checks.";
             "";
           ] ~f:(fun reason ->
             match classify reason with
             | Needs_checks -> true
             | Missing | Non_failure -> false))

let prop_removal_totality =
  QCheck2.Test.make ~name:"MQD removal evidence parser is total" ~count:1000
    QCheck2.Gen.string (fun body ->
      try
        ignore (Merge_queue_decision.removal_processed ~body ~checks:[] : bool);
        true
      with _ -> false)

let prop_removal_boundaries =
  QCheck2.Test.make ~name:"MQD incomplete removal evidence retries" ~count:1
    QCheck2.Gen.unit (fun () ->
      let response reason =
        Printf.sprintf
          {|{"data":{"repository":{"pullRequest":{"timelineItems":{"nodes":[{"reason":%S}]}}}}}|}
          reason
      in
      (not (Merge_queue_decision.removal_processed ~body:"{}" ~checks:[]))
      && (not
            (Merge_queue_decision.removal_processed
               ~body:(response "failed_checks") ~checks:[]))
      && Merge_queue_decision.removal_processed ~body:(response "failed_checks")
           ~checks:[ failing_check ]
      && Merge_queue_decision.removal_processed
           ~body:(response "The merge commit failed required status checks.")
           ~checks:[ failing_check ]
      && Merge_queue_decision.removal_processed
           ~body:(response "The merge commit has conflicts.")
           ~checks:[])

let () =
  List.iter
    [
      prop_failed_checks;
      prop_removal_classification;
      prop_repair_evidence;
      prop_head_change;
      prop_readiness_interleavings;
      prop_resolution;
      prop_removal_totality;
      prop_removal_boundaries;
      prop_totality;
      prop_exact_predicate;
      prop_boundary_cases;
      prop_apply_shape;
      prop_idempotent;
      prop_interleaving_threaded_prior_state;
    ] ~f:(fun test -> QCheck2.Test.check_exn test)
