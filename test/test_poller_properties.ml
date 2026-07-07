(* @archlint.module test
   @archlint.domain poller *)

open Base
open Onton_core.Types

let merge_queue_entry =
  Onton_core.Pr_state.{ id = "mq"; state = Mq_queued; position = 1 }

let () =
  let open QCheck2 in
  let open Onton_test_support.Test_generators in
  let tests =
    [
      (* merge_ready is passed through from PR state *)
      Test.make ~name:"merge_ready passed through" ~count:500 gen_pr_state
        (fun pr ->
          let result = Onton_core.Poller.poll ~was_merged:false pr in
          Bool.equal result.Onton_core.Poller.merge_ready
            (Onton_core.Pr_state.merge_ready pr));
      Test.make ~name:"is_draft passed through" ~count:500 gen_pr_state
        (fun pr ->
          let result = Onton_core.Poller.poll ~was_merged:false pr in
          Bool.equal result.Onton_core.Poller.is_draft
            (Onton_core.Pr_state.is_draft pr));
      (* The tri-state merge_state is passed through verbatim, and the derived
         accessors agree with it. *)
      Test.make ~name:"merge_state passed through" ~count:500 gen_pr_state
        (fun pr ->
          let result = Onton_core.Poller.poll ~was_merged:false pr in
          Onton_core.Pr_state.equal_merge_state
            result.Onton_core.Poller.merge_state
            pr.Onton_core.Pr_state.merge_state);
      Test.make ~name:"Poller.has_conflict iff merge_state = Conflicting"
        ~count:500 gen_pr_state (fun pr ->
          let result = Onton_core.Poller.poll ~was_merged:false pr in
          Bool.equal
            (Onton_core.Poller.has_conflict result)
            (Onton_core.Pr_state.equal_merge_state
               result.Onton_core.Poller.merge_state
               Onton_core.Pr_state.Conflicting));
      Test.make
        ~name:
          "POLL-MQ-1: Conflicting enqueues Merge_conflict iff not in merge \
           queue"
        ~count:500 gen_pr_state (fun pr ->
          let conflicting =
            {
              pr with
              Onton_core.Pr_state.status = Onton_core.Pr_state.Open;
              merge_state = Onton_core.Pr_state.Conflicting;
              merge_queue_entry = None;
            }
          in
          let queued =
            {
              conflicting with
              Onton_core.Pr_state.merge_queue_entry = Some merge_queue_entry;
            }
          in
          let unqueued_result =
            Onton_core.Poller.poll ~was_merged:false conflicting
          in
          let queued_result = Onton_core.Poller.poll ~was_merged:false queued in
          List.mem unqueued_result.Onton_core.Poller.queue
            Operation_kind.Merge_conflict ~equal:Operation_kind.equal
          && not
               (List.mem queued_result.Onton_core.Poller.queue
                  Operation_kind.Merge_conflict ~equal:Operation_kind.equal));
      Test.make
        ~name:"POLL-MQ-2: Conflicting still mirrors has_conflict while queued"
        ~count:500 gen_pr_state (fun pr ->
          let queued =
            {
              pr with
              Onton_core.Pr_state.status = Onton_core.Pr_state.Open;
              merge_state = Onton_core.Pr_state.Conflicting;
              Onton_core.Pr_state.merge_queue_entry = Some merge_queue_entry;
            }
          in
          let result = Onton_core.Poller.poll ~was_merged:false queued in
          Onton_core.Poller.has_conflict result);
      Test.make ~name:"Poller.mergeability_unknown iff merge_state = Unknown"
        ~count:500 gen_pr_state (fun pr ->
          let result = Onton_core.Poller.poll ~was_merged:false pr in
          Bool.equal
            (Onton_core.Poller.mergeability_unknown result)
            (Onton_core.Pr_state.equal_merge_state
               result.Onton_core.Poller.merge_state Onton_core.Pr_state.Unknown));
      (* checks_passing is passed through from PR state *)
      Test.make ~name:"checks_passing passed through" ~count:500 gen_pr_state
        (fun pr ->
          let result = Onton_core.Poller.poll ~was_merged:false pr in
          Bool.equal result.Onton_core.Poller.checks_passing
            (Onton_core.Pr_state.checks_passing pr));
      (* ci_checks are passed through from PR state *)
      Test.make ~name:"ci_checks passed through" ~count:500 gen_pr_state
        (fun pr ->
          let result = Onton_core.Poller.poll ~was_merged:false pr in
          List.equal Ci_check.equal result.Onton_core.Poller.ci_checks
            pr.Onton_core.Pr_state.ci_checks);
      (* -- derive_check_status semantics ------------------------------- *)
      (* Failing iff any conclusion is in failure_conclusions *)
      Test.make ~name:"derive_check_status: Failing iff any is_failure"
        ~count:1000
        Gen.(list_size (int_range 0 8) gen_ci_check)
        (fun checks ->
          let status = Onton_core.Pr_state.derive_check_status checks in
          let any_failure = List.exists checks ~f:Ci_check.is_failure in
          Bool.equal
            (Onton_core.Pr_state.equal_check_status status
               Onton_core.Pr_state.Failing)
            any_failure);
      (* Passing iff non-empty and every conclusion is in success_conclusions *)
      Test.make ~name:"derive_check_status: Passing iff non-empty all_success"
        ~count:1000
        Gen.(list_size (int_range 0 8) gen_ci_check)
        (fun checks ->
          let status = Onton_core.Pr_state.derive_check_status checks in
          let all_success =
            (not (List.is_empty checks))
            && List.for_all checks ~f:Ci_check.is_success
          in
          Bool.equal
            (Onton_core.Pr_state.equal_check_status status
               Onton_core.Pr_state.Passing)
            all_success);
      (* Cancelled-only lists must NOT be Failing (regression for the
         orchestrator needs-intervention loop seen in production). *)
      Test.make ~name:"derive_check_status: cancelled-only is not Failing"
        ~count:500
        Gen.(
          list_size (int_range 1 6)
            (map
               (fun (name, details_url, description) ->
                 Ci_check.
                   {
                     name;
                     conclusion = "cancelled";
                     details_url;
                     description;
                     started_at = None;
                     id = None;
                   })
               (triple
                  (string_size ~gen:(char_range 'a' 'z') (int_range 3 10))
                  (option (pure "https://ci.example.com/x"))
                  (option (string_size ~gen:printable (int_range 0 20))))))
        (fun checks ->
          let status = Onton_core.Pr_state.derive_check_status checks in
          not
            (Onton_core.Pr_state.equal_check_status status
               Onton_core.Pr_state.Failing));
      (* Empty list -> Pending (never Passing, never Failing) *)
      Test.make ~name:"derive_check_status: empty is Pending" ~count:1
        Gen.(pure ())
        (fun () ->
          Onton_core.Pr_state.equal_check_status
            (Onton_core.Pr_state.derive_check_status [])
            Onton_core.Pr_state.Pending);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t)
