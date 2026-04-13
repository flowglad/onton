open Base
open Onton.Types
open Onton.Patch_agent
open Onton.Patch_decision

(* -- Generators -- *)

let gen_pid =
  QCheck2.Gen.(
    map Patch_id.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 12)))

let gen_branch =
  QCheck2.Gen.(
    map Branch.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 20)))

let feedback_ops =
  Operation_kind.
    [ Human; Merge_conflict; Ci; Review_comments; Implementation_notes ]

let gen_feedback_op = QCheck2.Gen.oneof_list feedback_ops

let gen_conclusion =
  QCheck2.Gen.oneof_list
    (ci_failure_conclusions @ [ "success"; "neutral"; "cancelled"; "skipped" ])

let gen_ci_check =
  QCheck2.Gen.(
    map
      (fun (name, conclusion) ->
        {
          Ci_check.name;
          conclusion;
          details_url = None;
          description = None;
          started_at = None;
        })
      (pair
         (string_size ~gen:(char_range 'a' 'z') (int_range 2 8))
         gen_conclusion))

let gen_ci_checks =
  QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 8) gen_ci_check

(** Start + set PR so the agent is in has_pr=true, busy=false state. *)
let with_pr pid br =
  let a = create ~branch:br pid |> fun a -> start a ~base_branch:br in
  let a = set_pr_number a (Pr_number.of_int 1) in
  complete a

let () =
  let open QCheck2 in
  let tests =
    [
      (* ---- disposition: merged always Skip ---- *)
      Test.make ~name:"disposition: merged -> Skip"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> mark_merged in
          equal_disposition (disposition a) Skip);
      (* ---- disposition: needs_intervention -> Blocked ---- *)
      Test.make ~name:"disposition: needs_intervention -> Blocked"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            (* Drive ci_failure_count to 3 via respond (which increments on
               Ci) to trigger needs_intervention after complete *)
            let a = with_pr pid br in
            let a =
              increment_ci_failure_count a |> increment_ci_failure_count
            in
            let a = enqueue a Operation_kind.Ci in
            (* respond Ci increments ci_failure_count to 3 *)
            let a = respond a Operation_kind.Ci in
            let a = complete a in
            equal_disposition (disposition a) Blocked
          with _ -> false);
      (* ---- disposition: busy -> Busy ---- *)
      Test.make ~name:"disposition: busy -> Busy"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Human in
          let a = respond a Operation_kind.Human in
          (* a is now busy *)
          equal_disposition (disposition a) Busy);
      (* ---- disposition: no PR -> Ready_start ---- *)
      Test.make ~name:"disposition: no PR -> Ready_start" gen_pid (fun pid ->
          let a = create ~branch:(Branch.of_string "b") pid in
          equal_disposition (disposition a) Ready_start);
      (* ---- disposition: idle (has_pr, empty queue) -> Idle ---- *)
      Test.make ~name:"disposition: has_pr, empty queue -> Idle"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          equal_disposition (disposition a) Idle);
      (* ---- disposition: queued feedback -> Ready_respond ---- *)
      Test.make ~name:"disposition: queued feedback -> Ready_respond"
        Gen.(triple gen_pid gen_branch gen_feedback_op)
        (fun (pid, br, k) ->
          let a = with_pr pid br |> fun a -> enqueue a k in
          try
            let d = disposition a in
            equal_disposition d (Ready_respond k)
          with _ -> false);
      (* ---- disposition: queued Rebase -> Ready_rebase ---- *)
      Test.make ~name:"disposition: queued Rebase -> Ready_rebase"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> fun a -> enqueue a Operation_kind.Rebase in
          equal_disposition (disposition a) Ready_rebase);
      (* ---- disposition: Rebase + feedback -> Ready_rebase (Rebase wins) ---- *)
      Test.make ~name:"disposition: Rebase wins over feedback"
        Gen.(triple gen_pid gen_branch gen_feedback_op)
        (fun (pid, br, k) ->
          let a =
            with_pr pid br |> fun a ->
            enqueue a k |> fun a -> enqueue a Operation_kind.Rebase
          in
          equal_disposition (disposition a) Ready_rebase);
      (* ---- on_ci_failure: below cap -> Enqueue ---- *)
      Test.make ~name:"on_ci_failure: count < 3 -> Enqueue"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          (* count = 0 *)
          equal_ci_decision (on_ci_failure a) Enqueue_ci);
      (* ---- on_ci_failure: at cap -> Cap_reached ---- *)
      Test.make ~name:"on_ci_failure: count >= 3 -> Cap_reached"
        Gen.(triple gen_pid gen_branch (int_range 3 20))
        (fun (pid, br, n) ->
          let a = with_pr pid br in
          let rec apply_n n f x =
            if n <= 0 then x else apply_n (n - 1) f (f x)
          in
          let a = apply_n n increment_ci_failure_count a in
          equal_ci_decision (on_ci_failure a) Cap_reached);
      (* ---- on_ci_failure: exactly 2 -> Enqueue ---- *)
      Test.make ~name:"on_ci_failure: count = 2 -> Enqueue"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = increment_ci_failure_count (increment_ci_failure_count a) in
          equal_ci_decision (on_ci_failure a) Enqueue_ci);
      (* ---- on_ci_failure: Ci already queued -> Already_queued ---- *)
      Test.make ~name:"on_ci_failure: Ci in queue -> Already_queued"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Ci in
          equal_ci_decision (on_ci_failure a) Ci_already_queued);
      (* ---- on_ci_failure: active Ci fix -> Ci_fix_in_progress ---- *)
      Test.make ~name:"on_ci_failure: active Ci fix -> Ci_fix_in_progress"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          equal_ci_decision (on_ci_failure a) Ci_fix_in_progress);
      Test.make
        ~name:
          "on_ci_failure: completed failed CI attempt re-enqueues on next poll"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          let a = complete a in
          equal_ci_decision (on_ci_failure a) Enqueue_ci);
      (* ---- on_human_message: fresh queue -> Enqueue_human ---- *)
      Test.make ~name:"on_human_message: no Human in queue -> Enqueue_human"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          equal_human_decision (on_human_message a) Enqueue_human);
      (* ---- on_human_message: already queued -> Already_queued ---- *)
      Test.make ~name:"on_human_message: Human in queue -> Already_queued"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> fun a -> enqueue a Operation_kind.Human in
          equal_human_decision (on_human_message a) Already_queued);
      (* ---- on_merge_conflict: no conflict -> Enqueue_conflict ---- *)
      Test.make ~name:"on_merge_conflict: no conflict -> Enqueue_conflict"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          equal_conflict_decision (on_merge_conflict a) Enqueue_conflict);
      (* ---- on_merge_conflict: already conflicting -> Already_conflicting ---- *)
      Test.make ~name:"on_merge_conflict: has_conflict -> Already_conflicting"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> set_has_conflict in
          equal_conflict_decision (on_merge_conflict a) Already_conflicting);
      (* ---- on_checks_passing: failures + passing -> Reset ---- *)
      Test.make
        ~name:"on_checks_passing: ci_failure_count > 0 + passing -> Reset"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> increment_ci_failure_count in
          equal_checks_passing_decision
            (on_checks_passing a ~checks_passing:true)
            Reset_ci_failure_count);
      (* ---- on_checks_passing: no failures + passing -> No_ci_reset ---- *)
      Test.make
        ~name:"on_checks_passing: ci_failure_count = 0 + passing -> No_ci_reset"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          equal_checks_passing_decision
            (on_checks_passing a ~checks_passing:true)
            No_ci_reset);
      (* ---- on_checks_passing: failures + not passing -> No_ci_reset ---- *)
      Test.make
        ~name:
          "on_checks_passing: ci_failure_count > 0 + not passing -> No_ci_reset"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> increment_ci_failure_count in
          equal_checks_passing_decision
            (on_checks_passing a ~checks_passing:false)
            No_ci_reset);
      (* ---- should_clear_conflict: no active conflict op -> true ---- *)
      Test.make ~name:"should_clear_conflict: idle agent -> true"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          should_clear_conflict a);
      (* ---- should_clear_conflict: Merge_conflict queued -> false ---- *)
      Test.make ~name:"should_clear_conflict: Merge_conflict queued -> false"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> set_has_conflict in
          let a = enqueue a Operation_kind.Merge_conflict in
          not (should_clear_conflict a));
      (* ---- should_clear_conflict: Merge_conflict in-flight -> false ---- *)
      Test.make ~name:"should_clear_conflict: Merge_conflict in-flight -> false"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> set_has_conflict in
          let a = enqueue a Operation_kind.Merge_conflict in
          let a = respond a Operation_kind.Merge_conflict in
          not (should_clear_conflict a));
      (* ---- should_clear_conflict: other op in-flight -> true ---- *)
      Test.make ~name:"should_clear_conflict: non-conflict op in-flight -> true"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Human in
          let a = respond a Operation_kind.Human in
          should_clear_conflict a);
      (* ---- delivery_decision: Human with messages -> Deliver ---- *)
      Test.make ~name:"delivery_decision: Human with inflight -> Deliver"
        Gen.(
          list_size (int_range 0 5)
            (string_size ~gen:(char_range 'a' 'z') (int_range 1 10)))
        (fun msgs ->
          let msgs = List.filter msgs ~f:(fun s -> not (String.is_empty s)) in
          if List.is_empty msgs then true
          else
            equal_delivery_decision
              (delivery_decision ~kind:Operation_kind.Human
                 ~inflight_human_messages:msgs ~review_comment_count:0
                 ~ci_checks:[])
              Deliver);
      (* ---- delivery_decision: Human with no messages -> Skip_empty ---- *)
      Test.make
        ~name:"delivery_decision: Human with empty inflight -> Skip_empty"
        Gen.unit (fun () ->
          equal_delivery_decision
            (delivery_decision ~kind:Operation_kind.Human
               ~inflight_human_messages:[] ~review_comment_count:0 ~ci_checks:[])
            Skip_empty);
      (* ---- delivery_decision: Review with comments -> Deliver ---- *)
      Test.make ~name:"delivery_decision: Review with comments -> Deliver"
        Gen.(int_range 1 100)
        (fun n ->
          equal_delivery_decision
            (delivery_decision ~kind:Operation_kind.Review_comments
               ~inflight_human_messages:[] ~review_comment_count:n ~ci_checks:[])
            Deliver);
      (* ---- delivery_decision: Review with 0 comments -> Skip_empty ---- *)
      Test.make ~name:"delivery_decision: Review with 0 comments -> Skip_empty"
        Gen.unit (fun () ->
          equal_delivery_decision
            (delivery_decision ~kind:Operation_kind.Review_comments
               ~inflight_human_messages:[] ~review_comment_count:0 ~ci_checks:[])
            Skip_empty);
      (* ---- delivery_decision: Ci with failures -> Deliver ---- *)
      Test.make ~name:"delivery_decision: Ci with failures -> Deliver"
        Gen.(oneof_list ci_failure_conclusions)
        (fun conclusion ->
          let check =
            {
              Ci_check.name = "ci";
              conclusion;
              details_url = None;
              description = None;
              started_at = None;
            }
          in
          equal_delivery_decision
            (delivery_decision ~kind:Operation_kind.Ci
               ~inflight_human_messages:[] ~review_comment_count:0
               ~ci_checks:[ check ])
            Deliver);
      (* ---- delivery_decision: Ci with only success -> Skip_empty ---- *)
      Test.make ~name:"delivery_decision: Ci with success only -> Skip_empty"
        Gen.unit (fun () ->
          let check =
            {
              Ci_check.name = "ci";
              conclusion = "success";
              details_url = None;
              description = None;
              started_at = None;
            }
          in
          equal_delivery_decision
            (delivery_decision ~kind:Operation_kind.Ci
               ~inflight_human_messages:[] ~review_comment_count:0
               ~ci_checks:[ check ])
            Skip_empty);
      (* ---- delivery_decision: Merge_conflict always Deliver ---- *)
      Test.make ~name:"delivery_decision: Merge_conflict -> Deliver" Gen.unit
        (fun () ->
          equal_delivery_decision
            (delivery_decision ~kind:Operation_kind.Merge_conflict
               ~inflight_human_messages:[] ~review_comment_count:0 ~ci_checks:[])
            Deliver);
      (* ---- delivery_decision agrees with respond postcondition ---- *)
      (* This is the property that would have caught the original bug:
         after respond(Human), the post-fire agent's inflight_human_messages
         is non-empty iff there were human_messages before respond. *)
      Test.make
        ~name:
          "delivery_decision: after respond(Human), Deliver iff messages \
           existed"
        Gen.(
          triple gen_pid gen_branch
            (list_size (int_range 0 5)
               (string_size ~gen:(char_range 'a' 'z') (int_range 1 10))))
        (fun (pid, br, msgs) ->
          try
            let msgs = List.filter msgs ~f:(fun s -> not (String.is_empty s)) in
            let a = with_pr pid br in
            let a = List.fold msgs ~init:a ~f:add_human_message in
            let a = enqueue a Operation_kind.Human in
            let post_fire = respond a Operation_kind.Human in
            let decision =
              delivery_decision ~kind:Operation_kind.Human
                ~inflight_human_messages:post_fire.inflight_human_messages
                ~review_comment_count:0 ~ci_checks:[]
            in
            if List.is_empty msgs then
              equal_delivery_decision decision Skip_empty
            else equal_delivery_decision decision Deliver
          with _ -> false);
      (* ==== is_stale ==== *)
      (* ---- merged implies stale (even when busy) ---- *)
      Test.make ~name:"is_stale: merged -> true"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let a = with_pr pid br in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            (* busy=true now *)
            let a = mark_merged a in
            is_stale a
          with _ -> false);
      (* ---- needs_intervention implies stale ---- *)
      Test.make ~name:"is_stale: needs_intervention -> true"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let a = with_pr pid br in
            let a =
              increment_ci_failure_count a |> increment_ci_failure_count
            in
            let a = enqueue a Operation_kind.Ci in
            let a = respond a Operation_kind.Ci in
            (* ci_failure_count is now 3 after respond Ci increments;
               agent is busy from respond — needs_intervention still true *)
            is_stale a
          with _ -> false);
      (* ---- branch_blocked implies stale ---- *)
      Test.make ~name:"is_stale: branch_blocked -> true"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let a = with_pr pid br in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            let a = set_branch_blocked a in
            is_stale a
          with _ -> false);
      (* ---- not busy implies stale ---- *)
      Test.make ~name:"is_stale: not busy -> true"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          (* a is idle (busy=false) after with_pr *)
          is_stale a);
      (* ---- busy + not merged + not intervention + not blocked -> not stale ---- *)
      Test.make ~name:"is_stale: healthy busy agent -> false"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let a = with_pr pid br in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            (* busy=true, not merged, ci_failure_count=0, branch_blocked=false *)
            not (is_stale a)
          with _ -> false);
      (* ---- is_stale cross-check with disposition ---- *)
      Test.make ~name:"is_stale: merged disposition Skip -> stale when busy"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let a = with_pr pid br in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            let a = mark_merged a in
            equal_disposition (disposition a) Skip && is_stale a
          with _ -> false);
      (* ==== filter_failed_ci_checks ==== *)
      (* ---- output is subset of input ---- *)
      Test.make ~name:"filter_failed_ci_checks: output subset of input"
        gen_ci_checks (fun checks ->
          let filtered = filter_failed_ci_checks checks in
          List.for_all filtered ~f:(fun c ->
              List.exists checks ~f:(Ci_check.equal c)));
      (* ---- all results have failure conclusions ---- *)
      Test.make ~name:"filter_failed_ci_checks: all results are failures"
        gen_ci_checks (fun checks ->
          let filtered = filter_failed_ci_checks checks in
          List.for_all filtered ~f:(fun (c : Ci_check.t) ->
              List.mem ci_failure_conclusions c.Ci_check.conclusion
                ~equal:String.equal));
      (* ---- no failures lost ---- *)
      Test.make ~name:"filter_failed_ci_checks: no failures lost" gen_ci_checks
        (fun checks ->
          let filtered = filter_failed_ci_checks checks in
          List.for_all checks ~f:(fun (c : Ci_check.t) ->
              if
                List.mem ci_failure_conclusions c.Ci_check.conclusion
                  ~equal:String.equal
              then List.exists filtered ~f:(Ci_check.equal c)
              else true));
      (* ---- idempotent ---- *)
      Test.make ~name:"filter_failed_ci_checks: idempotent" gen_ci_checks
        (fun checks ->
          let once = filter_failed_ci_checks checks in
          let twice = filter_failed_ci_checks once in
          List.equal Ci_check.equal once twice);
      (* ==== ci_prompt_kind ==== *)
      (* ---- Known_failures list is non-empty ---- *)
      Test.make ~name:"ci_prompt_kind: Known_failures is non-empty"
        gen_ci_checks (fun checks ->
          match ci_prompt_kind checks with
          | Known_failures fs -> not (List.is_empty fs)
          | Unknown_failure -> true);
      (* ---- Unknown_failure means no failures ---- *)
      Test.make ~name:"ci_prompt_kind: Unknown_failure -> no failed checks"
        gen_ci_checks (fun checks ->
          match ci_prompt_kind checks with
          | Unknown_failure -> not (has_failed_ci_checks checks)
          | Known_failures _ -> true);
      (* ---- Known_failures agrees with filter ---- *)
      Test.make ~name:"ci_prompt_kind: Known_failures = filter_failed_ci_checks"
        gen_ci_checks (fun checks ->
          match ci_prompt_kind checks with
          | Known_failures fs ->
              let expected = filter_failed_ci_checks checks in
              List.equal
                (fun (a : Ci_check.t) (b : Ci_check.t) ->
                  String.equal a.Ci_check.name b.Ci_check.name
                  && String.equal a.Ci_check.conclusion b.Ci_check.conclusion)
                fs expected
          | Unknown_failure -> true);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t)
