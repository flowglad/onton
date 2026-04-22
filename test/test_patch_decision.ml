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
  Operation_kind.[ Human; Merge_conflict; Ci; Review_comments; Pr_body ]

let gen_feedback_op = QCheck2.Gen.oneof_list feedback_ops

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
            (* Drive ci_failure_count to 3 to trigger needs_intervention *)
            let a = with_pr pid br in
            let a =
              increment_ci_failure_count a
              |> increment_ci_failure_count |> increment_ci_failure_count
            in
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
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t);

  (* ========== respond_delivery property tests ========== *)
  let main_branch = "main" in

  (* RD-1: Staleness — merged, intervention, blocked, not-busy → Respond_stale *)
  let () =
    (* merged → Stale (make busy first, then mark merged to simulate race) *)
    let a = with_pr (Patch_id.of_string "rd1") (Branch.of_string "b") in
    let a = enqueue a Operation_kind.Human in
    let a = respond a Operation_kind.Human in
    let a = mark_merged a in
    assert (
      equal_respond_delivery
        (respond_delivery ~agent:a ~kind:Operation_kind.Human
           ~pre_fire_agent:None ~prefetched_comments:[] ~main_branch)
        Respond_stale);

    (* needs_intervention → Stale *)
    let a = with_pr (Patch_id.of_string "rd1b") (Branch.of_string "b") in
    (* Make busy first, then drive to needs_intervention state *)
    let a = enqueue a Operation_kind.Human in
    let a = respond a Operation_kind.Human in
    let a =
      increment_ci_failure_count a
      |> increment_ci_failure_count |> increment_ci_failure_count
    in
    (* ci_failure_count = 3 → needs_intervention, but agent is still busy *)
    assert (needs_intervention a);
    assert (
      equal_respond_delivery
        (respond_delivery ~agent:a ~kind:Operation_kind.Human
           ~pre_fire_agent:None ~prefetched_comments:[] ~main_branch)
        Respond_stale);

    (* not busy → Stale *)
    let a = with_pr (Patch_id.of_string "rd1c") (Branch.of_string "b") in
    assert (
      equal_respond_delivery
        (respond_delivery ~agent:a ~kind:Operation_kind.Human
           ~pre_fire_agent:None ~prefetched_comments:[] ~main_branch)
        Respond_stale);
    Stdlib.print_endline "RD-1 passed"
  in

  (* RD-2: Empty delivery — Human with no messages → Skip_empty *)
  let () =
    let pid = Patch_id.of_string "rd2" in
    let br = Branch.of_string "b" in
    let pre_fire = with_pr pid br in
    (* pre_fire has no human_messages → Skip_empty *)
    let a = enqueue pre_fire Operation_kind.Human in
    let a = respond a Operation_kind.Human in
    assert (
      equal_respond_delivery
        (respond_delivery ~agent:a ~kind:Operation_kind.Human
           ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[] ~main_branch)
        Skip_empty);
    Stdlib.print_endline "RD-2a passed"
  in

  (* RD-2b: Ci with no failure conclusions in [agent.ci_checks] → Skip_empty.
     The runner's freshness gate is the primary defense, but this
     belt-and-suspenders guard keeps [respond_delivery] correct in
     isolation so a future caller that forgets the pre-fetch can't emit
     an empty Ci_payload. *)
  let () =
    let pid = Patch_id.of_string "rd2b" in
    let br = Branch.of_string "b" in
    let pre_fire =
      with_pr pid br |> fun a ->
      set_ci_checks a
        [
          {
            Ci_check.name = "build";
            conclusion = "success";
            details_url = None;
            description = None;
            started_at = None;
            id = None;
          };
        ]
    in
    let a = enqueue pre_fire Operation_kind.Ci in
    let a = respond a Operation_kind.Ci in
    assert (
      equal_respond_delivery
        (respond_delivery ~agent:a ~kind:Operation_kind.Ci
           ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[] ~main_branch)
        Skip_empty);
    Stdlib.print_endline "RD-2b passed"
  in

  (* RD-2c: Review with no comments → Skip_empty *)
  let () =
    let pid = Patch_id.of_string "rd2c" in
    let br = Branch.of_string "b" in
    let a = with_pr pid br in
    let a = enqueue a Operation_kind.Review_comments in
    let a = respond a Operation_kind.Review_comments in
    assert (
      equal_respond_delivery
        (respond_delivery ~agent:a ~kind:Operation_kind.Review_comments
           ~pre_fire_agent:None ~prefetched_comments:[] ~main_branch)
        Skip_empty);
    Stdlib.print_endline "RD-2c passed"
  in

  (* RD-3: Human with messages → Deliver (Human_payload) *)
  let () =
    let pid = Patch_id.of_string "rd3" in
    let br = Branch.of_string "b" in
    let pre_fire = with_pr pid br |> fun a -> add_human_message a "fix this" in
    let a = enqueue pre_fire Operation_kind.Human in
    let a = respond a Operation_kind.Human in
    (match
       respond_delivery ~agent:a ~kind:Operation_kind.Human
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[] ~main_branch
     with
    | Deliver { payload = Human_payload { messages }; _ } ->
        assert (List.equal String.equal messages [ "fix this" ])
    | Deliver
        {
          payload =
            ( Ci_payload _ | Review_payload _ | Pr_body_payload
            | Merge_conflict_payload );
          _;
        }
    | Skip_empty | Respond_stale ->
        failwith "RD-3: expected Deliver(Human_payload)");
    Stdlib.print_endline "RD-3 passed"
  in

  (* RD-4: Source agent selection — pre_fire_agent's human_messages are used,
     not agent's inflight_human_messages *)
  let () =
    let pid = Patch_id.of_string "rd4" in
    let br = Branch.of_string "b" in
    let pre_fire =
      with_pr pid br |> fun a ->
      add_human_message a "msg1" |> fun a -> add_human_message a "msg2"
    in
    (* After fire, messages move to inflight; human_messages is empty *)
    let post_fire = enqueue pre_fire Operation_kind.Human in
    let post_fire = respond post_fire Operation_kind.Human in
    assert (List.is_empty post_fire.human_messages);
    assert (not (List.is_empty post_fire.inflight_human_messages));
    (match
       respond_delivery ~agent:post_fire ~kind:Operation_kind.Human
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[] ~main_branch
     with
    | Deliver { payload = Human_payload { messages }; _ } ->
        (* Messages come from pre_fire.human_messages (reversed) *)
        assert (Int.equal (List.length messages) 2)
    | Deliver
        {
          payload =
            ( Ci_payload _ | Review_payload _ | Pr_body_payload
            | Merge_conflict_payload );
          _;
        }
    | Skip_empty | Respond_stale ->
        failwith "RD-4: expected Deliver(Human_payload)");
    Stdlib.print_endline "RD-4 passed"
  in

  (* RD-5: Base change detection *)
  let () =
    let pid = Patch_id.of_string "rd5" in
    let br = Branch.of_string "b" in
    let pre_fire =
      with_pr pid br |> fun a ->
      add_human_message a "msg" |> fun a ->
      set_base_branch a (Branch.of_string "feature")
    in
    let a = enqueue pre_fire Operation_kind.Human in
    let a = respond a Operation_kind.Human in
    (* notified_base_branch defaults to base_branch at start time (= br),
       but base_branch is now "feature" → base changed *)
    (match
       respond_delivery ~agent:a ~kind:Operation_kind.Human
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[] ~main_branch
     with
    | Deliver { base_change = Some bc; _ } ->
        assert (String.equal bc.old_base (Branch.to_string br));
        assert (String.equal bc.new_base "feature")
    | (Deliver _ | Skip_empty | Respond_stale) as other ->
        failwith
          (Printf.sprintf "RD-5: expected Deliver with base_change, got %s"
             (show_respond_delivery other)));
    Stdlib.print_endline "RD-5 passed"
  in

  (* RD-6: failure_conclusions consistency — each conclusion produces Deliver *)
  let () =
    let pid = Patch_id.of_string "rd6" in
    let br = Branch.of_string "b" in
    List.iter failure_conclusions ~f:(fun conclusion ->
        let pre_fire =
          with_pr pid br |> fun a ->
          set_ci_checks a
            [
              {
                Ci_check.name = "test";
                conclusion;
                details_url = None;
                description = None;
                started_at = None;
                id = None;
              };
            ]
        in
        let a = enqueue pre_fire Operation_kind.Ci in
        let a = respond a Operation_kind.Ci in
        match
          respond_delivery ~agent:a ~kind:Operation_kind.Ci
            ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[] ~main_branch
        with
        | Deliver { payload = Ci_payload { failed_checks }; _ } ->
            assert (not (List.is_empty failed_checks))
        | Deliver
            {
              payload =
                ( Human_payload _ | Review_payload _ | Pr_body_payload
                | Merge_conflict_payload );
              _;
            }
        | Skip_empty | Respond_stale ->
            failwith
              (Printf.sprintf
                 "RD-6: conclusion %s: expected Deliver(Ci_payload)" conclusion));
    Stdlib.print_endline "RD-6 passed"
  in

  (* RD-7: Merge_conflict and Pr_body never Skip_empty *)
  let () =
    let pid = Patch_id.of_string "rd7" in
    let br = Branch.of_string "b" in
    List.iter [ Operation_kind.Merge_conflict; Operation_kind.Pr_body ]
      ~f:(fun kind ->
        let a = with_pr pid br in
        let a = enqueue a kind in
        let a = respond a kind in
        match
          respond_delivery ~agent:a ~kind ~pre_fire_agent:None
            ~prefetched_comments:[] ~main_branch
        with
        | Skip_empty ->
            failwith
              (Printf.sprintf "RD-7: %s should never be Skip_empty"
                 (Operation_kind.to_label kind))
        | Deliver _ | Respond_stale -> ());
    Stdlib.print_endline "RD-7 passed"
  in

  (* RD-8: Ci with all failing run ids already delivered -> Skip_empty *)
  let () =
    let pid = Patch_id.of_string "rd8" in
    let br = Branch.of_string "b" in
    let failing_check id =
      {
        Ci_check.name = Printf.sprintf "check-%d" id;
        conclusion = "failure";
        details_url = None;
        description = None;
        started_at = None;
        id = Some id;
      }
    in
    let pre_fire =
      with_pr pid br |> fun a ->
      set_ci_checks a [ failing_check 101; failing_check 202 ] |> fun a ->
      record_delivered_ci_run_ids a [ 101; 202 ]
    in
    let a = enqueue pre_fire Operation_kind.Ci in
    let a = respond a Operation_kind.Ci in
    (match
       respond_delivery ~agent:a ~kind:Operation_kind.Ci
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[] ~main_branch
     with
    | Skip_empty -> ()
    | Deliver _ | Respond_stale ->
        failwith "RD-8: expected Skip_empty when all run ids already delivered");
    Stdlib.print_endline "RD-8 passed"
  in

  (* RD-9: Ci with one delivered and one undelivered failing run -> Deliver
     with only the undelivered check. *)
  let () =
    let pid = Patch_id.of_string "rd9" in
    let br = Branch.of_string "b" in
    let failing_check id =
      {
        Ci_check.name = Printf.sprintf "check-%d" id;
        conclusion = "failure";
        details_url = None;
        description = None;
        started_at = None;
        id = Some id;
      }
    in
    let pre_fire =
      with_pr pid br |> fun a ->
      set_ci_checks a [ failing_check 11; failing_check 22 ] |> fun a ->
      record_delivered_ci_run_ids a [ 11 ]
    in
    let a = enqueue pre_fire Operation_kind.Ci in
    let a = respond a Operation_kind.Ci in
    (match
       respond_delivery ~agent:a ~kind:Operation_kind.Ci
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[] ~main_branch
     with
    | Deliver { payload = Ci_payload { failed_checks }; _ } ->
        let ids =
          List.filter_map failed_checks ~f:(fun (c : Ci_check.t) ->
              c.Ci_check.id)
        in
        assert (List.equal Int.equal ids [ 22 ])
    | Deliver
        {
          payload =
            ( Human_payload _ | Review_payload _ | Pr_body_payload
            | Merge_conflict_payload );
          _;
        }
    | Skip_empty | Respond_stale ->
        failwith "RD-9: expected Deliver with only id=22");
    Stdlib.print_endline "RD-9 passed"
  in

  (* RD-10: checks with id=None (StatusContext) bypass dedup and always
     deliver. *)
  let () =
    let pid = Patch_id.of_string "rd10" in
    let br = Branch.of_string "b" in
    let no_id_check =
      {
        Ci_check.name = "legacy-status";
        conclusion = "failure";
        details_url = None;
        description = None;
        started_at = None;
        id = None;
      }
    in
    let pre_fire =
      with_pr pid br |> fun a ->
      set_ci_checks a [ no_id_check ]
      (* recording unrelated ids does not suppress an id=None check *)
      |> fun a -> record_delivered_ci_run_ids a [ 999 ]
    in
    let a = enqueue pre_fire Operation_kind.Ci in
    let a = respond a Operation_kind.Ci in
    (match
       respond_delivery ~agent:a ~kind:Operation_kind.Ci
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[] ~main_branch
     with
    | Deliver { payload = Ci_payload { failed_checks }; _ } ->
        assert (List.length failed_checks = 1)
    | Deliver
        {
          payload =
            ( Human_payload _ | Review_payload _ | Pr_body_payload
            | Merge_conflict_payload );
          _;
        }
    | Skip_empty | Respond_stale ->
        failwith "RD-10: expected Deliver for id=None check");
    Stdlib.print_endline "RD-10 passed"
  in

  ignore main_branch
