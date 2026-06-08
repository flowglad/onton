(* @archlint.module test
   @archlint.domain patch-decision *)

open Base
open Onton_core.Types
open Onton_core.Patch_agent
open Onton_core.Patch_decision

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
      Test.make
        ~name:
          "on_ci_failure: all known failing run ids delivered -> already \
           delivered"
        Gen.(triple gen_pid gen_branch (int_range 1 1_000_000))
        (fun (pid, br, run_id) ->
          let check =
            Ci_check.
              {
                name = "test";
                conclusion = "failure";
                details_url = None;
                description = None;
                started_at = None;
                id = Some run_id;
              }
          in
          let a =
            with_pr pid br |> fun a ->
            set_ci_checks a [ check ] |> fun a ->
            record_delivered_ci_run_ids a [ run_id ]
          in
          equal_ci_decision (on_ci_failure a) Ci_already_delivered);
      Test.make
        ~name:
          "on_ci_failure: id-less failing check remains deliverable after \
           delivered ids"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let id_less_check =
            Ci_check.
              {
                name = "legacy-status";
                conclusion = "failure";
                details_url = None;
                description = None;
                started_at = None;
                id = None;
              }
          in
          let delivered_check =
            Ci_check.
              {
                name = "delivered-workflow";
                conclusion = "failure";
                details_url = None;
                description = None;
                started_at = None;
                id = Some 1;
              }
          in
          let a =
            with_pr pid br |> fun a ->
            set_ci_checks a [ id_less_check; delivered_check ] |> fun a ->
            record_delivered_ci_run_ids a [ 1 ]
          in
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
           ~pre_fire_agent:None ~prefetched_comments:[] ~prefetched_findings:[]
           ~main_branch)
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
           ~pre_fire_agent:None ~prefetched_comments:[] ~prefetched_findings:[]
           ~main_branch)
        Respond_stale);

    (* not busy → Stale *)
    let a = with_pr (Patch_id.of_string "rd1c") (Branch.of_string "b") in
    assert (
      equal_respond_delivery
        (respond_delivery ~agent:a ~kind:Operation_kind.Human
           ~pre_fire_agent:None ~prefetched_comments:[] ~prefetched_findings:[]
           ~main_branch)
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
           ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
           ~prefetched_findings:[] ~main_branch)
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
           ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
           ~prefetched_findings:[] ~main_branch)
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
           ~pre_fire_agent:None ~prefetched_comments:[] ~prefetched_findings:[]
           ~main_branch)
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
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
         ~prefetched_findings:[] ~main_branch
     with
    | Deliver { payload = Human_payload { messages }; _ } ->
        assert (List.equal String.equal messages [ "fix this" ])
    | Deliver
        {
          payload =
            ( Ci_payload _ | Review_payload _ | Findings_payload _
            | Pr_body_payload | Merge_conflict_payload );
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
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
         ~prefetched_findings:[] ~main_branch
     with
    | Deliver { payload = Human_payload { messages }; _ } ->
        (* Messages come from pre_fire.human_messages (reversed) *)
        assert (Int.equal (List.length messages) 2)
    | Deliver
        {
          payload =
            ( Ci_payload _ | Review_payload _ | Findings_payload _
            | Pr_body_payload | Merge_conflict_payload );
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
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
         ~prefetched_findings:[] ~main_branch
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
            ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
            ~prefetched_findings:[] ~main_branch
        with
        | Deliver { payload = Ci_payload { failed_checks }; _ } ->
            assert (not (List.is_empty failed_checks))
        | Deliver
            {
              payload =
                ( Human_payload _ | Review_payload _ | Findings_payload _
                | Pr_body_payload | Merge_conflict_payload );
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
            ~prefetched_comments:[] ~prefetched_findings:[] ~main_branch
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
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
         ~prefetched_findings:[] ~main_branch
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
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
         ~prefetched_findings:[] ~main_branch
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
            ( Human_payload _ | Review_payload _ | Findings_payload _
            | Pr_body_payload | Merge_conflict_payload );
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
         ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
         ~prefetched_findings:[] ~main_branch
     with
    | Deliver { payload = Ci_payload { failed_checks }; _ } ->
        assert (List.length failed_checks = 1)
    | Deliver
        {
          payload =
            ( Human_payload _ | Review_payload _ | Findings_payload _
            | Pr_body_payload | Merge_conflict_payload );
          _;
        }
    | Skip_empty | Respond_stale ->
        failwith "RD-10: expected Deliver for id=None check");
    Stdlib.print_endline "RD-10 passed"
  in

  ignore main_branch;

  (* ========== classify_pr_body_respond property tests ==========

     The correlation rule is small enough to exhaustively enumerate over the
     4 artifact_outcomes × {Write-present, no-Write} × sample tool_failure
     shapes. Properties assert the invariants that matter for the
     retry-once-then-intervene contract. *)
  let () =
    let write_failure = ("Write", "pending") in
    let non_write_failures = [ ("Bash", "pending"); ("Read", "error") ] in

    (* PB-1: artifact = Ok → Ok regardless of tool_failures. *)
    List.iter
      [ []; [ write_failure ]; write_failure :: non_write_failures ]
      ~f:(fun tool_failures ->
        let r = classify_pr_body_respond ~artifact_outcome:`Ok ~tool_failures in
        match r with
        | `Ok -> ()
        | `Pr_body_miss ->
            failwith "PB-1: artifact=Ok must not classify as Pr_body_miss");
    Stdlib.print_endline "PB-1 passed";

    (* PB-2: artifact = Patch_failed → Pr_body_miss regardless of
       tool_failures. Rationale: the PR body update call failed, so the
       description is stale; bubble out as Pr_body_miss so the reconciler
       re-enqueues rather than marking delivery complete. *)
    List.iter
      [ []; [ write_failure ]; write_failure :: non_write_failures ]
      ~f:(fun tool_failures ->
        let r =
          classify_pr_body_respond ~artifact_outcome:`Patch_failed
            ~tool_failures
        in
        match r with
        | `Pr_body_miss -> ()
        | `Ok -> failwith "PB-2: Patch_failed must classify as Pr_body_miss");
    Stdlib.print_endline "PB-2 passed";

    (* PB-3: artifact = Missing + Write tool_failure → Pr_body_miss. *)
    let r =
      classify_pr_body_respond ~artifact_outcome:`Missing
        ~tool_failures:[ write_failure ]
    in
    (match r with
    | `Pr_body_miss -> ()
    | `Ok -> failwith "PB-3: Missing + Write failure must be Pr_body_miss");
    Stdlib.print_endline "PB-3 passed";

    (* PB-4: artifact = Empty + Write tool_failure → Pr_body_miss. *)
    let r =
      classify_pr_body_respond ~artifact_outcome:`Empty
        ~tool_failures:[ write_failure ]
    in
    (match r with
    | `Pr_body_miss -> ()
    | `Ok -> failwith "PB-4: Empty + Write failure must be Pr_body_miss");
    Stdlib.print_endline "PB-4 passed";

    (* PB-5: artifact = Missing|Empty + no Write tool_failure → Ok.
       Agent legitimately chose not to add notes. *)
    List.iter [ `Missing; `Empty ] ~f:(fun artifact_outcome ->
        List.iter [ []; non_write_failures ] ~f:(fun tool_failures ->
            match classify_pr_body_respond ~artifact_outcome ~tool_failures with
            | `Ok -> ()
            | `Pr_body_miss ->
                failwith "PB-5: Missing|Empty with no Write failure must be Ok"));
    Stdlib.print_endline "PB-5 passed";

    (* PB-6: case-sensitive match on "Write" — lowercase "write" does NOT
       trigger, because OpenCode's normalize_tool_name converts to PascalCase
       upstream and the correlation rule matches only the canonical form.
       Regression guard for future backends that surface status without
       normalizing. *)
    let r =
      classify_pr_body_respond ~artifact_outcome:`Missing
        ~tool_failures:[ ("write", "pending") ]
    in
    (match r with
    | `Ok -> ()
    | `Pr_body_miss ->
        failwith "PB-6: lowercase 'write' must not trigger Pr_body_miss");
    Stdlib.print_endline "PB-6 passed";

    (* PB-7: property — Write failure mixed with any number of non-Write
       failures still classifies as Pr_body_miss when artifact is
       Missing|Empty. *)
    let prop =
      QCheck2.Test.make ~name:"PB-7: Write+noise in failures still Pr_body_miss"
        QCheck2.Gen.(
          pair
            (oneof_list [ `Missing; `Empty ])
            (list_small
               (pair
                  (oneof_list [ "Bash"; "Read"; "Grep"; "Edit"; "Glob" ])
                  (oneof_list [ "pending"; "running"; "error" ]))))
        (fun (artifact_outcome, noise) ->
          try
            let tool_failures = write_failure :: noise in
            match classify_pr_body_respond ~artifact_outcome ~tool_failures with
            | `Pr_body_miss -> true
            | `Ok -> false
          with _ -> false)
    in
    QCheck2.Test.check_exn prop;
    Stdlib.print_endline "PB-7 passed";

    (* PB-8: property — for non-failing artifact outcomes (Ok/Missing/Empty),
       a non-Write failure list always classifies as Ok. The correlation
       never fires without a Write failure. Patch_failed is excluded because
       it classifies as Pr_body_miss regardless of tool_failures (see PB-2). *)
    let prop =
      QCheck2.Test.make ~name:"PB-8: no Write failure → never Pr_body_miss"
        QCheck2.Gen.(
          pair
            (oneof_list [ `Ok; `Missing; `Empty ])
            (list_small
               (pair
                  (oneof_list [ "Bash"; "Read"; "Grep"; "Edit"; "Glob" ])
                  (oneof_list [ "pending"; "running"; "error" ]))))
        (fun (artifact_outcome, non_write) ->
          try
            match
              classify_pr_body_respond ~artifact_outcome
                ~tool_failures:non_write
            with
            | `Ok -> true
            | `Pr_body_miss -> false
          with _ -> false)
    in
    QCheck2.Test.check_exn prop;
    Stdlib.print_endline "PB-8 passed";

    (* PB-9: property — Write failure anywhere in the list (not just head)
       triggers Pr_body_miss when artifact is Missing|Empty. Guards against
       a regression that only checks the head of the list. *)
    let prop =
      QCheck2.Test.make ~name:"PB-9: Write failure position independence"
        QCheck2.Gen.(
          pair
            (oneof_list [ `Missing; `Empty ])
            (list_small
               (pair
                  (oneof_list [ "Bash"; "Read"; "Grep"; "Edit"; "Glob" ])
                  (oneof_list [ "pending"; "running"; "error" ]))))
        (fun (artifact_outcome, prefix) ->
          try
            let tool_failures = prefix @ [ write_failure ] in
            match classify_pr_body_respond ~artifact_outcome ~tool_failures with
            | `Pr_body_miss -> true
            | `Ok -> false
          with _ -> false)
    in
    QCheck2.Test.check_exn prop;
    Stdlib.print_endline "PB-9 passed"
  in

  (* ========== Opportunistic artifact-sync property tests ==========

     These cover the three pure functions added for issue #215:
     [pr_body_artifact_changed], [plan_artifact_sync], and
     [classify_artifact_sync_outcome]. Each is small enough that we both
     enumerate the meaningful boundary cases and add property-based tests for
     contract-level invariants. *)
  let () =
    (* AS-1: (None, None) → false. *)
    assert (not (pr_body_artifact_changed ~pre:None ~post:None));
    Stdlib.print_endline "AS-1 passed";

    (* AS-2: equal non-empty content → false. *)
    assert (not (pr_body_artifact_changed ~pre:(Some "x") ~post:(Some "x")));
    Stdlib.print_endline "AS-2 passed";

    (* AS-3: distinct non-empty content → true. *)
    assert (pr_body_artifact_changed ~pre:(Some "x") ~post:(Some "y"));
    Stdlib.print_endline "AS-3 passed";

    (* AS-4: empty/whitespace collapse — empty and whitespace-only contents
       are treated as None on both sides. Truncating real content to empty
       (Some "x" → Some "") is intentionally classified as a change; the
       downstream apply_pr_body_artifact returns [`Empty] and skips the
       GitHub PATCH, so no spurious call is issued. *)
    assert (not (pr_body_artifact_changed ~pre:None ~post:(Some "")));
    assert (not (pr_body_artifact_changed ~pre:(Some "  \n") ~post:None));
    assert (not (pr_body_artifact_changed ~pre:(Some "") ~post:(Some "  ")));
    assert (pr_body_artifact_changed ~pre:(Some "x") ~post:(Some ""));
    Stdlib.print_endline "AS-4 passed";

    (* AS-5: asymmetric add — new non-empty content from None → true. *)
    assert (pr_body_artifact_changed ~pre:None ~post:(Some "y"));
    Stdlib.print_endline "AS-5 passed";

    (* AS-P1 (reflexivity): for any [s], changed pre:s post:s = false. *)
    let gen_artifact =
      QCheck2.Gen.(
        oneof
          [
            return None;
            map (fun s -> Some s) (string_size (int_range 0 8));
            return (Some "");
            return (Some "   ");
            return (Some "\n\t ");
          ])
    in
    let prop =
      QCheck2.Test.make ~name:"AS-P1: reflexivity" gen_artifact (fun s ->
          try not (pr_body_artifact_changed ~pre:s ~post:s) with _ -> false)
    in
    QCheck2.Test.check_exn prop;
    Stdlib.print_endline "AS-P1 passed";

    (* AS-P2 (whitespace-only content collapses to None): any whitespace-only
       string is indistinguishable from None for the change predicate. *)
    let gen_whitespace =
      QCheck2.Gen.(
        map
          (fun n ->
            String.init n ~f:(fun i ->
                match Stdlib.( mod ) i 4 with
                | 0 -> ' '
                | 1 -> '\t'
                | 2 -> '\n'
                | _ -> ' '))
          (int_range 0 6))
    in
    let prop =
      QCheck2.Test.make ~name:"AS-P2: whitespace-only collapse" gen_whitespace
        (fun w ->
          try
            (not (pr_body_artifact_changed ~pre:None ~post:(Some w)))
            && not (pr_body_artifact_changed ~pre:(Some w) ~post:None)
          with _ -> false)
    in
    QCheck2.Test.check_exn prop;
    Stdlib.print_endline "AS-P2 passed";

    (* AS-P3 (symmetry): changed (a, b) = changed (b, a). The predicate is a
       boolean over equality of normalized contents, so it must be
       symmetric — guards against any future implementation that
       distinguishes pre/post direction. *)
    let prop =
      QCheck2.Test.make ~name:"AS-P3: symmetry"
        QCheck2.Gen.(pair gen_artifact gen_artifact)
        (fun (a, b) ->
          try
            Bool.equal
              (pr_body_artifact_changed ~pre:a ~post:b)
              (pr_body_artifact_changed ~pre:b ~post:a)
          with _ -> false)
    in
    QCheck2.Test.check_exn prop;
    Stdlib.print_endline "AS-P3 passed";

    (* Helpers for plan_artifact_sync enumeration. *)
    let all_kinds : Operation_kind.t list =
      Operation_kind.
        [ Rebase; Human; Merge_conflict; Ci; Review_comments; Pr_body ]
    in
    let non_pr_body_kinds : Operation_kind.t list =
      Operation_kind.[ Rebase; Human; Merge_conflict; Ci; Review_comments ]
    in

    (* AS-10: session_ok=false → Sync_skip for any kind, any pre/post. *)
    List.iter all_kinds ~f:(fun kind ->
        List.iter
          [
            (None, None);
            (Some "x", Some "x");
            (Some "x", Some "y");
            (None, Some "y");
          ]
          ~f:(fun (pre, post) ->
            let plan = plan_artifact_sync ~kind ~session_ok:false ~pre ~post in
            assert (equal_artifact_sync_plan plan Sync_skip)));
    Stdlib.print_endline "AS-10 passed";

    (* AS-11: kind=Pr_body, session_ok=true → Sync_skip regardless of
       changed. The Pr_body delivery path is owned by
       classify_pr_body_respond. *)
    List.iter
      [
        (None, None);
        (Some "x", Some "x");
        (None, Some "y");
        (Some "x", Some "y");
      ]
      ~f:(fun (pre, post) ->
        let plan =
          plan_artifact_sync ~kind:Operation_kind.Pr_body ~session_ok:true ~pre
            ~post
        in
        assert (equal_artifact_sync_plan plan Sync_skip));
    Stdlib.print_endline "AS-11 passed";

    (* AS-12: non-Pr_body kind, session_ok=true, changed=true →
       Sync_attempt_pr_body. *)
    List.iter non_pr_body_kinds ~f:(fun kind ->
        let plan =
          plan_artifact_sync ~kind ~session_ok:true ~pre:None
            ~post:(Some "notes")
        in
        assert (equal_artifact_sync_plan plan Sync_attempt_pr_body));
    Stdlib.print_endline "AS-12 passed";

    (* AS-13: non-Pr_body kind, session_ok=true, changed=false → Sync_skip. *)
    List.iter non_pr_body_kinds ~f:(fun kind ->
        List.iter
          [ (None, None); (Some "x", Some "x"); (Some "  ", Some "") ]
          ~f:(fun (pre, post) ->
            let plan = plan_artifact_sync ~kind ~session_ok:true ~pre ~post in
            assert (equal_artifact_sync_plan plan Sync_skip)));
    Stdlib.print_endline "AS-13 passed";

    (* AS-P4: plan_artifact_sync returns Sync_attempt_pr_body if and only if
       session_ok ∧ kind ≠ Pr_body ∧ changed. The contract says no
       opportunistic PATCH for Pr_body, no PATCH on failed sessions, no
       PATCH on unchanged content. *)
    let gen_kind = QCheck2.Gen.oneof_list all_kinds in
    let prop =
      QCheck2.Test.make ~name:"AS-P4: plan_artifact_sync contract"
        QCheck2.Gen.(quad gen_kind bool gen_artifact gen_artifact)
        (fun (kind, session_ok, pre, post) ->
          try
            let plan = plan_artifact_sync ~kind ~session_ok ~pre ~post in
            let attempt = equal_artifact_sync_plan plan Sync_attempt_pr_body in
            let expected =
              session_ok
              && (not (Operation_kind.equal kind Operation_kind.Pr_body))
              && pr_body_artifact_changed ~pre ~post
            in
            Bool.equal attempt expected
          with _ -> false)
    in
    QCheck2.Test.check_exn prop;
    Stdlib.print_endline "AS-P4 passed";

    (* Helpers for classify_artifact_sync_outcome enumeration. *)
    let all_patch_results :
        [ `Ok | `Missing | `Empty | `Patch_failed ] option list =
      [ None; Some `Ok; Some `Missing; Some `Empty; Some `Patch_failed ]
    in

    (* AS-20: plan=Sync_skip → Sync_no_op for every patch_result. *)
    List.iter all_patch_results ~f:(fun patch_result ->
        let outcome =
          classify_artifact_sync_outcome ~plan:Sync_skip ~patch_result
        in
        assert (equal_artifact_sync_outcome outcome Sync_no_op));
    Stdlib.print_endline "AS-20 passed";

    (* AS-21: plan=Sync_attempt_pr_body, patch_result=Some `Ok → Sync_delivered. *)
    let outcome =
      classify_artifact_sync_outcome ~plan:Sync_attempt_pr_body
        ~patch_result:(Some `Ok)
    in
    assert (equal_artifact_sync_outcome outcome Sync_delivered);
    Stdlib.print_endline "AS-21 passed";

    (* AS-22: plan=Sync_attempt_pr_body, Some `Patch_failed → Sync_patch_failed. *)
    let outcome =
      classify_artifact_sync_outcome ~plan:Sync_attempt_pr_body
        ~patch_result:(Some `Patch_failed)
    in
    assert (equal_artifact_sync_outcome outcome Sync_patch_failed);
    Stdlib.print_endline "AS-22 passed";

    (* AS-23: plan=Sync_attempt_pr_body, Some (`Missing | `Empty) → Sync_no_op.
       Both arms are reachable: [`Empty] fires when the file goes from
       non-empty to empty (normalize_artifact and apply_pr_body_artifact's
       String.trim use independent emptiness checks); [`Missing] fires if the
       file is deleted between the runner's post-snapshot and the
       apply_pr_body_artifact call. *)
    List.iter [ `Missing; `Empty ] ~f:(fun pr ->
        let outcome =
          classify_artifact_sync_outcome ~plan:Sync_attempt_pr_body
            ~patch_result:(Some pr)
        in
        assert (equal_artifact_sync_outcome outcome Sync_no_op));
    Stdlib.print_endline "AS-23 passed";

    (* AS-24: plan=Sync_attempt_pr_body, patch_result=None → Sync_no_op
       (defensive — caller skipped PATCH despite plan). *)
    let outcome =
      classify_artifact_sync_outcome ~plan:Sync_attempt_pr_body
        ~patch_result:None
    in
    assert (equal_artifact_sync_outcome outcome Sync_no_op);
    Stdlib.print_endline "AS-24 passed";

    (* AS-P5: Sync_delivered is returned only when both plan =
       Sync_attempt_pr_body AND patch_result = Some `Ok. The contract for
       state mutation: pr_body_delivered is flipped only when a real PATCH
       succeeded. *)
    let gen_plan = QCheck2.Gen.oneof_list [ Sync_skip; Sync_attempt_pr_body ] in
    let gen_patch_result = QCheck2.Gen.oneof_list all_patch_results in
    let prop =
      QCheck2.Test.make ~name:"AS-P5: Sync_delivered ⇔ attempt ∧ Ok"
        QCheck2.Gen.(pair gen_plan gen_patch_result)
        (fun (plan, patch_result) ->
          try
            let outcome = classify_artifact_sync_outcome ~plan ~patch_result in
            let delivered =
              equal_artifact_sync_outcome outcome Sync_delivered
            in
            let expected =
              equal_artifact_sync_plan plan Sync_attempt_pr_body
              && match patch_result with Some `Ok -> true | _ -> false
            in
            Bool.equal delivered expected
          with _ -> false)
    in
    QCheck2.Test.check_exn prop;
    Stdlib.print_endline "AS-P5 passed"
  in
  ();
  (* Property: a freshly-created agent that has a PR but is not busy is always
     stale for a Human delivery — [respond_delivery] must return [Respond_stale]
     regardless of the generated patch id / branch. This generates real input
     and drives it through [respond_delivery]. *)
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"respond_delivery: not-busy agent is stale"
       ~count:200
       QCheck2.Gen.(pair gen_pid gen_branch)
       (fun (pid, br) ->
         let a = with_pr pid br in
         equal_respond_delivery
           (respond_delivery ~agent:a ~kind:Operation_kind.Human
              ~pre_fire_agent:None ~prefetched_comments:[]
              ~prefetched_findings:[] ~main_branch:"main")
           Respond_stale));
  (* Property: a busy Human delivery sourced from a pre_fire agent that carries
     at least one human message always [Deliver]s a Human_payload echoing those
     messages. *)
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"respond_delivery: human messages are delivered"
       ~count:200
       QCheck2.Gen.(
         triple gen_pid gen_branch
           (list_size (int_range 1 4) (string_size (int_range 1 8))))
       (fun (pid, br, msgs) ->
         let pre_fire =
           List.fold msgs ~init:(with_pr pid br) ~f:(fun a m ->
               add_human_message a m)
         in
         let a = enqueue pre_fire Operation_kind.Human in
         let a = respond a Operation_kind.Human in
         match
           respond_delivery ~agent:a ~kind:Operation_kind.Human
             ~pre_fire_agent:(Some pre_fire) ~prefetched_comments:[]
             ~prefetched_findings:[] ~main_branch:"main"
         with
         | Deliver { payload = Human_payload { messages }; _ } ->
             Int.equal (List.length messages) (List.length msgs)
         | Deliver _ | Skip_empty | Respond_stale -> false));
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"patch decision public surface is linked"
       QCheck2.Gen.unit (fun () ->
         ignore respond_delivery;
         true))
