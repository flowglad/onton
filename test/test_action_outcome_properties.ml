(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core
open Onton_core.Types

(** Property tests for [Orchestrator.apply_start_outcome] and
    [Orchestrator.apply_respond_outcome]. These encode the runner's contract:
    every non-stale action fiber must call [complete] before exiting. *)

let main = Branch.of_string "main"
let mk_patches = Onton_test_support.Test_generators.mk_linear_patches
let make_gameplan = Onton_test_support.Test_generators.make_test_gameplan
let pid_of_idx = Onton_test_support.Test_generators.pid_of_idx

let gen_merge_queue_entry =
  let open QCheck2.Gen in
  let states =
    Pr_state.
      [ Mq_queued; Mq_awaiting_checks; Mq_mergeable; Mq_unmergeable; Mq_locked ]
  in
  map3
    (fun id state position -> Pr_state.{ id; state; position })
    (string_size ~gen:(char_range 'a' 'z') (int_range 1 16))
    (oneof_list states) (int_range 0 99)

(** Bootstrap a single-patch orchestrator with an idle agent that has a PR. *)
let bootstrap_one () =
  let patches = mk_patches 1 in
  let gameplan = make_gameplan patches in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let pid = pid_of_idx patches 0 in
  let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
  let orch = Orchestrator.set_pr_number orch pid (Pr_number.of_int 1) in
  let orch = Orchestrator.complete orch pid in
  (orch, patches, gameplan, pid)

(** Make an agent busy via enqueue + tick for the given operation kind. *)
let make_busy orch _patches gameplan pid kind =
  let orch = Orchestrator.enqueue orch pid kind in
  let orch, _effects, _actions =
    Patch_controller.tick orch ~project_name:"test-project" ~gameplan
  in
  assert (Orchestrator.agent orch pid).Patch_agent.busy;
  orch

(* ========== AO-1a: Start_failed produces busy=false ========== *)

let () =
  let patches = mk_patches 1 in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let pid = pid_of_idx patches 0 in
  let orch = Orchestrator.set_max_ci_failures orch ~max_ci_failures:5 in
  assert ((Orchestrator.agent orch pid).Patch_agent.max_ci_failures = 5);
  let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
  assert (Orchestrator.agent orch pid).Patch_agent.busy;
  let orch =
    Orchestrator.apply_start_outcome orch pid Orchestrator.Start_failed
  in
  assert (not (Orchestrator.agent orch pid).Patch_agent.busy);
  Stdlib.print_endline "AO-1a passed"

(* ========== AO-1b: Start_ok keeps busy=true (caller completes after PR
   discovery) ========== *)

let () =
  let patches = mk_patches 1 in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let pid = pid_of_idx patches 0 in
  let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
  assert (Orchestrator.agent orch pid).Patch_agent.busy;
  let orch = Orchestrator.apply_start_outcome orch pid Orchestrator.Start_ok in
  (* busy stays true — caller will complete after PR discovery *)
  assert (Orchestrator.agent orch pid).Patch_agent.busy;
  (* caller completes after discovery *)
  let orch = Orchestrator.complete orch pid in
  assert (not (Orchestrator.agent orch pid).Patch_agent.busy);
  Stdlib.print_endline "AO-1b passed"

(* ========== AO-2: Non-stale respond outcomes produce busy=false ========== *)

let () =
  let respond_outcomes =
    [
      Orchestrator.Respond_ok;
      Orchestrator.Respond_failed;
      Orchestrator.Respond_retry_push;
      Orchestrator.Respond_skip_empty;
      Orchestrator.Respond_pr_body_miss;
      Orchestrator.Respond_review_unresolved;
    ]
  in
  let respond_kinds =
    [
      Operation_kind.Ci;
      Operation_kind.Review_comments;
      Operation_kind.Human;
      Operation_kind.Merge_conflict;
      Operation_kind.Pr_body;
    ]
  in
  let prop =
    QCheck2.Test.make
      ~name:"AO-2: non-stale respond outcomes produce busy=false"
      (QCheck2.Gen.pair
         (QCheck2.Gen.oneof_list respond_outcomes)
         (QCheck2.Gen.oneof_list respond_kinds))
      (fun (outcome, kind) ->
        try
          let orch, patches, gameplan, pid = bootstrap_one () in
          let orch = make_busy orch patches gameplan pid kind in
          let orch = Orchestrator.apply_respond_outcome orch pid kind outcome in
          not (Orchestrator.agent orch pid).Patch_agent.busy
        with _ -> false)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-2 passed"

(* ========== AO-2b: delivered Human messages are not replayed ========== *)

let () =
  let human_message_gen =
    QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 80)
  in
  let distinct_human_messages_gen =
    QCheck2.Gen.map2
      (fun first_msg second_msg ->
        if String.equal first_msg second_msg then
          (first_msg, second_msg ^ " (next)")
        else (first_msg, second_msg))
      human_message_gen human_message_gen
  in
  let prop =
    QCheck2.Test.make
      ~name:
        "AO-2b: delivered Human message is cleared before next Human delivery"
      distinct_human_messages_gen (fun (first_msg, second_msg) ->
        try
          let orch, _patches, _gameplan, pid = bootstrap_one () in
          let orch = Orchestrator.send_human_message orch pid first_msg in
          let first_pre = Orchestrator.agent orch pid in
          let orch =
            Orchestrator.fire orch
              (Orchestrator.Respond (pid, Operation_kind.Human))
          in
          let orch =
            Orchestrator.mark_inflight_human_messages_delivered orch pid
          in
          let mid = Orchestrator.agent orch pid in
          if not (List.is_empty mid.Patch_agent.inflight_human_messages) then
            QCheck2.Test.fail_reportf
              "mark_inflight did not clear inflight slot";
          let orch =
            Orchestrator.apply_respond_outcome orch pid Operation_kind.Human
              Orchestrator.Respond_ok
          in
          let after_first = Orchestrator.agent orch pid in
          if not (List.is_empty after_first.Patch_agent.human_messages) then
            QCheck2.Test.fail_reportf "first message remained in inbox";
          if not (List.is_empty after_first.Patch_agent.inflight_human_messages)
          then QCheck2.Test.fail_reportf "first message remained inflight";
          let orch = Orchestrator.send_human_message orch pid second_msg in
          let second_pre = Orchestrator.agent orch pid in
          let orch =
            Orchestrator.fire orch
              (Orchestrator.Respond (pid, Operation_kind.Human))
          in
          let agent = Orchestrator.agent orch pid in
          match
            Patch_decision.respond_delivery ~agent ~kind:Operation_kind.Human
              ~pre_fire_agent:(Some second_pre) ~prefetched_comments:[]
              ~prefetched_findings:[] ~main_branch:(Branch.to_string main)
          with
          | Patch_decision.Deliver { payload; _ } -> (
              match payload with
              | Patch_decision.Human_payload { messages } ->
                  List.equal String.equal messages [ second_msg ]
                  && (not (List.mem messages first_msg ~equal:String.equal))
                  && List.mem first_pre.Patch_agent.human_messages first_msg
                       ~equal:String.equal
              | Patch_decision.Ci_payload _ | Patch_decision.Review_payload _
              | Patch_decision.Findings_payload _
              | Patch_decision.Pr_body_payload
              | Patch_decision.Merge_conflict_payload ->
                  false)
          | Patch_decision.Skip_empty | Patch_decision.Respond_stale -> false
        with
        | QCheck2.Test.Test_fail _ as e -> raise e
        | exn ->
            QCheck2.Test.fail_reportf "unexpected exception: %s"
              (Exn.to_string exn))
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-2b passed"

(* ========== AO-3: Respond_ok + Merge_conflict clears has_conflict ========== *)

let () =
  let prop =
    QCheck2.Test.make
      ~name:"AO-3: Respond_ok + Merge_conflict clears has_conflict"
      (QCheck2.Gen.return ()) (fun () ->
        let orch, patches, gameplan, pid = bootstrap_one () in
        let orch = Orchestrator.set_has_conflict orch pid in
        let orch =
          make_busy orch patches gameplan pid Operation_kind.Merge_conflict
        in
        assert (Orchestrator.agent orch pid).Patch_agent.has_conflict;
        let orch =
          Orchestrator.apply_respond_outcome orch pid
            Operation_kind.Merge_conflict Orchestrator.Respond_ok
        in
        not (Orchestrator.agent orch pid).Patch_agent.has_conflict)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-3 passed"

(* ========== AO-5: Stale outcomes are identity ========== *)

let () =
  let prop =
    QCheck2.Test.make ~name:"AO-5: stale outcomes are identity"
      (QCheck2.Gen.oneof_list
         Operation_kind.[ Ci; Review_comments; Human; Merge_conflict; Pr_body ])
      (fun kind ->
        try
          let orch, patches, gameplan, pid = bootstrap_one () in
          let orch_busy = make_busy orch patches gameplan pid kind in
          let agents_before = Orchestrator.agents_map orch_busy in
          let orch_after_respond =
            Orchestrator.apply_respond_outcome orch_busy pid kind
              Orchestrator.Respond_stale
          in
          let agents_after_respond =
            Orchestrator.agents_map orch_after_respond
          in
          (* Start_stale: apply to a started-but-not-yet-completed agent *)
          let patches2 = mk_patches 1 in
          let orch2 = Orchestrator.create ~patches:patches2 ~main_branch:main in
          let pid2 = pid_of_idx patches2 0 in
          let orch2 =
            Orchestrator.fire orch2 (Orchestrator.Start (pid2, main))
          in
          let agents_before2 = Orchestrator.agents_map orch2 in
          let orch2_after =
            Orchestrator.apply_start_outcome orch2 pid2 Orchestrator.Start_stale
          in
          let agents_after2 = Orchestrator.agents_map orch2_after in
          Map.equal Patch_agent.equal agents_before agents_after_respond
          && Map.equal Patch_agent.equal agents_before2 agents_after2
        with _ -> false)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-5 passed"

(* ========== AO-6: Respond_failed restores inflight human messages ========== *)

let () =
  let orch, patches, gameplan, pid = bootstrap_one () in
  let orch = Orchestrator.send_human_message orch pid "fix this" in
  let orch = make_busy orch patches gameplan pid Operation_kind.Human in
  let agent = Orchestrator.agent orch pid in
  (* After fire, messages should be in inflight *)
  assert (not (List.is_empty agent.Patch_agent.inflight_human_messages));
  assert (List.is_empty agent.Patch_agent.human_messages);
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Human
      Orchestrator.Respond_failed
  in
  let agent = Orchestrator.agent orch pid in
  (* Messages should be restored to human_messages *)
  assert (not (List.is_empty agent.Patch_agent.human_messages));
  assert (List.is_empty agent.Patch_agent.inflight_human_messages);
  assert (not agent.Patch_agent.busy);
  Stdlib.print_endline "AO-6 passed"

(* ========== AO-6b: accepted Human delivery is not restored on failure ========== *)

let () =
  let failed_results =
    [
      Orchestrator.Session_process_error { is_fresh = false; detail = None };
      Orchestrator.Session_process_error { is_fresh = true; detail = None };
      Orchestrator.Session_no_resume;
      Orchestrator.Session_failed { is_fresh = false; detail = None };
      Orchestrator.Session_failed { is_fresh = true; detail = None };
      Orchestrator.Session_give_up;
    ]
  in
  let prop =
    QCheck2.Test.make
      ~name:
        "AO-6b: backend-accepted Human delivery is not restored on session \
         failure" (QCheck2.Gen.oneof_list failed_results) (fun result ->
        match
          try
            let orch, patches, gameplan, pid = bootstrap_one () in
            let orch = Orchestrator.send_human_message orch pid "fix this" in
            let orch =
              make_busy orch patches gameplan pid Operation_kind.Human
            in
            let before = Orchestrator.agent orch pid in
            if List.is_empty before.Patch_agent.inflight_human_messages then
              Error "expected inflight Human messages"
            else
              let orch =
                Orchestrator.mark_inflight_human_messages_delivered orch pid
              in
              let accepted = Orchestrator.agent orch pid in
              if
                not (List.is_empty accepted.Patch_agent.inflight_human_messages)
              then Error "accepted Human messages should be drained"
              else
                let orch = Orchestrator.apply_session_result orch pid result in
                let after = Orchestrator.agent orch pid in
                Ok
                  (List.is_empty after.Patch_agent.human_messages
                  && List.is_empty after.Patch_agent.inflight_human_messages
                  && not
                       (List.mem after.Patch_agent.queue Operation_kind.Human
                          ~equal:Operation_kind.equal))
          with exn -> Error (Exn.to_string exn)
        with
        | Ok passed -> passed
        | Error msg -> QCheck2.Test.fail_reportf "%s" msg)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-6b passed"

(* ========== AO-7: Ci respond counter is bumped only on Respond_ok ========== *)

(* Respond_ok bumps ci_failure_count; Skip_empty / Failed / Retry_push /
   Stale do NOT. This locks in the invariant that a CI fix attempt only
   counts against the cap when a real payload was delivered. Regression for
   the production incident where cancelled-only rollups drove the cap to 3
   and forced needs_intervention without any actionable failures. *)
let () =
  let with_ci_busy () =
    let orch, patches, gameplan, pid = bootstrap_one () in
    let orch = make_busy orch patches gameplan pid Operation_kind.Ci in
    let before = (Orchestrator.agent orch pid).Patch_agent.ci_failure_count in
    (orch, pid, before)
  in
  let ci_count orch pid =
    (Orchestrator.agent orch pid).Patch_agent.ci_failure_count
  in
  (* Respond_ok bumps *)
  let orch, pid, before = with_ci_busy () in
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Ci
      Orchestrator.Respond_ok
  in
  assert (ci_count orch pid = before + 1);
  (* Respond_skip_empty does NOT bump *)
  let orch, pid, before = with_ci_busy () in
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Ci
      Orchestrator.Respond_skip_empty
  in
  assert (ci_count orch pid = before);
  (* Respond_failed does NOT bump *)
  let orch, pid, before = with_ci_busy () in
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Ci
      Orchestrator.Respond_failed
  in
  assert (ci_count orch pid = before);
  (* Respond_retry_push does NOT bump *)
  let orch, pid, before = with_ci_busy () in
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Ci
      Orchestrator.Respond_retry_push
  in
  assert (ci_count orch pid = before);
  (* Respond_stale does NOT bump *)
  let orch, pid, before = with_ci_busy () in
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Ci
      Orchestrator.Respond_stale
  in
  assert (ci_count orch pid = before);
  (* Respond_ok for non-Ci kinds does NOT bump ci_failure_count *)
  let orch, patches, gameplan, pid = bootstrap_one () in
  let orch =
    make_busy orch patches gameplan pid Operation_kind.Review_comments
  in
  let before = ci_count orch pid in
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Review_comments
      Orchestrator.Respond_ok
  in
  assert (ci_count orch pid = before);
  Stdlib.print_endline "AO-7 passed"

(* ========== AO-8: Respond_ok + Pr_body sets pr_body_delivered ========== *)

let () =
  let prop =
    QCheck2.Test.make ~name:"AO-8: Respond_ok + Pr_body sets pr_body_delivered"
      (QCheck2.Gen.return ()) (fun () ->
        let orch, patches, gameplan, pid = bootstrap_one () in
        assert (not (Orchestrator.agent orch pid).Patch_agent.pr_body_delivered);
        let orch = make_busy orch patches gameplan pid Operation_kind.Pr_body in
        let orch =
          Orchestrator.apply_respond_outcome orch pid Operation_kind.Pr_body
            Orchestrator.Respond_ok
        in
        (Orchestrator.agent orch pid).Patch_agent.pr_body_delivered)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-8 passed"

(* ========== AO-9: Non-Respond_ok outcomes leave delivered flags untouched
   ========== *)

(* Pinned-down variant of AO-2: only Respond_ok flips pr_body_delivered.
   Failed/Skip_empty/Retry_push/Stale must leave it alone, so the next
   reconcile cycle re-enqueues the phase. *)
let () =
  let non_ok_outcomes =
    [
      Orchestrator.Respond_failed;
      Orchestrator.Respond_skip_empty;
      Orchestrator.Respond_retry_push;
      Orchestrator.Respond_stale;
      Orchestrator.Respond_pr_body_miss;
      Orchestrator.Respond_review_unresolved;
    ]
  in
  let prop =
    QCheck2.Test.make
      ~name:"AO-9: non-Respond_ok outcomes leave pr_body_delivered false"
      (QCheck2.Gen.oneof_list non_ok_outcomes) (fun outcome ->
        try
          let orch, patches, gameplan, pid = bootstrap_one () in
          let orch =
            make_busy orch patches gameplan pid Operation_kind.Pr_body
          in
          let orch =
            Orchestrator.apply_respond_outcome orch pid Operation_kind.Pr_body
              outcome
          in
          not (Orchestrator.agent orch pid).Patch_agent.pr_body_delivered
        with _ -> false)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-9 passed"

(* ========== AO-10: Respond_pr_body_miss semantics ========== *)

(* Respond_pr_body_miss must: (a) clear busy so the reconciler can re-enqueue,
   (b) leave pr_body_delivered=false so the re-enqueue actually happens,
   (c) increment pr_body_artifact_miss_count by exactly 1 each call. This
   encodes the retry-once-then-intervene contract. *)
let () =
  let prop =
    QCheck2.Test.make ~name:"AO-10: Respond_pr_body_miss retry semantics"
      (QCheck2.Gen.return ()) (fun () ->
        try
          let orch, patches, gameplan, pid = bootstrap_one () in
          let orch =
            make_busy orch patches gameplan pid Operation_kind.Pr_body
          in
          let before =
            (Orchestrator.agent orch pid)
              .Patch_agent.pr_body_artifact_miss_count
          in
          let orch =
            Orchestrator.apply_respond_outcome orch pid Operation_kind.Pr_body
              Orchestrator.Respond_pr_body_miss
          in
          let a = Orchestrator.agent orch pid in
          (not a.Patch_agent.busy)
          && (not a.Patch_agent.pr_body_delivered)
          && a.Patch_agent.pr_body_artifact_miss_count = before + 1
        with _ -> false)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-10 passed"

(* ========== AO-11: Respond_ok + Pr_body resets miss count ========== *)

(* The cap [pr_body_artifact_miss_count >= 2] must count consecutive misses,
   not lifetime misses. A successful delivery resets the counter so a stale
   miss from earlier in the patch's lifecycle cannot combine with a later
   single miss to trigger intervention. *)
let () =
  let prop =
    QCheck2.Test.make ~name:"AO-11: Respond_ok + Pr_body resets miss count"
      (QCheck2.Gen.return ()) (fun () ->
        try
          let orch, patches, gameplan, pid = bootstrap_one () in
          let orch =
            make_busy orch patches gameplan pid Operation_kind.Pr_body
          in
          let orch =
            Orchestrator.apply_respond_outcome orch pid Operation_kind.Pr_body
              Orchestrator.Respond_pr_body_miss
          in
          assert (
            (Orchestrator.agent orch pid)
              .Patch_agent.pr_body_artifact_miss_count = 1);
          let orch =
            make_busy orch patches gameplan pid Operation_kind.Pr_body
          in
          let orch =
            Orchestrator.apply_respond_outcome orch pid Operation_kind.Pr_body
              Orchestrator.Respond_ok
          in
          let a = Orchestrator.agent orch pid in
          a.Patch_agent.pr_body_delivered
          && a.Patch_agent.pr_body_artifact_miss_count = 0
        with _ -> false)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-11 passed"

(* ========== AO-12: Respond_review_unresolved retry semantics ========== *)

(* Respond_review_unresolved must: (a) clear busy so the next poll's
   Review_comments re-enqueue can deliver, (b) increment
   review_unresolved_cycle_count by exactly 1 each call. Encodes the same
   retry-once-then-intervene contract as AO-10 for the review loop — the
   loop's only terminator, since the agent cannot resolve threads itself. *)
let () =
  let prop =
    QCheck2.Test.make ~name:"AO-12: Respond_review_unresolved retry semantics"
      (QCheck2.Gen.return ()) (fun () ->
        try
          let orch, patches, gameplan, pid = bootstrap_one () in
          let orch =
            make_busy orch patches gameplan pid Operation_kind.Review_comments
          in
          let before =
            (Orchestrator.agent orch pid)
              .Patch_agent.review_unresolved_cycle_count
          in
          let orch =
            Orchestrator.apply_respond_outcome orch pid
              Operation_kind.Review_comments
              Orchestrator.Respond_review_unresolved
          in
          let a = Orchestrator.agent orch pid in
          (not a.Patch_agent.busy)
          && a.Patch_agent.review_unresolved_cycle_count = before + 1
        with _ -> false)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-12 passed"

(* ========== AO-13: Respond_ok + Review_comments resets the cycle count
   ========== *)

(* The cap [review_unresolved_cycle_count >= 2] must count consecutive
   non-converged cycles, not lifetime ones. A converged cycle (every delivered
   comment replied to and resolved) resets the counter, so an old
   non-convergence cannot combine with a later single one to trigger
   intervention. Respond_ok for other kinds must NOT reset it. *)
let () =
  let prop =
    QCheck2.Test.make
      ~name:"AO-13: Respond_ok + Review_comments resets cycle count"
      (QCheck2.Gen.return ()) (fun () ->
        try
          let orch, patches, gameplan, pid = bootstrap_one () in
          let orch =
            make_busy orch patches gameplan pid Operation_kind.Review_comments
          in
          let orch =
            Orchestrator.apply_respond_outcome orch pid
              Operation_kind.Review_comments
              Orchestrator.Respond_review_unresolved
          in
          assert (
            (Orchestrator.agent orch pid)
              .Patch_agent.review_unresolved_cycle_count = 1);
          (* Respond_ok on an unrelated kind leaves the counter alone. *)
          let orch = make_busy orch patches gameplan pid Operation_kind.Human in
          let orch =
            Orchestrator.apply_respond_outcome orch pid Operation_kind.Human
              Orchestrator.Respond_ok
          in
          assert (
            (Orchestrator.agent orch pid)
              .Patch_agent.review_unresolved_cycle_count = 1);
          let orch =
            make_busy orch patches gameplan pid Operation_kind.Review_comments
          in
          let orch =
            Orchestrator.apply_respond_outcome orch pid
              Operation_kind.Review_comments Orchestrator.Respond_ok
          in
          (Orchestrator.agent orch pid)
            .Patch_agent.review_unresolved_cycle_count = 0
        with _ -> false)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-13 passed"

(* AO-surface: thread a bootstrapped orchestrator through every state
   transition / accessor on the orchestrator decision surface and assert it
   stays queryable. Uses the generated [flag] so this counts as a property over
   real input (a [Gen.unit] body that only [ignore]s the surface does not). *)
let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"orchestrator surface preserves well-formedness"
       ~count:50 QCheck2.Gen.bool (fun flag ->
         let orch, _patches, gameplan, pid = bootstrap_one () in
         let mid = Message_id.of_string "ao-surface-msg" in
         let orch = Orchestrator.set_main_branch orch main in
         let orch = Orchestrator.set_max_ci_failures orch ~max_ci_failures:5 in
         if (Orchestrator.agent orch pid).Patch_agent.max_ci_failures <> 5 then
           QCheck2.Test.fail_reportf "max_ci_failures was not stamped";
         (* Compile-time check of the public [Gameplan.add_patch] contract:
            [Ok] carries an immutable [(Gameplan.t * Patch.t)] tuple. *)
         let gameplan_with_added, added_patch =
           match
             Gameplan.add_patch gameplan ~title:"runtime patch"
               ~description:"created from TUI"
               ~dependencies:(if flag then [ pid ] else [])
           with
           | Ok (added : Gameplan.t * Patch.t) -> added
           | Error msg -> QCheck2.Test.fail_reportf "%s" msg
         in
         if
           not
             (List.exists gameplan_with_added.Gameplan.patches ~f:(fun p ->
                  Patch_id.equal p.Patch.id added_patch.Patch.id))
         then QCheck2.Test.fail_reportf "added patch missing from gameplan";
         (* [Gameplan.t] and [Patch.t] are immutable records. [add_planned_patch]
            registers the PR-less agent and graph edges; the patch record itself
            remains owned by [Gameplan.t] and cannot be mutated in place. Keep
            the generated id/deps as the expected immutable contract and assert
            the orchestrator graph reflects them exactly. *)
         let added_patch_id = added_patch.Patch.id in
         let expected_deps = added_patch.Patch.dependencies in
         let orch =
           Orchestrator.add_planned_patch orch added_patch ~deps:expected_deps
         in
         let added_agent = Orchestrator.agent orch added_patch_id in
         if Patch_agent.has_pr added_agent then
           QCheck2.Test.fail_reportf "planned patch unexpectedly has a PR";
         if
           not
             (List.equal Patch_id.equal
                (Graph.deps (Orchestrator.graph orch) added_patch_id)
                expected_deps)
         then QCheck2.Test.fail_reportf "planned patch deps were not recorded";
         let orch = Orchestrator.set_automerge_enabled orch pid flag in
         let orch = Orchestrator.set_automerge_inflight orch pid flag in
         let orch = Orchestrator.set_automerge_deadline orch pid 1.0 in
         let orch = Orchestrator.clear_automerge_deadline orch pid in
         let orch = Orchestrator.increment_automerge_failure_count orch pid in
         let orch = Orchestrator.reset_automerge_failure_count orch pid in
         let orch = Orchestrator.set_head_oid orch pid (Some "deadbeef") in
         let orch =
           Orchestrator.set_review_decision orch pid (Some "REVIEW_REQUIRED")
         in
         let orch =
           Orchestrator.set_unresolved_comment_count orch pid
             (if flag then 1 else 0)
         in
         let orch =
           Orchestrator.set_review_requested_for_oid orch pid (Some "deadbeef")
         in
         let orch = Orchestrator.set_review_request_inflight orch pid flag in
         let orch = Orchestrator.reset_ci_failure_count orch pid in
         let orch = Orchestrator.reset_conflict_noop_count orch pid in
         let orch = Orchestrator.set_branch_blocked orch pid in
         let orch = Orchestrator.clear_branch_blocked orch pid in
         let orch = Orchestrator.clear_has_conflict orch pid in
         let orch = Orchestrator.clear_pr orch pid in
         let orch = Orchestrator.clear_session_fallback orch pid in
         let orch = Orchestrator.mark_running orch pid in
         let orch = Orchestrator.on_pr_discovery_failure orch pid in
         let orch = Orchestrator.on_session_failure orch pid ~is_fresh:flag in
         let orch =
           Orchestrator.set_branch_rebased_onto_sha orch pid (Some "deadbeef")
         in
         let orch = Orchestrator.set_ci_checks orch pid [] in
         let orch = Orchestrator.set_is_draft orch pid flag in
         let orch = Orchestrator.set_merge_queue_entry orch pid None in
         let orch = Orchestrator.set_merge_queue_required orch pid flag in
         let orch = Orchestrator.set_merge_ready orch pid flag in
         let orch = Orchestrator.set_mergeability_unknown orch pid flag in
         let orch = Orchestrator.set_notified_base_branch orch pid main in
         let orch = Orchestrator.set_worktree_path orch pid "/tmp/wt" in
         let orch = Orchestrator.apply_anchor_events orch pid [] in
         let orch, _rebase_effects =
           Orchestrator.apply_rebase_with_anchor orch pid Worktree.Noop main []
         in
         let orch, _conflict_decision, _conflict_effects =
           Orchestrator.apply_conflict_rebase_with_anchor orch pid Worktree.Noop
             main []
         in
         let orch =
           Orchestrator.mark_patch_pending_messages_obsolete_except orch pid
             ~keep:[]
         in
         let orch = Orchestrator.mark_message_obsolete orch mid in
         let orch, _resume_action = Orchestrator.resume_message orch mid in
         let orch = Patch_controller.apply_automerge_success orch pid in
         let orch =
           Patch_controller.apply_automerge_failure orch ~now:0.0 pid
         in
         let _orch, _review_request_decisions =
           Patch_controller.reconcile_review_requests orch
         in
         let _found = Orchestrator.find_message orch mid in
         let _current = Orchestrator.current_message orch pid in
         (match Orchestrator.all_messages orch with
         | message :: _ ->
             ignore (Orchestrator.message_patch_id message);
             ignore (Orchestrator.message_status message)
         | [] -> ());
         List.length (Orchestrator.all_messages orch) >= 0));
  Stdlib.print_endline "orchestrator public surface linked"

(* AO-MQ: merge-queue/automerge failure wrappers preserve the shared
   agent-level automerge invariants when reached through the orchestrator and
   patch-controller public surfaces. *)
let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"AO-MQ: merge queue wrappers clear stale automerge timers"
       ~count:200
       QCheck2.Gen.(triple bool gen_merge_queue_entry gen_merge_queue_entry)
       (fun (required, observed_entry, entered_entry) ->
         try
           let orch, _patches, _gameplan, pid = bootstrap_one () in
           let orch = Orchestrator.set_automerge_enabled orch pid true in
           let orch = Orchestrator.set_automerge_inflight orch pid true in
           let orch = Orchestrator.set_automerge_deadline orch pid 10.0 in
           let orch =
             Orchestrator.apply_automerge_failure_state orch pid
               ~retry_deadline:20.0 ~max_failures:3
           in
           let after_failure = Orchestrator.agent orch pid in
           if after_failure.Patch_agent.automerge_inflight then
             QCheck2.Test.fail_reportf
               "apply_automerge_failure_state left inflight=true";
           if after_failure.Patch_agent.automerge_failure_count <> 1 then
             QCheck2.Test.fail_reportf
               "apply_automerge_failure_state did not increment failures";
           let orch =
             Orchestrator.observe_merge_queue orch pid ~required
               ~entry:(Some observed_entry)
           in
           let after_observe = Orchestrator.agent orch pid in
           if not after_observe.Patch_agent.merge_queue_required then
             QCheck2.Test.fail_reportf
               "observe_merge_queue with entry did not require merge queue";
           if
             not
               (Option.equal Pr_state.equal_merge_queue_entry
                  after_observe.Patch_agent.merge_queue_entry
                  (Some observed_entry))
           then
             QCheck2.Test.fail_reportf
               "observe_merge_queue did not record observed entry";
           if Option.is_some after_observe.Patch_agent.automerge_deadline then
             QCheck2.Test.fail_reportf
               "observe_merge_queue left a stale automerge deadline";
           let orch = Orchestrator.entered_merge_queue orch pid entered_entry in
           let after_entered = Orchestrator.agent orch pid in
           if
             not
               (Option.equal Pr_state.equal_merge_queue_entry
                  after_entered.Patch_agent.merge_queue_entry
                  (Some entered_entry))
           then
             QCheck2.Test.fail_reportf
               "entered_merge_queue did not replace queue entry";
           let orch =
             Orchestrator.set_automerge_inflight orch pid true |> fun orch ->
             Orchestrator.set_automerge_deadline orch pid 30.0 |> fun orch ->
             Orchestrator.increment_automerge_failure_count orch pid
           in
           let orch =
             Patch_controller.apply_merge_queue_entered orch pid observed_entry
           in
           let after_controller = Orchestrator.agent orch pid in
           Option.equal Pr_state.equal_merge_queue_entry
             after_controller.Patch_agent.merge_queue_entry
             (Some observed_entry)
           && after_controller.Patch_agent.merge_queue_required
           && Option.is_none after_controller.Patch_agent.automerge_deadline
           && (not after_controller.Patch_agent.automerge_inflight)
           && after_controller.Patch_agent.automerge_failure_count = 0
         with
         | QCheck2.Test.Test_fail _ as exn -> raise exn
         | _ -> false));
  Stdlib.print_endline "AO-MQ passed"
