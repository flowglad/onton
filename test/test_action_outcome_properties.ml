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

let publish_fixture t pid candidate =
  Onton_core_test_support.Publication_fixture.confirmed ~candidate
    ~step:(fun t event -> fst (Orchestrator.reconcile_branch t pid event))
    ~state:(fun t -> (Orchestrator.agent t pid).Patch_agent.branch_reconcile)
    t

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

let merge_queue_entry ?(state = Pr_state.Mq_queued) id =
  Pr_state.{ id; state; position = 0 }

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

let accept_only_message orch gameplan =
  let orch, _effects, messages =
    Patch_controller.plan_tick_messages orch ~project_name:"test-project"
      ~gameplan
  in
  match messages with
  | [ msg ] ->
      let message_id = Orchestrator.message_id msg in
      let orch, action = Orchestrator.accept_message orch message_id in
      assert (Option.is_some action);
      (orch, message_id)
  | _ -> failwith "expected exactly one runnable message"

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

(* ========== AO-1c: accepted Human Start remains recoverable until successful
   post-PR completion ========== *)

let () =
  let patches = mk_patches 1 in
  let pid = pid_of_idx patches 0 in
  let start_human () =
    let orch = Orchestrator.create ~patches ~main_branch:main in
    let orch = Orchestrator.send_human_message orch pid "keep this guidance" in
    Orchestrator.fire orch (Orchestrator.Start (pid, main))
  in
  let failed = start_human () in
  let before_failure = Orchestrator.agent failed pid in
  assert (not (List.is_empty before_failure.Patch_agent.inflight_human_messages));
  (* Backend acceptance no longer clears Human Start guidance. A clean session
     that made no commit is still a failed Start and must restore it. *)
  let failed =
    Orchestrator.apply_session_result failed pid Orchestrator.Session_no_commits
  in
  let failed =
    Orchestrator.apply_start_outcome failed pid Orchestrator.Start_failed
  in
  let after_failure = Orchestrator.agent failed pid in
  assert (List.is_empty after_failure.Patch_agent.inflight_human_messages);
  assert (not (List.is_empty after_failure.Patch_agent.human_messages));
  assert (
    List.mem after_failure.Patch_agent.queue Operation_kind.Human
      ~equal:Operation_kind.equal);
  (* Even a healthy backend session is not a successful Start when PR
     association fails. The post-discovery transition restores guidance. *)
  let discovery_failed = start_human () in
  let discovery_failed =
    Orchestrator.apply_session_result discovery_failed pid
      Orchestrator.Session_ok
  in
  let discovery_failed =
    Orchestrator.complete_start_after_pr_discovery discovery_failed pid
  in
  let after_discovery_failure = Orchestrator.agent discovery_failed pid in
  assert (not (List.is_empty after_discovery_failure.Patch_agent.human_messages));
  (* PR association followed by explicit completion is the only Start success
     path that consumes the retained guidance. *)
  let succeeded = start_human () in
  let succeeded =
    Orchestrator.apply_session_result succeeded pid Orchestrator.Session_ok
  in
  let succeeded =
    Orchestrator.set_pr_number succeeded pid (Pr_number.of_int 1)
  in
  let succeeded =
    Orchestrator.complete_start_after_pr_discovery succeeded pid
  in
  let after_success = Orchestrator.agent succeeded pid in
  assert (List.is_empty after_success.Patch_agent.human_messages);
  assert (List.is_empty after_success.Patch_agent.inflight_human_messages);
  assert (not after_success.Patch_agent.busy);
  Stdlib.print_endline "AO-1c passed"

(* ========== AO-1d: a stale daemon cannot force-complete a newer Start ========== *)

let () =
  let patches = mk_patches 1 in
  let gameplan = make_gameplan patches in
  let pid = pid_of_idx patches 0 in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let orch = Orchestrator.send_human_message orch pid "keep this guidance" in
  let orch, old_message_id = accept_only_message orch gameplan in
  let orch =
    Orchestrator.apply_force_complete ~message_id:old_message_id orch pid
      Orchestrator.Cancelled
  in
  let orch, new_message_id = accept_only_message orch gameplan in
  let before = Orchestrator.agent orch pid in
  assert before.Patch_agent.busy;
  assert (
    Option.equal Message_id.equal before.Patch_agent.current_message_id
      (Some new_message_id));
  let orch =
    Orchestrator.apply_force_complete ~message_id:old_message_id orch pid
      Orchestrator.Cancelled
  in
  let after = Orchestrator.agent orch pid in
  assert (Patch_agent.equal before after);
  assert after.Patch_agent.busy;
  assert (
    Option.equal Message_id.equal after.Patch_agent.current_message_id
      (Some new_message_id));
  Stdlib.print_endline "AO-1d passed"

(* ========== AO-2: Non-stale respond outcomes produce busy=false ========== *)

let () =
  let respond_outcomes =
    [
      Orchestrator.Respond_ok;
      Orchestrator.Respond_failed;
      Orchestrator.Respond_retry_push;
      Orchestrator.Respond_no_commits;
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
      Operation_kind.Uncommitted_changes;
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
              | Patch_decision.Uncommitted_changes_payload
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

(* ========== AO-3: successful response preserves unresolved owner conflict ========== *)

let () =
  let prop =
    QCheck2.Test.make
      ~name:"AO-3: successful response preserves unresolved owner conflict"
      (QCheck2.Gen.return ()) (fun () ->
        let orch, _, _, pid = bootstrap_one () in
        let orch = Onton_test_support.Conflict_fixture.conflict orch pid in
        let orch =
          Orchestrator.fire
            (Orchestrator.enqueue orch pid Operation_kind.Merge_conflict)
            (Orchestrator.Respond (pid, Operation_kind.Merge_conflict))
        in
        let before = Orchestrator.agent orch pid in
        assert (Patch_agent.has_conflict before);
        let orch =
          Orchestrator.apply_respond_outcome orch pid
            Operation_kind.Merge_conflict Orchestrator.Respond_ok
        in
        let after = Orchestrator.agent orch pid in
        Patch_agent.has_conflict after
        && Branch_reconcile.equal before.Patch_agent.branch_reconcile
             after.Patch_agent.branch_reconcile)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-3 passed"

(* A current but unconfirmed forge pair revokes queued work and invalidates
   previously planned messages without changing the owner's repair evidence. *)
let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"unconfirmed forge pair invalidates queued dispatch context"
       ~count:100 QCheck2.Gen.bool (fun local_conflict ->
         try
           let orch, _, _, pid = bootstrap_one () in
           let orch =
             if local_conflict then
               Onton_test_support.Conflict_fixture.conflict orch pid
             else orch
           in
           let orch = Orchestrator.set_merge_ready orch pid true in
           let orch = Orchestrator.set_checks_passing orch pid true in
           let orch =
             List.fold
               [
                 Operation_kind.Ci;
                 Operation_kind.Merge_conflict;
                 Operation_kind.Human;
               ] ~init:orch ~f:(fun orch kind ->
                 Orchestrator.enqueue orch pid kind)
           in
           let before = Orchestrator.agent orch pid in
           let orch = Orchestrator.defer_forge_revision_pair orch pid in
           let after = Orchestrator.agent orch pid in
           (not after.Patch_agent.merge_ready)
           && (not after.Patch_agent.checks_passing)
           && after.Patch_agent.mergeability_unknown
           && List.equal Operation_kind.equal after.Patch_agent.queue
                [ Operation_kind.Human ]
           && (not (Poll_cycle.same_context ~requested:before ~current:after))
           && Branch_reconcile.equal before.Patch_agent.branch_reconcile
                after.Patch_agent.branch_reconcile
           && Patch_agent.equal after
                (Orchestrator.agent
                   (Orchestrator.defer_forge_revision_pair orch pid)
                   pid)
         with _ -> false));
  Stdlib.print_endline "forge pair dispatch revocation passed"

(* ========== AO-5: Stale outcomes are identity ========== *)

let () =
  let prop =
    QCheck2.Test.make ~name:"AO-5: stale outcomes are identity"
      (QCheck2.Gen.oneof_list
         Operation_kind.
           [
             Uncommitted_changes;
             Ci;
             Review_comments;
             Human;
             Merge_conflict;
             Pr_body;
           ])
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

(* ========== AO-5b: completed cleanup responses retry the rebase ========== *)

let () =
  let completion_outcomes =
    [
      Orchestrator.Respond_ok;
      Orchestrator.Respond_failed;
      Orchestrator.Respond_retry_push;
      Orchestrator.Respond_no_commits;
      Orchestrator.Respond_skip_empty;
    ]
  in
  let prop =
    QCheck2.Test.make
      ~name:"AO-5b: completed Uncommitted_changes response enqueues Rebase"
      (QCheck2.Gen.oneof_list completion_outcomes) (fun outcome ->
        try
          let orch, patches, gameplan, pid = bootstrap_one () in
          let orch =
            make_busy orch patches gameplan pid
              Operation_kind.Uncommitted_changes
          in
          let orch =
            Orchestrator.apply_respond_outcome orch pid
              Operation_kind.Uncommitted_changes outcome
          in
          let agent = Orchestrator.agent orch pid in
          (not agent.Patch_agent.busy)
          && List.mem agent.Patch_agent.queue Operation_kind.Rebase
               ~equal:Operation_kind.equal
        with _ -> false)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-5b passed"

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
   No_commits / Stale do NOT. This locks in the invariant that a CI fix attempt only
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
    Orchestrator.apply_session_result orch pid Orchestrator.Session_no_commits
  in
  assert ((Orchestrator.agent orch pid).Patch_agent.no_commits_push_count = 1);
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Ci
      Orchestrator.Respond_ok
  in
  assert (ci_count orch pid = before + 1);
  assert ((Orchestrator.agent orch pid).Patch_agent.no_commits_push_count = 0);
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
  (* Respond_no_commits does NOT bump *)
  let orch, pid, before = with_ci_busy () in
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Ci
      Orchestrator.Respond_no_commits
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

(* ========== AO-7b: failed CI push remains deliverable ========== *)

let () =
  let run_id = 4242 in
  let check =
    Ci_check.
      {
        name = "test";
        conclusion = "failure";
        details_url = None;
        description = None;
        started_at = None;
        app_id = None;
        check_suite_id = None;
        id = Some run_id;
      }
  in
  let orch, patches, gameplan, pid = bootstrap_one () in
  let orch = Orchestrator.set_ci_checks orch pid [ check ] in
  let orch = make_busy orch patches gameplan pid Operation_kind.Ci in
  let orch =
    Orchestrator.apply_respond_outcome orch pid Operation_kind.Ci
      Orchestrator.Respond_retry_push
  in
  let agent = Orchestrator.agent orch pid in
  assert (List.is_empty agent.Patch_agent.delivered_ci_run_ids);
  assert (
    Patch_decision.equal_ci_decision
      (Patch_decision.on_ci_failure agent)
      Patch_decision.Enqueue_ci);
  let agent = Patch_agent.enqueue agent Operation_kind.Ci in
  let agent = Patch_agent.respond agent Operation_kind.Ci in
  let redelivered =
    match
      Patch_decision.respond_delivery ~agent ~kind:Operation_kind.Ci
        ~pre_fire_agent:None ~prefetched_comments:[] ~prefetched_findings:[]
        ~main_branch:"main"
    with
    | Patch_decision.Deliver { payload; _ } -> (
        match payload with
        | Patch_decision.Ci_payload { failed_checks } ->
            List.exists failed_checks ~f:(fun (check : Ci_check.t) ->
                Option.equal Int.equal check.Ci_check.id (Some run_id))
        | Patch_decision.Uncommitted_changes_payload
        | Patch_decision.Human_payload _ | Patch_decision.Review_payload _
        | Patch_decision.Findings_payload _ | Patch_decision.Pr_body_payload
        | Patch_decision.Merge_conflict_payload ->
            false)
    | Patch_decision.Skip_empty | Patch_decision.Respond_stale -> false
  in
  assert redelivered;
  Stdlib.print_endline "AO-7b passed"

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
      Orchestrator.Respond_no_commits;
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

(* ========== AO-14: no-commit Pr_body cycle with delivered artifact never
   marches toward needs_intervention ========== *)

(* Encodes the runner contract restored after the v0.51.0 regression
   (938efeb7): a healthy Pr_body session authors the notes artifact outside
   the worktree and makes NO commits, so the session driver reports
   Session_no_commits (bumping no_commits_push_count) while the runner's
   artifact verdict maps the respond outcome to Respond_ok — which must reset
   the counter, flip pr_body_delivered, and leave the agent idle without
   intervention. The regression skipped the artifact apply for no-commit
   sessions, so the counter incremented without the Respond_ok reset and every
   patch escalated at >= 2 right after its first commit. The miss leg checks
   the other verdict: Respond_pr_body_miss after a no-commit session leaves
   both counters at 1 — retry-once territory, no intervention yet. *)
let () =
  let prop =
    QCheck2.Test.make
      ~name:
        "AO-14: Session_no_commits + Respond_ok Pr_body cycle resets the noop \
         counter" (QCheck2.Gen.return ()) (fun () ->
        try
          let orch, patches, gameplan, pid = bootstrap_one () in
          (* Miss leg: no-commit session, artifact NOT delivered. *)
          let orch =
            make_busy orch patches gameplan pid Operation_kind.Pr_body
          in
          let orch =
            Orchestrator.apply_session_result orch pid
              Orchestrator.Session_no_commits
          in
          assert (Orchestrator.agent orch pid).Patch_agent.busy;
          assert (
            (Orchestrator.agent orch pid).Patch_agent.no_commits_push_count = 1);
          let orch =
            Orchestrator.apply_respond_outcome orch pid Operation_kind.Pr_body
              Orchestrator.Respond_pr_body_miss
          in
          let a_miss = Orchestrator.agent orch pid in
          assert (not a_miss.Patch_agent.busy);
          assert (a_miss.Patch_agent.no_commits_push_count = 1);
          assert (a_miss.Patch_agent.pr_body_artifact_miss_count = 1);
          assert (not (Patch_agent.needs_intervention a_miss));
          (* Delivery leg: second no-commit session, artifact delivered. The
             counter reaches 2 transiently between apply_session_result and
             apply_respond_outcome, but Respond_ok resets it before the agent
             goes idle — the cycle must end clean. *)
          let orch =
            make_busy orch patches gameplan pid Operation_kind.Pr_body
          in
          let orch =
            Orchestrator.apply_session_result orch pid
              Orchestrator.Session_no_commits
          in
          let orch =
            Orchestrator.apply_respond_outcome orch pid Operation_kind.Pr_body
              Orchestrator.Respond_ok
          in
          let a = Orchestrator.agent orch pid in
          (not a.Patch_agent.busy)
          && a.Patch_agent.no_commits_push_count = 0
          && a.Patch_agent.pr_body_delivered
          && a.Patch_agent.pr_body_artifact_miss_count = 0
          && not (Patch_agent.needs_intervention a)
        with _ -> false)
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "AO-14 passed"

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
         let candidate =
           Onton_core_test_support.Publication_fixture.sha "published"
         in
         let orch = if flag then publish_fixture orch pid candidate else orch in
         if
           not
             (Option.equal String.equal
                (Patch_agent.expected_remote_head_oid
                   (Orchestrator.agent orch pid))
                (if flag then Some candidate else None))
         then QCheck2.Test.fail_reportf "owner publication projection differs";
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
         let orch = Orchestrator.set_branch_blocked orch pid in
         let orch = Orchestrator.clear_branch_blocked orch pid in
         let orch = Onton_test_support.Conflict_fixture.resolve orch pid in
         let orch = Orchestrator.complete_start_after_pr_discovery orch pid in
         let orch = Orchestrator.clear_pr orch pid in
         let orch = Orchestrator.clear_session_fallback orch pid in
         let orch = Orchestrator.mark_running orch pid in
         let orch = Orchestrator.on_pr_discovery_failure orch pid in
         let orch = Orchestrator.on_session_failure orch pid ~is_fresh:flag in
         let orch = Orchestrator.set_ci_checks orch pid [] in
         let orch = Orchestrator.set_is_draft orch pid flag in
         let orch = Orchestrator.set_merge_queue_entry orch pid None in
         let orch = Orchestrator.set_merge_queue_required orch pid flag in
         let orch = Orchestrator.set_merge_ready orch pid flag in
         let orch = Orchestrator.set_mergeability_unknown orch pid flag in
         let orch = Orchestrator.set_notified_base_branch orch pid main in
         let orch = Orchestrator.set_worktree_path orch pid "/tmp/wt" in
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
           let dequeue_now = 100.0 in
           let orch =
             Orchestrator.set_automerge_inflight orch pid true |> fun orch ->
             Orchestrator.set_automerge_deadline orch pid 40.0 |> fun orch ->
             Orchestrator.increment_automerge_failure_count orch pid
           in
           let orch =
             Patch_controller.apply_merge_queue_dequeued orch ~now:dequeue_now
               pid
           in
           let after_dequeued = Orchestrator.agent orch pid in
           let stuck_entry =
             merge_queue_entry ~state:Pr_state.Mq_unmergeable "stuck-entry"
           in
           let stuck_agent =
             Patch_agent.set_merge_queue_entry after_dequeued (Some stuck_entry)
           in
           Option.equal Pr_state.equal_merge_queue_entry
             after_controller.Patch_agent.merge_queue_entry
             (Some observed_entry)
           && after_controller.Patch_agent.merge_queue_required
           && Option.is_none after_controller.Patch_agent.automerge_deadline
           && (not after_controller.Patch_agent.automerge_inflight)
           && after_controller.Patch_agent.automerge_failure_count = 0
           && Option.is_none after_dequeued.Patch_agent.merge_queue_entry
           && after_dequeued.Patch_agent.merge_queue_required
           && (not after_dequeued.Patch_agent.automerge_inflight)
           && after_dequeued.Patch_agent.automerge_failure_count = 0
           && (match after_dequeued.Patch_agent.automerge_deadline with
             | Some deadline ->
                 Float.( = ) deadline
                   (dequeue_now +. Patch_controller.default_automerge_timeout)
             | None -> false)
           && Patch_controller.should_dequeue_merge_queue stuck_agent
                ~main_branch:main ~entry_id:stuck_entry.Pr_state.id
           && List.mem
                (Patch_controller.dequeue_merge_queue_reasons stuck_agent
                   ~main_branch:main ~entry_id:stuck_entry.Pr_state.id)
                "GitHub queue entry UNMERGEABLE (position 0)"
                ~equal:String.equal
           && List.is_empty
                (Patch_controller.dequeue_merge_queue_reasons stuck_agent
                   ~main_branch:main ~entry_id:"different-entry")
           && not
                (Patch_controller.should_dequeue_merge_queue stuck_agent
                   ~main_branch:main ~entry_id:"different-entry")
         with
         | QCheck2.Test.Test_fail _ as exn -> raise exn
         | _ -> false));
  Stdlib.print_endline "AO-MQ passed"

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"feature mode publication and promotion decisions"
       ~count:100
       QCheck2.Gen.(pair bool string)
       (fun (claim, head_sha) ->
         let head_sha =
           Onton_core_test_support.Publication_fixture.sha head_sha
         in
         try
           let patches = mk_patches 2 in
           let graph = Graph.of_patches patches in
           let root = pid_of_idx patches 0 in
           let child = pid_of_idx patches 1 in
           let mode =
             match Execution_mode.infer graph with
             | Ok mode -> mode
             | Error error -> failwith error
           in
           let orch =
             Orchestrator.create ~patches ~main_branch:main |> fun orch ->
             Orchestrator.set_execution_mode orch mode
           in
           let root_branch =
             (Orchestrator.agent orch root).Patch_agent.branch
           in
           assert (Execution_mode.equal (Orchestrator.execution_mode orch) mode);
           assert (Orchestrator.is_integration_root orch root);
           assert (Orchestrator.is_feature_descendant orch child);
           assert (
             Branch.equal (Orchestrator.terminal_branch orch child) root_branch);
           assert (List.is_empty (Orchestrator.open_deps orch child));
           assert (
             Option.equal Branch.equal
               (Orchestrator.expected_base orch child)
               (Some root_branch));
           assert (Orchestrator.construction_open orch);
           assert (Orchestrator.additions_allowed orch ~dependencies:[ root ]);
           let claimed = Orchestrator.claim_promotion orch in
           assert (not (Orchestrator.construction_open claimed));
           let orch =
             if claim then claimed else Orchestrator.release_promotion claimed
           in
           assert (Bool.equal (Orchestrator.construction_open orch) (not claim));
           let orch = Orchestrator.release_promotion orch in
           let orch = Orchestrator.settle_restored_promotion orch in
           assert (Orchestrator.construction_open orch);
           let orch = publish_fixture orch child head_sha in
           assert (Orchestrator.branch_only_published orch child);
           let orch = Orchestrator.refresh_base_branch orch child in
           let orch =
             Patch_controller.apply_branch_observation orch child ~head_sha
               ~checks:[]
           in
           assert (
             Option.equal String.equal
               (Orchestrator.agent orch child).Patch_agent.head_oid
               (Some head_sha));
           assert (not (Patch_controller.is_integration_candidate orch child));
           assert (not (Patch_controller.ready_for_review orch child));
           let orch = Orchestrator.invalidate_root_readiness orch in
           assert
             (Orchestrator.agent orch root).Patch_agent.mergeability_unknown;
           match
             Branch_poll_decision.plan ~now:0. ~expected_head:(Some head_sha)
               ~checks:[] None
           with
           | Branch_poll_decision.Probe { reuse_checks = false } -> true
           | Branch_poll_decision.Probe { reuse_checks = true }
           | Branch_poll_decision.Skip ->
               false
         with _ -> false));
  Stdlib.print_endline "AO-feature-mode passed"

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:
         "publication merge requirements block Start until publication merges"
       ~count:300
       QCheck2.Gen.(list (triple bool bool bool))
       (fun steps ->
         try
           let patches = mk_patches 2 in
           let child = pid_of_idx patches 0 in
           let publication_id = Gameplan.publication_patch_id in
           let publication =
             match
               Gameplan_publication.create ~directory:"gameplans"
                 ~project_name:"test-project" ~yaml:true ~content:"source"
             with
             | Ok publication -> publication
             | Error error -> failwith error
           in
           let gameplan =
             match Gameplan.publish (make_gameplan patches) publication with
             | Ok gameplan -> gameplan
             | Error error -> failwith error
           in
           let orch =
             Orchestrator.create ~patches:gameplan.Gameplan.patches
               ~main_branch:main
             |> fun orch ->
             Orchestrator.apply_gameplan_merge_requirements orch gameplan
           in
           let rec check orch merged = function
             | [] -> true
             | (apply_plan, observe_pr, merge) :: rest ->
                 let orch =
                   if apply_plan then
                     Orchestrator.apply_gameplan_merge_requirements orch
                       gameplan
                   else
                     Orchestrator.require_dependency_merge orch child
                       ~dep:publication_id
                 in
                 let orch =
                   if observe_pr then
                     Orchestrator.set_pr_number orch publication_id
                       (Pr_number.of_int 1)
                   else orch
                 in
                 let orch =
                   if merge then Orchestrator.mark_merged orch publication_id
                   else orch
                 in
                 let merged = merged || merge in
                 let attempted =
                   Orchestrator.fire orch (Orchestrator.Start (child, main))
                 in
                 Bool.equal
                   (Orchestrator.agent attempted child).Patch_agent.busy merged
                 && List.equal Patch_id.equal
                      (Graph.merge_required_deps (Orchestrator.graph orch) child)
                      [ publication_id ]
                 && check orch merged rest
           in
           (not
              (Orchestrator.agent
                 (Orchestrator.fire orch (Orchestrator.Start (child, main)))
                 child)
                .Patch_agent.busy)
           && check orch false steps
         with _ -> false));
  Stdlib.print_endline "AO-publication-merge-requirements passed"

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"feature publication acknowledgement preserves later integrations"
       ~count:200 QCheck2.Gen.bool (fun merge_during_publication ->
         try
           let patches = mk_patches 3 in
           let root = pid_of_idx patches 0 in
           let first = pid_of_idx patches 1 in
           let second = pid_of_idx patches 2 in
           match Execution_mode.infer (Graph.of_patches patches) with
           | Error _ -> false
           | Ok mode ->
               let orch =
                 Orchestrator.create ~patches ~main_branch:main |> fun o ->
                 Orchestrator.set_execution_mode o mode |> fun o ->
                 Orchestrator.set_pr_number o root (Pr_number.of_int 1)
                 |> fun o -> Orchestrator.mark_merged o first
               in
               let publication = Orchestrator.agent orch root in
               let orch =
                 if merge_during_publication then
                   Orchestrator.mark_merged orch second
                 else orch
               in
               let orch =
                 Orchestrator.acknowledge_pr_body_refresh orch root ~publication
                 |> fun o -> Orchestrator.set_pr_body_delivered o root true
               in
               Bool.equal
                 (Orchestrator.agent orch root).Patch_agent.pr_body_refresh
                   .Patch_agent.pending merge_during_publication
         with _ -> false))

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:200
       ~name:"publication observation preserves running patch-message authority"
       QCheck2.Gen.(option string)
       (fun observed_head ->
         try
           let orch, patches, gameplan, pid = bootstrap_one () in
           let candidate =
             Onton_core_test_support.Publication_fixture.sha "published"
           in
           let orch = publish_fixture orch pid candidate in
           let orch =
             make_busy orch patches gameplan pid Operation_kind.Human
           in
           let observed_head =
             Option.map observed_head
               ~f:Onton_core_test_support.Publication_fixture.sha
           in
           let before = Orchestrator.agent orch pid in
           let observed =
             Orchestrator.observe_publication_head
               ?confirmed_remote_head:observed_head orch pid observed_head
           in
           let after = Orchestrator.agent observed pid in
           before.Patch_agent.busy && after.Patch_agent.busy
           && Int.equal before.Patch_agent.generation
                after.Patch_agent.generation
           && Option.equal Message_id.equal
                before.Patch_agent.current_message_id
                after.Patch_agent.current_message_id
           && Option.equal Operation_kind.equal before.Patch_agent.current_op
                after.Patch_agent.current_op
           && Option.equal String.equal before.Patch_agent.head_oid
                after.Patch_agent.head_oid
           && Option.equal Branch_reconcile.equal_publication_identity
                (Branch_reconcile.publication_observation_target
                   before.Patch_agent.branch_reconcile)
                (Branch_reconcile.publication_observation_target
                   after.Patch_agent.branch_reconcile)
           && Bool.equal
                (Option.is_none (Patch_agent.expected_remote_head_oid after))
                (Option.is_some observed_head)
         with _ -> false))

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:
         "confirmed branch publication preserves mode-specific PR scheduling"
       ~count:100 QCheck2.Gen.bool (fun feature ->
         try
           let patches = mk_patches 2 in
           let child = pid_of_idx patches 1 in
           let root = pid_of_idx patches 0 in
           let orch = Orchestrator.create ~patches ~main_branch:main in
           let orch =
             if feature then
               match Execution_mode.infer (Graph.of_patches patches) with
               | Ok mode -> Orchestrator.set_execution_mode orch mode
               | Error message -> failwith message
             else Orchestrator.mark_merged orch root
           in
           let candidate =
             Onton_core_test_support.Publication_fixture.sha "child"
           in
           let orch = publish_fixture orch child candidate in
           let orch = Orchestrator.set_pr_body_delivered orch child true in
           let orch = Orchestrator.enqueue orch child Operation_kind.Pr_body in
           let actions = Patch_controller.plan_actions orch ~patches in
           Bool.equal (Orchestrator.branch_only_published orch child) feature
           && List.exists actions ~f:(function
             | Orchestrator.Start (pid, _) ->
                 Patch_id.equal pid child && not feature
             | Orchestrator.Respond (pid, kind) ->
                 Patch_id.equal pid child && feature
                 && Operation_kind.equal kind Operation_kind.Pr_body
             | Orchestrator.Rebase _ | Orchestrator.Reconcile_branch _ -> false)
         with _ -> false))

let () =
  let kinds =
    [
      Operation_kind.Pr_body;
      Operation_kind.Rebase;
      Operation_kind.Findings;
      Operation_kind.Ci;
      Operation_kind.Review_comments;
      Operation_kind.Human;
      Operation_kind.Merge_conflict;
      Operation_kind.Uncommitted_changes;
    ]
  in
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"response PR routing follows execution mode"
       ~count:500
       QCheck2.Gen.(
         pair (triple bool bool bool)
           (triple bool (int_range 0 1) (oneof_list kinds)))
       (fun ((feature, root_present, child_present), (has_pr, index, kind)) ->
         try
           let patches = mk_patches 2 in
           let root = pid_of_idx patches 0 in
           let child = pid_of_idx patches 1 in
           let orch = Orchestrator.create ~patches ~main_branch:main in
           let orch =
             if feature then
               match Execution_mode.infer (Graph.of_patches patches) with
               | Ok mode -> Orchestrator.set_execution_mode orch mode
               | Error error -> failwith error
             else orch
           in
           let orch =
             if has_pr then
               Orchestrator.set_pr_number orch root (Pr_number.of_int 11)
               |> fun o ->
               Orchestrator.set_pr_number o child (Pr_number.of_int 22)
             else orch
           in
           let orch =
             if root_present then orch else Orchestrator.remove_agent orch root
           in
           let orch =
             if child_present then orch
             else Orchestrator.remove_agent orch child
           in
           let route_to_root =
             index = 0
             || (feature && Operation_kind.equal kind Operation_kind.Pr_body)
           in
           let expected =
             if has_pr && if route_to_root then root_present else child_present
             then Some (Pr_number.of_int (if route_to_root then 11 else 22))
             else None
           in
           Option.equal Pr_number.equal expected
             (Orchestrator.respond_pr_number orch
                (if index = 0 then root else child)
                kind)
         with _ -> false))
