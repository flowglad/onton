open Base
open Onton
open Onton.Types

(** Property tests for [Orchestrator.apply_start_outcome] and
    [Orchestrator.apply_respond_outcome]. These encode the runner's contract:
    every non-stale action fiber must call [complete] before exiting. *)

let main = Branch.of_string "main"
let mk_patches = Onton_test_support.Test_generators.mk_linear_patches
let make_gameplan = Onton_test_support.Test_generators.make_test_gameplan
let pid_of_idx = Onton_test_support.Test_generators.pid_of_idx

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

(* ========== AO-4: Respond_ok + Pr_body sets delivered ========== *)

let () =
  let prop =
    QCheck2.Test.make ~name:"AO-4: Respond_ok + Pr_body sets pr_body_delivered"
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
  Stdlib.print_endline "AO-4 passed"

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
