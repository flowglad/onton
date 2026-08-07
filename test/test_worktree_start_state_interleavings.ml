(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core
open Onton_core.Types

(** Worktree-start-state interleaving properties.

    Patch scheduling and worktree materialization are independent state axes:

    - [Unmaterialized] means the next no-PR Start will cut a new worktree, so
      dependency availability, review-readiness, and base freshness gate it.
    - [Materialized path] means the checkout already exists. A failed or timed-
      out no-PR session retries in that checkout, so subsequent dependency state
      changes cannot gate the retry.

    The properties below drive the real patch controller and orchestrator over
    arbitrary interleavings of dependency PR/CI/review/conflict/rebase/merge
    changes and child session lifecycle changes. *)

let main = Branch.of_string "main"
let patches () = Onton_test_support.Test_generators.mk_linear_patches 2

let parent_and_child patches =
  ( Onton_test_support.Test_generators.pid_of_idx patches 0,
    Onton_test_support.Test_generators.pid_of_idx patches 1 )

let bootstrap () =
  let patches = patches () in
  let parent, child = parent_and_child patches in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let orch = Orchestrator.fire orch (Orchestrator.Start (parent, main)) in
  let orch = Orchestrator.set_pr_number orch parent (Pr_number.of_int 1) in
  let orch = Orchestrator.complete orch parent in
  (patches, parent, child, orch)

let child_start_action orch ~patches ~child =
  Patch_controller.plan_actions orch ~patches
  |> List.find ~f:(function
    | Orchestrator.Start (patch_id, _) -> Patch_id.equal patch_id child
    | Orchestrator.Respond _ | Orchestrator.Rebase _ -> false)

let child_start_planned orch ~patches ~child =
  Option.is_some (child_start_action orch ~patches ~child)

type dependency_op =
  | Pr_present
  | Pr_absent
  | Checks_passing
  | Checks_failing
  | Notes_delivered
  | Notes_missing
  | Conflict_detected
  | Conflict_cleared
  | Rebase_enqueued
  | Rebase_started
  | Rebase_finished
  | Merged

let gen_dependency_op =
  QCheck2.Gen.oneof_list
    [
      Pr_present;
      Pr_absent;
      Checks_passing;
      Checks_failing;
      Notes_delivered;
      Notes_missing;
      Conflict_detected;
      Conflict_cleared;
      Rebase_enqueued;
      Rebase_started;
      Rebase_finished;
      Merged;
    ]

let gen_dependency_ops =
  QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 80) gen_dependency_op

let gen_materialized_path =
  QCheck2.Gen.map
    (fun suffix -> "/tmp/worktree-start-state-child-" ^ Int.to_string suffix)
    QCheck2.Gen.nat_small

let gen_path_and_dependency_ops =
  QCheck2.Gen.pair gen_materialized_path gen_dependency_ops

let apply_dependency_op orch parent = function
  | Pr_present ->
      let agent = Orchestrator.agent orch parent in
      if agent.Patch_agent.merged || Patch_agent.has_pr agent then orch
      else Orchestrator.set_pr_number orch parent (Pr_number.of_int 1)
  | Pr_absent ->
      let agent = Orchestrator.agent orch parent in
      if
        agent.Patch_agent.merged || agent.Patch_agent.busy
        || not (Patch_agent.is_pr_present agent)
      then orch
      else Orchestrator.clear_pr orch parent
  | Checks_passing -> Orchestrator.set_checks_passing orch parent true
  | Checks_failing -> Orchestrator.set_checks_passing orch parent false
  | Notes_delivered -> Orchestrator.set_pr_body_delivered orch parent true
  | Notes_missing -> Orchestrator.set_pr_body_delivered orch parent false
  | Conflict_detected -> Orchestrator.set_has_conflict orch parent
  | Conflict_cleared -> Orchestrator.clear_has_conflict orch parent
  | Rebase_enqueued ->
      let agent = Orchestrator.agent orch parent in
      if
        agent.Patch_agent.merged || agent.Patch_agent.busy
        || not (Patch_agent.is_pr_present agent)
      then orch
      else Orchestrator.enqueue orch parent Operation_kind.Rebase
  | Rebase_started ->
      let agent = Orchestrator.agent orch parent in
      if
        agent.Patch_agent.merged || agent.Patch_agent.busy
        || not (Patch_agent.is_pr_present agent)
      then orch
      else
        let orch = Orchestrator.enqueue orch parent Operation_kind.Rebase in
        let agent = Orchestrator.agent orch parent in
        if
          Option.equal Operation_kind.equal
            (Patch_agent.highest_priority agent)
            (Some Operation_kind.Rebase)
        then Orchestrator.fire orch (Orchestrator.Rebase (parent, main))
        else orch
  | Rebase_finished ->
      let agent = Orchestrator.agent orch parent in
      if
        agent.Patch_agent.busy
        && Option.equal Operation_kind.equal agent.Patch_agent.current_op
             (Some Operation_kind.Rebase)
      then Orchestrator.apply_rebase_result orch parent Worktree.Ok main |> fst
      else orch
  | Merged ->
      let agent = Orchestrator.agent orch parent in
      if agent.Patch_agent.merged then orch
      else Orchestrator.mark_merged orch parent

let dependency_materialization_ready orch parent =
  let agent = Orchestrator.agent orch parent in
  agent.Patch_agent.merged
  || Patch_agent.is_pr_present agent
     && agent.Patch_agent.pr_body_delivered
     && (not agent.Patch_agent.has_conflict)
     && agent.Patch_agent.checks_passing

let materialized_with_same_path orch child expected_path =
  match Patch_agent.worktree_state (Orchestrator.agent orch child) with
  | Patch_agent.Materialized path -> String.equal path expected_path
  | Patch_agent.Unmaterialized -> false

(** WSI-1: before the cut, every prefix whose dependency is unavailable or not
    review-ready keeps the child Start blocked. This is the safety half of the
    state distinction. *)
let prop_unmaterialized_gate_safety =
  QCheck2.Test.make
    ~name:
      "WSI-1: Unmaterialized Start stays gated across dependency interleavings"
    ~count:500 gen_dependency_ops (fun ops ->
      try
        let patches, parent, child, orch = bootstrap () in
        let invariant orch =
          let child_agent = Orchestrator.agent orch child in
          Patch_agent.equal_worktree_state
            (Patch_agent.worktree_state child_agent)
            Patch_agent.Unmaterialized
          && (dependency_materialization_ready orch parent
             || not (child_start_planned orch ~patches ~child))
        in
        let rec loop orch = function
          | [] -> true
          | op :: rest ->
              let orch = apply_dependency_op orch parent op in
              invariant orch && loop orch rest
        in
        invariant orch && loop orch ops
      with _ -> false)

(** WSI-2: once the cut exists, every dependency-state prefix still plans the
    child's no-PR retry. This covers CI regression, missing/changed PR state,
    conflicts, queued/running rebases, and merge transitions in any order. *)
let prop_materialized_retry_dependency_invariance =
  QCheck2.Test.make
    ~name:"WSI-2: Materialized retry ignores all later dependency interleavings"
    ~count:500 gen_path_and_dependency_ops (fun (path, ops) ->
      try
        let patches, parent, child, orch = bootstrap () in
        let orch = Orchestrator.set_worktree_path orch child path in
        let rec loop orch = function
          | ops -> (
              materialized_with_same_path orch child path
              && child_start_planned orch ~patches ~child
              &&
              match ops with
              | [] -> true
              | op :: rest -> loop (apply_dependency_op orch parent op) rest)
        in
        loop orch ops
      with _ -> false)

type lifecycle_op =
  | Dependency of dependency_op
  | Schedule_child
  | Fail_child
  | Complete_child
  | Land_child_pr
  | Clear_child_pr
  | Send_child_message
  | Reset_child_intervention

let gen_lifecycle_op =
  QCheck2.Gen.oneof_weighted
    [
      (5, QCheck2.Gen.map (fun op -> Dependency op) gen_dependency_op);
      (1, QCheck2.Gen.return Schedule_child);
      (1, QCheck2.Gen.return Fail_child);
      (1, QCheck2.Gen.return Complete_child);
      (1, QCheck2.Gen.return Land_child_pr);
      (1, QCheck2.Gen.return Clear_child_pr);
      (1, QCheck2.Gen.return Send_child_message);
      (1, QCheck2.Gen.return Reset_child_intervention);
    ]

let gen_lifecycle_ops =
  QCheck2.Gen.list_size (QCheck2.Gen.int_range 1 100) gen_lifecycle_op

let gen_path_and_lifecycle_ops =
  QCheck2.Gen.pair gen_materialized_path gen_lifecycle_ops

let apply_lifecycle_op orch ~patches ~parent ~child = function
  | Dependency op -> apply_dependency_op orch parent op
  | Schedule_child -> (
      match child_start_action orch ~patches ~child with
      | Some action -> Orchestrator.fire orch action
      | None -> orch)
  | Fail_child ->
      let agent = Orchestrator.agent orch child in
      if agent.Patch_agent.busy then
        Orchestrator.apply_session_result orch child
          (Orchestrator.Session_failed
             { is_fresh = true; detail = Some "interleaving timeout" })
      else orch
  | Complete_child ->
      let agent = Orchestrator.agent orch child in
      if agent.Patch_agent.busy then Orchestrator.complete orch child else orch
  | Land_child_pr ->
      let agent = Orchestrator.agent orch child in
      if agent.Patch_agent.merged || Patch_agent.has_pr agent then orch
      else
        let orch = Orchestrator.set_pr_number orch child (Pr_number.of_int 2) in
        if (Orchestrator.agent orch child).Patch_agent.busy then
          Orchestrator.complete orch child
        else orch
  | Clear_child_pr ->
      let agent = Orchestrator.agent orch child in
      if agent.Patch_agent.busy || not (Patch_agent.is_pr_present agent) then
        orch
      else Orchestrator.clear_pr orch child
  | Send_child_message -> Orchestrator.send_human_message orch child "continue"
  | Reset_child_intervention -> Orchestrator.reset_intervention_state orch child

(** WSI-3: materialization is sticky across both axes of the state machine.
    Session scheduling/failure/completion, PR creation/recreation, human input,
    intervention resets, and arbitrary dependency changes never erase or alter
    the established checkout. *)
let prop_materialized_state_is_sticky =
  QCheck2.Test.make
    ~name:
      "WSI-3: Materialized path is sticky across full lifecycle interleavings"
    ~count:500 gen_path_and_lifecycle_ops (fun (path, ops) ->
      try
        let patches, parent, child, orch = bootstrap () in
        let orch = Orchestrator.set_worktree_path orch child path in
        let rec loop orch = function
          | [] -> materialized_with_same_path orch child path
          | op :: rest ->
              let orch = apply_lifecycle_op orch ~patches ~parent ~child op in
              materialized_with_same_path orch child path && loop orch rest
        in
        loop orch ops
      with _ -> false)

(** WSI-4: the Patch 6 witness generalized over arbitrary dependency suffixes.
    The same unsafe dependency snapshot blocks before materialization, allows a
    Start immediately after materialization, survives a timed-out fresh session,
    and remains retryable after every later dependency interleaving. *)
let prop_materialization_is_the_gate_cutover =
  QCheck2.Test.make
    ~name:
      "WSI-4: materialization is the one-way cutover from gated Start to retry"
    ~count:500 gen_path_and_dependency_ops (fun (path, ops) ->
      try
        let patches, parent, child, orch = bootstrap () in
        let orch = Orchestrator.clear_pr orch parent in
        let orch = Orchestrator.set_checks_passing orch parent false in
        let orch = Orchestrator.set_pr_body_delivered orch parent false in
        let orch = Orchestrator.set_has_conflict orch parent in
        if child_start_planned orch ~patches ~child then false
        else
          let orch = Orchestrator.set_worktree_path orch child path in
          match child_start_action orch ~patches ~child with
          | None -> false
          | Some action ->
              let orch = Orchestrator.fire orch action in
              let orch =
                Orchestrator.apply_session_result orch child
                  (Orchestrator.Session_failed
                     { is_fresh = true; detail = Some "timed out" })
              in
              let rec loop orch = function
                | [] ->
                    materialized_with_same_path orch child path
                    && child_start_planned orch ~patches ~child
                | op :: rest ->
                    let orch = apply_dependency_op orch parent op in
                    materialized_with_same_path orch child path
                    && child_start_planned orch ~patches ~child
                    && loop orch rest
              in
              loop orch ops
      with _ -> false)

let () =
  let runner = QCheck_base_runner.run_tests_main in
  ignore
    (runner
       [
         prop_unmaterialized_gate_safety;
         prop_materialized_retry_dependency_invariance;
         prop_materialized_state_is_sticky;
         prop_materialization_is_the_gate_cutover;
       ])
