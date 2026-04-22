open Base
open Onton
open Onton.Types

(** Property tests for [apply_poll_result] log output.

    These properties treat the activity log as a user-facing surface: log
    entries must only appear when the corresponding state transition actually
    occurs. Catches phantom messages like "conflict cleared" on PRs that never
    had a conflict. *)

let main = Branch.of_string "main"

(* -- Helpers -- *)

let make_patch pid branch =
  Patch.
    {
      id = pid;
      title = "Test patch";
      description = "";
      branch;
      dependencies = [];
      spec = "";
      acceptance_criteria = [];
      files = [];
      classification = "";
      changes = [];
      test_stubs_introduced = [];
      test_stubs_implemented = [];
    }

let make_orch patch agent =
  let graph = Graph.of_patches [ patch ] in
  let agents = Map.of_alist_exn (module Patch_id) [ (patch.Patch.id, agent) ] in
  Orchestrator.restore ~graph ~agents
    ~outbox:(Map.empty (module Message_id))
    ~main_branch:main

let make_agent ~patch_id ~branch ~has_conflict ~ci_failure_count ~current_op
    ~checks_passing ~queue ~merged ~is_draft ~branch_blocked ~worktree_path =
  let busy = Option.is_some current_op in
  Patch_agent.restore ~patch_id ~branch
    ~pr_number:(Some (Pr_number.of_int 42))
    ~has_session:busy ~busy ~merged ~queue ~satisfies:false ~changed:false
    ~has_conflict ~base_branch:(Some main) ~notified_base_branch:(Some main)
    ~ci_failure_count ~session_fallback:Patch_agent.Fresh_available
    ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
    ~merge_ready:false ~is_draft ~pr_body_delivered:true
    ~start_attempts_without_pr:0 ~conflict_noop_count:0 ~no_commits_push_count:0
    ~branch_rebased_onto:None ~checks_passing ~current_op
    ~current_message_id:None ~generation:0 ~worktree_path ~branch_blocked
    ~llm_session_id:None ~automerge_enabled:false ~automerge_deadline:None
    ~automerge_inflight:false ~automerge_failure_count:0
    ~delivered_ci_run_ids:[]

let make_poll_observation ~branch_in_root ~worktree_path poll_result =
  Patch_controller.
    { poll_result; base_branch = None; branch_in_root; worktree_path }

let make_poll ~has_conflict ~merged ~checks_passing ~is_draft ~queue =
  Poller.
    {
      queue;
      merged;
      closed = false;
      is_draft;
      has_conflict;
      merge_ready = false;
      checks_passing;
      ci_checks = [];
    }

let has_log_matching needle logs =
  List.exists logs ~f:(fun (entry : Patch_controller.poll_log_entry) ->
      String.is_substring entry.message ~substring:needle)

let count_log_matching needle logs =
  List.count logs ~f:(fun (entry : Patch_controller.poll_log_entry) ->
      String.is_substring entry.message ~substring:needle)

(* -- Generators -- *)

open Onton_test_support.Test_generators

(** Generate a poll log test case: (patch, orchestrator, agent_snapshot,
    poll_observation). The agent_snapshot captures the agent state before the
    poll is applied. *)
let gen_poll_log_case =
  QCheck2.Gen.(
    let* pid = gen_patch_id in
    let* branch = gen_branch in
    (* Agent dimensions *)
    let* agent_has_conflict = bool in
    let* agent_merged = bool in
    let* ci_failure_count = int_range 0 5 in
    let* agent_checks_passing = bool in
    let* agent_queue = gen_operation_kind_queue in
    let* has_current_op = bool in
    let* current_op =
      if has_current_op then map Option.some gen_operation_kind else pure None
    in
    let* agent_is_draft = bool in
    let* agent_branch_blocked = bool in
    (* Agent worktree_path: generate optionally *)
    let* has_agent_worktree_path = bool in
    let* agent_worktree_path =
      if has_agent_worktree_path then
        map Option.some (string_size ~gen:(char_range 'a' 'z') (int_range 5 20))
      else pure None
    in
    (* Poll dimensions *)
    let* poll_has_conflict = bool in
    let* poll_merged = bool in
    let* poll_checks_passing = bool in
    let* poll_is_draft = bool in
    let* poll_queue =
      map
        (List.dedup_and_sort ~compare:Operation_kind.compare)
        (list_small gen_feedback_kind)
    in
    (* Observation dimensions *)
    let* obs_branch_in_root = bool in
    let* has_obs_worktree_path = bool in
    let* obs_worktree_path =
      if has_obs_worktree_path then
        map Option.some (string_size ~gen:(char_range 'a' 'z') (int_range 5 20))
      else pure None
    in
    let patch = make_patch pid branch in
    let agent =
      make_agent ~patch_id:pid ~branch ~has_conflict:agent_has_conflict
        ~ci_failure_count ~current_op ~checks_passing:agent_checks_passing
        ~queue:agent_queue ~merged:agent_merged ~is_draft:agent_is_draft
        ~branch_blocked:agent_branch_blocked ~worktree_path:agent_worktree_path
    in
    let orch = make_orch patch agent in
    let poll =
      make_poll ~has_conflict:poll_has_conflict ~merged:poll_merged
        ~checks_passing:poll_checks_passing ~is_draft:poll_is_draft
        ~queue:poll_queue
    in
    let observation =
      make_poll_observation ~branch_in_root:obs_branch_in_root
        ~worktree_path:obs_worktree_path poll
    in
    return (patch, pid, orch, agent, observation, poll))

let print_case =
 fun (patch, pid, _orch, agent, obs, poll) ->
  Printf.sprintf
    "patch=%s pid=%s agent={merged=%b has_conflict=%b ci_failure_count=%d \
     checks_passing=%b is_draft=%b branch_blocked=%b queue=[%s] current_op=%s} \
     poll={merged=%b has_conflict=%b checks_passing=%b is_draft=%b queue=[%s]} \
     obs={branch_in_root=%b worktree_path=%s}"
    (Patch_id.to_string patch.Patch.id)
    (Patch_id.to_string pid) agent.Patch_agent.merged agent.has_conflict
    agent.ci_failure_count agent.checks_passing agent.is_draft
    agent.branch_blocked
    (String.concat ~sep:"," (List.map agent.queue ~f:Operation_kind.to_label))
    (Option.value_map agent.current_op ~default:"none"
       ~f:Operation_kind.to_label)
    poll.Poller.merged poll.has_conflict poll.checks_passing poll.is_draft
    (String.concat ~sep:"," (List.map poll.queue ~f:Operation_kind.to_label))
    obs.Patch_controller.branch_in_root
    (Option.value obs.Patch_controller.worktree_path ~default:"none")

(* -- Properties -- *)

let () =
  let open QCheck2 in
  (* L-1: No phantom "conflict cleared" — if agent never had conflict and
     poll reports no conflict, no conflict-related logs should appear. *)
  let prop_no_phantom_conflict_cleared =
    Test.make
      ~name:
        "poll-log L-1: no phantom conflict cleared when agent has no conflict"
      ~count:300 ~print:print_case gen_poll_log_case
      (fun (_patch, pid, orch, agent, observation, poll) ->
        try
          if
            (not agent.Patch_agent.has_conflict) && not poll.Poller.has_conflict
          then
            let _orch, logs, _blocked =
              Patch_controller.apply_poll_result orch pid observation
            in
            (not (has_log_matching "Conflict cleared" logs))
            && not (has_log_matching "Conflict flag retained" logs)
          else true
        with _ -> false)
  in

  (* L-2: No phantom "conflict detected" — if agent already has conflict
     and poll reports conflict, no "merge conflict detected" log. *)
  let prop_no_phantom_conflict_detected =
    Test.make
      ~name:
        "poll-log L-2: no phantom conflict detected when agent already \
         conflicted"
      ~count:300 ~print:print_case gen_poll_log_case
      (fun (_patch, pid, orch, agent, observation, poll) ->
        try
          if agent.Patch_agent.has_conflict && poll.Poller.has_conflict then
            let _orch, logs, _blocked =
              Patch_controller.apply_poll_result orch pid observation
            in
            not (has_log_matching "Merge conflict detected" logs)
          else true
        with _ -> false)
  in

  (* L-3: Merged is one-shot — if agent is already merged and poll says
     merged, no "merged" log. *)
  let prop_merged_one_shot =
    Test.make
      ~name:
        "poll-log L-3: merged log is one-shot (no repeat for already merged)"
      ~count:300 ~print:print_case gen_poll_log_case
      (fun (_patch, pid, orch, agent, observation, poll) ->
        try
          if agent.Patch_agent.merged && poll.Poller.merged then
            let _orch, logs, _blocked =
              Patch_controller.apply_poll_result orch pid observation
            in
            not (has_log_matching "Merged" logs)
          else true
        with _ -> false)
  in

  (* L-4: Merged transition logged — if agent is not merged and poll says
     merged, exactly one "merged" log entry. *)
  let prop_merged_transition_logged =
    Test.make
      ~name:"poll-log L-4: merged transition produces exactly one merged log"
      ~count:300 ~print:print_case gen_poll_log_case
      (fun (_patch, pid, orch, agent, observation, poll) ->
        try
          if (not agent.Patch_agent.merged) && poll.Poller.merged then
            let _orch, logs, _blocked =
              Patch_controller.apply_poll_result orch pid observation
            in
            count_log_matching "Merged" logs = 1
          else true
        with _ -> false)
  in

  (* L-5: Idempotent poll is silent on transitions — applying the same
     poll twice, the second produces no transition logs. *)
  let prop_idempotent_poll_silent =
    Test.make
      ~name:
        "poll-log L-5: idempotent poll is silent on transitions (second apply)"
      ~count:300 ~print:print_case gen_poll_log_case
      (fun (_patch, pid, orch, _agent, observation, _poll) ->
        try
          let orch1, _logs1, _blocked1 =
            Patch_controller.apply_poll_result orch pid observation
          in
          let _orch2, logs2, _blocked2 =
            Patch_controller.apply_poll_result orch1 pid observation
          in
          (not (has_log_matching "Merged" logs2))
          && (not (has_log_matching "Merge conflict detected" logs2))
          && (not (has_log_matching "Conflict cleared" logs2))
          && (not (has_log_matching "Marked as" logs2))
          && (not (has_log_matching "blocking worktree" logs2))
          && (not (has_log_matching "unblocked" logs2))
          && not (has_log_matching "Worktree path discovered" logs2)
        with _ -> false)
  in

  (* L-6: CI decision logs are mutually exclusive — when Ci is in poll
     queue, at most one CI decision log appears. *)
  let prop_ci_decision_exclusive =
    Test.make ~name:"poll-log L-6: CI decision logs are mutually exclusive"
      ~count:300 ~print:print_case gen_poll_log_case
      (fun (_patch, pid, orch, _agent, observation, poll) ->
        try
          if
            List.mem poll.Poller.queue Operation_kind.Ci
              ~equal:Operation_kind.equal
          then
            let _orch, logs, _blocked =
              Patch_controller.apply_poll_result orch pid observation
            in
            let ci_log_count =
              count_log_matching "Enqueued ci" logs
              + count_log_matching "CI already queued" logs
              + count_log_matching "CI fix in progress" logs
              + count_log_matching "CI failure cap reached" logs
            in
            ci_log_count = 1
          else true
        with _ -> false)
  in

  (* L-7: CI reset requires ci_failure_count > 0 and checks_passing. *)
  let prop_ci_reset_requires_conditions =
    Test.make
      ~name:
        "poll-log L-7: CI reset log requires ci_failure_count > 0 and \
         checks_passing" ~count:300 ~print:print_case gen_poll_log_case
      (fun (_patch, pid, orch, agent, observation, poll) ->
        try
          let _orch, logs, _blocked =
            Patch_controller.apply_poll_result orch pid observation
          in
          if has_log_matching "CI checks now passing" logs then
            agent.Patch_agent.ci_failure_count > 0 && poll.Poller.checks_passing
          else true
        with _ -> false)
  in

  (* L-8: Enqueue log only when operation is new — for each "enqueued X"
     in logs, X was not in agent.queue and not agent.current_op. *)
  let prop_enqueue_log_only_when_new =
    Test.make ~name:"poll-log L-8: enqueue log only emitted for new operations"
      ~count:300 ~print:print_case gen_poll_log_case
      (fun (_patch, pid, orch, agent, observation, _poll) ->
        try
          let _orch, logs, _blocked =
            Patch_controller.apply_poll_result orch pid observation
          in
          List.for_all logs ~f:(fun (entry : Patch_controller.poll_log_entry) ->
              if String.is_prefix entry.message ~prefix:"Enqueued " then
                let label =
                  String.chop_prefix_exn entry.message ~prefix:"Enqueued "
                in
                let kind =
                  List.find
                    Operation_kind.
                      [ Rebase; Human; Merge_conflict; Ci; Review_comments ]
                    ~f:(fun k -> String.equal (Operation_kind.to_label k) label)
                in
                match kind with
                | Some k ->
                    let was_queued =
                      List.mem agent.Patch_agent.queue k
                        ~equal:Operation_kind.equal
                    in
                    let was_current_op =
                      Option.equal Operation_kind.equal agent.current_op
                        (Some k)
                    in
                    (not was_queued) && not was_current_op
                | None -> true
              else true)
        with _ -> false)
  in

  let suite =
    [
      prop_no_phantom_conflict_cleared;
      prop_no_phantom_conflict_detected;
      prop_merged_one_shot;
      prop_merged_transition_logged;
      prop_idempotent_poll_silent;
      prop_ci_decision_exclusive;
      prop_ci_reset_requires_conditions;
      prop_enqueue_log_only_when_new;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode;
  Stdlib.print_endline "all poll-log properties passed"
