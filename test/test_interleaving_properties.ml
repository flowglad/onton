open Base
open Onton
open Onton.Types

(** Comprehensive interleaving property tests.

    These properties exercise random interleavings of poller and runner
    operations on the orchestrator, checking 10 invariants after every step. The
    command set covers the full lifecycle: polling (conflict, merge, CI failure,
    checks passing, review comments), session results (ok, failure, give-up),
    rebase results, human messages, and intervention resets. *)

(* -- Helpers -- *)

let main = Branch.of_string "main"
let mk_patches = Onton_test_support.Test_generators.mk_linear_patches
let make_gameplan = Onton_test_support.Test_generators.make_test_gameplan
let pid_of_idx = Onton_test_support.Test_generators.pid_of_idx

let branch_of_patches patches =
  let map =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) ->
        Map.set acc ~key:p.Patch.id ~data:p.Patch.branch)
  in
  fun pid -> Option.value (Map.find map pid) ~default:main

let bootstrap patches =
  let branch_of = branch_of_patches patches in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  List.foldi patches ~init:orch ~f:(fun i o _p ->
      let pid = pid_of_idx patches i in
      let has_merged dep_pid =
        (Orchestrator.agent o dep_pid).Patch_agent.merged
      in
      let base =
        Graph.initial_base (Orchestrator.graph o) pid ~has_merged ~branch_of
          ~main
      in
      let o = Orchestrator.fire o (Orchestrator.Start (pid, base)) in
      let o = Orchestrator.set_pr_number o pid (Pr_number.of_int (i + 1)) in
      Orchestrator.complete o pid)

(* -- Command type -- *)

type poll_kind =
  | Poll_normal
  | Poll_conflict
  | Poll_merged
  | Poll_ci_failed
  | Poll_checks_passing
  | Poll_review_comments

type session_result_kind =
  | Sess_ok
  | Sess_failed_fresh
  | Sess_failed_resume
  | Sess_give_up
  | Sess_worktree_missing
  | Sess_process_error_fresh
  | Sess_process_error_resume
  | Sess_no_resume

type rebase_result_kind =
  | Rebase_ok
  | Rebase_noop
  | Rebase_conflict
  | Rebase_error

type push_result_kind =
  | Push_ok_k
  | Push_up_to_date_k
  | Push_rejected_k
  | Push_error_k

type command =
  | Apply_poll of { patch_idx : int; poll_kind : poll_kind }
  | Reconcile
  | Runner_tick
  | Complete of int
  | Apply_session_result of { patch_idx : int; result : session_result_kind }
  | Apply_rebase_result of { patch_idx : int; result : rebase_result_kind }
  | Send_human_message of int
  | Reset_intervention of int
  | Atomic_poll_reconcile of { patch_idx : int; poll_kind : poll_kind }
  | Apply_conflict_rebase_result of {
      patch_idx : int;
      result : rebase_result_kind;
    }
  | Apply_rebase_push_result of { patch_idx : int; result : push_result_kind }
  | Add_adhoc of int
  | Remove_adhoc of int

let show_poll_kind = function
  | Poll_normal -> "Normal"
  | Poll_conflict -> "Conflict"
  | Poll_merged -> "Merged"
  | Poll_ci_failed -> "Ci_failed"
  | Poll_checks_passing -> "Checks_passing"
  | Poll_review_comments -> "Review_comments"

let show_session_result_kind = function
  | Sess_ok -> "Ok"
  | Sess_failed_fresh -> "Failed_fresh"
  | Sess_failed_resume -> "Failed_resume"
  | Sess_give_up -> "Give_up"
  | Sess_worktree_missing -> "Worktree_missing"
  | Sess_process_error_fresh -> "Process_error_fresh"
  | Sess_process_error_resume -> "Process_error_resume"
  | Sess_no_resume -> "No_resume"

let show_rebase_result_kind = function
  | Rebase_ok -> "Ok"
  | Rebase_noop -> "Noop"
  | Rebase_conflict -> "Conflict"
  | Rebase_error -> "Error"

let show_push_result_kind = function
  | Push_ok_k -> "Push_ok"
  | Push_up_to_date_k -> "Push_up_to_date"
  | Push_rejected_k -> "Push_rejected"
  | Push_error_k -> "Push_error"

let show_command = function
  | Apply_poll { patch_idx; poll_kind } ->
      Printf.sprintf "Apply_poll(%d, %s)" patch_idx (show_poll_kind poll_kind)
  | Reconcile -> "Reconcile"
  | Runner_tick -> "Runner_tick"
  | Complete i -> Printf.sprintf "Complete(%d)" i
  | Apply_session_result { patch_idx; result } ->
      Printf.sprintf "Apply_session_result(%d, %s)" patch_idx
        (show_session_result_kind result)
  | Apply_rebase_result { patch_idx; result } ->
      Printf.sprintf "Apply_rebase_result(%d, %s)" patch_idx
        (show_rebase_result_kind result)
  | Send_human_message i -> Printf.sprintf "Send_human_message(%d)" i
  | Reset_intervention i -> Printf.sprintf "Reset_intervention(%d)" i
  | Atomic_poll_reconcile { patch_idx; poll_kind } ->
      Printf.sprintf "Atomic_poll_reconcile(%d, %s)" patch_idx
        (show_poll_kind poll_kind)
  | Apply_conflict_rebase_result { patch_idx; result } ->
      Printf.sprintf "Apply_conflict_rebase_result(%d, %s)" patch_idx
        (show_rebase_result_kind result)
  | Apply_rebase_push_result { patch_idx; result } ->
      Printf.sprintf "Apply_rebase_push_result(%d, %s)" patch_idx
        (show_push_result_kind result)
  | Add_adhoc i -> Printf.sprintf "Add_adhoc(%d)" i
  | Remove_adhoc i -> Printf.sprintf "Remove_adhoc(%d)" i

(* -- Ad-hoc patch helpers -- *)

(** Maximum number of ad-hoc patches in interleaving tests. *)
let max_adhoc = 2

let adhoc_pid i = Patch_id.of_string (Printf.sprintf "adhoc-%d" i)
let adhoc_branch i = Branch.of_string (Printf.sprintf "adhoc-b%d" i)
let adhoc_pr i = Pr_number.of_int (100 + i)

(** Resolve a command index to a patch_id. Indices [0..n-1] map to gameplan
    patches; indices [n..n+max_adhoc-1] map to ad-hoc patches. *)
let resolve_pid patches idx =
  let n = List.length patches in
  if idx < n then pid_of_idx patches idx else adhoc_pid (idx - n)

(** Extended branch lookup covering both gameplan and ad-hoc patches. *)
let extended_branch_of patches =
  let map =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) ->
        Map.set acc ~key:p.Patch.id ~data:p.Patch.branch)
  in
  let map =
    List.fold (List.init max_adhoc ~f:Fn.id) ~init:map ~f:(fun acc i ->
        Map.set acc ~key:(adhoc_pid i) ~data:(adhoc_branch i))
  in
  fun pid -> Option.value (Map.find map pid) ~default:main

(* -- Generators -- *)

let gen_poll_kind =
  QCheck2.Gen.oneof_list
    [
      Poll_normal;
      Poll_conflict;
      Poll_merged;
      Poll_ci_failed;
      Poll_checks_passing;
      Poll_review_comments;
    ]

let gen_session_result_kind =
  QCheck2.Gen.oneof_list
    [
      Sess_ok;
      Sess_failed_fresh;
      Sess_failed_resume;
      Sess_give_up;
      Sess_worktree_missing;
      Sess_process_error_fresh;
      Sess_process_error_resume;
      Sess_no_resume;
    ]

let gen_rebase_result_kind =
  QCheck2.Gen.oneof_list
    [ Rebase_ok; Rebase_noop; Rebase_conflict; Rebase_error ]

let gen_push_result_kind =
  QCheck2.Gen.oneof_list
    [ Push_ok_k; Push_up_to_date_k; Push_rejected_k; Push_error_k ]

let gen_command ~n =
  let total = n + max_adhoc in
  QCheck2.Gen.(
    let gen_idx = int_range 0 (total - 1) in
    oneof
      [
        map2
          (fun i pk -> Apply_poll { patch_idx = i; poll_kind = pk })
          gen_idx gen_poll_kind;
        pure Reconcile;
        pure Runner_tick;
        map (fun i -> Complete i) gen_idx;
        map2
          (fun i r -> Apply_session_result { patch_idx = i; result = r })
          gen_idx gen_session_result_kind;
        map2
          (fun i r -> Apply_rebase_result { patch_idx = i; result = r })
          gen_idx gen_rebase_result_kind;
        map2
          (fun i r ->
            Apply_conflict_rebase_result { patch_idx = i; result = r })
          gen_idx gen_rebase_result_kind;
        map2
          (fun i r -> Apply_rebase_push_result { patch_idx = i; result = r })
          gen_idx gen_push_result_kind;
        map (fun i -> Send_human_message i) gen_idx;
        map (fun i -> Reset_intervention i) gen_idx;
        map (fun i -> Add_adhoc i) (int_range 0 (max_adhoc - 1));
        map (fun i -> Remove_adhoc i) (int_range 0 (max_adhoc - 1));
      ])

let gen_command_seq ~n ~len =
  if len < 1 then invalid_arg "gen_command_seq: len must be at least 1";
  QCheck2.Gen.(list_size (int_range 1 len) (gen_command ~n))

let gen_atomic_command ~n =
  let total = n + max_adhoc in
  QCheck2.Gen.(
    let gen_idx = int_range 0 (total - 1) in
    oneof
      [
        map2
          (fun i pk -> Atomic_poll_reconcile { patch_idx = i; poll_kind = pk })
          gen_idx gen_poll_kind;
        pure Runner_tick;
        map (fun i -> Complete i) gen_idx;
        map2
          (fun i r -> Apply_session_result { patch_idx = i; result = r })
          gen_idx gen_session_result_kind;
        map2
          (fun i r -> Apply_rebase_result { patch_idx = i; result = r })
          gen_idx gen_rebase_result_kind;
        map2
          (fun i r ->
            Apply_conflict_rebase_result { patch_idx = i; result = r })
          gen_idx gen_rebase_result_kind;
        map2
          (fun i r -> Apply_rebase_push_result { patch_idx = i; result = r })
          gen_idx gen_push_result_kind;
        map (fun i -> Send_human_message i) gen_idx;
        map (fun i -> Reset_intervention i) gen_idx;
        map (fun i -> Add_adhoc i) (int_range 0 (max_adhoc - 1));
        map (fun i -> Remove_adhoc i) (int_range 0 (max_adhoc - 1));
      ])

let gen_atomic_command_seq ~n ~len =
  if len < 1 then invalid_arg "gen_atomic_command_seq: len must be at least 1";
  QCheck2.Gen.(list_size (int_range 1 len) (gen_atomic_command ~n))

(* -- Command application -- *)

let make_poll_result ~has_conflict ~merged ~ci_failed ~checks_passing
    ~review_comments =
  Poller.
    {
      queue =
        (let acc = [] in
         let acc =
           if review_comments then Operation_kind.Review_comments :: acc
           else acc
         in
         let acc =
           if has_conflict then Operation_kind.Merge_conflict :: acc else acc
         in
         let acc = if ci_failed then Operation_kind.Ci :: acc else acc in
         List.rev acc);
      merged;
      closed = false;
      is_draft = false;
      has_conflict;
      merge_ready =
        (not has_conflict) && (not merged) && (not ci_failed)
        && not review_comments;
      checks_passing;
      ci_checks = [];
    }

let apply_reconcile orch patches =
  let branch_of = branch_of_patches patches in
  Onton_test_support.Test_generators.apply_reconcile_actions orch ~main
    ~branch_of

let to_session_result = function
  | Sess_ok -> Orchestrator.Session_ok
  | Sess_failed_fresh -> Orchestrator.Session_failed { is_fresh = true }
  | Sess_failed_resume -> Orchestrator.Session_failed { is_fresh = false }
  | Sess_give_up -> Orchestrator.Session_give_up
  | Sess_worktree_missing -> Orchestrator.Session_worktree_missing
  | Sess_process_error_fresh ->
      Orchestrator.Session_process_error { is_fresh = true }
  | Sess_process_error_resume ->
      Orchestrator.Session_process_error { is_fresh = false }
  | Sess_no_resume -> Orchestrator.Session_no_resume

let to_worktree_result = function
  | Rebase_ok -> Worktree.Ok
  | Rebase_noop -> Worktree.Noop
  | Rebase_conflict -> Worktree.Conflict
  | Rebase_error -> Worktree.Error "simulated error"

let to_push_result = function
  | Push_ok_k -> Worktree.Push_ok
  | Push_up_to_date_k -> Worktree.Push_up_to_date
  | Push_rejected_k -> Worktree.Push_rejected
  | Push_error_k -> Worktree.Push_error "simulated error"

let poll_params_of_kind = function
  | Poll_normal -> (false, false, false, true, false)
  | Poll_conflict -> (true, false, false, false, false)
  | Poll_merged -> (false, true, false, false, false)
  | Poll_ci_failed -> (false, false, true, false, false)
  | Poll_checks_passing -> (false, false, false, true, false)
  | Poll_review_comments -> (false, false, false, true, true)

let rec apply_command orch patches cmd =
  let gameplan = make_gameplan patches in
  let branch_of = extended_branch_of patches in
  match cmd with
  | Apply_poll { patch_idx; poll_kind } -> (
      let pid = resolve_pid patches patch_idx in
      let has_conflict, merged, ci_failed, checks_passing, review_comments =
        poll_params_of_kind poll_kind
      in
      let poll_result =
        make_poll_result ~has_conflict ~merged ~ci_failed ~checks_passing
          ~review_comments
      in
      let observation =
        Patch_controller.
          {
            poll_result;
            base_branch = None;
            branch_in_root = false;
            worktree_path = None;
          }
      in
      try
        let orch, _logs, _blocked =
          Patch_controller.apply_poll_result orch pid observation
        in
        orch
      with Invalid_argument _ -> orch)
  | Reconcile ->
      let orch = apply_reconcile orch patches in
      let gameplan = make_gameplan patches in
      let orch, _effects =
        Patch_controller.reconcile_all orch ~project_name:"test-project"
          ~gameplan
      in
      orch
  | Runner_tick -> (
      try
        let orch, _effects, _actions =
          Patch_controller.tick orch ~project_name:"test-project" ~gameplan
        in
        orch
      with Invalid_argument _ -> orch)
  | Complete patch_idx -> (
      let pid = resolve_pid patches patch_idx in
      match Orchestrator.find_agent orch pid with
      | Some agent when agent.Patch_agent.busy -> Orchestrator.complete orch pid
      | _ -> orch)
  | Apply_session_result { patch_idx; result } -> (
      let pid = resolve_pid patches patch_idx in
      try
        let o =
          Orchestrator.apply_session_result orch pid (to_session_result result)
        in
        match result with
        | Sess_ok ->
            Orchestrator.set_llm_session_id o pid
              (Some (Printf.sprintf "sess-%s" (Patch_id.to_string pid)))
        | Sess_failed_fresh | Sess_failed_resume | Sess_give_up
        | Sess_worktree_missing | Sess_process_error_fresh
        | Sess_process_error_resume | Sess_no_resume ->
            o
      with Invalid_argument _ -> orch)
  | Apply_rebase_result { patch_idx; result } -> (
      let pid = resolve_pid patches patch_idx in
      try
        let has_merged dep_pid =
          match Orchestrator.find_agent orch dep_pid with
          | Some a -> a.Patch_agent.merged
          | None -> false
        in
        let new_base =
          Graph.initial_base (Orchestrator.graph orch) pid ~has_merged
            ~branch_of ~main
        in
        let orch, _effects =
          Orchestrator.apply_rebase_result orch pid
            (to_worktree_result result)
            new_base
        in
        orch
      with Invalid_argument _ -> orch)
  | Send_human_message patch_idx ->
      let pid = resolve_pid patches patch_idx in
      Orchestrator.send_human_message orch pid "test message"
  | Reset_intervention patch_idx ->
      let pid = resolve_pid patches patch_idx in
      Orchestrator.reset_intervention_state orch pid
  | Atomic_poll_reconcile { patch_idx; poll_kind } ->
      let orch =
        apply_command orch patches (Apply_poll { patch_idx; poll_kind })
      in
      apply_command orch patches Reconcile
  | Apply_conflict_rebase_result { patch_idx; result } -> (
      let pid = resolve_pid patches patch_idx in
      try
        let has_merged dep_pid =
          match Orchestrator.find_agent orch dep_pid with
          | Some a -> a.Patch_agent.merged
          | None -> false
        in
        let new_base =
          Graph.initial_base (Orchestrator.graph orch) pid ~has_merged
            ~branch_of ~main
        in
        let orch', decision, _effects =
          Orchestrator.apply_conflict_rebase_result orch pid
            (to_worktree_result result)
            new_base
        in
        match decision with
        | Orchestrator.Deliver_to_agent ->
            (* Don't auto-resolve — leave the agent busy so subsequent random
               Apply_session_result commands drive the outcome, just like real
               execution. The agent may or may not clear has_conflict. *)
            orch'
        | Orchestrator.Conflict_resolved | Orchestrator.Conflict_failed -> orch'
      with Invalid_argument _ -> orch)
  | Apply_rebase_push_result { patch_idx; result } -> (
      let pid = resolve_pid patches patch_idx in
      try
        let orch, _resolution =
          Orchestrator.apply_rebase_push_result orch pid
            (Some (to_push_result result))
        in
        orch
      with Invalid_argument _ -> orch)
  | Add_adhoc i ->
      Orchestrator.add_agent orch ~patch_id:(adhoc_pid i)
        ~branch:(adhoc_branch i) ~pr_number:(adhoc_pr i)
  | Remove_adhoc i -> Orchestrator.remove_agent orch (adhoc_pid i)

type poll_log_info = {
  agent_before : Patch_agent.t;
  poll_kind : poll_kind;
  patch_id : Patch_id.t;
  logs : Patch_controller.poll_log_entry list;
}
(** Apply a command and return (orchestrator, poll_log_info option).
    poll_log_info is Some when the command was an Apply_poll, carrying the agent
    state before the poll, the poll_kind, and the resulting logs. *)

let rec apply_command_with_logs orch patches cmd =
  match cmd with
  | Apply_poll { patch_idx; poll_kind } -> (
      let pid = resolve_pid patches patch_idx in
      match Orchestrator.find_agent orch pid with
      | None -> (orch, None)
      | Some agent_before -> (
          let has_conflict, merged, ci_failed, checks_passing, review_comments =
            poll_params_of_kind poll_kind
          in
          let poll_result =
            make_poll_result ~has_conflict ~merged ~ci_failed ~checks_passing
              ~review_comments
          in
          let observation =
            Patch_controller.
              {
                poll_result;
                base_branch = None;
                branch_in_root = false;
                worktree_path = None;
              }
          in
          try
            let orch, logs, _blocked =
              Patch_controller.apply_poll_result orch pid observation
            in
            let info = Some { agent_before; poll_kind; patch_id = pid; logs } in
            (orch, info)
          with Invalid_argument _ -> (orch, None)))
  | Atomic_poll_reconcile { patch_idx; poll_kind } ->
      let orch, info =
        apply_command_with_logs orch patches
          (Apply_poll { patch_idx; poll_kind })
      in
      let orch = apply_command orch patches Reconcile in
      (orch, info)
  | Reconcile | Runner_tick | Complete _ | Apply_session_result _
  | Apply_rebase_result _ | Send_human_message _ | Reset_intervention _
  | Apply_conflict_rebase_result _ | Apply_rebase_push_result _ | Add_adhoc _
  | Remove_adhoc _ ->
      (apply_command orch patches cmd, None)

(* ========== Log invariant checks ========== *)

let has_log_matching needle logs =
  List.exists logs ~f:(fun (entry : Patch_controller.poll_log_entry) ->
      String.is_substring entry.message ~substring:needle)

(** L-I-1: No phantom conflict cleared — if agent had no conflict before and
    poll reports no conflict, no conflict-related logs should appear. *)
let check_no_phantom_conflict_cleared (info : poll_log_info) =
  let poll_has_conflict =
    match info.poll_kind with
    | Poll_conflict -> true
    | Poll_normal | Poll_merged | Poll_ci_failed | Poll_checks_passing
    | Poll_review_comments ->
        false
  in
  if (not info.agent_before.Patch_agent.has_conflict) && not poll_has_conflict
  then
    if
      has_log_matching "conflict cleared" info.logs
      || has_log_matching "conflict flag retained" info.logs
    then
      failwith
        (Printf.sprintf "L-I-1 no_phantom_conflict_cleared violated for %s"
           (Patch_id.to_string info.patch_id))

(** L-I-2: Merged at most once per patch across the full sequence. *)
let check_merged_logged_at_most_once (info : poll_log_info) ~merged_logged =
  if has_log_matching "merged" info.logs then
    if Set.mem merged_logged info.patch_id then
      failwith
        (Printf.sprintf "L-I-2 merged_logged_at_most_once violated for %s"
           (Patch_id.to_string info.patch_id))
    else Set.add merged_logged info.patch_id
  else merged_logged

(** L-I-3: Conflict detected requires transition — if agent already had
    conflict, no "merge conflict detected" log. *)
let check_conflict_detected_requires_transition (info : poll_log_info) =
  if info.agent_before.Patch_agent.has_conflict then
    if has_log_matching "merge conflict detected" info.logs then
      failwith
        (Printf.sprintf
           "L-I-3 conflict_detected_requires_transition violated for %s"
           (Patch_id.to_string info.patch_id))

let check_log_invariants info ~merged_logged =
  check_no_phantom_conflict_cleared info;
  check_conflict_detected_requires_transition info;
  check_merged_logged_at_most_once info ~merged_logged

(* ========== Invariant checks ========== *)

let pid_of_action = function
  | Orchestrator.Start (pid, _)
  | Orchestrator.Respond (pid, _)
  | Orchestrator.Rebase (pid, _) ->
      pid

(** I-1: busy -> has_session *)
let check_busy_implies_has_session (a : Patch_agent.t) =
  if a.busy && not a.has_session then
    failwith
      (Printf.sprintf "I-1 busy_implies_has_session violated for %s"
         (Patch_id.to_string a.patch_id))

(** I-2: ci_failure_count >= 0 *)
let check_ci_failure_count_non_negative (a : Patch_agent.t) =
  if a.ci_failure_count < 0 then
    failwith
      (Printf.sprintf "I-2 ci_failure_count_non_negative violated for %s"
         (Patch_id.to_string a.patch_id))

(** I-3: base_branch freshness — when Respond(Merge_conflict) would fire,
    agent.base_branch must equal Graph.initial_base(pid). *)
let check_base_branch_freshness orch patches action =
  match action with
  | Orchestrator.Respond (pid, kind)
    when Operation_kind.equal kind Operation_kind.Merge_conflict ->
      let branch_of = branch_of_patches patches in
      let agent = Orchestrator.agent orch pid in
      let has_merged dep_pid =
        (Orchestrator.agent orch dep_pid).Patch_agent.merged
      in
      let expected =
        Graph.initial_base (Orchestrator.graph orch) pid ~has_merged ~branch_of
          ~main
      in
      let actual = Option.value agent.Patch_agent.base_branch ~default:main in
      if not (Branch.equal actual expected) then
        failwith
          (Printf.sprintf "I-3 base_branch_freshness violated for %s: %s != %s"
             (Patch_id.to_string pid) (Branch.to_string actual)
             (Branch.to_string expected))
  | Orchestrator.Respond (_, _)
  | Orchestrator.Rebase (_, _)
  | Orchestrator.Start (_, _) ->
      ()

(** I-4: merged is monotonic — once merged, never un-merged. Patches that were
    removed from the orchestrator are excluded: their disappearance from the
    merged set is expected, not a violation. *)
let check_merged_monotonicity ~prev_merged ~curr_merged ~removed_pids =
  Set.iter prev_merged ~f:(fun pid ->
      if (not (Set.mem curr_merged pid)) && not (Set.mem removed_pids pid) then
        failwith
          (Printf.sprintf "I-4 merged_monotonicity violated: %s un-merged"
             (Patch_id.to_string pid)))

(** I-5: merged patches produce no actions. *)
let check_merged_blocks_actions orch action =
  let pid = pid_of_action action in
  let agent = Orchestrator.agent orch pid in
  if agent.Patch_agent.merged then
    failwith
      (Printf.sprintf "I-5 merged_blocks_actions violated for %s"
         (Patch_id.to_string pid))

(** I-6: plan_actions respects priority ordering. *)
let check_priority_ordering orch action =
  let pid = pid_of_action action in
  let agent = Orchestrator.agent orch pid in
  let expected_hp = Patch_agent.highest_priority agent in
  let actual_kind =
    match action with
    | Orchestrator.Start _ -> None
    | Orchestrator.Respond (_, k) -> Some k
    | Orchestrator.Rebase _ -> Some Operation_kind.Rebase
  in
  match (expected_hp, actual_kind) with
  | Some hp, Some ak ->
      if not (Operation_kind.equal hp ak) then
        failwith
          (Printf.sprintf
             "I-6 priority_ordering violated for %s: expected %s, got %s"
             (Patch_id.to_string pid)
             (Operation_kind.to_label hp)
             (Operation_kind.to_label ak))
  | None, Some ak ->
      failwith
        (Printf.sprintf
           "I-6 priority_ordering violated for %s: unexpected %s with empty \
            queue"
           (Patch_id.to_string pid)
           (Operation_kind.to_label ak))
  | _ -> ()

(** I-7: no duplicate operations in any agent's queue. *)
let check_queue_no_duplicates (a : Patch_agent.t) =
  let unique = List.dedup_and_sort a.queue ~compare:Operation_kind.compare in
  if List.length a.queue <> List.length unique then
    failwith
      (Printf.sprintf "I-7 queue_no_duplicates violated for %s"
         (Patch_id.to_string a.patch_id))

(** I-8: needs_intervention blocks Respond. *)
let check_needs_intervention_blocks_respond orch action =
  match action with
  | Orchestrator.Respond (pid, _) ->
      let agent = Orchestrator.agent orch pid in
      if Patch_agent.needs_intervention agent then
        failwith
          (Printf.sprintf
             "I-8 needs_intervention_blocks_respond violated for %s"
             (Patch_id.to_string pid))
  | Orchestrator.Start (_, _) | Orchestrator.Rebase (_, _) -> ()

(** I-9: conflict not cleared while Merge_conflict is queued or in-flight. *)
let check_conflict_not_cleared_while_in_flight (a : Patch_agent.t) =
  let mc_queued =
    List.mem a.queue Operation_kind.Merge_conflict ~equal:Operation_kind.equal
  in
  let mc_inflight =
    Option.equal Operation_kind.equal a.current_op
      (Some Operation_kind.Merge_conflict)
  in
  if mc_queued || mc_inflight then
    if Patch_decision.should_clear_conflict a then
      failwith
        (Printf.sprintf
           "I-9 conflict_not_cleared_while_in_flight violated for %s"
           (Patch_id.to_string a.patch_id))

(** I-10: busy patches get no actions. *)
let check_busy_mutual_exclusion orch action =
  let pid = pid_of_action action in
  let agent = Orchestrator.agent orch pid in
  if agent.Patch_agent.busy then
    failwith
      (Printf.sprintf "I-10 busy_mutual_exclusion violated for %s"
         (Patch_id.to_string pid))

(** I-11: merged agents produce zero GitHub effects from reconcile_patch. *)
let check_merged_no_github_effects orch patches =
  let gameplan = make_gameplan patches in
  List.iter (Orchestrator.all_agents orch) ~f:(fun (a : Patch_agent.t) ->
      if a.merged then
        let patch =
          List.find gameplan.Gameplan.patches ~f:(fun (p : Patch.t) ->
              Patch_id.equal p.id a.patch_id)
        in
        match patch with
        | None -> ()
        | Some patch ->
            let _, effects =
              Patch_controller.reconcile_patch orch ~project_name:"test-project"
                ~gameplan ~patch
            in
            if not (List.is_empty effects) then
              failwith
                (Printf.sprintf
                   "I-11 merged_no_github_effects violated for %s: %d effects"
                   (Patch_id.to_string a.patch_id)
                   (List.length effects)))

(** I-12: notified_base_branch coherence — when has_session is true and the
    agent has not been rebased (notified_base_branch is Some), then
    notified_base_branch must have been set at start time. The field should only
    be None before the first Start. After Start, it should be Some. *)
let check_notified_base_branch_coherence (a : Patch_agent.t) =
  (* If the agent has started (has_session), notified_base_branch should be
     Some. If it hasn't started, both base_branch and notified_base_branch
     should be None. *)
  if a.has_session then (
    if Option.is_some a.base_branch && Option.is_none a.notified_base_branch
    then
      failwith
        (Printf.sprintf
           "I-12 notified_base_branch_coherence violated for %s: base_branch \
            is Some but notified_base_branch is None after session started"
           (Patch_id.to_string a.patch_id)))
  else if Option.is_some a.notified_base_branch then
    failwith
      (Printf.sprintf
         "I-12 notified_base_branch_coherence violated for %s: \
          notified_base_branch is Some but has_session is false"
         (Patch_id.to_string a.patch_id))

(** I-13: llm_session_id coherence — session ID is preserved through fallback
    escalation so the operator can resume after intervention. No constraint
    between session_fallback and llm_session_id. *)
let check_llm_session_id_coherence (_a : Patch_agent.t) = ()

(* ========== Combined check ========== *)

let merged_set_of orch =
  List.filter_map (Orchestrator.all_agents orch) ~f:(fun (a : Patch_agent.t) ->
      if a.merged then Some a.patch_id else None)
  |> Set.of_list (module Patch_id)

let check_all_invariants orch patches ~prev_merged ~curr_merged ~removed_pids =
  let agents = Orchestrator.all_agents orch in
  let actions =
    Patch_controller.plan_actions orch ~patches:(make_gameplan patches).patches
  in
  (* Per-agent invariants *)
  List.iter agents ~f:(fun a ->
      check_busy_implies_has_session a;
      check_ci_failure_count_non_negative a;
      check_queue_no_duplicates a;
      check_conflict_not_cleared_while_in_flight a;
      check_notified_base_branch_coherence a;
      check_llm_session_id_coherence a);
  (* Monotonicity *)
  check_merged_monotonicity ~prev_merged ~curr_merged ~removed_pids;
  (* Per-action invariants *)
  List.iter actions ~f:(fun action ->
      check_merged_blocks_actions orch action;
      check_priority_ordering orch action;
      check_needs_intervention_blocks_respond orch action;
      check_busy_mutual_exclusion orch action;
      check_base_branch_freshness orch patches action);
  (* Reconciliation invariants *)
  check_merged_no_github_effects orch patches

let removed_pids_of_cmd cmd prev_removed =
  match cmd with
  | Remove_adhoc i -> Set.add prev_removed (adhoc_pid i)
  | Add_adhoc _ | Apply_poll _ | Reconcile | Runner_tick | Complete _
  | Apply_session_result _ | Apply_rebase_result _ | Send_human_message _
  | Reset_intervention _ | Atomic_poll_reconcile _
  | Apply_conflict_rebase_result _ | Apply_rebase_push_result _ ->
      prev_removed

let run_sequence ?(debug = false) orch patches cmds =
  let final, _final_merged, _final_merged_logged, _final_removed =
    List.fold cmds
      ~init:
        ( orch,
          merged_set_of orch,
          Set.empty (module Patch_id),
          Set.empty (module Patch_id) )
      ~f:(fun (o, prev_merged, merged_logged, removed_pids) cmd ->
        if debug then Stdlib.Printf.eprintf "  CMD: %s\n%!" (show_command cmd);
        let o, log_info = apply_command_with_logs o patches cmd in
        let removed_pids = removed_pids_of_cmd cmd removed_pids in
        (* Re-adding an adhoc patch resets its lifecycle — clear merge log. *)
        let merged_logged =
          match cmd with
          | Add_adhoc i -> Set.remove merged_logged (adhoc_pid i)
          | Remove_adhoc _ | Apply_poll _ | Reconcile | Runner_tick | Complete _
          | Apply_session_result _ | Apply_rebase_result _
          | Send_human_message _ | Reset_intervention _
          | Atomic_poll_reconcile _ | Apply_conflict_rebase_result _
          | Apply_rebase_push_result _ ->
              merged_logged
        in
        let curr_merged = merged_set_of o in
        if debug then
          List.iter (Orchestrator.all_agents o) ~f:(fun (a : Patch_agent.t) ->
              Stdlib.Printf.eprintf
                "    %s: base=%s merged=%b busy=%b queue=[%s] ni=%b\n%!"
                (Patch_id.to_string a.patch_id)
                (Option.value_map a.base_branch ~default:"<none>"
                   ~f:Branch.to_string)
                a.merged a.busy
                (String.concat ~sep:","
                   (List.map a.queue ~f:Operation_kind.to_label))
                (Patch_agent.needs_intervention a));
        check_all_invariants o patches ~prev_merged ~curr_merged ~removed_pids;
        let merged_logged =
          match log_info with
          | Some info -> check_log_invariants info ~merged_logged
          | None -> merged_logged
        in
        (o, curr_merged, merged_logged, removed_pids))
  in
  final

(** Drain all busy agents by completing them, checking invariants after each.
    Models the runner's contract: every fired action must eventually complete
    without violating invariants. *)
let drain_and_check orch patches =
  let busy_pids =
    List.filter_map (Orchestrator.all_agents orch)
      ~f:(fun (a : Patch_agent.t) -> if a.busy then Some a.patch_id else None)
  in
  let no_removed = Set.empty (module Patch_id) in
  List.fold busy_pids ~init:orch ~f:(fun o pid ->
      let prev_merged = merged_set_of o in
      let o = Orchestrator.complete o pid in
      let curr_merged = merged_set_of o in
      check_all_invariants o patches ~prev_merged ~curr_merged
        ~removed_pids:no_removed;
      o)

let safe_verbose cmds patches f =
  try f () with
  | Failure msg ->
      if List.length cmds <= 100 then (
        Stdlib.Printf.eprintf "INVARIANT VIOLATION: %s\n%!" msg;
        let orch =
          if List.is_empty patches then
            Orchestrator.create ~patches ~main_branch:main
          else bootstrap patches
        in
        try ignore (run_sequence ~debug:true orch patches cmds)
        with Failure _ | Invalid_argument _ -> ());
      false
  | Invalid_argument msg ->
      if List.length cmds <= 100 then (
        Stdlib.Printf.eprintf "UNEXPECTED Invalid_argument: %s\n%!" msg;
        let orch =
          if List.is_empty patches then
            Orchestrator.create ~patches ~main_branch:main
          else bootstrap patches
        in
        try ignore (run_sequence ~debug:true orch patches cmds)
        with Failure _ | Invalid_argument _ -> ());
      false

(* ========== Properties ========== *)

(** PI-1: Random interleavings preserve all scheduling invariants. *)
let () =
  let n = 3 in
  let patches = mk_patches n in
  let prop_pi1 =
    QCheck2.Test.make
      ~name:"PI-1: random interleavings preserve scheduling invariants"
      ~count:1000 (gen_command_seq ~n ~len:50) (fun cmds ->
        (safe_verbose cmds patches) (fun () ->
            let orch = bootstrap patches in
            let final = run_sequence orch patches cmds in
            ignore (drain_and_check final patches);
            true))
  in
  QCheck2.Test.check_exn prop_pi1;
  Stdlib.print_endline "PI-1 passed"

(** PI-2: Atomic poll+reconcile preserves all scheduling invariants. *)
let () =
  let n = 3 in
  let patches = mk_patches n in
  let prop_pi2 =
    QCheck2.Test.make
      ~name:"PI-2: atomic poll+reconcile preserves scheduling invariants"
      ~count:1000 (gen_atomic_command_seq ~n ~len:50) (fun cmds ->
        (safe_verbose cmds patches) (fun () ->
            let orch = bootstrap patches in
            let final = run_sequence orch patches cmds in
            ignore (drain_and_check final patches);
            true))
  in
  QCheck2.Test.check_exn prop_pi2;
  Stdlib.print_endline "PI-2 passed"

(** PI-3: Fresh-start interleavings — agents begin without PRs, exercising the
    Start path and its failure modes. Safety invariants must hold throughout. *)
let () =
  let n = 3 in
  let patches = mk_patches n in
  let prop_pi3 =
    QCheck2.Test.make
      ~name:"PI-3: fresh-start interleavings preserve scheduling invariants"
      ~count:1000 (gen_command_seq ~n ~len:50) (fun cmds ->
        (safe_verbose cmds patches) (fun () ->
            let orch = Orchestrator.create ~patches ~main_branch:main in
            let final = run_sequence orch patches cmds in
            ignore (drain_and_check final patches);
            true))
  in
  QCheck2.Test.check_exn prop_pi3;
  Stdlib.print_endline "PI-3 passed"

(* ========== Liveness / convergence properties ========== *)

(** Helper: create a single-patch orchestrator with no PR, start the agent, and
    return (orch, patch_id). The agent is busy after this call. *)
let mk_started_no_pr () =
  let patches = mk_patches 1 in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let pid = pid_of_idx patches 0 in
  let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
  (orch, pid)

(** PI-4: Repeated worktree failure on a no-PR agent converges to
    needs_intervention. After 2 worktree failures (each producing
    complete_failed + on_worktree_failure), the agent must require intervention.
*)
let () =
  let prop_pi4 =
    QCheck2.Test.make
      ~name:"PI-4: repeated worktree failure converges to needs_intervention"
      (QCheck2.Gen.return ()) (fun () ->
        let orch, pid = mk_started_no_pr () in
        (* First worktree failure *)
        let orch =
          Orchestrator.apply_session_result orch pid
            Orchestrator.Session_worktree_missing
        in
        let a = Orchestrator.agent orch pid in
        if Patch_agent.needs_intervention a then
          failwith "needs_intervention too early after 1 failure";
        (* Re-start and fail again *)
        let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
        let orch =
          Orchestrator.apply_session_result orch pid
            Orchestrator.Session_worktree_missing
        in
        let a = Orchestrator.agent orch pid in
        Patch_agent.needs_intervention a)
  in
  QCheck2.Test.check_exn prop_pi4;
  Stdlib.print_endline "PI-4 passed"

(** PI-5: Repeated Session_process_error { is_fresh = true } on a no-PR agent
    converges to needs_intervention. This tests the same liveness guarantee as
    PI-4 but for the process-error path. *)
let () =
  let prop_pi5 =
    QCheck2.Test.make
      ~name:
        "PI-5: repeated process error (fresh) on no-PR converges to \
         needs_intervention" (QCheck2.Gen.return ()) (fun () ->
        let orch, pid = mk_started_no_pr () in
        (* First process error *)
        let orch =
          Orchestrator.apply_session_result orch pid
            (Orchestrator.Session_process_error { is_fresh = true })
        in
        let a = Orchestrator.agent orch pid in
        if Patch_agent.needs_intervention a then
          failwith "needs_intervention too early after 1 failure";
        (* Re-start and fail again *)
        let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
        let orch =
          Orchestrator.apply_session_result orch pid
            (Orchestrator.Session_process_error { is_fresh = true })
        in
        let a = Orchestrator.agent orch pid in
        Patch_agent.needs_intervention a)
  in
  QCheck2.Test.check_exn prop_pi5;
  Stdlib.print_endline "PI-5 passed"

(** Helper: create a single-patch orchestrator with a PR, idle. Returns (orch,
    patch_id). *)
let mk_bootstrapped () =
  let patches = mk_patches 1 in
  let orch = bootstrap patches in
  let pid = pid_of_idx patches 0 in
  (orch, pid, patches)

(** Simulate one full conflict-noop cycle: poll(conflict) → fire
    Respond(Merge_conflict) → apply_conflict_rebase_result(Noop) →
    apply_conflict_push_result(Conflict_resolved, Push_ok) → done.

    Noop means the local branch already contains the target, so there is no
    in-progress rebase to hand to the agent. We just push and complete. The push
    result is [Push_ok] because [force_push_with_lease] succeeds even when the
    rebase was a noop — it pushes the existing (conflicting) content. *)
let conflict_noop_cycle orch pid patches =
  let branch_of = branch_of_patches patches in
  let gameplan = make_gameplan patches in
  (* 1. Poll: conflict detected *)
  let poll_result =
    make_poll_result ~has_conflict:true ~merged:false ~ci_failed:false
      ~checks_passing:false ~review_comments:false
  in
  let observation =
    Patch_controller.
      {
        poll_result;
        base_branch = None;
        branch_in_root = false;
        worktree_path = None;
      }
  in
  let orch, _logs, _blocked =
    Patch_controller.apply_poll_result orch pid observation
  in
  (* 2. Reconcile + fire to make agent busy with Merge_conflict *)
  let orch, _effects, _actions =
    Patch_controller.tick orch ~project_name:"test-project" ~gameplan
  in
  (* 3. Apply Noop rebase result — agent is completed, not delivered to *)
  let has_merged dep_pid =
    (Orchestrator.agent orch dep_pid).Patch_agent.merged
  in
  let new_base =
    Graph.initial_base (Orchestrator.graph orch) pid ~has_merged ~branch_of
      ~main
  in
  let orch, decision, _effects =
    Orchestrator.apply_conflict_rebase_result orch pid Worktree.Noop new_base
  in
  (* 4. Apply push result — force-push succeeds even on noop rebase *)
  let orch, resolution =
    Orchestrator.apply_conflict_push_result orch pid decision
      (Some Worktree.Push_ok)
  in
  (* Conflict_resolved + Push_ok -> Conflict_done. Agent already completed. *)
  match resolution with
  | Orchestrator.Conflict_done -> orch
  | Orchestrator.Conflict_needs_agent | Orchestrator.Conflict_retry_push
  | Orchestrator.Conflict_give_up ->
      failwith "unexpected resolution in conflict_noop_cycle"

(** PI-6: Repeated Noop conflict rebase converges to needs_intervention. If the
    rebase keeps returning Noop (stale refs or agent unable to resolve), the
    system must eventually stop retrying. After 2 noop cycles,
    needs_intervention must trigger. *)
let () =
  let prop_pi6 =
    QCheck2.Test.make
      ~name:"PI-6: repeated conflict noop converges to needs_intervention"
      (QCheck2.Gen.return ()) (fun () ->
        let orch, pid, patches = mk_bootstrapped () in
        (* First noop cycle *)
        let orch = conflict_noop_cycle orch pid patches in
        let a = Orchestrator.agent orch pid in
        if Patch_agent.needs_intervention a then
          failwith "needs_intervention too early after 1 noop cycle";
        (* Second noop cycle *)
        let orch = conflict_noop_cycle orch pid patches in
        let a = Orchestrator.agent orch pid in
        Patch_agent.needs_intervention a)
  in
  QCheck2.Test.check_exn prop_pi6;
  Stdlib.print_endline "PI-6 passed"

(** PI-7: Rebase push failure converges to Merge_conflict retry. After a
    successful rebase whose push is rejected, the system must enqueue
    Merge_conflict and fire it on the next tick. *)
let () =
  let prop_pi7 =
    QCheck2.Test.make
      ~name:"PI-7: rebase push failure converges to merge-conflict retry"
      (QCheck2.Gen.return ()) (fun () ->
        let orch, pid, patches = mk_bootstrapped () in
        let branch_of = branch_of_patches patches in
        let gameplan = make_gameplan patches in
        (* 1. Poll with conflict so Merge_conflict gets enqueued *)
        let poll_result =
          make_poll_result ~has_conflict:true ~merged:false ~ci_failed:false
            ~checks_passing:false ~review_comments:false
        in
        let observation =
          Patch_controller.
            {
              poll_result;
              base_branch = None;
              branch_in_root = false;
              worktree_path = None;
            }
        in
        let orch, _logs, _blocked =
          Patch_controller.apply_poll_result orch pid observation
        in
        (* 2. Tick to fire the Merge_conflict → agent becomes busy *)
        let orch, _effects, _actions =
          Patch_controller.tick orch ~project_name:"test-project" ~gameplan
        in
        (* 3. Rebase succeeds *)
        let has_merged dep_pid =
          (Orchestrator.agent orch dep_pid).Patch_agent.merged
        in
        let new_base =
          Graph.initial_base (Orchestrator.graph orch) pid ~has_merged
            ~branch_of ~main
        in
        let orch, effects =
          Orchestrator.apply_rebase_result orch pid Worktree.Ok new_base
        in
        (* Verify Push_branch effect was emitted *)
        if
          not
            (List.equal Orchestrator.equal_rebase_effect effects
               [ Orchestrator.Push_branch ])
        then failwith "expected [Push_branch] effect";
        (* 4. Push fails *)
        let orch, resolution =
          Orchestrator.apply_rebase_push_result orch pid
            (Some Worktree.Push_rejected)
        in
        if
          not
            (Orchestrator.equal_rebase_push_resolution resolution
               Orchestrator.Rebase_push_failed)
        then failwith "expected Rebase_push_failed";
        let a = Orchestrator.agent orch pid in
        (* Verify: has_conflict set, Merge_conflict enqueued, agent idle *)
        if not a.Patch_agent.has_conflict then failwith "expected has_conflict";
        if
          not
            (List.mem a.Patch_agent.queue Operation_kind.Merge_conflict
               ~equal:Operation_kind.equal)
        then failwith "expected Merge_conflict in queue";
        if a.Patch_agent.busy then failwith "expected agent idle";
        (* 5. Next tick fires Merge_conflict for retry *)
        let orch, _effects, _actions =
          Patch_controller.tick orch ~project_name:"test-project" ~gameplan
        in
        let a = Orchestrator.agent orch pid in
        (* Agent is now busy handling Merge_conflict *)
        a.Patch_agent.busy)
  in
  QCheck2.Test.check_exn prop_pi7;
  Stdlib.print_endline "PI-7 passed"

(** PI-8: After a noop cycle, has_conflict is cleared and the next poll
    re-enqueues Merge_conflict. This ensures the system retries rather than
    getting stuck — repeated retries increment conflict_noop_count towards the
    intervention threshold (tested by PI-6). *)
let () =
  let prop_pi8 =
    QCheck2.Test.make
      ~name:
        "PI-8: noop cycle clears has_conflict; next poll re-enqueues \
         Merge_conflict" (QCheck2.Gen.return ()) (fun () ->
        let orch, pid, patches = mk_bootstrapped () in
        (* Run one full conflict-noop cycle *)
        let orch = conflict_noop_cycle orch pid patches in
        let a = Orchestrator.agent orch pid in
        (* After the cycle, has_conflict must be false — it purely tracks
           GitHub state, and the Noop path clears it. *)
        if a.Patch_agent.has_conflict then
          failwith
            "has_conflict is true after conflict_noop_cycle — should be cleared";
        (* Simulate the next poll with conflict still reported by GitHub *)
        let poll_result =
          make_poll_result ~has_conflict:true ~merged:false ~ci_failed:false
            ~checks_passing:false ~review_comments:false
        in
        let observation =
          Patch_controller.
            {
              poll_result;
              base_branch = None;
              branch_in_root = false;
              worktree_path = None;
            }
        in
        let orch, _logs, _blocked =
          Patch_controller.apply_poll_result orch pid observation
        in
        let a = Orchestrator.agent orch pid in
        (* Merge_conflict MUST be re-enqueued — this drives convergence *)
        List.mem a.Patch_agent.queue Operation_kind.Merge_conflict
          ~equal:Operation_kind.equal)
  in
  QCheck2.Test.check_exn prop_pi8;
  Stdlib.print_endline "PI-8 passed"

(** PI-9: Remove ad-hoc patch while busy, then complete — must not crash.
    Regression test for the with_busy_guard crash where Orchestrator.agent was
    called on a removed patch_id in the finally handler. *)
let () =
  let prop_pi9 =
    QCheck2.Test.make
      ~name:"PI-9: remove ad-hoc patch while busy does not crash complete/tick"
      (QCheck2.Gen.return ()) (fun () ->
        let patches = mk_patches 1 in
        let gameplan = make_gameplan patches in
        let orch = bootstrap patches in
        (* Add an ad-hoc patch *)
        let pid = adhoc_pid 0 in
        let orch =
          Orchestrator.add_agent orch ~patch_id:pid ~branch:(adhoc_branch 0)
            ~pr_number:(adhoc_pr 0)
        in
        (* Poll conflict to enqueue Merge_conflict on the ad-hoc patch *)
        let poll_result =
          make_poll_result ~has_conflict:true ~merged:false ~ci_failed:false
            ~checks_passing:false ~review_comments:false
        in
        let observation =
          Patch_controller.
            {
              poll_result;
              base_branch = None;
              branch_in_root = false;
              worktree_path = None;
            }
        in
        let orch, _logs, _blocked =
          Patch_controller.apply_poll_result orch pid observation
        in
        (* Tick to fire the Merge_conflict — ad-hoc agent becomes busy *)
        let orch, _effects, _actions =
          Patch_controller.tick orch ~project_name:"test-project" ~gameplan
        in
        let agent = Orchestrator.agent orch pid in
        if not agent.Patch_agent.busy then
          failwith "expected ad-hoc agent to be busy after tick";
        (* Remove the ad-hoc patch while it's busy — like the TUI handler *)
        let orch = Orchestrator.remove_agent orch pid in
        (* complete on the removed patch — this is what with_busy_guard does *)
        let orch = Orchestrator.complete orch pid in
        (* Subsequent tick must not crash *)
        let orch, _effects, _actions =
          Patch_controller.tick orch ~project_name:"test-project" ~gameplan
        in
        ignore (drain_and_check orch patches);
        true)
  in
  QCheck2.Test.check_exn prop_pi9;
  Stdlib.print_endline "PI-9 passed"

(** PI-10: Random interleavings with ad-hoc patches preserve invariants.
    Exercises Add_adhoc/Remove_adhoc commands alongside the standard vocabulary,
    including the case where ad-hoc patches are removed while busy. *)
let () =
  let n = 3 in
  let patches = mk_patches n in
  let prop_pi10 =
    QCheck2.Test.make
      ~name:"PI-10: interleavings with ad-hoc add/remove preserve invariants"
      ~count:1000 (gen_command_seq ~n ~len:50) (fun cmds ->
        (safe_verbose cmds patches) (fun () ->
            let orch = bootstrap patches in
            let final = run_sequence orch patches cmds in
            ignore (drain_and_check final patches);
            true))
  in
  QCheck2.Test.check_exn prop_pi10;
  Stdlib.print_endline "PI-10 passed"

(** PI-11: All-adhoc session — no gameplan patches, only ad-hoc additions. Tests
    that the system handles a completely dynamic patch set where every patch is
    added at runtime and can be removed at any point. All command indices
    [0..max_adhoc-1] map to ad-hoc patch slots. *)
let () =
  let patches = [] in
  let prop_pi11 =
    QCheck2.Test.make
      ~name:"PI-11: all-adhoc session with no gameplan preserves invariants"
      ~count:1000 (gen_command_seq ~n:0 ~len:50) (fun cmds ->
        (safe_verbose cmds patches) (fun () ->
            let orch = Orchestrator.create ~patches ~main_branch:main in
            let final = run_sequence orch patches cmds in
            ignore (drain_and_check final patches);
            true))
  in
  QCheck2.Test.check_exn prop_pi11;
  Stdlib.print_endline "PI-11 passed"
