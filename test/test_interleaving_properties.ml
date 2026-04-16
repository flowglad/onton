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
  | Discover_pr of int

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
  | Discover_pr i -> Printf.sprintf "Discover_pr(%d)" i

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
        map (fun i -> Discover_pr i) gen_idx;
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
        map (fun i -> Discover_pr i) gen_idx;
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
      match Orchestrator.find_agent orch pid with
      | Some agent
        when Option.equal Operation_kind.equal agent.Patch_agent.current_op
               (Some Operation_kind.Rebase) -> (
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
      | _ -> orch)
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
      match Orchestrator.find_agent orch pid with
      | Some agent
        when Option.equal Operation_kind.equal agent.Patch_agent.current_op
               (Some Operation_kind.Merge_conflict) -> (
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
            | Orchestrator.Deliver_to_agent -> orch'
            | Orchestrator.Conflict_resolved | Orchestrator.Conflict_failed ->
                orch'
          with Invalid_argument _ -> orch)
      | _ -> orch)
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
  | Discover_pr patch_idx -> (
      let pid = resolve_pid patches patch_idx in
      match Orchestrator.find_agent orch pid with
      | Some agent
        when agent.Patch_agent.has_session
             && (not (Patch_agent.has_pr agent))
             && not agent.Patch_agent.merged ->
          let pr = Pr_number.of_int (patch_idx + 50) in
          let has_merged dep_pid =
            match Orchestrator.find_agent orch dep_pid with
            | Some a -> a.Patch_agent.merged
            | None -> false
          in
          let base =
            try
              Graph.initial_base (Orchestrator.graph orch) pid ~has_merged
                ~branch_of:(extended_branch_of patches)
                ~main
            with Invalid_argument _ -> main
          in
          let orch = Orchestrator.set_pr_number orch pid pr in
          Orchestrator.set_base_branch orch pid base
      | _ -> orch)

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
  | Remove_adhoc _ | Discover_pr _ ->
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
      has_log_matching "Conflict cleared" info.logs
      || has_log_matching "Conflict flag retained" info.logs
    then
      failwith
        (Printf.sprintf "L-I-1 no_phantom_conflict_cleared violated for %s"
           (Patch_id.to_string info.patch_id))

(** L-I-2: Merged at most once per patch across the full sequence. *)
let check_merged_logged_at_most_once (info : poll_log_info) ~merged_logged =
  if has_log_matching "Merged" info.logs then
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
    if has_log_matching "Merge conflict detected" info.logs then
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

(** I-14: human messages are never silently lost. The total count of messages
    across human_messages + inflight_human_messages must be monotonically
    non-decreasing between steps, except when an explicit successful completion
    consumes them. [~successfully_delivered] is determined from the command that
    was applied, not inferred from state, so a failed delivery that drops
    inflight messages is not silently exempted. *)
let check_human_messages_preserved ~(prev : Patch_agent.t)
    ~(curr : Patch_agent.t) ~successfully_delivered =
  let prev_total =
    List.length prev.human_messages + List.length prev.inflight_human_messages
  in
  let curr_total =
    List.length curr.human_messages + List.length curr.inflight_human_messages
  in
  if (not successfully_delivered) && curr_total < prev_total then
    failwith
      (Printf.sprintf
         "I-14 human_messages_preserved violated for %s: total went from %d to \
          %d without successful delivery"
         (Patch_id.to_string curr.patch_id)
         prev_total curr_total)

(* ========== Combined check ========== *)

let merged_set_of orch =
  List.filter_map (Orchestrator.all_agents orch) ~f:(fun (a : Patch_agent.t) ->
      if a.merged then Some a.patch_id else None)
  |> Set.of_list (module Patch_id)

let check_all_invariants orch patches ~prev_agents ~prev_merged ~curr_merged
    ~removed_pids ~delivered_pid =
  let agents = Orchestrator.all_agents orch in
  let actions =
    Patch_controller.plan_actions orch ~patches:(make_gameplan patches).patches
  in
  (* Per-agent invariants *)
  List.iter agents ~f:(fun (a : Patch_agent.t) ->
      check_busy_implies_has_session a;
      check_ci_failure_count_non_negative a;
      check_queue_no_duplicates a;
      check_conflict_not_cleared_while_in_flight a;
      check_notified_base_branch_coherence a;
      check_llm_session_id_coherence a;
      (* I-14: human messages preserved — skip for removed/re-added patches *)
      match Map.find prev_agents a.Patch_agent.patch_id with
      | Some prev ->
          let successfully_delivered =
            match delivered_pid with
            | Some pid -> Patch_id.equal pid a.Patch_agent.patch_id
            | None -> false
          in
          check_human_messages_preserved ~prev ~curr:a ~successfully_delivered
      | None -> ());
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

let delivered_pid_of_cmd cmd patches =
  match cmd with
  | Complete patch_idx -> Some (resolve_pid patches patch_idx)
  | Apply_poll _ | Reconcile | Runner_tick | Apply_session_result _
  | Apply_rebase_result _ | Send_human_message _ | Reset_intervention _
  | Atomic_poll_reconcile _ | Apply_conflict_rebase_result _
  | Apply_rebase_push_result _ | Add_adhoc _ | Remove_adhoc _ | Discover_pr _ ->
      None

let removed_pids_of_cmd cmd prev_removed =
  match cmd with
  | Remove_adhoc i -> Set.add prev_removed (adhoc_pid i)
  | Add_adhoc _ | Apply_poll _ | Reconcile | Runner_tick | Complete _
  | Apply_session_result _ | Apply_rebase_result _ | Send_human_message _
  | Reset_intervention _ | Atomic_poll_reconcile _
  | Apply_conflict_rebase_result _ | Apply_rebase_push_result _ | Discover_pr _
    ->
      prev_removed

let run_sequence ?(debug = false) orch patches cmds =
  let final, _final_merged, _final_merged_logged, _final_removed, _final_agents
      =
    List.fold cmds
      ~init:
        ( orch,
          merged_set_of orch,
          Set.empty (module Patch_id),
          Set.empty (module Patch_id),
          Orchestrator.agents_map orch )
      ~f:(fun (o, prev_merged, merged_logged, removed_pids, prev_agents) cmd ->
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
          | Apply_rebase_push_result _ | Discover_pr _ ->
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
        let delivered_pid = delivered_pid_of_cmd cmd patches in
        check_all_invariants o patches ~prev_agents ~prev_merged ~curr_merged
          ~removed_pids ~delivered_pid;
        let merged_logged =
          match log_info with
          | Some info -> check_log_invariants info ~merged_logged
          | None -> merged_logged
        in
        let curr_agents = Orchestrator.agents_map o in
        (o, curr_merged, merged_logged, removed_pids, curr_agents))
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
      let prev_agents = Orchestrator.agents_map o in
      let prev_merged = merged_set_of o in
      let o = Orchestrator.complete o pid in
      let curr_merged = merged_set_of o in
      check_all_invariants o patches ~prev_agents ~prev_merged ~curr_merged
        ~removed_pids:no_removed ~delivered_pid:(Some pid);
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

(** PI-12: Human message liveness — send_human_message on an idle patch with a
    PR always produces a Respond(Human) action on the next tick, and after
    firing, the message is in inflight_human_messages. This is the end-to-end
    invariant that the bug (reading inflight instead of inbox on the pre-fire
    snapshot) violated. *)
let () =
  let prop_pi12 =
    QCheck2.Test.make
      ~name:
        "PI-12: human message on idle patch produces Respond(Human) with \
         inflight messages"
      ~count:500
      (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 100))
      (fun msg ->
        let orch, pid, patches = mk_bootstrapped () in
        let gameplan = make_gameplan patches in
        let orch = Orchestrator.send_human_message orch pid msg in
        (* Tick fires the action *)
        let orch, _effects, actions =
          Patch_controller.tick orch ~project_name:"test-project" ~gameplan
        in
        (* Must have produced Respond(_, Human) *)
        let has_human_respond =
          List.exists actions ~f:(fun action ->
              match action with
              | Orchestrator.Respond (p, k) ->
                  Patch_id.equal p pid
                  && Operation_kind.equal k Operation_kind.Human
              | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)
        in
        if not has_human_respond then
          failwith "no Respond(Human) action produced after send_human_message";
        let agent = Orchestrator.agent orch pid in
        (* Agent must be busy with inflight messages *)
        if not agent.Patch_agent.busy then failwith "agent not busy after tick";
        if List.is_empty agent.Patch_agent.inflight_human_messages then
          failwith "inflight_human_messages empty after firing Human action";
        if not (List.is_empty agent.Patch_agent.human_messages) then
          failwith "human_messages not empty after firing Human action";
        (* The message content must be preserved *)
        List.mem agent.Patch_agent.inflight_human_messages msg
          ~equal:String.equal)
  in
  QCheck2.Test.check_exn prop_pi12;
  Stdlib.print_endline "PI-12 passed"

(** PI-13: Human messages enqueued while a Pr_body session is in flight are
    re-delivered after the supervisor's push fails. This validates the
    Session_push_failed contract end-to-end at the interleaving level: the LLM
    session itself ran cleanly (so session_fallback is preserved), but
    [complete_failed] still restored any inflight human messages so the next
    iteration delivers them. Regression for the class of bug where a push
    failure during a non-Human phase silently drops human messages. *)
let () =
  let prop_pi13 =
    QCheck2.Test.make
      ~name:"PI-13: human messages survive Session_push_failed during Pr_body"
      ~count:300
      (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 80))
      (fun msg ->
        let orch, pid, _patches = mk_bootstrapped () in
        (* Reach a Pr_body-eligible state: PR exists, !pr_body_delivered.
           Enqueue Pr_body so the Respond action's precondition holds. *)
        let orch = Orchestrator.set_pr_body_delivered orch pid false in
        let orch = Orchestrator.enqueue orch pid Operation_kind.Pr_body in
        (* Begin a Pr_body session by firing the action — moves agent into
           busy with current_op = Pr_body. *)
        let orch =
          Orchestrator.fire orch
            (Orchestrator.Respond (pid, Operation_kind.Pr_body))
        in
        if not (Orchestrator.agent orch pid).Patch_agent.busy then
          failwith "agent not busy after firing Respond(Pr_body)";
        (* A human message arrives mid-session — supervisor enqueues it. *)
        let orch = Orchestrator.send_human_message orch pid msg in
        let pre = Orchestrator.agent orch pid in
        if not (List.mem pre.Patch_agent.human_messages msg ~equal:String.equal)
        then failwith "human message not in inbox after send";
        (* Session ends with Session_push_failed (LLM ok, push failed).
           apply_session_result clears session_fallback (LLM was healthy)
           but does NOT complete — completion is deferred to
           apply_respond_outcome via Respond_retry_push. *)
        let orch =
          Orchestrator.apply_session_result orch pid
            Orchestrator.Session_push_failed
        in
        let mid = Orchestrator.agent orch pid in
        if not mid.Patch_agent.busy then
          failwith
            "agent should stay busy after apply_session_result — completion is \
             deferred to apply_respond_outcome";
        (* Complete the full pipeline via apply_respond_outcome. The caller
           maps Session_push_failed to Respond_retry_push which calls plain
           [complete] — this does NOT restore inflight messages, preventing
           the infinite loop that occurred when complete_failed re-enqueued
           Human on every push failure. *)
        let orch =
          Orchestrator.apply_respond_outcome orch pid Operation_kind.Pr_body
            Orchestrator.Respond_retry_push
        in
        let post = Orchestrator.agent orch pid in
        if post.Patch_agent.busy then
          failwith "agent still busy after apply_respond_outcome";
        (* The crucial invariant: the human message is still pending.
           It was never inflight (it arrived mid-Pr_body, not mid-Human),
           so it lives in human_messages throughout. *)
        if
          not (List.mem post.Patch_agent.human_messages msg ~equal:String.equal)
        then failwith "human message lost after Session_push_failed";
        (* And session_fallback was reset to Fresh_available so the next
           iteration resumes the same session rather than burning its
           fallback budget on a push problem. *)
        Patch_agent.equal_session_fallback post.Patch_agent.session_fallback
          Patch_agent.Fresh_available)
  in
  QCheck2.Test.check_exn prop_pi13;
  Stdlib.print_endline "PI-13 passed"

(** PI-14: Two consecutive Session_no_commits promote to needs_intervention —
    without a pending Human in the queue, the counter crossing 2 is enough to
    stop the scheduler from re-enqueueing Start. Regression for the class of bug
    where an LLM produces no commits and the supervisor silently loops forever
    trying to push an empty branch.

    Session_no_commits defers completion to the caller (apply_start_outcome or
    apply_respond_outcome), so we model the full pipeline by calling [complete]
    after each [apply_session_result]. *)
let () =
  let prop_pi14 =
    QCheck2.Test.make
      ~name:"PI-14: two consecutive Session_no_commits flip needs_intervention"
      ~count:200 (QCheck2.Gen.return ()) (fun () ->
        let patches = mk_patches 1 in
        let orch = Orchestrator.create ~patches ~main_branch:main in
        let pid = pid_of_idx patches 0 in
        let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
        if not (Orchestrator.agent orch pid).Patch_agent.busy then
          failwith "agent not busy after Start";
        let orch =
          Orchestrator.apply_session_result orch pid
            Orchestrator.Session_no_commits
        in
        (* Completion is deferred by apply_session_result for Session_no_commits
           — exercise the real production path via apply_start_outcome. *)
        let orch =
          Orchestrator.apply_start_outcome orch pid Orchestrator.Start_failed
        in
        let a1 = Orchestrator.agent orch pid in
        if a1.Patch_agent.busy then
          failwith "agent still busy after first Session_no_commits + complete";
        if not (Int.equal a1.Patch_agent.no_commits_push_count 1) then
          failwith "counter not incremented after first Session_no_commits";
        if Patch_agent.needs_intervention a1 then
          failwith "needs_intervention fired too early (count = 1)";
        if
          not
            (Patch_agent.equal_session_fallback a1.Patch_agent.session_fallback
               Patch_agent.Fresh_available)
        then
          failwith
            "session_fallback was not preserved as Fresh_available (LLM itself \
             was healthy)";
        let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
        if not (Orchestrator.agent orch pid).Patch_agent.busy then
          failwith "agent not busy after re-Start";
        let orch =
          Orchestrator.apply_session_result orch pid
            Orchestrator.Session_no_commits
        in
        let orch =
          Orchestrator.apply_start_outcome orch pid Orchestrator.Start_failed
        in
        let a2 = Orchestrator.agent orch pid in
        if not (Int.equal a2.Patch_agent.no_commits_push_count 2) then
          failwith "counter not incremented to 2 after second no-commits";
        if not (Patch_agent.needs_intervention a2) then
          failwith "needs_intervention not flipped at count = 2";
        true)
  in
  QCheck2.Test.check_exn prop_pi14;
  Stdlib.print_endline "PI-14 passed"

(** PI-14b: A human message sent mid-session survives Session_no_commits — the
    message lives in [human_messages] (not inflight, since this is a Start
    session) and is untouched by [complete]. Note: while a Human is pending,
    needs_intervention is deliberately gated off (operator may need to respond
    first); the counter still increments and will surface once the Human queue
    is drained.

    Session_no_commits defers completion to the caller, so we model the full
    pipeline by calling [complete] after [apply_session_result]. *)
let () =
  let prop_pi14b =
    QCheck2.Test.make
      ~name:
        "PI-14b: human message survives Session_no_commits (deferred complete)"
      ~count:200
      (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 80))
      (fun msg ->
        let patches = mk_patches 1 in
        let orch = Orchestrator.create ~patches ~main_branch:main in
        let pid = pid_of_idx patches 0 in
        let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
        let orch = Orchestrator.send_human_message orch pid msg in
        let orch =
          Orchestrator.apply_session_result orch pid
            Orchestrator.Session_no_commits
        in
        (* Exercise the production path via apply_start_outcome so any
           future side-effects added to the Start_failed branch are
           observed by this test. *)
        let orch =
          Orchestrator.apply_start_outcome orch pid Orchestrator.Start_failed
        in
        let a = Orchestrator.agent orch pid in
        if a.Patch_agent.busy then
          failwith "agent still busy after Session_no_commits + complete";
        (* Message was in human_messages (not inflight), so plain [complete]
           does not clear it. *)
        if not (List.mem a.Patch_agent.human_messages msg ~equal:String.equal)
        then failwith "human message lost after Session_no_commits";
        if not (Int.equal a.Patch_agent.no_commits_push_count 1) then
          failwith "counter not incremented";
        (* With Human pending in queue, needs_intervention is gated off —
           deliberately. *)
        not (Patch_agent.needs_intervention a))
  in
  QCheck2.Test.check_exn prop_pi14b;
  Stdlib.print_endline "PI-14b passed"

(** PI-14c: Session_no_commits on the Respond(Human) path increments
    [no_commits_push_count] and preserves the pending human message content. The
    Start-path coverage in PI-14/PI-14b exercises the counter increment and
    message survival separately; this variant exercises both on the Respond path
    where [respond] moves [human_messages] into [inflight_human_messages] and
    the success path consumes them via plain [complete]. *)
let () =
  let prop_pi14c =
    QCheck2.Test.make
      ~name:
        "PI-14c: Session_no_commits on Respond(Human) increments counter and \
         drains inflight"
      ~count:200
      (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 80))
      (fun msg ->
        let orch, pid, _patches = mk_bootstrapped () in
        let orch = Orchestrator.send_human_message orch pid msg in
        let orch =
          Orchestrator.fire orch
            (Orchestrator.Respond (pid, Operation_kind.Human))
        in
        let pre = Orchestrator.agent orch pid in
        if List.is_empty pre.Patch_agent.inflight_human_messages then
          failwith "PI-14c: message should be inflight after fire(Human)";
        let orch =
          Orchestrator.apply_session_result orch pid
            Orchestrator.Session_no_commits
        in
        let orch =
          Orchestrator.apply_respond_outcome orch pid Operation_kind.Human
            Orchestrator.Respond_retry_push
        in
        let post = Orchestrator.agent orch pid in
        if post.Patch_agent.busy then
          failwith "PI-14c: agent still busy after Respond_retry_push";
        if not (Int.equal post.Patch_agent.no_commits_push_count 1) then
          failwith "PI-14c: no_commits_push_count not incremented to 1";
        (* The delivered message is consumed — not restored to the inbox.
           This is the invariant that prevents the infinite loop. *)
        if not (List.is_empty post.Patch_agent.inflight_human_messages) then
          failwith "PI-14c: inflight_human_messages not drained";
        if not (List.is_empty post.Patch_agent.human_messages) then
          failwith "PI-14c: delivered message incorrectly restored to inbox";
        (* Fallback preserved — LLM itself was healthy. *)
        Patch_agent.equal_session_fallback post.Patch_agent.session_fallback
          Patch_agent.Fresh_available)
  in
  QCheck2.Test.check_exn prop_pi14c;
  Stdlib.print_endline "PI-14c passed"

(** PI-15: Session_ok after a Session_no_commits cleanly resets the counter and
    intervention state — the agent recovers on its next healthy session and
    isn't stuck in a half-broken state. Regression for the bug where the counter
    leaks across successful sessions. *)
let () =
  let prop_pi15 =
    QCheck2.Test.make
      ~name:
        "PI-15: Session_ok after Session_no_commits resets \
         no_commits_push_count"
      ~count:100 (QCheck2.Gen.int_range 1 2) (fun n_no_commits ->
        let patches = mk_patches 1 in
        let orch = Orchestrator.create ~patches ~main_branch:main in
        let pid = pid_of_idx patches 0 in
        let orch =
          (* Fire Start -> Session_no_commits -> complete n times.
             Session_no_commits defers completion, so we call complete
             to model the full apply_start_outcome pipeline. *)
          List.fold (List.range 0 n_no_commits) ~init:orch ~f:(fun o _ ->
              let o = Orchestrator.fire o (Orchestrator.Start (pid, main)) in
              let o =
                Orchestrator.apply_session_result o pid
                  Orchestrator.Session_no_commits
              in
              Orchestrator.complete o pid)
        in
        if
          not
            (Int.equal
               (Orchestrator.agent orch pid).Patch_agent.no_commits_push_count
               n_no_commits)
        then failwith "counter did not accumulate as expected";
        (* Fire Start one more time and end with Session_ok. *)
        let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
        let orch =
          Orchestrator.apply_session_result orch pid Orchestrator.Session_ok
        in
        let a = Orchestrator.agent orch pid in
        (* Session_ok does NOT complete — agent stays busy per existing
           contract — but the counter MUST be zeroed. *)
        Int.equal a.Patch_agent.no_commits_push_count 0
        && not (Patch_agent.needs_intervention a))
  in
  QCheck2.Test.check_exn prop_pi15;
  Stdlib.print_endline "PI-15 passed"

(** PI-16: The retire-blue-green/patch-2 scenario end-to-end — a dependent patch
    whose PR gets auto-retargeted by GitHub when its dep merges and whose branch
    is never thereby rebased. The drift detector must catch it.

    Setup: two patches, patch b depends on patch a. Both bootstrap to having
    PRs; b's branch_rebased_onto is patch-a's branch (where it started). a
    merges (agent flagged merged, GitHub deletes a's branch and retargets b's PR
    to main — modeled by updating b.base_branch to main). The reconciler must
    enqueue a Rebase on b. *)
let () =
  let prop_pi16 =
    QCheck2.Test.make
      ~name:
        "PI-16: drift detector catches GitHub-auto-retargeted PR after dep \
         merge"
      ~count:1 (QCheck2.Gen.return ()) (fun () ->
        (* Build a linear 2-patch gameplan: b depends on a *)
        let a : Patch.t =
          {
            id = Patch_id.of_string "a";
            title = "a";
            description = "";
            branch = Branch.of_string "a";
            dependencies = [];
            spec = "";
            acceptance_criteria = [];
            files = [];
            classification = "";
            changes = [];
            test_stubs_introduced = [];
            test_stubs_implemented = [];
          }
        in
        let b : Patch.t =
          {
            a with
            id = Patch_id.of_string "b";
            title = "b";
            branch = Branch.of_string "b";
            dependencies = [ Patch_id.of_string "a" ];
          }
        in
        let patches = [ a; b ] in
        let orch = bootstrap patches in
        let pid_a = a.id and pid_b = b.id in
        (* Sanity: b started with base_branch = a's branch (per
           Graph.initial_base) and branch_rebased_onto set to same. *)
        let agent_b = Orchestrator.agent orch pid_b in
        if
          not
            (Option.equal Branch.equal agent_b.Patch_agent.branch_rebased_onto
               (Some a.branch))
        then
          failwith "patch b did not start with branch_rebased_onto = a's branch";
        (* a merges *)
        let orch = Orchestrator.mark_merged orch pid_a in
        (* GitHub auto-retargets b's PR to main (modeled by explicit
           set_base_branch on b). Simultaneously b's branch_rebased_onto is
           still pointing at a's branch because no Rebase has run yet. *)
        let orch = Orchestrator.set_base_branch orch pid_b main in
        (* Now run reconcile like the poll fiber does *)
        let agents = Orchestrator.all_agents orch in
        let branch_of = branch_of_patches patches in
        let patch_views =
          List.map agents ~f:(fun (ag : Patch_agent.t) ->
              Reconciler.
                {
                  id = ag.Patch_agent.patch_id;
                  has_pr = Patch_agent.has_pr ag;
                  merged = ag.Patch_agent.merged;
                  busy = ag.Patch_agent.busy;
                  needs_intervention = Patch_agent.needs_intervention ag;
                  branch_blocked = ag.Patch_agent.branch_blocked;
                  queue = ag.Patch_agent.queue;
                  base_branch =
                    Option.value ag.Patch_agent.base_branch ~default:main;
                  branch_rebased_onto = ag.Patch_agent.branch_rebased_onto;
                })
        in
        let merged_patches =
          List.filter_map agents ~f:(fun (ag : Patch_agent.t) ->
              if ag.Patch_agent.merged then Some ag.Patch_agent.patch_id
              else None)
        in
        let actions =
          Reconciler.reconcile ~graph:(Orchestrator.graph orch) ~main
            ~merged_pr_patches:merged_patches ~branch_of patch_views
        in
        (* The reconciler must emit Enqueue_rebase for b. *)
        List.exists actions ~f:(function
          | Reconciler.Enqueue_rebase p -> Patch_id.equal p pid_b
          | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> false))
  in
  QCheck2.Test.check_exn prop_pi16;
  Stdlib.print_endline "PI-16 passed"

(** PI-17: When a dependency merges while its dependent's session is running,
    re-deriving the base branch at PR-creation time yields [main], not the
    now-deleted dependency branch.

    Regression test for the spec-aware-review/patch-4 422: the Start message
    captured [base = dep-branch] at dispatch time, but by the time the session
    finished and the supervisor tried to create the PR, the dep had merged and
    its branch was deleted — causing a GitHub 422. The fix re-computes
    [Graph.initial_base] from fresh orchestrator state at PR creation time.

    The property generates random linear chains of 2-5 patches. For each
    dependent patch, it: 1. Fires Start (capturing the dep-branch as base) 2.
    Merges the dependency while the dependent is busy 3. Re-derives initial_base
    from current state 4. Asserts the fresh base is [main] (all deps now merged)
*)
let () =
  let prop_pi17 =
    QCheck2.Test.make
      ~name:
        "PI-17: fresh initial_base after dep merge yields main, not stale \
         dep-branch" ~count:300 (QCheck2.Gen.int_range 2 5) (fun n ->
        try
          let patches = mk_patches n in
          let branch_of = branch_of_patches patches in
          let orch = Orchestrator.create ~patches ~main_branch:main in
          (* Bootstrap: start and complete all patches in dependency order.
           Only patch 0 gets a PR; all dependents (patches 1..n-1) are left
           without one, so Start can fire for them again in each sub-scenario. *)
          let orch =
            List.foldi patches ~init:orch ~f:(fun i o _p ->
                let pid = pid_of_idx patches i in
                let has_merged dep_pid =
                  (Orchestrator.agent o dep_pid).Patch_agent.merged
                in
                let base =
                  Graph.initial_base (Orchestrator.graph o) pid ~has_merged
                    ~branch_of ~main
                in
                let o = Orchestrator.fire o (Orchestrator.Start (pid, base)) in
                let o =
                  if i = 0 then
                    Orchestrator.set_pr_number o pid (Pr_number.of_int 1)
                  else o
                in
                Orchestrator.complete o pid)
          in
          (* For each patch with a dependency, test the interleaving:
           1. Start the dependent (no PR — first session)
           2. Merge the immediate dependency while the dependent is busy
           3. Re-derive initial_base from fresh state
           4. Assert the fresh base is main (all deps of this patch merged) *)
          List.for_alli patches ~f:(fun i _p ->
              if i = 0 then true (* patch 0 has no deps *)
              else
                let dep_pid = pid_of_idx patches (i - 1) in
                let pid = pid_of_idx patches i in
                (* Merge all patches before the immediate dep so that
                 the only open dep is [dep_pid] *)
                let orch =
                  List.foldi patches ~init:orch ~f:(fun j o _p ->
                      if j < i - 1 then
                        let jpid = pid_of_idx patches j in
                        if not (Orchestrator.agent o jpid).Patch_agent.merged
                        then Orchestrator.mark_merged o jpid
                        else o
                      else o)
                in
                (* Verify the stale base is the dep's branch *)
                let has_merged_pre dep =
                  (Orchestrator.agent orch dep).Patch_agent.merged
                in
                let stale_base =
                  Graph.initial_base (Orchestrator.graph orch) pid
                    ~has_merged:has_merged_pre ~branch_of ~main
                in
                let dep_branch = branch_of dep_pid in
                if not (Branch.equal stale_base dep_branch) then
                  QCheck2.Test.fail_reportf
                    "PI-17 setup: expected stale_base=%s, got %s"
                    (Branch.to_string dep_branch)
                    (Branch.to_string stale_base);
                (* Fire Start to make the dependent busy (no PR → fire succeeds) *)
                let orch =
                  Orchestrator.fire orch (Orchestrator.Start (pid, stale_base))
                in
                let agent = Orchestrator.agent orch pid in
                if not agent.Patch_agent.busy then
                  QCheck2.Test.fail_reportf
                    "PI-17: patch %s should be busy after Start"
                    (Patch_id.to_string pid);
                (* Dependency merges while dependent is busy *)
                let orch = Orchestrator.mark_merged orch dep_pid in
                (* Re-derive initial_base from fresh state — this is what
                 the supervisor now does at PR creation time *)
                let has_merged_post dep =
                  (Orchestrator.agent orch dep).Patch_agent.merged
                in
                let fresh_base =
                  Graph.initial_base (Orchestrator.graph orch) pid
                    ~has_merged:has_merged_post ~branch_of ~main
                in
                if not (Branch.equal fresh_base main) then
                  QCheck2.Test.fail_reportf
                    "PI-17: after dep %s merged, fresh initial_base for %s \
                     should be main, got %s"
                    (Patch_id.to_string dep_pid)
                    (Patch_id.to_string pid)
                    (Branch.to_string fresh_base);
                true)
        with
        | QCheck2.Test.Test_fail _ as e -> raise e
        | _ -> false)
  in
  QCheck2.Test.check_exn prop_pi17;
  Stdlib.print_endline "PI-17 passed"

(* ================================================================== *)
(* Convergence properties (CV-1 .. CV-5)                               *)
(*                                                                     *)
(* These properties test TERMINATION: that repeatedly applying the     *)
(* full Respond(Human) pipeline under a persistent failure mode        *)
(* eventually drains the Human operation from the queue or reaches     *)
(* needs_intervention within a bounded number of iterations.           *)
(*                                                                     *)
(* The bug these guard against: complete_failed re-enqueues Human,     *)
(* the reconciler re-dispatches it, the same failure recurs, and the   *)
(* system loops forever.  Single-step invariant tests cannot catch     *)
(* this class of bug because every individual state is well-formed;    *)
(* only the infinite sequence is pathological.                         *)
(* ================================================================== *)

(** Determine whether the Respond(Human) pipeline has converged: no human
    payload is pending (queue, inbox, or inflight), OR the agent needs
    intervention. Checking all three pending-state fields — not just queue
    membership — guards against a regression where [Human] is dropped from the
    queue while [human_messages] or [inflight_human_messages] still holds
    undelivered content. *)
let converged orch pid =
  let a = Orchestrator.agent orch pid in
  let human_in_queue =
    List.mem a.Patch_agent.queue Operation_kind.Human
      ~equal:Operation_kind.equal
  in
  let pending_human =
    human_in_queue
    || (not (List.is_empty a.Patch_agent.human_messages))
    || not (List.is_empty a.Patch_agent.inflight_human_messages)
  in
  (not pending_human) || Patch_agent.needs_intervention a

(** Map a session_result to the respond_outcome the runner would produce. This
    mirrors the mapping in bin/main.ml (run_claude_and_handle →
    combine_session_and_push → respond_outcome). *)
let respond_outcome_of session_result =
  match session_result with
  | Orchestrator.Session_ok -> Orchestrator.Respond_ok
  | Orchestrator.Session_push_failed | Orchestrator.Session_no_commits ->
      Orchestrator.Respond_retry_push
  | Orchestrator.Session_process_error _ | Orchestrator.Session_no_resume
  | Orchestrator.Session_failed _ | Orchestrator.Session_give_up
  | Orchestrator.Session_worktree_missing ->
      Orchestrator.Respond_failed

(** Given the current agent state, produce the session_result that a
    persistent LLM failure would yield.  This models the session_mode →
    failure chain: Resume → Session_failed{is_fresh=false}, Fresh →
    Session_failed{is_fresh=true}, Give_up → Session_give_up. *)
let session_failure_for_state (a : Patch_agent.t) =
  match a.Patch_agent.session_fallback with
  | Patch_agent.Fresh_available -> (
      match a.Patch_agent.llm_session_id with
      | Some _ ->
          (* Would attempt Resume → fails *)
          Orchestrator.Session_failed { is_fresh = false }
      | None ->
          (* Would attempt Fresh → fails *)
          Orchestrator.Session_failed { is_fresh = true })
  | Patch_agent.Tried_fresh ->
      (* Would attempt Fresh → fails *)
      Orchestrator.Session_failed { is_fresh = true }
  | Patch_agent.Given_up -> Orchestrator.Session_give_up

(** Run one full Respond(Human) pipeline iteration: fire → apply_session_result
    → apply_respond_outcome. Returns None if the preconditions for
    fire(Respond(Human)) are not met (agent busy, merged, needs_intervention,
    Human not in queue, or not highest priority). Returns Some orch on success.
*)
let try_respond_human_pipeline orch pid session_result =
  let a = Orchestrator.agent orch pid in
  if
    a.Patch_agent.busy || a.Patch_agent.merged
    || Patch_agent.needs_intervention a
    || not
         (List.mem a.Patch_agent.queue Operation_kind.Human
            ~equal:Operation_kind.equal)
  then None
  else
    let hp = Patch_agent.highest_priority a in
    if not (Option.equal Operation_kind.equal hp (Some Operation_kind.Human))
    then None
    else
      let respond_outcome = respond_outcome_of session_result in
      let orch =
        Orchestrator.fire orch
          (Orchestrator.Respond (pid, Operation_kind.Human))
      in
      let orch = Orchestrator.apply_session_result orch pid session_result in
      let orch =
        Orchestrator.apply_respond_outcome orch pid Operation_kind.Human
          respond_outcome
      in
      Some orch

(** CV-1: Persistent push failure during Respond(Human) converges in 1
    iteration. The session succeeds (messages delivered) but the push fails.
    After the fix, inflight messages are NOT restored — Human drains from the
    queue immediately. *)
let () =
  let prop_cv1 =
    QCheck2.Test.make
      ~name:
        "CV-1: persistent Session_push_failed with Human converges in 1 \
         iteration"
      ~count:200
      (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 80))
      (fun msg ->
        let orch, pid, _patches = mk_bootstrapped () in
        let orch = Orchestrator.send_human_message orch pid msg in
        match
          try_respond_human_pipeline orch pid Orchestrator.Session_push_failed
        with
        | None -> QCheck2.Test.fail_report "CV-1: pipeline precondition failed"
        | Some orch ->
            if not (converged orch pid) then
              QCheck2.Test.fail_report
                "CV-1: Human still schedulable after 1 push-failure iteration";
            true)
  in
  QCheck2.Test.check_exn prop_cv1;
  Stdlib.print_endline "CV-1 passed"

(** CV-2: Persistent Session_no_commits during Respond(Human) converges in 1
    iteration. Same reasoning as CV-1 — session succeeded, no commits, but
    messages were delivered. *)
let () =
  let prop_cv2 =
    QCheck2.Test.make
      ~name:
        "CV-2: persistent Session_no_commits with Human converges in 1 \
         iteration"
      ~count:200
      (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 80))
      (fun msg ->
        let orch, pid, _patches = mk_bootstrapped () in
        let orch = Orchestrator.send_human_message orch pid msg in
        match
          try_respond_human_pipeline orch pid Orchestrator.Session_no_commits
        with
        | None -> QCheck2.Test.fail_report "CV-2: pipeline precondition failed"
        | Some orch ->
            if not (converged orch pid) then
              QCheck2.Test.fail_report
                "CV-2: Human still schedulable after 1 no-commits iteration";
            true)
  in
  QCheck2.Test.check_exn prop_cv2;
  Stdlib.print_endline "CV-2 passed"

(** CV-3: Persistent LLM session failure during Respond(Human) converges within
    4 iterations via the escalation chain: Resume fail → Tried_fresh → Fresh
    fail → Given_up → Give_up → intervention. Each iteration picks the
    session_result that matches the current session_mode, modeling a persistent
    LLM-side failure. *)
let () =
  let prop_cv3 =
    QCheck2.Test.make
      ~name:
        "CV-3: persistent session failure with Human converges within 4 \
         iterations"
      ~count:200
      (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 80))
      (fun msg ->
        let orch, pid, _patches = mk_bootstrapped () in
        let orch = Orchestrator.send_human_message orch pid msg in
        let max_iter = 4 in
        let rec loop orch iter =
          if converged orch pid then true
          else if iter >= max_iter then
            QCheck2.Test.fail_reportf
              "CV-3: not converged after %d iterations (session_fallback=%s)"
              max_iter
              (Patch_agent.show_session_fallback
                 (Orchestrator.agent orch pid).Patch_agent.session_fallback)
          else
            let a = Orchestrator.agent orch pid in
            let result = session_failure_for_state a in
            match try_respond_human_pipeline orch pid result with
            | None ->
                (* Can't fire — might already be converged via
                   needs_intervention, or Human is not highest priority. *)
                converged orch pid
            | Some orch -> loop orch (iter + 1)
        in
        loop orch 0)
  in
  QCheck2.Test.check_exn prop_cv3;
  Stdlib.print_endline "CV-3 passed"

(** CV-3b: Same as CV-3, but with a pre-existing [llm_session_id] so the first
    iteration takes the Resume→Fresh escalation path
    ([Session_failed {is_fresh=false}]) rather than Fresh-first.
    [mk_bootstrapped] leaves [llm_session_id = None], so CV-3 only exercises
    [Fresh fail → Given_up]; this variant covers the full
    [Resume fail → Tried_fresh → Fresh fail → Given_up] chain, which is the
    typical runtime ordering after a resumable session has been established. *)
let () =
  let prop_cv3b =
    QCheck2.Test.make
      ~name:
        "CV-3b: persistent failure starting from Resume path converges within \
         4 iterations"
      ~count:200
      (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 80))
      (fun msg ->
        let orch, pid, _patches = mk_bootstrapped () in
        let orch = Orchestrator.send_human_message orch pid msg in
        let orch =
          Orchestrator.set_llm_session_id orch pid (Some "cv-3b-session")
        in
        (* Sanity: the first failure should be a Resume failure. *)
        let a0 = Orchestrator.agent orch pid in
        let first = session_failure_for_state a0 in
        let is_resume_failure =
          Orchestrator.equal_session_result first
            (Orchestrator.Session_failed { is_fresh = false })
        in
        if not is_resume_failure then
          QCheck2.Test.fail_reportf
            "CV-3b: expected first failure on Resume path, got %s"
            (Orchestrator.show_session_result first);
        let max_iter = 4 in
        let rec loop orch iter =
          if converged orch pid then true
          else if iter >= max_iter then
            QCheck2.Test.fail_reportf
              "CV-3b: not converged after %d iterations (session_fallback=%s)"
              max_iter
              (Patch_agent.show_session_fallback
                 (Orchestrator.agent orch pid).Patch_agent.session_fallback)
          else
            let a = Orchestrator.agent orch pid in
            let result = session_failure_for_state a in
            match try_respond_human_pipeline orch pid result with
            | None -> converged orch pid
            | Some orch -> loop orch (iter + 1)
        in
        loop orch 0)
  in
  QCheck2.Test.check_exn prop_cv3b;
  Stdlib.print_endline "CV-3b passed"

(** CV-4: Session_give_up with inflight Human messages converges. The Give_up
    handler's complete_failed restores messages and re-enqueues Human, but
    needs_intervention now overrides the Human exemption for Given_up agents, so
    the reconciler stops scheduling and the agent surfaces for manual
    intervention.

    Note: send_human_message resets intervention state (by design — a new user
    message gives the agent a fresh chance). The loop scenario arises from
    INFLIGHT messages that complete_failed restores without resetting fallback.
*)
let () =
  let prop_cv4 =
    QCheck2.Test.make
      ~name:
        "CV-4: Session_give_up with inflight Human converges via \
         needs_intervention"
      ~count:200
      (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 80))
      (fun msg ->
        let orch, pid, _patches = mk_bootstrapped () in
        (* Send a message, then start delivering it. *)
        let orch = Orchestrator.send_human_message orch pid msg in
        let orch =
          Orchestrator.fire orch
            (Orchestrator.Respond (pid, Operation_kind.Human))
        in
        let a = Orchestrator.agent orch pid in
        if List.is_empty a.Patch_agent.inflight_human_messages then
          QCheck2.Test.fail_report
            "CV-4: message should be inflight after fire(Human)";
        (* Session escalates to Give_up (simulates the end of the
           Resume → Fresh → Give_up chain). *)
        let orch =
          Orchestrator.apply_session_result orch pid
            Orchestrator.Session_give_up
        in
        let orch =
          Orchestrator.apply_respond_outcome orch pid Operation_kind.Human
            Orchestrator.Respond_failed
        in
        let a = Orchestrator.agent orch pid in
        (* Messages should be restored by complete_failed. *)
        if List.is_empty a.Patch_agent.human_messages then
          QCheck2.Test.fail_report
            "CV-4: messages should be restored after Session_give_up";
        (* But needs_intervention should fire — Given_up overrides the
           Human exemption, preventing the infinite loop. *)
        if not (Patch_agent.needs_intervention a) then
          QCheck2.Test.fail_report
            "CV-4: Given_up + Human should trigger needs_intervention";
        true)
  in
  QCheck2.Test.check_exn prop_cv4;
  Stdlib.print_endline "CV-4 passed"

(** CV-5: Universal convergence — for ANY session_result category, the full
    Respond(Human) pipeline repeated up to 10 times either drains the queue or
    reaches needs_intervention. This is the catch-all property that would have
    detected the original Session_push_failed infinite loop.

    Two classes of session_result:

    (a) "Session succeeded" (Session_ok, Session_push_failed,
    Session_no_commits): the LLM ran. Messages were delivered. Converges in 1
    iteration because inflight messages are consumed.

    (b) "Session failed" (all others): the LLM could not run or crashed.
    Messages were NOT delivered. Converges via the escalation chain (Resume fail
    -> Fresh fail -> Give_up -> intervention). Each iteration adapts the result
    to the current session_mode, matching the production runner's behavior. *)
let () =
  let prop_cv5 =
    QCheck2.Test.make
      ~name:
        "CV-5: universal convergence — any session_result category terminates \
         within 10 iterations"
      ~count:500
      (QCheck2.Gen.pair
         (QCheck2.Gen.string_size (QCheck2.Gen.int_range 1 40))
         Onton_test_support.Test_generators.gen_session_result)
      (fun (msg, result_template) ->
        let orch, pid, _patches = mk_bootstrapped () in
        let orch = Orchestrator.send_human_message orch pid msg in
        let max_iter = 10 in
        (* For "session succeeded" results, use the template as-is.
           For "session failed" results, adapt to the current session_mode
           to model the real escalation chain. *)
        let is_success_result r =
          match r with
          | Orchestrator.Session_ok | Orchestrator.Session_push_failed
          | Orchestrator.Session_no_commits ->
              true
          | Orchestrator.Session_failed _ | Orchestrator.Session_process_error _
          | Orchestrator.Session_no_resume | Orchestrator.Session_give_up
          | Orchestrator.Session_worktree_missing ->
              false
        in
        let rec loop orch iter =
          if converged orch pid then true
          else if iter >= max_iter then
            QCheck2.Test.fail_reportf
              "CV-5: not converged after %d iterations with template %s"
              max_iter
              (Orchestrator.show_session_result result_template)
          else
            let a = Orchestrator.agent orch pid in
            (* For failure templates, use the generated template on the
               first iteration so variants like [Session_no_resume],
               [Session_process_error], and [Session_worktree_missing]
               actually exercise their [apply_session_result] branches.
               Subsequent iterations model the persistent-failure escalation
               chain via [session_failure_for_state] because those specific
               variants wouldn't normally recur (the state no longer
               matches). *)
            let result =
              if is_success_result result_template then result_template
              else if iter = 0 then result_template
              else session_failure_for_state a
            in
            match try_respond_human_pipeline orch pid result with
            | None ->
                if converged orch pid then true
                else
                  let queue = (Orchestrator.agent orch pid).Patch_agent.queue in
                  QCheck2.Test.fail_reportf
                    "CV-5: stuck — Human not fireable and not converged (iter \
                     %d, template %s, actual %s, queue=[%s])"
                    iter
                    (Orchestrator.show_session_result result_template)
                    (Orchestrator.show_session_result result)
                    (String.concat ~sep:","
                       (List.map queue ~f:Operation_kind.show))
            | Some orch -> loop orch (iter + 1)
        in
        loop orch 0)
  in
  QCheck2.Test.check_exn prop_cv5;
  Stdlib.print_endline "CV-5 passed"
