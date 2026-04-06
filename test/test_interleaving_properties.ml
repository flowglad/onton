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

type rebase_result_kind =
  | Rebase_ok
  | Rebase_noop
  | Rebase_conflict
  | Rebase_error

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

let show_rebase_result_kind = function
  | Rebase_ok -> "Ok"
  | Rebase_noop -> "Noop"
  | Rebase_conflict -> "Conflict"
  | Rebase_error -> "Error"

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
    [ Sess_ok; Sess_failed_fresh; Sess_failed_resume; Sess_give_up ]

let gen_rebase_result_kind =
  QCheck2.Gen.oneof_list
    [ Rebase_ok; Rebase_noop; Rebase_conflict; Rebase_error ]

let gen_command ~n =
  if n <= 0 then invalid_arg "gen_command: n must be positive";
  QCheck2.Gen.(
    let gen_idx = int_range 0 (n - 1) in
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
        map (fun i -> Send_human_message i) gen_idx;
        map (fun i -> Reset_intervention i) gen_idx;
      ])

let gen_command_seq ~n ~len =
  if len < 1 then invalid_arg "gen_command_seq: len must be at least 1";
  QCheck2.Gen.(list_size (int_range 1 len) (gen_command ~n))

let gen_atomic_command ~n =
  if n <= 0 then invalid_arg "gen_atomic_command: n must be positive";
  QCheck2.Gen.(
    let gen_idx = int_range 0 (n - 1) in
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
        map (fun i -> Send_human_message i) gen_idx;
        map (fun i -> Reset_intervention i) gen_idx;
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

let to_worktree_result = function
  | Rebase_ok -> Worktree.Ok
  | Rebase_noop -> Worktree.Noop
  | Rebase_conflict -> Worktree.Conflict
  | Rebase_error -> Worktree.Error "simulated error"

let poll_params_of_kind = function
  | Poll_normal -> (false, false, false, true, false)
  | Poll_conflict -> (true, false, false, false, false)
  | Poll_merged -> (false, true, false, false, false)
  | Poll_ci_failed -> (false, false, true, false, false)
  | Poll_checks_passing -> (false, false, false, true, false)
  | Poll_review_comments -> (false, false, false, true, true)

let rec apply_command orch patches cmd =
  let gameplan = make_gameplan patches in
  match cmd with
  | Apply_poll { patch_idx; poll_kind } -> (
      let pid = pid_of_idx patches patch_idx in
      let has_conflict, merged, ci_failed, checks_passing, review_comments =
        poll_params_of_kind poll_kind
      in
      let poll_result =
        make_poll_result ~has_conflict ~merged ~ci_failed ~checks_passing
          ~review_comments
      in
      let branch_of = branch_of_patches patches in
      let observation =
        Patch_controller.
          {
            poll_result;
            head_branch = Some (branch_of pid);
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
      let pid = pid_of_idx patches patch_idx in
      match Orchestrator.find_agent orch pid with
      | Some agent when agent.Patch_agent.busy -> Orchestrator.complete orch pid
      | _ -> orch)
  | Apply_session_result { patch_idx; result } ->
      let pid = pid_of_idx patches patch_idx in
      Orchestrator.apply_session_result orch pid (to_session_result result)
  | Apply_rebase_result { patch_idx; result } -> (
      let pid = pid_of_idx patches patch_idx in
      let branch_of = branch_of_patches patches in
      try
        let has_merged dep_pid =
          (Orchestrator.agent orch dep_pid).Patch_agent.merged
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
      let pid = pid_of_idx patches patch_idx in
      Orchestrator.send_human_message orch pid "test message"
  | Reset_intervention patch_idx ->
      let pid = pid_of_idx patches patch_idx in
      Orchestrator.reset_intervention_state orch pid
  | Atomic_poll_reconcile { patch_idx; poll_kind } ->
      let orch =
        apply_command orch patches (Apply_poll { patch_idx; poll_kind })
      in
      apply_command orch patches Reconcile
  | Apply_conflict_rebase_result { patch_idx; result } -> (
      let pid = pid_of_idx patches patch_idx in
      let branch_of = branch_of_patches patches in
      try
        let has_merged dep_pid =
          (Orchestrator.agent orch dep_pid).Patch_agent.merged
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
            (* Agent stays busy; simulate successful session + complete.
               Mirror the runner: clear_has_conflict before complete. *)
            let orch' =
              Orchestrator.apply_session_result orch' pid
                Orchestrator.Session_ok
            in
            let orch' = Orchestrator.clear_has_conflict orch' pid in
            Orchestrator.complete orch' pid
        | Orchestrator.Conflict_resolved | Orchestrator.Conflict_failed -> orch'
      with Invalid_argument _ -> orch)

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
      let pid = pid_of_idx patches patch_idx in
      let agent_before = Orchestrator.agent orch pid in
      let has_conflict, merged, ci_failed, checks_passing, review_comments =
        poll_params_of_kind poll_kind
      in
      let poll_result =
        make_poll_result ~has_conflict ~merged ~ci_failed ~checks_passing
          ~review_comments
      in
      let branch_of = branch_of_patches patches in
      let observation =
        Patch_controller.
          {
            poll_result;
            head_branch = Some (branch_of pid);
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
      with Invalid_argument _ -> (orch, None))
  | Atomic_poll_reconcile { patch_idx; poll_kind } ->
      let orch, info =
        apply_command_with_logs orch patches
          (Apply_poll { patch_idx; poll_kind })
      in
      let orch = apply_command orch patches Reconcile in
      (orch, info)
  | Reconcile | Runner_tick | Complete _ | Apply_session_result _
  | Apply_rebase_result _ | Send_human_message _ | Reset_intervention _
  | Apply_conflict_rebase_result _ ->
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

(** I-4: merged is monotonic — once merged, never un-merged. *)
let check_merged_monotonicity ~prev_merged ~curr_merged =
  Set.iter prev_merged ~f:(fun pid ->
      if not (Set.mem curr_merged pid) then
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

(* ========== Combined check ========== *)

let merged_set_of orch =
  List.filter_map (Orchestrator.all_agents orch) ~f:(fun (a : Patch_agent.t) ->
      if a.merged then Some a.patch_id else None)
  |> Set.of_list (module Patch_id)

let check_all_invariants orch patches ~prev_merged ~curr_merged =
  let agents = Orchestrator.all_agents orch in
  let actions =
    Patch_controller.plan_actions orch ~patches:(make_gameplan patches).patches
  in
  (* Per-agent invariants *)
  List.iter agents ~f:(fun a ->
      check_busy_implies_has_session a;
      check_ci_failure_count_non_negative a;
      check_queue_no_duplicates a;
      check_conflict_not_cleared_while_in_flight a);
  (* Monotonicity *)
  check_merged_monotonicity ~prev_merged ~curr_merged;
  (* Per-action invariants *)
  List.iter actions ~f:(fun action ->
      check_merged_blocks_actions orch action;
      check_priority_ordering orch action;
      check_needs_intervention_blocks_respond orch action;
      check_busy_mutual_exclusion orch action;
      check_base_branch_freshness orch patches action);
  (* Reconciliation invariants *)
  check_merged_no_github_effects orch patches

let run_sequence ?(debug = false) orch patches cmds =
  let _final, _final_merged, _final_merged_logged =
    List.fold cmds
      ~init:(orch, merged_set_of orch, Set.empty (module Patch_id))
      ~f:(fun (o, prev_merged, merged_logged) cmd ->
        if debug then Stdlib.Printf.eprintf "  CMD: %s\n%!" (show_command cmd);
        let o, log_info = apply_command_with_logs o patches cmd in
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
        check_all_invariants o patches ~prev_merged ~curr_merged;
        let merged_logged =
          match log_info with
          | Some info -> check_log_invariants info ~merged_logged
          | None -> merged_logged
        in
        (o, curr_merged, merged_logged))
  in
  ()

let safe_verbose cmds patches f =
  try f ()
  with Failure msg ->
    if List.length cmds <= 100 then (
      Stdlib.Printf.eprintf "INVARIANT VIOLATION: %s\n%!" msg;
      let orch = bootstrap patches in
      try run_sequence ~debug:true orch patches cmds with Failure _ -> ());
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
            run_sequence orch patches cmds;
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
            run_sequence orch patches cmds;
            true))
  in
  QCheck2.Test.check_exn prop_pi2;
  Stdlib.print_endline "PI-2 passed"
