(* @archlint.module stateTest
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core
open Onton_core.Types

(** State machine command sequence properties (P1–P7).

    These properties exercise multi-step command sequences on the orchestrator
    and patch agent state machines, using the Invariants module as a
    postcondition oracle after every transition. *)

(* -- Helpers -- *)

let main = Branch.of_string "main"
let make_gameplan = Onton_test_support.Test_generators.make_test_gameplan
let mk_patches = Onton_test_support.Test_generators.mk_linear_patches

(** Generate a random command to apply to the orchestrator. Commands model the
    external events that the runner can produce. *)
type command =
  | Tick
  | Complete of int (* patch index *)
  | Enqueue of int * Operation_kind.t
  | Mark_merged of int
  | Set_session_failed of int
  | Increment_ci_failure of int
  | Reset_intervention_state of int
[@@deriving show]

let gen_command ~n =
  if n <= 0 then invalid_arg "gen_command: n must be positive";
  QCheck2.Gen.(
    let gen_idx = int_range 0 (n - 1) in
    oneof
      [
        pure Tick;
        map (fun i -> Complete i) gen_idx;
        map2
          (fun i k -> Enqueue (i, k))
          gen_idx Onton_test_support.Test_generators.gen_operation_kind;
        map (fun i -> Mark_merged i) gen_idx;
        map (fun i -> Set_session_failed i) gen_idx;
        map (fun i -> Increment_ci_failure i) gen_idx;
        map (fun i -> Reset_intervention_state i) gen_idx;
      ])

let gen_command_seq ~n ~len =
  if len < 1 then invalid_arg "gen_command_seq: len must be at least 1";
  QCheck2.Gen.(list_size (int_range 1 len) (gen_command ~n))

let pid_of_idx = Onton_test_support.Test_generators.pid_of_idx

(** Apply a command to the orchestrator, swallowing precondition failures
    (Invalid_argument) since random sequences will hit impossible transitions.
*)
let apply_command orch patches cmd =
  try
    match cmd with
    | Tick ->
        let orch, _effects, _actions =
          Patch_controller.tick orch ~project_name:"test-project"
            ~gameplan:(make_gameplan patches)
        in
        orch
    | Complete i -> Orchestrator.complete orch (pid_of_idx patches i)
    | Enqueue (i, k) -> Orchestrator.enqueue orch (pid_of_idx patches i) k
    | Mark_merged i -> Orchestrator.mark_merged orch (pid_of_idx patches i)
    | Set_session_failed i ->
        Orchestrator.set_session_failed orch (pid_of_idx patches i)
    | Increment_ci_failure i ->
        Orchestrator.increment_ci_failure_count orch (pid_of_idx patches i)
    | Reset_intervention_state i ->
        Orchestrator.reset_intervention_state orch (pid_of_idx patches i)
  with Invalid_argument _ -> orch

(** Check per-agent invariants mirroring [Invariants.all_checks].

    The canonical [Invariants] module operates on [State.t] which is not
    available at the orchestrator test level. These checks replicate the same
    conditions against [Patch_agent.t] directly so they stay in sync as
    invariants evolve. When adding a new check to [lib/invariants.ml], add the
    corresponding agent-level check here. *)
let check_agent_invariants (a : Patch_agent.t) =
  (* Mirrors Invariants.check_busy_implies_has_session *)
  if a.busy && not a.has_session then
    failwith
      (Printf.sprintf "busy_implies_has_session violated for %s"
         (Patch_id.to_string a.patch_id));
  (* Mirrors Invariants.check_ci_failure_count_non_negative *)
  if a.ci_failure_count < 0 then
    failwith
      (Printf.sprintf "ci_failure_count_non_negative violated for %s"
         (Patch_id.to_string a.patch_id));
  (* notified_base_branch coherence: after session starts, if base_branch is
     Some then notified_base_branch must also be Some. Before any session,
     notified_base_branch must be None. *)
  if a.has_session then (
    if Option.is_some a.base_branch && Option.is_none a.notified_base_branch
    then
      failwith
        (Printf.sprintf
           "notified_base_branch_coherence violated for %s: base_branch is \
            Some but notified_base_branch is None"
           (Patch_id.to_string a.patch_id)))
  else if Option.is_some a.notified_base_branch then
    failwith
      (Printf.sprintf
         "notified_base_branch_coherence violated for %s: notified_base_branch \
          is Some but has_session is false"
         (Patch_id.to_string a.patch_id))

(** Run a command sequence and check invariants after every step. Returns the
    final orchestrator state or raises on invariant violation. *)
let run_sequence orch patches cmds =
  List.fold cmds ~init:orch ~f:(fun o cmd ->
      let o = apply_command o patches cmd in
      List.iter (Orchestrator.all_agents o) ~f:check_agent_invariants;
      o)

(** Wrap a property predicate so unexpected exceptions falsify cleanly instead
    of aborting the QCheck run. [Invalid_argument] from precondition violations
    is expected in random sequences; all other exceptions indicate real bugs but
    should still produce a QCheck counterexample rather than a crash. *)
let safe f = try f () with _ -> false

(* ========== Properties ========== *)

(** P1: Random command sequences never violate invariants. The core "no crash"
    property — any sequence of valid and invalid commands applied to a fresh
    orchestrator preserves structural invariants. *)
let () =
  let n = 4 in
  let patches = mk_patches n in
  let prop_p1 =
    QCheck2.Test.make ~name:"P1: random sequences preserve invariants"
      ~count:500 (gen_command_seq ~n ~len:30) (fun cmds ->
        let orch = Orchestrator.create ~patches ~main_branch:main in
        let _final = run_sequence orch patches cmds in
        true)
  in
  QCheck2.Test.check_exn prop_p1;
  Stdlib.print_endline "P1 passed"

(** P2: Tick is idempotent on a quiescent state. After running tick until no new
    Start actions fire, an additional tick produces no Start actions. *)
let () =
  let prop_p2 =
    QCheck2.Test.make ~name:"P2: tick idempotent after convergence" ~count:300
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        safe (fun () ->
            let orch = Orchestrator.create ~patches ~main_branch:main in
            (* |patches|+2 ticks suffices for convergence: in a linear chain
               each tick starts one patch; in a DAG, multiple patches with
               satisfied deps start per tick, converging even faster. *)
            let rec stabilize o n =
              if n = 0 then o
              else
                let o, _effects, _actions =
                  Patch_controller.tick o ~project_name:"test-project"
                    ~gameplan:(make_gameplan patches)
                in
                stabilize o (n - 1)
            in
            let stable = stabilize orch (List.length patches + 2) in
            let _, _effects, actions1 =
              Patch_controller.tick stable ~project_name:"test-project"
                ~gameplan:(make_gameplan patches)
            in
            let starts1 =
              List.count actions1 ~f:(function
                | Orchestrator.Start _ -> true
                | Orchestrator.Respond _ | Orchestrator.Rebase _ -> false)
            in
            starts1 = 0))
  in
  QCheck2.Test.check_exn prop_p2;
  Stdlib.print_endline "P2 passed"

(** P3: Start → Complete → Enqueue → Tick produces Respond. The canonical
    lifecycle: after a patch is started and completed, enqueuing an operation
    and ticking must fire a Respond for that operation. *)
let () =
  let prop_p3 =
    QCheck2.Test.make ~name:"P3: start-complete-enqueue-tick yields respond"
      ~count:300
      QCheck2.Gen.(
        pair Onton_test_support.Test_generators.gen_patch_list_unique
          Onton_test_support.Test_generators.gen_operation_kind)
      (fun (patches, kind) ->
        safe (fun () ->
            match patches with
            | [] -> true
            | first :: _ ->
                let pid = first.Patch.id in
                let orch = Orchestrator.create ~patches ~main_branch:main in
                let rec tick_all o n =
                  if n = 0 then o
                  else
                    let o, _effects, _actions =
                      Patch_controller.tick o ~project_name:"test-project"
                        ~gameplan:(make_gameplan patches)
                    in
                    tick_all o (n - 1)
                in
                let orch = tick_all orch (List.length patches + 1) in
                let orch =
                  Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1)
                in
                (* Mark the post-PR-creation phases done so the controller
                   doesn't auto-enqueue Pr_body / Implementation_notes and
                   shadow the [kind] under test (they have lower priority than
                   each other but higher than the explicitly-enqueued kind). *)
                let orch = Orchestrator.set_pr_body_delivered orch pid true in
                let orch = Orchestrator.complete orch pid in
                let orch = Orchestrator.enqueue orch pid kind in
                let _, _effects, actions =
                  Patch_controller.tick orch ~project_name:"test-project"
                    ~gameplan:(make_gameplan patches)
                in
                List.exists actions ~f:(function
                  | Orchestrator.Respond (p, k) ->
                      Patch_id.equal p pid && Operation_kind.equal k kind
                  | Orchestrator.Rebase (p, _) ->
                      Patch_id.equal p pid
                      && Operation_kind.equal kind Operation_kind.Rebase
                  | Orchestrator.Start _ -> false)))
  in
  QCheck2.Test.check_exn prop_p3;
  Stdlib.print_endline "P3 passed"

(** P4: Merged patches never produce actions. Once a patch is marked merged, no
    subsequent tick should fire Start or Respond for that patch, regardless of
    what operations are enqueued. *)
let () =
  let prop_p4 =
    QCheck2.Test.make ~name:"P4: merged patches produce no actions" ~count:300
      QCheck2.Gen.(
        pair Onton_test_support.Test_generators.gen_patch_list_unique
          Onton_test_support.Test_generators.gen_operation_kind_queue)
      (fun (patches, ops) ->
        safe (fun () ->
            match patches with
            | [] -> true
            | first :: _ ->
                let pid = first.Patch.id in
                let orch = Orchestrator.create ~patches ~main_branch:main in
                let orch, _effects, _actions =
                  Patch_controller.tick orch ~project_name:"test-project"
                    ~gameplan:(make_gameplan patches)
                in
                let orch =
                  Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1)
                in
                let orch = Orchestrator.complete orch pid in
                let orch = Orchestrator.mark_merged orch pid in
                let orch =
                  List.fold ops ~init:orch ~f:(fun o k ->
                      Orchestrator.enqueue o pid k)
                in
                let _, _effects, actions =
                  Patch_controller.tick orch ~project_name:"test-project"
                    ~gameplan:(make_gameplan patches)
                in
                not
                  (List.exists actions ~f:(function
                      | Orchestrator.Start (p, _)
                      | Orchestrator.Respond (p, _)
                      | Orchestrator.Rebase (p, _)
                      -> Patch_id.equal p pid))))
  in
  QCheck2.Test.check_exn prop_p4;
  Stdlib.print_endline "P4 passed"

(** P5: needs_intervention blocks Respond until cleared. When ci_failure_count
    >= 3 and a session completes, the patch enters needs_intervention. No
    Respond fires until clear_needs_intervention. *)
let () =
  let prop_p5 =
    QCheck2.Test.make
      ~name:"P5: needs_intervention blocks respond until cleared" ~count:300
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        safe (fun () ->
            match patches with
            | [] -> true
            | first :: _ ->
                let pid = first.Patch.id in
                let orch = Orchestrator.create ~patches ~main_branch:main in
                let orch, _effects, _actions =
                  Patch_controller.tick orch ~project_name:"test-project"
                    ~gameplan:(make_gameplan patches)
                in
                let orch =
                  Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1)
                in
                let orch = Orchestrator.set_session_failed orch pid in
                let orch = Orchestrator.set_tried_fresh orch pid in
                let orch = Orchestrator.complete orch pid in
                let agent = Orchestrator.agent orch pid in
                if not (Patch_agent.needs_intervention agent) then false
                else
                  let orch = Orchestrator.enqueue orch pid Operation_kind.Ci in
                  let _, _effects, actions =
                    Patch_controller.tick orch ~project_name:"test-project"
                      ~gameplan:(make_gameplan patches)
                  in
                  let blocked =
                    not
                      (List.exists actions ~f:(function
                        | Orchestrator.Respond (p, _) -> Patch_id.equal p pid
                        | Orchestrator.Start _ | Orchestrator.Rebase _ -> false))
                  in
                  let orch = Orchestrator.reset_intervention_state orch pid in
                  let _, _effects, actions2 =
                    Patch_controller.tick orch ~project_name:"test-project"
                      ~gameplan:(make_gameplan patches)
                  in
                  let unblocked =
                    List.exists actions2 ~f:(function
                      | Orchestrator.Respond (p, _) -> Patch_id.equal p pid
                      | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)
                  in
                  blocked && unblocked))
  in
  QCheck2.Test.check_exn prop_p5;
  Stdlib.print_endline "P5 passed"

(** P6: Enqueue is idempotent. Enqueuing the same operation kind twice does not
    change the queue or produce duplicate Respond actions. *)
let () =
  let prop_p6 =
    QCheck2.Test.make ~name:"P6: enqueue is idempotent" ~count:300
      QCheck2.Gen.(
        pair Onton_test_support.Test_generators.gen_patch_list_unique
          Onton_test_support.Test_generators.gen_operation_kind)
      (fun (patches, kind) ->
        safe (fun () ->
            match patches with
            | [] -> true
            | first :: _ ->
                let pid = first.Patch.id in
                let orch = Orchestrator.create ~patches ~main_branch:main in
                let orch, _effects, _actions =
                  Patch_controller.tick orch ~project_name:"test-project"
                    ~gameplan:(make_gameplan patches)
                in
                let orch =
                  Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1)
                in
                let orch = Orchestrator.complete orch pid in
                let orch1 = Orchestrator.enqueue orch pid kind in
                let orch2 = Orchestrator.enqueue orch1 pid kind in
                let a1 = Orchestrator.agent orch1 pid in
                let a2 = Orchestrator.agent orch2 pid in
                Patch_agent.equal a1 a2))
  in
  QCheck2.Test.check_exn prop_p6;
  Stdlib.print_endline "P6 passed"

(** P7: Priority ordering — Respond always picks highest-priority operation.
    When multiple operations are enqueued, tick fires the one with lowest
    priority value (Rebase < Human < Merge_conflict < Ci < Review_comments). *)
let () =
  let prop_p7 =
    QCheck2.Test.make ~name:"P7: respond picks highest priority" ~count:300
      QCheck2.Gen.(
        pair Onton_test_support.Test_generators.gen_patch_list_unique
          (list_size (int_range 2 5)
             Onton_test_support.Test_generators.gen_operation_kind))
      (fun (patches, ops) ->
        safe (fun () ->
            match patches with
            | [] -> true
            | first :: _ -> (
                let pid = first.Patch.id in
                let orch = Orchestrator.create ~patches ~main_branch:main in
                let orch, _effects, _actions =
                  Patch_controller.tick orch ~project_name:"test-project"
                    ~gameplan:(make_gameplan patches)
                in
                let orch =
                  Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1)
                in
                let orch = Orchestrator.complete orch pid in
                let orch =
                  List.fold ops ~init:orch ~f:(fun o k ->
                      Orchestrator.enqueue o pid k)
                in
                let agent = Orchestrator.agent orch pid in
                let expected_hp = Patch_agent.highest_priority agent in
                let _, _effects, actions =
                  Patch_controller.tick orch ~project_name:"test-project"
                    ~gameplan:(make_gameplan patches)
                in
                let responded_kind =
                  List.find_map actions ~f:(function
                    | Orchestrator.Respond (p, k) ->
                        if Patch_id.equal p pid then Some k else None
                    | Orchestrator.Rebase (p, _) ->
                        if Patch_id.equal p pid then Some Operation_kind.Rebase
                        else None
                    | Orchestrator.Start _ -> None)
                in
                match (expected_hp, responded_kind) with
                | Some hp, Some rk -> Operation_kind.equal hp rk
                | None, None -> true
                | _ -> false)))
  in
  QCheck2.Test.check_exn prop_p7;
  Stdlib.print_endline "P7 passed"

(** P8: Enqueue Rebase → tick yields Rebase action (not Respond). *)
let () =
  let prop_p8 =
    QCheck2.Test.make ~name:"P8: enqueue Rebase yields Rebase action" ~count:300
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        safe (fun () ->
            match patches with
            | [] -> true
            | first :: _ ->
                let pid = first.Patch.id in
                let orch = Orchestrator.create ~patches ~main_branch:main in
                let rec tick_all o n =
                  if n = 0 then o
                  else
                    let o, _effects, _actions =
                      Patch_controller.tick o ~project_name:"test-project"
                        ~gameplan:(make_gameplan patches)
                    in
                    tick_all o (n - 1)
                in
                let orch = tick_all orch (List.length patches + 1) in
                let orch =
                  Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1)
                in
                let orch = Orchestrator.complete orch pid in
                let orch =
                  Orchestrator.enqueue orch pid Operation_kind.Rebase
                in
                let _, _effects, actions =
                  Patch_controller.tick orch ~project_name:"test-project"
                    ~gameplan:(make_gameplan patches)
                in
                List.exists actions ~f:(function
                  | Orchestrator.Rebase (p, _) -> Patch_id.equal p pid
                  | Orchestrator.Respond _ | Orchestrator.Start _ -> false)))
  in
  QCheck2.Test.check_exn prop_p8;
  Stdlib.print_endline "P8 passed"

(* ========== Force-complete properties (P9–P14) ==========

   Cover [Orchestrator.apply_force_complete], the pure decision invoked when
   a runner fiber exits abnormally (cancellation or unhandled exception)
   while [busy]. The decision must:
   - never silently drop [inflight_human_messages] (regression: a delivered
     human instruction was lost in the id-brands run on 2026-04-29)
   - re-enqueue [Operation_kind.Human] iff the inflight slot was non-empty
   - clear [busy], [current_op], [current_message_id], and the inflight slot
   - advance [session_fallback] iff [reason = Unexpected_exception] *)

let gen_force_complete_reason =
  QCheck2.Gen.oneof_list Orchestrator.[ Cancelled; Unexpected_exception ]

(** Build an orchestrator + patch_id where the agent is busy with the given
    operation kind, optionally carrying inflight human messages (only when
    [kind = Human]; messages are ignored for other kinds since they would
    enqueue Human, contaminating the queue with a higher-priority operation and
    breaking [Patch_agent.respond]'s "k == highest_priority" precondition).
    Mirrors the live [accept_message → fire(Respond k)] code path so the
    inflight slot is populated by the same [Patch_agent.respond] used in
    production.

    The setup deliberately skips [tick_all] reconciliation: those passes enqueue
    arbitrary higher-priority operations (e.g. Rebase) that would similarly fail
    the highest-priority precondition. We construct the minimal idle-with-PR
    state by hand and enqueue only the kind under test, so [fire] always
    satisfies its preconditions and the property runs on every input rather than
    vacuously passing on setup failures. *)
let make_busy_orch ~patches ~kind ~messages =
  match patches with
  | [] -> None
  | first :: _ ->
      let pid = first.Patch.id in
      let orch = Orchestrator.create ~patches ~main_branch:main in
      let orch = Orchestrator.set_pr_number orch pid (Pr_number.of_int 1) in
      let orch = Orchestrator.set_pr_body_delivered orch pid true in
      let orch =
        match kind with
        | Operation_kind.Human ->
            (* [send_human_message] both appends to [human_messages] and
               enqueues [Human], so no separate [enqueue] is needed. *)
            List.fold messages ~init:orch ~f:(fun acc msg ->
                Orchestrator.send_human_message acc pid msg)
        | Operation_kind.Rebase | Operation_kind.Merge_conflict
        | Operation_kind.Ci | Operation_kind.Review_comments
        | Operation_kind.Findings | Operation_kind.Pr_body ->
            Orchestrator.enqueue orch pid kind
      in
      let orch =
        if
          Operation_kind.equal kind Operation_kind.Human
          && List.is_empty messages
        then Orchestrator.enqueue orch pid Operation_kind.Human
        else orch
      in
      Some (Orchestrator.fire orch (Orchestrator.Respond (pid, kind)), pid)

let count_messages (a : Patch_agent.t) =
  List.length a.human_messages + List.length a.inflight_human_messages

(** P9: message preservation — for any busy agent and any reason, the total of
    [human_messages + inflight_human_messages] never decreases. Strengthens the
    existing I-14 (Human messages never lost) at the unit level. *)
let () =
  let prop =
    QCheck2.Test.make ~name:"P9: force_complete preserves human messages"
      ~count:500
      QCheck2.Gen.(
        triple Onton_test_support.Test_generators.gen_patch_list_unique
          Onton_test_support.Test_generators.gen_feedback_kind
          (let* msgs =
             list_size (int_range 0 4)
               (string_size ~gen:printable (int_range 1 40))
           in
           pair (return msgs) gen_force_complete_reason))
      (fun (patches, kind, (messages, reason)) ->
        safe (fun () ->
            (* Inflight is only populated when fire(Human) consumes
               [human_messages]. For non-Human kinds the messages remain in
               [human_messages] and the property is trivial; we still exercise
               the path to confirm no field is mishandled. *)
            match make_busy_orch ~patches ~kind ~messages with
            | None -> true
            | Some (orch, pid) ->
                let before = Orchestrator.agent orch pid in
                let orch' = Orchestrator.apply_force_complete orch pid reason in
                let after = Orchestrator.agent orch' pid in
                count_messages after >= count_messages before))
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "P9 passed"

(** P10: post-state idle — when the agent was busy before, it is no longer busy
    after, [current_op] and [current_message_id] are cleared, and the inflight
    slot is empty (its contents have been moved to [human_messages] when
    non-empty). *)
let () =
  let prop =
    QCheck2.Test.make ~name:"P10: force_complete clears busy state" ~count:300
      QCheck2.Gen.(
        triple Onton_test_support.Test_generators.gen_patch_list_unique
          Onton_test_support.Test_generators.gen_feedback_kind
          gen_force_complete_reason)
      (fun (patches, kind, reason) ->
        safe (fun () ->
            match make_busy_orch ~patches ~kind ~messages:[ "msg" ] with
            | None -> true
            | Some (orch, pid) ->
                let orch' = Orchestrator.apply_force_complete orch pid reason in
                let after = Orchestrator.agent orch' pid in
                (not after.busy)
                && Option.is_none after.current_op
                && Option.is_none after.current_message_id
                && List.is_empty after.inflight_human_messages))
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "P10 passed"

(** P11: Human re-enqueue iff inflight was non-empty. After force-complete,
    [Operation_kind.Human] is in the queue iff there was at least one inflight
    message at fire time. *)
let () =
  let prop =
    QCheck2.Test.make ~name:"P11: force_complete re-enqueues Human iff inflight"
      ~count:500
      QCheck2.Gen.(
        triple Onton_test_support.Test_generators.gen_patch_list_unique
          (let* msgs =
             list_size (int_range 0 3)
               (string_size ~gen:printable (int_range 1 40))
           in
           return msgs)
          gen_force_complete_reason)
      (fun (patches, messages, reason) ->
        safe (fun () ->
            (* Use Human as the kind so messages move into [inflight] on fire. *)
            match
              make_busy_orch ~patches ~kind:Operation_kind.Human ~messages
            with
            | None -> true
            | Some (orch, pid) ->
                let before = Orchestrator.agent orch pid in
                let orch' = Orchestrator.apply_force_complete orch pid reason in
                let after = Orchestrator.agent orch' pid in
                let had_inflight =
                  not (List.is_empty before.inflight_human_messages)
                in
                let human_in_queue =
                  List.mem after.queue Operation_kind.Human
                    ~equal:Operation_kind.equal
                in
                Bool.equal had_inflight human_in_queue))
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "P11 passed"

(** P12: session_fallback advances iff reason is Unexpected_exception. The
    [Cancelled] path leaves [session_fallback] unchanged; clean shutdown must
    not poison the fallback chain. [Unexpected_exception] runs both
    [set_session_failed] and [set_tried_fresh], pushing the agent two steps
    along the chain (Fresh_available → Tried_fresh → Given_up). *)
let () =
  let prop =
    QCheck2.Test.make ~name:"P12: session_fallback advances iff Unexpected"
      ~count:300
      QCheck2.Gen.(
        triple Onton_test_support.Test_generators.gen_patch_list_unique
          Onton_test_support.Test_generators.gen_feedback_kind
          gen_force_complete_reason)
      (fun (patches, kind, reason) ->
        safe (fun () ->
            match make_busy_orch ~patches ~kind ~messages:[] with
            | None -> true
            | Some (orch, pid) -> (
                let before = Orchestrator.agent orch pid in
                let orch' = Orchestrator.apply_force_complete orch pid reason in
                let after = Orchestrator.agent orch' pid in
                match reason with
                | Orchestrator.Cancelled ->
                    Patch_agent.equal_session_fallback before.session_fallback
                      after.session_fallback
                | Orchestrator.Unexpected_exception ->
                    let expected =
                      Patch_agent.set_tried_fresh
                        (Patch_agent.set_session_failed before)
                    in
                    Patch_agent.equal_session_fallback expected.session_fallback
                      after.session_fallback)))
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "P12 passed"

(** P13: applying force_complete twice in a row is the same as applying it once.
    After the first call the agent is idle and (for [Unexpected_exception])
    [session_fallback] has already advanced, so a second call must be the
    identity. Exercises both the [complete] path (inflight empty) and the
    [complete_failed] path (inflight non-empty) so a non-idempotence in either
    branch fails the property. *)
let () =
  let prop =
    QCheck2.Test.make ~name:"P13: force_complete on idle agent is identity"
      ~count:300
      QCheck2.Gen.(
        quad Onton_test_support.Test_generators.gen_patch_list_unique
          Onton_test_support.Test_generators.gen_feedback_kind
          gen_force_complete_reason
          (oneof_list [ []; [ "msg" ] ]))
      (fun (patches, kind, reason, messages) ->
        safe (fun () ->
            match make_busy_orch ~patches ~kind ~messages with
            | None -> true
            | Some (orch, pid) ->
                let orch1 = Orchestrator.apply_force_complete orch pid reason in
                let after1 = Orchestrator.agent orch1 pid in
                if after1.busy then false
                else
                  let orch2 =
                    Orchestrator.apply_force_complete orch1 pid reason
                  in
                  let after2 = Orchestrator.agent orch2 pid in
                  Patch_agent.equal after1 after2))
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "P13 passed"

(** P14: unknown patch is identity. [apply_force_complete] on a [patch_id] not
    present in the orchestrator must not raise and must leave the state
    untouched. *)
let () =
  let prop =
    QCheck2.Test.make ~name:"P14: force_complete on unknown patch is identity"
      ~count:200
      QCheck2.Gen.(
        triple Onton_test_support.Test_generators.gen_patch_list_unique
          Onton_test_support.Test_generators.gen_patch_id
          gen_force_complete_reason)
      (fun (patches, missing_pid, reason) ->
        safe (fun () ->
            (* Skip when the random pid happens to clash with one of the
               generated patches. *)
            if
              List.exists patches ~f:(fun (p : Patch.t) ->
                  Patch_id.equal p.Patch.id missing_pid)
            then true
            else
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch' =
                Orchestrator.apply_force_complete orch missing_pid reason
              in
              List.equal Patch_agent.equal
                (Orchestrator.all_agents orch)
                (Orchestrator.all_agents orch')))
  in
  QCheck2.Test.check_exn prop;
  Stdlib.print_endline "P14 passed"

let () = Stdlib.print_endline "all state machine properties passed"
