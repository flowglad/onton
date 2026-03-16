open Base
open Onton
open Onton.Types

(** State machine command sequence properties (P1–P7).

    These properties exercise multi-step command sequences on the orchestrator
    and patch agent state machines, using the Invariants module as a
    postcondition oracle after every transition. *)

(* -- Helpers -- *)

let main = Branch.of_string "main"

let mk_patches n =
  List.init n ~f:(fun i ->
      let id = Patch_id.of_string (Printf.sprintf "p%d" i) in
      let branch = Branch.of_string (Printf.sprintf "b%d" i) in
      let dependencies =
        if i = 0 then []
        else [ Patch_id.of_string (Printf.sprintf "p%d" (i - 1)) ]
      in
      Patch.{ id; title = Printf.sprintf "Patch %d" i; branch; dependencies })

(** Generate a random command to apply to the orchestrator. Commands model the
    external events that the runner can produce. *)
type command =
  | Tick
  | Complete of int (* patch index *)
  | Enqueue of int * Operation_kind.t
  | Mark_merged of int
  | Set_session_failed of int
  | Increment_ci_failure of int
  | Clear_needs_intervention of int
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
        map (fun i -> Clear_needs_intervention i) gen_idx;
      ])

let gen_command_seq ~n ~len =
  QCheck2.Gen.(list_size (int_range 1 len) (gen_command ~n))

let pid_of_idx patches i =
  let (p : Patch.t) = List.nth_exn patches i in
  p.id

(** Apply a command to the orchestrator, swallowing precondition failures
    (Invalid_argument) since random sequences will hit impossible transitions.
*)
let apply_command orch patches cmd =
  try
    match cmd with
    | Tick ->
        let orch, _actions = Orchestrator.tick orch ~patches in
        orch
    | Complete i -> Orchestrator.complete orch (pid_of_idx patches i)
    | Enqueue (i, k) -> Orchestrator.enqueue orch (pid_of_idx patches i) k
    | Mark_merged i -> Orchestrator.mark_merged orch (pid_of_idx patches i)
    | Set_session_failed i ->
        Orchestrator.set_session_failed orch (pid_of_idx patches i)
    | Increment_ci_failure i ->
        Orchestrator.increment_ci_failure_count orch (pid_of_idx patches i)
    | Clear_needs_intervention i ->
        Orchestrator.clear_needs_intervention orch (pid_of_idx patches i)
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
            let rec stabilize o n =
              if n = 0 then o
              else
                let o, _ = Orchestrator.tick o ~patches in
                stabilize o (n - 1)
            in
            let stable = stabilize orch (List.length patches + 2) in
            let _, actions1 = Orchestrator.tick stable ~patches in
            let starts1 =
              List.count actions1 ~f:(function
                | Orchestrator.Start _ -> true
                | Orchestrator.Respond _ -> false)
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
                    let o, _ = Orchestrator.tick o ~patches in
                    tick_all o (n - 1)
                in
                let orch = tick_all orch (List.length patches + 1) in
                let orch = Orchestrator.complete orch pid in
                let orch = Orchestrator.enqueue orch pid kind in
                let _, actions = Orchestrator.tick orch ~patches in
                List.exists actions ~f:(function
                  | Orchestrator.Respond (p, k) ->
                      Patch_id.equal p pid && Operation_kind.equal k kind
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
                let orch, _ = Orchestrator.tick orch ~patches in
                let orch = Orchestrator.complete orch pid in
                let orch = Orchestrator.mark_merged orch pid in
                let orch =
                  List.fold ops ~init:orch ~f:(fun o k ->
                      Orchestrator.enqueue o pid k)
                in
                let _, actions = Orchestrator.tick orch ~patches in
                not
                  (List.exists actions ~f:(function
                      | Orchestrator.Start (p, _) | Orchestrator.Respond (p, _)
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
                let orch, _ = Orchestrator.tick orch ~patches in
                let orch = Orchestrator.increment_ci_failure_count orch pid in
                let orch = Orchestrator.increment_ci_failure_count orch pid in
                let orch = Orchestrator.increment_ci_failure_count orch pid in
                let orch = Orchestrator.complete orch pid in
                let agent = Orchestrator.agent orch pid in
                if not agent.Patch_agent.needs_intervention then true
                else
                  let orch = Orchestrator.enqueue orch pid Operation_kind.Ci in
                  let _, actions = Orchestrator.tick orch ~patches in
                  let blocked =
                    not
                      (List.exists actions ~f:(function
                        | Orchestrator.Respond (p, _) -> Patch_id.equal p pid
                        | Orchestrator.Start _ -> false))
                  in
                  let orch = Orchestrator.clear_needs_intervention orch pid in
                  let _, actions2 = Orchestrator.tick orch ~patches in
                  let unblocked =
                    List.exists actions2 ~f:(function
                      | Orchestrator.Respond (p, _) -> Patch_id.equal p pid
                      | Orchestrator.Start _ -> false)
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
                let orch, _ = Orchestrator.tick orch ~patches in
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
                let orch, _ = Orchestrator.tick orch ~patches in
                let orch = Orchestrator.complete orch pid in
                let orch =
                  List.fold ops ~init:orch ~f:(fun o k ->
                      Orchestrator.enqueue o pid k)
                in
                let agent = Orchestrator.agent orch pid in
                let expected_hp = Patch_agent.highest_priority agent in
                let _, actions = Orchestrator.tick orch ~patches in
                let responded_kind =
                  List.find_map actions ~f:(function
                    | Orchestrator.Respond (p, k) ->
                        if Patch_id.equal p pid then Some k else None
                    | Orchestrator.Start _ -> None)
                in
                match (expected_hp, responded_kind) with
                | Some hp, Some rk -> Operation_kind.equal hp rk
                | None, None -> true
                | _ -> false)))
  in
  QCheck2.Test.check_exn prop_p7;
  Stdlib.print_endline "P7 passed"

let () = Stdlib.print_endline "all state machine properties passed"
