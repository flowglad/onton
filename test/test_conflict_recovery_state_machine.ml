open Base
open Onton

(** Model-based / interleaving property tests for the conflict-recovery
    lifecycle.

    The single-rebase lifecycle modelled here is orthogonal to the multi-patch
    orchestrator interleavings in test_interleaving_properties.ml, so it lives
    in its own file (different state machine, simpler model).

    Invariants (R-1..R-5) ensure that whatever sequence of commands the system
    sees, the recovery information surfaced to the patch-agent prompt is
    sufficient to reconstruct the correct [git rebase --onto] command. *)

(* -- Model state -- *)

type phase =
  | Idle
  | Auto_rebased of { had_unique : bool }
  | Conflict_delivered of {
      ci : Worktree.conflict_info;
      prior_commits : Worktree.unique_commit list;
    }
  | Resolved
  | Aborted_then_recovered of { used_onto : bool }
  | Terminal

(* -- Commands -- *)

type cmd =
  | Attempt_rebase of {
      unique_commits : Worktree.unique_commit list;
      old_base : string;
      target : string;
    }
  | Auto_resolve
  | Hit_conflict
  | Reconstruct_in_progress
  | Agent_aborts
  | Agent_recovers_with_onto
  | Agent_recovers_with_plain

(* -- Apply: total transition function. Invalid transitions are no-ops. -- *)

let onto_fixture : Worktree.conflict_info =
  Worktree.
    {
      target = "main";
      old_base = "abc1234567890abc1234567890abc1234567890a";
      unique_commits =
        [
          { sha = "newcommit"; subject = "[proj] Patch 7: head" };
          { sha = "midcommit"; subject = "[proj] Patch 7: middle" };
          { sha = "oldcommit"; subject = "[proj] Patch 7: tail" };
        ];
      strategy = Onto;
      orig_head = "fff1234567890fff1234567890fff1234567890f";
    }

let plain_fixture : Worktree.conflict_info =
  Worktree.
    {
      target = "main";
      old_base = "";
      unique_commits = [];
      strategy = Plain;
      orig_head = "";
    }

let apply (state : phase) (c : cmd) : phase =
  match state with
  | Idle -> (
      match c with
      | Attempt_rebase { unique_commits; _ } ->
          Auto_rebased { had_unique = not (List.is_empty unique_commits) }
      | Auto_resolve | Hit_conflict | Reconstruct_in_progress | Agent_aborts
      | Agent_recovers_with_onto | Agent_recovers_with_plain ->
          state)
  | Auto_rebased { had_unique } -> (
      match c with
      | Auto_resolve -> Resolved
      | Hit_conflict ->
          let ci = if had_unique then onto_fixture else plain_fixture in
          Conflict_delivered { ci; prior_commits = ci.Worktree.unique_commits }
      | Attempt_rebase _ | Reconstruct_in_progress | Agent_aborts
      | Agent_recovers_with_onto | Agent_recovers_with_plain ->
          state)
  | Conflict_delivered { ci; prior_commits } -> (
      match c with
      | Reconstruct_in_progress ->
          (* Reconstruction in production goes through
             [Worktree.read_in_progress_conflict_info]; the parse layer is
             covered by single-shot properties in test_rebase_onto.ml. The
             model contract here: reconstruction recovers the SAME ci. *)
          Conflict_delivered { ci; prior_commits }
      | Agent_recovers_with_onto -> (
          match ci.Worktree.strategy with
          | Worktree.Onto -> Aborted_then_recovered { used_onto = true }
          | Worktree.Plain ->
              (* Plain delivery cannot recover with --onto; stays delivered. *)
              state)
      | Agent_recovers_with_plain -> (
          match ci.Worktree.strategy with
          | Worktree.Plain -> Aborted_then_recovered { used_onto = false }
          | Worktree.Onto ->
              (* R-2: Onto delivery cannot accept Plain recovery. *)
              state)
      | Agent_aborts -> state
      | Attempt_rebase _ | Auto_resolve | Hit_conflict -> state)
  | Resolved -> Terminal
  | Aborted_then_recovered _ -> Terminal
  | Terminal -> Terminal

(* -- Per-step invariants (R-1..R-5) -- *)

let unique_commit_set commits =
  List.map commits ~f:(fun (c : Worktree.unique_commit) -> c.Worktree.sha)
  |> Set.of_list (module String)

let invariants (state : phase) =
  match state with
  | Idle | Auto_rebased _ | Resolved | Terminal | Aborted_then_recovered _ ->
      Ok ()
  | Conflict_delivered { ci; prior_commits } ->
      (* R-1: Onto requires a non-empty old_base (needed to render the
         [--onto] command). unique_commits MAY be empty — production's
         parse_rebase_merge_state degrades to an empty list when the log is
         empty or every commit is ancestor-filtered, and the renderer omits
         the bullet header in that case. Plain strategy still requires both
         to be empty (fresh-rebase Plain branch never has unique commits). *)
      let r1 =
        match ci.strategy with
        | Worktree.Onto -> not (String.is_empty ci.old_base)
        | Worktree.Plain ->
            String.is_empty ci.old_base && List.is_empty ci.unique_commits
      in
      if not r1 then
        Error
          "R-1 violated: Onto with empty old_base, or Plain with non-empty \
           old_base/commits"
      else
        (* R-3 / R-4: prior_commits set is preserved across reconstruction
           (the model carries it; production reconstruction must round-trip). *)
        let r34 =
          Set.equal
            (unique_commit_set prior_commits)
            (unique_commit_set ci.unique_commits)
        in
        if not r34 then
          Error "R-3/R-4 violated: reconstruction lost commit information"
        else Ok ()

(* -- Cross-state invariants on transitions (R-2). -- *)

let transition_invariant ~(prev : phase) ~(curr : phase) ~(cmd : cmd) =
  match cmd with
  | Agent_recovers_with_plain -> (
      match prev with
      | Conflict_delivered { ci; _ } -> (
          match ci.Worktree.strategy with
          | Worktree.Onto -> (
              match curr with
              | Aborted_then_recovered _ ->
                  (* R-2: Onto-strategy delivery should never accept a Plain
                     recovery (the prompt's recovery command was --onto). *)
                  Error "R-2 violated: Onto delivery accepted Plain recovery"
              | Idle | Auto_rebased _ | Conflict_delivered _ | Resolved
              | Terminal ->
                  Ok ())
          | Worktree.Plain -> Ok ())
      | Idle | Auto_rebased _ | Resolved | Aborted_then_recovered _ | Terminal
        ->
          Ok ())
  | Attempt_rebase _ | Auto_resolve | Hit_conflict | Reconstruct_in_progress
  | Agent_aborts | Agent_recovers_with_onto ->
      Ok ()

(* -- Termination invariant (R-5) -- *)

let is_terminal_or_active = function
  | Terminal | Resolved | Aborted_then_recovered _ -> `Terminal
  | Idle | Auto_rebased _ | Conflict_delivered _ -> `Active

(* -- Generators -- *)

let gen_unique_commit =
  QCheck2.Gen.(
    let* sha = string_size ~gen:(char_range 'a' 'f') (int_range 6 12) in
    let* subject_tail =
      string_size ~gen:(char_range 'a' 'z') (int_range 1 12)
    in
    return Worktree.{ sha; subject = "feat: " ^ subject_tail })

let gen_attempt =
  QCheck2.Gen.(
    let* unique_commits = list_size (int_range 0 5) gen_unique_commit in
    let* old_base = string_size ~gen:(char_range 'a' 'f') (int_range 8 16) in
    return (Attempt_rebase { unique_commits; old_base; target = "main" }))

let gen_cmd =
  QCheck2.Gen.(
    oneof
      [
        gen_attempt;
        return Auto_resolve;
        return Hit_conflict;
        return Reconstruct_in_progress;
        return Agent_aborts;
        return Agent_recovers_with_onto;
        return Agent_recovers_with_plain;
      ])

let gen_command_seq = QCheck2.Gen.(list_size (int_range 1 12) gen_cmd)

(* -- Runner: apply commands; check invariants per step. -- *)

let run_sequence (cmds : cmd list) =
  let state = ref Idle in
  let error = ref None in
  List.iter cmds ~f:(fun cmd ->
      if Option.is_some !error then ()
      else
        let prev = !state in
        let curr = apply prev cmd in
        (match invariants curr with
        | Ok () -> ()
        | Error msg -> error := Some msg);
        (match transition_invariant ~prev ~curr ~cmd with
        | Ok () -> ()
        | Error msg -> if Option.is_none !error then error := Some msg);
        state := curr);
  (!state, !error)

(* -- Properties -- *)

let () =
  let open QCheck2 in
  let prop_invariants_hold =
    Test.make ~name:"R-1..R-4: invariants hold across random command sequences"
      ~count:500 gen_command_seq (fun cmds ->
        let _, err = run_sequence cmds in
        Option.is_none err)
  in
  let prop_eventually_terminal_when_finished =
    (* R-5: when the final command's precondition holds — i.e. the prev state
       is one in which the command CAN make a transition — the post-state
       must be Terminal-equivalent (Resolved / Aborted_then_recovered /
       Terminal). Splitting the precondition out of the assertion is what
       keeps this property non-vacuous: a body that returned [true] for both
       Terminal and Active outcomes (the previous shape) had no failing path
       once a no-op was indistinguishable from a real termination. *)
    Test.make
      ~name:
        "R-5: terminating commands reach Terminal/Resolved when their \
         precondition holds"
      ~count:500
      Gen.(
        let* core = list_size (int_range 0 8) gen_cmd in
        let* tail =
          oneof_list
            [
              Auto_resolve; Agent_recovers_with_onto; Agent_recovers_with_plain;
            ]
        in
        return (core, tail))
      (fun (core, tail) ->
        let prev_state, prev_err = run_sequence core in
        if Option.is_some prev_err then false
        else
          let final_state, final_err = run_sequence (core @ [ tail ]) in
          if Option.is_some final_err then false
          else
            let precondition_holds =
              match prev_state with
              | Auto_rebased _ -> (
                  match tail with
                  | Auto_resolve -> true
                  | Attempt_rebase _ | Hit_conflict | Reconstruct_in_progress
                  | Agent_aborts | Agent_recovers_with_onto
                  | Agent_recovers_with_plain ->
                      false)
              | Conflict_delivered { ci; _ } -> (
                  match tail with
                  | Agent_recovers_with_onto ->
                      Worktree.equal_rebase_strategy ci.Worktree.strategy
                        Worktree.Onto
                  | Agent_recovers_with_plain ->
                      Worktree.equal_rebase_strategy ci.Worktree.strategy
                        Worktree.Plain
                  | Attempt_rebase _ | Auto_resolve | Hit_conflict
                  | Reconstruct_in_progress | Agent_aborts ->
                      false)
              | Idle | Resolved | Aborted_then_recovered _ | Terminal -> false
            in
            match (is_terminal_or_active final_state, precondition_holds) with
            | `Terminal, _ -> true
            | `Active, false -> true
            | `Active, true -> false)
  in
  let prop_onto_delivery_rejects_plain_recovery =
    Test.make
      ~name:
        "R-2: Onto-strategy delivery cannot transition via \
         Agent_recovers_with_plain" ~count:200 Gen.unit (fun () ->
        let cmds =
          [
            Attempt_rebase
              {
                unique_commits = [ { sha = "abc1234"; subject = "feat: head" } ];
                old_base = "deadbeef";
                target = "main";
              };
            Hit_conflict;
            Agent_recovers_with_plain;
          ]
        in
        let final, err = run_sequence cmds in
        Option.is_none err
        &&
        match final with
        | Conflict_delivered _ -> true (* recovery rejected, stays delivered *)
        | Aborted_then_recovered { used_onto = false } -> false
        | Aborted_then_recovered { used_onto = true }
        | Idle | Auto_rebased _ | Resolved | Terminal ->
            true)
  in
  let suite =
    [
      prop_invariants_hold;
      prop_eventually_terminal_when_finished;
      prop_onto_delivery_rejects_plain_recovery;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

let () =
  Stdlib.print_endline
    "All conflict-recovery state-machine interleaving tests passed."
