open Base
open Onton
open Onton.Types

let main = Branch.of_string "main"
let pid = Patch_id.of_string "p0"
let patch_branch = Branch.of_string "feature/p0"

let patch =
  Patch.
    {
      id = pid;
      title = "Patch 0";
      description = "";
      branch = patch_branch;
      dependencies = [];
      spec = "";
      acceptance_criteria = [];
      files = [];
      classification = "";
      changes = [];
      test_stubs_introduced = [];
      test_stubs_implemented = [];
    }

let gameplan =
  Gameplan.
    {
      project_name = "test-project";
      problem_statement = "";
      solution_summary = "";
      design_decisions = "";
      patches = [ patch ];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
    }

let action_equal a b =
  match (a, b) with
  | Orchestrator.Start (p1, b1), Orchestrator.Start (p2, b2) ->
      Patch_id.equal p1 p2 && Branch.equal b1 b2
  | Orchestrator.Respond (p1, k1), Orchestrator.Respond (p2, k2) ->
      Patch_id.equal p1 p2 && Operation_kind.equal k1 k2
  | Orchestrator.Rebase (p1, b1), Orchestrator.Rebase (p2, b2) ->
      Patch_id.equal p1 p2 && Branch.equal b1 b2
  | ( Orchestrator.Start _,
      (Orchestrator.Respond _ | Orchestrator.Rebase _) )
  | ( Orchestrator.Respond _,
      (Orchestrator.Start _ | Orchestrator.Rebase _) )
  | ( Orchestrator.Rebase _,
      (Orchestrator.Start _ | Orchestrator.Respond _) ) ->
      false

let has_start_action actions =
  List.exists actions ~f:(function
    | Orchestrator.Start (action_pid, _) -> Patch_id.equal action_pid pid
    | Orchestrator.Respond _ | Orchestrator.Rebase _ -> false)

let has_notes_action actions =
  List.exists actions ~f:(function
    | Orchestrator.Respond (action_pid, kind) ->
        Patch_id.equal action_pid pid
        && Operation_kind.equal kind Operation_kind.Implementation_notes
    | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)

let has_description_effect effects =
  List.exists effects ~f:(function
    | Patch_controller.Set_pr_description { patch_id; _ } ->
        Patch_id.equal patch_id pid
    | Patch_controller.Set_pr_draft _ -> false)

let normalize orch =
  let orch1, effects1, actions1 =
    Patch_controller.plan_tick orch ~project_name:"test-project" ~gameplan
  in
  let orch2, effects2, actions2 =
    Patch_controller.plan_tick orch1 ~project_name:"test-project" ~gameplan
  in
  let deterministic =
    Map.equal Patch_agent.equal (Orchestrator.agents_map orch1)
      (Orchestrator.agents_map orch2)
    && List.equal Patch_controller.equal_github_effect effects1 effects2
    && List.equal action_equal actions1 actions2
  in
  let agent = Orchestrator.agent orch1 pid in
  let bootstrap_consistent =
    (not agent.Patch_agent.pr_description_applied
    || not (has_description_effect effects1))
    &&
    if agent.Patch_agent.implementation_notes_delivered then
      not (has_notes_action actions1)
    else true
  in
  let intervention_consistent =
    if (not agent.Patch_agent.has_pr) && agent.Patch_agent.start_attempts_without_pr >= 2
    then agent.Patch_agent.needs_intervention && not (has_start_action actions1)
    else true
  in
  if not (deterministic && bootstrap_consistent && intervention_consistent)
  then failwith "controller state machine invariant violated";
  orch1

type command =
  | Observe_poll of {
      ci_failed : bool;
      checks_passing : bool;
      mergeable : bool;
      has_conflict : bool;
      branch_in_root : bool;
    }
  | Ack_effects
  | Fire_first_action
  | Complete_current of { deliver_notes : bool }
  | Discover_pr
  | Record_no_pr_attempt
[@@deriving show]

let gen_command =
  let open QCheck2.Gen in
  oneof
    [
      map5
        (fun ci_failed checks_passing mergeable has_conflict branch_in_root ->
          Observe_poll
            {
              ci_failed;
              checks_passing;
              mergeable;
              has_conflict;
              branch_in_root;
            })
        bool bool bool bool bool;
      pure Ack_effects;
      pure Fire_first_action;
      map (fun deliver_notes -> Complete_current { deliver_notes }) bool;
      pure Discover_pr;
      pure Record_no_pr_attempt;
    ]

let make_poll_observation ~ci_failed ~checks_passing ~mergeable ~has_conflict
    ~branch_in_root =
  let queue =
    if ci_failed then [ Operation_kind.Ci ] else []
  in
  Patch_controller.
    {
      poll_result =
        Poller.
          {
            queue;
            merged = false;
            closed = false;
            is_draft = true;
            has_conflict;
            mergeable;
            merge_ready = false;
            checks_passing;
            ci_checks = [];
          };
      head_branch = Some patch_branch;
      base_branch = Some main;
      branch_in_root;
      worktree_path = None;
    }

let apply_command orch = function
  | Observe_poll
      { ci_failed; checks_passing; mergeable; has_conflict; branch_in_root } ->
      if not (Orchestrator.agent orch pid).Patch_agent.has_pr then orch
      else
        let observation =
          make_poll_observation ~ci_failed ~checks_passing ~mergeable
            ~has_conflict ~branch_in_root
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid observation
        in
        orch
  | Ack_effects ->
      let orch, effects, _actions =
        Patch_controller.plan_tick orch ~project_name:"test-project" ~gameplan
      in
      List.fold effects ~init:orch ~f:Patch_controller.apply_github_effect_success
  | Fire_first_action ->
      let orch, _effects, actions =
        Patch_controller.plan_tick orch ~project_name:"test-project" ~gameplan
      in
      (match List.hd actions with
      | Some action -> Orchestrator.fire orch action
      | None -> orch)
  | Complete_current { deliver_notes } ->
      let orch =
        match (Orchestrator.agent orch pid).Patch_agent.current_op with
        | Some kind
          when deliver_notes
               && Operation_kind.equal kind Operation_kind.Implementation_notes
          ->
            Orchestrator.set_implementation_notes_delivered orch pid true
        | _ -> orch
      in
      Orchestrator.complete orch pid
  | Discover_pr ->
      if (Orchestrator.agent orch pid).Patch_agent.has_pr then orch
      else Orchestrator.set_pr_number orch pid (Pr_number.of_int 42)
  | Record_no_pr_attempt ->
      if (Orchestrator.agent orch pid).Patch_agent.has_pr then orch
      else Orchestrator.on_pr_discovery_failure orch pid

let run_sequence cmds =
  let orch = Orchestrator.create ~patches:[ patch ] ~main_branch:main in
  List.fold cmds ~init:orch ~f:(fun orch cmd ->
      let orch = apply_command orch cmd in
      normalize orch)

let () =
  let open QCheck2 in
  let prop =
    Test.make
      ~name:
        "patch_controller_state_machine: mixed command sequences replay to the \
         same planned snapshot"
      ~count:300
      Gen.(list_size (int_range 1 40) gen_command)
      (fun cmds ->
        try
          let _final = run_sequence cmds in
          true
        with _ -> false)
  in
  QCheck_base_runner.run_tests ~verbose:true [ prop ] |> Stdlib.exit
