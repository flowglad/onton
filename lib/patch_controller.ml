open Base
open Types

let action_identity = function
  | Orchestrator.Start (patch_id, base) ->
      Printf.sprintf "start:%s:%s"
        (Patch_id.to_string patch_id)
        (Branch.to_string base)
  | Orchestrator.Respond (patch_id, kind) ->
      Printf.sprintf "respond:%s:%s"
        (Patch_id.to_string patch_id)
        (Operation_kind.to_label kind)
  | Orchestrator.Rebase (patch_id, base) ->
      Printf.sprintf "rebase:%s:%s"
        (Patch_id.to_string patch_id)
        (Branch.to_string base)

let message_of_action (patch_agent : Patch_agent.t) action =
  let identity =
    Printf.sprintf "%d:%s" patch_agent.generation (action_identity action)
  in
  let digest = Stdlib.Digest.to_hex (Stdlib.Digest.string identity) in
  let msg_id =
    Message_id.of_string
      (Printf.sprintf "%s:%s" (Patch_id.to_string patch_agent.patch_id) digest)
  in
  Orchestrator.
    {
      message_id = msg_id;
      patch_id = patch_agent.patch_id;
      generation = patch_agent.generation;
      action;
      payload_hash = digest;
      status = Pending;
    }

type github_effect =
  | Set_pr_description of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      body : string;
    }
  | Set_pr_draft of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      draft : bool;
    }
[@@deriving show, eq, sexp_of]

type poll_log_entry = { message : string; patch_id : Patch_id.t }
[@@deriving show, eq]

type poll_observation = {
  poll_result : Poller.t;
  head_branch : Branch.t option;
  base_branch : Branch.t option;
  branch_in_root : bool;
  worktree_path : string option;
}

let enqueue_notes_if_needed t patch_id (agent : Patch_agent.t) =
  if
    (not (Patch_agent.has_pr agent))
    || agent.merged || agent.implementation_notes_delivered
  then t
  else
    let already_queued =
      List.mem agent.queue Operation_kind.Implementation_notes
        ~equal:Operation_kind.equal
      || Option.equal Operation_kind.equal agent.current_op
           (Some Operation_kind.Implementation_notes)
    in
    if already_queued then t
    else Orchestrator.enqueue t patch_id Operation_kind.Implementation_notes

let apply_poll_result t patch_id
    ({ poll_result; head_branch; base_branch; branch_in_root; worktree_path } :
      poll_observation) =
  let logs = ref [] in
  let log message = logs := { message; patch_id } :: !logs in
  let t =
    if poll_result.merged then (
      let agent = Orchestrator.agent t patch_id in
      if not agent.Patch_agent.merged then log "merged";
      Orchestrator.mark_merged t patch_id)
    else t
  in
  let had_conflict_before =
    (Orchestrator.agent t patch_id).Patch_agent.has_conflict
  in
  let t =
    if poll_result.has_conflict then (
      let agent = Orchestrator.agent t patch_id in
      if not agent.Patch_agent.has_conflict then log "merge conflict detected";
      Orchestrator.set_has_conflict t patch_id)
    else
      let agent = Orchestrator.agent t patch_id in
      if agent.Patch_agent.has_conflict then
        if Patch_decision.should_clear_conflict agent then (
          log "conflict cleared (no longer detected)";
          Orchestrator.clear_has_conflict t patch_id)
        else (
          log "conflict flag retained (resolution in flight)";
          t)
      else t
  in
  let agent_before = Orchestrator.agent t patch_id in
  let t =
    List.fold poll_result.queue ~init:t ~f:(fun acc kind ->
        let current_agent = Orchestrator.agent acc patch_id in
        let already_queued =
          List.mem current_agent.Patch_agent.queue kind
            ~equal:Operation_kind.equal
        in
        let is_new =
          (not already_queued)
          && not
               (Option.equal Operation_kind.equal
                  agent_before.Patch_agent.current_op (Some kind))
        in
        match kind with
        | Operation_kind.Ci -> (
            match Patch_decision.on_ci_failure agent_before with
            | Patch_decision.Enqueue_ci ->
                if is_new then
                  log
                    (Printf.sprintf "enqueued %s"
                       (Operation_kind.to_label kind));
                Orchestrator.enqueue acc patch_id kind
            | Patch_decision.Ci_already_queued ->
                log "CI already queued, skipping";
                acc
            | Patch_decision.Ci_fix_in_progress ->
                log "CI fix in progress, skipping";
                acc
            | Patch_decision.Cap_reached ->
                log "CI failure cap reached (>=3), skipping CI enqueue";
                acc)
        | Operation_kind.Review_comments ->
            if is_new then
              log (Printf.sprintf "enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind
        | Operation_kind.Merge_conflict ->
            if is_new && not had_conflict_before then (
              log (Printf.sprintf "enqueued %s" (Operation_kind.to_label kind));
              Orchestrator.enqueue acc patch_id kind)
            else (
              log "merge-conflict already tracked, skipping";
              acc)
        | Operation_kind.Rebase | Operation_kind.Human
        | Operation_kind.Implementation_notes ->
            if is_new then
              log (Printf.sprintf "enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind)
  in
  let t = Orchestrator.set_merge_ready t patch_id poll_result.merge_ready in
  let t =
    let was_draft = (Orchestrator.agent t patch_id).Patch_agent.is_draft in
    let t = Orchestrator.set_is_draft t patch_id poll_result.is_draft in
    if (not was_draft) && poll_result.is_draft then log "marked as draft"
    else if was_draft && not poll_result.is_draft then
      log "marked as ready for review";
    t
  in
  let t = Orchestrator.set_ci_checks t patch_id poll_result.ci_checks in
  let t =
    Orchestrator.set_checks_passing t patch_id poll_result.checks_passing
  in
  let t =
    let agent = Orchestrator.agent t patch_id in
    match
      Patch_decision.on_checks_passing agent
        ~checks_passing:poll_result.checks_passing
    with
    | Patch_decision.Reset_ci_failure_count ->
        log "CI checks now passing, clearing failure history";
        Orchestrator.reset_ci_failure_count t patch_id
    | Patch_decision.No_ci_reset -> t
  in
  let t, newly_blocked =
    match head_branch with
    | Some branch ->
        let t = Orchestrator.set_head_branch t patch_id branch in
        if branch_in_root then
          let agent = Orchestrator.agent t patch_id in
          if not agent.Patch_agent.branch_blocked then (
            log "branch checked out in repo root, blocking worktree operations";
            (Orchestrator.set_branch_blocked t patch_id, true))
          else (Orchestrator.set_branch_blocked t patch_id, false)
        else
          let agent = Orchestrator.agent t patch_id in
          if agent.Patch_agent.branch_blocked then (
            log "branch no longer in repo root, unblocked";
            (Orchestrator.clear_branch_blocked t patch_id, false))
          else (t, false)
    | None -> (t, false)
  in
  let t =
    let agent = Orchestrator.agent t patch_id in
    match (agent.Patch_agent.base_branch, base_branch) with
    | None, Some branch -> Orchestrator.set_base_branch t patch_id branch
    | _ -> t
  in
  let t =
    let agent = Orchestrator.agent t patch_id in
    match worktree_path with
    | Some path when Option.is_none agent.Patch_agent.worktree_path ->
        let t = Orchestrator.set_worktree_path t patch_id path in
        let agent = Orchestrator.agent t patch_id in
        if agent.Patch_agent.branch_blocked then
          Orchestrator.clear_branch_blocked t patch_id
        else t
    | _ -> t
  in
  (t, List.rev !logs, newly_blocked)

let apply_replacement_pr t patch_id ~pr_number ~base_branch ~merged =
  let t = Orchestrator.set_pr_number t patch_id pr_number in
  let t = Orchestrator.set_base_branch t patch_id base_branch in
  if merged then Orchestrator.mark_merged t patch_id else t

let reconcile_patch t ~project_name ~gameplan ~(patch : Patch.t) =
  let patch_id = patch.id in
  let agent = Orchestrator.agent t patch_id in
  if agent.Patch_agent.merged then (t, [])
  else
    let t = enqueue_notes_if_needed t patch_id agent in
    let agent = Orchestrator.agent t patch_id in
    let effects = ref [] in
    (match agent.pr_number with
    | Some pr_number -> (
        if not agent.pr_description_applied then
          effects :=
            Set_pr_description
              {
                patch_id;
                pr_number;
                body = Prompt.render_pr_description ~project_name patch gameplan;
              }
            :: !effects;
        match agent.base_branch with
        | Some base_branch ->
            let desired_draft =
              if Branch.equal base_branch (Orchestrator.main_branch t) then
                not agent.implementation_notes_delivered
              else true
            in
            if Bool.(agent.is_draft <> desired_draft) then
              effects :=
                Set_pr_draft { patch_id; pr_number; draft = desired_draft }
                :: !effects
        | None -> ())
    | None -> ());
    (t, List.rev !effects)

let reconcile_all t ~project_name ~gameplan =
  List.fold gameplan.Gameplan.patches ~init:(t, []) ~f:(fun (orch, acc) patch ->
      let orch, effects = reconcile_patch orch ~project_name ~gameplan ~patch in
      (orch, acc @ effects))

let branch_map_of_patches patches =
  List.fold patches
    ~init:(Map.empty (module Patch_id))
    ~f:(fun acc (p : Patch.t) ->
      match Map.add acc ~key:p.Patch.id ~data:p.Patch.branch with
      | `Ok m -> m
      | `Duplicate ->
          invalid_arg
            (Printf.sprintf
               "Patch_controller.plan_actions: duplicate patch id %s"
               (Patch_id.to_string p.Patch.id)))

let plan_action_for_patch t ~branch_map patch_id =
  let agent = Orchestrator.agent t patch_id in
  let has_merged pid = (Orchestrator.agent t pid).Patch_agent.merged in
  let has_pr pid = Patch_agent.has_pr (Orchestrator.agent t pid) in
  if
    (not (Patch_agent.has_pr agent))
    && (not agent.Patch_agent.busy)
    && (not agent.Patch_agent.merged)
    && (not (Patch_agent.needs_intervention agent))
    && Graph.deps_satisfied (Orchestrator.graph t) patch_id ~has_merged ~has_pr
  then
    let branch_of pid =
      match Map.find branch_map pid with
      | Some b -> b
      | None ->
          invalid_arg
            (Printf.sprintf
               "Patch_controller.plan_actions: no branch for patch %s"
               (Patch_id.to_string pid))
    in
    let base =
      Graph.initial_base (Orchestrator.graph t) patch_id ~has_merged ~branch_of
        ~main:(Orchestrator.main_branch t)
    in
    Some (Orchestrator.Start (patch_id, base))
  else if
    Patch_agent.has_pr agent
    && (not agent.Patch_agent.merged)
    && (not agent.Patch_agent.busy)
    && (not (Patch_agent.needs_intervention agent))
    && (not agent.Patch_agent.branch_blocked)
    && List.mem agent.Patch_agent.queue Operation_kind.Rebase
         ~equal:Operation_kind.equal
  then
    match Patch_agent.highest_priority agent with
    | Some highest when Operation_kind.equal highest Operation_kind.Rebase ->
        let branch_of dep_pid =
          match Map.find branch_map dep_pid with
          | Some b -> b
          | None ->
              Option.value
                (Orchestrator.agent t dep_pid).Patch_agent.base_branch
                ~default:(Orchestrator.main_branch t)
        in
        let new_base =
          Graph.initial_base (Orchestrator.graph t) patch_id ~has_merged
            ~branch_of
            ~main:(Orchestrator.main_branch t)
        in
        Some (Orchestrator.Rebase (patch_id, new_base))
    | _ -> None
  else if
    Patch_agent.has_pr agent
    && (not agent.Patch_agent.merged)
    && (not agent.Patch_agent.busy)
    && (not (Patch_agent.needs_intervention agent))
    && not agent.Patch_agent.branch_blocked
  then
    Patch_agent.highest_priority agent
    |> Option.bind ~f:(fun kind ->
        if Priority.is_feedback kind then
          Some (Orchestrator.Respond (patch_id, kind))
        else None)
  else None

let reconcile_action_message t action =
  let patch_id =
    match action with
    | Orchestrator.Start (patch_id, _)
    | Orchestrator.Respond (patch_id, _)
    | Orchestrator.Rebase (patch_id, _) ->
        patch_id
  in
  let agent = Orchestrator.agent t patch_id in
  let msg = message_of_action agent action in
  let t = Orchestrator.reconcile_message t msg in
  let msg = Option.value_exn (Orchestrator.find_message t msg.message_id) in
  (t, msg)

let reconcile_messages t ~patches =
  let branch_map = branch_map_of_patches patches in
  let missing =
    Graph.all_patch_ids (Orchestrator.graph t)
    |> List.filter ~f:(fun pid ->
        (not (Map.mem branch_map pid))
        && not (Patch_agent.has_pr (Orchestrator.agent t pid)))
  in
  if not (List.is_empty missing) then
    invalid_arg
      (Printf.sprintf
         "Patch_controller.plan_messages: tick input missing %d patch id(s) \
          from graph"
         (List.length missing));
  let patch_ids = Graph.all_patch_ids (Orchestrator.graph t) in
  let t =
    List.fold patch_ids ~init:t ~f:(fun acc patch_id ->
        let keep =
          match Orchestrator.current_message acc patch_id with
          | Some msg when Orchestrator.equal_message_status msg.status Acked ->
              [ msg.message_id ]
          | _ -> []
        in
        Orchestrator.mark_patch_pending_messages_obsolete_except acc patch_id
          ~keep)
  in
  let t, desired_ids =
    List.fold patch_ids ~init:(t, []) ~f:(fun (acc, ids) patch_id ->
        match Orchestrator.current_message acc patch_id with
        | Some msg when Orchestrator.equal_message_status msg.status Acked ->
            (acc, msg.message_id :: ids)
        | _ -> (
            match plan_action_for_patch acc ~branch_map patch_id with
            | None -> (acc, ids)
            | Some action ->
                let acc, msg = reconcile_action_message acc action in
                (acc, msg.message_id :: ids)))
  in
  let t =
    List.fold patch_ids ~init:t ~f:(fun acc patch_id ->
        let keep =
          List.filter desired_ids ~f:(fun message_id ->
              match Orchestrator.find_message acc message_id with
              | Some msg when Patch_id.equal msg.patch_id patch_id -> true
              | _ -> false)
        in
        Orchestrator.mark_patch_pending_messages_obsolete_except acc patch_id
          ~keep)
  in
  (t, Orchestrator.runnable_messages t)

let plan_messages t ~patches = snd (reconcile_messages t ~patches)

let plan_actions t ~patches =
  plan_messages t ~patches
  |> List.map ~f:(fun (msg : Orchestrator.patch_agent_message) -> msg.action)

let plan_tick_messages t ~project_name ~gameplan =
  let t, effects = reconcile_all t ~project_name ~gameplan in
  let t, messages = reconcile_messages t ~patches:gameplan.Gameplan.patches in
  (t, effects, messages)

let plan_tick t ~project_name ~gameplan =
  let t, effects, messages = plan_tick_messages t ~project_name ~gameplan in
  let actions =
    List.map messages ~f:(fun (msg : Orchestrator.patch_agent_message) ->
        msg.action)
  in
  (t, effects, actions)

let tick t ~project_name ~gameplan =
  let t, effects, actions = plan_tick t ~project_name ~gameplan in
  let t =
    List.fold actions ~init:t ~f:(fun acc action ->
        Orchestrator.fire acc action)
  in
  (t, effects, actions)

let apply_github_effect_success t = function
  | Set_pr_description { patch_id; _ } ->
      Orchestrator.set_pr_description_applied t patch_id true
  | Set_pr_draft { patch_id; draft; _ } ->
      Orchestrator.set_is_draft t patch_id draft

let make_orchestrator ~patch_id ~main_branch =
  let patch =
    Patch.
      {
        id = patch_id;
        title = "test";
        description = "test";
        branch = Branch.of_string "test-branch";
        dependencies = [];
        spec = "";
        acceptance_criteria = [];
        changes = [];
        files = [];
        classification = "";
        test_stubs_introduced = [];
        test_stubs_implemented = [];
      }
  in
  (patch, Orchestrator.create ~patches:[ patch ] ~main_branch)

let pid = Patch_id.of_string "p1"
let main = Branch.of_string "main"

let%test "reconcile_patch escalates repeated start discovery failures" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t =
    let t = Orchestrator.increment_start_attempts_without_pr t pid in
    Orchestrator.increment_start_attempts_without_pr t pid
  in
  let t, _ =
    reconcile_patch t ~project_name:"proj"
      ~gameplan:
        Gameplan.
          {
            project_name = "proj";
            problem_statement = "";
            solution_summary = "";
            final_state_spec = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
            open_questions = [];
          }
      ~patch
  in
  Patch_agent.needs_intervention (Orchestrator.agent t pid)

let%test "reconcile_patch enqueues implementation notes while missing" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let t, _ =
    reconcile_patch t ~project_name:"proj"
      ~gameplan:
        Gameplan.
          {
            project_name = "proj";
            problem_statement = "";
            solution_summary = "";
            final_state_spec = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
            open_questions = [];
          }
      ~patch
  in
  List.mem (Orchestrator.agent t pid).Patch_agent.queue
    Operation_kind.Implementation_notes ~equal:Operation_kind.equal

let%test "reconcile_patch emits description effect while unapplied" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let _, effects =
    reconcile_patch t ~project_name:"proj"
      ~gameplan:
        Gameplan.
          {
            project_name = "proj";
            problem_statement = "";
            solution_summary = "";
            final_state_spec = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
            open_questions = [];
          }
      ~patch
  in
  List.exists effects ~f:(function
    | Set_pr_description { patch_id; _ } -> Patch_id.equal patch_id pid
    | Set_pr_draft _ -> false)

let%test "reconcile_patch requests ready-for-review after notes on main" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.set_implementation_notes_delivered t pid true in
  let t = Orchestrator.complete t pid in
  let _, effects =
    reconcile_patch t ~project_name:"proj"
      ~gameplan:
        Gameplan.
          {
            project_name = "proj";
            problem_statement = "";
            solution_summary = "";
            final_state_spec = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
            open_questions = [];
          }
      ~patch
  in
  List.exists effects ~f:(function
    | Set_pr_draft { patch_id; draft = false; _ } -> Patch_id.equal patch_id pid
    | Set_pr_description _ | Set_pr_draft _ -> false)

let%test "reconcile_patch emits no effects for merged agent" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let t = Orchestrator.mark_merged t pid in
  let _, effects =
    reconcile_patch t ~project_name:"proj"
      ~gameplan:
        Gameplan.
          {
            project_name = "proj";
            problem_statement = "";
            solution_summary = "";
            final_state_spec = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
            open_questions = [];
          }
      ~patch
  in
  List.is_empty effects

let%test "no merge-conflict re-enqueue after noop" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  (* First poll: conflict detected, enqueues Merge_conflict *)
  let poll_conflict =
    Poller.
      {
        queue = [ Operation_kind.Merge_conflict ];
        merged = false;
        closed = false;
        is_draft = false;
        has_conflict = true;
        merge_ready = false;
        checks_passing = false;
        ci_checks = [];
      }
  in
  let obs =
    {
      poll_result = poll_conflict;
      head_branch = Some (Branch.of_string "test-branch");
      base_branch = Some main;
      branch_in_root = false;
      worktree_path = None;
    }
  in
  let t, _, _ = apply_poll_result t pid obs in
  let agent = Orchestrator.agent t pid in
  assert (
    List.mem agent.Patch_agent.queue Operation_kind.Merge_conflict
      ~equal:Operation_kind.equal);
  (* Fire the Merge_conflict, then simulate Noop via apply_conflict_rebase_result *)
  let t =
    Orchestrator.fire t
      (Orchestrator.Respond (pid, Operation_kind.Merge_conflict))
  in
  let t, decision, _effects =
    Orchestrator.apply_conflict_rebase_result t pid Worktree.Noop main
  in
  (* Noop -> Deliver_to_agent, agent still busy *)
  Orchestrator.equal_conflict_rebase_decision decision
    Orchestrator.Deliver_to_agent
  && (Orchestrator.agent t pid).Patch_agent.has_conflict
  && (Orchestrator.agent t pid).Patch_agent.busy
