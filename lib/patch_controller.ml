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
  | Set_pr_draft of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      draft : bool;
    }
  | Set_pr_base of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      base : Branch.t;
    }
[@@deriving show, eq, sexp_of]

type poll_log_entry = { message : string; patch_id : Patch_id.t }
[@@deriving show, eq]

type poll_observation = {
  poll_result : Poller.t;
  base_branch : Branch.t option;
  branch_in_root : bool;
  worktree_path : string option;
}

let discovery_intents orch =
  Orchestrator.all_agents orch
  |> List.filter_map ~f:(fun (agent : Patch_agent.t) ->
      if
        agent.has_session
        && (not (Patch_agent.has_pr agent))
        && not agent.merged
      then Some (agent.patch_id, agent.branch)
      else None)

let enqueue_pr_body_if_needed t patch_id (agent : Patch_agent.t) =
  if (not (Patch_agent.has_pr agent)) || agent.merged || agent.pr_body_delivered
  then t
  else
    let already_queued =
      List.mem agent.queue Operation_kind.Pr_body ~equal:Operation_kind.equal
      || Option.equal Operation_kind.equal agent.current_op
           (Some Operation_kind.Pr_body)
    in
    if already_queued then t
    else Orchestrator.enqueue t patch_id Operation_kind.Pr_body

let apply_poll_result t patch_id
    ({ poll_result; base_branch; branch_in_root; worktree_path } :
      poll_observation) =
  let logs = ref [] in
  let log message = logs := { message; patch_id } :: !logs in
  let t =
    if poll_result.merged then (
      let agent = Orchestrator.agent t patch_id in
      if not agent.Patch_agent.merged then log "Merged";
      Orchestrator.mark_merged t patch_id)
    else t
  in
  let t =
    if poll_result.has_conflict then (
      let agent = Orchestrator.agent t patch_id in
      if not agent.Patch_agent.has_conflict then log "Merge conflict detected";
      Orchestrator.set_has_conflict t patch_id)
    else
      let agent = Orchestrator.agent t patch_id in
      if agent.Patch_agent.has_conflict then
        if Patch_decision.should_clear_conflict agent then (
          log "Conflict cleared — no longer detected";
          let t = Orchestrator.clear_has_conflict t patch_id in
          Orchestrator.reset_conflict_noop_count t patch_id)
        else (
          log "Conflict flag retained — resolution in flight";
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
                    (Printf.sprintf "Enqueued %s"
                       (Operation_kind.to_label kind));
                Orchestrator.enqueue acc patch_id kind
            | Patch_decision.Ci_already_queued ->
                log "CI already queued — skipping";
                acc
            | Patch_decision.Ci_fix_in_progress ->
                log "CI fix in progress — skipping";
                acc
            | Patch_decision.Cap_reached ->
                log "CI failure cap reached (>=3) — skipping CI enqueue";
                acc)
        | Operation_kind.Review_comments ->
            if is_new then
              log (Printf.sprintf "Enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind
        | Operation_kind.Merge_conflict ->
            if is_new then
              log (Printf.sprintf "Enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind
        | Operation_kind.Rebase | Operation_kind.Human | Operation_kind.Pr_body
          ->
            if is_new then
              log (Printf.sprintf "Enqueued %s" (Operation_kind.to_label kind));
            Orchestrator.enqueue acc patch_id kind)
  in
  let t = Orchestrator.set_merge_ready t patch_id poll_result.merge_ready in
  let t =
    let was_draft = (Orchestrator.agent t patch_id).Patch_agent.is_draft in
    let t = Orchestrator.set_is_draft t patch_id poll_result.is_draft in
    if (not was_draft) && poll_result.is_draft then log "Marked as draft"
    else if was_draft && not poll_result.is_draft then
      log "Marked as ready for review";
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
        log "CI checks now passing — cleared failure history";
        Orchestrator.reset_ci_failure_count t patch_id
    | Patch_decision.No_ci_reset -> t
  in
  let t, newly_blocked =
    if branch_in_root then
      let agent = Orchestrator.agent t patch_id in
      if
        (not agent.Patch_agent.branch_blocked)
        && Option.is_none agent.Patch_agent.worktree_path
      then (
        log "Branch checked out in repo root — blocking worktree operations";
        (Orchestrator.set_branch_blocked t patch_id, true))
      else if
        (not agent.Patch_agent.branch_blocked)
        && Option.is_some agent.Patch_agent.worktree_path
      then (t, false)
      else (Orchestrator.set_branch_blocked t patch_id, false)
    else
      let agent = Orchestrator.agent t patch_id in
      if agent.Patch_agent.branch_blocked then (
        log "Branch no longer in repo root — unblocked";
        (Orchestrator.clear_branch_blocked t patch_id, false))
      else (t, false)
  in
  let t =
    match base_branch with
    | Some branch -> Orchestrator.set_base_branch t patch_id branch
    | None -> t
  in
  let t =
    let agent = Orchestrator.agent t patch_id in
    match worktree_path with
    | Some path when Option.is_none agent.Patch_agent.worktree_path ->
        let t = Orchestrator.set_worktree_path t patch_id path in
        let agent = Orchestrator.agent t patch_id in
        if agent.Patch_agent.branch_blocked then (
          log "Worktree path discovered — unblocking branch operations";
          Orchestrator.clear_branch_blocked t patch_id)
        else t
    | _ -> t
  in
  (t, List.rev !logs, newly_blocked)

let apply_replacement_pr t patch_id ~pr_number ~base_branch ~merged =
  let t = Orchestrator.set_pr_number t patch_id pr_number in
  let t = Orchestrator.set_base_branch t patch_id base_branch in
  if merged then Orchestrator.mark_merged t patch_id else t

let reconcile_patch t ~project_name:_ ~gameplan:_ ~(patch : Patch.t) =
  let patch_id = patch.id in
  let agent = Orchestrator.agent t patch_id in
  if agent.Patch_agent.merged then (t, [])
  else
    let t = enqueue_pr_body_if_needed t patch_id agent in
    let agent = Orchestrator.agent t patch_id in
    let effects = ref [] in
    (match agent.pr_number with
    | Some pr_number -> (
        match agent.base_branch with
        | Some actual_base ->
            let has_merged pid =
              (Orchestrator.agent t pid).Patch_agent.merged
            in
            let open_deps =
              Graph.open_pr_deps (Orchestrator.graph t) patch_id ~has_merged
            in
            if List.length open_deps <= 1 then begin
              let branch_of pid =
                (Orchestrator.agent t pid).Patch_agent.branch
              in
              let expected_base =
                Graph.initial_base (Orchestrator.graph t) patch_id ~has_merged
                  ~branch_of
                  ~main:(Orchestrator.main_branch t)
              in
              let desired_draft =
                if Branch.equal expected_base (Orchestrator.main_branch t) then
                  not agent.pr_body_delivered
                else true
              in
              if Bool.(agent.is_draft <> desired_draft) then
                effects :=
                  Set_pr_draft { patch_id; pr_number; draft = desired_draft }
                  :: !effects;
              if not (Branch.equal actual_base expected_base) then
                effects :=
                  Set_pr_base { patch_id; pr_number; base = expected_base }
                  :: !effects
            end
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
  | Set_pr_draft { patch_id; draft; _ } ->
      Orchestrator.set_is_draft t patch_id draft
  | Set_pr_base { patch_id; base; _ } ->
      Orchestrator.set_base_branch t patch_id base

let automerge_idle_timeout = 300.0
let automerge_max_failures = 3

(** Pure predicate: a patch is a candidate for automerge merging when automerge
    is enabled, the PR is approved, CI is passing, the queue is empty, and the
    consecutive failure count is under [automerge_max_failures]. New feedback
    (Review_comments, Human, Ci, Merge_conflict, Pr_body) enqueues an operation,
    which fails this check and so resets the deadline. [checks_passing] is
    derived separately from [merge_ready] and captures CI conclusions that
    GitHub's [mergeStateStatus] may consider optional.

    [automerge_inflight] is intentionally NOT checked here: that flag protects
    the reconciler from double-claiming a decision, not the predicate's
    definition of candidacy. Callers that must reject a concurrent claim (i.e.
    [reconcile_automerge]) add the [not inflight] guard themselves; the executor
    re-check in [reconcile_and_execute_automerge] runs while [inflight = true]
    and relies on this predicate returning [true] so long as the underlying
    candidacy still holds. *)
let is_automerge_candidate (agent : Patch_agent.t) ~main_branch =
  agent.Patch_agent.automerge_enabled
  && Patch_agent.is_approved agent ~main_branch
  && agent.Patch_agent.checks_passing
  && List.is_empty agent.Patch_agent.queue
  && agent.Patch_agent.automerge_failure_count < automerge_max_failures

type automerge_decision = {
  merge_patch_id : Patch_id.t;
  merge_pr_number : Pr_number.t;
}
[@@deriving show, eq, sexp_of]

(** Reconcile the automerge deadline for every agent. Returns the updated
    orchestrator and the list of patches whose deadline has now elapsed (the
    caller is responsible for invoking the GitHub merge API for each).

    For each agent:
    - If the patch is [merged], clear any stale deadline/inflight flag — PRs
      merged outside [apply_automerge_success] (manual merge, replacement PR,
      etc.) must not keep a leftover timer in persisted state or the UI.
    - If [automerge_inflight = true], no-op. The executor owns the deadline and
      inflight transitions while a merge call is in progress.
    - If [automerge_enabled = false], [is_automerge_candidate] returns false,
      which funnels into the non-candidate branches below (clearing any stale
      deadline). No separate early-return is needed.
    - If the patch is a candidate and has no deadline, set one at
      [now +. automerge_idle_timeout].
    - If the patch is not a candidate and has a deadline, clear it — any
      feedback resets the timer, so enabling again must wait out a fresh
      5-minute window.
    - If the patch is a candidate and the deadline has elapsed, mark the agent
      [automerge_inflight = true] atomically and include it in the returned
      decision list. The deadline is left in place; clearing it, clearing the
      inflight flag, and advancing the failure counter are the caller's
      responsibility via [apply_automerge_success]/[apply_automerge_failure]. *)
let reconcile_automerge t ~now =
  let agents = Orchestrator.all_agents t in
  let main_branch = Orchestrator.main_branch t in
  List.fold agents ~init:(t, []) ~f:(fun (t, decisions) agent ->
      let patch_id = agent.Patch_agent.patch_id in
      if agent.Patch_agent.merged then
        let t =
          if Option.is_some agent.Patch_agent.automerge_deadline then
            Orchestrator.clear_automerge_deadline t patch_id
          else t
        in
        let t =
          if agent.Patch_agent.automerge_inflight then
            Orchestrator.set_automerge_inflight t patch_id false
          else t
        in
        (t, decisions)
      else if agent.Patch_agent.automerge_inflight then
        (* A merge is already in flight for this patch. Don't touch the
           deadline or issue a second decision — the caller clears the
           inflight flag and advances state via apply_automerge_success /
           apply_automerge_failure on resolution. *)
        (t, decisions)
      else
        let candidate = is_automerge_candidate agent ~main_branch in
        match (candidate, agent.Patch_agent.automerge_deadline) with
        | false, Some _ ->
            (* Feedback arrived, lost approval, CI flipped, or failure cap
               hit: drop the deadline so the next reconcile re-arms a fresh
               idle window once the patch becomes a candidate again. *)
            (Orchestrator.clear_automerge_deadline t patch_id, decisions)
        | false, None -> (t, decisions)
        | true, None ->
            let deadline = now +. automerge_idle_timeout in
            (Orchestrator.set_automerge_deadline t patch_id deadline, decisions)
        | true, Some deadline ->
            if Float.( >= ) now deadline then
              match agent.Patch_agent.pr_number with
              | Some pr_number ->
                  let t = Orchestrator.set_automerge_inflight t patch_id true in
                  ( t,
                    { merge_patch_id = patch_id; merge_pr_number = pr_number }
                    :: decisions )
              | None ->
                  (* Unreachable today because [is_automerge_candidate]
                     requires [is_approved] which requires [has_pr]. Defensive
                     clear so a future predicate change can't leave a patch
                     with a permanently-elapsed deadline that fires every
                     tick. *)
                  (Orchestrator.clear_automerge_deadline t patch_id, decisions)
            else (t, decisions))
  |> fun (t, decisions) -> (t, List.rev decisions)

let apply_automerge_success t patch_id =
  let t = Orchestrator.mark_merged t patch_id in
  let t = Orchestrator.clear_automerge_deadline t patch_id in
  let t = Orchestrator.set_automerge_inflight t patch_id false in
  Orchestrator.reset_automerge_failure_count t patch_id

(** Apply the durable state change that follows a failed merge call. Clears the
    inflight flag and increments the failure counter. Pushes the deadline out by
    [automerge_idle_timeout] so the retry is at least one idle window away — the
    runner may call [reconcile_and_execute_automerge] every tick (~1s), and
    without an explicit push-out a persistent GitHub failure could produce a
    burst of calls within a single poll cycle.

    Skip the deadline re-arm in two terminal cases:
    - The failure cap has now been reached: reconciliation will no longer issue
      merge calls for this patch, so writing a deadline that the next reconcile
      will immediately clear is wasted work and confusing in traces.
    - Automerge has been disabled (the user toggled it off while the merge was
      in flight): writing a deadline would contradict the disabled state and
      stick around until the next tick cleared it. *)
let apply_automerge_failure t ~now patch_id =
  let t = Orchestrator.set_automerge_inflight t patch_id false in
  let t = Orchestrator.increment_automerge_failure_count t patch_id in
  let agent = Orchestrator.agent t patch_id in
  if agent.Patch_agent.automerge_failure_count >= automerge_max_failures then
    (* Clear any existing deadline so the persisted snapshot unambiguously
       reflects the terminal state; the next reconcile would clear it anyway
       via the non-candidate path, but leaving it in place between these two
       events is misleading in traces and on-disk state. *)
    Orchestrator.clear_automerge_deadline t patch_id
  else if not agent.Patch_agent.automerge_enabled then
    (* User disabled automerge while the call was in flight — toggling off
       already cleared the deadline, so nothing more to do here. *)
    t
  else
    Orchestrator.set_automerge_deadline t patch_id
      (now +. automerge_idle_timeout)

let make_orchestrator ~patch_id ~main_branch =
  let patch =
    {
      Patch.id = patch_id;
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

let%test "reconcile_patch enqueues pr_body after PR creation" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let gp =
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
  in
  let t, _ = reconcile_patch t ~project_name:"proj" ~gameplan:gp ~patch in
  let queue = (Orchestrator.agent t pid).Patch_agent.queue in
  List.mem queue Operation_kind.Pr_body ~equal:Operation_kind.equal

let%test "reconcile_patch requests ready-for-review after pr_body on main" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.set_pr_body_delivered t pid true in
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
    | Set_pr_draft _ | Set_pr_base _ -> false)

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
  (* Noop -> Conflict_resolved, agent completed (not busy) *)
  Orchestrator.equal_conflict_rebase_decision decision
    Orchestrator.Conflict_resolved
  && not (Orchestrator.agent t pid).Patch_agent.busy

(* -- Automerge reconciliation tests -- *)

let make_approved_agent t =
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let t = Orchestrator.set_is_draft t pid false in
  let t = Orchestrator.set_merge_ready t pid true in
  let t = Orchestrator.set_checks_passing t pid true in
  let t = Orchestrator.set_pr_body_delivered t pid true in
  Orchestrator.set_automerge_enabled t pid true

let%test "reconcile_automerge clears deadline on merged agent" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t = Orchestrator.mark_merged t pid in
  let t, decisions = reconcile_automerge t ~now:100.0 in
  List.is_empty decisions
  && Option.is_none (Orchestrator.agent t pid).Patch_agent.automerge_deadline

let%test "reconcile_automerge clears inflight on merged agent" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_inflight t pid true in
  let t = Orchestrator.mark_merged t pid in
  let t, decisions = reconcile_automerge t ~now:100.0 in
  List.is_empty decisions
  && not (Orchestrator.agent t pid).Patch_agent.automerge_inflight

let%test "reconcile_automerge skips when checks_passing is false" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_checks_passing t pid false in
  let t, decisions = reconcile_automerge t ~now:1000.0 in
  List.is_empty decisions
  && Option.is_none (Orchestrator.agent t pid).Patch_agent.automerge_deadline

let%test "reconcile_automerge clears deadline when checks_passing flips false" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  (* Pre-arm a deadline so the (false, Some _) clearing branch actually
     fires. Without this the test passes trivially via (false, None). *)
  let t = Orchestrator.set_automerge_deadline t pid 1300.0 in
  let t = Orchestrator.set_checks_passing t pid false in
  let t, decisions = reconcile_automerge t ~now:1000.0 in
  List.is_empty decisions
  && Option.is_none (Orchestrator.agent t pid).Patch_agent.automerge_deadline

let%test "reconcile_automerge marks inflight when firing a decision" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t, decisions = reconcile_automerge t ~now:100.0 in
  List.length decisions = 1
  && (Orchestrator.agent t pid).Patch_agent.automerge_inflight

let%test "reconcile_automerge skips when inflight is true" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t = Orchestrator.set_automerge_inflight t pid true in
  let t, decisions = reconcile_automerge t ~now:100.0 in
  let a = Orchestrator.agent t pid in
  (* No new decision while a merge is in flight, and reconcile leaves the
     inflight flag and deadline alone — the caller owns both via
     apply_automerge_success / apply_automerge_failure. *)
  List.is_empty decisions && a.Patch_agent.automerge_inflight
  && Option.is_some a.Patch_agent.automerge_deadline

let%test "reconcile_automerge skips when failure cap is hit" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  (* Arm a deadline before hitting the cap so the (false, Some _) branch
     actually fires and we verify the deadline gets cleared — otherwise the
     test passes trivially via (false, None). *)
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t =
    Fn.apply_n_times ~n:automerge_max_failures
      (fun t -> Orchestrator.increment_automerge_failure_count t pid)
      t
  in
  let t, decisions = reconcile_automerge t ~now:1000.0 in
  List.is_empty decisions
  && Option.is_none (Orchestrator.agent t pid).Patch_agent.automerge_deadline

let%test "apply_automerge_success clears inflight and resets failures" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_inflight t pid true in
  let t = Orchestrator.increment_automerge_failure_count t pid in
  let t = apply_automerge_success t pid in
  let a = Orchestrator.agent t pid in
  a.Patch_agent.merged
  && (not a.Patch_agent.automerge_inflight)
  && a.Patch_agent.automerge_failure_count = 0
  && Option.is_none a.Patch_agent.automerge_deadline

let%test "apply_automerge_failure bumps deadline one idle window out" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t = Orchestrator.set_automerge_inflight t pid true in
  let now = 1000.0 in
  let t = apply_automerge_failure t ~now pid in
  let a = Orchestrator.agent t pid in
  a.Patch_agent.automerge_failure_count = 1
  && (not a.Patch_agent.automerge_inflight)
  &&
  match a.Patch_agent.automerge_deadline with
  | Some d -> Float.( = ) d (now +. automerge_idle_timeout)
  | None -> false

let%test "apply_automerge_failure clears deadline once cap is reached" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  (* Prime the counter so this failure hits the cap. *)
  let t =
    Fn.apply_n_times
      ~n:(automerge_max_failures - 1)
      (fun t -> Orchestrator.increment_automerge_failure_count t pid)
      t
  in
  (* Pre-arm a deadline so we can verify it's actively cleared, not just
     skipped. *)
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t = Orchestrator.set_automerge_inflight t pid true in
  let t = apply_automerge_failure t ~now:1000.0 pid in
  let a = Orchestrator.agent t pid in
  a.Patch_agent.automerge_failure_count = automerge_max_failures
  && (not a.Patch_agent.automerge_inflight)
  && Option.is_none a.Patch_agent.automerge_deadline

let%test "apply_automerge_failure does not re-arm when automerge is disabled" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.set_automerge_deadline t pid 1.0 in
  let t = Orchestrator.set_automerge_inflight t pid true in
  (* User disables automerge while the merge call is in flight. Toggling
     clears the deadline and inflight flag; the late failure then resolves. *)
  let t = Orchestrator.set_automerge_enabled t pid false in
  let t = apply_automerge_failure t ~now:1000.0 pid in
  let a = Orchestrator.agent t pid in
  (not a.Patch_agent.automerge_enabled)
  && (not a.Patch_agent.automerge_inflight)
  && Option.is_none a.Patch_agent.automerge_deadline

let%test "toggling automerge resets failure count and inflight" =
  let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = make_approved_agent t in
  let t = Orchestrator.increment_automerge_failure_count t pid in
  let t = Orchestrator.increment_automerge_failure_count t pid in
  let t = Orchestrator.set_automerge_inflight t pid true in
  (* Disable → enable *)
  let t = Orchestrator.set_automerge_enabled t pid false in
  let t = Orchestrator.set_automerge_enabled t pid true in
  let a = Orchestrator.agent t pid in
  a.Patch_agent.automerge_enabled
  && a.Patch_agent.automerge_failure_count = 0
  && not a.Patch_agent.automerge_inflight
