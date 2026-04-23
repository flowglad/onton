open Base
open Types

(** Orchestrator state: the dependency graph plus per-patch agent state.

    Encodes the spec fragment:
    {v
    > The orchestrator ensures liveness: all actions that can fire, do fire.
    > It wires together the state store, patch agents, poller, and reconciler.
    v} *)

type message_status = Pending | Acked | Completed | Obsolete
[@@deriving sexp_of, show, eq]

type action =
  | Start of Patch_id.t * Branch.t
  | Respond of Patch_id.t * Operation_kind.t
  | Rebase of Patch_id.t * Branch.t
[@@deriving sexp_of, show, eq]

type patch_agent_message = {
  message_id : Message_id.t;
  patch_id : Patch_id.t;
  generation : int;
  action : action;
  payload_hash : string;
  status : message_status;
}
[@@deriving sexp_of, show, eq]

type t = {
  graph : Graph.t;
  agents : Patch_agent.t Map.M(Patch_id).t;
  outbox : patch_agent_message Map.M(Message_id).t;
  main_branch : Branch.t;
}

let create ~patches ~main_branch =
  let graph = Graph.of_patches patches in
  let agents =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) ->
        match
          Map.add acc ~key:p.Patch.id
            ~data:(Patch_agent.create ~branch:p.Patch.branch p.Patch.id)
        with
        | `Ok m -> m
        | `Duplicate ->
            invalid_arg
              (Printf.sprintf "Orchestrator.create: duplicate patch id %s"
                 (Patch_id.to_string p.Patch.id)))
  in
  { graph; agents; outbox = Map.empty (module Message_id); main_branch }

let agent t patch_id =
  match Map.find t.agents patch_id with
  | Some a -> a
  | None ->
      invalid_arg
        (Printf.sprintf "Orchestrator.agent: unknown patch_id %s"
           (Patch_id.to_string patch_id))

let find_agent t patch_id = Map.find t.agents patch_id

let update_agent t patch_id ~f =
  match Map.find t.agents patch_id with
  | None -> t
  | Some a ->
      let a' = f a in
      let a' =
        if Patch_agent.equal a a' then a' else Patch_agent.bump_generation a'
      in
      { t with agents = Map.set t.agents ~key:patch_id ~data:a' }

let fire t action =
  match action with
  | Start (pid, base) ->
      let a = agent t pid in
      if Patch_agent.has_pr a || a.Patch_agent.busy then t
      else
        update_agent t pid ~f:(fun a -> Patch_agent.start a ~base_branch:base)
  | Respond (pid, k) -> update_agent t pid ~f:(fun a -> Patch_agent.respond a k)
  | Rebase (pid, base) ->
      update_agent t pid ~f:(fun a -> Patch_agent.rebase a ~base_branch:base)

(** {2 External event application} *)

let refresh_base_branch t patch_id =
  match find_agent t patch_id with
  | None -> t
  | Some _ ->
      let has_merged pid =
        match find_agent t pid with
        | Some a -> a.Patch_agent.merged
        | None -> false
      in
      let open_deps = Graph.open_pr_deps t.graph patch_id ~has_merged in
      (* When more than 1 dep is still open, we cannot determine a unique
         base branch yet — skip the refresh until enough deps merge. *)
      if List.length open_deps > 1 then t
      else
        let branch_of pid =
          match find_agent t pid with
          | Some a -> a.Patch_agent.branch
          | None -> t.main_branch
        in
        let fresh =
          Graph.initial_base t.graph patch_id ~has_merged ~branch_of
            ~main:t.main_branch
        in
        update_agent t patch_id ~f:(fun a ->
            Patch_agent.set_base_branch a fresh)

let complete t patch_id =
  (* Refresh base_branch before transitioning to idle — a dependency may
     have merged while this patch was busy, leaving base_branch stale. *)
  let t = refresh_base_branch t patch_id in
  let current_message_id =
    match find_agent t patch_id with
    | None -> None
    | Some agent -> agent.current_message_id
  in
  let t = update_agent t patch_id ~f:Patch_agent.complete in
  match current_message_id with
  | None -> t
  | Some message_id -> (
      match Map.find t.outbox message_id with
      | None -> t
      | Some msg ->
          {
            t with
            outbox =
              Map.set t.outbox ~key:message_id
                ~data:{ msg with status = Completed };
          })

let enqueue t patch_id kind =
  update_agent t patch_id ~f:(fun a -> Patch_agent.enqueue a kind)

let mark_merged t patch_id =
  let t = update_agent t patch_id ~f:Patch_agent.mark_merged in
  (* Eagerly update base_branch for direct dependents so it is never
     stale when Merge_conflict fires. *)
  let dependents = Graph.dependents t.graph patch_id in
  List.fold dependents ~init:t ~f:(fun t dep_id ->
      match find_agent t dep_id with
      | None -> t
      | Some _ -> refresh_base_branch t dep_id)

let remove_agent t patch_id =
  {
    t with
    graph = Graph.remove_patch t.graph patch_id;
    agents = Map.remove t.agents patch_id;
    outbox =
      Map.filter t.outbox ~f:(fun msg ->
          not (Patch_id.equal msg.patch_id patch_id));
  }

let reconcile_message t msg =
  let t =
    Map.fold t.outbox ~init:t ~f:(fun ~key ~data acc ->
        if
          Patch_id.equal data.patch_id msg.patch_id
          && equal_message_status data.status Pending
          && not (Message_id.equal key msg.message_id)
        then
          {
            acc with
            outbox =
              Map.set acc.outbox ~key ~data:{ data with status = Obsolete };
          }
        else acc)
  in
  match Map.find t.outbox msg.message_id with
  | Some { status = Pending | Acked | Completed; _ } -> t
  | Some { status = Obsolete; _ } | None ->
      { t with outbox = Map.set t.outbox ~key:msg.message_id ~data:msg }

let mark_message_obsolete t message_id =
  match Map.find t.outbox message_id with
  | None -> t
  | Some msg ->
      {
        t with
        outbox =
          Map.set t.outbox ~key:message_id ~data:{ msg with status = Obsolete };
      }

let mark_patch_pending_messages_obsolete_except t patch_id ~keep =
  let keep = Set.of_list (module Message_id) keep in
  Map.fold t.outbox ~init:t ~f:(fun ~key ~data acc ->
      if
        Patch_id.equal data.patch_id patch_id
        && equal_message_status data.status Pending
        && not (Set.mem keep key)
      then
        {
          acc with
          outbox = Map.set acc.outbox ~key ~data:{ data with status = Obsolete };
        }
      else acc)

let find_message t message_id = Map.find t.outbox message_id
let all_messages t = Map.data t.outbox
let message_id (msg : patch_agent_message) = msg.message_id
let message_patch_id (msg : patch_agent_message) = msg.patch_id
let message_action (msg : patch_agent_message) = msg.action
let message_status (msg : patch_agent_message) = msg.status

let current_message t patch_id =
  match (agent t patch_id).Patch_agent.current_message_id with
  | None -> None
  | Some message_id -> Map.find t.outbox message_id

let runnable_messages t =
  let action_rank = function
    | Start _ -> 0
    | Rebase _ -> 1
    | Respond (_, kind) -> Priority.priority kind
  in
  Map.data t.outbox
  |> List.filter ~f:(fun msg ->
      match msg.status with
      | Pending -> true
      | Acked ->
          let agent = agent t msg.patch_id in
          Option.equal Message_id.equal agent.current_message_id
            (Some msg.message_id)
          && not agent.busy
      | Completed | Obsolete -> false)
  |> List.sort ~compare:(fun a b ->
      Int.compare (action_rank a.action) (action_rank b.action))

let accept_message t message_id =
  match Map.find t.outbox message_id with
  | None -> (t, None)
  | Some msg -> (
      match msg.status with
      | Acked | Completed | Obsolete -> (t, None)
      | Pending ->
          let agent = agent t msg.patch_id in
          if agent.Patch_agent.generation <> msg.generation then
            let t = mark_message_obsolete t message_id in
            (t, None)
          else
            let t = fire t msg.action in
            let t =
              update_agent t msg.patch_id ~f:(fun a ->
                  Patch_agent.set_current_message_id a (Some message_id))
            in
            let msg = { msg with status = Acked } in
            ( { t with outbox = Map.set t.outbox ~key:message_id ~data:msg },
              Some msg.action ))

let resume_message t message_id =
  match Map.find t.outbox message_id with
  | None -> (t, None)
  | Some msg -> (
      match msg.status with
      | Pending | Completed | Obsolete -> (t, None)
      | Acked ->
          let agent = agent t msg.patch_id in
          if
            Option.equal Message_id.equal agent.current_message_id
              (Some message_id)
            && not agent.busy
          then
            let op =
              match msg.action with
              | Start _ -> None
              | Respond (_, kind) -> Some kind
              | Rebase _ -> Some Operation_kind.Rebase
            in
            ( update_agent t msg.patch_id
                ~f:(Patch_agent.resume_current_message ~op),
              Some msg.action )
          else (t, None))

let send_human_message t patch_id message =
  let t = update_agent t patch_id ~f:Patch_agent.reset_intervention_state in
  let t = refresh_base_branch t patch_id in
  let t =
    update_agent t patch_id ~f:(fun a ->
        Patch_agent.add_human_message a message)
  in
  enqueue t patch_id Operation_kind.Human

let set_pr_number t patch_id pr_number =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_pr_number a pr_number)

let clear_pr t patch_id = update_agent t patch_id ~f:Patch_agent.clear_pr

let set_session_failed t patch_id =
  update_agent t patch_id ~f:Patch_agent.set_session_failed

let set_tried_fresh t patch_id =
  update_agent t patch_id ~f:Patch_agent.set_tried_fresh

let clear_session_fallback t patch_id =
  update_agent t patch_id ~f:Patch_agent.clear_session_fallback

let on_session_failure t patch_id ~is_fresh =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.on_session_failure a ~is_fresh)

let on_pr_discovery_failure t patch_id =
  update_agent t patch_id ~f:Patch_agent.on_pr_discovery_failure

let set_has_conflict t patch_id =
  update_agent t patch_id ~f:Patch_agent.set_has_conflict

let clear_has_conflict t patch_id =
  update_agent t patch_id ~f:Patch_agent.clear_has_conflict

let reset_conflict_noop_count t patch_id =
  update_agent t patch_id ~f:Patch_agent.reset_conflict_noop_count

let set_base_branch t patch_id branch =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_base_branch a branch)

let set_notified_base_branch t patch_id branch =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.set_notified_base_branch a branch)

let increment_ci_failure_count t patch_id =
  update_agent t patch_id ~f:Patch_agent.increment_ci_failure_count

let reset_ci_failure_count t patch_id =
  update_agent t patch_id ~f:Patch_agent.reset_ci_failure_count

let set_ci_checks t patch_id checks =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_ci_checks a checks)

let record_delivered_ci_run_ids t patch_id ids =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.record_delivered_ci_run_ids a ids)

let set_checks_passing t patch_id v =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_checks_passing a v)

let set_merge_ready t patch_id v =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_merge_ready a v)

let set_is_draft t patch_id v =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_is_draft a v)

let set_pr_body_delivered t patch_id v =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_pr_body_delivered a v)

let increment_conflict_noop_count t patch_id =
  update_agent t patch_id ~f:Patch_agent.increment_conflict_noop_count

let increment_start_attempts_without_pr t patch_id =
  update_agent t patch_id ~f:Patch_agent.increment_start_attempts_without_pr

let reset_intervention_state t patch_id =
  let t = update_agent t patch_id ~f:Patch_agent.reset_intervention_state in
  refresh_base_branch t patch_id

let set_branch_blocked t patch_id =
  update_agent t patch_id ~f:Patch_agent.set_branch_blocked

let clear_branch_blocked t patch_id =
  update_agent t patch_id ~f:Patch_agent.clear_branch_blocked

let reset_busy t patch_id = update_agent t patch_id ~f:Patch_agent.reset_busy

let set_worktree_path t patch_id path =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_worktree_path a path)

let set_llm_session_id t patch_id session_id =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.set_llm_session_id a session_id)

let set_automerge_enabled t patch_id v =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_automerge_enabled a v)

let set_automerge_deadline t patch_id deadline =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.set_automerge_deadline a deadline)

let clear_automerge_deadline t patch_id =
  update_agent t patch_id ~f:Patch_agent.clear_automerge_deadline

let set_automerge_inflight t patch_id v =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_automerge_inflight a v)

let increment_automerge_failure_count t patch_id =
  update_agent t patch_id ~f:Patch_agent.increment_automerge_failure_count

let reset_automerge_failure_count t patch_id =
  update_agent t patch_id ~f:Patch_agent.reset_automerge_failure_count

(** {2 Queries} *)

let all_agents t = Map.data t.agents
let graph t = t.graph

let restore ~graph ~agents ~outbox ~main_branch =
  let outbox = Map.filter outbox ~f:(fun msg -> Map.mem agents msg.patch_id) in
  { graph; agents; outbox; main_branch }

let main_branch t = t.main_branch
let set_main_branch t branch = { t with main_branch = branch }
let agents_map t = t.agents

let find_patch_by_branch t branch =
  Map.to_alist t.agents
  |> List.find ~f:(fun (_, a) -> Branch.equal a.Patch_agent.branch branch)
  |> Option.map ~f:fst

let add_agent t ~patch_id ~branch ~base_branch ~pr_number =
  if Map.mem t.agents patch_id then t
  else
    let deps =
      if Branch.equal base_branch t.main_branch then []
      else
        match find_patch_by_branch t base_branch with
        | Some dep_pid -> (
            match find_agent t dep_pid with
            | Some a when not a.Patch_agent.merged -> [ dep_pid ]
            | _ -> [])
        | None -> []
    in
    (* Do not seed agent.base_branch: persistence infers branch_rebased_onto
       from base_branch when the former is absent, which would fabricate a
       stale-rebase state on round-trip. The poller populates it next tick. *)
    let agent = Patch_agent.create_adhoc ~patch_id ~branch ~pr_number in
    let graph = Graph.add_patch_with_deps t.graph patch_id ~deps in
    { t with graph; agents = Map.set t.agents ~key:patch_id ~data:agent }

type rebase_effect = Push_branch [@@deriving show, eq, sexp_of]

let apply_rebase_result t patch_id rebase_result new_base =
  match rebase_result with
  | Worktree.Ok ->
      (* TODO: consider calling [reset_intervention_state] on a successful
         rebase — a fresh base may well have resolved the CI failure, noop
         cycle, or other condition that latched [needs_intervention]. Left
         conservative for now: rebase only clears conflict-specific state. *)
      let t = set_base_branch t patch_id new_base in
      let t =
        update_agent t patch_id ~f:(fun a ->
            Patch_agent.set_branch_rebased_onto a new_base)
      in
      let t = clear_has_conflict t patch_id in
      let t = reset_conflict_noop_count t patch_id in
      (complete t patch_id, [ Push_branch ])
  | Worktree.Noop ->
      let t = set_base_branch t patch_id new_base in
      (* Noop: the local branch already contains [new_base] in its history,
         so the branch is (transitively) based on it. Record this so the
         drift detector doesn't re-fire. Push even on Noop — the remote may
         be stale from a prior failed push. *)
      let t =
        update_agent t patch_id ~f:(fun a ->
            Patch_agent.set_branch_rebased_onto a new_base)
      in
      (complete t patch_id, [ Push_branch ])
  | Worktree.Conflict ->
      let t = set_base_branch t patch_id new_base in
      let t = set_has_conflict t patch_id in
      let t = enqueue t patch_id Operation_kind.Merge_conflict in
      (complete t patch_id, [])
  | Worktree.Error _ ->
      let t = set_session_failed t patch_id in
      let t = set_tried_fresh t patch_id in
      (complete t patch_id, [])

type rebase_push_resolution =
  | Rebase_push_ok
  | Rebase_push_failed
  | Rebase_push_error
[@@deriving show, eq, sexp_of]

let apply_rebase_push_result t patch_id
    (push_outcome : Worktree.push_result option) =
  match push_outcome with
  | None -> (t, Rebase_push_ok) (* no push effect emitted; already handled *)
  | Some Worktree.Push_ok | Some Worktree.Push_up_to_date -> (t, Rebase_push_ok)
  | Some Worktree.Push_rejected ->
      let t = set_has_conflict t patch_id in
      let t = enqueue t patch_id Operation_kind.Merge_conflict in
      (t, Rebase_push_failed)
  | Some Worktree.Push_no_commits | Some (Worktree.Push_error _) ->
      (* Push_no_commits after rebase means every commit squashed away — treat
         as an infrastructure error and retry the rebase. *)
      let t = enqueue t patch_id Operation_kind.Rebase in
      (t, Rebase_push_error)

type conflict_rebase_decision =
  | Conflict_resolved
  | Deliver_to_agent
  | Conflict_failed
[@@deriving show, eq, sexp_of]

let apply_conflict_rebase_result t patch_id rebase_result new_base =
  match rebase_result with
  | Worktree.Ok ->
      let t = set_base_branch t patch_id new_base in
      let t =
        update_agent t patch_id ~f:(fun a ->
            Patch_agent.set_branch_rebased_onto a new_base)
      in
      let t = clear_has_conflict t patch_id in
      let t = reset_conflict_noop_count t patch_id in
      let t = complete t patch_id in
      (t, Conflict_resolved, [ Push_branch ])
  | Worktree.Noop ->
      (* Local branch already contains the target — just push to sync
         the remote.  Clear has_conflict so it purely tracks GitHub
         state; the poller will re-set it and re-enqueue Merge_conflict
         if the conflict persists, and conflict_noop_count will
         eventually trigger intervention. *)
      let t = set_base_branch t patch_id new_base in
      let t =
        update_agent t patch_id ~f:(fun a ->
            Patch_agent.set_branch_rebased_onto a new_base)
      in
      let t = clear_has_conflict t patch_id in
      let t = increment_conflict_noop_count t patch_id in
      let t = complete t patch_id in
      (t, Conflict_resolved, [ Push_branch ])
  | Worktree.Conflict ->
      let t = set_base_branch t patch_id new_base in
      let t = set_has_conflict t patch_id in
      (t, Deliver_to_agent, [])
  | Worktree.Error _ ->
      let t = set_session_failed t patch_id in
      let t = complete t patch_id in
      (t, Conflict_failed, [])

type conflict_resolution =
  | Conflict_done
  | Conflict_retry_push
  | Conflict_needs_agent
  | Conflict_give_up
[@@deriving show, eq, sexp_of]

let apply_conflict_push_result t patch_id decision
    (push_outcome : Worktree.push_result option) =
  match (decision, push_outcome) with
  | Conflict_resolved, Some Worktree.Push_ok -> (t, Conflict_done)
  | Conflict_resolved, Some Worktree.Push_up_to_date -> (t, Conflict_done)
  | ( Conflict_resolved,
      ( None
      | Some
          ( Worktree.Push_rejected | Worktree.Push_no_commits
          | Worktree.Push_error _ ) ) ) ->
      let t = set_has_conflict t patch_id in
      let t = enqueue t patch_id Operation_kind.Merge_conflict in
      (t, Conflict_retry_push)
  | ( Deliver_to_agent,
      ( None
      | Some
          ( Worktree.Push_ok | Worktree.Push_up_to_date
          | Worktree.Push_no_commits | Worktree.Push_rejected
          | Worktree.Push_error _ ) ) ) ->
      (t, Conflict_needs_agent)
  | Conflict_failed, _ -> (t, Conflict_give_up)

type session_result =
  | Session_ok
  | Session_process_error of { is_fresh : bool }
  | Session_no_resume
  | Session_failed of { is_fresh : bool }
  | Session_give_up
  | Session_worktree_missing
  | Session_push_failed
  | Session_no_commits
[@@deriving show, eq, sexp_of]

(** Complete a failed session, restoring inflight human messages to the inbox.
    On failure the messages were NOT delivered, so we prepend
    [inflight_human_messages] back to [human_messages] before [complete] clears
    the inflight slot. *)
let complete_failed t patch_id =
  let a = agent t patch_id in
  let inflight = a.Patch_agent.inflight_human_messages in
  let t =
    if not (List.is_empty inflight) then
      update_agent t patch_id ~f:(fun a ->
          Patch_agent.add_human_messages a inflight)
    else t
  in
  let t = complete t patch_id in
  let has_messages =
    not (List.is_empty (agent t patch_id).Patch_agent.human_messages)
  in
  if has_messages then enqueue t patch_id Operation_kind.Human else t

let apply_session_result t patch_id result =
  match result with
  | Session_ok ->
      let t = clear_session_fallback t patch_id in
      (* A healthy session that pushed commits clears the no-commits counter. *)
      update_agent t patch_id ~f:Patch_agent.reset_no_commits_push_count
  | Session_process_error { is_fresh } ->
      let t = on_session_failure t patch_id ~is_fresh in
      let t = update_agent t patch_id ~f:Patch_agent.on_pre_session_failure in
      complete_failed t patch_id
  | Session_no_resume ->
      let t = on_session_failure t patch_id ~is_fresh:false in
      let t =
        update_agent t patch_id ~f:(fun a ->
            Patch_agent.set_llm_session_id a None)
      in
      complete_failed t patch_id
  | Session_failed { is_fresh } ->
      let t = on_session_failure t patch_id ~is_fresh in
      complete_failed t patch_id
  | Session_give_up ->
      let t = set_session_failed t patch_id in
      let t = set_tried_fresh t patch_id in
      let t =
        update_agent t patch_id ~f:(fun a ->
            Patch_agent.set_llm_session_id a None)
      in
      complete_failed t patch_id
  | Session_worktree_missing ->
      let t = update_agent t patch_id ~f:Patch_agent.on_pre_session_failure in
      complete_failed t patch_id
  | Session_push_failed ->
      (* The LLM session itself ran cleanly — clear its fallback state so we
         resume the same session next iteration. The push failure does NOT
         use [complete_failed]: the session succeeded, so any inflight human
         messages were already delivered to the LLM and must not be restored
         to the inbox.  Completion is deferred to [apply_respond_outcome]
         which calls plain [complete] via [Respond_retry_push].  Using
         [complete_failed] here caused an infinite loop: messages were
         re-enqueued, the Human operation re-dispatched, the session
         re-delivered the same messages, the push failed again, ad
         infinitum — with [clear_session_fallback] preventing escalation
         and the Human-in-queue exemption in [needs_intervention]
         preventing the circuit breaker from firing. *)
      clear_session_fallback t patch_id
  | Session_no_commits ->
      (* The LLM session ran cleanly but left no commits on the branch (HEAD
         == base), so the supervisor skipped the push. Clear session fallback
         (the LLM itself was healthy), bump the no-commits counter (which
         feeds [needs_intervention] at >= 2).  Like [Session_push_failed],
         do NOT call [complete_failed] — the session succeeded and any
         inflight human messages were delivered.  Completion is handled by
         [apply_respond_outcome] via [Respond_retry_push]. *)
      let t = clear_session_fallback t patch_id in
      update_agent t patch_id ~f:Patch_agent.increment_no_commits_push_count

let combine_session_and_push ~(session : session_result)
    ~(push : Worktree.push_result) : session_result =
  match session with
  | Session_ok -> (
      match push with
      | Worktree.Push_ok | Worktree.Push_up_to_date -> Session_ok
      | Worktree.Push_no_commits -> Session_no_commits
      | Worktree.Push_rejected | Worktree.Push_error _ -> Session_push_failed)
  | Session_process_error _ | Session_no_resume | Session_failed _
  | Session_give_up | Session_worktree_missing | Session_push_failed
  | Session_no_commits ->
      session

type start_outcome = Start_ok | Start_failed | Start_stale
[@@deriving show, eq, sexp_of]

let apply_start_outcome t patch_id outcome =
  match outcome with
  | Start_stale -> t
  | Start_ok ->
      (* Caller must complete explicitly after PR discovery finishes,
         so busy=true is held throughout the network call. *)
      t
  | Start_failed -> complete t patch_id

type respond_outcome =
  | Respond_ok
  | Respond_failed
  | Respond_retry_push
  | Respond_stale
  | Respond_skip_empty
  | Respond_pr_body_miss
      (** Pr_body session finished cleanly but the PR body was not durably
          delivered. Two cases:
          - artifact outcome [`Missing | `Empty] AND a Write tool_use did not
            complete — evidence the agent was blocked mid-call (e.g. OpenCode's
            [--dir] sandbox rejecting a write outside the worktree); or
          - artifact outcome [`Patch_failed] — the notes were written but the
            subsequent GitHub [update_pr_body] call failed, leaving the PR
            description stale. Does NOT flip [pr_body_delivered]; instead
            increments [pr_body_artifact_miss_count] and lets the reconciler
            re-enqueue. At cap (>=2) the agent surfaces via
            [needs_intervention]. *)
[@@deriving show, eq, sexp_of]

let apply_respond_outcome t patch_id kind outcome =
  match outcome with
  | Respond_stale -> t
  | Respond_failed -> complete_failed t patch_id
  | Respond_retry_push -> complete t patch_id
  | Respond_skip_empty -> complete t patch_id
  | Respond_pr_body_miss ->
      let t = complete t patch_id in
      update_agent t patch_id
        ~f:Patch_agent.increment_pr_body_artifact_miss_count
  | Respond_ok ->
      let t = complete t patch_id in
      (* Only count CI fix attempts that actually delivered a payload with
         failure conclusions. Cancelled/pending/success-only runs yield
         [Respond_skip_empty] above and do not pollute the cap. *)
      let t =
        if Operation_kind.equal kind Operation_kind.Ci then
          increment_ci_failure_count t patch_id
        else t
      in
      let t =
        if Operation_kind.equal kind Operation_kind.Merge_conflict then
          let t = clear_has_conflict t patch_id in
          reset_conflict_noop_count t patch_id
        else t
      in
      if Operation_kind.equal kind Operation_kind.Pr_body then
        let t = set_pr_body_delivered t patch_id true in
        update_agent t patch_id ~f:Patch_agent.reset_pr_body_artifact_miss_count
      else t
