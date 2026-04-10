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

let set_checks_passing t patch_id v =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_checks_passing a v)

let set_merge_ready t patch_id v =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_merge_ready a v)

let set_is_draft t patch_id v =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_is_draft a v)

let set_pr_description_applied t patch_id v =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.set_pr_description_applied a v)

let set_implementation_notes_delivered t patch_id v =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.set_implementation_notes_delivered a v)

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

let set_head_branch t patch_id branch =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_head_branch a branch)

(** {2 Queries} *)

let all_agents t = Map.data t.agents
let graph t = t.graph

let restore ~graph ~agents ~outbox ~main_branch =
  let outbox = Map.filter outbox ~f:(fun msg -> Map.mem agents msg.patch_id) in
  { graph; agents; outbox; main_branch }

let main_branch t = t.main_branch
let set_main_branch t branch = { t with main_branch = branch }
let agents_map t = t.agents

let add_agent t ~patch_id ~branch ~pr_number =
  if Map.mem t.agents patch_id then t
  else
    let agent = Patch_agent.create_adhoc ~patch_id ~branch ~pr_number in
    let graph = Graph.add_patch t.graph patch_id in
    { t with graph; agents = Map.set t.agents ~key:patch_id ~data:agent }

type rebase_effect = Push_branch [@@deriving show, eq, sexp_of]

let apply_rebase_result t patch_id rebase_result new_base =
  match rebase_result with
  | Worktree.Ok ->
      let t = set_base_branch t patch_id new_base in
      let t = clear_has_conflict t patch_id in
      (complete t patch_id, [ Push_branch ])
  | Worktree.Noop ->
      let t = set_base_branch t patch_id new_base in
      (* Push even on Noop: the local branch may already be rebased from a
         prior attempt whose push failed, leaving the remote stale. *)
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
  | Some (Worktree.Push_error _) ->
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
      let t = clear_has_conflict t patch_id in
      let t = complete t patch_id in
      (t, Conflict_resolved, [ Push_branch ])
  | Worktree.Noop ->
      (* Local branch already contains the target — just push to sync
         the remote.  There is no in-progress rebase to hand to the
         agent, so complete instead of delivering.  If GitHub still
         reports a conflict after the push, the poller will re-enqueue
         Merge_conflict, and conflict_noop_count will eventually trigger
         intervention. *)
      let t = set_base_branch t patch_id new_base in
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
      (None | Some (Worktree.Push_rejected | Worktree.Push_error _)) ) ->
      let t = clear_has_conflict t patch_id in
      let t = set_has_conflict t patch_id in
      let t = enqueue t patch_id Operation_kind.Merge_conflict in
      (t, Conflict_retry_push)
  | ( Deliver_to_agent,
      ( None
      | Some
          ( Worktree.Push_ok | Worktree.Push_up_to_date | Worktree.Push_rejected
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
[@@deriving show, eq, sexp_of]

(** Complete a failed session, preserving human messages. When
    [current_op = Human], [complete] clears [human_messages] because it assumes
    the messages were delivered. On failure they were NOT delivered, so we save
    them before [complete] and restore + re-enqueue afterwards. *)
let complete_failed t patch_id =
  let agent = agent t patch_id in
  let was_human =
    Option.equal Operation_kind.equal agent.Patch_agent.current_op
      (Some Operation_kind.Human)
  in
  let saved_messages = agent.Patch_agent.human_messages in
  let t = complete t patch_id in
  if was_human && not (List.is_empty saved_messages) then
    let t =
      update_agent t patch_id ~f:(fun a ->
          Patch_agent.restore_human_messages a saved_messages)
    in
    enqueue t patch_id Operation_kind.Human
  else t

let apply_session_result t patch_id result =
  match result with
  | Session_ok -> clear_session_fallback t patch_id
  | Session_process_error { is_fresh } ->
      let t = on_session_failure t patch_id ~is_fresh in
      let t = update_agent t patch_id ~f:Patch_agent.on_pre_session_failure in
      complete_failed t patch_id
  | Session_no_resume ->
      let t = on_session_failure t patch_id ~is_fresh:false in
      complete_failed t patch_id
  | Session_failed { is_fresh } ->
      let t = on_session_failure t patch_id ~is_fresh in
      complete_failed t patch_id
  | Session_give_up ->
      let t = set_session_failed t patch_id in
      let t = set_tried_fresh t patch_id in
      complete_failed t patch_id
  | Session_worktree_missing ->
      let t = update_agent t patch_id ~f:Patch_agent.on_pre_session_failure in
      complete_failed t patch_id

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
[@@deriving show, eq, sexp_of]

let apply_respond_outcome t patch_id kind outcome =
  match outcome with
  | Respond_stale -> t
  | Respond_failed -> complete t patch_id
  | Respond_retry_push -> complete t patch_id
  | Respond_ok ->
      let t = complete t patch_id in
      let t =
        if Operation_kind.equal kind Operation_kind.Merge_conflict then
          clear_has_conflict t patch_id
        else t
      in
      if Operation_kind.equal kind Operation_kind.Implementation_notes then
        set_implementation_notes_delivered t patch_id true
      else t
