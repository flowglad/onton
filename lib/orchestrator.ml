open Base
open Types

(** Orchestrator state: the dependency graph plus per-patch agent state.

    Encodes the spec fragment:
    {v
    > The orchestrator ensures liveness: all actions that can fire, do fire.
    > It wires together the state store, patch agents, poller, and reconciler.
    v} *)

type t = {
  graph : Graph.t;
  agents : Patch_agent.t Map.M(Patch_id).t;
  main_branch : Branch.t;
}

let create ~patches ~main_branch =
  let graph = Graph.of_patches patches in
  let agents =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) ->
        match
          Map.add acc ~key:p.Patch.id ~data:(Patch_agent.create p.Patch.id)
        with
        | `Ok m -> m
        | `Duplicate ->
            invalid_arg
              (Printf.sprintf "Orchestrator.create: duplicate patch id %s"
                 (Patch_id.to_string p.Patch.id)))
  in
  { graph; agents; main_branch }

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
  | Some a -> { t with agents = Map.set t.agents ~key:patch_id ~data:(f a) }

let has_merged t pid = (agent t pid).Patch_agent.merged
let has_pr t pid = (agent t pid).Patch_agent.has_pr

let branch_map_of_patches patches =
  List.fold patches
    ~init:(Map.empty (module Patch_id))
    ~f:(fun acc (p : Patch.t) ->
      match Map.add acc ~key:p.Patch.id ~data:p.Patch.branch with
      | `Ok m -> m
      | `Duplicate ->
          invalid_arg
            (Printf.sprintf "Orchestrator: duplicate patch id in tick input %s"
               (Patch_id.to_string p.Patch.id)))

(** {2 Liveness: fire all actions whose preconditions hold} *)

type action =
  | Start of Patch_id.t * Branch.t
  | Respond of Patch_id.t * Operation_kind.t
  | Rebase of Patch_id.t * Branch.t
[@@deriving sexp_of]

let startable_patches t ~branch_map =
  Graph.all_patch_ids t.graph
  |> List.filter_map ~f:(fun pid ->
      let a = agent t pid in
      if
        (not a.Patch_agent.has_pr) && (not a.Patch_agent.busy)
        && (not a.Patch_agent.merged)
        && (not a.Patch_agent.removed)
        && Graph.deps_satisfied t.graph pid
             ~has_merged:(fun pid ->
               has_merged t pid && not (agent t pid).Patch_agent.removed)
             ~has_pr:(fun pid ->
               has_pr t pid && not (agent t pid).Patch_agent.removed)
      then
        let has_merged_excluding_removed pid =
          has_merged t pid && not (agent t pid).Patch_agent.removed
        in
        let branch_of pid =
          match Map.find branch_map pid with
          | Some b -> b
          | None ->
              invalid_arg
                (Printf.sprintf
                   "Orchestrator.startable_patches: no branch for patch %s"
                   (Patch_id.to_string pid))
        in
        let base =
          Graph.initial_base t.graph pid
            ~has_merged:has_merged_excluding_removed ~branch_of
            ~main:t.main_branch
        in
        Some (Start (pid, base))
      else None)

let rebaseable_patches t ~branch_map =
  Graph.all_patch_ids t.graph
  |> List.filter_map ~f:(fun pid ->
      let (a : Patch_agent.t) = agent t pid in
      if
        a.Patch_agent.has_pr && (not a.Patch_agent.merged)
        && (not a.Patch_agent.removed)
        && (not a.Patch_agent.busy)
        && List.mem a.Patch_agent.queue Operation_kind.Rebase
             ~equal:Operation_kind.equal
      then
        match Patch_agent.highest_priority a with
        | Some hp when Operation_kind.equal hp Operation_kind.Rebase ->
            let has_merged_excluding_removed pid =
              has_merged t pid && not (agent t pid).Patch_agent.removed
            in
            let branch_of pid =
              match Map.find branch_map pid with
              | Some b -> b
              | None ->
                  Option.value a.Patch_agent.base_branch ~default:t.main_branch
            in
            let new_base =
              Graph.initial_base t.graph pid
                ~has_merged:has_merged_excluding_removed ~branch_of
                ~main:t.main_branch
            in
            Some (Rebase (pid, new_base))
        | _ -> None
      else None)

let respondable_patches t =
  Graph.all_patch_ids t.graph
  |> List.filter_map ~f:(fun pid ->
      let (a : Patch_agent.t) = agent t pid in
      if
        a.Patch_agent.has_pr && (not a.Patch_agent.merged)
        && (not a.Patch_agent.removed)
        && (not a.Patch_agent.busy)
        && not a.Patch_agent.needs_intervention
      then
        Patch_agent.highest_priority a
        |> Option.bind ~f:(fun k ->
            if Priority.is_feedback k then Some (Respond (pid, k)) else None)
      else None)

let pending_actions t ~patches =
  let branch_map = branch_map_of_patches patches in
  (* Only validate patches that could be started (no PR yet). Ad-hoc patches
     already have PRs and are not in the gameplan's patch list. *)
  let missing =
    Graph.all_patch_ids t.graph
    |> List.filter ~f:(fun pid ->
        (not (Map.mem branch_map pid)) && not (has_pr t pid))
  in
  if not (List.is_empty missing) then
    invalid_arg
      (Printf.sprintf
         "Orchestrator.pending_actions: tick input missing %d patch id(s) from \
          graph"
         (List.length missing));
  startable_patches t ~branch_map
  @ rebaseable_patches t ~branch_map
  @ respondable_patches t

let fire t action =
  match action with
  | Start (pid, base) ->
      update_agent t pid ~f:(fun a -> Patch_agent.start a ~base_branch:base)
  | Respond (pid, k) -> update_agent t pid ~f:(fun a -> Patch_agent.respond a k)
  | Rebase (pid, base) ->
      update_agent t pid ~f:(fun a -> Patch_agent.rebase a ~base_branch:base)

let tick t ~patches =
  let actions = pending_actions t ~patches in
  let t = List.fold actions ~init:t ~f:(fun t a -> fire t a) in
  (t, actions)

(** {2 External event application} *)

let complete t patch_id = update_agent t patch_id ~f:Patch_agent.complete

let enqueue t patch_id kind =
  update_agent t patch_id ~f:(fun a -> Patch_agent.enqueue a kind)

let mark_merged t patch_id = update_agent t patch_id ~f:Patch_agent.mark_merged

let mark_removed t patch_id =
  update_agent t patch_id ~f:Patch_agent.mark_removed

let add_pending_comment t patch_id comment ~valid =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.add_pending_comment a comment ~valid)

let send_human_message t patch_id message =
  let comment =
    Comment.
      {
        id = Comment_id.next_synthetic ();
        body = message;
        path = None;
        line = None;
      }
  in
  let t = update_agent t patch_id ~f:Patch_agent.clear_needs_intervention in
  let t = add_pending_comment t patch_id comment ~valid:true in
  enqueue t patch_id Operation_kind.Human

let set_pr_number t patch_id pr_number =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_pr_number a pr_number)

let set_session_failed t patch_id =
  update_agent t patch_id ~f:Patch_agent.set_session_failed

let set_last_session_id t patch_id session_id =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.set_last_session_id a session_id)

let set_tried_fresh t patch_id =
  update_agent t patch_id ~f:Patch_agent.set_tried_fresh

let clear_session_fallback t patch_id =
  update_agent t patch_id ~f:Patch_agent.clear_session_fallback

let set_has_conflict t patch_id =
  update_agent t patch_id ~f:Patch_agent.set_has_conflict

let increment_ci_failure_count t patch_id =
  update_agent t patch_id ~f:Patch_agent.increment_ci_failure_count

let clear_needs_intervention t patch_id =
  update_agent t patch_id ~f:Patch_agent.clear_needs_intervention

let set_ci_checks t patch_id checks =
  update_agent t patch_id ~f:(fun a -> Patch_agent.set_ci_checks a checks)

let add_addressed_comment_id t patch_id id =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.add_addressed_comment_id a id)

let reset_busy t patch_id = update_agent t patch_id ~f:Patch_agent.reset_busy

(** {2 Queries} *)

let all_agents t = Map.data t.agents
let graph t = t.graph
let restore ~graph ~agents ~main_branch = { graph; agents; main_branch }
let main_branch t = t.main_branch
let agents_map t = t.agents

let add_agent t ~patch_id ~pr_number =
  if Map.mem t.agents patch_id then t
  else
    let agent = Patch_agent.create_adhoc ~patch_id ~pr_number in
    let graph = Graph.add_patch t.graph patch_id in
    { t with graph; agents = Map.set t.agents ~key:patch_id ~data:agent }
