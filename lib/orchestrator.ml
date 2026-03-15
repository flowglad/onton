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
        Map.set acc ~key:p.Patch.id ~data:(Patch_agent.create p.Patch.id))
  in
  { graph; agents; main_branch }

let agent t patch_id = Map.find_exn t.agents patch_id

let update_agent t patch_id ~f =
  {
    t with
    agents = Map.update t.agents patch_id ~f:(fun a -> f (Option.value_exn a));
  }

let has_merged t pid = (agent t pid).Patch_agent.merged
let has_pr t pid = (agent t pid).Patch_agent.has_pr

let branch_of ~patches pid =
  let p =
    List.find_exn patches ~f:(fun (p : Patch.t) ->
        Patch_id.equal p.Patch.id pid)
  in
  p.Patch.branch

(** {2 Liveness: fire all actions whose preconditions hold} *)

type action =
  | Start of Patch_id.t * Branch.t
  | Respond of Patch_id.t * Operation_kind.t
[@@deriving sexp_of]

let startable_patches t ~patches =
  Graph.all_patch_ids t.graph
  |> List.filter_map ~f:(fun pid ->
      let a = agent t pid in
      if
        (not a.Patch_agent.has_pr)
        && Graph.deps_satisfied t.graph pid ~has_merged:(has_merged t)
             ~has_pr:(has_pr t)
      then
        let base =
          Graph.initial_base t.graph pid ~has_merged:(has_merged t)
            ~branch_of:(branch_of ~patches) ~main:t.main_branch
        in
        Some (Start (pid, base))
      else None)

let respondable_patches t =
  Graph.all_patch_ids t.graph
  |> List.filter_map ~f:(fun pid ->
      let (a : Patch_agent.t) = agent t pid in
      if
        a.Patch_agent.has_pr && (not a.Patch_agent.merged)
        && (not a.Patch_agent.busy)
        && not a.Patch_agent.needs_intervention
      then
        match Patch_agent.highest_priority a with
        | Some k -> Some (Respond (pid, k))
        | None -> None
      else None)

let pending_actions t ~patches =
  startable_patches t ~patches @ respondable_patches t

let fire t action =
  match action with
  | Start (pid, base) ->
      update_agent t pid ~f:(fun a -> Patch_agent.start a ~base_branch:base)
  | Respond (pid, k) -> update_agent t pid ~f:(fun a -> Patch_agent.respond a k)

let tick t ~patches =
  let actions = pending_actions t ~patches in
  let t = List.fold actions ~init:t ~f:(fun t a -> fire t a) in
  (t, actions)

(** {2 External event application} *)

let complete t patch_id = update_agent t patch_id ~f:Patch_agent.complete

let enqueue t patch_id kind =
  update_agent t patch_id ~f:(fun a -> Patch_agent.enqueue a kind)

let mark_merged t patch_id = update_agent t patch_id ~f:Patch_agent.mark_merged

let add_pending_comment t patch_id comment ~valid =
  update_agent t patch_id ~f:(fun a ->
      Patch_agent.add_pending_comment a comment ~valid)

let set_session_failed t patch_id =
  update_agent t patch_id ~f:Patch_agent.set_session_failed

let set_has_conflict t patch_id =
  update_agent t patch_id ~f:Patch_agent.set_has_conflict

let increment_ci_failure_count t patch_id =
  update_agent t patch_id ~f:Patch_agent.increment_ci_failure_count

let clear_needs_intervention t patch_id =
  update_agent t patch_id ~f:Patch_agent.clear_needs_intervention

(** {2 Queries} *)

let all_agents t = Map.data t.agents
let graph t = t.graph
