(* @archlint.module core
   @archlint.domain execution-mode *)
open Base
open Types

type t = Mainline | Feature_branch of Patch_id.t
[@@deriving sexp_of, show, eq]

let mainline = Mainline
let root = function Mainline -> None | Feature_branch id -> Some id
let is_root t id = Option.equal Patch_id.equal (root t) (Some id)
let is_descendant t id = Option.is_some (root t) && not (is_root t id)

let infer graph =
  match
    List.filter (Graph.all_patch_ids graph) ~f:(fun id ->
        List.is_empty (Graph.deps graph id))
  with
  | [ id ]
    when List.for_all (Graph.all_patch_ids graph) ~f:(fun p ->
             Patch_id.equal id p
             || List.mem
                  (Graph.transitive_ancestors graph p)
                  id ~equal:Patch_id.equal) ->
      Ok (Feature_branch id)
  | [] ->
      Error
        "--feature-branch requires a nonempty gameplan with one dependency root"
  | _ ->
      Error
        "--feature-branch requires exactly one dependency root reachable from \
         every patch"

let restore graph = function
  | None -> Ok Mainline
  | Some id -> (
      match infer graph with
      | Ok (Feature_branch inferred as mode) when Patch_id.equal inferred id ->
          Ok mode
      | Ok Mainline | Ok (Feature_branch _) | Error _ ->
          Error "Persisted feature branch root does not match the gameplan")

let terminal t id ~branch_of ~main =
  match t with
  | Feature_branch r when not (Patch_id.equal r id) -> branch_of r
  | Mainline | Feature_branch _ -> main

let open_deps t graph id ~has_merged =
  Graph.open_pr_deps graph id ~has_merged
  |> List.filter ~f:(fun d -> not (is_root t d))

let base t graph id ~has_merged ~branch_of ~main =
  match open_deps t graph id ~has_merged with
  | [] -> Some (terminal t id ~branch_of ~main)
  | [ d ] -> Some (branch_of d)
  | _ -> None

let deps_satisfied t graph id ~has_merged ~has_pr =
  List.length (open_deps t graph id ~has_merged) <= 1
  && List.for_all (Graph.deps graph id) ~f:(fun d -> has_merged d || has_pr d)

let additions_allowed t graph ~construction_open ~dependencies =
  match root t with
  | None -> true
  | Some r ->
      construction_open
      && List.exists dependencies ~f:(fun d ->
          Patch_id.equal d r
          || List.mem
               (Graph.transitive_ancestors graph d)
               r ~equal:Patch_id.equal)

let descendants_complete t graph ~has_merged =
  Option.is_some (root t)
  && List.for_all (Graph.all_patch_ids graph) ~f:(fun id ->
      is_root t id || has_merged id)

let integration_ready t ~construction_open ~ignore_inflight ~max_failures
    ~terminal (a : Patch_agent.t) =
  is_descendant t a.patch_id && construction_open && a.branch_published
  && (not a.merged) && (not a.busy)
  && (not (Patch_agent.needs_intervention a))
  && (not a.branch_blocked) && (not a.native_stack) && a.automerge_enabled
  && (ignore_inflight || not a.automerge_inflight)
  && a.automerge_failure_count < max_failures
  && a.checks_passing && (not a.has_conflict)
  && a.unresolved_comment_count = 0
  && List.is_empty a.queue
  && Option.is_none a.current_op
  && Option.is_some a.head_oid
  && Option.is_none a.expected_remote_head_oid
  && Option.equal Branch.equal a.base_branch (Some terminal)
  && Option.equal Branch.equal a.branch_rebased_onto a.base_branch

let root_ready t graph ~has_merged ~pending_integrations (a : Patch_agent.t) =
  is_root t a.patch_id
  && descendants_complete t graph ~has_merged
  && (not a.busy) && List.is_empty a.queue
  && Option.is_none a.current_op
  && (not (Patch_agent.needs_intervention a))
  && a.unresolved_comment_count = 0
  && (not a.mergeability_unknown)
  && Option.is_some a.head_oid
  && Option.is_none a.expected_remote_head_oid
  && (not pending_integrations) && (not a.has_conflict) && a.checks_passing
  && a.pr_body_delivered

let validate_terminal t ~branch_of ~main =
  match root t with
  | Some r when Branch.equal (branch_of r) main ->
      Error
        "Integration root branch must differ from the repository main branch"
  | None | Some _ -> Ok ()

let observation_pending t (a : Patch_agent.t) observed =
  if is_root t a.patch_id || is_descendant t a.patch_id then
    match a.expected_remote_head_oid with
    | None -> false
    | Some expected -> not (Option.equal String.equal (Some expected) observed)
  else Patch_decision.defer_remote_head a observed
