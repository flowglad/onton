(* @archlint.module core
   @archlint.domain execution-mode *)
open Base
open Types

type feature_branch = { root : Patch_id.t; publication : Patch_id.t option }
[@@deriving sexp_of, show, eq]

type t = Mainline | Feature_branch of feature_branch
[@@deriving sexp_of, show, eq]

let mainline = Mainline
let root = function Mainline -> None | Feature_branch f -> Some f.root
let is_root t id = Option.equal Patch_id.equal (root t) (Some id)

let is_descendant t id =
  match t with
  | Mainline -> false
  | Feature_branch f ->
      (not (Patch_id.equal f.root id))
      && not (Option.equal Patch_id.equal f.publication (Some id))

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
      Ok (Feature_branch { root = id; publication = None })
  | [] ->
      Error
        "--feature-branch requires a nonempty gameplan with one dependency root"
  | _ ->
      Error
        "--feature-branch requires exactly one dependency root reachable from \
         every patch"

let restore_inferred inferred = function
  | None -> Ok Mainline
  | Some id -> (
      match inferred with
      | Ok (Feature_branch f as mode) when Patch_id.equal f.root id -> Ok mode
      | Ok Mainline | Ok (Feature_branch _) | Error _ ->
          Error "Persisted feature branch root does not match the gameplan")

let restore graph root = restore_inferred (infer graph) root

let infer_gameplan (gameplan : Gameplan.t) =
  if
    List.contains_dup gameplan.patches ~compare:(fun p q ->
        Patch_id.compare p.Patch.id q.Patch.id)
  then Error "Feature branch gameplan has duplicate patch IDs"
  else
    let graph = Graph.of_gameplan gameplan in
    match gameplan.publication with
    | None -> infer graph
    | Some _ -> (
        (* Publication is a merge prerequisite, outside the integration tree.
         Select the original implementation root and keep Patch 0 on the
         ordinary PR path throughout construction. *)
        let publication = Gameplan.publication_patch_id in
        match
          List.find gameplan.patches ~f:(fun p ->
              Patch_id.equal p.id publication)
        with
        | Some p when List.is_empty p.dependencies ->
            Result.map
              (infer (Graph.remove_patch graph publication))
              ~f:(function
                | Mainline -> Mainline
                | Feature_branch f ->
                    Feature_branch { f with publication = Some publication })
        | Some _ | None -> Error "Invalid gameplan publication prerequisite")

let restore_gameplan gameplan root =
  restore_inferred (infer_gameplan gameplan) root

let terminal t id ~branch_of ~main =
  match t with
  | Feature_branch f when is_descendant t id -> branch_of f.root
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
  Graph.merge_deps_satisfied graph id ~has_merged
  && List.length (open_deps t graph id ~has_merged) <= 1
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
  && a.pr_body_delivered && (not a.merged) && (not a.busy)
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
  && not a.pr_body_refresh.Patch_agent.pending

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
    | Some expected -> (
        match observed with
        | None -> true
        | Some head ->
            (not (String.equal head expected))
            && Option.equal String.equal (Some head) a.head_oid)
  else Patch_decision.defer_remote_head a observed
