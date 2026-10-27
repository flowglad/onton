open Base
open Types

type input = {
  graph : Graph.t;
  mode : Execution_mode.t;
  agents : Patch_agent.t list;
  main : Branch.t;
}

type decision =
  | Start of Patch_id.t * Branch.t
  | Respond of Patch_id.t * Operation_kind.t
  | Rebase of Patch_id.t * Branch.t
  | Reconcile_branch of Patch_id.t * int

let find v id =
  List.find v.agents ~f:(fun a -> Patch_id.equal a.Patch_agent.patch_id id)

let merged v id =
  Option.value_map (find v id) ~default:false ~f:(fun a -> a.merged)

let branch v id = Option.map (find v id) ~f:(fun a -> a.Patch_agent.branch)

let branch_only v id =
  Option.value_map (find v id) ~default:false
    ~f:(Execution_mode.branch_only_published v.mode)

let selected_base v id =
  (* Feature bases may traverse the root as well as direct dependencies.
     Never synthesize an unknown peer's branch from [main]. *)
  let ids = Graph.all_patch_ids v.graph in
  if
    (not (List.mem ids id ~equal:Patch_id.equal))
    || Option.value_map (Execution_mode.root v.mode) ~default:false
         ~f:(fun root -> not (List.mem ids root ~equal:Patch_id.equal))
    || List.exists ids ~f:(fun peer -> Option.is_none (find v peer))
  then None
  else
    let has_merged = merged v in
    match
      Execution_mode.base v.mode v.graph id ~has_merged
        ~branch_of:(fun p -> Option.value_exn (branch v p))
        ~main:v.main
    with
    | Some b -> Some b
    | None ->
        (* Only the terminal base is meaningful when multiple open dependencies
           prevent selecting a single immediate base. *)
        None

let open_dependency_ready a =
  a.Patch_agent.pr_body_delivered
  && Patch_agent.forge_revision_pair_confirmed a
  && Option.is_none (Branch_reconcile.unobserved_publication a.branch_reconcile)
  && (not (Branch_reconcile.is_unsettled a.branch_reconcile))
  && (not (Patch_agent.has_conflict a))
  && a.Patch_agent.checks_passing

let in_graph v id =
  let ids = Graph.all_patch_ids v.graph in
  List.mem ids id ~equal:Patch_id.equal
  && Option.value_map (Execution_mode.root v.mode) ~default:true ~f:(fun root ->
      List.mem ids root ~equal:Patch_id.equal)

let dependencies_admitted v id =
  if not (in_graph v id) then false
  else
    let deps = Graph.deps v.graph id in
    match find v id with
    | None -> false
    | Some agent -> (
        List.for_all deps ~f:(fun d -> Option.is_some (find v d))
        && Graph.merge_deps_satisfied v.graph id ~has_merged:(merged v)
        && List.length
             (Execution_mode.open_deps v.mode v.graph id ~has_merged:(merged v))
           <= 1
        &&
        match Patch_agent.worktree_state agent with
        | Patch_agent.Materialized _ -> true
        | Patch_agent.Unmaterialized ->
            Execution_mode.deps_satisfied v.mode v.graph id
              ~has_merged:(merged v) ~has_pr:(fun d ->
                Option.value_map (find v d) ~default:false ~f:(fun a ->
                    Patch_agent.is_pr_present a || branch_only v d))
            && List.for_all
                 (Graph.open_pr_deps v.graph id ~has_merged:(merged v))
                 ~f:(fun d ->
                   Option.value_map (find v d) ~default:false ~f:(fun a ->
                       open_dependency_ready a
                       || branch_only v d && a.pr_body_delivered
                          && a.checks_passing
                          && (not (Patch_agent.has_conflict a))
                          && (not
                                (Branch_reconcile.is_unsettled
                                   a.branch_reconcile))
                          && Option.is_none
                               (Branch_reconcile.unobserved_publication
                                  a.branch_reconcile))))

let base_mismatch v id a =
  a.Patch_agent.native_stack
  && Option.value_map (selected_base v id) ~default:false ~f:(fun expected ->
      Option.value_map a.base_branch ~default:false ~f:(fun actual ->
          not (Branch.equal actual expected)))

let for_patch v id =
  if not (in_graph v id) then None
  else
    match find v id with
    | None -> None
    | Some a ->
        let present = Patch_agent.is_pr_present a || branch_only v id in
        if Option.is_some (Patch_agent.reconciliation_hold_reason a) then None
        else if Branch_reconcile.is_pending a.branch_reconcile then
          if a.busy || a.automerge_inflight then None
          else
            Option.map (Branch_reconcile.operation a.branch_reconcile)
              ~f:(fun op -> Reconcile_branch (id, op.id))
        else if
          Option.is_some a.wontdo_reason
          || a.automerge_inflight
          || base_mismatch v id a
             && not
                  (List.mem a.queue Operation_kind.Human
                     ~equal:Operation_kind.equal)
        then None
        else if
          (not (Patch_agent.has_pr a || branch_only v id))
          && (not a.busy) && (not a.merged)
          && (not (Patch_agent.needs_intervention a))
          && dependencies_admitted v id
        then Option.map (selected_base v id) ~f:(fun b -> Start (id, b))
        else if
          present
          && Option.is_some (selected_base v id)
          && (not a.merged) && (not a.busy) && (not a.branch_blocked)
          && (not (Branch_reconcile.is_unsettled a.branch_reconcile))
          && Option.value_map
               (Patch_agent.highest_priority a)
               ~default:false
               ~f:(Operation_kind.equal Operation_kind.Rebase)
        then Option.map (selected_base v id) ~f:(fun b -> Rebase (id, b))
        else if
          present && (not a.merged) && (not a.busy)
          && (not (Patch_agent.needs_intervention a))
          && not a.branch_blocked
        then
          Option.bind (Patch_agent.highest_priority a) ~f:(fun kind ->
              if Priority.is_feedback kind then Some (Respond (id, kind))
              else None)
        else None

let plan v =
  List.map (Graph.all_patch_ids v.graph) ~f:(fun id -> (id, for_patch v id))

let ready_for_review v id =
  if not (in_graph v id) then false
  else
    match find v id with
    | None -> false
    | Some a ->
        let pending =
          List.exists v.agents ~f:(fun a -> a.Patch_agent.automerge_inflight)
        in
        let expected = selected_base v id in
        Patch_agent.forge_revision_pair_confirmed a
        && (not (Branch_reconcile.is_unsettled a.branch_reconcile))
        && Option.is_none
             (Branch_reconcile.unobserved_publication a.branch_reconcile)
        && List.length
             (Execution_mode.open_deps v.mode v.graph id ~has_merged:(merged v))
           <= 1
        && (not (Execution_mode.is_descendant v.mode id))
        && ((not (Execution_mode.is_root v.mode id))
           || Option.equal Branch.equal a.base_branch (Some v.main)
              && Execution_mode.root_ready v.mode v.graph ~has_merged:(merged v)
                   ~pending_integrations:pending a)
        && Option.value_map expected ~default:false ~f:(Branch.equal v.main)
        && a.pr_body_delivered
        && (not (Patch_agent.has_conflict a))
        && Option.value_map expected ~default:false ~f:(fun b ->
            Option.value_map
              (Patch_agent.branch_rebased_onto a)
              ~default:false ~f:(Branch.equal b))
        && a.checks_passing

let evidence_id v id =
  Option.map (find v id) ~f:(fun a ->
      let scope =
        String.concat ~sep:":"
          [
            Patch_id.to_string id;
            Int.to_string a.Patch_agent.generation;
            (if Patch_agent.forge_revision_pair_confirmed a then "confirmed"
             else "unconfirmed");
            Option.value a.head_oid ~default:"";
            Option.value_map a.base_branch ~default:"" ~f:Branch.to_string;
            Option.value_map (Patch_agent.pr_number a) ~default:"" ~f:(fun n ->
                Int.to_string (Pr_number.to_int n));
          ]
      in
      Stdlib.Digest.to_hex (Stdlib.Digest.string scope))

let draft_allowed v id =
  match find v id with
  | None -> false
  | Some a ->
      a.is_draft
      && Scheduler_recovery.write_allowed a.scheduler_recovery
           Scheduler_recovery.Pr_draft
      && (not (base_mismatch v id a))
      && ready_for_review v id

type recovery_intent = { pr_base_update : bool; pr_draft_update : bool }

let recovery_intent v ~gameplan_patch id =
  match find v id with
  | None -> { pr_base_update = false; pr_draft_update = false }
  | Some a ->
      let pr_base_update =
        (not a.native_stack)
        && Option.value_map (selected_base v id) ~default:false
             ~f:(fun expected ->
               Option.value_map a.base_branch ~default:false ~f:(fun actual ->
                   not (Branch.equal actual expected)))
      in
      let pr_draft_update =
        gameplan_patch && a.is_draft
        && (Scheduler_recovery.contains a.scheduler_recovery
              Scheduler_recovery.Pr_draft
           || (ready_for_review v id && not (base_mismatch v id a)))
      in
      { pr_base_update; pr_draft_update }

let automerge_candidate ?(ignore_inflight = false) a ~main =
  (ignore_inflight || not a.Patch_agent.automerge_inflight)
  && (not a.merged) && a.automerge_enabled && (not a.native_stack)
  && Patch_agent.is_approved a ~main_branch:main
  && Option.is_none a.merge_queue_entry
  && a.checks_passing && List.is_empty a.queue
  && a.automerge_failure_count < 3
