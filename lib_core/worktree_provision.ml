(* @archlint.module core
   @archlint.domain branch-reconcile *)

type failure = Temporary of string | Unsafe of string

let message = function
  | Temporary "" | Unsafe "" -> "checkout_unavailable"
  | Temporary reason | Unsafe reason -> reason

let reconciliation_result failure =
  let reason = message failure in
  match failure with
  | Temporary _ -> Branch_reconcile.Retryable { reason; retry_after = None }
  | Unsafe _ -> Branch_reconcile.Needs_diagnosis reason

let materialization_failure reason =
  if Git_observation.materialization_failure_is_unsafe reason then Unsafe reason
  else Temporary reason

let creation_failure refusal =
  let reason = Start_point_plan.short_label (Start_point_plan.Refuse refusal) in
  match refusal with
  | Start_point_plan.Ancestry_unavailable _ -> Temporary reason
  | Start_point_plan.Branch_checked_out_in_main_root
  | Start_point_plan.Worktree_already_registered _ ->
      Unsafe reason

type decision =
  | Ready
  | Wait of string
  | Stop of string
  | Run of Branch_reconcile.event

let next ~intent ~at state =
  let module B = Branch_reconcile in
  let valid =
    match intent.B.purpose with
    | B.Provision_checkout request -> request <> "" && intent.base <> ""
    | B.Reconcile_base | B.Reconcile_request _ | B.Reconcile_scoped _
    | B.Integrate_revision _ | B.Publish_revision _ | B.Publish_session _
    | B.Verify_publication ->
        false
  in
  if not valid then Stop "provisioning_intent_required"
  else
    match B.operation state with
    | None -> Run (B.Request intent)
    | Some operation -> (
        match operation.B.phase with
        | B.Intervention reason -> Stop reason
        | B.Settled ->
            if B.equal_intent operation.intent intent then Ready
            else Run (B.Request intent)
        | B.Preparing | B.Integrating | B.Repairing _ | B.Publishing
        | B.Confirming | B.Waiting _ | B.Recovering -> (
            if not (B.is_provisioning operation.intent.purpose) then
              Wait "branch_reconciliation_pending"
            else
              match B.wake_event ~at state with
              | Some event -> Run event
              | None -> Wait "checkout_provisioning_pending"))
