(* @archlint.module core
   @archlint.domain display-status *)

open Base
open Types

(** Pure projection of agent activity, reconciliation commands, and waiting
    reasons. The TUI supplies live agents; historical telemetry can supply a
    reduced [State.Patch_ctx] plus its captured reconciliation owner. *)

type t =
  | Merged
  | Wontdo
  | Needs_help
  | In_merge_queue
  | Approved_idle
  | Approved_running
  | Fixing_ci
  | Addressing_review
  | Addressing_findings
  | Resolving_conflict
  | Responding_to_human
  | Writing_pr_body
  | Cleaning_worktree
  | Rebasing
  | Starting
  | Updating
  | Ci_queued
  | Review_queued
  | Findings_queued
  | Cleanup_queued
  | Awaiting_feedback
  | Waiting_for_integration_root
  | Blocked_by_dep
  | Pending
  | Human_queued
  | Conflict_queued
  | Pr_body_queued
  | Rebase_queued
  | Waiting_for_ci
  | Waiting_for_review
  | Waiting_for_forge
  | Ci_failed
  | Conflict
  | Draft
  | Ready_for_integration
  | Reconciling
  | Checking_branch
  | Verifying_publication
  | Integrating
  | Repairing_branch
  | Publishing
  | Waiting_to_retry
[@@deriving show, eq, sexp_of, compare, yojson]

(** Migration-aware deserializer: the removed [Awaiting_ci]/[Awaiting_review]
    constructors were collapsed into [Awaiting_feedback], so map them to it when
    loading activity-log transitions persisted by older versions. Shadows the
    derived [t_of_yojson] so every consumer — including the derived
    [Activity_log.Transition_entry] decoder — picks up the migration. *)
let derived_t_of_yojson = t_of_yojson

let t_of_yojson json =
  match json with
  | `List [ `String ("Awaiting_ci" | "Awaiting_review") ]
  | `String ("Awaiting_ci" | "Awaiting_review") ->
      Awaiting_feedback
  | json -> derived_t_of_yojson json

let is_on_main ctx ~patch_id ~main_branch =
  match State.Patch_ctx.base_branch ctx ~patch_id with
  | Some b -> Branch.equal b main_branch
  | None -> true

(** Feedback facts are interpreted only after active and queued work. A passing
    CI flag is the poller's complete, pagination-aware verdict. *)
let feedback_status ~confirmed ~has_conflict ~checks_passing ~ci_checks
    ~review_decision ~unresolved_comment_count ~is_draft =
  if not confirmed then Waiting_for_forge
  else if has_conflict then Conflict
  else if List.exists ci_checks ~f:Ci_check.is_failure then Ci_failed
  else if not checks_passing then Waiting_for_ci
  else if
    Pr_state.review_blocking review_decision || unresolved_comment_count > 0
  then Waiting_for_review
  else if is_draft then Draft
  else Awaiting_feedback

(** The scheduled operation is an intent, not evidence of a Git mutation.
    Display the owner's current command so inspection cannot look like rebase.
*)
let reconciliation_status owner =
  if not (Branch_reconcile.is_unsettled owner) then None
  else
    Some
      (match Branch_reconcile.pending owner with
      | Some
          {
            kind =
              Observe | Inspect | Verify_recovery | Plan_remote_replay _ | Pin _;
            _;
          } ->
          Checking_branch
      | Some { kind = Integrate { policy = Rewrite; _ }; _ } -> Rebasing
      | Some
          {
            kind =
              ( Integrate { policy = Preserve_ancestry; _ }
              | Checkout_remote _ | Commit_merge _ | Continue _ );
            _;
          } ->
          Integrating
      | Some { kind = Publish _; _ } -> Publishing
      | Some { kind = Confirm _; _ } -> Verifying_publication
      | None -> (
          match Branch_reconcile.phase owner with
          | Some (Waiting _) -> Waiting_to_retry
          | Some Publishing -> Publishing
          | Some Confirming -> Verifying_publication
          | Some (Repairing _) -> Repairing_branch
          | Some (Intervention _) -> Needs_help
          | Some Preparing -> Checking_branch
          | Some Integrating -> Integrating
          | Some (Recovering | Settled) | None -> Reconciling))

let queued_status = function
  | Operation_kind.Uncommitted_changes -> Cleanup_queued
  | Rebase -> Rebase_queued
  | Human -> Human_queued
  | Merge_conflict -> Conflict_queued
  | Ci -> Ci_queued
  | Review_comments -> Review_queued
  | Findings -> Findings_queued
  | Pr_body -> Pr_body_queued

let derive ?(feedback = Awaiting_feedback) ?reconciliation
    (ctx : State.Patch_ctx.t) ~patch_id ~(current_op : Operation_kind.t option)
    ~(main_branch : Branch.t) =
  if State.Patch_ctx.is_merged ctx ~patch_id then Merged
  else if State.Patch_ctx.is_wontdo ctx ~patch_id then Wontdo
  else if State.Patch_ctx.needs_intervention ctx ~patch_id then Needs_help
  else if State.Patch_ctx.is_busy ctx ~patch_id then
    match current_op with
    | Some Uncommitted_changes -> Cleaning_worktree
    | Some Ci -> Fixing_ci
    | Some Review_comments -> Addressing_review
    | Some Findings -> Addressing_findings
    | Some Merge_conflict -> Resolving_conflict
    | Some Human -> Responding_to_human
    | Some Pr_body -> Writing_pr_body
    | Some Rebase -> Option.value reconciliation ~default:Checking_branch
    | None ->
        Option.value reconciliation
          ~default:
            (if State.Patch_ctx.has_pr ctx ~patch_id then Updating else Starting)
  else
    match reconciliation with
    | Some status when not (equal status Waiting_to_retry) -> status
    | Some _ | None -> (
        if State.Patch_ctx.is_enqueued ctx ~patch_id then In_merge_queue
        else
          let queued =
            List.filter
              Operation_kind.
                [
                  Uncommitted_changes;
                  Rebase;
                  Human;
                  Merge_conflict;
                  Ci;
                  Review_comments;
                  Findings;
                  Pr_body;
                ]
              ~f:(fun kind -> State.Patch_ctx.is_queued ctx ~patch_id ~kind)
            |> List.min_elt ~compare:(fun a b ->
                Int.compare (Priority.priority a) (Priority.priority b))
          in
          match queued with
          | Some kind -> queued_status kind
          | None when Option.equal equal reconciliation (Some Waiting_to_retry)
            ->
              Waiting_to_retry
          | None ->
              if State.Patch_ctx.is_approved ctx ~patch_id then Approved_idle
              else if State.Patch_ctx.has_pr ctx ~patch_id then
                if not (is_on_main ctx ~patch_id ~main_branch) then
                  Blocked_by_dep
                else feedback
              else Pending)

(** Use the agent's authoritative predicates rather than reconstructing
    readiness from a subset of flags in each rendering consumer. *)
let of_agent ~execution_mode ~main_branch (agent : Patch_agent.t) =
  let patch_id = agent.patch_id in
  let has_change =
    Patch_agent.has_pr agent
    || Execution_mode.branch_only_published execution_mode agent
  in
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_merged ~patch_id ~value:agent.merged
    |> State.Patch_ctx.set_wontdo ~patch_id
         ~value:(Option.is_some agent.wontdo_reason)
    |> State.Patch_ctx.set_needs_intervention ~patch_id
         ~value:(Patch_agent.needs_intervention agent || agent.branch_blocked)
    |> State.Patch_ctx.set_busy ~patch_id ~value:agent.busy
    |> State.Patch_ctx.set_has_pr ~patch_id ~value:has_change
    |> State.Patch_ctx.set_approved ~patch_id
         ~value:(Patch_agent.is_approved agent ~main_branch)
    |> State.Patch_ctx.set_enqueued ~patch_id
         ~value:(Option.is_some agent.merge_queue_entry)
  in
  let ctx =
    match agent.base_branch with
    | Some branch -> State.Patch_ctx.set_base_branch ctx ~patch_id ~branch
    | None -> ctx
  in
  let ctx =
    List.fold agent.queue ~init:ctx ~f:(fun ctx kind ->
        State.Patch_ctx.set_queued ctx ~patch_id ~kind ~value:true)
  in
  let feedback =
    feedback_status
      ~confirmed:
        ((not (Patch_agent.has_pr agent))
        || Patch_agent.forge_revision_pair_confirmed agent)
      ~has_conflict:(Patch_agent.has_conflict agent)
      ~checks_passing:agent.checks_passing ~ci_checks:agent.ci_checks
      ~review_decision:agent.review_decision
      ~unresolved_comment_count:agent.unresolved_comment_count
      ~is_draft:agent.is_draft
  in
  derive ~feedback
    ?reconciliation:(reconciliation_status agent.branch_reconcile)
    ctx ~patch_id ~current_op:agent.current_op ~main_branch

let%test "merged takes priority over everything" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_merged ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_needs_intervention ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal Merged
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "needs_help over approved" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_needs_intervention ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal Needs_help
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "in_merge_queue over approved" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_enqueued ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal In_merge_queue
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "needs_help over in_merge_queue" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_needs_intervention ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_enqueued ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal Needs_help
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "merged over in_merge_queue" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_merged ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_enqueued ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal Merged
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "approved idle" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
  in
  equal Approved_idle
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "active operation takes priority over approval" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Checking_branch
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:(Some Rebase)
       ~main_branch:(Branch.of_string "main"))

let%test "busy without an operation shows starting" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_approved ~patch_id:(Patch_id.of_string "1")
         ~value:true
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Starting
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "busy with ci op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Fixing_ci
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:(Some Ci)
       ~main_branch:(Branch.of_string "main"))

let%test "busy with uncommitted changes op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Cleaning_worktree
    (derive ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Uncommitted_changes)
       ~main_branch:(Branch.of_string "main"))

let%test "busy with review op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Addressing_review
    (derive ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Review_comments) ~main_branch:(Branch.of_string "main"))

let%test "busy with merge conflict op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Resolving_conflict
    (derive ctx ~patch_id:(Patch_id.of_string "1")
       ~current_op:(Some Merge_conflict) ~main_branch:(Branch.of_string "main"))

let%test "busy with human op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Responding_to_human
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:(Some Human)
       ~main_branch:(Branch.of_string "main"))

let%test "busy with rebase op" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Checking_branch
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:(Some Rebase)
       ~main_branch:(Branch.of_string "main"))

let%test "busy no op with pr falls back to updating" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Updating
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "busy no op without pr falls back to starting" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_busy ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Starting
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "ci queued" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_queued ~patch_id:(Patch_id.of_string "1") ~kind:Ci
         ~value:true
  in
  equal Ci_queued
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "uncommitted changes queued" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_queued ~patch_id:(Patch_id.of_string "1")
         ~kind:Uncommitted_changes ~value:true
  in
  equal Cleanup_queued
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "review queued" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_queued ~patch_id:(Patch_id.of_string "1")
         ~kind:Review_comments ~value:true
  in
  equal Review_queued
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "awaiting feedback with prior ci failures" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_ci_failure_count ~patch_id:(Patch_id.of_string "1")
         ~count:2
  in
  equal Awaiting_feedback
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "awaiting feedback default" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
  in
  equal Awaiting_feedback
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "blocked by dep when base_branch is not main" =
  let ctx =
    State.Patch_ctx.empty
    |> State.Patch_ctx.set_has_pr ~patch_id:(Patch_id.of_string "1") ~value:true
    |> State.Patch_ctx.set_base_branch ~patch_id:(Patch_id.of_string "1")
         ~branch:(Branch.of_string "feature/dep")
  in
  equal Blocked_by_dep
    (derive ctx ~patch_id:(Patch_id.of_string "1") ~current_op:None
       ~main_branch:(Branch.of_string "main"))

let%test "pending is default" =
  equal Pending
    (derive State.Patch_ctx.empty ~patch_id:(Patch_id.of_string "1")
       ~current_op:None ~main_branch:(Branch.of_string "main"))

(* Backward-compat: snapshots persisted before the collapse stored the removed
   [Awaiting_ci]/[Awaiting_review] constructors in their activity-log
   transitions. The compat [t_of_yojson] must map both legacy forms (canonical
   [`List [`String _]] and bare [`String _]) to [Awaiting_feedback] so those
   snapshots still load. *)
let%test "legacy Awaiting_ci decodes to Awaiting_feedback" =
  equal Awaiting_feedback (t_of_yojson (`List [ `String "Awaiting_ci" ]))
  && equal Awaiting_feedback (t_of_yojson (`String "Awaiting_ci"))

let%test "legacy Awaiting_review decodes to Awaiting_feedback" =
  equal Awaiting_feedback (t_of_yojson (`List [ `String "Awaiting_review" ]))
  && equal Awaiting_feedback (t_of_yojson (`String "Awaiting_review"))

let%test "current statuses round-trip through yojson" =
  List.for_all
    [
      Awaiting_feedback;
      Cleanup_queued;
      Cleaning_worktree;
      Ci_queued;
      Merged;
      Pending;
      Needs_help;
      In_merge_queue;
    ] ~f:(fun s -> equal s (t_of_yojson (yojson_of_t s)))
