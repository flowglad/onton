(* @archlint.module test
   @archlint.domain display-status *)

open Base
open Onton_core

(** Property tests for {!Display_status} — totality + priority dominance.

    Cover state-flag precedence, live-agent feedback, and owner-command
    lifecycles (including no-op reconciliation and checkpoint restoration):

    - {!Display_status.derive} is total (never raises) for any combination.
    - The priority ladder dominates: e.g. [Merged] beats every other flag, and
      [Needs_help] beats [Approved] which beats [Pending]. *)

(* ─────────────────────────────────────────────────────────────────────────
   Generators
   ───────────────────────────────────────────────────────────────────────── *)

let patch_id = Types.Patch_id.of_string "1"
let main_branch = Types.Branch.of_string "main"
let gen_bool = QCheck2.Gen.bool
let gen_int_small = QCheck2.Gen.int_range 0 5

let gen_op =
  let open QCheck2.Gen in
  oneof_list
    [
      None;
      Some Types.Operation_kind.Uncommitted_changes;
      Some Findings;
      Some Types.Operation_kind.Ci;
      Some Review_comments;
      Some Merge_conflict;
      Some Human;
      Some Pr_body;
      Some Rebase;
    ]

(* Build a Patch_ctx by replaying setter calls — reaches every reachable state
   shape without poking at private fields. *)
type flag =
  | Set_wontdo of bool
  | Set_merged of bool
  | Set_needs_intervention of bool
  | Set_approved of bool
  | Set_enqueued of bool
  | Set_busy of bool
  | Set_has_pr of bool
  | Set_queued_ci of bool
  | Set_queued_review of bool
  | Set_failure_count of int
  | Set_base_branch of string

let gen_flag =
  let open QCheck2.Gen in
  oneof
    [
      map (fun b -> Set_wontdo b) gen_bool;
      map (fun b -> Set_merged b) gen_bool;
      map (fun b -> Set_needs_intervention b) gen_bool;
      map (fun b -> Set_approved b) gen_bool;
      map (fun b -> Set_enqueued b) gen_bool;
      map (fun b -> Set_busy b) gen_bool;
      map (fun b -> Set_has_pr b) gen_bool;
      map (fun b -> Set_queued_ci b) gen_bool;
      map (fun b -> Set_queued_review b) gen_bool;
      map (fun n -> Set_failure_count n) gen_int_small;
      map (fun s -> Set_base_branch s) (oneof_list [ "main"; "feature/foo" ]);
    ]

let apply_flag ctx = function
  | Set_wontdo value -> State.Patch_ctx.set_wontdo ctx ~patch_id ~value
  | Set_merged value -> State.Patch_ctx.set_merged ctx ~patch_id ~value
  | Set_needs_intervention value ->
      State.Patch_ctx.set_needs_intervention ctx ~patch_id ~value
  | Set_approved value -> State.Patch_ctx.set_approved ctx ~patch_id ~value
  | Set_enqueued value -> State.Patch_ctx.set_enqueued ctx ~patch_id ~value
  | Set_busy value -> State.Patch_ctx.set_busy ctx ~patch_id ~value
  | Set_has_pr value -> State.Patch_ctx.set_has_pr ctx ~patch_id ~value
  | Set_queued_ci value ->
      State.Patch_ctx.set_queued ctx ~patch_id ~kind:Ci ~value
  | Set_queued_review value ->
      State.Patch_ctx.set_queued ctx ~patch_id ~kind:Review_comments ~value
  | Set_failure_count count ->
      State.Patch_ctx.set_ci_failure_count ctx ~patch_id ~count
  | Set_base_branch name ->
      State.Patch_ctx.set_base_branch ctx ~patch_id
        ~branch:(Types.Branch.of_string name)

let gen_ctx =
  let open QCheck2.Gen in
  map
    (List.fold ~init:State.Patch_ctx.empty ~f:apply_flag)
    (list_size (int_range 0 8) gen_flag)

(* ─────────────────────────────────────────────────────────────────────────
   Properties
   ───────────────────────────────────────────────────────────────────────── *)

let prop_total =
  QCheck2.Test.make ~name:"derive is total over arbitrary ctx + current_op"
    ~count:1000
    QCheck2.Gen.(pair gen_ctx gen_op)
    (fun (ctx, current_op) ->
      try
        ignore (Display_status.derive ctx ~patch_id ~current_op ~main_branch);
        true
      with _ -> false)

let prop_merged_dominates =
  QCheck2.Test.make
    ~name:"set_merged true ⇒ derive returns Merged regardless of other flags"
    ~count:500
    QCheck2.Gen.(pair gen_ctx gen_op)
    (fun (ctx, current_op) ->
      let ctx = State.Patch_ctx.set_merged ctx ~patch_id ~value:true in
      Display_status.equal Merged
        (Display_status.derive ctx ~patch_id ~current_op ~main_branch))

let prop_wontdo_dominates =
  QCheck2.Test.make ~name:"WONTDO dominates every unmerged display state"
    ~count:500
    QCheck2.Gen.(pair gen_ctx gen_op)
    (fun (ctx, current_op) ->
      let ctx = State.Patch_ctx.set_merged ctx ~patch_id ~value:false in
      let ctx = State.Patch_ctx.set_wontdo ctx ~patch_id ~value:true in
      Display_status.equal Wontdo
        (Display_status.derive ctx ~patch_id ~current_op ~main_branch))

let prop_needs_help_beats_approved =
  QCheck2.Test.make
    ~name:"needs_intervention=true ⇒ Needs_help even when approved + busy + ..."
    ~count:500
    QCheck2.Gen.(pair gen_ctx gen_op)
    (fun (ctx, current_op) ->
      (* needs_intervention beats every flag except merged. *)
      let ctx = State.Patch_ctx.set_merged ctx ~patch_id ~value:false in
      let ctx = State.Patch_ctx.set_wontdo ctx ~patch_id ~value:false in
      let ctx = State.Patch_ctx.set_enqueued ctx ~patch_id ~value:false in
      let ctx =
        State.Patch_ctx.set_needs_intervention ctx ~patch_id ~value:true
      in
      Display_status.equal Needs_help
        (Display_status.derive ctx ~patch_id ~current_op ~main_branch))

let prop_enqueued_beats_approved =
  QCheck2.Test.make
    ~name:
      "enqueued + not merged + not needs_intervention ⇒ In_merge_queue when \
       idle, even when approved + queued + ..."
    ~count:500
    QCheck2.Gen.(pair gen_ctx gen_op)
    (fun (ctx, current_op) ->
      (* Terminal states and active work precede merge queue membership. *)
      let ctx = State.Patch_ctx.set_merged ctx ~patch_id ~value:false in
      let ctx = State.Patch_ctx.set_wontdo ctx ~patch_id ~value:false in
      let ctx =
        State.Patch_ctx.set_needs_intervention ctx ~patch_id ~value:false
      in
      let ctx = State.Patch_ctx.set_busy ctx ~patch_id ~value:false in
      let ctx = State.Patch_ctx.set_enqueued ctx ~patch_id ~value:true in
      Display_status.equal In_merge_queue
        (Display_status.derive ctx ~patch_id ~current_op ~main_branch))

let prop_approved_busy_is_running =
  QCheck2.Test.make ~name:"approval cannot hide active work" ~count:500 gen_op
    (fun current_op ->
      let ctx =
        ( ( ( ( State.Patch_ctx.empty |> fun c ->
                State.Patch_ctx.set_merged c ~patch_id ~value:false )
            |> fun c ->
              State.Patch_ctx.set_needs_intervention c ~patch_id ~value:false )
          |> fun c -> State.Patch_ctx.set_enqueued c ~patch_id ~value:false )
        |> fun c -> State.Patch_ctx.set_approved c ~patch_id ~value:true )
        |> fun c -> State.Patch_ctx.set_busy c ~patch_id ~value:true
      in
      Display_status.equal
        (Display_status.derive
           (State.Patch_ctx.set_approved ctx ~patch_id ~value:false)
           ~patch_id ~current_op ~main_branch)
        (Display_status.derive ctx ~patch_id ~current_op ~main_branch))

let prop_pending_when_no_pr =
  QCheck2.Test.make
    ~name:"empty ctx (no PR, not busy, not approved, ...) ⇒ Pending" ~count:1
    QCheck2.Gen.unit (fun () ->
      Display_status.equal Pending
        (Display_status.derive State.Patch_ctx.empty ~patch_id ~current_op:None
           ~main_branch))

let prop_blocked_by_dep_requires_off_main =
  QCheck2.Test.make ~name:"has_pr + base_branch ≠ main + idle ⇒ Blocked_by_dep"
    ~count:200 QCheck2.Gen.unit (fun () ->
      let ctx =
        ( State.Patch_ctx.empty |> fun c ->
          State.Patch_ctx.set_has_pr c ~patch_id ~value:true )
        |> fun c ->
        State.Patch_ctx.set_base_branch c ~patch_id
          ~branch:(Types.Branch.of_string "feature/dep")
      in
      Display_status.equal Blocked_by_dep
        (Display_status.derive ctx ~patch_id ~current_op:None ~main_branch))

(* [is_on_main] reflects the tracked base branch: equal to main ⇒ true, any
   other branch ⇒ false. Generate the base-branch name and assert the
   equivalence. *)
let prop_is_on_main_matches_base =
  QCheck2.Test.make ~name:"is_on_main ⇔ base_branch = main_branch" ~count:200
    QCheck2.Gen.(oneof_list [ "main"; "feature/foo"; "dev"; "release" ])
    (fun name ->
      let ctx =
        State.Patch_ctx.set_base_branch State.Patch_ctx.empty ~patch_id
          ~branch:(Types.Branch.of_string name)
      in
      Bool.equal
        (Display_status.is_on_main ctx ~patch_id ~main_branch)
        (String.equal name "main"))

let gen_status : Display_status.t QCheck2.Gen.t =
  QCheck2.Gen.oneof_list
    Display_status.
      [
        Merged;
        Wontdo;
        Needs_help;
        In_merge_queue;
        Approved_idle;
        Approved_running;
        Fixing_ci;
        Addressing_review;
        Addressing_findings;
        Resolving_conflict;
        Responding_to_human;
        Writing_pr_body;
        Cleaning_worktree;
        Rebasing;
        Starting;
        Updating;
        Ci_queued;
        Review_queued;
        Findings_queued;
        Cleanup_queued;
        Awaiting_feedback;
        Waiting_for_integration_root;
        Blocked_by_dep;
        Pending;
        Human_queued;
        Conflict_queued;
        Pr_body_queued;
        Rebase_queued;
        Waiting_for_ci;
        Waiting_for_review;
        Waiting_for_forge;
        Ci_failed;
        Conflict;
        Draft;
        Ready_for_integration;
        Reconciling;
        Checking_branch;
        Verifying_publication;
        Integrating;
        Repairing_branch;
        Publishing;
        Waiting_to_retry;
      ]

(* [yojson_of_t] then [t_of_yojson] round-trips every status value. *)
let prop_yojson_round_trip =
  QCheck2.Test.make ~name:"t_of_yojson ∘ yojson_of_t = id" ~count:500 gen_status
    (fun status ->
      Display_status.equal status
        (Display_status.t_of_yojson (Display_status.yojson_of_t status)))

let prop_public_surface_is_linked =
  QCheck2.Test.make ~name:"display status public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Display_status.is_on_main;
      ignore Display_status.t_of_yojson;
      true)

module B = Branch_reconcile
module R = Onton_core_test_support.Replay_fixture
module Forge = Onton_core_test_support.Forge_fixture

let agent () =
  Patch_agent.create ~branch:(Types.Branch.of_string "patch") patch_id
  |> fun a ->
  Patch_agent.set_pr_number a (Types.Pr_number.of_int 7) |> fun a ->
  Patch_agent.set_base_branch a main_branch

let status a =
  Display_status.of_agent ~execution_mode:Execution_mode.mainline ~main_branch a

let prop_owner_command_lifecycle =
  QCheck2.Test.make
    ~name:
      "inspection, actual replay, publication and confirmation have distinct \
       statuses across restart"
    ~count:200
    QCheck2.Gen.(pair bool (int_range 1 1000))
    (fun (noop, n) ->
      try
        let source = R.commit n and target = R.commit (n + 1000) in
        let candidate = if noop then source else R.commit (n + 2000) in
        let initial = R.step B.empty (B.Materialized (B.New_branch source)) in
        let requested = R.request ~name:"display" initial in
        let pinned =
          R.observe ~source ~target ~boundary:(Recorded source) ~noop requested
        in
        let integrating = R.reply pinned B.Pinned in
        let publishing =
          if noop then integrating
          else R.reply integrating (B.Integrated candidate)
        in
        let confirming =
          if noop then publishing else R.reply publishing B.Published
        in
        let settled =
          R.reply confirming
            (B.Remote { sha = Some candidate; topology = Equal })
        in
        let cases =
          [
            (requested, Some Display_status.Checking_branch);
            (pinned, Some Checking_branch);
            ( integrating,
              Some (if noop then Verifying_publication else Rebasing) );
            ( publishing,
              Some (if noop then Verifying_publication else Publishing) );
            (confirming, Some Verifying_publication);
            (settled, None);
          ]
        in
        List.for_all cases ~f:(fun (owner, expected) ->
            List.for_all
              [ owner; R.restore owner ]
              ~f:(fun owner ->
                Option.equal Display_status.equal
                  (Display_status.reconciliation_status owner)
                  expected))
      with _ -> false)

let prop_retry_wait =
  QCheck2.Test.make
    ~name:"retry backoff and resumed Git inspection have distinct statuses"
    ~count:100
    QCheck2.Gen.(int_range 1 1000)
    (fun n ->
      try
        let initial =
          R.step B.empty (B.Materialized (B.New_branch (R.commit n)))
        in
        let requested = R.request ~name:"retry-display" initial in
        let waiting =
          R.reply requested
            (B.Retryable
               { reason = "transport_unavailable"; retry_after = None })
        in
        let waiting = R.restore waiting in
        let resumed =
          match B.wake_event ~at:100000. waiting with
          | Some event -> R.step waiting event
          | None -> waiting
        in
        Option.equal Display_status.equal
          (Display_status.reconciliation_status waiting)
          (Some Waiting_to_retry)
        && Option.equal Display_status.equal
             (Display_status.reconciliation_status resumed)
             (Some Checking_branch)
      with _ -> false)

let prop_live_inspection =
  QCheck2.Test.make
    ~name:"a scheduled rebase that only verifies Git displays checking-branch"
    ~count:100 QCheck2.Gen.bool (fun noop ->
      try
        let a = agent () in
        let intent =
          B.{ base = "main"; policy = Rewrite; purpose = Reconcile_base }
        in
        let a = Patch_agent.enqueue a Types.Operation_kind.Rebase in
        let a = Patch_agent.rebase a ~base_branch:main_branch in
        let a, _ = Patch_agent.reconcile_branch a (B.Request intent) in
        let source = R.commit 1 and target = R.commit 2 in
        let check a = Display_status.equal (status a) Checking_branch in
        let before = check a in
        let event =
          Onton_core_test_support.Publication_fixture.completion
            ~state:(fun a -> a.Patch_agent.branch_reconcile)
            a
            (B.Observed (R.observation ~source ~target ~boundary:Plain ~noop))
        in
        let a, _ = Patch_agent.reconcile_branch a event in
        before && check a
      with _ -> false)

let prop_live_feedback =
  QCheck2.Test.make
    ~name:
      "live statuses distinguish revision confirmation, CI, review, draft and \
       merge readiness" ~count:100 QCheck2.Gen.unit (fun () ->
      try
        let raw = agent () in
        let confirmed = Forge.readiness_agent raw in
        let green = Patch_agent.set_checks_passing confirmed true in
        let review =
          Patch_agent.set_review_decision green (Some "REVIEW_REQUIRED")
        in
        let draft = Patch_agent.set_is_draft green true in
        let ready =
          Patch_agent.set_is_draft green false |> fun a ->
          Patch_agent.set_merge_ready a true
        in
        List.for_all
          [
            (raw, Display_status.Waiting_for_forge);
            (confirmed, Waiting_for_ci);
            (review, Waiting_for_review);
            (draft, Draft);
            (ready, Approved_idle);
          ]
          ~f:(fun (a, expected) -> Display_status.equal (status a) expected)
        && Display_status.equal
             (status
                (Patch_agent.set_head_oid ready (Some (String.make 40 'c'))))
             Waiting_for_forge
      with _ -> false)

let prop_queued_repairs_before_dependency =
  QCheck2.Test.make
    ~name:"every queued operation remains visible on a dependency base"
    ~count:200
    QCheck2.Gen.(
      oneof_list
        [
          ( Types.Operation_kind.Uncommitted_changes,
            Display_status.Cleanup_queued );
          (Rebase, Rebase_queued);
          (Human, Human_queued);
          (Merge_conflict, Conflict_queued);
          (Ci, Ci_queued);
          (Review_comments, Review_queued);
          (Findings, Findings_queued);
          (Pr_body, Pr_body_queued);
        ])
    (fun (kind, expected) ->
      try
        let a =
          agent () |> fun a ->
          Patch_agent.set_base_branch a (Types.Branch.of_string "dependency")
          |> fun a -> Patch_agent.enqueue a kind
        in
        Display_status.equal (status a) expected
      with _ -> false)

let prop_queue_priority =
  QCheck2.Test.make
    ~name:"cleanup and rebase precede human and CI work in the displayed queue"
    ~count:300
    QCheck2.Gen.(
      list
        (oneof_list
           [
             Types.Operation_kind.Ci;
             Human;
             Review_comments;
             Findings;
             Pr_body;
             Merge_conflict;
           ]))
    (fun rest ->
      try
        let a = List.fold rest ~init:(agent ()) ~f:Patch_agent.enqueue in
        let rebase = Patch_agent.enqueue a Rebase in
        let cleanup = Patch_agent.enqueue rebase Uncommitted_changes in
        Display_status.equal (status rebase) Rebase_queued
        && Display_status.equal (status cleanup) Cleanup_queued
      with _ -> false)

let prop_feedback_total =
  QCheck2.Test.make
    ~name:"feedback classification is total for arbitrary review metadata"
    ~count:500
    QCheck2.Gen.(pair string int)
    (fun (review, unresolved_comment_count) ->
      try
        ignore
          (Display_status.feedback_status ~confirmed:true ~has_conflict:false
             ~checks_passing:true ~ci_checks:[] ~review_decision:(Some review)
             ~unresolved_comment_count ~is_draft:false);
        true
      with _ -> false)

let () =
  let suite =
    [
      prop_owner_command_lifecycle;
      prop_retry_wait;
      prop_live_inspection;
      prop_live_feedback;
      prop_queued_repairs_before_dependency;
      prop_queue_priority;
      prop_feedback_total;
      prop_total;
      prop_merged_dominates;
      prop_wontdo_dominates;
      prop_needs_help_beats_approved;
      prop_enqueued_beats_approved;
      prop_approved_busy_is_running;
      prop_pending_when_no_pr;
      prop_blocked_by_dep_requires_off_main;
      prop_is_on_main_matches_base;
      prop_yojson_round_trip;
      prop_public_surface_is_linked;
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true suite in
  if exit_code <> 0 then Stdlib.exit exit_code
