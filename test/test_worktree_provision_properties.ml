(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton_core
module B = Branch_reconcile
module P = Worktree_provision
module G = QCheck2.Gen

let intent request =
  B.{ base = "main"; policy = Rewrite; purpose = Provision_checkout request }

let request name = fst (B.step B.empty (B.Request (intent name)))

let reply state result =
  match B.pending state with
  | Some command ->
      fst
        (B.step state (B.Result { token = command.B.token; at = 10.; result }))
  | None -> state

let tests =
  [
    QCheck2.Test.make ~count:500
      ~name:"provisioning never requests an intent the owner cannot admit"
      G.(triple string string bool)
      (fun[@warning "-4"] (base, name, settled) ->
        try
          let state =
            if settled then reply (request "previous") B.Checkout_ready
            else B.empty
          in
          let desired =
            B.{ base; policy = Rewrite; purpose = Provision_checkout name }
          in
          match P.next ~intent:desired ~at:100. state with
          | P.Run (B.Request admitted) ->
              let next, effects = B.step state (B.Request admitted) in
              (not (B.equal state next))
              && effects <> []
              && B.decode (B.yojson_of_t next) = Ok next
          | P.Stop "provisioning_intent_required" -> base = "" || name = ""
          | P.Ready -> B.equal_intent desired (intent "previous")
          | P.Stop _ | P.Wait _ | P.Run _ -> false
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "creation probe failures retry while checkout ownership refusals \
         intervene"
      ~count:500
      G.(triple string string (int_range 0 2))
      (fun (local_sha, remote_sha, kind) ->
        try
          let refusal =
            match kind with
            | 0 ->
                Start_point_plan.Ancestry_unavailable { local_sha; remote_sha }
            | 1 -> Start_point_plan.Branch_checked_out_in_main_root
            | _ ->
                Start_point_plan.Worktree_already_registered
                  { existing_path = local_sha }
          in
          let failure = P.creation_failure refusal in
          let reason =
            Start_point_plan.short_label (Start_point_plan.Refuse refusal)
          in
          let expected =
            if kind = 0 then B.Retryable { reason; retry_after = None }
            else B.Permanent reason
          in
          let state =
            reply (request "checkout") (P.reconciliation_result failure)
          in
          P.message failure = reason
          && B.equal_result (P.reconciliation_result failure) expected
          &&
          if kind = 0 then
            P.next ~intent:(intent "later") ~at:14. state
            = P.Wait "checkout_provisioning_pending"
          else P.next ~intent:(intent "later") ~at:14. state = P.Stop reason
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "provisioning failure classification is total and retains diagnostics"
      ~count:1000 (G.pair G.bool G.string) (fun (unsafe, reason) ->
        try
          let failure =
            if unsafe then P.Unsafe reason else P.Temporary reason
          in
          let text = P.message failure in
          text <> ""
          && (reason = "" || text = reason)
          && B.equal_result
               (P.reconciliation_result failure)
               (if unsafe then B.Permanent text
                else B.Retryable { reason = text; retry_after = None })
        with _ -> false);
    QCheck2.Test.make
      ~name:"only contradictory materialization evidence is permanent"
      ~count:1000 G.string (fun reason ->
        try
          let failure = P.materialization_failure reason in
          let unsafe =
            List.mem reason
              [
                "materialization_revision_changed";
                "materialization_branch_changed";
                "contradictory_materialization_receipts";
              ]
          in
          match failure with
          | P.Unsafe actual -> unsafe && actual = reason
          | P.Temporary actual -> (not unsafe) && actual = reason
        with _ -> false);
    QCheck2.Test.make
      ~name:"provisioning respects retry deadline and the captured request"
      ~count:500 G.string (fun suffix ->
        try
          let state =
            reply (request "first")
              (B.Retryable { reason = "offline"; retry_after = None })
          in
          let next_intent = intent ("new:" ^ suffix) in
          P.next ~intent:next_intent ~at:14. state
          = P.Wait "checkout_provisioning_pending"
          && P.next ~intent:next_intent ~at:15. state = P.Run (B.Tick 15.)
        with _ -> false);
    QCheck2.Test.make
      ~name:"unrelated reconciliation cannot be superseded by provisioning"
      ~count:500 G.string (fun suffix ->
        try
          let other =
            B.
              {
                base = "main";
                policy = Rewrite;
                purpose = Publish_session ("session:" ^ suffix);
              }
          in
          let state, _ = B.step B.empty (B.Request other) in
          P.next ~intent:(intent "checkout") ~at:100000. state
          = P.Wait "branch_reconciliation_pending"
          && P.next ~intent:other ~at:100000. B.empty
             = P.Stop "provisioning_intent_required"
        with _ -> false);
    QCheck2.Test.make
      ~name:"ready checkouts require a new inspection for a new request"
      ~count:500 G.bool (fun same ->
        try
          let state = reply (request "first") B.Checkout_ready in
          let desired = intent (if same then "first" else "second") in
          let expected = if same then P.Ready else P.Run (B.Request desired) in
          P.next ~intent:desired ~at:100. state = expected
        with _ -> false);
    QCheck2.Test.make
      ~name:"permanent owner intervention is sticky across checkout requests"
      ~count:500 G.string (fun reason ->
        try
          let state = reply (request "first") (B.Permanent reason) in
          P.next ~intent:(intent "next") ~at:100000. state = P.Stop reason
          && P.next ~intent:(intent "") ~at:0. B.empty
             = P.Stop "provisioning_intent_required"
        with _ -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests
