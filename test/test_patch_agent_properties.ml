(* @archlint.module test
   @archlint.domain patch-agent *)
open Base
open Onton_core
open Types

let agent id =
  Patch_agent.create ~branch:(Branch.of_string "branch") (Patch_id.of_string id)

let tests =
  [
    QCheck2.Test.make
      ~name:"readiness flags require proof for the current PR head and base"
      ~count:400
      QCheck2.Gen.(int_range 0 6)
      (fun mutation ->
        try
          let initial =
            agent "readiness" |> fun a ->
            Patch_agent.set_pr_number a (Pr_number.of_int 7) |> fun a ->
            Patch_agent.set_base_branch a (Branch.of_string "main") |> fun a ->
            Patch_agent.set_is_draft a false |> fun a ->
            Patch_agent.set_merge_ready a true |> fun a ->
            Patch_agent.set_checks_passing a true
          in
          let confirmed =
            Onton_core_test_support.Forge_fixture.readiness_agent initial
          in
          let current =
            match mutation with
            | 0 -> initial
            | 1 -> confirmed
            | 2 -> Patch_agent.set_pr_number confirmed (Pr_number.of_int 8)
            | 3 ->
                Patch_agent.set_head_oid confirmed (Some (String.make 40 'c'))
            | 4 ->
                Patch_agent.set_base_branch confirmed (Branch.of_string "other")
            | 5 -> Patch_agent.mark_pr_missing confirmed
            | _ -> Patch_agent.bump_generation confirmed
          in
          let expected = mutation = 1 || mutation = 6 in
          Bool.equal
            (Patch_agent.forge_revision_pair_confirmed current)
            expected
          && Bool.equal
               (Patch_agent.is_approved current
                  ~main_branch:(Branch.of_string "main"))
               expected
          && Bool.equal
               (Patch_agent.should_request_review
                  (Patch_agent.set_review_decision current
                     (Some "REVIEW_REQUIRED"))
                  ~main_branch:(Branch.of_string "main"))
               expected
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "unconfirmed revision pair revokes queued forge work without clearing \
         owner repair" ~count:100 QCheck2.Gen.bool (fun local_conflict ->
        try
          let initial = agent "pending" in
          let initial =
            if local_conflict then
              Onton_core_test_support.Conflict_fixture.agent initial
            else initial
          in
          let initial =
            List.fold
              [
                Operation_kind.Ci;
                Operation_kind.Merge_conflict;
                Operation_kind.Human;
              ]
              ~init:initial ~f:Patch_agent.enqueue
          in
          let initial = Patch_agent.set_merge_ready initial true in
          let initial = Patch_agent.set_checks_passing initial true in
          let deferred = Patch_agent.defer_forge_revision_pair initial in
          (not deferred.Patch_agent.merge_ready)
          && (not deferred.Patch_agent.checks_passing)
          && deferred.Patch_agent.mergeability_unknown
          && List.equal Operation_kind.equal deferred.Patch_agent.queue
               [ Operation_kind.Human ]
          && Branch_reconcile.equal initial.Patch_agent.branch_reconcile
               deferred.Patch_agent.branch_reconcile
          && Bool.equal (Patch_agent.has_conflict deferred) local_conflict
          && Patch_agent.equal deferred
               (Patch_agent.defer_forge_revision_pair deferred)
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "patch forge tickets reject replaced local context and retain accepted \
         evidence"
      ~count:200
      QCheck2.Gen.(int_range 0 3)
      (fun mutation ->
        try
          let module F = Forge_observation in
          let module X = Onton_core_test_support.Forge_fixture in
          let initial =
            Patch_agent.set_pr_number (agent "patch") (Pr_number.of_int 7)
          in
          let initial =
            Patch_agent.set_head_oid initial X.base.Pr_state.head_oid
          in
          let initial =
            Patch_agent.set_base_branch initial (Branch.of_string "main")
          in
          let requested, ticket =
            Patch_agent.begin_forge_observation initial
              ~request:(X.request "patch")
          in
          match ticket with
          | None -> false
          | Some ticket ->
              let current =
                match mutation with
                | 0 -> requested
                | 1 -> Patch_agent.bump_generation requested
                | 2 -> Patch_agent.set_pr_number requested (Pr_number.of_int 8)
                | _ ->
                    Patch_agent.set_base_branch requested
                      (Branch.of_string "retarget")
              in
              let result_agent, result =
                Patch_agent.accept_forge_observation current ~ticket
                  ~confirmed_head:X.base.Pr_state.head_oid
                  (X.observe ticket.F.request Pr_state.Conflicting)
              in
              Bool.equal (Result.is_ok result) (mutation = 0)
              && Bool.equal
                   (F.equal_scope
                      (Patch_agent.forge_scope current)
                      ticket.F.scope)
                   (mutation = 0)
              && Bool.equal
                   (Option.is_some
                      (F.latest_fact
                         (Branch_reconcile.forge_observations
                            result_agent.Patch_agent.branch_reconcile)))
                   (mutation = 0)
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "base evidence follows integrations across scheduling and publication \
         histories"
      ~print:(fun history ->
        String.concat ~sep:", "
          (List.map history ~f:(fun (action, label) ->
               Printf.sprintf "%d:%S" action label)))
      ~count:300
      QCheck2.Gen.(list_size (int_range 0 40) (pair (int_range 0 3) string))
      (fun history ->
        try
          let module F = Onton_core_test_support.Publication_fixture in
          let _, _, valid =
            List.foldi history
              ~init:(agent "patch", None, true)
              ~f:(fun index (a, expected, valid) (action, label) ->
                let base = Branch.of_string ("base/" ^ F.sha label) in
                let a, expected =
                  match action with
                  | 0 -> (F.reconciled_agent ~base a, Some base)
                  | 1 ->
                      ( Patch_agent.complete
                          (Patch_agent.start a ~base_branch:base),
                        expected )
                  | 2 -> (Patch_agent.set_base_branch a base, expected)
                  | _ ->
                      ( F.agent
                          ~candidate:(F.sha (label ^ Int.to_string index))
                          a,
                        expected )
                in
                ( a,
                  expected,
                  valid
                  && Option.equal Branch.equal
                       (Patch_agent.branch_rebased_onto a)
                       expected ))
          in
          valid
        with _ -> false);
    QCheck2.Test.make
      ~name:"only first confirmed publication resets bootstrap failures"
      ~count:200
      QCheck2.Gen.(pair (int_range 0 4) string)
      (fun (failures, label) ->
        try
          let module F = Onton_core_test_support.Publication_fixture in
          let initial =
            List.fold (List.range 0 failures) ~init:(agent "patch")
              ~f:(fun a _ -> Patch_agent.on_pre_session_failure a)
          in
          let step a event = fst (Patch_agent.reconcile_branch a event) in
          let state (a : Patch_agent.t) = a.Patch_agent.branch_reconcile in
          let pending =
            F.publishing ~candidate:(F.sha label) ~step ~state initial
          in
          let pushed =
            F.reply ~step ~state pending Branch_reconcile.Published
          in
          let published = F.agent ~candidate:(F.sha label) initial in
          let failed = Patch_agent.on_pr_discovery_failure published in
          let republished =
            F.agent ~candidate:(F.sha (label ^ "-next")) failed
          in
          let twice_failed = Patch_agent.on_pr_discovery_failure republished in
          Int.equal initial.Patch_agent.start_attempts_without_pr failures
          && Int.equal pending.Patch_agent.start_attempts_without_pr failures
          && Int.equal pushed.Patch_agent.start_attempts_without_pr failures
          && Int.equal published.Patch_agent.start_attempts_without_pr 0
          && Int.equal republished.Patch_agent.start_attempts_without_pr 1
          && Patch_agent.equal republished
               (Patch_agent.on_pre_session_failure republished)
          && Patch_agent.needs_intervention twice_failed
          && Patch_decision.equal_disposition
               (Patch_decision.disposition published)
               Patch_decision.Ready_start
          && Patch_decision.equal_disposition
               (Patch_decision.disposition ~branch_only:true published)
               Patch_decision.Idle
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "publication evidence requires owner confirmation and survives restart"
      ~count:200 QCheck2.Gen.string (fun label ->
        try
          let module F = Onton_core_test_support.Publication_fixture in
          let initial = agent "patch" in
          let candidate = F.sha label in
          let step a event = fst (Patch_agent.reconcile_branch a event) in
          let state (a : Patch_agent.t) = a.Patch_agent.branch_reconcile in
          let pending = F.publishing ~candidate ~step ~state initial in
          let confirmed = F.agent ~candidate initial in
          let restored =
            Branch_reconcile.decode
              (Branch_reconcile.yojson_of_t (state confirmed))
          in
          (not (Patch_agent.branch_published initial))
          && (not (Patch_agent.branch_published pending))
          && Patch_agent.branch_published confirmed
          && Result.is_ok restored
          && Result.equal Branch_reconcile.equal String.equal restored
               (Ok (state confirmed))
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "branch dispatch preserves backend completion across busy lifecycles"
      ~count:200
      QCheck2.Gen.(pair bool (list bool))
      (fun (implemented, activity) ->
        let initial = agent "patch" in
        let initial =
          if implemented then
            Patch_agent.start initial ~base_branch:(Branch.of_string "main")
          else initial
        in
        let completion =
          Session_result.
            {
              session_uuid = "completed-session";
              delivery_mode = Start;
              kind = None;
              message_id = None;
              result = Session_ok;
              head = None;
              guidance = [];
              turn_accepted = true;
            }
        in
        let initial =
          Patch_agent.record_session_completion initial completion
        in
        let initial, _ =
          Patch_agent.reconcile_branch initial
            Branch_reconcile.(
              Request
                { base = "main"; policy = Rewrite; purpose = Reconcile_base })
        in
        let final =
          List.fold activity ~init:initial ~f:(fun a running ->
              if running then Patch_agent.begin_branch_reconciliation a
              else Patch_agent.complete a)
          |> Patch_agent.complete
        in
        (not final.Patch_agent.busy)
        && Bool.equal initial.Patch_agent.has_session
             final.Patch_agent.has_session
        && Branch_reconcile.equal initial.Patch_agent.branch_reconcile
             final.Patch_agent.branch_reconcile
        && Option.equal Session_result.equal_completion
             final.Patch_agent.session_completion (Some completion));
    QCheck2.Test.make
      ~name:"WONTDO normalizes arbitrary reasons and is idempotent" ~count:500
      QCheck2.Gen.string (fun reason ->
        let a = agent "patch" in
        let refused = Patch_agent.set_wontdo a reason in
        let trimmed = String.strip reason in
        let expected = if String.is_empty trimmed then None else Some trimmed in
        Option.equal String.equal refused.Patch_agent.wontdo_reason expected
        && Bool.equal
             (Patch_agent.needs_intervention refused)
             (Option.is_some expected)
        && Patch_agent.equal refused (Patch_agent.set_wontdo refused reason));
    QCheck2.Test.make
      ~name:"WONTDO pause survives automatic work until explicit reset"
      ~count:500
      QCheck2.Gen.(list (int_range 0 6))
      (fun operations ->
        let _, _, valid =
          List.fold operations
            ~init:(agent "patch", false, true)
            ~f:(fun (a, paused, valid) op ->
              let a, paused =
                match op with
                | 0 -> (Patch_agent.set_wontdo a "Needs prerequisite", true)
                | 1 -> (Patch_agent.reset_intervention_state a, false)
                | 2 -> (Patch_agent.enqueue a Operation_kind.Human, paused)
                | 3 ->
                    (Patch_agent.add_human_message a "Queued guidance", paused)
                | 4 -> (Patch_agent.clear_session_fallback a, paused)
                | 5 -> (Patch_agent.set_ci_checks a [], paused)
                | _ ->
                    let active =
                      Patch_agent.start a ~base_branch:(Branch.of_string "main")
                    in
                    (Patch_agent.complete active, paused)
              in
              ( a,
                paused,
                valid
                && Bool.equal
                     (Option.is_some a.Patch_agent.wontdo_reason)
                     paused
                && ((not paused)
                   || Patch_agent.needs_intervention a
                      && Patch_decision.equal_disposition
                           (Patch_decision.disposition a)
                           Patch_decision.Blocked) ))
        in
        valid);
    QCheck2.Test.make ~name:"blank WONTDO does not clear an existing refusal"
      ~count:50
      QCheck2.Gen.(oneof_list [ ""; " "; "\n\t " ])
      (fun reason ->
        let a = Patch_agent.set_wontdo (agent "patch") "Prerequisite missing" in
        Patch_agent.equal a (Patch_agent.set_wontdo a reason));
    QCheck2.Test.make
      ~name:"publication settles only its captured refresh across interleavings"
      ~count:500
      QCheck2.Gen.(list (int_range 0 3))
      (fun operations ->
        let initial = agent "root" in
        let _, _, _, _, _, valid =
          List.fold operations ~init:(initial, initial, 0, 0, false, true)
            ~f:(fun (a, publication, version, captured, pending, valid) op ->
              let a, publication, version, captured, pending =
                match op with
                | 0 ->
                    ( Patch_agent.request_pr_body_refresh a,
                      publication,
                      version + 1,
                      captured,
                      true )
                | 1 -> (a, a, version, version, pending)
                | 2 ->
                    ( Patch_agent.acknowledge_pr_body_refresh a ~publication,
                      publication,
                      version,
                      captured,
                      pending && not (Int.equal version captured) )
                | _ ->
                    ( Patch_agent.set_pr_body_delivered a true,
                      publication,
                      version,
                      captured,
                      pending )
              in
              ( a,
                publication,
                version,
                captured,
                pending,
                valid
                && Bool.equal a.Patch_agent.pr_body_refresh.Patch_agent.pending
                     pending
                && Int.equal a.Patch_agent.pr_body_refresh.Patch_agent.version
                     version ))
        in
        valid);
    QCheck2.Test.make ~name:"another patch cannot settle refresh" ~count:100
      QCheck2.Gen.bool (fun delivered ->
        let a = Patch_agent.request_pr_body_refresh (agent "root") in
        let publication =
          agent "child" |> Patch_agent.request_pr_body_refresh |> fun a ->
          Patch_agent.set_pr_body_delivered a delivered
        in
        Patch_agent.equal a
          (Patch_agent.acknowledge_pr_body_refresh a ~publication));
  ]

let () = QCheck_base_runner.run_tests_main tests
