(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton_core
module B = Branch_reconcile
module F = Onton_core_test_support.Replay_fixture
module G = QCheck2.Gen

let request base =
  B.Request B.{ base; policy = Rewrite; purpose = Reconcile_base }

let permitted state =
  match B.pending state with
  | Some { kind = B.Publish { candidate; _ }; _ } -> (
      match B.operation state with
      | Some operation ->
          operation.B.scope_revision = Some candidate
          && Replay_scope.valid_request operation.B.approved_scope
      | None -> false)
  | Some { kind = B.Checkout_remote _; _ } -> false
  | Some { kind = B.Integrate { source; target; boundary; policy }; _ } -> (
      match B.operation state with
      | Some operation -> (
          B.integration_scope_matches operation ~source ~target ~boundary
            ~policy
          &&
          match (policy, boundary) with
          | B.Preserve_ancestry, _ | B.Rewrite, B.Recorded _ -> true
          | ( B.Rewrite,
              ( B.Plain | B.Inferred _ | B.Reconstructed _
              | B.Patch_equivalent _ | B.Subject_inferred _ ) ) ->
              false)
      | None -> false)
  | Some
      {
        kind =
          ( Observe | Pin _ | Verify_scope _ | Inspect | Verify_recovery
          | Continue _ | Confirm _ | Commit_merge _ | Plan_remote_replay _ );
        _;
      }
  | None ->
      true

let boundary kind =
  let revision = F.commit 1 in
  match kind with
  | 0 -> B.Recorded revision
  | 1 -> B.Plain
  | 2 -> B.Inferred revision
  | 3 -> B.Patch_equivalent revision
  | 4 -> B.Subject_inferred revision
  | _ -> B.Reconstructed { original = F.commit 2; upstream = revision }

let observation kind =
  F.observation ~source:(F.commit 10) ~target:(F.commit 20)
    ~boundary:(boundary kind) ~noop:false

let respond state kind =
  match B.pending state with
  | None -> state
  | Some command ->
      let result =
        match command.B.kind with
        | B.Observe -> B.Observed (observation kind)
        | B.Pin _ -> B.Pinned
        | B.Inspect -> B.Inspected (observation kind)
        | B.Integrate _ | B.Continue _ | B.Commit_merge _ | B.Checkout_remote _
        | B.Verify_scope _ | B.Plan_remote_replay _ | B.Publish _ | B.Confirm _
        | B.Verify_recovery ->
            B.Retryable
              { reason = "fixture transport failure"; retry_after = None }
      in
      F.reply state result

let () =
  QCheck_base_runner.run_tests_main
    [
      QCheck2.Test.make ~count:1000
        ~name:
          "queued dirty recovery may extend local work only before durable \
           agent dispatch"
        G.(pair bool (list_size (int_range 0 80) (int_range 0 3)))
        (fun (sequencer, actions) ->
          try
            let state = F.step B.empty (request "main") in
            let state = F.reply state (B.Observed (observation 0)) in
            let state = F.reply state B.Pinned in
            let state = F.reply state (B.Recovery_required "external head") in
            let command = Option.get (B.pending state) in
            let state, effects =
              B.step state
                (B.Result
                   {
                     token = command.token;
                     at = 100.;
                     result =
                       B.Recovery_verified
                         {
                           observation = observation 0;
                           local_extension = None;
                           source_preserved = false;
                           remote_preserved = true;
                         };
                   })
            in
            let token =
              List.find_map
                (function
                  | B.Repair token -> Some token
                  | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
                effects
              |> Option.get
            in
            let state = F.step state (B.Repair_started token) in
            let state, dispatched =
              List.fold_left
                (fun (state, dispatched) action ->
                  let state, dispatched =
                    match action with
                    | 0 -> (F.restore state, dispatched)
                    | 1 -> (F.step state (B.Repair_started token), dispatched)
                    | 2 ->
                        ( F.step state (B.Repair_dispatched command.token),
                          dispatched )
                    | _ -> (F.step state (B.Repair_dispatched token), true)
                  in
                  assert (
                    (Option.get (B.operation state)).B.agent_dispatched
                    = dispatched);
                  (state, dispatched))
                (state, false) actions
            in
            let state = F.step (F.restore state) B.Recover in
            let state =
              F.reply state
                (B.Recovery_verified
                   {
                     observation =
                       {
                         (observation 0) with
                         clean = false;
                         sequencer = (if sequencer then Some "rebase" else None);
                       };
                     local_extension = None;
                     source_preserved = false;
                     remote_preserved = true;
                   })
            in
            let op = Option.get (B.operation state) in
            let expected =
              if dispatched || sequencer then B.Reconstruct_history
              else B.Finish_local_work
            in
            permitted state
            && B.equal state (F.restore state)
            &&
            match op.repair with
            | Some { mode = B.History_recovery { task; _ }; _ } ->
                task = expected
            | None | Some { mode = B.Content_repair | B.Diagnosis _; _ } ->
                false
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "retained recovery revisions survive new operations without becoming \
           replay evidence"
        G.(
          pair
            (list_size (int_range 0 30) (int_range 1000 2000))
            (list_size (int_range 1 30) bool))
        (fun (revisions, restarts) ->
          try
            let retained =
              List.sort_uniq compare (List.map F.commit revisions)
            in
            let state =
              Onton_core_test_support.Publication_fixture.confirmed
                ~candidate:(B.Commit.to_string (F.commit 30))
                ~step:F.step ~state:Fun.id B.empty
            in
            let json =
              match B.yojson_of_t state with
              | `Assoc fields ->
                  `Assoc
                    (List.map
                       (fun (key, value) ->
                         if key <> "active" then (key, value)
                         else
                           match value with
                           | `Assoc operation ->
                               ( key,
                                 `Assoc
                                   (( "retained_revisions",
                                      `List
                                        (List.map
                                           (fun revision ->
                                             `String
                                               (B.Commit.to_string revision))
                                           retained) )
                                   :: List.remove_assoc "retained_revisions"
                                        operation) )
                           | _ -> failwith "missing fixture operation")
                       fields)
              | _ -> failwith "missing fixture checkpoint"
            in
            let state =
              match B.decode json with
              | Ok state -> state
              | Error reason -> failwith reason
            in
            let state = F.step state (request "next-base") in
            let valid state =
              List.for_all
                (fun revision -> List.mem revision (B.required_revisions state))
                retained
              &&
              match B.operation state with
              | Some op ->
                  op.B.approved_scope = Replay_scope.Unproven
                  && op.boundary_candidates = []
              | None -> false
            in
            let _, ok =
              List.fold_left
                (fun (state, ok) recover ->
                  let state =
                    if recover then F.step state B.Recover else F.restore state
                  in
                  (state, ok && valid state))
                (state, valid state)
                restarts
            in
            ok
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "adopted rebase cannot weaken a preserving contribution contract \
           across restart"
        G.(pair bool (list_size (int_range 0 80) bool))
        (fun (contributor, restarts) ->
          try
            let observed = observation 0 in
            let initial =
              F.step B.empty
                (B.Request
                   {
                     base = "main";
                     policy =
                       (if contributor then B.Rewrite else B.Preserve_ancestry);
                     purpose =
                       (if contributor then
                          B.Integrate_revision
                            {
                              contributor = "child";
                              revision = observed.target;
                            }
                        else B.Reconcile_base);
                   })
            in
            let state =
              F.reply initial
                (B.Observed_active
                   { observation = observed; policy = B.Rewrite })
            in
            let expected =
              Replay_scope.Merge
                {
                  source = B.Commit.to_string observed.source;
                  target = B.Commit.to_string observed.target;
                }
            in
            let valid state =
              match B.operation state with
              | Some op ->
                  op.B.approved_scope = expected
                  && B.execution_policy op = B.Preserve_ancestry
              | None -> false
            in
            valid state
            &&
            let _, valid =
              List.fold_left
                (fun (state, ok) recover ->
                  let state =
                    if recover then F.step state B.Recover else F.restore state
                  in
                  (state, ok && valid state))
                (state, true) restarts
            in
            valid
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "repair authority cannot widen across restart, retarget and \
           stale-completion interleavings without a matching linear extension"
        G.(
          triple (int_range 0 2) (int_range 0 2)
            (list_size (int_range 0 80) (int_range 0 4)))
        (fun (task, evidence, actions) ->
          try
            let source = F.commit 10
            and candidate = F.commit 30
            and extended = F.commit 50 in
            let initial = F.step B.empty (request "main") in
            let initial =
              F.reply initial
                (B.Observed { (observation 0) with clean = task <> 0 })
            in
            let initial =
              if task = 0 then initial
              else
                let initial = F.reply initial B.Pinned in
                if task = 2 then
                  F.reply initial
                    (B.Recovery_required "fixture integration failure")
                else
                  let initial = F.reply initial (B.Integrated candidate) in
                  let operation = Option.get (B.operation initial) in
                  let initial =
                    F.reply initial
                      (Onton_core_test_support.Scope_fixture.verified
                         operation.B.approved_scope candidate)
                  in
                  F.reply initial
                    (B.Publication_rejected
                       (Push_reject_classify.Hook_failure "fixture gate"))
            in
            let baseline = if task = 1 then candidate else source in
            assert (
              Option.map fst
                (B.local_extension_contract (Option.get (B.operation initial)))
              = if task = 2 then None else Some baseline);
            let inspection =
              {
                (observation 0) with
                source = baseline;
                head = baseline;
                clean = task <> 0;
                target_included = true;
              }
            in
            let command = Option.get (B.pending initial) in
            let state, effects =
              B.step initial
                (B.Result
                   {
                     token = command.token;
                     at = 100.;
                     result =
                       B.Recovery_verified
                         {
                           observation = inspection;
                           local_extension = None;
                           source_preserved = task = 1;
                           remote_preserved = task = 1;
                         };
                   })
            in
            let token =
              List.find_map
                (function
                  | B.Repair token -> Some token
                  | B.Execute _ | B.Start_repair _ | B.Completed _ -> None)
                effects
              |> Option.get
            in
            let state = F.step state (B.Repair_started token) in
            let state =
              F.step state (B.Repair_completed { token; at = 100. })
            in
            let original = (Option.get (B.operation state)).B.approved_scope in
            let state =
              List.fold_left
                (fun state action ->
                  match action with
                  | 0 -> F.restore state
                  | 1 -> F.step state (request "deferred-base")
                  | 2 -> F.step state (B.Tick 1000.)
                  | 3 -> F.step state (B.Repair_completed { token; at = 100. })
                  | _ -> F.step state B.Recover)
                state actions
            in
            let local_extension =
              if evidence = 2 then None
              else
                Onton_core_test_support.Scope_fixture.local_extension
                  ~source:(if evidence = 0 then baseline else F.commit 99)
                  ~candidate:extended
            in
            let state =
              F.reply state
                (B.Recovery_verified
                   {
                     observation =
                       {
                         inspection with
                         source = extended;
                         head = extended;
                         clean = true;
                       };
                     local_extension;
                     source_preserved = true;
                     remote_preserved = true;
                   })
            in
            let contract = (Option.get (B.operation state)).B.approved_scope in
            let expected =
              if evidence <> 0 || task = 2 then original
              else if task = 1 then
                Replay_scope.Identity (B.Commit.to_string extended)
              else
                Replay_scope.Replay
                  {
                    source = B.Commit.to_string extended;
                    boundary = B.Commit.to_string (F.commit 1);
                    target = B.Commit.to_string (F.commit 20);
                  }
            in
            Replay_scope.equal_request contract expected
            && permitted state
            && B.equal state (F.restore state)
            && List.mem baseline (B.required_revisions state)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "publication requires a matching scope proof across recovery and \
           stale-result interleavings"
        G.(list_size (int_range 1 150) (int_range 0 9))
        (fun actions ->
          try
            let candidate = F.commit 30 in
            let state = F.step B.empty (request "main") in
            let state = F.reply state (B.Observed (observation 0)) in
            let state = F.reply state B.Pinned in
            let state = F.reply state (B.Integrated candidate) in
            let proof, token =
              match (B.operation state, B.pending state) with
              | Some operation, Some command ->
                  ( Onton_core_test_support.Scope_fixture.verified
                      operation.B.approved_scope candidate,
                    command.B.token )
              | None, _ | _, None -> failwith "missing scope verification"
            in
            let rec run state = function
              | [] -> permitted state
              | action :: rest ->
                  let state =
                    match action with
                    | 0 ->
                        F.step state
                          (B.Result { token; at = 100.; result = proof })
                    | 1 -> F.step state (request "other-base")
                    | 2 ->
                        F.restore state |> fun state -> F.step state B.Recover
                    | 3 -> (
                        match B.pending state with
                        | Some { kind = B.Inspect; _ } ->
                            F.reply state
                              (B.Inspected
                                 {
                                   (observation 0) with
                                   source = candidate;
                                   head = candidate;
                                   completed_integration = true;
                                   target_included = true;
                                 })
                        | Some
                            {
                              kind =
                                ( Observe | Pin _ | Verify_scope _
                                | Verify_recovery | Continue _ | Publish _
                                | Confirm _ | Commit_merge _
                                | Plan_remote_replay _ | Checkout_remote _
                                | Integrate _ );
                              _;
                            }
                        | None ->
                            state)
                    | 4 -> (
                        match B.pending state with
                        | None -> state
                        | Some _ ->
                            F.reply state
                              (Onton_core_test_support.Scope_fixture.verified
                                 (Replay_scope.Identity
                                    (B.Commit.to_string (F.commit 99)))
                                 (F.commit 99)))
                    | 5 -> (
                        match B.pending state with
                        | None -> state
                        | Some _ ->
                            F.reply state
                              (B.Retryable
                                 { reason = "offline"; retry_after = None }))
                    | 6 -> F.step state (B.Tick 1e9)
                    | 7 -> F.step state B.Resume
                    | 8 ->
                        F.step state (B.Repair_completed { token; at = 100. })
                    | _ -> (
                        match B.pending state with
                        | None -> state
                        | Some _ -> F.reply state (B.Integrated (F.commit 99)))
                  in
                  permitted state && run state rest
            in
            B.integrations state = [] && permitted state && run state actions
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"remote replay cannot reset the checkout using inferred ownership"
        G.(pair (int_range 1 5) (list_size (int_range 0 100) bool))
        (fun (kind, restarts) ->
          try
            let source = F.commit 10 and candidate = F.commit 30 in
            let state = F.step B.empty (request "main") in
            let state = F.reply state (B.Observed (observation 0)) in
            let state = F.reply state B.Pinned in
            let state = F.reply state (B.Integrated candidate) in
            let state =
              match B.operation state with
              | Some operation ->
                  F.reply state
                    (Onton_core_test_support.Scope_fixture.verified
                       operation.B.approved_scope candidate)
              | None -> failwith "missing scope operation"
            in
            let state = F.reply state B.Published in
            let state =
              F.reply state
                (B.Remote { sha = Some (F.commit 40); topology = Diverged })
            in
            let command = B.pending state in
            let boundary_probe =
              match command with
              | Some { kind = B.Plan_remote_replay _; _ } -> true
              | Some
                  {
                    kind =
                      ( Observe | Pin _ | Verify_scope _ | Inspect | Continue _
                      | Publish _ | Confirm _ | Commit_merge _ | Verify_recovery
                      | Checkout_remote _ | Integrate _ );
                    _;
                  }
              | None ->
                  false
            in
            let rejected =
              F.reply state (B.Remote_replay_selected (boundary kind))
            in
            let rec run state = function
              | [] -> permitted state && B.publications state = []
              | restart :: rest ->
                  let state =
                    if restart then
                      F.restore state |> fun state -> F.step state B.Recover
                    else F.step state B.Resume
                  in
                  let state =
                    match command with
                    | None -> state
                    | Some command ->
                        F.step state
                          (B.Result
                             {
                               token = command.B.token;
                               at = 101.;
                               result =
                                 B.Remote_replay_selected (B.Inferred source);
                             })
                  in
                  permitted state && run state rest
            in
            permitted rejected && boundary_probe && run rejected restarts
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"unknown replay boundaries request agent recovery before mutation"
        G.(int_range 1 5)
        (fun kind ->
          try
            let state =
              F.step B.empty (request "main") |> fun state ->
              respond state kind |> fun state -> respond state kind
            in
            permitted state
            &&
            match B.operation state with
            | Some { repair = Some { mode = History_recovery _; _ }; _ } -> true
            | Some
                {
                  repair =
                    None | Some { mode = Diagnosis _ | Content_repair; _ };
                  _;
                }
            | None ->
                false
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "restart, retry, retarget and stale-result interleavings cannot \
           authorize an unrecorded replay"
        G.(pair (int_range 0 5) (list_size (int_range 1 150) (int_range 0 7)))
        (fun (kind, actions) ->
          try
            let initial = F.step B.empty (request "main") in
            let stale = B.pending initial in
            let rec run state = function
              | [] -> permitted state
              | action :: rest ->
                  let state =
                    match action with
                    | 0 | 1 -> respond state kind
                    | 2 ->
                        F.restore state |> fun state -> F.step state B.Recover
                    | 3 -> F.step state B.Resume
                    | 4 -> F.step state (B.Tick 1e9)
                    | 5 -> F.step state (request "new-base")
                    | 6 -> F.step state (request "main")
                    | _ -> (
                        match stale with
                        | None -> state
                        | Some command ->
                            F.step state
                              (B.Result
                                 {
                                   token = command.B.token;
                                   at = 100.;
                                   result = B.Observed (observation kind);
                                 }))
                  in
                  permitted state && run state rest
            in
            run initial actions
          with _ -> false);
    ]
