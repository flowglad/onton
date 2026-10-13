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
      | Some operation -> (
          match operation.B.candidate_origin with
          | B.Accepted_agent_repair { revision; turn } ->
              revision = candidate
              && operation.candidate_evidence = Some (B.Agent_reported turn)
          | B.Deterministic | B.Capture_agent_repair _ ->
              operation.B.candidate = Some candidate
              && operation.source <> None && operation.target <> None)
      | None -> false)
  | Some { kind = B.Checkout_remote _; _ } -> false
  | Some { kind = B.Integrate { source; target; boundary; policy }; _ } -> (
      match B.operation state with
      | Some operation ->
          B.integration_inputs_match operation ~source ~target ~boundary ~policy
      | None -> false)
  | Some
      {
        kind =
          ( Observe | Pin _ | Inspect | Verify_recovery | Continue _ | Confirm _
          | Commit_merge _ | Plan_remote_replay _ );
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
        | B.Plan_remote_replay _ | B.Publish _ | B.Confirm _ | B.Verify_recovery
          ->
            B.Retryable
              { reason = "fixture transport failure"; retry_after = None }
      in
      F.reply state result

let () =
  QCheck_base_runner.run_tests_main
    [
      QCheck2.Test.make ~count:300
        ~name:"legacy certificates are discarded rather than carried forward"
        G.(int_range 1 50)
        (fun revision ->
          try
            let state = F.step B.empty (request "main") in
            let json =
              match B.yojson_of_t state with
              | `Assoc fields ->
                  `Assoc
                    (List.map
                       (fun (key, value) ->
                         if key = "active" then
                           ( key,
                             match value with
                             | `Assoc fields ->
                                 `Assoc
                                   (( "scope_revision",
                                      `String
                                        (B.Commit.to_string (F.commit revision))
                                    )
                                   :: ( "approved_scope",
                                        `String "malformed legacy certificate"
                                      )
                                   :: List.filter
                                        (fun (key, _) ->
                                          key <> "scope_revision")
                                        fields)
                             | value -> value )
                         else (key, value))
                       fields)
              | json -> json
            in
            match B.decode json with
            | Error _ -> false
            | Ok restored -> B.equal restored state && permitted restored
          with _ -> false);
      QCheck2.Test.make ~count:300
        ~name:"retired certificate commands resume through Git observation"
        G.bool (fun confirming ->
          try
            let state = F.step B.empty (request "main") in
            let state = F.reply state (B.Observed (observation 0)) in
            let state = F.reply state B.Pinned in
            let candidate = F.commit 30 in
            let state = F.reply state (B.Integrated candidate) in
            let json =
              match B.yojson_of_t state with
              | `Assoc fields ->
                  `Assoc
                    (List.map
                       (fun (key, value) ->
                         if key <> "active" then (key, value)
                         else
                           ( key,
                             match value with
                             | `Assoc fields ->
                                 `Assoc
                                   (List.map
                                      (fun (key, value) ->
                                        if key = "phase" then
                                          ( key,
                                            `List
                                              [
                                                `String
                                                  (if confirming then
                                                     "Confirming"
                                                   else "Recovering");
                                              ] )
                                        else if key = "pending" then
                                          ( key,
                                            match value with
                                            | `Assoc fields ->
                                                `Assoc
                                                  (List.map
                                                     (fun (key, value) ->
                                                       if key = "kind" then
                                                         ( key,
                                                           `List
                                                             [
                                                               `String
                                                                 "Verify_scope";
                                                               `String
                                                                 (B.Commit
                                                                  .to_string
                                                                    candidate);
                                                             ] )
                                                       else (key, value))
                                                     fields)
                                            | value -> value )
                                        else (key, value))
                                      fields)
                             | value -> value ))
                       fields)
              | json -> json
            in
            match B.decode json with
            | Error _ -> false
            | Ok restored ->
                B.phase restored = Some B.Recovering
                && (match B.pending restored with
                  | Some command -> B.equal_command_kind command.kind B.Inspect
                  | None -> false)
                && B.equal restored (F.restore restored)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "complete recovery retains its captured task across dispatch and \
           restart"
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
                     result = B.Recovery_observed (observation 0);
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
            let state, _dispatched =
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
                (B.Recovery_observed
                   {
                     (observation 0) with
                     clean = false;
                     sequencer = (if sequencer then Some "rebase" else None);
                   })
            in
            let op = Option.get (B.operation state) in
            let expected = B.Reconstruct_history in
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
              | Some op -> op.B.boundary_candidates = []
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
            let valid state =
              match B.operation state with
              | Some op ->
                  op.source = Some observed.source
                  && op.target = Some observed.target
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
          "successful repair authority survives restart, retarget and stale \
           completions"
        G.(
          triple (int_range 0 2) (int_range 0 2)
            (list_size (int_range 0 80) (int_range 0 4)))
        (fun (task, _evidence, actions) ->
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
                  F.reply initial
                    (B.Publication_rejected
                       (Push_reject_classify.Hook_failure "fixture gate"))
            in
            let baseline = if task = 1 then candidate else source in
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
                     result = B.Recovery_observed inspection;
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
            let state = F.reply state (B.Integrated extended) in
            let operation = Option.get (B.operation state) in
            operation.candidate_origin
            = B.Accepted_agent_repair { turn = token; revision = extended }
            && permitted state
            && B.equal state (F.restore state)
            && List.mem baseline (B.required_revisions state)
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "publication retains its pinned candidate across retries and stale \
           results"
        G.(list_size (int_range 1 150) (int_range 0 5))
        (fun actions ->
          try
            let candidate = F.commit 30 in
            let initial = F.step B.empty (request "main") in
            let stale = Option.get (B.pending initial) in
            let state = F.reply initial (B.Observed (observation 2)) in
            let state = F.reply state B.Pinned in
            let state = F.reply state (B.Integrated candidate) in
            let valid state =
              permitted state
              && (Option.get (B.operation state)).B.candidate = Some candidate
              && B.equal state (F.restore state)
            in
            let _, valid =
              List.fold_left
                (fun (state, ok) action ->
                  let state =
                    match action with
                    | 0 -> F.restore state
                    | 1 -> F.step state (request "other-base")
                    | 2 -> F.step state B.Recover
                    | 3 ->
                        F.step state
                          (B.Result
                             {
                               token = stale.token;
                               at = 100.;
                               result = B.Integrated (F.commit 99);
                             })
                    | 4 -> F.step state (B.Tick 1e9)
                    | _ -> F.step state B.Resume
                  in
                  (state, ok && valid state))
                (state, valid state)
                actions
            in
            valid
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:"remote replay uses observed ancestors across restart"
        G.(pair (int_range 1 5) (list_size (int_range 0 100) bool))
        (fun (kind, restarts) ->
          try
            let source = F.commit 10 and candidate = F.commit 30 in
            let state = F.step B.empty (request "main") in
            let state = F.reply state (B.Observed (observation 0)) in
            let state = F.reply state B.Pinned in
            let state = F.reply state (B.Integrated candidate) in
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
                      ( Observe | Pin _ | Inspect | Continue _ | Publish _
                      | Confirm _ | Commit_merge _ | Verify_recovery
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
        ~name:"rebase inputs do not require recorded ownership boundaries"
        G.(int_range 1 5)
        (fun kind ->
          try
            let state =
              F.step B.empty (request "main") |> fun state ->
              respond state kind |> fun state -> respond state kind
            in
            permitted state
            &&
            match B.pending state with
            | Some { kind = B.Integrate { source; target; boundary; _ }; _ } ->
                source = F.commit 10
                && target = F.commit 20
                && B.equal_boundary boundary (observation kind).boundary
            | Some
                {
                  kind =
                    ( Observe | Pin _ | Inspect | Verify_recovery | Continue _
                    | Publish _ | Confirm _ | Commit_merge _
                    | Plan_remote_replay _ | Checkout_remote _ );
                  _;
                }
            | None ->
                false
          with _ -> false);
      QCheck2.Test.make ~count:1000
        ~name:
          "restart, retry, retarget and stale results retain captured Git \
           inputs"
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
