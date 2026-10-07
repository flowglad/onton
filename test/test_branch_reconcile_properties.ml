(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton_core
module B = Branch_reconcile
module Gen = QCheck2.Gen

let sha c =
  match B.Commit.make (String.make 40 c) with
  | Some sha -> sha
  | None -> assert false

let source = sha 'a'
let target = sha 'b'
let candidate = sha 'c'
let intent = B.{ base = "main"; policy = Rewrite; purpose = Reconcile_base }

let observation =
  B.
    {
      source;
      target;
      remote = Some source;
      boundary = Recorded source;
      topology = Equal;
      clean = true;
      sequencer = None;
      conflicts = 0;
      target_included = false;
      head = source;
      completed_integration = false;
      destination = Branch_reconcile.Remote_id.of_destination "fixture-origin";
    }

let inspected (observation : B.observation) =
  match observation.sequencer with
  | None -> B.Inspected observation
  | Some _ ->
      B.Inspected_active { observation; ready = observation.conflicts = 0 }

let request () = B.step B.empty (B.Request intent)

let reply state result =
  match B.pending state with
  | None -> (state, [])
  | Some command ->
      B.step state (B.Result { token = command.token; at = 100.; result })

let prepared () =
  let state, _ = request () in
  let state, _ = reply state (B.Observed observation) in
  reply state B.Pinned |> fst

let publishing () = reply (prepared ()) (B.Integrated candidate) |> fst

let settled () =
  let state, _ = reply (publishing ()) B.Published in
  reply state (B.Remote { sha = Some candidate; topology = Equal }) |> fst

let progress_result command =
  match command.B.kind with
  | B.Verify_recovery ->
      B.Recovery_verified
        { observation; source_preserved = false; remote_preserved = false }
  | B.Commit_merge _ -> B.Merge_completed candidate
  | B.Plan_remote_replay _ -> B.Remote_replay_selected (B.Recorded source)
  | B.Checkout_remote _ -> B.Remote_checked_out
  | B.Observe -> B.Observed observation
  | B.Pin _ -> B.Pinned
  | B.Integrate _ | B.Continue _ -> B.Integrated candidate
  | B.Inspect ->
      inspected
        {
          observation with
          source = candidate;
          remote = Some candidate;
          target_included = true;
          completed_integration = true;
        }
  | B.Publish _ -> B.Published
  | B.Confirm _ -> B.Remote { sha = Some candidate; topology = Equal }

let recovery () =
  let state, _ = B.step (prepared ()) B.Recover in
  reply state
    (inspected { observation with source = candidate; head = candidate })
  |> fst

let unverified =
  B.Recovery_verified
    {
      observation = { observation with source = candidate; head = candidate };
      source_preserved = false;
      remote_preserved = false;
    }

let tests =
  [
    QCheck2.Test.make
      ~name:
        "BR destination authority survives restart and requires explicit resume"
      ~count:100 Gen.string (fun suffix ->
        let other = B.Remote_id.of_destination ("changed-origin/" ^ suffix) in
        let original = publishing () in
        let state =
          match B.decode (B.yojson_of_t original) with
          | Ok state -> state
          | Error _ -> assert false
        in
        let state, _ = B.step state B.Recover in
        let state, _ =
          reply state (B.Inspected { observation with destination = other })
        in
        let stopped =
          B.phase state
          = Some (B.Intervention "publication_destination_changed")
        in
        let resumed, _ = B.step state B.Resume in
        let duplicate, effects = B.step resumed B.Resume in
        let resumed, _ =
          reply resumed (B.Inspected { observation with destination = other })
        in
        stopped
        && B.equal duplicate (fst (B.step state B.Resume))
        && effects = []
        &&
        match B.operation resumed with
        | None -> false
        | Some op ->
            B.check_destination op ~observed:other = Ok ()
            && B.check_destination op ~observed:observation.destination
               = Error "publication_destination_changed");
    QCheck2.Test.make
      ~name:
        "BR publication destination rejects ambiguity and ignores duplicates"
      ~count:100
      Gen.(int_range 0 5)
      (fun copies ->
        B.publication_destination [] = Error "missing_push_destination"
        && B.publication_destination [ "" ] = Error "missing_push_destination"
        && B.publication_destination [ "one"; "two" ]
           = Error "multiple_push_destinations"
        && B.publication_destination (List.init (copies + 1) (fun _ -> "one"))
           = Ok "one");
    QCheck2.Test.make
      ~name:
        "BR recorded replay publication authority is revision and policy bound"
      ~count:200
      Gen.(triple bool bool bool)
      (fun (recorded, preserve, matches) ->
        let intent =
          {
            intent with
            policy = (if preserve then B.Preserve_ancestry else B.Rewrite);
          }
        in
        let state, _ = B.step B.empty (B.Request intent) in
        let state, _ =
          reply state
            (B.Observed
               {
                 observation with
                 boundary =
                   (if recorded then B.Recorded source else B.Inferred source);
               })
        in
        let state, _ = reply state B.Pinned in
        let state, _ = reply state (B.Integrated candidate) in
        match B.operation state with
        | None -> false
        | Some op ->
            (B.rewrite_publication_input op
               ~candidate:(if matches then candidate else target)
               ~expected:source
            =
            if recorded && (not preserve) && matches then Some (source, source)
            else None)
            && B.rewrite_publication_input op ~candidate ~expected:target = None);
    QCheck2.Test.make
      ~name:"BR completion proof preserves captured integration policy"
      ~count:100
      Gen.(triple bool bool bool)
      (fun (preserve, source_included, reflog_receipt) ->
        B.completion_proven
          ~policy:(if preserve then B.Preserve_ancestry else B.Rewrite)
          ~source_included ~reflog_receipt
        = (reflog_receipt && ((not preserve) || source_included)));
    QCheck2.Test.make
      ~name:"BR satisfied conflict evidence is bound to published head and base"
      ~count:200
      Gen.(pair (pair bool bool) (pair bool bool))
      (fun ((head_matches, base_matches), (branch_matches, published)) ->
        let state = if published then settled () else publishing () in
        B.conflict_satisfied state
          ~head:(if head_matches then candidate else source)
          ~base:(if base_matches then target else source)
          ~base_branch:(if branch_matches then "main" else "other")
        = (head_matches && base_matches && branch_matches && published));
    QCheck2.Test.make
      ~name:"BR history recovery retains dirty work without an agent turn"
      ~count:100 Gen.bool (fun remote_changed ->
        let observation =
          {
            observation with
            clean = false;
            remote = Some (if remote_changed then target else source);
          }
        in
        let state, effects =
          reply (recovery ())
            (B.Recovery_verified
               {
                 observation;
                 source_preserved = false;
                 remote_preserved = false;
               })
        in
        match B.decode (B.yojson_of_t state) with
        | Error _ -> false
        | Ok restarted ->
            let stable, again = B.step restarted B.Recover in
            B.phase state
            = Some (B.Intervention "history_recovery_dirty_worktree")
            && effects = [] && again = [] && B.equal state stable);
    QCheck2.Test.make
      ~name:"BR observed recovery revisions survive new turns and restart"
      ~count:200
      Gen.(list_size (int_range 1 10) (pair (int_range 0 5) (int_range 0 5)))
      (fun revisions ->
        let revision n = sha (Char.chr (Char.code 'a' + n)) in
        let state, required =
          List.fold_left
            (fun (state, required) (local, remote) ->
              let state, _ = B.step state B.Recover in
              let head = revision local and remote = revision remote in
              let state, _ =
                reply state
                  (B.Recovery_verified
                     {
                       observation =
                         {
                           observation with
                           head;
                           source = head;
                           remote = Some remote;
                         };
                       source_preserved = false;
                       remote_preserved = false;
                     })
              in
              (state, head :: remote :: required))
            (recovery (), [])
            revisions
        in
        match (B.decode (B.yojson_of_t state), B.operation state) with
        | Ok restored, Some op -> (
            B.equal state restored
            && List.for_all
                 (fun sha ->
                   List.exists (B.Commit.equal sha) op.recovery_revisions)
                 required
            &&
            match op.repair with
            | Some r -> r.attempts_without_progress = 0
            | None -> false)
        | Error _, _ | _, None -> false);
    QCheck2.Test.make
      ~name:
        "BR explicit resume inspects retained revisions and is duplicate safe"
      ~count:100 Gen.bool (fun history ->
        let original = if history then recovery () else publishing () in
        let stopped, _ =
          reply original (B.Permanent "operator_action_required")
        in
        let polled, effects = B.step stopped B.Recover in
        let resumed, commands = B.step stopped B.Resume in
        let duplicate, duplicate_effects = B.step resumed B.Resume in
        match (B.operation stopped, B.operation resumed, B.pending resumed) with
        | Some before, Some after, Some command ->
            B.equal polled stopped && effects = [] && before.id = after.id
            && before.source = after.source
            && before.target = after.target
            && before.candidate = after.candidate
            && (command.kind = if history then B.Verify_recovery else B.Inspect)
            && commands = [ B.Execute command ]
            && B.equal resumed duplicate && duplicate_effects = []
        | _ -> false);
    QCheck2.Test.make
      ~name:"BR diagnostics distinguish execution, repair, retry and settlement"
      ~count:100
      Gen.(int_range 0 3)
      (fun scenario ->
        let state, expected =
          match scenario with
          | 0 -> (prepared (), "integrating")
          | 1 ->
              ( fst
                  (reply (prepared ())
                     (B.Conflict
                        { head = source; sequencer = "step"; conflicts = 1 })),
                "content repair" )
          | 2 ->
              ( fst
                  (reply (prepared ())
                     (B.Retryable
                        { reason = "network unavailable"; retry_after = None })),
                "waiting" )
          | _ -> (settled (), "settled")
        in
        let details = B.diagnostics state in
        List.assoc_opt "Reconcile" details = Some expected
        && List.assoc_opt "Source SHA" details
           = Some (B.Commit.to_string source)
        && List.assoc_opt "Target SHA" details
           = Some (B.Commit.to_string target)
        && (scenario <> 2
           || List.assoc_opt "Reason" details = Some "network unavailable"));
    QCheck2.Test.make
      ~name:"BR adopted policy governs authority through restart" ~count:100
      Gen.(pair bool bool)
      (fun (requested_merge, adopted_merge) ->
        try
          let policy b = if b then B.Preserve_ancestry else B.Rewrite in
          let state, _ =
            B.step B.empty
              (B.Request { intent with policy = policy requested_merge })
          in
          let state, _ =
            reply state
              (B.Observed_active
                 {
                   observation = { observation with sequencer = Some "active" };
                   policy = policy adopted_merge;
                 })
          in
          let restored =
            match B.decode (B.yojson_of_t state) with
            | Ok restored -> restored
            | Error _ -> assert false
          in
          List.for_all
            (fun state ->
              match B.operation state with
              | Some op ->
                  B.equal_policy (B.execution_policy op) (policy adopted_merge)
              | None -> false)
            [ state; restored ]
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR sequencer stops only dispatch content repair for content changes"
      ~count:100
      Gen.(int_range 0 4)
      (fun status ->
        let checkout =
          Git_observation.of_porcelain ~branch:None
            ~head:(B.Commit.to_string source)
            ~sequencer:
              (Git_observation.Merge { target = B.Commit.to_string target })
            (List.nth
               [
                 "";
                 "M  staged\000";
                 "?? untracked\000";
                 " M unstaged\000";
                 "UU conflict\000";
               ]
               status)
        in
        let result =
          B.stopped_integration_result checkout
            ~detail:"hook or ref lock failed"
        in
        let state, effects = reply (prepared ()) result in
        if status < 3 then
          match B.phase state with
          | Some (B.Waiting _) -> (
              effects = []
              &&
              match B.operation state with
              | Some op -> op.repair = None
              | None -> false)
          | None
          | Some
              ( B.Preparing | B.Integrating | B.Repairing _ | B.Publishing
              | B.Confirming | B.Recovering | B.Settled | B.Intervention _ ) ->
              false
        else
          match effects with
          | [ B.Repair _ ] -> true
          | []
          | B.Execute _ :: _
          | B.Start_repair _ :: _
          | B.Completed _ :: _
          | B.Repair _ :: _ :: _ ->
              false);
    QCheck2.Test.make
      ~name:"BR malformed checkout evidence retries without repair" ~count:100
      Gen.string (fun text ->
        try
          let state, _ = B.step (prepared ()) B.Recover in
          let checkout =
            Git_observation.of_porcelain ~branch:(Some "patch")
              ~head:(B.Commit.to_string source)
              ~sequencer:Git_observation.None_active ("bad" ^ text)
          in
          match B.operation state with
          | None -> false
          | Some op -> (
              let result =
                B.inspection_result op ~branch:"patch" observation checkout
              in
              B.equal_result result
                (B.Retryable
                   {
                     reason = "invalid_checkout_observation";
                     retry_after = None;
                   })
              &&
              let next, effects = reply state result in
              B.is_pending next && effects = []
              &&
              match B.operation next with
              | Some op -> op.repair = None
              | None -> false)
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR active inspection resumes only the captured integration and a \
         ready index"
      ~count:500
      Gen.(pair (triple bool bool bool) (pair bool (int_range 0 3)))
      (fun ( (source_matches, target_matches, branch_matches),
             (policy_matches, status) )
         ->
        try
          let intent =
            {
              intent with
              policy =
                (if policy_matches then B.Rewrite else B.Preserve_ancestry);
            }
          in
          let state, _ = B.step B.empty (B.Request intent) in
          let state, _ = reply state (B.Observed observation) in
          let state, _ = reply state B.Pinned in
          let state, _ = B.step state B.Recover in
          let checkout =
            Git_observation.of_porcelain ~branch:None
              ~head:(B.Commit.to_string candidate)
              ~sequencer:
                (Git_observation.Rebase
                   {
                     target =
                       B.Commit.to_string
                         (if target_matches then target else candidate);
                     original =
                       B.Commit.to_string
                         (if source_matches then source else candidate);
                     step = "2";
                     head_ref =
                       (if branch_matches then "refs/heads/patch"
                        else "refs/heads/other");
                     merge_heads = [];
                   })
              (match status with
              | 0 -> ""
              | 1 -> "M  staged\000"
              | 2 -> " M unstaged\000"
              | _ -> "UU conflicted\000")
          in
          let observed =
            {
              observation with
              head = candidate;
              clean = status = 0;
              sequencer = Some (Git_observation.progress_key checkout);
              conflicts = (if status = 3 then 1 else 0);
            }
          in
          match B.operation state with
          | None -> false
          | Some op ->
              let result =
                B.inspection_result op ~branch:"patch" observed checkout
              in
              if
                source_matches && target_matches && branch_matches
                && policy_matches
              then
                let ready = status < 2 in
                let next, effects = reply state result in
                B.equal_result result
                  (B.Inspected_active { observation = observed; ready })
                &&
                if ready then
                  (match B.pending next with
                    | Some command ->
                        B.equal_command_kind command.kind
                          (B.Continue
                             {
                               head = candidate;
                               target;
                               sequencer = Git_observation.progress_key checkout;
                             })
                    | None -> false)
                  && List.for_all
                       (function
                         | B.Execute _ -> true
                         | B.Repair _ | B.Start_repair _ | B.Completed _ ->
                             false)
                       effects
                else
                  match B.phase next with
                  | Some (B.Repairing r) ->
                      B.equal_repair_mode r.mode B.Content_repair
                  | None
                  | Some
                      ( B.Preparing | B.Integrating | B.Publishing
                      | B.Confirming | B.Waiting _ | B.Recovering | B.Settled
                      | B.Intervention _ ) ->
                      false
              else
                B.equal_result result
                  (B.Recovery_required "active_integration_context_changed")
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR empty merge completion resumes without another repair after lost \
         acknowledgement" ~count:100 Gen.bool (fun restart ->
        try
          let state, _ =
            reply (prepared ())
              (B.Conflict
                 { head = source; sequencer = "merging"; conflicts = 1 })
          in
          let state, _ = B.step state B.Recover in
          let state, _ =
            reply state
              (inspected
                 { observation with sequencer = Some "merging"; conflicts = 0 })
          in
          let sequencer =
            Git_observation.Rebase
              {
                target = B.Commit.to_string target;
                original = B.Commit.to_string source;
                step = "3";
                head_ref = "refs/heads/patch";
                merge_heads = [];
              }
          in
          let resumed_sequencer = Git_observation.sequencer_key sequencer in
          let capture =
            B.
              {
                head = source;
                target;
                parents = [ sha 'd' ];
                sequencer = "merging";
                resumed_sequencer;
              }
          in
          let state, _ = reply state (B.Merge_completion_needed capture) in
          let state =
            match B.decode (B.yojson_of_t state) with
            | Ok state -> state
            | Error e -> failwith e
          in
          let state = if restart then fst (B.step state B.Recover) else state in
          let state, effects = reply state (B.Merge_completed candidate) in
          let checkout =
            Git_observation.of_porcelain ~branch:None
              ~head:(B.Commit.to_string candidate)
              ~sequencer ""
          in
          (match (B.pending state, B.operation state) with
            | ( Some
                  { kind = B.Continue { head; target = actual; sequencer }; _ },
                Some op ) ->
                head = candidate && actual = target
                && sequencer = resumed_sequencer
                && B.continuation op checkout = Git_observation.Continue
            | ( ( None
                | Some
                    {
                      kind =
                        ( B.Observe | B.Pin _ | B.Plan_remote_replay _
                        | B.Checkout_remote _ | B.Commit_merge _ | B.Integrate _
                        | B.Inspect | B.Verify_recovery | B.Publish _
                        | B.Confirm _ );
                      _;
                    } ),
                _ )
            | _, None ->
                false)
          && List.for_all
               (function
                 | B.Execute _ -> true
                 | B.Repair _ | B.Start_repair _ | B.Completed _ -> false)
               effects
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR replay boundary evidence survives checkpoint and checkout"
      ~count:100 Gen.bool (fun inferred ->
        try
          let state, _ = reply (publishing ()) B.Published in
          let incoming = sha 'd' in
          let state, _ =
            reply state (B.Remote { sha = Some incoming; topology = Diverged })
          in
          let boundary =
            if inferred then B.Inferred source else B.Recorded source
          in
          let state, _ = reply state (B.Remote_replay_selected boundary) in
          let state =
            match B.decode (B.yojson_of_t state) with
            | Ok state -> state
            | Error e -> failwith e
          in
          let state, _ = B.step state B.Recover in
          let state, _ =
            reply state
              (inspected
                 {
                   observation with
                   source = candidate;
                   head = candidate;
                   target = candidate;
                   remote = Some incoming;
                   target_included = true;
                 })
          in
          let state, _ = reply state B.Remote_checked_out in
          match B.pending state with
          | Some { kind = B.Integrate actual; _ } -> (
              actual.source = incoming && actual.target = candidate
              && actual.boundary = boundary && actual.policy = B.Rewrite
              &&
              let result = sha 'e' in
              let state, _ = reply state (B.Integrated result) in
              let receipts = B.remote_integrations state in
              let state, _ = reply state (B.Integrated result) in
              let state, _ = reply state B.Published in
              let state, _ =
                reply state (B.Remote { sha = Some result; topology = Equal })
              in
              let state, _ =
                B.step state
                  (B.Request
                     { intent with purpose = Publish_session "later-session" })
              in
              let state =
                match B.decode (B.yojson_of_t state) with
                | Ok state -> state
                | Error e -> failwith e
              in
              B.remote_integrations state = receipts
              &&
              match receipts with
              | [ receipt ] ->
                  receipt.B.capture.source_revision = incoming
                  && receipt.capture.target_revision = candidate
                  && receipt.capture.replay_boundary = boundary
                  && receipt.integrated_revision = result
              | [] | _ :: _ :: _ -> false)
          | None
          | Some
              {
                kind =
                  ( B.Observe | B.Commit_merge _ | B.Plan_remote_replay _
                  | B.Checkout_remote _ | B.Pin _ | B.Inspect
                  | B.Verify_recovery | B.Continue _ | B.Publish _ | B.Confirm _
                    );
                _;
              } ->
              false
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR boundary selection requests missing evidence and respects recorded \
         order"
      ~count:100
      Gen.(pair bool bool)
      (fun (newer, older) ->
        let candidates = [ B.Recorded target; B.Recorded source ] in
        B.choose_boundary ~candidates ~evidence:[] = B.Probe target
        && B.choose_boundary ~candidates ~evidence:[ (target, false) ]
           = B.Probe source
        && B.choose_boundary ~candidates ~evidence:[ (target, true) ]
           = B.Chosen (B.Recorded target)
        &&
        let evidence = [ (target, newer); (source, older) ] in
        let expected =
          B.Chosen
            (if newer then B.Recorded target
             else if older then B.Recorded source
             else B.Plain)
        in
        B.choose_boundary ~candidates ~evidence = expected
        && B.choose_boundary ~candidates ~evidence:(List.rev evidence)
           = expected);
    QCheck2.Test.make
      ~name:
        "BR preparation retains older replay receipts and materialization \
         across restart" ~count:100 Gen.bool (fun restart ->
        try
          let finish state observation result =
            let state, _ = reply state (B.Observed observation) in
            let state, _ = reply state B.Pinned in
            let state, _ = reply state (B.Integrated result) in
            let state, _ = reply state B.Published in
            fst (reply state (B.Remote { sha = Some result; topology = Equal }))
          in
          let state, _ =
            B.step B.empty (B.Materialized (B.New_branch source))
          in
          let state, _ = B.step state (B.Request intent) in
          let state = finish state observation candidate in
          let state, _ =
            B.step state
              (B.Request { intent with purpose = Reconcile_request "second" })
          in
          let state =
            finish state
              {
                observation with
                source = candidate;
                head = candidate;
                target = sha 'd';
                boundary = Recorded target;
                remote = Some candidate;
              }
              (sha 'e')
          in
          let state, _ =
            B.step state
              (B.Request { intent with purpose = Reconcile_request "third" })
          in
          let state =
            if restart then
              match B.decode (B.yojson_of_t state) with
              | Ok restored -> restored
              | Error _ -> assert false
            else state
          in
          let op = Option.get (B.operation state) in
          op.boundary_candidates
          = [ B.Recorded (sha 'd'); B.Recorded target; B.Recorded source ]
          && B.observation_boundaries op = op.boundary_candidates
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR unfinished availability includes intervention but releases settled \
         branches" ~count:100 Gen.bool (fun permanent ->
        let state = prepared () in
        let state, _ =
          reply state
            (if permanent then B.Permanent "policy"
             else B.Retryable { reason = "transport"; retry_after = None })
        in
        B.is_unsettled state
        && B.is_pending state = not permanent
        && (not (B.is_unsettled B.empty))
        && not (B.is_unsettled (settled ())));
    QCheck2.Test.make
      ~name:"BR integration completion requires observed captured ancestry"
      ~count:100
      Gen.(triple bool bool bool)
      (fun (preserve, target_preserved, source_preserved) ->
        try
          let state, _ =
            B.step B.empty
              (B.Request
                 {
                   intent with
                   policy = (if preserve then Preserve_ancestry else Rewrite);
                 })
          in
          let state, _ = reply state (B.Observed observation) in
          let state, _ = reply state B.Pinned in
          let op = Option.get (B.operation state) in
          let result =
            B.integration_result op ~candidate ~target_preserved
              ~source_preserved
          in
          result = B.Integrated candidate
          = (target_preserved && ((not preserve) || source_preserved))
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR captured contributions preserve ancestry and receipt only \
         confirmed publication" ~count:100 Gen.bool (fun restart ->
        try
          let request =
            {
              intent with
              purpose =
                B.Integrate_revision { contributor = "2"; revision = target };
            }
          in
          let state, _ = B.step B.empty (B.Request request) in
          let state, _ = reply state (B.Observed observation) in
          let state, _ = reply state B.Pinned in
          let preserving =
            match B.pending state with
            | Some
                {
                  kind =
                    B.Integrate
                      { policy = Preserve_ancestry; target = actual; _ };
                  _;
                } ->
                actual = target
            | None
            | Some
                {
                  kind =
                    ( B.Commit_merge _ | B.Plan_remote_replay _
                    | B.Checkout_remote _ | B.Observe | B.Pin _ | B.Inspect
                    | B.Verify_recovery | B.Continue _ | B.Publish _
                    | B.Confirm _
                    | B.Integrate { policy = Rewrite; _ } );
                  _;
                } ->
                false
          in
          let state, _ = reply state (B.Integrated candidate) in
          let unpublished =
            B.publications state = [] && B.integrations state = []
          in
          let state, _ = reply state B.Published in
          let state =
            if restart then
              match B.decode (B.yojson_of_t state) with
              | Ok restored -> restored
              | Error _ -> assert false
            else state
          in
          let unconfirmed = B.publications state = [] in
          let state, _ =
            reply state (B.Remote { sha = Some candidate; topology = Equal })
          in
          preserving && unpublished && unconfirmed
          &&
          match B.publications state with
          | [ receipt ] ->
              receipt.published_revision = candidate
              && receipt.published_intent.purpose = request.purpose
              && receipt.published_intent.policy = Preserve_ancestry
          | [] | _ :: _ :: _ -> false
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR adoption pins its original pair before repair and reobserves after \
         publication" ~count:100 Gen.bool (fun restart ->
        try
          let state, _ = request () in
          let state, effects =
            reply state
              (B.Observed_active
                 {
                   observation =
                     {
                       observation with
                       sequencer = Some "existing";
                       conflicts = 0;
                     };
                   policy = Rewrite;
                 })
          in
          let pin = Option.get (B.pending state) in
          let pinned = effects = [ B.Execute pin ] in
          let state =
            if restart then
              match B.decode (B.yojson_of_t state) with
              | Ok restored -> restored
              | Error _ -> assert false
            else state
          in
          let state, _ = reply state B.Pinned in
          let continuing =
            match B.pending state with
            | Some { kind = B.Continue { head; target = actual; sequencer }; _ }
              ->
                head = source && actual = target && sequencer = "existing"
            | None
            | Some
                {
                  kind =
                    ( B.Commit_merge _ | B.Plan_remote_replay _
                    | B.Checkout_remote _ | B.Observe | B.Pin _ | B.Integrate _
                    | B.Inspect | B.Verify_recovery | B.Publish _ | B.Confirm _
                      );
                  _;
                } ->
                false
          in
          let state, _ = reply state (B.Integrated candidate) in
          let state, _ = reply state B.Published in
          let state, _ =
            reply state (B.Remote { sha = Some candidate; topology = Equal })
          in
          pinned && continuing
          &&
          match B.pending state with
          | Some { kind = B.Observe; token; _ } ->
              token.operation > pin.token.operation
          | None
          | Some
              {
                kind =
                  ( B.Commit_merge _ | B.Plan_remote_replay _
                  | B.Checkout_remote _ | B.Pin _ | B.Integrate _ | B.Inspect
                  | B.Verify_recovery | B.Continue _ | B.Publish _ | B.Confirm _
                    );
                _;
              } ->
              false
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR remote descendants confirm publication without further mutation"
      ~count:100 Gen.bool (fun same_as_lease ->
        try
          let state, _ = reply (publishing ()) B.Published in
          let command = Option.get (B.pending state) in
          let result =
            B.Remote
              {
                sha = Some (if same_as_lease then source else sha 'd');
                topology = Behind;
              }
          in
          let state, effects = reply state result in
          let duplicate, repeated =
            B.step state (B.Result { token = command.token; at = 100.; result })
          in
          B.phase state = Some B.Settled
          && B.pending state = None
          && (Option.get (B.operation state)).candidate = Some candidate
          && effects = [ B.Completed command.token.operation ]
          && B.equal duplicate state && repeated = []
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR unchanged remote after failed rewrite publication preserves \
         candidate and lease" ~count:100 Gen.bool (fun restart ->
        try
          let state = publishing () in
          let receipts = B.integrations state in
          let original = Option.get (B.operation state) in
          let state, _ =
            reply state
              (B.Retryable { reason = "transport"; retry_after = None })
          in
          let state =
            if restart then
              match B.decode (B.yojson_of_t state) with
              | Ok restored -> restored
              | Error _ -> assert false
            else state
          in
          let state, _ = B.step state (B.Tick 105.) in
          let state, _ =
            reply state
              (inspected
                 {
                   observation with
                   source = candidate;
                   head = candidate;
                   target_included = true;
                 })
          in
          let state, _ =
            reply state (B.Remote { sha = Some source; topology = Diverged })
          in
          B.integrations state = receipts
          && (Option.get (B.operation state)).id = original.id
          &&
          match B.pending state with
          | Some { kind = B.Publish { candidate = actual; expected }; _ } ->
              B.Commit.equal actual candidate && expected = Some source
          | None
          | Some
              {
                kind =
                  ( B.Commit_merge _ | B.Plan_remote_replay _
                  | B.Checkout_remote _ | B.Observe | B.Pin _ | B.Integrate _
                  | B.Inspect | B.Verify_recovery | B.Continue _ | B.Confirm _ );
                _;
              } ->
              false
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR integration receipt survives failed publication and restart"
      ~count:100 Gen.string (fun reason ->
        try
          let before = prepared () in
          let command = Option.get (B.pending before) in
          let state, _ = reply before (B.Integrated candidate) in
          let duplicate, effects =
            B.step state
              (B.Result
                 {
                   token = command.token;
                   at = 100.;
                   result = B.Integrated candidate;
                 })
          in
          let state, _ =
            reply state
              (B.Retryable
                 { reason = "transport: " ^ reason; retry_after = None })
          in
          match (B.integrations state, B.decode (B.yojson_of_t state)) with
          | [ receipt ], Ok restored ->
              effects = []
              && B.equal duplicate (fst (reply before (B.Integrated candidate)))
              && B.equal state restored
              && receipt.capture.base_branch = Some "main"
              && B.Commit.equal receipt.capture.source_revision source
              && B.Commit.equal receipt.capture.target_revision target
              && B.Commit.equal receipt.integrated_revision candidate
              && B.integrations state = B.integrations duplicate
          | _, Error _ | [], Ok _ | _ :: _ :: _, Ok _ -> false
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR remote races cannot change the recorded base pair" ~count:100
      Gen.(int_range 0 20)
      (fun duplicate_count ->
        let state, _ = reply (publishing ()) B.Published in
        let state, _ =
          reply state (B.Remote { sha = Some (sha 'd'); topology = Diverged })
        in
        let receipts = B.integrations state in
        let state, _ =
          reply state (B.Remote_replay_selected (B.Recorded source))
        in
        let state, _ = reply state B.Remote_checked_out in
        let state, _ = reply state (B.Integrated (sha 'e')) in
        let state =
          List.fold_left
            (fun state _ -> fst (B.step state (B.Request intent)))
            state
            (List.init duplicate_count Fun.id)
        in
        match receipts with
        | [ receipt ] ->
            B.integrations state = receipts
            && B.Commit.equal receipt.capture.target_revision target
            && B.Commit.equal receipt.integrated_revision candidate
        | [] | _ :: _ :: _ -> false);
    QCheck2.Test.make
      ~name:
        "BR explicit rebase requests pin satisfied pairs without branch \
         mutation"
      ~count:100
      Gen.(int_range 1 1000)
      (fun n ->
        let intent =
          { intent with purpose = B.Reconcile_request (string_of_int n) }
        in
        let state, _ = B.step B.empty (B.Request intent) in
        let state, _ =
          reply state (B.Observed { observation with target_included = true })
        in
        let pins =
          Option.map (fun command -> command.B.kind) (B.pending state)
          = Some (B.Pin { source; target })
        in
        let state, _ = reply state B.Pinned in
        let confirms =
          Option.map (fun command -> command.B.kind) (B.pending state)
          = Some (B.Confirm source)
        in
        let state, effects =
          reply state (B.Remote { sha = Some source; topology = Equal })
        in
        let duplicate, duplicate_effects = B.step state (B.Request intent) in
        let changed, _ =
          B.step state
            (B.Request
               {
                 intent with
                 purpose = B.Reconcile_request (string_of_int (n + 1));
               })
        in
        let changed, _ =
          reply changed (B.Observed { observation with target = sha 'd' })
        in
        pins && confirms
        && B.phase state = Some B.Settled
        && (match effects with
          | [ B.Completed _ ] -> true
          | []
          | B.Execute _ :: _
          | B.Repair _ :: _
          | B.Start_repair _ :: _
          | B.Completed _ :: _ :: _ ->
              false)
        && B.equal state duplicate && duplicate_effects = []
        && B.phase changed = Some B.Preparing);
    QCheck2.Test.make
      ~name:"BR materialization distinguishes boundary proof from adoption"
      ~count:100
      Gen.(pair bool (char_range 'a' 'f'))
      (fun (adopted, digit) ->
        try
          let head = sha digit in
          let receipt =
            if adopted then B.Adopted_branch head else B.New_branch head
          in
          let state, _ = B.step B.empty (B.Materialized receipt) in
          B.materialization state = Some receipt
          && B.Commit.equal (B.materialization_head receipt) head
          && B.materialization_boundary receipt
             = if adopted then None else Some head
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR backoff wake keeps pending work without early execution"
      ~count:200
      Gen.(int_range 0 10)
      (fun elapsed ->
        let state, _ =
          reply (prepared ())
            (B.Retryable { reason = "transport"; retry_after = None })
        in
        B.is_pending state
        && B.can_execute_git ~at:(100. +. float_of_int elapsed) state
           = (elapsed >= 5)
        && (B.wake_event ~at:(100. +. float_of_int elapsed) state
           =
           if elapsed < 5 then None
           else Some (B.Tick (100. +. float_of_int elapsed)))
        && not (B.is_pending (settled ())));
    QCheck2.Test.make
      ~name:"BR recovery publication requires all preservation evidence"
      ~count:200
      Gen.(pair (pair bool bool) (pair bool bool))
      (fun ((source_preserved, remote_preserved), (clean, target_included)) ->
        let state, _ =
          reply (recovery ())
            (B.Recovery_verified
               {
                 observation =
                   {
                     observation with
                     source = candidate;
                     head = candidate;
                     clean;
                     target_included;
                   };
                 source_preserved;
                 remote_preserved;
               })
        in
        let publishes =
          match B.pending state with
          | Some { kind = B.Publish _; _ } -> true
          | None
          | Some
              {
                kind =
                  ( B.Commit_merge _ | B.Plan_remote_replay _
                  | B.Checkout_remote _ | B.Observe | B.Pin _ | B.Integrate _
                  | B.Inspect | B.Verify_recovery | B.Continue _ | B.Confirm _ );
                _;
              } ->
              false
        in
        publishes
        = (source_preserved && remote_preserved && clean && target_included));
    QCheck2.Test.make
      ~name:"BR recovery budget ignores transport and survives restart"
      ~count:100
      Gen.(int_range 0 12)
      (fun interruptions ->
        try
          let claim state =
            match reply state unverified with
            | state, [ B.Repair token ] ->
                let claimed, effects = B.step state (B.Repair_started token) in
                assert (effects = [ B.Start_repair token ]);
                let duplicate, effects =
                  B.step claimed (B.Repair_started token)
                in
                assert (B.equal claimed duplicate && effects = []);
                (claimed, token)
            | ( _,
                ( []
                | B.Execute _ :: _
                | B.Start_repair _ :: _
                | B.Completed _ :: _
                | B.Repair _ :: _ :: _ ) ) ->
                failwith "recovery turn not offered"
          in
          let rec interrupt n state =
            if n = 0 then state
            else
              let state, token = claim state in
              let state, _ =
                B.step state
                  (B.Repair_interrupted
                     { token; at = 100.; reason = "transport" })
              in
              let state =
                match B.decode (B.yojson_of_t state) with
                | Ok state -> state
                | Error e -> failwith e
              in
              let state, _ = B.step state (B.Tick 10000.) in
              interrupt (n - 1) state
          in
          let state, token = claim (interrupt interruptions (recovery ())) in
          let turn = Option.get (B.repair_turn state ~branch:"patch" token) in
          let state, _ =
            B.step state
              (B.repair_result ~turn ~at:100. ~before_head:(Some candidate)
                 ~after_head:(Some target) ~timed_out:false ~final_result:true
                 ~detail:"")
          in
          let state, token = claim state in
          let state, _ =
            B.step state (B.Repair_completed { token; at = 100. })
          in
          let state, _ = reply state unverified in
          B.phase state = Some (B.Intervention "recovery_preservation_unproven")
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR repair policy denial stops without dispatching recovery"
      ~count:100
      Gen.(int_range 1 10)
      (fun conflicts ->
        let state, effects =
          reply (prepared ())
            (B.Conflict { head = source; sequencer = "step"; conflicts })
        in
        match effects with
        | [ B.Repair token ] ->
            let claimed, _ = B.step state (B.Repair_started token) in
            let denial =
              B.Repair_denied
                { token; reason = "scripted_patch_requires_manual_repair" }
            in
            let stopped, effects = B.step claimed denial in
            let duplicate, repeated = B.step stopped denial in
            let unrelated = publishing () in
            let unchanged, stale = B.step unrelated denial in
            B.phase stopped
            = Some (B.Intervention "scripted_patch_requires_manual_repair")
            && effects = []
            && B.pending stopped = None
            && B.equal duplicate stopped && repeated = []
            && B.equal unrelated unchanged
            && stale = []
        | []
        | B.Execute _ :: _
        | B.Start_repair _ :: _
        | B.Completed _ :: _
        | B.Repair _ :: _ :: _ ->
            false);
    QCheck2.Test.make ~name:"BR repair claims authorize exactly one agent turn"
      ~count:100
      Gen.(int_range 1 10)
      (fun conflicts ->
        let state, effects =
          reply (prepared ())
            (B.Conflict { head = source; sequencer = "step"; conflicts })
        in
        match effects with
        | [ B.Repair token ] ->
            let unclaimed = B.repair_turn state ~branch:"patch" token in
            let claimed, effects = B.step state (B.Repair_started token) in
            let duplicate, replay = B.step claimed (B.Repair_started token) in
            unclaimed = None
            && Option.is_some (B.repair_turn claimed ~branch:"patch" token)
            && effects = [ B.Start_repair token ]
            && B.equal claimed duplicate && replay = []
        | []
        | B.Execute _ :: _
        | B.Start_repair _ :: _
        | B.Completed _ :: _
        | B.Repair _ :: _ :: _ ->
            false);
    QCheck2.Test.make
      ~name:"BR repair backend failure and HEAD mutation stay distinct"
      ~count:200
      Gen.(triple bool bool bool)
      (fun (changed, timeout, unknown) ->
        try
          let state, effects =
            reply (prepared ())
              (B.Conflict { head = source; sequencer = "step"; conflicts = 1 })
          in
          match effects with
          | [ B.Repair token ] -> (
              let state, _ = B.step state (B.Repair_started token) in
              let before_head = Some source in
              let after_head =
                if unknown then None
                else Some (if changed then target else source)
              in
              let turn =
                Option.get (B.repair_turn state ~branch:"patch" token)
              in
              assert (B.repair_head_matches turn before_head);
              assert (
                B.repair_head_matches turn after_head
                = ((not changed) && not unknown));
              let event =
                B.repair_result ~turn ~at:100. ~before_head ~after_head
                  ~timed_out:timeout ~final_result:true ~detail:""
              in
              let state, _ = B.step state event in
              let expected =
                if changed && not unknown then B.Recovering
                else if timeout || unknown then
                  Waiting
                    { until = 105.; reason = "repair_session_interrupted" }
                else Recovering
              in
              B.phase state = Some expected
              &&
              match B.operation state with
              | Some { repair = Some r; _ } -> r.attempts_without_progress = 0
              | _ -> false)
          | []
          | B.Execute _ :: _
          | B.Start_repair _ :: _
          | B.Completed _ :: _
          | B.Repair _ :: _ :: _ ->
              false
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR repair authority survives lost probes and restart" ~count:200
      Gen.(pair bool bool)
      (fun (head_changed, step_changed) ->
        try
          let state, effects =
            reply (prepared ())
              (B.Conflict { head = source; sequencer = "step"; conflicts = 1 })
          in
          match effects with
          | [ B.Repair token ] -> (
              let state, _ = B.step state (B.Repair_started token) in
              let state, _ =
                B.step state
                  (B.Repair_interrupted
                     { token; at = 100.; reason = "head_probe_unavailable" })
              in
              let recover state =
                let state, _ = B.step state (B.Tick 105.) in
                reply state
                  (inspected
                     {
                       observation with
                       head = (if head_changed then target else source);
                       sequencer =
                         Some (if step_changed then "different" else "step");
                       conflicts = 1;
                       clean = false;
                     })
              in
              match B.decode (B.yojson_of_t state) with
              | Error _ -> false
              | Ok restored -> (
                  let state, effects = recover state in
                  let restarted, restarted_effects = recover restored in
                  B.equal state restarted
                  && effects = restarted_effects
                  &&
                  if head_changed then B.phase state = Some B.Recovering
                  else if step_changed then B.phase state = Some B.Recovering
                  else
                    match B.phase state with
                    | Some (B.Repairing r) -> r.attempts_without_progress = 0
                    | None
                    | Some
                        ( B.Preparing | B.Integrating | B.Publishing
                        | B.Confirming | B.Waiting _ | B.Recovering | B.Settled
                        | B.Intervention _ ) ->
                        false))
          | []
          | B.Execute _ :: _
          | B.Start_repair _ :: _
          | B.Completed _ :: _
          | B.Repair _ :: _ :: _ ->
              false
        with _ -> false);
    QCheck2.Test.make ~name:"BR continuation checks its captured repair HEAD"
      ~count:100 Gen.bool (fun changed ->
        let checkout head =
          Git_observation.of_porcelain ~branch:None
            ~head:(B.Commit.to_string head)
            ~sequencer:
              (Git_observation.Rebase
                 {
                   target = B.Commit.to_string target;
                   original = B.Commit.to_string source;
                   step = "1";
                   head_ref = "refs/heads/patch";
                   merge_heads = [];
                 })
            "M  file\000"
        in
        let sequencer = Git_observation.progress_key (checkout source) in
        let state, _ =
          reply (prepared ())
            (B.Conflict { head = source; sequencer; conflicts = 1 })
        in
        let state, _ = B.step state B.Recover in
        let state, _ =
          reply state
            (inspected
               {
                 observation with
                 head = source;
                 sequencer = Some sequencer;
                 conflicts = 0;
                 clean = false;
               })
        in
        match B.pending state with
        | None -> false
        | Some command ->
            Result.is_ok
              (B.check_checkout command ~branch:"patch"
                 (checkout (if changed then candidate else source)))
            = not changed);
    QCheck2.Test.make
      ~name:"BR restart retains the captured remote-integration action"
      ~count:100
      Gen.(pair (int_range 0 2) bool)
      (fun (purpose, preserve) ->
        try
          let policy = if preserve then B.Preserve_ancestry else B.Rewrite in
          let intent =
            {
              intent with
              policy;
              purpose =
                (match purpose with
                | 0 -> B.Reconcile_base
                | 1 -> B.Publish_revision source
                | _ -> B.Publish_session "completed-session");
            }
          in
          let state, _ = B.step B.empty (B.Request intent) in
          let state, _ =
            reply state (B.Observed { observation with target = source })
          in
          let state, _ = reply state B.Pinned in
          let state =
            if purpose = 0 then fst (reply state (B.Integrated source))
            else state
          in
          let state, _ = reply state B.Published in
          let state, _ =
            reply state (B.Remote { sha = Some target; topology = Diverged })
          in
          let state =
            match B.decode (B.yojson_of_t state) with
            | Ok s -> s
            | Error e -> failwith e
          in
          let state, _ = B.step state B.Recover in
          let state, _ =
            reply state
              (inspected
                 { observation with remote = Some target; topology = Diverged })
          in
          match B.pending state with
          | Some { kind = Integrate actual; _ } ->
              preserve
              && B.Commit.equal actual.source source
              && B.Commit.equal actual.target target
              && B.equal_policy actual.policy Preserve_ancestry
              && B.equal_boundary actual.boundary Plain
          | Some { kind = Plan_remote_replay replay; _ } ->
              (not preserve)
              && B.Commit.equal replay.preserved source
              && B.Commit.equal replay.incoming target
              && List.mem (B.Recorded source) replay.boundaries
          | None
          | Some
              {
                kind =
                  ( Commit_merge _ | Checkout_remote _ | Observe | Pin _
                  | Inspect | Verify_recovery | Continue _ | Publish _
                  | Confirm _ );
                _;
              } ->
              false
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR interrupted session captures once and publishes without base \
         integration" ~count:100 Gen.bool (fun dirty ->
        try
          let state, _ =
            B.step B.empty
              (B.Request { intent with purpose = Publish_session "session-1" })
          in
          let state, _ =
            reply state
              (B.Observed
                 { observation with clean = not dirty; target = source })
          in
          let state, _ = reply state B.Pinned in
          match B.pending state with
          | Some { kind = Publish { candidate; _ }; _ } ->
              B.Commit.equal candidate source
              &&
              let waiting, _ =
                reply state
                  (B.Retryable
                     { reason = "lost acknowledgement"; retry_after = None })
              in
              let restored =
                match B.decode (B.yojson_of_t waiting) with
                | Ok x -> x
                | Error e -> failwith e
              in
              let inspection_state, _ = B.step restored B.Recover in
              let settled, _ =
                reply inspection_state
                  (inspected
                     { observation with clean = not dirty; target = source })
              in
              B.phase settled = Some B.Settled
          | None
          | Some
              {
                kind =
                  ( Commit_merge _ | Plan_remote_replay _ | Checkout_remote _
                  | Observe | Pin _ | Integrate _ | Inspect | Verify_recovery
                  | Continue _ | Confirm _ );
                _;
              } ->
              false
        with _ -> false);
    QCheck2.Test.make ~name:"BR adopted history is not a proven replay boundary"
      ~count:100 Gen.bool (fun adopted ->
        let receipt =
          if adopted then B.Adopted_branch source else B.New_branch source
        in
        let state, _ = B.step B.empty (B.Materialized receipt) in
        let state, _ = B.step state (B.Request intent) in
        match B.operation state with
        | None -> false
        | Some op ->
            B.equal_boundary op.boundary
              (if adopted then Plain else Recorded source));
    QCheck2.Test.make
      ~name:"BR project and branch recovery namespaces are injective" ~count:500
      Gen.(quad string string string string)
      (fun (project, branch, other_project, other_branch) ->
        let same =
          B.recovery_prefix ~project ~branch
          = B.recovery_prefix ~project:other_project ~branch:other_branch
        in
        same = (project = other_project && branch = other_branch));
    QCheck2.Test.make
      ~name:"BR mutation authority rejects another branch or revision"
      ~count:500
      Gen.(triple bool bool bool)
      (fun (publish, different_branch, different_head) ->
        try
          let state = if publish then publishing () else prepared () in
          let command = Option.get (B.pending state) in
          let expected = if publish then candidate else source in
          let checkout =
            Git_observation.of_porcelain
              ~branch:(Some (if different_branch then "other" else "patch"))
              ~head:
                (B.Commit.to_string
                   (if different_head then target else expected))
              ~sequencer:None_active ""
          in
          let allowed =
            B.check_checkout command ~branch:"patch" checkout = Ok ()
          in
          allowed = ((not different_branch) && not different_head)
        with _ -> false);
    QCheck2.Test.make ~name:"BR malformed status cannot authorize mutation"
      ~count:100 Gen.bool (fun publish ->
        try
          let state = if publish then publishing () else prepared () in
          let command = Option.get (B.pending state) in
          let checkout =
            Git_observation.of_porcelain ~branch:(Some "patch")
              ~head:(B.Commit.to_string (if publish then candidate else source))
              ~sequencer:None_active " M unterminated"
          in
          Result.is_error (B.check_checkout command ~branch:"patch" checkout)
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR session publication never authorizes base integration"
      ~count:100 Gen.bool (fun recover ->
        let state, _ =
          B.step B.empty
            (B.Request { intent with purpose = Publish_revision source })
        in
        let state, _ = reply state (B.Observed observation) in
        let state, _ =
          if recover then
            let state, _ = B.step state B.Recover in
            reply state (inspected observation)
          else reply state B.Pinned
        in
        match B.pending state with
        | Some { kind = B.Publish { candidate; _ }; _ } ->
            B.Commit.equal candidate source
        | None
        | Some
            {
              kind =
                ( B.Commit_merge _ | B.Plan_remote_replay _
                | B.Checkout_remote _ | B.Observe | B.Pin _ | B.Integrate _
                | B.Inspect | B.Verify_recovery | B.Continue _ | B.Confirm _ );
              _;
            } ->
            false);
    QCheck2.Test.make
      ~name:"BR session publication requires the captured revision" ~count:100
      Gen.bool (fun dirty ->
        let state, _ =
          B.step B.empty
            (B.Request { intent with purpose = Publish_revision candidate })
        in
        let state, effects =
          reply state (B.Observed { observation with clean = not dirty })
        in
        B.phase state = Some (B.Intervention "publication_source_changed")
        && effects = []);
    QCheck2.Test.make ~name:"BR duplicate command results are fixed points"
      ~count:500
      Gen.(int_range 0 5)
      (fun n ->
        let rec walk state n =
          if n = 0 then state
          else
            match B.pending state with
            | None -> state
            | Some c -> walk (fst (reply state (progress_result c))) (n - 1)
        in
        let state = walk (fst (request ())) n in
        match B.pending state with
        | None -> true
        | Some c ->
            let event =
              B.Result { token = c.token; at = 10.; result = progress_result c }
            in
            let after, _ = B.step state event in
            let duplicate, effects = B.step after event in
            B.equal after duplicate && effects = []);
    QCheck2.Test.make ~name:"BR stale result cannot advance a successor"
      ~count:100 Gen.bool (fun changed ->
        try
          let old = Option.get (B.pending (fst (request ()))) in
          let state = settled () in
          let state, _ =
            B.step state
              (B.Request
                 { intent with base = (if changed then "other" else "main") })
          in
          let after, effects =
            B.step state
              (B.Result { token = old.token; at = 200.; result = B.Pinned })
          in
          B.equal after state && effects = []
        with _ -> false);
    QCheck2.Test.make ~name:"BR serialization preserves recovery decisions"
      ~count:500
      Gen.(int_range 0 5)
      (fun n ->
        try
          let rec advance state n =
            if n = 0 then state
            else
              match B.pending state with
              | None -> state
              | Some c -> advance (fst (reply state (progress_result c))) (n - 1)
          in
          let state = advance (fst (request ())) n in
          match B.decode (B.yojson_of_t state) with
          | Error _ -> false
          | Ok decoded ->
              let before, commands = B.step state B.Recover in
              let after, restored = B.step decoded B.Recover in
              B.equal before after && commands = restored
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR settled state ignores polling and duplicate intent" ~count:500
      Gen.float (fun at ->
        let state = settled () in
        let next, effects = B.step state (B.Tick at) in
        let next, effects2 = B.step next (B.Request intent) in
        B.equal state next && effects = [] && effects2 = []);
    QCheck2.Test.make
      ~name:"BR transport failures do not consume repair attempts" ~count:500
      Gen.(int_range 1 30)
      (fun count ->
        let rec fail state n =
          if n = 0 then state
          else
            let state, _ =
              reply state
                (B.Retryable { reason = "transport"; retry_after = None })
            in
            let state, _ =
              B.step state (B.Tick (Float.of_int (1000 * (count - n + 1))))
            in
            fail state (n - 1)
        in
        match B.operation (fail (publishing ()) count) with
        | Some op -> op.repair = None && op.candidate = Some candidate
        | None -> false);
    QCheck2.Test.make
      ~name:"BR successful services converge without repeated implementation"
      ~count:500
      Gen.(list_size (int_range 0 100) bool)
      (fun duplicates ->
        let state = ref (fst (request ())) in
        let mutations = ref 0 and completed = ref 0 in
        List.iter
          (fun duplicate ->
            match B.pending !state with
            | None -> ()
            | Some command ->
                (match command.kind with
                | B.Integrate _ -> incr mutations
                | B.Commit_merge _ | B.Plan_remote_replay _
                | B.Checkout_remote _ | B.Observe | B.Pin _ | B.Inspect
                | B.Verify_recovery | B.Continue _ | B.Publish _ | B.Confirm _
                  ->
                    ());
                let event =
                  B.Result
                    {
                      token = command.token;
                      at = 100.;
                      result = progress_result command;
                    }
                in
                let next, effects = B.step !state event in
                state := next;
                List.iter
                  (function
                    | B.Completed _ -> incr completed
                    | B.Execute _ | B.Repair _ | B.Start_repair _ -> ())
                  effects;
                if duplicate then state := fst (B.step !state event))
          (duplicates @ [ false; false; false; false; false ]);
        B.phase !state = Some B.Settled && !mutations = 1 && !completed = 1);
    QCheck2.Test.make
      ~name:"BR compatible materialization and intent observations commute"
      ~count:100 Gen.bool (fun preserve ->
        let intent =
          {
            intent with
            policy = (if preserve then B.Preserve_ancestry else B.Rewrite);
          }
        in
        let left =
          fst
            (B.step
               (fst (B.step B.empty (B.Materialized (B.New_branch source))))
               (B.Request intent))
        in
        let right =
          fst
            (B.step
               (fst (B.step B.empty (B.Request intent)))
               (B.Materialized (B.New_branch source)))
        in
        (* Both orders must establish the same boundary before Git is observed,
           not rely on a later observation to repair a lost receipt. *)
        B.equal left right);
    QCheck2.Test.make
      ~name:"BR exhausted content repair enters history recovery" ~count:100
      Gen.(int_range 1 30)
      (fun conflicts ->
        let state, effects =
          reply (prepared ())
            (B.Conflict { head = source; sequencer = "step1"; conflicts })
        in
        let finish state effects =
          match effects with
          | [ B.Repair token ] ->
              let state, _ = B.step state (B.Repair_started token) in
              let state, _ =
                B.step state (B.Repair_completed { token; at = 100. })
              in
              reply state
                (inspected
                   {
                     observation with
                     sequencer = Some "step1";
                     conflicts;
                     clean = false;
                   })
          | []
          | B.Execute _ :: _
          | B.Start_repair _ :: _
          | B.Completed _ :: _
          | B.Repair _ :: _ :: _ ->
              (state, [])
        in
        let state, effects = finish state effects in
        let state, _ = finish state effects in
        match B.pending state with
        | Some { kind = B.Verify_recovery; _ } -> true
        | None
        | Some
            {
              kind =
                ( B.Commit_merge _ | B.Plan_remote_replay _
                | B.Checkout_remote _ | B.Observe | B.Pin _ | B.Integrate _
                | B.Inspect | B.Continue _ | B.Publish _ | B.Confirm _ );
              _;
            } ->
            false);
    QCheck2.Test.make ~name:"BR resolving index conflicts resets repair budget"
      ~count:100
      Gen.(int_range 2 30)
      (fun conflicts ->
        let state, effects =
          reply (prepared ())
            (B.Conflict { head = source; sequencer = "step1"; conflicts })
        in
        match effects with
        | [ B.Repair token ] -> (
            let state, _ = B.step state (B.Repair_started token) in
            let state, _ =
              B.step state (B.Repair_completed { token; at = 100. })
            in
            let state, _ =
              reply state
                (inspected
                   {
                     observation with
                     sequencer = Some "step1";
                     conflicts = conflicts - 1;
                     clean = false;
                   })
            in
            match B.operation state with
            | Some { repair = Some r; _ } -> r.attempts_without_progress = 0
            | _ -> false)
        | []
        | B.Execute _ :: _
        | B.Start_repair _ :: _
        | B.Completed _ :: _
        | B.Repair _ :: _ :: _ ->
            false);
    QCheck2.Test.make ~name:"BR arbitrary checkpoint decoding is total"
      ~count:500 Gen.string (fun text ->
        try
          ignore (B.decode (`String text));
          true
        with _ -> false);
    QCheck2.Test.make ~name:"BR retry deadline honors provider delay" ~count:500
      Gen.(int_range 0 1000)
      (fun delay ->
        let state, _ =
          reply (publishing ())
            (B.Retryable
               { reason = "limit"; retry_after = Some (Float.of_int delay) })
        in
        let before, effects = B.step state (B.Tick 104.) in
        let after, _ =
          B.step before (B.Tick (100. +. Float.of_int (max 5 delay)))
        in
        effects = [] && B.phase after = Some B.Recovering);
  ]

let () = QCheck_base_runner.run_tests_main tests
