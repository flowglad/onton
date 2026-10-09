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
      base_contains_source = false;
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

let is_diagnosis state =
  match B.phase state with
  | Some (B.Repairing { mode = Diagnosis _; _ }) -> true
  | None
  | Some
      ( Preparing | Integrating
      | Repairing { mode = Content_repair | History_recovery _; _ }
      | Publishing | Confirming | Waiting _ | Recovering | Settled
      | Intervention _ ) ->
      false

(* Terminal-state fixtures must actually consume the two owned diagnostic turns. *)
let exhaust_diagnosis state =
  let rec loop remaining state =
    if remaining = 0 then state
    else
      match B.operation state with
      | Some { phase = Repairing { mode = Diagnosis { reason }; _ }; _ } ->
          let state = fst (B.step state B.Recover) in
          let state, effects = reply state (B.Needs_diagnosis reason) in
          let token =
            match effects with
            | [ B.Repair token ] -> token
            | []
            | (B.Execute _ | B.Start_repair _ | B.Completed _) :: _
            | B.Repair _ :: _ :: _ ->
                failwith "diagnosis not offered"
          in
          let state = fst (B.step state (B.Repair_started token)) in
          let state =
            fst (B.step state (B.Repair_completed { token; at = 100. }))
          in
          loop (remaining - 1) (fst (reply state (B.Needs_diagnosis reason)))
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating
              | Repairing { mode = Content_repair | History_recovery _; _ }
              | Publishing | Confirming | Waiting _ | Recovering | Settled
              | Intervention _ );
            _;
          } ->
          state
  in
  loop 2 state

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
          base_contains_source = false;
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

let equivalence_fixture equivalent =
  let revision n = Printf.sprintf "%040x" n in
  let records =
    List.mapi
      (fun index equivalent ->
        Printf.sprintf "%s\000%s\000%s\n"
          (if equivalent then "=" else ">")
          (revision (index + 1))
          (revision index))
      equivalent
  in
  let source =
    match B.Commit.make (revision (List.length equivalent)) with
    | Some value -> value
    | None -> assert false
  in
  (source, String.concat "" (List.rev records), revision)

let tests =
  [
    QCheck2.Test.make ~count:200
      ~name:"BR successful evidence resets only its observation budget" Gen.bool
      (fun mutate ->
        try
          let state = if mutate then prepared () else fst (request ()) in
          let state =
            if mutate then
              fst (reply state (B.Attempt_failed "validation timeout"))
            else state
          in
          let state = if mutate then fst (B.step state B.Recover) else state in
          let state =
            fst
              (reply state
                 (B.Retryable
                    { reason = "probe unavailable"; retry_after = None }))
          in
          let failed = Option.get (B.operation state) in
          assert (failed.observation_failures = 1);
          assert (not (is_diagnosis state));
          let state = Result.get_ok (B.decode (B.yojson_of_t state)) in
          let state = fst (B.step state B.Recover) in
          let state = fst (reply state (B.Inspected observation)) in
          let observed = Option.get (B.operation state) in
          observed.observation_failures = 0
          && observed.deterministic_failures = failed.deterministic_failures
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "BR diagnostic completion works without HEAD and charges accepted \
         failures"
      Gen.(triple bool bool bool)
      (fun (accepted, timed_out, final_result) ->
        try
          let state, _ = request () in
          let state, effects =
            reply state (B.Needs_diagnosis "missing checkout")
          in
          let token =
            match effects with
            | [ B.Repair token ] -> token
            | []
            | (B.Execute _ | B.Start_repair _ | B.Completed _) :: _
            | B.Repair _ :: _ :: _ ->
                failwith "missing diagnosis"
          in
          let state, _ = B.step state (B.Repair_started token) in
          let turn = Option.get (B.repair_turn state ~branch:"patch" token) in
          let event =
            B.repair_result ~turn ~turn_accepted:accepted ~at:100.
              ~before_head:None ~after_head:None ~timed_out ~final_result
              ~detail:"unavailable"
          in
          let expected =
            if (not timed_out) && final_result then
              B.Repair_completed { token; at = 100. }
            else if accepted then
              B.Repair_failed { token; at = 100.; reason = "unavailable" }
            else
              B.Repair_interrupted { token; at = 100.; reason = "unavailable" }
          in
          B.repair_head_matches turn None
          && B.equal_event event expected
          && B.equal_event
               (B.repair_result ~turn ~turn_accepted:true ~at:100.
                  ~before_head:(Some source) ~after_head:(Some candidate)
                  ~timed_out:false ~final_result:true ~detail:"")
               (B.Repair_failed
                  { token; at = 100.; reason = "diagnostic_checkout_changed" })
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "BR repeated unavailable observations request diagnosis without \
         consuming mutation attempts" Gen.string (fun reason ->
        try
          let state, _ = request () in
          let fail state =
            reply state (B.Retryable { reason; retry_after = None })
          in
          let state, _ = fail state in
          let state = Result.get_ok (B.decode (B.yojson_of_t state)) in
          let state, _ = B.step state (B.Tick 1000.) in
          let state, effects = fail state in
          is_diagnosis state && effects <> []
          &&
          match B.operation state with
          | Some op -> op.deterministic_failures = 0 && op.source = None
          | None -> false
        with _ -> false);
    QCheck2.Test.make ~count:400
      ~name:
        "BR arbitrary diagnostic reasons require two owned turns before \
         holding across restart"
      Gen.(triple string string bool)
      (fun (first, second, failed) ->
        try
          let restore s = Result.get_ok (B.decode (B.yojson_of_t s)) in
          let state, _ = request () in
          let state, effects = reply state (B.Needs_diagnosis first) in
          let complete state effects =
            assert (is_diagnosis state);
            let token =
              match effects with
              | [ B.Repair token ] -> token
              | []
              | (B.Execute _ | B.Start_repair _ | B.Completed _) :: _
              | B.Repair _ :: _ :: _ ->
                  failwith "no diagnostic turn"
            in
            let state = restore state in
            let state, _ = B.step state (B.Repair_started token) in
            let state = restore state in
            let turn = Option.get (B.repair_turn state ~branch:"patch" token) in
            assert (turn.head = None);
            assert (
              Base.String.is_substring turn.prompt
                ~substring:"repair local gate state"
              && Base.String.is_substring turn.prompt
                   ~substring:"dependencies and system tools"
              && Base.String.is_substring turn.prompt
                   ~substring:"does not authorize"
              && Base.String.is_substring turn.prompt ~substring:"Git history");
            let event =
              if failed then
                B.Repair_failed { token; at = 100.; reason = "backend timeout" }
              else B.Repair_completed { token; at = 100. }
            in
            let state, _ = B.step state event in
            let duplicate, effects = B.step state event in
            assert (B.equal state duplicate && effects = []);
            let state = restore state in
            let state =
              if failed then fst (B.step state (B.Tick 1000.)) else state
            in
            restore state
          in
          let state = complete state effects in
          let state, effects = reply state (B.Needs_diagnosis second) in
          let state = complete state effects in
          let state, effects = reply state (B.Needs_diagnosis first) in
          let state = restore state in
          effects = []
          && B.pending state = None
          &&
          match B.phase state with
          | Some (B.Intervention _) -> true
          | None
          | Some
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled ) ->
              false
        with _ -> false);
    QCheck2.Test.make ~count:300
      ~name:"BR recovery acceptance excludes startup and error-only streams"
      Gen.string (fun text ->
        let module S = Types.Stream_event in
        List.for_all B.repair_event_accepted
          [
            S.Turn_started;
            S.Text_delta text;
            S.Tool_use { name = text; input = text; status = None };
            S.Final_result { text; stop_reason = Types.Stop_reason.End_turn };
          ]
        && (not (B.repair_event_accepted (S.Error text)))
        && not
             (B.repair_event_accepted
                (S.Session_init
                   {
                     session_id = text;
                     api_key_source = None;
                     model = None;
                     claude_code_version = None;
                     permission_mode = None;
                   })));
    QCheck2.Test.make ~count:300
      ~name:
        "BR recovery prompt retains arbitrary task context guidance and owner \
         instructions"
      Gen.(pair string (list_size (int_range 0 10) string))
      (fun (context, guidance) ->
        try
          let state, effects = reply (recovery ()) unverified in
          match effects with
          | [ B.Repair token ] ->
              let state, _ = B.step state (B.Repair_started token) in
              let turn =
                Option.get (B.repair_turn state ~branch:"patch" token)
              in
              let prompt = B.recovery_prompt ~context ~guidance turn in
              Base.String.is_prefix prompt ~prefix:context
              && Base.String.is_suffix prompt ~suffix:turn.prompt
              && List.for_all
                   (fun text -> Base.String.is_substring prompt ~substring:text)
                   guidance
          | []
          | B.Execute _ :: _
          | B.Start_repair _ :: _
          | B.Completed _ :: _
          | B.Repair _ :: _ :: _ ->
              false
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "BR verified remote completion discharges exhausted mutation attempts \
         without an agent" Gen.unit (fun () ->
        try
          let state, _ =
            reply (publishing ()) (B.Attempt_failed "lost push acknowledgement")
          in
          let state, _ = B.step state (B.Tick 1000.) in
          let state, _ =
            reply state
              (inspected
                 {
                   observation with
                   source = candidate;
                   head = candidate;
                   target_included = true;
                   completed_integration = true;
                 })
          in
          let state, _ =
            reply state (B.Remote { sha = Some source; topology = B.Diverged })
          in
          let state, _ =
            reply state (B.Attempt_failed "second lost acknowledgement")
          in
          let state, effects =
            reply state
              (B.Recovery_verified
                 {
                   observation =
                     {
                       observation with
                       source = candidate;
                       head = candidate;
                       remote = Some candidate;
                       target_included = true;
                     };
                   source_preserved = true;
                   remote_preserved = true;
                 })
          in
          B.phase state = Some B.Settled
          &&
          match effects with
          | [ B.Completed _ ] -> true
          | []
          | B.Execute _ :: _
          | B.Repair _ :: _
          | B.Start_repair _ :: _
          | B.Completed _ :: _ :: _ ->
              false
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "BR publication rejection offers bounded repair even when history is \
         already valid"
      Gen.(pair bool bool)
      (fun (restart, accepted_failure) ->
        try
          let restored state =
            if restart then
              match B.decode (B.yojson_of_t state) with
              | Ok state -> state
              | Error message -> failwith message
            else state
          in
          let verified =
            B.Recovery_verified
              {
                observation =
                  {
                    observation with
                    source = candidate;
                    head = candidate;
                    target_included = true;
                  };
                source_preserved = true;
                remote_preserved = true;
              }
          in
          let reject state =
            fst
              (reply (restored state)
                 (B.Publication_rejected
                    (Push_reject_classify.Hook_failure "validation failed")))
          in
          let attempt state =
            let state, effects = reply (restored state) verified in
            match effects with
            | [ B.Repair token ] ->
                assert (
                  match B.operation state with
                  | Some
                      {
                        repair =
                          Some
                            {
                              mode =
                                B.History_recovery
                                  { task = B.Repair_publication; _ };
                              _;
                            };
                        _;
                      } ->
                      true
                  | None
                  | Some { repair = None; _ }
                  | Some
                      {
                        repair =
                          Some
                            {
                              mode =
                                ( B.Diagnosis _ | B.Content_repair
                                | B.History_recovery
                                    {
                                      task =
                                        ( B.Finish_local_work
                                        | B.Reconstruct_history );
                                      _;
                                    } );
                              _;
                            };
                        _;
                      } ->
                      false);
                let state, _ =
                  B.step (restored state) (B.Repair_started token)
                in
                let state, _ =
                  B.step state
                    (if accepted_failure then
                       B.Repair_failed
                         { token; at = 100.; reason = "accepted timeout" }
                     else B.Repair_completed { token; at = 100. })
                in
                let state, _ = B.step (restored state) (B.Tick 1000.) in
                let state, _ = reply state verified in
                assert (
                  match B.pending state with
                  | Some { kind = B.Publish _; _ } -> true
                  | None
                  | Some
                      {
                        kind =
                          ( B.Observe | B.Inspect | B.Pin _ | B.Integrate _
                          | B.Verify_recovery | B.Continue _ | B.Confirm _
                          | B.Commit_merge _ | B.Plan_remote_replay _
                          | B.Checkout_remote _ );
                        _;
                      } ->
                      false);
                restored state
            | []
            | B.Execute _ :: _
            | B.Start_repair _ :: _
            | B.Completed _ :: _
            | B.Repair _ :: _ :: _ ->
                failwith "publication agent was bypassed"
          in
          let state = attempt (reject (publishing ())) in
          assert (
            Option.map
              (fun (op : B.operation) -> op.publication_recovery_attempts)
              (B.operation state)
            = Some 1);
          let state = attempt (reject state) in
          assert (
            Option.map
              (fun (op : B.operation) -> op.publication_recovery_attempts)
              (B.operation state)
            = Some 2);
          let state, effects = reply (reject state) verified in
          B.pending state = None
          && effects = []
          &&
          match B.phase state with
          | Some (B.Intervention _) -> true
          | None
          | Some
              ( B.Preparing | B.Integrating | B.Repairing _ | B.Publishing
              | B.Confirming | B.Recovering | B.Waiting _ | B.Settled ) ->
              false
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "BR publication authority denials enter bounded publication recovery"
      Gen.bool (fun workflow ->
        let rejection =
          if workflow then Push_reject_classify.Workflow_scope_missing
          else Push_reject_classify.Permission_denied
        in
        let state, effects =
          reply (publishing ()) (B.Publication_rejected rejection)
        in
        effects <> []
        && Option.map (fun c -> c.B.kind) (B.pending state)
           = Some B.Verify_recovery);
    QCheck2.Test.make ~count:100
      ~name:"BR publication mutation budget survives confirmation and restart"
      Gen.unit (fun () ->
        try
          let state, _ = reply (publishing ()) (B.Attempt_failed "timeout") in
          let state =
            match B.decode (B.yojson_of_t state) with
            | Ok state -> state
            | Error message -> failwith message
          in
          let state, _ = B.step state (B.Tick 1000.) in
          let state, _ =
            reply state
              (inspected
                 {
                   observation with
                   source = candidate;
                   head = candidate;
                   target_included = true;
                   completed_integration = true;
                 })
          in
          let state, _ =
            reply state (B.Remote { sha = Some source; topology = B.Diverged })
          in
          assert (
            match B.pending state with
            | Some { kind = B.Publish _; _ } -> true
            | None
            | Some
                {
                  kind =
                    ( B.Observe | B.Inspect | B.Pin _ | B.Integrate _
                    | B.Verify_recovery | B.Continue _ | B.Confirm _
                    | B.Commit_merge _ | B.Plan_remote_replay _
                    | B.Checkout_remote _ );
                  _;
                } ->
                false);
          let state, _ = reply state (B.Attempt_failed "timeout") in
          let state, effects =
            reply state
              (B.Recovery_verified
                 {
                   observation =
                     {
                       observation with
                       source = candidate;
                       head = candidate;
                       target_included = true;
                     };
                   source_preserved = true;
                   remote_preserved = true;
                 })
          in
          (match effects with
            | [ B.Repair _ ] -> true
            | []
            | B.Execute _ :: _
            | B.Start_repair _ :: _
            | B.Completed _ :: _
            | B.Repair _ :: _ :: _ ->
                false)
          &&
          match B.operation state with
          | Some
              {
                deterministic_failures;
                repair =
                  Some
                    {
                      mode =
                        B.History_recovery { task = B.Repair_publication; _ };
                      _;
                    };
                _;
              } ->
              deterministic_failures = 2
          | None
          | Some { repair = None; _ }
          | Some
              {
                repair =
                  Some
                    {
                      mode =
                        ( B.Diagnosis _ | B.Content_repair
                        | B.History_recovery
                            {
                              task = B.Finish_local_work | B.Reconstruct_history;
                              _;
                            } );
                      _;
                    };
                _;
              } ->
              false
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "legacy verification preserves a newer queued intent before \
         verification"
      Gen.string (fun base ->
        try
          let first =
            B.
              {
                base = "main";
                policy = Preserve_ancestry;
                purpose = Publish_session "first";
              }
          in
          let second =
            B.
              {
                base;
                policy = Preserve_ancestry;
                purpose = Publish_session "second";
              }
          in
          let owner = fst (B.step B.empty (B.Request first)) in
          let owner = fst (B.step owner (B.Request second)) in
          let imported = B.import_legacy_publication owner ~base:"main" in
          let imported =
            match B.decode (B.yojson_of_t imported) with
            | Ok owner -> owner
            | Error e -> failwith e
          in
          let no_work =
            B.Observed
              { observation with target = source; base_contains_source = true }
          in
          let next = fst (reply imported no_work) in
          let verifying = fst (reply next no_work) in
          B.pending owner = B.pending imported
          && (match B.operation next with
            | Some op -> B.equal_intent op.B.intent second
            | None -> false)
          && (match B.operation verifying with
            | Some op -> B.equal_purpose op.B.intent.purpose Verify_publication
            | None -> false)
          && B.publications verifying = []
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "publication verification does not acquire replay-boundary authority"
      Gen.bool (fun before ->
        let materialize s =
          fst (B.step s (B.Materialized (B.New_branch target)))
        in
        let owner =
          B.import_legacy_publication
            (if before then materialize B.empty else B.empty)
            ~base:"main"
        in
        let owner = if before then owner else materialize owner in
        match B.operation owner with
        | Some op ->
            B.observation_boundaries op = []
            && B.equal_boundary op.B.boundary B.Plain
        | None -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "legacy publication imports are total idempotent and retain unknown \
         bases"
      Gen.string (fun base ->
        try
          let owner = B.import_legacy_publication B.empty ~base in
          B.is_pending owner
          && B.publications owner = []
          && B.equal owner (B.import_legacy_publication owner ~base)
          &&
          match B.decode (B.yojson_of_t owner) with
          | Ok restored -> B.equal owner restored
          | Error _ -> false
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"legacy verification cannot decode a mutation command" Gen.bool
      (fun mutation ->
        try
          let field key = function
            | `Assoc fields -> List.assoc key fields
            | _ -> failwith "not object"
          in
          let replace key value = function
            | `Assoc fields ->
                `Assoc ((key, value) :: List.remove_assoc key fields)
            | _ -> failwith "not object"
          in
          let owner = B.import_legacy_publication B.empty ~base:"main" in
          let json = B.yojson_of_t owner in
          let kind =
            field "kind"
              (field "pending"
                 (field "active"
                    (B.yojson_of_t (if mutation then prepared () else owner))))
          in
          let active = field "active" json in
          let active =
            replace "pending"
              (replace "kind" kind (field "pending" active))
              active
          in
          let result = B.decode (replace "active" active json) in
          if mutation then Result.is_error result
          else
            match result with
            | Ok restored -> B.equal owner restored
            | Error _ -> false
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "legacy verification confirms exact remote without publication commands"
      Gen.(pair bool bool)
      (fun (exists, contained) ->
        try
          let restart s =
            match B.decode (B.yojson_of_t s) with
            | Ok s -> s
            | Error e -> failwith e
          in
          let initial = B.import_legacy_publication B.empty ~base:"main" in
          let o =
            {
              observation with
              target = source;
              base_contains_source = contained;
            }
          in
          let pinned = fst (reply (restart initial) (B.Observed o)) in
          let confirming = fst (reply (restart pinned) B.Pinned) in
          let sha = if exists then Some source else None in
          let final =
            fst
              (reply (restart confirming) (B.Remote { sha; topology = Equal }))
          in
          B.equal initial (B.import_legacy_publication initial ~base:"main")
          && B.equal final (restart final)
          && (match (B.pending pinned, B.pending confirming) with
            | Some pin, Some confirm ->
                B.equal_command_kind pin.B.kind
                  (B.Pin { source; target = source })
                && B.equal_command_kind confirm.B.kind (B.Confirm source)
            | None, _ | _, None -> false)
          && Bool.equal (B.publications final <> []) exists
          && (if exists then B.phase final = Some B.Settled
              else is_diagnosis final)
          &&
          match B.operation final with
          | Some op -> B.initial_publication_candidate op ~remote:sha = None
          | None -> false
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "explicit resume hands failed legacy verification to queued \
         reconciliation without losing evidence"
      Gen.(triple bool bool bool)
      (fun (changed, requested, resume) ->
        let open B in
        try
          let initial = B.import_legacy_publication B.empty ~base:"main" in
          let pinned =
            fst
              (reply initial
                 (B.Observed
                    { observation with target = source; remote = Some target }))
          in
          let confirming = fst (reply pinned B.Pinned) in
          let old_command = Option.get (B.pending confirming) in
          let stopped =
            fst
              (reply confirming
                 (if changed then
                    B.Needs_diagnosis "legacy_publication_source_changed"
                  else B.Remote { sha = Some target; topology = Diverged }))
          in
          let stopped = exhaust_diagnosis stopped in
          let requested_state, effects =
            if requested then B.step stopped (B.Request intent)
            else (stopped, [])
          in
          assert (
            effects = [] && B.operation requested_state = B.operation stopped);
          let state =
            Result.get_ok (B.decode (B.yojson_of_t requested_state))
          in
          let next, _ = B.step state (if resume then B.Resume else B.Recover) in
          let op = Option.get (B.operation next) in
          if requested && resume then (
            assert (B.equal_intent op.intent intent);
            assert (op.id <> old_command.token.operation);
            assert (B.execution_policy op = B.Preserve_ancestry);
            assert (
              List.mem source op.recovery_revisions
              && List.mem target op.recovery_revisions);
            assert (op.source = None && op.candidate = None);
            assert ((Option.get (B.pending next)).kind = B.Observe);
            let unchanged, effects =
              B.step next
                (B.Result
                   {
                     token = old_command.token;
                     at = 200.;
                     result = B.Remote { sha = Some source; topology = Equal };
                   })
            in
            assert (B.equal unchanged next && effects = []))
          else assert (op.intent.purpose = B.Verify_publication);
          B.equal next (Result.get_ok (B.decode (B.yojson_of_t next)))
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "legacy claim preserves pending command and verifies after no-work \
         settlement" Gen.bool (fun no_work ->
        try
          let pending, _ =
            B.step B.empty
              (B.Request
                 {
                   base = "main";
                   policy = Preserve_ancestry;
                   purpose = Publish_session "old";
                 })
          in
          let imported = B.import_legacy_publication pending ~base:"main" in
          let restored =
            match B.decode (B.yojson_of_t imported) with
            | Ok s -> s
            | Error e -> failwith e
          in
          let observed =
            fst
              (reply restored
                 (B.Observed
                    {
                      observation with
                      target = source;
                      remote = None;
                      base_contains_source = no_work;
                    }))
          in
          let final =
            if no_work then observed
            else
              let pinned = fst (reply observed B.Pinned) in
              let pushed = fst (reply pinned B.Published) in
              fst
                (reply pushed
                   (B.Remote { sha = Some source; topology = Equal }))
          in
          B.pending imported = B.pending pending
          && B.operation imported = B.operation pending
          && B.equal imported restored
          && B.equal imported
               (B.import_legacy_publication imported ~base:"other")
          &&
          match B.operation final with
          | Some op when no_work ->
              B.equal_purpose op.intent.purpose B.Verify_publication
              && B.publications final = []
              && B.is_pending final
          | Some op ->
              B.equal_purpose op.intent.purpose (B.Publish_session "old")
              && B.publications final <> []
              && B.phase final = Some B.Settled
          | None -> false
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "legacy verification is read-only under hostile results and restarts"
      Gen.(list_size (int_range 0 80) (int_range 0 9))
      (fun actions ->
        try
          let initial = B.import_legacy_publication B.empty ~base:"main" in
          let _, valid =
            List.fold_left
              (fun (state, valid) action ->
                let next, effects =
                  match action with
                  | 0 -> B.step state B.Recover
                  | 1 -> B.step state B.Resume
                  | 2 ->
                      reply state
                        (B.Observed { observation with target = source })
                  | 3 ->
                      reply state
                        (B.Inspected { observation with target = source })
                  | 4 -> reply state B.Pinned
                  | 5 ->
                      reply state
                        (B.Remote { sha = Some source; topology = Equal })
                  | 6 -> reply state (B.Integrated candidate)
                  | 7 -> reply state B.Published
                  | 8 -> reply state (B.Recovery_required "uncertain")
                  | _ ->
                      reply state
                        (B.Retryable { reason = "offline"; retry_after = None })
                in
                let allowed =
                  List.for_all
                    (function
                      | B.Execute c ->
                          B.command_allowed_for_purpose B.Verify_publication
                            c.kind
                      | B.Completed _ -> true
                      | B.Repair _ | B.Start_repair _ -> is_diagnosis next)
                    effects
                in
                let restored =
                  match B.decode (B.yojson_of_t next) with
                  | Ok s -> s
                  | Error e -> failwith e
                in
                (restored, valid && allowed && B.equal restored next))
              (initial, true) actions
          in
          valid
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:"verification checkout requires clean named captured HEAD"
      Gen.(triple bool bool bool)
      (fun (named, same, dirty) ->
        let checkout =
          Git_observation.of_porcelain
            ~branch:(if named then Some "patch" else None)
            ~head:(B.Commit.to_string (if same then source else candidate))
            ~sequencer:Git_observation.None_active
            (if dirty then "?? work\000" else "")
        in
        Bool.equal
          (B.verification_checkout_valid ~branch:"patch" ~candidate:source
             checkout)
          (named && same && not dirty));
    QCheck2.Test.make ~count:500
      ~name:
        "BR recovery dependencies include every serialized revision across \
         interleavings"
      Gen.(list_size (int_range 0 100) (int_range 0 15))
      (fun actions ->
        try
          (* Fixture text fields are deliberately not commit-shaped. The
             destination is a hash too, but is not an object dependency. This
             oracle discovers revisions without knowing the checkpoint's
             command, repair, replay or receipt schema. *)
          let rec serialized_revisions = function
            | `String value -> Option.to_list (B.Commit.make value)
            | `List values | `Tuple values ->
                List.concat_map serialized_revisions values
            | `Assoc fields ->
                List.concat_map
                  (fun (key, value) ->
                    if key = "destination" then []
                    else serialized_revisions value)
                  fields
            | `Variant (_, value) ->
                List.concat_map serialized_revisions (Option.to_list value)
            | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ -> []
          in
          let validate state =
            let expected =
              List.sort_uniq B.Commit.compare
                (serialized_revisions (B.yojson_of_t state))
            in
            assert (B.required_revisions state = expected);
            match B.decode (B.yojson_of_t state) with
            | Error reason -> failwith reason
            | Ok restored -> assert (B.required_revisions restored = expected)
          in
          let state = ref B.empty in
          List.iter validate
            [ !state; prepared (); publishing (); settled (); recovery () ];
          List.iteri
            (fun index action ->
              let revision =
                match B.Commit.make (Printf.sprintf "%040x" (index + 7)) with
                | Some revision -> revision
                | None -> assert false
              in
              let next =
                match action with
                | 0 -> B.step !state (B.Materialized (B.New_branch revision))
                | 1 ->
                    B.step !state (B.Materialized (B.Adopted_branch revision))
                | 2 -> B.step !state (B.Request intent)
                | 3 ->
                    B.step !state
                      (B.Request
                         { intent with purpose = B.Publish_revision revision })
                | 4 ->
                    B.step !state
                      (B.Request
                         {
                           intent with
                           purpose =
                             B.Integrate_revision
                               { contributor = "child"; revision };
                         })
                | 5 -> B.step !state B.Recover
                | 6 -> B.step !state B.Resume
                | 7 -> B.step !state (B.Tick (Float.of_int (index * 1000)))
                | 8 -> reply !state (B.Recovery_required "fixture")
                | 9 ->
                    reply !state
                      (B.Conflict
                         {
                           head = revision;
                           sequencer = "rebase";
                           conflicts = 1;
                         })
                | 10 ->
                    reply !state
                      (B.Merge_completion_needed
                         {
                           head = revision;
                           target;
                           parents = [ source; candidate ];
                           sequencer = "merge";
                           resumed_sequencer = "rebase";
                         })
                | 11 ->
                    reply !state
                      (B.Remote { sha = Some revision; topology = B.Diverged })
                | 12 ->
                    reply !state
                      (B.Remote_replay_selected
                         (B.Reconstructed
                            { original = revision; upstream = target }))
                | 13 ->
                    reply !state
                      (B.Retryable { reason = "transport"; retry_after = None })
                | 14 -> reply !state unverified
                | _ -> (
                    match B.pending !state with
                    | Some command -> reply !state (progress_result command)
                    | None -> B.step !state B.Reconfirm_publication)
              in
              state := fst next;
              validate !state)
            actions;
          true
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "BR dependency inventory retains either object hash width without \
         duplication"
      Gen.(pair bool (int_range 1 20))
      (fun (wide, repeats) ->
        try
          let revision =
            match B.Commit.make (String.make (if wide then 64 else 40) 'd') with
            | Some revision -> revision
            | None -> assert false
          in
          let rec repeat state n =
            if n = 0 then state
            else
              repeat
                (fst (B.step state (B.Materialized (B.New_branch revision))))
                (n - 1)
          in
          B.required_revisions B.empty = []
          && B.required_revisions (repeat B.empty repeats) = [ revision ]
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:"BR poll confirmation cannot rebind a captured destination"
      Gen.string (fun suffix ->
        try
          let operation state =
            match B.operation state with
            | Some operation -> operation
            | None -> failwith "missing fixture operation"
          in
          let unobserved = fst (request ()) in
          let prepared = prepared () in
          let restored =
            match B.decode (B.yojson_of_t prepared) with
            | Ok restored -> restored
            | Error reason -> failwith reason
          in
          let other = B.Remote_id.of_destination ("different/" ^ suffix) in
          let stopped =
            exhaust_diagnosis
              (fst (reply restored (B.Needs_diagnosis "test resume")))
          in
          let resumed = fst (B.step stopped B.Resume) in
          B.check_observation_destination (operation unobserved)
            ~observed:observation.destination
          = Error "publication_destination_unconfirmed"
          && B.check_observation_destination (operation restored)
               ~observed:observation.destination
             = Ok ()
          && B.check_observation_destination (operation restored)
               ~observed:other
             = Error "publication_destination_changed"
          && B.check_observation_destination (operation resumed)
               ~observed:observation.destination
             = Error "publication_destination_unconfirmed"
          && B.check_observation_destination (operation resumed) ~observed:other
             = Error "publication_destination_unconfirmed"
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR unrelated topology reaches agent recovery without charging \
         infrastructure" ~count:100 Gen.bool (fun unrelated ->
        let checkout =
          Git_observation.of_porcelain ~branch:(Some "patch")
            ~head:(B.Commit.to_string source)
            ~sequencer:Git_observation.None_active ""
        in
        let result =
          B.stopped_integration_result checkout
            ~detail:
              (if unrelated then "fatal: refusing to merge unrelated histories"
               else "fatal: Unable to create index.lock: File exists")
        in
        if unrelated then result = B.Recovery_required "unrelated_histories"
        else
          result
          = B.Attempt_failed "fatal: Unable to create index.lock: File exists");
    QCheck2.Test.make ~name:"BR reconstruction decoding is total" ~count:500
      Gen.(pair string string)
      (fun (tree, history) ->
        try
          ignore
            (B.reconstructed_boundary ~source
               ~recorded:[ (target, tree) ]
               ~history);
          true
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR reconstruction prefers recorded order and newest exact tree"
      ~count:200 Gen.bool (fun same_tree ->
        let original = sha 'd' in
        let history =
          Printf.sprintf "%s\000%s\000%s\n%s\000\000%s\n"
            (B.Commit.to_string source)
            (B.Commit.to_string target)
            (B.Commit.to_string (if same_tree then candidate else source))
            (B.Commit.to_string target)
            (B.Commit.to_string candidate)
        in
        B.reconstructed_boundary ~source
          ~recorded:
            [
              (original, B.Commit.to_string candidate);
              (candidate, B.Commit.to_string source);
            ]
          ~history
        = Ok
            (Some
               (B.Reconstructed
                  {
                    original;
                    upstream = (if same_tree then source else target);
                  })));
    QCheck2.Test.make
      ~name:"BR missing reconstruction topology stays a probe error" ~count:200
      Gen.bool (fun truncated ->
        let history =
          Printf.sprintf "%s\000%s\000%s%s"
            (B.Commit.to_string source)
            (B.Commit.to_string target)
            (B.Commit.to_string candidate)
            (if truncated then "" else "\n")
        in
        match
          B.reconstructed_boundary ~source
            ~recorded:[ (target, B.Commit.to_string candidate) ]
            ~history
        with
        | Error _ -> true
        | Ok _ -> false);
    QCheck2.Test.make
      ~name:"BR captured subject scope survives queued intent and restart"
      ~count:200 Gen.string (fun project ->
        try
          let ancestors = [ Types.Patch_id.of_string "1" ] in
          let original =
            {
              intent with
              purpose =
                B.Reconcile_scoped { request = "original"; project; ancestors };
            }
          in
          let state, _ = B.step B.empty (B.Request original) in
          let state, _ = B.step state (B.Request intent) in
          match B.decode (B.yojson_of_t state) with
          | Error _ -> false
          | Ok state -> (
              match B.operation state with
              | None -> false
              | Some operation ->
                  (B.subject_scope operation.intent.purpose
                  = if project = "" then None else Some (project, ancestors))
                  && B.subject_scope intent.purpose = None)
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR subject inference is total on arbitrary observations" ~count:500
      Gen.(pair string string)
      (fun (project, text) ->
        try
          ignore
            (B.subject_boundary ~source ~project
               ~ancestors:[ Types.Patch_id.of_string "1" ]
               text);
          true
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR subject inference stays within captured dependency scope"
      ~count:500
      Gen.(pair (int_range 0 100000) bool)
      (fun (number, matches) ->
        try
          let project = "project-" ^ string_of_int number in
          let text =
            Printf.sprintf
              "%s\000%s\000own patch\n%s\000%s\000[%s] Patch 1: dep\n"
              (B.Commit.to_string source)
              (B.Commit.to_string target)
              (B.Commit.to_string target)
              (B.Commit.to_string candidate)
              project
          in
          let ancestors =
            [ Types.Patch_id.of_string (if matches then "1" else "11") ]
          in
          B.subject_boundary ~source ~project ~ancestors text
          = Ok (if matches then Some target else None)
          && B.subject_boundary ~source ~project:(project ^ "-other") ~ancestors
               text
             = Ok None
          && B.subject_boundary ~source ~project ~ancestors:[] text = Ok None
        with _ -> false);
    QCheck2.Test.make ~name:"BR patch-equivalence decoding is total" ~count:500
      Gen.string (fun text ->
        try
          ignore (B.patch_equivalent_boundary ~source text);
          true
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR patch equivalence excludes only an equivalent prefix" ~count:500
      Gen.(list_size (int_range 1 40) bool)
      (fun equivalent ->
        try
          let source, text, revision = equivalence_fixture equivalent in
          let rec prefix count = function
            | true :: rest -> prefix (count + 1) rest
            | false :: _ | [] -> count
          in
          let count = prefix 0 equivalent in
          let expected =
            if count = 0 then None else B.Commit.make (revision count)
          in
          B.patch_equivalent_boundary ~source text = Ok expected
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR truncated equivalence observations fail as probes" ~count:200
      Gen.(list_size (int_range 1 20) bool)
      (fun equivalent ->
        try
          let source, text, _ = equivalence_fixture equivalent in
          match
            B.patch_equivalent_boundary ~source
              (String.sub text 0 (String.length text - 1))
          with
          | Error _ -> true
          | Ok _ -> false
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR disconnected and merge history cannot infer ownership"
      ~count:200 Gen.bool (fun merge ->
        let record =
          Printf.sprintf "=\000%s\000%s%s\n"
            (B.Commit.to_string target)
            (B.Commit.to_string candidate)
            (if merge then " " ^ B.Commit.to_string source else "")
        in
        B.patch_equivalent_boundary ~source record = Ok None);
    QCheck2.Test.make
      ~name:"BR repeated publication confirmations preserve receipt history"
      ~count:200
      Gen.(pair (int_range 1 30) bool)
      (fun (count, restart) ->
        let initial = settled () in
        let receipts = B.publications initial in
        let rec confirm state remaining =
          if remaining = 0 then
            B.publications state = receipts
            && B.publication_status state = `Published
          else
            let state =
              if restart then
                match B.decode (B.yojson_of_t state) with
                | Ok state -> Some state
                | Error _ -> None
              else Some state
            in
            match state with
            | None -> false
            | Some state ->
                let state, _ = B.step state B.Reconfirm_publication in
                let state, _ =
                  reply state
                    (B.Remote { sha = Some candidate; topology = Equal })
                in
                B.publications state = receipts && confirm state (remaining - 1)
        in
        confirm initial count);
    QCheck2.Test.make
      ~name:"BR a retained candidate is not publication confirmation" ~count:100
      Gen.bool (fun reconfirm ->
        let state =
          if reconfirm then fst (B.step (settled ()) B.Reconfirm_publication)
          else publishing ()
        in
        B.publication_status state = `Pending
        && B.publication_status (settled ()) = `Published
        && B.publication_status B.empty = `Pending);
    QCheck2.Test.make
      ~name:
        "BR adopted publication with no commits ahead settles without a \
         candidate"
      ~count:100
      Gen.(pair bool bool)
      (fun (adopted, published) ->
        let receipt =
          if adopted then B.Adopted_branch source else B.New_branch source
        in
        let state, _ = B.step B.empty (B.Materialized receipt) in
        let state, _ =
          B.step state
            (B.Request { intent with purpose = Publish_revision source })
        in
        let state, effects =
          reply state
            (B.Observed
               {
                 observation with
                 base_contains_source = true;
                 remote = (if published then Some source else None);
               })
        in
        B.phase state = Some B.Settled
        && (match B.operation state with
          | Some op -> op.candidate = None
          | None -> false)
        && List.for_all
             (function
               | B.Completed _ -> true
               | B.Execute _ | B.Repair _ | B.Start_repair _ -> false)
             effects);
    QCheck2.Test.make
      ~name:
        "BR explicit publication reconfirmation restores a deleted remote with \
         an absent lease" ~count:100 Gen.bool (fun restart ->
        let state = settled () in
        let state =
          if restart then
            match B.decode (B.yojson_of_t state) with
            | Ok state -> state
            | Error _ -> assert false
          else state
        in
        let state, _ = B.step state B.Reconfirm_publication in
        let duplicate, effects = B.step state B.Reconfirm_publication in
        let restored, _ =
          reply state (B.Remote { sha = None; topology = Unproven })
        in
        B.equal state duplicate && effects = []
        &&
        match B.pending restored with
        | Some command ->
            B.equal_command_kind command.kind
              (B.Publish { candidate; expected = None })
        | None -> false);
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
        let prompted = is_diagnosis state in
        let state = exhaust_diagnosis state in
        let stopped =
          prompted
          && B.phase state
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
      ~name:
        "BR repeated continuation timeout escalates across restart and \
         inspection"
      ~count:100 Gen.bool (fun restart ->
        let active =
          { observation with sequencer = Some "merge"; conflicts = 0 }
        in
        let state, _ = B.step (prepared ()) B.Recover in
        let state, _ =
          reply state
            (B.Inspected_active { observation = active; ready = true })
        in
        let timeout =
          B.Attempt_failed "Git command outcome uncertain after timeout"
        in
        let state, _ = reply state timeout in
        let state =
          if restart then
            match B.decode (B.yojson_of_t state) with
            | Ok state -> state
            | Error _ -> assert false
          else state
        in
        let state, _ = B.step state (B.Tick 1000.) in
        let state, _ =
          reply state
            (B.Inspected_active { observation = active; ready = true })
        in
        let state, _ = reply state timeout in
        Option.map (fun c -> c.B.kind) (B.pending state)
        = Some B.Verify_recovery
        &&
        let state, effects =
          reply state
            (B.Recovery_verified
               {
                 observation = active;
                 source_preserved = false;
                 remote_preserved = false;
               })
        in
        match B.operation state with
        | Some { repair = Some { mode = B.History_recovery _; _ }; failures; _ }
          ->
            failures = 2 && effects <> []
        | None
        | Some { repair = None; _ }
        | Some
            { repair = Some { mode = B.Content_repair | B.Diagnosis _; _ }; _ }
          ->
            false);
    QCheck2.Test.make
      ~name:
        "BR finishing interrupted edits unlocks integration only after \
         independent proof" ~count:100 Gen.bool (fun preserved ->
        let state, _ = request () in
        let state, _ =
          reply state (B.Observed { observation with clean = false })
        in
        let state, _ =
          reply state
            (B.Recovery_verified
               {
                 observation = { observation with clean = false };
                 source_preserved = false;
                 remote_preserved = false;
               })
        in
        let state, _ = B.step state B.Recover in
        let state, _ =
          reply state
            (B.Recovery_verified
               {
                 observation =
                   { observation with source = candidate; head = candidate };
                 source_preserved = preserved;
                 remote_preserved = preserved;
               })
        in
        if preserved then
          Option.map (fun c -> c.B.kind) (B.pending state)
          = Some
              (B.Integrate
                 {
                   source = candidate;
                   target;
                   boundary = observation.boundary;
                   policy = intent.policy;
                 })
        else
          match B.operation state with
          | Some { repair = Some { mode = B.History_recovery _; _ }; _ } ->
              B.pending state = None
          | None
          | Some { repair = None; _ }
          | Some
              {
                repair = Some { mode = B.Content_repair | B.Diagnosis _; _ };
                _;
              } ->
              false);
    QCheck2.Test.make
      ~name:
        "BR old intervention diagnostics are re-evaluated without granting \
         destination authority"
      ~count:200
      Gen.(triple string bool bool)
      (fun (diagnostic, changed_destination, legacy_fields) ->
        try
          let stopped, _ =
            reply (prepared ()) (B.Needs_diagnosis ("old/" ^ diagnostic))
          in
          let stopped = exhaust_diagnosis stopped in
          let rec old = function
            | `Assoc fields ->
                `Assoc
                  (List.filter_map
                     (fun (key, value) ->
                       if legacy_fields && key = "deterministic_failures" then
                         None
                       else if key = "repair" then Some (key, `Null)
                       else Some (key, old value))
                     fields)
            | `List values -> `List (List.map old values)
            | value -> value
          in
          let state =
            match B.decode (old (B.yojson_of_t stopped)) with
            | Ok state -> state
            | Error message -> failwith message
          in
          assert (
            Option.map (fun c -> c.B.kind) (B.pending state) = Some B.Inspect);
          assert (
            match B.decode (B.yojson_of_t state) with
            | Ok restored -> B.equal state restored
            | Error _ -> false);
          let state, _ =
            reply state
              (B.Inspected
                 {
                   observation with
                   clean = false;
                   destination =
                     (if changed_destination then
                        B.Remote_id.of_destination "changed"
                      else observation.destination);
                 })
          in
          if changed_destination then is_diagnosis state
          else
            Option.map (fun c -> c.B.kind) (B.pending state)
            = Some B.Verify_recovery
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR upgrade retains an exhausted full-recovery budget" ~count:100
      Gen.unit (fun () ->
        try
          let attempt state =
            match reply state unverified with
            | state, [ B.Repair token ] ->
                let state, _ = B.step state (B.Repair_started token) in
                fst (B.step state (B.Repair_completed { token; at = 100. }))
            | ( _,
                ( []
                | B.Execute _ :: _
                | B.Start_repair _ :: _
                | B.Completed _ :: _
                | B.Repair _ :: _ :: _ ) ) ->
                failwith "missing recovery offer"
          in
          let stopped, _ = reply (attempt (attempt (recovery ()))) unverified in
          let stopped = exhaust_diagnosis stopped in
          let rec old = function
            | `Assoc fields ->
                `Assoc
                  (List.filter_map
                     (fun (key, value) ->
                       if key = "deterministic_failures" then None
                       else Some (key, old value))
                     fields)
            | `List values -> `List (List.map old values)
            | value -> value
          in
          match B.decode (old (B.yojson_of_t stopped)) with
          | Error _ -> false
          | Ok restored ->
              B.equal stopped restored
              && B.phase restored
                 = Some (B.Intervention "recovery_preservation_unproven")
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR history recovery offers an owned agent turn for dirty work"
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
            (match B.operation state with
              | Some { phase = B.Repairing r; command_sequence; id; _ } -> (
                  B.phase state = Some (B.Repairing r)
                  &&
                  match effects with
                  | [ B.Repair token ] ->
                      token.operation = id && token.command = command_sequence
                  | []
                  | B.Execute _ :: _
                  | B.Completed _ :: _
                  | B.Start_repair _ :: _
                  | B.Repair _ :: _ ->
                      false)
              | None
              | Some
                  {
                    phase =
                      ( B.Preparing | Integrating | Publishing | Confirming
                      | Waiting _ | Recovering | Settled | Intervention _ );
                    _;
                  } ->
                  false)
            && Option.map (fun c -> c.B.kind) (B.pending stable)
               = Some B.Verify_recovery
            && again <> []);
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
          reply original (B.Needs_diagnosis "operator_action_required")
        in
        let stopped = exhaust_diagnosis stopped in
        let polled, effects = B.step stopped B.Recover in
        let resumed, commands = B.step stopped B.Resume in
        let duplicate, duplicate_effects = B.step resumed B.Resume in
        match (B.operation stopped, B.operation resumed, B.pending resumed) with
        | Some before, Some after, Some command ->
            B.equal polled stopped && effects = [] && before.id = after.id
            && before.source = after.source
            && before.target = after.target
            && before.candidate = after.candidate
            && command.kind = B.Inspect
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
      ~name:
        "BR strongest requested or adopted policy governs authority through \
         restart"
      ~count:100
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
                  B.equal_policy (B.execution_policy op)
                    (policy (requested_merge || adopted_merge))
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
      ~name:
        "BR unfamiliar sequencers retain observations and offer owned recovery"
      ~count:100 Gen.bool (fun cherry_pick ->
        let sequencer =
          if cherry_pick then
            Git_observation.Cherry_pick { target = B.Commit.to_string target }
          else
            Git_observation.Rebase
              {
                target = B.Commit.to_string target;
                original = B.Commit.to_string source;
                step = "1";
                head_ref = "refs/heads/foreign";
                merge_heads = [];
              }
        in
        let checkout =
          Git_observation.of_porcelain ~branch:None
            ~head:(B.Commit.to_string source)
            ~sequencer "UU file\000"
        in
        let observation =
          {
            observation with
            clean = false;
            conflicts = 1;
            sequencer = Some (Git_observation.progress_key checkout);
          }
        in
        let state, _ = request () in
        let op = Option.get (B.operation state) in
        let result =
          B.inspection_result op ~branch:"patch" observation checkout
        in
        let state, _ = reply state result in
        Option.map (fun c -> c.B.kind) (B.pending state)
        = Some B.Verify_recovery
        &&
        let state, effects =
          reply state
            (B.Recovery_verified
               {
                 observation;
                 source_preserved = false;
                 remote_preserved = false;
               })
        in
        effects <> []
        && B.pending state = None
        && B.is_pending state
        &&
        match B.decode (B.yojson_of_t state) with
        | Ok decoded -> B.equal state decoded
        | Error _ -> false);
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
                   base_contains_source = false;
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
            (if permanent then B.Needs_diagnosis "policy"
             else B.Retryable { reason = "transport"; retry_after = None })
        in
        let state = if permanent then exhaust_diagnosis state else state in
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
                   base_contains_source = false;
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
      ~name:"BR fresh remote work cannot charge a proved recovery as failure"
      ~count:500
      Gen.(pair bool (pair bool (triple bool bool bool)))
      (fun ( retained_before,
             (fresh_remote, (source_preserved, clean, target_included)) )
         ->
        try
          let state = recovery () in
          let state =
            if not retained_before then state
            else
              match reply state unverified with
              | state, [ B.Repair token ] ->
                  let state, _ = B.step state (B.Repair_started token) in
                  fst (B.step state (B.Repair_completed { token; at = 100. }))
              | ( _,
                  ( []
                  | B.Execute _ :: _
                  | B.Start_repair _ :: _
                  | B.Completed _ :: _
                  | B.Repair _ :: _ :: _ ) ) ->
                  failwith "missing recovery offer"
          in
          let remote = if fresh_remote then sha 'd' else source in
          let state, _ =
            reply state
              (B.Recovery_verified
                 {
                   observation =
                     {
                       observation with
                       source = candidate;
                       head = candidate;
                       remote = Some remote;
                       clean;
                       target_included;
                     };
                   source_preserved;
                   remote_preserved = false;
                 })
          in
          let reconciles =
            match B.pending state with
            | Some { kind = B.Plan_remote_replay request; _ } ->
                request.B.incoming = remote && request.B.preserved = candidate
            | None
            | Some
                {
                  kind =
                    ( B.Observe | B.Pin _ | B.Integrate _ | B.Inspect
                    | B.Verify_recovery | B.Continue _ | B.Confirm _
                    | B.Publish _ | B.Checkout_remote _ | B.Commit_merge _ );
                  _;
                } ->
                false
          in
          let publishes =
            Option.fold ~none:false
              ~some:(fun command ->
                B.equal_command_kind command.B.kind
                  (B.Publish { candidate; expected = Some remote }))
              (B.pending state)
          in
          reconciles
          = (retained_before && fresh_remote && source_preserved && clean
           && target_included)
          && (not publishes)
          && (match B.operation state with
            | None -> false
            | Some op ->
                List.mem source op.B.recovery_revisions
                && List.mem remote op.B.recovery_revisions)
          &&
          match B.decode (B.yojson_of_t state) with
          | Ok restored -> B.equal restored state
          | Error _ -> false
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR accepted recovery timeouts are bounded after verification across \
         restart" ~count:100 Gen.bool (fun final_proof ->
        try
          let claim state =
            match reply state unverified with
            | state, [ B.Repair token ] ->
                (fst (B.step state (B.Repair_started token)), token)
            | ( _,
                ( []
                | B.Execute _ :: _
                | B.Start_repair _ :: _
                | B.Completed _ :: _
                | B.Repair _ :: _ :: _ ) ) ->
                failwith "missing recovery offer"
          in
          let fail state =
            let state, token = claim state in
            let turn = Option.get (B.repair_turn state ~branch:"patch" token) in
            let event =
              B.repair_result ~turn ~turn_accepted:true ~at:100.
                ~before_head:(Some candidate) ~after_head:(Some candidate)
                ~timed_out:true ~final_result:false
                ~detail:"accepted turn timed out"
            in
            assert (
              event
              = B.Repair_failed
                  { token; at = 100.; reason = "accepted turn timed out" });
            let state, _ = B.step state event in
            let state =
              match B.decode (B.yojson_of_t state) with
              | Ok state -> state
              | Error message -> failwith message
            in
            fst (B.step state (B.Tick 10000.))
          in
          let state = fail (fail (recovery ())) in
          let state, _ =
            reply state
              (if final_proof then
                 B.Recovery_verified
                   {
                     observation =
                       {
                         observation with
                         source = candidate;
                         head = candidate;
                         target_included = true;
                       };
                     source_preserved = true;
                     remote_preserved = true;
                   }
               else unverified)
          in
          if final_proof then
            Option.map (fun c -> c.B.kind) (B.pending state)
            = Some (B.Publish { candidate; expected = observation.remote })
          else
            B.phase state
            = Some (B.Intervention "recovery_preservation_unproven")
        with _ -> false);
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
          assert (
            Base.String.is_substring turn.prompt
              ~substring:
                (Printf.sprintf "Repair turn: %d/%d" token.operation
                   token.command));
          assert (
            Base.String.is_substring turn.prompt
              ~substring:
                ("Replay boundary evidence: recorded "
               ^ B.Commit.to_string source));
          let state, _ =
            B.step state
              (B.repair_result ~turn_accepted:false ~turn ~at:100.
                 ~before_head:(Some candidate) ~after_head:(Some target)
                 ~timed_out:false ~final_result:true ~detail:"")
          in
          let state, token = claim state in
          let state, _ =
            B.step state (B.Repair_completed { token; at = 100. })
          in
          let state, _ = reply state unverified in
          let restored =
            match B.decode (B.yojson_of_t state) with
            | Ok restored -> restored
            | Error reason -> failwith reason
          in
          let details = B.diagnostics restored in
          B.equal state restored
          && B.phase restored
             = Some (B.Intervention "recovery_preservation_unproven")
          && List.assoc_opt "Repair no-progress count" details = Some "2"
          && List.assoc_opt "Repair completion" details = Some "completed"
          && List.assoc_opt "Last repair turn" details
             = Some (Printf.sprintf "%d/%d" token.operation token.command)
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR first accepted repair failure waits for verification without \
         terminal denial"
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
              B.Repair_failed
                { token; at = 1.; reason = "accepted repair failed" }
            in
            let stopped, effects = B.step claimed denial in
            let duplicate, repeated = B.step stopped denial in
            let unrelated = publishing () in
            let unchanged, stale = B.step unrelated denial in
            B.phase stopped
            = Some (B.Waiting { until = 6.; reason = "accepted repair failed" })
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
    QCheck2.Test.make
      ~name:"BR repair identity survives inspection, interruption and restart"
      ~count:200 Gen.bool (fun interrupted ->
        try
          let state, effects =
            reply (prepared ())
              (B.Conflict { head = source; sequencer = "step"; conflicts = 2 })
          in
          let token =
            match effects with
            | [ B.Repair token ] -> token
            | []
            | B.Execute _ :: _
            | B.Start_repair _ :: _
            | B.Completed _ :: _
            | B.Repair _ :: _ :: _ ->
                failwith "missing repair claim"
          in
          let state, _ = B.step state (B.Repair_started token) in
          let state, _ =
            B.step state
              (if interrupted then
                 B.Repair_interrupted
                   { token; at = 100.; reason = "transport unavailable" }
               else B.Repair_completed { token; at = 100. })
          in
          let state =
            match B.decode (B.yojson_of_t state) with
            | Ok state -> state
            | Error reason -> failwith reason
          in
          let expected = Printf.sprintf "%d/%d" token.operation token.command in
          let details = B.diagnostics state in
          List.assoc_opt "Last repair turn" details = Some expected
          && List.assoc_opt "Repair no-progress count" details = Some "0"
          &&
          match B.operation state with
          | Some op -> (
              match op.repair with
              | Some r -> r.last_turn = Some token
              | None -> false)
          | None -> false
        with _ -> false);
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
                B.repair_result ~turn_accepted:false ~turn ~at:100. ~before_head
                  ~after_head ~timed_out:timeout ~final_result:true ~detail:""
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
                 {
                   observation with
                   remote = Some target;
                   topology = Diverged;
                   target = (if preserve then target else source);
                 })
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
        same = (project = other_project && branch = other_branch)
        && String.starts_with
             ~prefix:(B.recovery_project_prefix ~project)
             (B.recovery_prefix ~project ~branch)
        && String.starts_with
             ~prefix:(B.recovery_project_prefix ~project)
             (B.recovery_prefix ~project:other_project ~branch:other_branch)
           = (project = other_project));
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
      ~name:
        "BR changed session publication retains its requested revision through \
         recovery" ~count:100 Gen.bool (fun dirty ->
        let state, _ =
          B.step B.empty
            (B.Request { intent with purpose = Publish_revision candidate })
        in
        let state, effects =
          reply state (B.Observed { observation with clean = not dirty })
        in
        Option.map (fun c -> c.B.kind) (B.pending state)
        = Some B.Verify_recovery
        && Option.bind (B.operation state) (fun op -> op.B.source)
           = Some candidate
        && effects <> []
        &&
        let state, _ =
          reply state
            (B.Recovery_verified
               {
                 observation = { observation with clean = not dirty };
                 source_preserved = false;
                 remote_preserved = false;
               })
        in
        B.pending state = None && B.is_pending state);
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
        | Some op -> (
            op.candidate = Some candidate
            && op.deterministic_failures = 0
            &&
            if count = 1 then op.repair = None
            else
              match op.repair with
              | Some { mode = Diagnosis _; attempts_without_progress = 0; _ } ->
                  true
              | None
              | Some { mode = Content_repair | History_recovery _; _ }
              | Some { mode = Diagnosis _; _ } ->
                  false)
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
        let deadline = 100. +. Float.of_int (max 5 delay) in
        let before, effects = B.step state (B.Tick (deadline -. 0.5)) in
        let after, _ = B.step before (B.Tick deadline) in
        effects = [] && B.equal before state
        && (not (B.can_execute_git ~at:(deadline -. 0.5) before))
        && B.wake_event ~at:(deadline -. 0.5) before = None
        && B.phase after = Some B.Recovering);
  ]

let tracking_tests =
  [
    QCheck2.Test.make
      ~name:
        "BR initial tracking authority requires matching confirmed candidate"
      ~count:200
      Gen.(triple bool bool bool)
      (fun (initial, matches, captured) ->
        try
          let state = fst (request ()) in
          let state =
            if not captured then state
            else
              let state =
                fst
                  (reply state
                     (B.Observed
                        {
                          observation with
                          remote = (if initial then None else Some source);
                        }))
              in
              let state = fst (reply state B.Pinned) in
              fst (reply state (B.Integrated candidate))
          in
          let op = Option.get (B.operation state) in
          B.initial_publication_candidate op
            ~remote:(if matches then Some candidate else None)
          = if initial && matches && captured then Some candidate else None
        with _ -> false);
    QCheck2.Test.make ~name:"BR initial tracking decisions are total" ~count:300
      Gen.(triple string (list string) (list string))
      (fun (branch, remotes, merges) ->
        try
          ignore
            (B.initial_tracking_edits ~branch ~base:"main"
               ~fetch_destination:"origin" ~push_destination:"origin" ~remotes
               ~merges);
          true
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR initial tracking partial writes converge and then settle"
      ~count:200
      Gen.(pair bool bool)
      (fun (remote_written, merge_written) ->
        let edits remotes merges =
          B.initial_tracking_edits ~branch:"patch" ~base:"main"
            ~fetch_destination:"url" ~push_destination:"url" ~remotes ~merges
        in
        let remote = if remote_written then [ "origin" ] else [] in
        let merge =
          if merge_written then [ "refs/heads/patch" ]
          else [ "refs/heads/main" ]
        in
        let expected =
          (if remote_written then [] else [ ("branch.patch.remote", "origin") ])
          @
          if merge_written then []
          else [ ("branch.patch.merge", "refs/heads/patch") ]
        in
        edits remote merge = Some expected
        && edits [ "origin" ] [ "refs/heads/patch" ] = Some []);
    QCheck2.Test.make
      ~name:
        "BR initial tracking preserves unrelated configuration and destinations"
      ~count:200
      Gen.(int_range 0 3)
      (fun case ->
        B.initial_tracking_edits ~branch:"patch" ~base:"main"
          ~fetch_destination:"url"
          ~push_destination:(if case = 0 then "elsewhere" else "url")
          ~remotes:
            (if case = 1 then [ "other" ]
             else if case = 2 then [ "origin"; "origin" ]
             else [ "origin" ])
          ~merges:(if case = 3 then [ "refs/heads/other" ] else [])
        = None);
  ]

let publication_observation_tests =
  let receipt state =
    match B.publication_observation_target state with
    | Some receipt -> receipt
    | None -> failwith "missing publication receipt"
  in
  let observe state receipt head remote =
    B.step state
      (B.Publication_observed { publication = receipt; head; remote })
  in
  let restore state =
    match B.decode (B.yojson_of_t state) with
    | Ok state -> state
    | Error reason -> failwith reason
  in
  [
    QCheck2.Test.make ~count:100
      ~name:
        "BR replacing an observed candidate invalidates its pre-confirmation \
         acknowledgement" Gen.bool (fun preserve ->
        try
          let policy = if preserve then B.Preserve_ancestry else B.Rewrite in
          let initial =
            fst (B.step B.empty (B.Request { intent with policy }))
          in
          let initial = fst (reply initial (B.Observed observation)) in
          let initial = fst (reply initial B.Pinned) in
          let initial = fst (reply initial (B.Integrated candidate)) in
          let old = receipt initial in
          let initial = fst (observe initial old candidate None) in
          let reply_checked state result = restore (fst (reply state result)) in
          let state = reply_checked initial B.Published in
          let state =
            reply_checked state
              (B.Remote { sha = Some (sha 'd'); topology = B.Diverged })
          in
          let state =
            if preserve then state
            else
              reply_checked state (B.Remote_replay_selected (B.Recorded source))
              |> fun state -> reply_checked state B.Remote_checked_out
          in
          let replacement = sha 'e' in
          let state = reply_checked state (B.Integrated replacement) in
          let rejected, effects = observe state old candidate None in
          let newer = receipt rejected in
          B.equal rejected state && effects = []
          && newer.operation_id <> old.operation_id
          && newer.revision = replacement
          && B.publication_observation_pending rejected <> None
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "BR publication observation wait survives restart and repeated recovery"
      Gen.(list_size (int_range 0 30) bool)
      (fun operations ->
        try
          let initial = settled () in
          let final =
            List.fold_left
              (fun state recover ->
                let state = restore state in
                fst
                  (B.step state
                     (if recover then B.Recover else B.Request intent)))
              initial operations
          in
          B.equal initial final && B.unobserved_publication final <> None
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "BR only expected or directly confirmed head acknowledges publication"
      Gen.(pair bool bool)
      (fun (expected, confirmed) ->
        try
          let initial = settled () in
          let head = if expected then candidate else source in
          let publication = receipt initial in
          let remote = if confirmed then Some head else None in
          let allowed =
            B.can_observe_publication initial ~publication ~head ~remote
          in
          let recorded =
            B.publication_identity (List.hd (B.publications initial))
          in
          let next, effects = observe initial publication head remote in
          B.equal_publication_identity publication recorded
          && allowed = (expected || confirmed)
          && effects = []
          && B.unobserved_publication (restore next)
             = None = (expected || confirmed)
          && (expected || confirmed || B.equal initial next)
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR publication acknowledgement is idempotent across restart"
      Gen.(list_size (int_range 0 30) bool)
      (fun operations ->
        try
          let initial = settled () in
          let receipt = receipt initial in
          let acknowledged = fst (observe initial receipt candidate None) in
          let final =
            List.fold_left
              (fun state acknowledge ->
                let state = restore state in
                if acknowledge then fst (observe state receipt candidate None)
                else fst (B.step state B.Recover))
              acknowledged operations
          in
          B.equal acknowledged final && B.unobserved_publication final = None
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "BR candidate observation commutes with Git publication confirmation"
      Gen.(int_range 0 2)
      (fun position ->
        try
          let before = publishing () in
          let publication = receipt before in
          let acknowledge state =
            fst (observe state publication candidate None)
          in
          let state = if position = 0 then acknowledge before else before in
          let state = fst (reply (restore state) B.Published) in
          let state = if position = 1 then acknowledge state else state in
          let state =
            fst
              (reply (restore state)
                 (B.Remote { sha = Some candidate; topology = B.Equal }))
          in
          let state = if position = 2 then acknowledge state else state in
          B.equal state (acknowledge (settled ()))
          && B.publication_observation_pending (restore state) = None
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR current remote cannot pre-acknowledge an unpublished candidate"
      Gen.bool (fun pushed ->
        try
          let state = publishing () in
          let state = if pushed then fst (reply state B.Published) else state in
          let rejected, effects =
            observe state (receipt state) source (Some source)
          in
          B.equal state rejected && effects = []
          && B.publication_observation_pending rejected <> None
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR delayed old receipt cannot acknowledge a newer publication"
      Gen.bool (fun confirmed ->
        try
          let initial = settled () in
          let old = receipt initial in
          let next_intent =
            { intent with purpose = B.Reconcile_request "new-publication" }
          in
          let next = fst (B.step initial (B.Request next_intent)) in
          let next = fst (reply next (B.Observed observation)) in
          let next = fst (reply next B.Pinned) in
          let next = fst (reply next (B.Integrated target)) in
          let next = fst (reply next B.Published) in
          let next =
            fst
              (reply next (B.Remote { sha = Some target; topology = B.Equal }))
          in
          let rejected, effects =
            observe next old target (if confirmed then Some target else None)
          in
          B.equal next rejected && effects = []
          && (receipt rejected).revision = target
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR old checkpoints default to an unobserved publication" Gen.bool
      (fun acknowledged ->
        try
          let initial = settled () in
          let initial =
            if acknowledged then
              fst (observe initial (receipt initial) candidate None)
            else initial
          in
          let json =
            match B.yojson_of_t initial with
            | `Assoc fields ->
                `Assoc (List.remove_assoc "observed_publication" fields)
            | _ -> failwith "checkpoint not an object"
          in
          match B.decode json with
          | Ok restored -> B.unobserved_publication restored <> None
          | Error _ -> false
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR decoder rejects acknowledgement without a publication receipt"
      Gen.bool (fun confirmed ->
        try
          let initial = settled () in
          let acknowledged =
            fst
              (observe initial (receipt initial) candidate
                 (if confirmed then Some candidate else None))
          in
          let json =
            match B.yojson_of_t acknowledged with
            | `Assoc fields ->
                `Assoc
                  (("publications", `List [])
                  :: List.remove_assoc "publications" fields)
            | _ -> failwith "checkpoint not an object"
          in
          match B.decode json with Error _ -> true | Ok _ -> false
        with _ -> false);
  ]

let remote_attempt_fixture policy =
  let state, _ =
    B.step B.empty
      (B.Request { intent with policy; purpose = B.Publish_revision source })
  in
  let state, _ =
    reply state (B.Observed { observation with remote = Some target })
  in
  let state, _ = reply state B.Pinned in
  fst (reply state (B.Recovery_required "remote_work_not_incorporated"))

let map_active f = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (key, value) ->
             (key, if key = "active" then f value else value))
           fields)
  | _ -> failwith "checkpoint is not an object"

let map_field name f = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (key, value) -> (key, if key = name then f value else value))
           fields)
  | _ -> failwith "checkpoint field is not an object"

let m2_upgrade_tests =
  let legacy ~phase ~failures state =
    map_active
      (function
        | `Assoc fields ->
            `Assoc
              (List.filter_map
                 (fun (name, value) ->
                   match name with
                   | "deterministic_failures" | "observation_failures" -> None
                   | "phase" -> Some (name, phase)
                   | "failures" -> Some (name, `Int failures)
                   | "pending" | "repair" -> Some (name, `Null)
                   | _ -> Some (name, value))
                 fields)
        | _ -> failwith "invalid active fixture")
      (B.yojson_of_t state)
  in
  [
    QCheck2.Test.make ~count:100
      ~name:
        "BR finished dirty work reobserves the deferred base before settlement"
      Gen.bool (fun retarget ->
        try
          let dirty = { observation with clean = false } in
          let initial, _ = reply (fst (request ())) (B.Observed dirty) in
          let initial, effects =
            reply initial
              (B.Recovery_verified
                 {
                   observation = dirty;
                   source_preserved = true;
                   remote_preserved = true;
                 })
          in
          let token =
            match effects with
            | [ B.Repair token ] -> token
            | []
            | B.Execute _ :: _
            | B.Start_repair _ :: _
            | B.Completed _ :: _
            | B.Repair _ :: _ :: _ ->
                failwith "missing dirty recovery turn"
          in
          let original = Option.get (B.operation initial) in
          let initial = fst (B.step initial (B.Repair_started token)) in
          let initial =
            if retarget then
              fst (B.step initial (B.Request { intent with base = "new-base" }))
            else initial
          in
          let initial =
            fst (B.step initial (B.Repair_completed { token; at = 100. }))
          in
          let clean =
            {
              observation with
              source = candidate;
              head = candidate;
              target_included = true;
            }
          in
          let publishing, _ =
            reply initial
              (B.Recovery_verified
                 {
                   observation = clean;
                   source_preserved = true;
                   remote_preserved = true;
                 })
          in
          assert
            (Option.get (B.operation publishing)).reobserve_after_completion;
          let publishing =
            Result.get_ok (B.decode (B.yojson_of_t publishing))
          in
          let confirming, _ = reply publishing B.Published in
          let refreshed, _ =
            reply confirming
              (B.Remote { sha = Some candidate; topology = Equal })
          in
          let successor = Option.get (B.operation refreshed) in
          assert (successor.id > original.id);
          assert (
            successor.intent.base = if retarget then "new-base" else intent.base);
          B.is_pending refreshed
          && Option.map (fun c -> c.B.kind) (B.pending refreshed)
             = Some B.Observe
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR M2 awaiting-session checkpoints retain work and deferred intent"
      Gen.bool (fun retarget ->
        try
          let initial = prepared () in
          let initial =
            if retarget then
              fst (B.step initial (B.Request { intent with base = "new-base" }))
            else initial
          in
          let json =
            legacy
              ~phase:(`List [ `String "Awaiting_session" ])
              ~failures:0 initial
          in
          let restored = Result.get_ok (B.decode json) in
          assert (B.is_pending restored);
          assert (
            Option.map (fun c -> c.B.kind) (B.pending restored) = Some B.Inspect);
          assert (
            Json.field "desired" json
            = Json.field "desired" (B.yojson_of_t restored));
          assert (
            B.equal restored (Result.get_ok (B.decode (B.yojson_of_t restored))));
          let dirty = { observation with clean = false } in
          let restored, _ = reply restored (B.Inspected dirty) in
          let restored, _ =
            reply restored
              (B.Recovery_verified
                 {
                   observation = dirty;
                   source_preserved = true;
                   remote_preserved = true;
                 })
          in
          match B.operation restored with
          | Some
              {
                repair =
                  Some
                    {
                      mode =
                        B.History_recovery { task = B.Finish_local_work; _ };
                      _;
                    };
                _;
              } ->
              true
          | None
          | Some { repair = None; _ }
          | Some
              {
                repair = Some { mode = B.Content_repair | B.Diagnosis _; _ };
                _;
              }
          | Some
              {
                repair =
                  Some
                    {
                      mode =
                        B.History_recovery
                          {
                            task = B.Reconstruct_history | B.Repair_publication;
                            _;
                          };
                      _;
                    };
                _;
              } ->
              false
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR M2 saturated retries escalate on the next failed observation"
      Gen.(pair (int_range 4 30) bool)
      (fun (failures, waiting) ->
        try
          let json =
            legacy
              ~phase:
                (if waiting then
                   `List
                     [
                       `String "Waiting";
                       `Assoc
                         [
                           ("until", `Float 100.); ("reason", `String "timeout");
                         ];
                     ]
                 else
                   `List
                     [
                       `String "Intervention";
                       `String "reconciliation_retry_exhausted: timeout";
                     ])
              ~failures (prepared ())
          in
          let restored = Result.get_ok (B.decode json) in
          let operation = Option.get (B.operation restored) in
          assert (
            operation.deterministic_failures = 2
            && operation.observation_failures = 2);
          let restored =
            if waiting then fst (B.step restored (B.Tick 1000.)) else restored
          in
          let restored, _ =
            reply restored
              (B.Retryable { reason = "timeout"; retry_after = None })
          in
          is_diagnosis restored
        with _ -> false);
  ]

let initial_remote_integration_tests =
  [
    QCheck2.Test.make ~count:100
      ~name:
        "BR unexpected inspection target cannot replace captured integration"
      Gen.(pair bool bool)
      (fun (rewrite, completed) ->
        try
          let state =
            remote_attempt_fixture
              (if rewrite then B.Rewrite else B.Preserve_ancestry)
          in
          let state =
            if rewrite then
              let state, _ =
                reply state (B.Remote_replay_selected (B.Inferred (sha 'd')))
              in
              fst (reply state B.Remote_checked_out)
            else state
          in
          let original = Option.get (B.operation state) in
          let state, _ = B.step state B.Recover in
          let state, _ =
            reply state
              (B.Inspected
                 {
                   observation with
                   source = sha 'e';
                   head = sha 'e';
                   target = sha 'f';
                   target_included = completed;
                   completed_integration = completed;
                 })
          in
          let op = Option.get (B.operation state) in
          let verified, _ =
            reply state
              (B.Recovery_verified
                 {
                   observation =
                     {
                       observation with
                       target = sha 'f';
                       target_included = true;
                     };
                   source_preserved = true;
                   remote_preserved = true;
                 })
          in
          B.is_pending verified
          && B.pending verified = None
          && (Option.get (B.operation verified)).source = original.source
          && (Option.get (B.operation verified)).target = original.target
          && B.publications verified = []
          && (Option.get (B.operation verified)).remote_integration
             = original.remote_integration
          && op.source = original.source
          && op.target = original.target
          && op.remote_integration = original.remote_integration
          && (match B.pending state with
            | Some { kind = B.Verify_recovery; _ } -> true
            | Some
                {
                  kind =
                    ( B.Observe | B.Pin _ | B.Integrate _ | B.Commit_merge _
                    | B.Plan_remote_replay _ | B.Checkout_remote _ | B.Inspect
                    | B.Continue _ | B.Publish _ | B.Confirm _ );
                  _;
                }
            | None ->
                false)
          &&
          match B.decode (B.yojson_of_t state) with
          | Ok restored -> B.equal restored state
          | Error _ -> false
        with _ -> false);
    QCheck2.Test.make ~count:400
      ~name:
        "BR every entrypoint attempts remote integration once before recovery"
      ~print:(fun ((rewrite, restart, entrypoint), lost_completion) ->
        Printf.sprintf "entrypoint=%d rewrite=%b restart=%b lost_completion=%b"
          entrypoint rewrite restart lost_completion)
      Gen.(pair (triple bool bool (int_range 0 5)) bool)
      (fun ((rewrite, restart, entrypoint), lost_completion) ->
        try
          let purpose, publication =
            match entrypoint with
            | 0 -> (B.Reconcile_base, false)
            | 1 -> (B.Reconcile_request "scheduled", false)
            | 2 ->
                ( B.Reconcile_scoped
                    {
                      request = "retarget";
                      project = "demo";
                      ancestors = [ Types.Patch_id.of_string "2" ];
                    },
                  false )
            | 3 ->
                ( B.Integrate_revision
                    { contributor = "child"; revision = target },
                  false )
            | 4 -> (B.Publish_revision source, true)
            | _ -> (B.Publish_session "session", true)
          in
          let rewrite = rewrite && entrypoint <> 3 in
          let policy = if rewrite then B.Rewrite else B.Preserve_ancestry in
          let incoming = sha 'e' and final = sha 'f' in
          let preserved = if publication then source else candidate in
          let restore state =
            if restart then
              match B.decode (B.yojson_of_t state) with
              | Ok state -> state
              | Error reason -> failwith reason
            else state
          in
          let state, _ =
            B.step B.empty (B.Request { intent with policy; purpose })
          in
          let state, _ =
            reply state (B.Observed { observation with remote = Some incoming })
          in
          let state, _ = reply state B.Pinned in
          let state =
            if publication then state
            else fst (reply state (B.Integrated candidate))
          in
          let base_receipts = B.integrations state in
          let old_command = Option.get (B.pending state) in
          let state, _ =
            reply (restore state)
              (B.Recovery_required "remote_work_not_incorporated")
          in
          let command = Option.get (B.pending state) in
          let deterministic =
            match command.kind with
            | B.Plan_remote_replay request ->
                rewrite
                && request.preserved = preserved
                && request.incoming = incoming
            | B.Integrate { source = kept; target = remote; policy; _ } ->
                (not rewrite) && kept = preserved && remote = incoming
                && policy = B.Preserve_ancestry
            | B.Observe | B.Pin _ | B.Commit_merge _ | B.Checkout_remote _
            | B.Inspect | B.Verify_recovery | B.Continue _ | B.Publish _
            | B.Confirm _ ->
                false
          in
          let stale, effects =
            B.step (restore state)
              (B.Result
                 {
                   token = old_command.token;
                   at = 101.;
                   result = B.Recovery_required "remote_work_not_incorporated";
                 })
          in
          let next =
            if rewrite then
              let state, _ =
                reply (restore state)
                  (B.Remote_replay_selected (B.Inferred (sha 'd')))
              in
              fst (reply (restore state) B.Remote_checked_out)
            else state
          in
          let next =
            if lost_completion then
              let next, _ = B.step (restore next) B.Recover in
              fst
                (reply next
                   (B.Inspected
                      {
                        observation with
                        source = final;
                        head = final;
                        remote = Some incoming;
                        target = (if rewrite then preserved else incoming);
                        target_included = true;
                        completed_integration = true;
                      }))
            else fst (reply (restore next) (B.Integrated final))
          in
          let capture =
            Option.get (Option.get (B.operation next)).remote_integration
          in
          let receipts = B.remote_integrations next in
          let next, _ =
            reply (restore next)
              (B.Retryable { reason = "offline"; retry_after = None })
          in
          let next, _ = B.step (restore next) B.Recover in
          let next, _ =
            reply next
              (B.Inspected
                 {
                   observation with
                   source = final;
                   head = final;
                   remote = Some incoming;
                   target = (if rewrite then preserved else incoming);
                   target_included = true;
                   completed_integration = true;
                 })
          in
          let next, _ =
            reply (restore next)
              (B.Remote { sha = Some incoming; topology = Diverged })
          in
          let next, _ =
            reply (restore next)
              (B.Recovery_required "remote_work_not_incorporated")
          in
          deterministic && B.equal stale state && effects = []
          && capture.source_revision = incoming
          && capture.target_revision = preserved
          && capture.integration_policy = policy
          && capture.base_branch = None
          && B.integrations next = base_receipts
          && B.remote_integrations next = receipts
          && (match receipts with
            | [ receipt ] ->
                receipt.B.capture = capture
                && receipt.integrated_revision = final
                && receipt.evidence
                   =
                   if lost_completion then B.Recovered_completion
                   else B.Executed
            | _ -> false)
          && List.for_all
               (fun revision -> List.mem revision (B.required_revisions next))
               [ preserved; incoming ]
          &&
          match B.pending next with
          | Some { kind = B.Verify_recovery; _ } -> true
          | Some
              {
                kind =
                  ( B.Observe | B.Pin _ | B.Integrate _ | B.Commit_merge _
                  | B.Plan_remote_replay _ | B.Checkout_remote _ | B.Inspect
                  | B.Continue _ | B.Publish _ | B.Confirm _ );
                _;
              }
          | None ->
              false
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "BR old remote captures migrate only from recorded operation evidence"
      Gen.(pair bool (int_range 0 3))
      (fun (rewrite, stage) ->
        try
          let policy = if rewrite then B.Rewrite else B.Preserve_ancestry in
          let state = remote_attempt_fixture policy in
          let state =
            if stage = 0 then state
            else if rewrite then
              let state, _ =
                reply state (B.Remote_replay_selected (B.Inferred source))
              in
              let state, _ = reply state B.Remote_checked_out in
              fst (reply state (B.Integrated candidate))
            else fst (reply state (B.Integrated candidate))
          in
          let state =
            if stage < 2 then state else fst (reply state B.Published)
          in
          let state =
            if stage < 3 then state
            else
              fst
                (reply state
                   (B.Remote { sha = Some candidate; topology = Equal }))
          in
          let old =
            map_active
              (function
                | `Assoc fields ->
                    `Assoc (List.remove_assoc "remote_integration" fields)
                | _ -> failwith "operation")
              (B.yojson_of_t state)
          in
          match B.decode old with
          | Error _ -> false
          | Ok restored ->
              if rewrite || stage > 0 then B.equal restored state
              else
                (Option.get (B.operation restored)).remote_integration = None
                && B.pending restored = B.pending state
                && B.remote_integrations restored = []
                && B.required_revisions restored = B.required_revisions state
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:"BR inconsistent remote attempt captures cannot authorize replay"
      Gen.(pair bool (int_range 0 4))
      (fun (rewrite, corruption) ->
        try
          let policy = if rewrite then B.Rewrite else B.Preserve_ancestry in
          let state = remote_attempt_fixture policy in
          let field, value =
            match corruption with
            | 0 -> ("source_revision", `String (B.Commit.to_string (sha 'f')))
            | 1 -> ("target_revision", `String (B.Commit.to_string (sha 'f')))
            | 2 -> ("base_branch", `String "main")
            | 3 ->
                ( "integration_policy",
                  `List
                    [
                      `String
                        (if rewrite then "Preserve_ancestry" else "Rewrite");
                    ] )
            | _ ->
                ( "replay_boundary",
                  `List
                    [ `String "Recorded"; `String (B.Commit.to_string source) ]
                )
          in
          let forged =
            map_active
              (map_field "remote_integration"
                 (map_field field (fun _ -> value)))
              (B.yojson_of_t state)
          in
          Result.is_error (B.decode forged)
        with _ -> false);
  ]

let preservation_policy_tests =
  [
    QCheck2.Test.make ~count:100
      ~name:"BR adopting a rebase cannot weaken requested ancestry preservation"
      Gen.bool (fun contributor ->
        try
          let intent =
            {
              intent with
              policy = (if contributor then B.Rewrite else B.Preserve_ancestry);
              purpose =
                (if contributor then
                   B.Integrate_revision
                     { contributor = "child"; revision = target }
                 else B.Reconcile_base);
            }
          in
          let state, _ = B.step B.empty (B.Request intent) in
          let state, _ =
            reply state
              (B.Observed_active
                 {
                   observation =
                     {
                       observation with
                       sequencer = Some "rebase";
                       head = target;
                     };
                   policy = B.Rewrite;
                 })
          in
          let op = Option.get (B.operation state) in
          B.execution_policy op = B.Preserve_ancestry
          && B.integration_result op ~candidate ~target_preserved:true
               ~source_preserved:false
             = B.Recovery_required "integration_source_not_preserved"
        with _ -> false);
  ]

let preservation_checkpoint_tests =
  [
    QCheck2.Test.make ~count:100
      ~name:
        "BR ancestry requirements retain captured source and integration target"
      Gen.(pair bool bool)
      (fun (preserve, adopt_merge) ->
        try
          let intent =
            {
              intent with
              policy = (if preserve then B.Preserve_ancestry else B.Rewrite);
            }
          in
          let state, _ = B.step B.empty (B.Request intent) in
          let state, _ =
            reply state
              (B.Observed_active
                 {
                   observation =
                     {
                       observation with
                       sequencer =
                         Some (if adopt_merge then "merge" else "rebase");
                     };
                   policy =
                     (if adopt_merge then B.Preserve_ancestry else B.Rewrite);
                 })
          in
          let op = Option.get (B.operation state) in
          let required = B.ancestry_requirements op in
          (required = if preserve || adopt_merge then [ source; target ] else [])
          && B.equal (Result.get_ok (B.decode (B.yojson_of_t state))) state
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:
        "BR preserving remote successors retain original ancestry obligations"
      Gen.(pair bool bool)
      (fun (publication, race) ->
        try
          let intent =
            {
              intent with
              policy = B.Preserve_ancestry;
              purpose =
                (if publication then B.Publish_revision source
                 else B.Reconcile_base);
            }
          in
          let state = fst (B.step B.empty (B.Request intent)) in
          let incoming = sha 'd' in
          let state =
            fst
              (reply state
                 (B.Observed
                    {
                      observation with
                      remote = Some (if race then source else incoming);
                    }))
          in
          let state = fst (reply state B.Pinned) in
          let state =
            if publication then state
            else fst (reply state (B.Integrated candidate))
          in
          let state =
            if race then
              let state = fst (reply state B.Published) in
              fst
                (reply state
                   (B.Remote { sha = Some incoming; topology = Diverged }))
            else
              fst
                (reply state
                   (B.Recovery_required "remote_work_not_incorporated"))
          in
          let op = Option.get (B.operation state) in
          let expected =
            if publication then [ source; incoming ]
            else [ source; target; candidate; incoming ]
          in
          B.ancestry_requirements op = expected
          && B.execution_policy op = B.Preserve_ancestry
          && B.equal (Result.get_ok (B.decode (B.yojson_of_t state))) state
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "BR legacy downgraded checkpoints restore policy and reopen settlement \
         once"
      Gen.(pair (int_range 0 2) bool)
      (fun (stage, acknowledged) ->
        try
          let state = publishing () in
          let state =
            if stage = 0 then state else fst (reply state B.Published)
          in
          let state =
            if stage < 2 then state
            else
              fst
                (reply state
                   (B.Remote { sha = Some candidate; topology = Equal }))
          in
          let state =
            if acknowledged && stage = 2 then
              let receipt = List.hd (B.publications state) in
              fst
                (B.step state
                   (B.Publication_observed
                      {
                        publication = B.publication_identity receipt;
                        head = candidate;
                        remote = None;
                      }))
            else state
          in
          let json =
            map_active
              (fun active ->
                let active =
                  map_field "intent"
                    (map_field "policy" (fun _ ->
                         `List [ `String "Preserve_ancestry" ]))
                    active
                in
                match active with
                | `Assoc fields ->
                    `Assoc (List.remove_assoc "preservation_policy" fields)
                | _ -> failwith "operation")
              (B.yojson_of_t state)
          in
          let migrated = Result.get_ok (B.decode json) in
          let op = Option.get (B.operation migrated) in
          B.execution_policy op = B.Preserve_ancestry
          && List.for_all
               (fun revision -> List.mem revision (B.ancestry_requirements op))
               [ source; target; candidate ]
          && B.rewrite_publication_input op ~candidate ~expected:source = None
          && B.equal
               (Result.get_ok (B.decode (B.yojson_of_t migrated)))
               migrated
          &&
          if stage < 2 then B.pending migrated = B.pending state
          else
            op.phase = B.Recovering
            && B.publications migrated = []
            && (Option.get (B.pending migrated)).kind = B.Inspect
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR legacy ancestry migration follows exact receipt links only"
      Gen.bool (fun confirmed ->
        try
          let incoming = sha 'd' and replayed = sha 'e' in
          let state = publishing () in
          let state = fst (reply state B.Published) in
          let state =
            fst
              (reply state
                 (B.Remote { sha = Some incoming; topology = Diverged }))
          in
          let state =
            fst (reply state (B.Remote_replay_selected (B.Recorded source)))
          in
          let state = fst (reply state B.Remote_checked_out) in
          let state = fst (reply state (B.Integrated replayed)) in
          let state =
            if confirmed then fst (reply state B.Published) else state
          in
          let legacy =
            map_active
              (fun active ->
                let active =
                  map_field "intent"
                    (map_field "policy" (fun _ ->
                         `List [ `String "Preserve_ancestry" ]))
                    active
                in
                match active with
                | `Assoc fields ->
                    `Assoc (List.remove_assoc "preservation_policy" fields)
                | _ -> failwith "operation")
              (B.yojson_of_t state)
          in
          let legacy =
            map_field "integrations"
              (function
                | `List (receipt :: rest) ->
                    let unrelated =
                      map_field "integrated_revision"
                        (fun _ -> `String (B.Commit.to_string (sha 'f')))
                        receipt
                      |> map_field "capture"
                           (map_field "source_revision" (fun _ ->
                                `String (B.Commit.to_string (sha '9'))))
                    in
                    `List (unrelated :: receipt :: rest)
                | _ -> failwith "missing original integration")
              legacy
          in
          let migrated = Result.get_ok (B.decode legacy) in
          let op = Option.get (B.operation migrated) in
          let required = B.ancestry_requirements op in
          List.for_all
            (fun revision -> List.mem revision required)
            [ source; target; candidate; incoming; replayed ]
          && (not (List.mem (sha '9') required))
          && (not (List.mem (sha 'f') required))
          && B.pending migrated = B.pending state
          && B.equal
               (Result.get_ok (B.decode (B.yojson_of_t migrated)))
               migrated
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR explicit weakened preservation checkpoints are rejected"
      Gen.bool (fun preserve ->
        try
          let json =
            map_active
              (map_field "intent"
                 (map_field "policy" (fun _ ->
                      `List
                        [
                          `String
                            (if preserve then "Preserve_ancestry" else "Rewrite");
                        ])))
              (B.yojson_of_t (publishing ()))
          in
          Result.is_error (B.decode json) = preserve
        with _ -> false);
  ]

let legacy_retention_tests =
  let import state revisions =
    B.import_legacy_anchors state
      (`List
         (List.map
            (fun revision ->
              `Assoc [ ("sha", `String (B.Commit.to_string revision)) ])
            revisions))
  in
  let without_retention state =
    Result.get_ok
      (B.decode
         (map_field "legacy_revisions"
            (fun _ -> `List [])
            (B.yojson_of_t state)))
  in
  let rec json depth =
    let scalar =
      Gen.oneof
        [
          Gen.return `Null;
          Gen.map (fun b -> `Bool b) Gen.bool;
          Gen.map (fun s -> `String s) Gen.string;
          Gen.map (fun n -> `Int n) Gen.int;
        ]
    in
    if depth = 0 then scalar
    else
      Gen.oneof
        [
          scalar;
          Gen.map
            (fun values -> `List values)
            Gen.(list_size (int_range 0 5) (json (depth - 1)));
          Gen.map (fun value -> `Assoc [ ("sha", value) ]) (json (depth - 1));
        ]
  in
  [
    QCheck2.Test.make ~count:100
      ~name:
        "BR scalar and history anchors retain identical evidence without \
         duplication" Gen.bool (fun active ->
        try
          let state = if active then prepared () else B.empty in
          let revision = `String (B.Commit.to_string source) in
          let scalar = B.import_legacy_anchors ~revision state `Null in
          let history = import state [ source ] in
          B.equal scalar history
          && B.equal scalar (B.import_legacy_anchors ~revision history `Null)
          && B.equal state (without_retention scalar)
          && B.decode (B.yojson_of_t scalar) = Ok scalar
        with _ -> false);
    QCheck2.Test.make ~count:500
      ~name:"BR legacy anchor import is total and grants no authority"
      Gen.(pair (json 3) (json 3))
      (fun (legacy, revision) ->
        try
          let state = prepared () in
          let migrated = B.import_legacy_anchors ~revision state legacy in
          B.equal state (without_retention migrated)
          && B.equal migrated
               (B.import_legacy_anchors ~revision migrated legacy)
          && B.equal migrated
               (Result.get_ok (B.decode (B.yojson_of_t migrated)))
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "BR malformed legacy anchor entries cannot discard neighboring valid \
         revisions" Gen.string (fun malformed ->
        try
          let json =
            `List
              [
                `Assoc [ ("sha", `String (B.Commit.to_string source)) ];
                `Assoc [ ("sha", `String ("invalid:" ^ malformed)) ];
                `Null;
                `Assoc [ ("sha", `Bool true) ];
                `String malformed;
                `Assoc [ ("sha", `String (B.Commit.to_string target)) ];
              ]
          in
          let state = B.import_legacy_anchors B.empty json in
          B.required_revisions state = [ source; target ]
          && B.equal B.empty (without_retention state)
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:
        "BR legacy retention is cumulative commutative and unbounded by old \
         history cap"
      Gen.(
        pair
          (list_size (int_range 0 40) (int_range 0 100))
          (list_size (int_range 0 40) (int_range 0 100)))
      (fun (left, right) ->
        try
          let revisions values =
            List.map
              (fun value ->
                Option.get (B.Commit.make (Printf.sprintf "%040x" value)))
              values
          in
          let left = revisions left and right = revisions right in
          let first = import (import B.empty left) right in
          let second = import (import B.empty right) left in
          B.equal first second
          && B.equal first (import first (left @ right))
          && B.required_revisions first
             = List.sort_uniq B.Commit.compare (left @ right)
          && B.operation first = None
          && B.materialization first = None
          && B.publications first = []
          && B.integrations first = []
          && B.remote_integrations first = []
        with _ -> false);
    QCheck2.Test.make ~count:200
      ~name:"BR legacy imports commute with owner transitions and restarts"
      Gen.(list_size (int_range 0 40) bool)
      (fun actions ->
        try
          let _, _, valid =
            List.fold_left
              (fun (baseline, retained, valid) should_import ->
                let retained =
                  Result.get_ok (B.decode (B.yojson_of_t retained))
                in
                if should_import then
                  let migrated = import retained [ sha 'e'; sha 'f' ] in
                  ( baseline,
                    migrated,
                    valid && B.equal baseline (without_retention migrated) )
                else
                  let event =
                    match B.pending baseline with
                    | Some command ->
                        B.Result
                          {
                            token = command.token;
                            at = 100.;
                            result = progress_result command;
                          }
                    | None -> B.Recover
                  in
                  let baseline, effects = B.step baseline event in
                  let retained, retained_effects = B.step retained event in
                  ( baseline,
                    retained,
                    valid && effects = retained_effects
                    && B.equal baseline (without_retention retained) ))
              (prepared (), prepared (), true)
              actions
          in
          valid
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR old owner checkpoints default to no legacy retention" Gen.bool
      (fun complete ->
        try
          let state = if complete then settled () else prepared () in
          let json =
            match B.yojson_of_t state with
            | `Assoc fields ->
                `Assoc
                  (List.remove_assoc "legacy_publication_base"
                     (List.remove_assoc "forge_observations"
                        (List.remove_assoc "legacy_revisions" fields)))
            | _ -> failwith "owner object"
          in
          B.equal state (Result.get_ok (B.decode json))
        with _ -> false);
    QCheck2.Test.make ~count:100
      ~name:"BR malformed retained revision cannot enter owner checkpoint"
      Gen.string (fun invalid ->
        try
          let json =
            map_field "legacy_revisions"
              (fun _ -> `List [ `String ("invalid:" ^ invalid) ])
              (B.yojson_of_t B.empty)
          in
          Result.is_error (B.decode json)
        with _ -> false);
  ]

let forge_history_tests =
  [
    QCheck2.Test.make
      ~name:
        "BR malformed requests cannot create unrestorable state or replace \
         active authority"
      ~print:(fun (base, identity, kind, active) ->
        Printf.sprintf "base=%S identity=%S kind=%d active=%b" base identity
          kind active)
      ~count:600
      Gen.(quad string string (int_range 0 7) bool)
      (fun (base, identity, kind, active) ->
        try
          let purpose =
            match kind with
            | 0 -> B.Reconcile_base
            | 1 -> B.Reconcile_request identity
            | 2 ->
                B.Reconcile_scoped
                  { request = identity; project = "project"; ancestors = [] }
            | 3 -> B.Provision_checkout identity
            | 4 -> B.Publish_session identity
            | 5 ->
                B.Integrate_revision
                  { contributor = identity; revision = target }
            | 6 -> B.Verify_publication
            | _ -> B.Publish_revision source
          in
          let valid =
            (kind > 3 || base <> "") && (kind = 0 || kind > 5 || identity <> "")
          in
          let initial = if active then prepared () else B.empty in
          let state, effects =
            B.step initial (B.Request { base; purpose; policy = B.Rewrite })
          in
          let checkpoint_valid state =
            B.decode (B.yojson_of_t state) = Ok state
          in
          checkpoint_valid state
          &&
          if not valid then B.equal initial state && effects = []
          else if (not active) && kind <= 2 then
            let observed, _ = reply state (B.Observed observation) in
            checkpoint_valid observed
          else true
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR unverified refresh suspends forge conflict authority but retains \
         evidence" ~count:100 Gen.bool (fun mergeable ->
        try
          let module F = Forge_observation in
          let module X = Onton_core_test_support.Forge_fixture in
          let accept state id confirmed_base merge_state =
            let request = X.request id in
            let state, ticket =
              B.begin_forge_observation state ~request ~scope:X.scope
            in
            match ticket with
            | None -> failwith "valid request rejected"
            | Some ticket ->
                fst
                  (B.accept_forge_observation ?confirmed_base state ~ticket
                     ~current:X.scope ~confirmed_head:X.base.Pr_state.head_oid
                     (X.observe request merge_state))
          in
          let initial =
            accept B.empty "verified"
              (Some (String.make 40 'b'))
              Pr_state.Conflicting
          in
          let refreshed =
            accept initial "unverified" None
              (if mergeable then Pr_state.Mergeable else Pr_state.Unknown)
          in
          B.forge_conflict initial ~scope:X.scope
          && (not (B.forge_conflict refreshed ~scope:X.scope))
          && (not (B.has_conflict refreshed ~scope:X.scope))
          && Option.is_some (F.conflict_fact (B.forge_observations refreshed))
          && B.pending refreshed = B.pending initial
          && B.decode (B.yojson_of_t refreshed) = Ok refreshed
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR local conflict survives restart and observation-free interleavings \
         until integration"
      ~count:300
      Gen.(list_size (int_range 0 30) (int_range 0 3))
      (fun events ->
        try
          let module C = Onton_core_test_support.Conflict_fixture in
          let module X = Onton_core_test_support.Forge_fixture in
          let step state event = fst (B.step state event) in
          let state = C.conflict ~base:"main" ~step ~state:Fun.id B.empty in
          let valid, state =
            List.fold_left
              (fun (valid, state) event ->
                let state =
                  match event with
                  | 0 -> step state B.Resume
                  | 1 -> step state B.Recover
                  | 2 -> step state (B.Tick 10000.)
                  | _ -> (
                      match B.decode (B.yojson_of_t state) with
                      | Ok state -> state
                      | Error reason -> failwith reason)
                in
                ( valid
                  && B.has_conflict state ~scope:X.scope
                  && not (B.forge_conflict state ~scope:X.scope),
                  state ))
              (B.has_conflict state ~scope:X.scope, state)
              events
          in
          let resolved = C.resolve ~step ~state:Fun.id state in
          valid
          && (not (B.has_conflict resolved ~scope:X.scope))
          && B.decode (B.yojson_of_t resolved) = Ok resolved
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "BR forge tickets survive restart without changing Git command \
         authority"
      ~count:200 Gen.bool (fun complete ->
        try
          let module F = Forge_observation in
          let module X = Onton_core_test_support.Forge_fixture in
          let state = if complete then settled () else prepared () in
          let pending = B.pending state and phase = B.phase state in
          let refs = B.required_revisions state in
          let request = X.request "owner-forge" in
          let state, ticket =
            B.begin_forge_observation state ~request ~scope:X.scope
          in
          match (ticket, B.decode (B.yojson_of_t state)) with
          | Some ticket, Ok restored ->
              let accepted, result =
                B.accept_forge_observation ~confirmed_base:(String.make 40 'b')
                  restored ~ticket ~current:X.scope
                  ~confirmed_head:X.scope.F.head
                  (X.observe request Pr_state.Conflicting)
              in
              let repeated, duplicate =
                B.accept_forge_observation accepted ~ticket ~current:X.scope
                  ~confirmed_head:None
                  (X.observe request Pr_state.Mergeable)
              in
              Result.is_ok result && Result.is_error duplicate
              && B.forge_conflict accepted ~scope:X.scope
              && B.has_conflict accepted ~scope:X.scope
              && B.equal repeated accepted
              && B.pending accepted = pending
              && B.phase accepted = phase
              && B.required_revisions accepted = refs
              && Option.is_some
                   (F.conflict_fact (B.forge_observations accepted))
              && B.decode (B.yojson_of_t accepted) = Ok accepted
          | None, _ | _, Error _ -> false
        with _ -> false);
    QCheck2.Test.make
      ~name:"BR Git recovery retains pending forge ticket across interleavings"
      ~count:200
      Gen.(list_size (int_range 0 40) (int_range 0 3))
      (fun actions ->
        try
          let module F = Forge_observation in
          let module X = Onton_core_test_support.Forge_fixture in
          let initial, _ =
            B.begin_forge_observation (prepared ())
              ~request:(X.request "retained") ~scope:X.scope
          in
          let expected = B.forge_observations initial in
          let _, valid =
            List.fold_left
              (fun (state, valid) action ->
                let event =
                  match action with
                  | 0 -> B.Recover
                  | 1 -> B.Resume
                  | 2 -> B.Tick 10000.
                  | _ -> B.Request intent
                in
                let state, _ = B.step state event in
                ( state,
                  valid
                  && F.equal_history expected (B.forge_observations state)
                  && B.decode (B.yojson_of_t state) = Ok state ))
              (initial, true) actions
          in
          valid
        with _ -> false);
  ]

let () =
  QCheck_base_runner.run_tests_main
    (tests @ tracking_tests @ publication_observation_tests
   @ initial_remote_integration_tests @ preservation_policy_tests
   @ preservation_checkpoint_tests @ legacy_retention_tests @ m2_upgrade_tests
   @ forge_history_tests)
