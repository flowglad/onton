(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module B = Branch_reconcile
module R = Branch_reconcile_runner

let get = function Ok x -> x | Error e -> failwith e
let check label value = if not value then failwith label
let id = Types.Patch_id.of_string "1"
let main = Types.Branch.of_string "main"
let sha c = Option.get (B.Commit.make (String.make 40 c))
let source = sha 'a'
let target = sha 'b'

let gameplan =
  (get
     (Gameplan_parser.parse_string
        "projectName: ownership\n\
         owner: test\n\
         repo: test\n\
         problemStatement: test\n\
         solutionSummary: test\n\
         patches:\n\
        \  - number: 1\n\
        \    title: patch\n\
        \    description: patch\n\
        \    dependsOn: []\n\
         dependencyGraph:\n\
        \  - patch: 1\n\
        \    dependsOn: []\n"))
    .Gameplan_parser.gameplan

let runtime ~root =
  let runtime = Runtime.create ~gameplan ~main_branch:main () in
  if root then
    Runtime.update_orchestrator runtime (fun orch ->
        Orchestrator.set_execution_mode orch
          (get (Execution_mode.infer_gameplan gameplan)));
  runtime

let persist _ = Ok ()
let now () = 100.

let prepare ~persist runtime =
  R.run ~runtime ~persist ~patch_id:id ~now
    ~execute:(fun ~operation:_ (command : B.command) ->
      match command.kind with
      | B.Observe ->
          B.Observed
            {
              head = source;
              source;
              target;
              remote = None;
              boundary = B.Recorded source;
              topology = B.Unproven;
              clean = true;
              sequencer = None;
              conflicts = 0;
              target_included = false;
              base_contains_source = false;
              completed_integration = false;
              destination =
                Branch_reconcile.Remote_id.of_destination "fixture-origin";
            }
      | B.Pin _ -> B.Pinned
      | B.Integrate _ ->
          B.Conflict { head = source; sequencer = "step"; conflicts = 1 }
      | B.Commit_merge _ | B.Plan_remote_replay _ | B.Checkout_remote _
      | B.Verify_scope _ | B.Inspect | B.Verify_recovery | B.Continue _
      | B.Publish _ | B.Confirm _ ->
          failwith "unexpected preparation command")
    (B.Request { base = "main"; policy = B.Rewrite; purpose = B.Reconcile_base })

let claim runtime =
  match prepare ~persist runtime with
  | R.Repair_needed token -> token
  | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
      failwith "expected claimed repair"

exception Claim_checkpoint_interrupted

let failed_claim ~root ~lost_ack =
  let runtime = runtime ~root in
  let path = Filename.temp_file "repair-claim-checkpoint" ".json" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let state snapshot =
        (Orchestrator.agent snapshot.Runtime.orchestrator id)
          .Patch_agent.branch_reconcile
      in
      let refusals = ref 0 in
      let save = Persistence.save_snapshot ~path in
      let failing_persist snapshot =
        match B.phase (state snapshot) with
        | Some (B.Repairing repair)
          when repair.B.attempt_started && !refusals = 0 ->
            incr refusals;
            if lost_ack then (
              get (save snapshot);
              raise Claim_checkpoint_interrupted)
            else Error "claim checkpoint unavailable"
        | Some
            ( B.Repairing _ | B.Preparing | B.Integrating | B.Publishing
            | B.Confirming | B.Waiting _ | B.Recovering | B.Settled
            | B.Intervention _ )
        | None ->
            save snapshot
      in
      (try
         let outcome = prepare ~persist:failing_persist runtime in
         check "failed claim cannot return permission to dispatch repair"
           ((not lost_ack)
           && outcome = R.Checkpoint_failed "claim checkpoint unavailable")
       with Claim_checkpoint_interrupted ->
         check "only durable claim acknowledgement is lost" lost_ack);
      check "claim checkpoint refusal was reached" (!refusals = 1);
      let saved = get (Persistence.load ~path) in
      check "disk distinguishes refused claims from lost claim acknowledgement"
        (match B.phase (state saved) with
        | Some (B.Repairing repair) -> repair.B.attempt_started = lost_ack
        | Some
            ( B.Preparing | B.Integrating | B.Publishing | B.Confirming
            | B.Waiting _ | B.Recovering | B.Settled | B.Intervention _ )
        | None ->
            false);
      if not lost_ack then
        check "failed claim rolls runtime back to the durable offer"
          (Runtime.read runtime (fun snapshot ->
               B.equal (state snapshot) (state saved)));
      let restored =
        Runtime.create ~gameplan ~main_branch:main ~snapshot:saved ()
      in
      let execute ~operation:_ (command : B.command) =
        match command.kind with
        | B.Inspect ->
            B.Inspected_active
              {
                observation =
                  B.
                    {
                      head = source;
                      source;
                      target;
                      remote = None;
                      boundary = Recorded source;
                      topology = Unproven;
                      clean = false;
                      sequencer = Some "step";
                      conflicts = 1;
                      target_included = false;
                      base_contains_source = false;
                      completed_integration = false;
                      destination = Remote_id.of_destination "fixture-origin";
                    };
                ready = false;
              }
        | B.Pin _ -> B.Pinned
        | B.Verify_scope _ | B.Observe | B.Integrate _ | B.Commit_merge _
        | B.Plan_remote_replay _ | B.Checkout_remote _ | B.Verify_recovery
        | B.Continue _ | B.Publish _ | B.Confirm _ ->
            failwith "claim recovery cannot mutate Git"
      in
      let token =
        match
          R.run ~runtime:restored ~persist:save ~patch_id:id ~now ~execute
            B.Recover
        with
        | R.Repair_needed token -> token
        | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
            failwith "claim recovery did not offer repair"
      in
      let calls = ref 0 in
      let outcome =
        R.run_repair ~runtime:restored ~persist:save ~patch_id:id ~now
          ~with_capacity:(fun f -> f ())
          ~execute:(fun ~agent:_ -> execute)
          ~perform:(fun ~agent:_ ~turn ->
            incr calls;
            check "backend invocation has a durable claim after restart"
              (Option.is_some
                 (B.repair_turn
                    (state (get (Persistence.load ~path)))
                    ~branch:"patch" turn.B.token));
            check "backend dispatch fence is durable before invocation"
              (match B.operation (state (get (Persistence.load ~path))) with
              | Some op -> op.B.agent_dispatched
              | None -> false);
            B.Repair_interrupted
              { token = turn.token; at = 100.; reason = "fixture stopped" })
          token
      in
      check "recovered claim invokes the backend once"
        (!calls = 1 && outcome = R.Waiting))

let check_exclusive acquire =
  let attempted, announce = Eio.Promise.create () in
  Eio.Fiber.first
    (fun () ->
      Eio.Promise.resolve announce ();
      acquire (fun () -> failwith "backend ran without exclusive Git ownership"))
    (fun () ->
      Eio.Promise.await attempted;
      Eio.Fiber.yield ())

let waiting_repair ~root ~stale ~changed =
  let runtime = runtime ~root in
  let token = claim runtime in
  let waiting, wake_waiting = Eio.Promise.create () in
  let capacity, release_capacity = Eio.Promise.create () in
  let has_capacity = ref false
  and calls = ref 0
  and external_work = ref false in
  Eio.Fiber.both
    (fun () ->
      let outcome =
        R.run_repair ~runtime ~persist ~patch_id:id ~now
          ~with_capacity:(fun f ->
            Eio.Promise.resolve wake_waiting ();
            Eio.Promise.await capacity;
            has_capacity := true;
            Fun.protect ~finally:(fun () -> has_capacity := false) f)
          ~execute:(fun ~agent:_ ~operation:_ (command : B.command) ->
            let observation =
              B.
                {
                  head = source;
                  source;
                  target;
                  remote = None;
                  boundary = Recorded source;
                  topology = Unproven;
                  clean = not !external_work;
                  sequencer = Some "step";
                  conflicts = 1;
                  target_included = false;
                  base_contains_source = false;
                  completed_integration = false;
                  destination =
                    Branch_reconcile.Remote_id.of_destination "fixture-origin";
                }
            in
            match command.kind with
            | B.Inspect when !external_work ->
                B.Recovery_required "external_work"
            | B.Inspect -> B.Inspected_active { observation; ready = false }
            | B.Verify_recovery ->
                B.Recovery_verified
                  {
                    observation;
                    local_extension = None;
                    source_preserved = false;
                    remote_preserved = false;
                  }
            | B.Verify_scope _ | B.Observe | B.Pin _ | B.Integrate _
            | B.Continue _ | B.Commit_merge _ | B.Plan_remote_replay _
            | B.Checkout_remote _ | B.Publish _ | B.Confirm _ ->
                failwith "repair handoff cannot blindly mutate Git")
          ~perform:(fun ~agent:_ ~turn ->
            incr calls;
            check "backend owns capacity" !has_capacity;
            check_exclusive (fun f ->
                Runtime.with_patch_ownership runtime ~patch_id:id (fun _ ->
                    f ()));
            if root then check_exclusive (Runtime.with_root_write runtime);
            B.Repair_interrupted
              { token = turn.token; at = 100.; reason = "backend interrupted" })
          token
      in
      check "stale repair is suppressed and changed work gets a fresh claim"
        (if stale then outcome = R.Idle else outcome = R.Waiting))
    (fun () ->
      Eio.Promise.await waiting;
      Runtime.with_patch_ownership runtime ~patch_id:id (fun _ -> ());
      Runtime.with_root_write runtime (fun () -> ());
      check "waiting does not own capacity" (not !has_capacity);
      external_work := changed;
      if stale then
        Runtime.update_orchestrator runtime (fun orch ->
            fst
              (Orchestrator.reconcile_branch orch id
                 (B.Repair_interrupted
                    { token; at = 100.; reason = "superseded" })));
      Eio.Promise.resolve release_capacity ());
  check "only current inspected claims invoke the backend"
    (!calls = if stale then 0 else 1);
  check "capacity released after repair" (not !has_capacity)

let terminal_during_capacity_wait ~root ~merged =
  let runtime = runtime ~root in
  let token = claim runtime in
  let before =
    Runtime.read runtime (fun snap ->
        (Orchestrator.agent snap.Runtime.orchestrator id)
          .Patch_agent.branch_reconcile)
  in
  let outcome =
    R.run_repair ~runtime ~persist ~patch_id:id ~now
      ~with_capacity:(fun f ->
        Runtime.update_orchestrator runtime (fun orch ->
            if merged then Orchestrator.mark_merged orch id
            else
              Orchestrator.apply_session_result orch id
                (Orchestrator.Session_wontdo "stop"));
        f ())
      ~execute:(fun ~agent:_ ~operation:_ _ ->
        failwith "terminal patch cannot mutate Git")
      ~perform:(fun ~agent:_ ~turn:_ ->
        failwith "terminal patch cannot start repair")
      token
  in
  let reason = if merged then "patch_merged" else "patch_wontdo" in
  check "terminal patch blocks a waiting repair"
    (outcome = R.Intervention reason);
  check "terminal state retains checkpoint"
    (Runtime.read runtime (fun snap ->
         B.equal before
           (Orchestrator.agent snap.Runtime.orchestrator id)
             .Patch_agent.branch_reconcile));
  let outcome =
    R.run ~runtime ~persist ~patch_id:id ~now
      ~execute:(fun ~operation:_ _ ->
        failwith "terminal patch cannot resume Git")
      B.Recover
  in
  check "terminal state also blocks direct recovery"
    (outcome = R.Intervention reason)

let cancelled_session ~root =
  let runtime = runtime ~root in
  let message =
    Runtime.update_orchestrator_returning runtime (fun orch ->
        let message =
          Orchestrator.message_of_action
            (Orchestrator.agent orch id)
            (Orchestrator.Start (id, main))
        in
        let orch = Orchestrator.reconcile_message orch message in
        let orch, accepted =
          Orchestrator.accept_message orch message.message_id
        in
        check "start accepted" (Option.is_some accepted);
        (orch, message.message_id))
  in
  let path = Filename.temp_file "busy-guard-ownership" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let module Env = struct
        let runtime = runtime
        let event_log = Event_log.create ~path
      end in
      let module Guard = With_busy_guard.Make (Env) in
      let waiting, wake_waiting = Eio.Promise.create () in
      let unavailable, _ = Eio.Promise.create () in
      Eio.Fiber.first
        (fun () ->
          Guard.run ~patch_id:id ~message_id:message
            ~with_capacity:(fun f ->
              Eio.Promise.resolve wake_waiting ();
              Eio.Promise.await unavailable;
              f ())
            (fun _ -> failwith "cancelled wait must not start a session"))
        (fun () ->
          Eio.Promise.await waiting;
          Runtime.with_patch_ownership runtime ~patch_id:id (fun _ -> ());
          Runtime.with_root_write runtime (fun () -> ()));
      check "cancellation during capacity wait clears busy"
        (not
           (Runtime.read runtime (fun snap ->
                (Orchestrator.agent snap.Runtime.orchestrator id)
                  .Patch_agent.busy))))

let cancelled_active_repair ~root ~resolved ~compete =
  let runtime = runtime ~root in
  let path = Filename.temp_file "cancelled-active-repair" ".json" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let persist = Persistence.save_snapshot ~path in
      let token =
        match prepare ~persist runtime with
        | R.Repair_needed token -> token
        | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
            failwith "expected repair before cancellation"
      in
      let staged = ref false
      and integrated = ref false
      and published = ref false in
      let capacity = ref false
      and calls = ref 0
      and continuations = ref 0
      and pushes = ref 0 in
      let candidate = sha 'c' in
      let queued_intent =
        B.
          {
            base = "main";
            policy = Rewrite;
            purpose = Reconcile_request "during-repair";
          }
      in
      let observed_successor = ref 0 in
      let execute ~agent:_ ~(operation : B.operation) (command : B.command) =
        match command.kind with
        | B.Verify_scope candidate ->
            Onton_core_test_support.Scope_fixture.verified
              operation.approved_scope candidate
        | B.Inspect ->
            B.Inspected_active
              {
                observation =
                  B.
                    {
                      destination = Remote_id.of_destination "fixture-origin";
                      head = (if !integrated then candidate else source);
                      source = (if !integrated then candidate else source);
                      target;
                      remote = (if !published then Some candidate else None);
                      boundary = Recorded source;
                      topology = Unproven;
                      clean = !integrated;
                      sequencer = (if !integrated then None else Some "step");
                      conflicts = (if !staged || !integrated then 0 else 1);
                      target_included = !integrated;
                      base_contains_source = false;
                      completed_integration = !integrated;
                    };
                ready = !staged;
              }
        | B.Continue { head; target = captured; sequencer } ->
            check "continuation requires the staged captured repair"
              (!staged && (not !integrated) && head = source
             && captured = target && sequencer = "step");
            incr continuations;
            integrated := true;
            B.Integrated candidate
        | B.Publish { candidate = captured; expected } ->
            check "publication follows continuation under its lease"
              (!integrated && captured = candidate && expected = None
             && not !published);
            incr pushes;
            published := true;
            B.Published
        | B.Confirm captured ->
            check "confirmation retains repaired candidate"
              (captured = candidate);
            B.Remote
              {
                sha = (if !published then Some candidate else None);
                topology = Equal;
              }
        | B.Observe ->
            check "successor observes only after repaired publication"
              (compete && !integrated && !published);
            incr observed_successor;
            B.Observed
              B.
                {
                  destination = Remote_id.of_destination "fixture-origin";
                  head = candidate;
                  source = candidate;
                  target;
                  remote = Some candidate;
                  boundary = Recorded source;
                  topology = Equal;
                  clean = true;
                  sequencer = None;
                  conflicts = 0;
                  target_included = true;
                  base_contains_source = false;
                  completed_integration = true;
                }
        | B.Pin { source = captured; target = captured_target } ->
            check "successor pins the already published result"
              (compete && !published && captured = candidate
             && captured_target = target);
            B.Pinned
        | B.Integrate _ | B.Commit_merge _ | B.Plan_remote_replay _
        | B.Checkout_remote _ | B.Verify_recovery ->
            failwith
              "cancelled repair must inspect before any repeated integration"
      in
      let with_capacity f =
        check "capacity cannot overlap" (not !capacity);
        capacity := true;
        Fun.protect ~finally:(fun () -> capacity := false) f
      in
      let started, announce = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let attempting, announce_attempt = Eio.Promise.create () in
      let request_finished = ref false in
      let cancel_repair () =
        Eio.Fiber.first
          (fun () ->
            ignore
              (R.run_repair ~runtime ~persist ~patch_id:id ~now ~with_capacity
                 ~execute
                 ~perform:(fun ~agent:_ ~turn:_ ->
                   incr calls;
                   staged := resolved;
                   Eio.Promise.resolve announce ();
                   Eio.Promise.await never)
                 token))
          (fun () ->
            Eio.Promise.await started;
            check "running backend holds capacity" !capacity;
            check_exclusive (fun f ->
                Runtime.with_patch_ownership runtime ~patch_id:id (fun _ ->
                    f ()));
            if root then check_exclusive (Runtime.with_root_write runtime);
            if compete then (
              Eio.Promise.await attempting;
              Eio.Fiber.yield ();
              check "competing request cannot acquire running repair ownership"
                (not !request_finished)))
      in
      Eio.Fiber.both cancel_repair (fun () ->
          if compete then (
            Eio.Promise.await started;
            Eio.Promise.resolve announce_attempt ();
            let result =
              R.run ~runtime ~persist ~patch_id:id ~now
                ~execute:(fun ~operation:_ _ ->
                  failwith "queued trigger dispatched during cancelled repair")
                (B.Request queued_intent)
            in
            check "competing trigger queues without changing active repair"
              (result = R.Waiting);
            request_finished := true));
      check "competing trigger completed after ownership release"
        (!request_finished = compete);
      check "cancellation releases capacity" (not !capacity);
      Runtime.with_patch_ownership runtime ~patch_id:id (fun _ -> ());
      Runtime.with_root_write runtime (fun () -> ());
      check "cancelled backend did not continue or publish"
        (!calls = 1 && !continuations = 0 && !pushes = 0);
      let snapshot = get (Persistence.load ~path) in
      let state =
        (Orchestrator.agent snapshot.Runtime.orchestrator id)
          .Patch_agent.branch_reconcile
      in
      check "cancelled claim stays durable without consuming budget"
        (match B.phase state with
        | Some (B.Repairing repair) ->
            repair.attempt_started
            && (not repair.attempt_completed)
            && repair.attempts_without_progress = 0
        | Some
            ( B.Preparing | B.Integrating | B.Publishing | B.Confirming
            | B.Waiting _ | B.Recovering | B.Settled | B.Intervention _ )
        | None ->
            false);
      let restored = Runtime.create ~gameplan ~main_branch:main ~snapshot () in
      let result =
        R.run ~runtime:restored ~persist ~patch_id:id ~now
          ~execute:(fun ~operation command ->
            let agent =
              Runtime.read restored (fun snapshot ->
                  Orchestrator.agent snapshot.Runtime.orchestrator id)
            in
            execute ~agent ~operation command)
          B.Recover
      in
      let result =
        match result with
        | R.Repair_needed fresh ->
            check "only unresolved cancellation needs another turn"
              (not resolved);
            R.run_repair ~runtime:restored ~persist ~patch_id:id ~now
              ~with_capacity ~execute
              ~perform:(fun ~agent:_ ~turn ->
                incr calls;
                staged := true;
                B.Repair_completed { token = turn.token; at = now () })
              fresh
        | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
            result
      in
      check "restarted cancellation settles" (result = R.Idle);
      check "queued trigger observes a fixed point instead of repeating work"
        (!observed_successor = if compete then 1 else 0);
      if compete then
        check "queued intent survives persisted cancellation and restart"
          (Runtime.read restored (fun snapshot ->
               match
                 B.operation
                   (Orchestrator.agent snapshot.Runtime.orchestrator id)
                     .Patch_agent.branch_reconcile
               with
               | Some operation ->
                   B.equal_intent operation.B.intent queued_intent
               | None -> false));
      check "staged cancellation avoids duplicate agent work"
        ((!calls = if resolved then 1 else 2)
        && !continuations = 1 && !pushes = 1);
      let stale =
        R.run_repair ~runtime:restored ~persist ~patch_id:id ~now ~with_capacity
          ~execute:(fun ~agent:_ ~operation:_ _ ->
            failwith "stale claim executed Git")
          ~perform:(fun ~agent:_ ~turn:_ ->
            failwith "stale claim invoked backend")
          token
      in
      check "cancelled old claim cannot restart settled work" (stale = R.Idle);
      let saved = get (Persistence.load ~path) in
      check "settlement is durable"
        (B.phase
           (Orchestrator.agent saved.Runtime.orchestrator id)
             .Patch_agent.branch_reconcile
        = Some B.Settled))

let backend_acceptance ~cwd ~diagnosis accepted =
  let runtime = runtime ~root:false in
  let state, token =
    if diagnosis then
      let state, _ =
        B.step B.empty
          (B.Request
             {
               base = "main";
               policy = Rewrite;
               purpose = Provision_checkout "missing";
             })
      in
      let command = Option.get (B.pending state) in
      let state, effects =
        B.step state
          (B.Result
             {
               token = command.token;
               at = now ();
               result = Needs_diagnosis "checkout missing";
             })
      in
      let token =
        match effects with
        | [ B.Repair token ] -> token
        | []
        | (B.Execute _ | B.Start_repair _ | B.Completed _) :: _
        | B.Repair _ :: _ :: _ ->
            failwith "missing diagnostic offer"
      in
      (fst (B.step state (B.Repair_started token)), token)
    else
      let token = claim runtime in
      ( Runtime.read runtime (fun snap ->
            (Orchestrator.agent snap.Runtime.orchestrator id)
              .Patch_agent.branch_reconcile),
        token )
  in
  let turn = Option.get (B.repair_turn state ~branch:"patch" token) in
  let backend =
    Llm_backend.
      {
        name = "acceptance fixture";
        run_streaming =
          (fun ~project_name:_
            ~cwd:_
            ~patch_id:_
            ~prompt:_
            ~resume_session:_
            ~session_uuid:_
            ~complexity:_
            ~on_event
          ->
            on_event
              (if accepted then Types.Stream_event.Turn_started
               else Types.Stream_event.Error "preflight");
            {
              exit_code = 1;
              stdout = "";
              stderr = "backend stopped";
              got_events = true;
              saw_final_result = false;
              timed_out = accepted;
            });
      }
  in
  let streamed = ref [] in
  let event =
    Branch_repair_session.run ~resume_session:(Some "patch-session")
      ~session_uuid:"patch-session"
      ~on_event:(fun event -> streamed := event :: !streamed)
      ~context:"" ~guidance:[] ~backend ~cwd ~project_name:"ownership"
      ~patch_id:id ~complexity:None ~turn
      ~read_head:(fun () ->
        if diagnosis then None else Some (B.Commit.to_string source))
      ~now
  in
  check "owned recovery exposes its backend events" (List.length !streamed = 1);
  check "only an accepted backend failure completes a repair attempt"
    (event
    =
    if accepted then
      B.Repair_failed { token; at = now (); reason = "backend stopped" }
    else B.Repair_interrupted { token; at = now (); reason = "backend stopped" }
    )

let () =
  Eio_main.run (fun env ->
      List.iter
        (fun diagnosis ->
          List.iter
            (backend_acceptance ~cwd:(Eio.Stdenv.fs env) ~diagnosis)
            [ false; true ])
        [ false; true ];
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
          List.iter
            (fun root ->
              List.iter
                (fun lost_ack -> failed_claim ~root ~lost_ack)
                [ false; true ];
              waiting_repair ~root ~stale:false ~changed:false;
              waiting_repair ~root ~stale:true ~changed:false;
              waiting_repair ~root ~stale:false ~changed:true;
              cancelled_session ~root;
              List.iter
                (fun compete ->
                  cancelled_active_repair ~root ~resolved:false ~compete;
                  cancelled_active_repair ~root ~resolved:true ~compete)
                [ false; true ];
              terminal_during_capacity_wait ~root ~merged:false;
              terminal_during_capacity_wait ~root ~merged:true)
            [ false; true ]));
  print_endline "reconciliation capacity and Git ownership: OK"
