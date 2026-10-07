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

let claim runtime =
  match
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
                completed_integration = false;
                destination =
                  Branch_reconcile.Remote_id.of_destination "fixture-origin";
              }
        | B.Pin _ -> B.Pinned
        | B.Integrate _ ->
            B.Conflict { head = source; sequencer = "step"; conflicts = 1 }
        | B.Commit_merge _ | B.Plan_remote_replay _ | B.Checkout_remote _
        | B.Inspect | B.Verify_recovery | B.Continue _ | B.Publish _
        | B.Confirm _ ->
            failwith "unexpected preparation command")
      (B.Request
         { base = "main"; policy = B.Rewrite; purpose = B.Reconcile_base })
  with
  | R.Repair_needed token -> token
  | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
      failwith "expected claimed repair"

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
                    source_preserved = false;
                    remote_preserved = false;
                  }
            | B.Observe | B.Pin _ | B.Integrate _ | B.Continue _
            | B.Commit_merge _ | B.Plan_remote_replay _ | B.Checkout_remote _
            | B.Publish _ | B.Confirm _ ->
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
      check "stale repair is suppressed"
        (outcome
        =
        if stale then R.Idle
        else if changed then R.Intervention "history_recovery_dirty_worktree"
        else R.Waiting))
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
    (!calls = if stale || changed then 0 else 1);
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

let () =
  Eio_main.run (fun env ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
          List.iter
            (fun root ->
              waiting_repair ~root ~stale:false ~changed:false;
              waiting_repair ~root ~stale:true ~changed:false;
              waiting_repair ~root ~stale:false ~changed:true;
              cancelled_session ~root;
              terminal_during_capacity_wait ~root ~merged:false;
              terminal_during_capacity_wait ~root ~merged:true)
            [ false; true ]));
  print_endline "reconciliation capacity and Git ownership: OK"
