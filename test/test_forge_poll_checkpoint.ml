(* @archlint.module test
   @archlint.domain forge-observation *)

open Base
open Onton
open Onton_core
open Types
module F = Forge_observation
module X = Onton_core_test_support.Forge_fixture

let check message condition = if not condition then failwith message
let get = function Ok value -> value | Error reason -> failwith reason
let pid = Patch_id.of_string "patch"
let main = Branch.of_string "main"

let patch : Patch.t =
  {
    id = pid;
    title = "patch";
    description = "";
    branch = Branch.of_string "patch";
    dependencies = [];
    spec = "";
    acceptance_criteria = [];
    files = [];
    classification = "";
    changes = [];
    test_stubs_introduced = [];
    test_stubs_implemented = [];
    complexity = None;
    precedents = [];
    required_context = [];
  }

let gameplan : Gameplan.t =
  {
    project_name = "forge-checkpoint";
    repo_owner = "";
    repo_name = "";
    problem_statement = "";
    architecture_design = None;
    solution_summary = "";
    final_state_spec = "";
    patches = [ patch ];
    operational_considerations = "";
    required_changes = "";
    ordering_constraints = [];
    current_state_analysis = "";
    explicit_opinions = "";
    acceptance_criteria = [];
    open_questions = [];
    functional_changes = [];
    context_resources = [];
    publication = None;
    reachability_traces = [];
  }

let current runtime =
  Runtime.read runtime (fun snap ->
      Orchestrator.agent snap.Runtime.orchestrator pid)

let history agent =
  Branch_reconcile.forge_observations agent.Patch_agent.branch_reconcile

let request runtime label = [ (pid, current runtime, X.request label) ]

let only = function
  | [ prepared ] -> prepared
  | _ -> failwith "expected one prepared request"

let accept runtime (prepared : Forge_poll.prepared) =
  Runtime.update_orchestrator_returning runtime (fun orch ->
      Orchestrator.accept_forge_observation orch pid ~ticket:prepared.ticket
        ~confirmed_head:X.scope.F.head
        (X.observe prepared.ticket.F.request Pr_state.Conflicting))

let () =
  Eio_main.run (fun _ ->
      let path = Stdlib.Filename.temp_file "onton-forge-checkpoint-" ".json" in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          if Stdlib.Sys.file_exists path then Stdlib.Sys.remove path)
        (fun () ->
          let runtime = Runtime.create ~gameplan ~main_branch:main () in
          Runtime.update_orchestrator runtime (fun orch ->
              Orchestrator.set_pr_number orch pid (Pr_number.of_int 7));
          let initial = current runtime in
          let failed =
            Forge_poll.prepare ~runtime
              ~persist:(fun _ -> Error "disk full")
              (request runtime "failed")
          in
          check "failed checkpoint cannot expose a dispatchable request"
            (Result.is_error failed);
          check "failed checkpoint leaves runtime unchanged"
            (Patch_agent.equal initial (current runtime));
          let original_requests = request runtime "first" in
          let first =
            only
              (get
                 (Forge_poll.prepare ~runtime
                    ~persist:(Persistence.save_snapshot ~path)
                    original_requests))
          in
          let saved = get (Persistence.load ~path) in
          let stored = Orchestrator.agent saved.Runtime.orchestrator pid in
          check "returned ticket is already durable"
            (Option.equal F.equal_ticket
               (F.pending_request (history stored))
               (Some first.ticket));
          check "saved context is the exact dispatch snapshot"
            (Poll_cycle.same_context ~requested:first.requested ~current:stored);
          let stale =
            get
              (Forge_poll.prepare ~runtime
                 ~persist:(Persistence.save_snapshot ~path)
                 original_requests)
          in
          check "changed context cannot dispatch an old request"
            (List.is_empty stale);
          let second =
            only
              (get
                 (Forge_poll.prepare ~runtime
                    ~persist:(Persistence.save_snapshot ~path)
                    (request runtime "second")))
          in
          check "replacement ticket increases"
            (second.ticket.F.sequence > first.ticket.F.sequence);
          let runtime =
            Runtime.create ~gameplan ~main_branch:main
              ~snapshot:(get (Persistence.load ~path))
              ()
          in
          check "old response cannot win after restart"
            (Result.is_error (accept runtime first));
          check "new response is accepted after restart"
            (Result.is_ok (accept runtime second));
          check "duplicate response cannot apply twice"
            (Result.is_error (accept runtime second));
          get (Runtime.read runtime (Persistence.save_snapshot ~path));
          let saved = get (Persistence.load ~path) in
          let restored = Orchestrator.agent saved.Runtime.orchestrator pid in
          check "accepted conflict evidence is durable"
            (Option.exists
               (F.conflict_fact (history restored))
               ~f:(fun fact -> F.equal_ticket fact.F.ticket second.ticket));
          let third =
            only
              (get
                 (Forge_poll.prepare ~runtime
                    ~persist:(Persistence.save_snapshot ~path)
                    (request runtime "third")))
          in
          Runtime.update_orchestrator runtime (fun orch ->
              Orchestrator.set_pr_number orch pid (Pr_number.of_int 8));
          check "replaced PR rejects outstanding response"
            (Result.is_error (accept runtime third));
          let before = current runtime in
          let cancelled =
            try
              ignore
                (Forge_poll.prepare ~runtime
                   ~persist:(fun _ -> failwith "cancelled")
                   [
                     ( pid,
                       before,
                       F.{ id = "cancelled"; pr_number = Pr_number.of_int 8 } );
                   ]);
              false
            with Failure reason -> String.equal reason "cancelled"
          in
          check "interrupted checkpoint propagates without dispatch" cancelled;
          check "interrupted checkpoint preserves state and releases mutex"
            (Patch_agent.equal before (current runtime));
          Stdlib.print_endline
            "forge request persistence, restart and stale-result isolation: OK"))
