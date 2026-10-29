(* @archlint.module test
   @archlint.domain coordinator *)
open Onton_core

[@@@warning "-4-40-42"]

module C = Coordinator_config

let test name gen f =
  QCheck2.Test.make ~count:500 ~name gen (fun x -> try f x with _ -> false)

let applied_entry ~id ~scope ~result =
  let journal =
    match
      Coordinator_journal.begin_attempt Coordinator_journal.empty ~id ~scope
    with
    | Ok journal -> journal
    | Error _ -> failwith "valid journal intent"
  in
  let journal =
    match Coordinator_journal.settle journal ~id ~result:(Some result) with
    | Ok journal -> journal
    | Error _ -> failwith "valid journal result"
  in
  match Coordinator_journal.find journal id with
  | Some entry -> entry
  | None -> failwith "journal entry missing"

let fields =
  [
    ("version", `Int 3);
    ("capacity", `Int 2);
    ("repo_root", `String "/repo");
    ("main_branch", `String "main");
    ("worker_executable", `String "onton-worker");
    ("backend", `String "codex");
    ("execution_mode", `String "mainline");
  ]

let patch : Types.Patch.t =
  {
    id = Types.Patch_id.of_string "a";
    branch = Types.Branch.of_string "patch-a";
    title = "a";
    description = "";
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

let () =
  assert (Result.is_ok (C.decode (Yojson.Safe.to_string (`Assoc fields))));
  List.iter
    (fun mode ->
      assert (
        Result.is_error
          (C.decode
             (Yojson.Safe.to_string
                (`Assoc
                   (("execution_mode", `String mode)
                   :: List.remove_assoc "execution_mode" fields))))))
    [ "unknown"; "" ];
  assert (
    Result.is_ok
      (C.decode
         (Yojson.Safe.to_string
            (`Assoc
               (("execution_mode", `String "feature")
               :: List.remove_assoc "execution_mode" fields)))));
  QCheck_base_runner.run_tests_main
    [
      test "body delivery cannot survive a new owner generation"
        QCheck2.Gen.(int_range 0 20)
        (fun generations ->
          let main = Types.Branch.of_string "main" in
          let agent = Patch_agent.create ~branch:patch.branch patch.id in
          let agent = Patch_agent.set_base_branch agent main in
          let agent =
            Patch_agent.set_pr_number agent (Types.Pr_number.of_int 1)
          in
          let agent =
            Patch_agent.set_head_oid agent
              (Some "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
          in
          let agent =
            List.init generations (fun _ -> ())
            |> List.fold_left (fun a () -> Patch_agent.bump_generation a) agent
          in
          let scope : Coordinator_observation.scope =
            {
              ticket = "body";
              pr = Types.Pr_number.of_int 1;
              head_branch = patch.branch;
              base_branch = main;
              head_revision = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
              base_revision = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
              push_destination = "origin";
              generation = agent.generation;
            }
          in
          let serialized =
            Yojson.Safe.to_string
              (`List
                 [
                   Coordinator_observation.yojson_of_scope scope;
                   `String "a";
                   `String "body:rendered";
                 ])
          in
          let e =
            applied_entry ~id:"effect:body" ~scope:serialized ~result:"applied"
          in
          C.body_delivered [ e ] agent
          && not (C.body_delivered [ e ] (Patch_agent.bump_generation agent)));
      test "configuration and durable work decoders are total"
        QCheck2.Gen.string (fun s ->
          ignore (C.decode s);
          ignore (C.work s);
          true);
      test "unknown and duplicate configuration fields fail closed"
        QCheck2.Gen.string (fun key ->
          Result.is_error
            (C.decode (Yojson.Safe.to_string (`Assoc ((key, `Null) :: fields)))));
      test "durable scheduling work roundtrip"
        QCheck2.Gen.(pair string bool)
        (fun (spec, start) ->
          let work =
            if start then
              Worker_protocol_v3.Session (Worker_protocol.Start, None, spec)
            else Reconcile
          in
          C.work (Yojson.Safe.to_string (C.work_json work)) = Ok work);
      test "malformed delivery evidence cannot grant PR body delivery"
        QCheck2.Gen.(pair string string)
        (fun (scope, result) ->
          let main = Types.Branch.of_string "main" in
          let agent = Patch_agent.create ~branch:patch.branch patch.id in
          let agent = Patch_agent.set_base_branch agent main in
          let agent =
            Patch_agent.set_pr_number agent (Types.Pr_number.of_int 1)
          in
          let agent =
            Patch_agent.set_head_oid agent
              (Some "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
          in
          let e =
            applied_entry ~id:"bad" ~scope:("malformed:" ^ scope)
              ~result:(result ^ "!")
          in
          not (C.body_delivered [ e ] agent));
      test "shared reconciliation is idempotent and does not mutate busy owners"
        QCheck2.Gen.(pair bool bool)
        (fun (busy, pr) ->
          let a = Patch_agent.create ~branch:patch.branch patch.id in
          let main = Types.Branch.of_string "main" in
          let a =
            if busy then Patch_agent.start a ~base_branch:main
            else Patch_agent.set_base_branch a main
          in
          let a =
            if pr then Patch_agent.set_pr_number a (Types.Pr_number.of_int 1)
            else a
          in
          let view : Scheduler_decision.input =
            {
              graph = Graph.of_patches [ patch ];
              mode = Execution_mode.mainline;
              agents = [ a ];
              main = Types.Branch.of_string "main";
            }
          in
          let next = C.reconcile view in
          let twice = C.reconcile next in
          List.map Patch_agent.sexp_of_t next.agents
          = List.map Patch_agent.sexp_of_t twice.agents
          && ((not busy)
             || List.map Patch_agent.sexp_of_t next.agents
                = [ Patch_agent.sexp_of_t a ]));
      QCheck2.Test.make ~count:300
        ~name:"coordinator config generated public decision surface"
        (QCheck2.Gen.int_range 1 1_000_000) (fun seed ->
          try
            let suffix = string_of_int seed in
            let main = Types.Branch.of_string "main" in
            let config_json =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("version", `Int 3);
                     ("capacity", `Int (1 + (seed mod 256)));
                     ("repo_root", `String ("/repo/" ^ suffix));
                     ("main_branch", `String "main");
                     ("worker_executable", `String ("worker-" ^ suffix));
                     ("backend", `String "codex");
                     ("execution_mode", `String "mainline");
                   ])
            in
            match C.decode config_json with
            | Error _ -> false
            | Ok config -> (
                let encoded_config =
                  Yojson.Safe.to_string (C.yojson_of_t config)
                in
                let work =
                  match seed mod 3 with
                  | 0 -> Worker_protocol_v3.Reconcile
                  | 1 ->
                      Worker_protocol_v3.Integrate_revision
                        { contributor = suffix; revision = String.make 40 'a' }
                  | _ ->
                      Worker_protocol_v3.Session
                        (Worker_protocol.Start, None, "spec-" ^ suffix)
                in
                let work_roundtrip =
                  C.work (Yojson.Safe.to_string (C.work_json work)) = Ok work
                in
                let agent = Patch_agent.create ~branch:patch.branch patch.id in
                let view : Scheduler_decision.input =
                  Scheduler_decision.
                    {
                      graph = Graph.of_patches [ patch ];
                      mode = Execution_mode.mainline;
                      agents = [ agent ];
                      main;
                    }
                in
                let reconciled = C.reconcile view in
                let repeated = C.reconcile reconciled in
                Result.is_ok (C.decode encoded_config)
                && work_roundtrip
                && C.body_delivered [] agent = false
                && List.map (fun a -> a.Patch_agent.queue) reconciled.agents
                   = List.map (fun a -> a.Patch_agent.queue) repeated.agents
                &&
                match
                  C.work_of_decision ~spec:suffix
                    (Scheduler_decision.Start (patch.id, main))
                with
                | Worker_protocol_v3.Session (Worker_protocol.Start, None, spec)
                  ->
                    String.equal spec suffix
                | _ -> false)
          with _ -> false);
    ]
