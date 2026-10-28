(* @archlint.module test
   @archlint.domain coordinator-state *)
open Onton_core

[@@@warning "-4-40-42"]

module C = Coordinator_config

let test name gen f =
  QCheck2.Test.make ~count:500 ~name gen (fun x -> try f x with _ -> false)

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
    [ "feature"; "unknown"; "" ];
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
            Coordinator_journal.entry_of_yojson
              (`Assoc
                 [
                   ("id", `String "effect:body");
                   ("scope", `String serialized);
                   ("attempts", `Int 1);
                   ("phase", `List [ `String "Applied"; `String "applied" ]);
                 ])
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
          let e =
            Coordinator_journal.entry_of_yojson
              (`Assoc
                 [
                   ("id", `String "bad");
                   ("scope", `String scope);
                   ("attempts", `Int 1);
                   ("phase", `List [ `String "Applied"; `String result ]);
                 ])
          in
          not
            (C.body_delivered [ e ]
               (Patch_agent.create ~branch:patch.branch patch.id)));
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
          List.map (fun a -> a.Patch_agent.queue) next.agents
          = List.map (fun a -> a.Patch_agent.queue) twice.agents
          && ((not busy)
             || List.for_all (fun a -> a.Patch_agent.queue = []) next.agents));
    ]
