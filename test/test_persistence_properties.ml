(* @archlint.module stateTest
   @archlint.domain orchestrator *)

open Base
open Onton_core.Types
open Onton_test_support.Test_generators

(** QCheck2 property-based tests for persistence round-trips.

    These tests verify that serialize → deserialize is the identity for all
    persisted types, using the generators from [Test_generators]. *)

(** Build a minimal gameplan containing a single patch with the given agent's
    [patch_id] and [branch], so [patch_agent_of_yojson] can derive the branch
    for legacy snapshots that lack a ["branch"] key. *)
let gameplan_for_agent (agent : Onton_core.Patch_agent.t) =
  Gameplan.
    {
      project_name = "t";
      repo_owner = "";
      repo_name = "";
      problem_statement = "t";
      architecture_design = None;
      solution_summary = "t";
      final_state_spec = "";
      patches =
        [
          {
            Patch.id = agent.patch_id;
            branch = agent.branch;
            title = "";
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
          };
        ];
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

(** Compare two snapshots field by field. Orchestrator.t is opaque without [eq],
    so we compare agents_map entries and main_branch. *)
let snapshots_equal (a : Onton.Runtime.snapshot) (b : Onton.Runtime.snapshot) =
  let agents_a = Onton.Orchestrator.agents_map a.orchestrator |> Map.to_alist in
  let agents_b = Onton.Orchestrator.agents_map b.orchestrator |> Map.to_alist in
  let agents_eq =
    List.equal
      (fun (ka, va) (kb, vb) ->
        Patch_id.equal ka kb && Onton_core.Patch_agent.equal va vb)
      agents_a agents_b
  in
  let main_eq =
    Branch.equal
      (Onton.Orchestrator.main_branch a.orchestrator)
      (Onton.Orchestrator.main_branch b.orchestrator)
  in
  let graph_pids_eq =
    let pids_a =
      Onton.Orchestrator.graph a.orchestrator
      |> Onton_core.Graph.all_patch_ids
      |> List.sort ~compare:Patch_id.compare
    in
    let pids_b =
      Onton.Orchestrator.graph b.orchestrator
      |> Onton_core.Graph.all_patch_ids
      |> List.sort ~compare:Patch_id.compare
    in
    List.equal Patch_id.equal pids_a pids_b
  in
  let gameplan_eq = Gameplan.equal a.gameplan b.gameplan in
  let log_eq = Onton_core.Activity_log.equal a.activity_log b.activity_log in
  agents_eq && main_eq && graph_pids_eq && gameplan_eq && log_eq
  && List.equal String.equal a.applied_control_ids b.applied_control_ids

(* ---------- Snapshot generators ---------- *)

let gen_snapshot =
  QCheck2.Gen.(
    map2
      (fun (gameplan, main_branch, activity_log) applied_control_ids ->
        let orchestrator =
          Onton.Orchestrator.create ~patches:gameplan.Gameplan.patches
            ~main_branch
        in
        {
          Onton.Runtime.orchestrator;
          activity_log;
          gameplan;
          transcripts = Base.Hashtbl.create (module Patch_id);
          applied_control_ids;
        })
      (triple gen_gameplan gen_branch gen_activity_log)
      (list_size (int_range 0 5)
         (map (fun n -> "control-" ^ Int.to_string n) int)))

(* ========== Round-trip property tests ========== *)

let () =
  let metadata_snapshot_roundtrip =
    QCheck2.Test.make ~name:"snapshot preserves non-default gameplan metadata"
      ~count:100 gen_patch_agent_fully_populated (fun agent ->
        try
          let gameplan = gameplan_for_agent agent in
          let consumer_id =
            Patch_id.of_string (Patch_id.to_string agent.patch_id ^ "-consumer")
          in
          let patches =
            match gameplan.patches with
            | [ producer ] ->
                [
                  producer;
                  {
                    producer with
                    id = consumer_id;
                    branch = Branch.of_string "metadata-consumer";
                    dependencies = [ producer.id ];
                  };
                ]
            | _ -> assert false
          in
          let gameplan =
            {
              gameplan with
              patches;
              operational_considerations = "Preserve rollout compatibility";
              required_changes = "lib/public.mli: val capability : unit -> bool";
              ordering_constraints =
                [
                  {
                    Ordering_constraint.before = agent.patch_id;
                    after = consumer_id;
                    reason = "Serialize shared interface edits";
                  };
                ];
            }
          in
          let runtime =
            Onton.Runtime.create ~gameplan
              ~main_branch:(Branch.of_string "main") ()
          in
          let snap = Onton.Runtime.read runtime Fn.id in
          match
            Onton.Persistence.snapshot_of_yojson
              (Onton.Persistence.snapshot_to_yojson snap)
          with
          | Ok restored -> snapshots_equal snap restored
          | Error _ -> false
        with _ -> false)
  in
  let snapshot_roundtrip =
    QCheck2.Test.make ~name:"snapshot round-trip (fresh agents)" ~count:200
      gen_snapshot (fun snap ->
        try
          let json = Onton.Persistence.snapshot_to_yojson snap in
          match Onton.Persistence.snapshot_of_yojson json with
          | Ok snap' -> snapshots_equal snap snap'
          | Error _msg -> false
        with _ -> false)
  in
  let legacy_publication_claim =
    QCheck2.Test.make
      ~name:
        "legacy publication claim grants no publication or failure-budget \
         authority"
      ~count:100
      QCheck2.Gen.(pair (int_range 0 4) bool)
      (fun (failures, owner_missing) ->
        try
          let module A = Onton_core.Patch_agent in
          let a =
            A.create ~branch:(Branch.of_string "patch")
              (Patch_id.of_string "patch")
          in
          let a =
            List.fold (List.range 0 failures) ~init:a ~f:(fun a _ ->
                A.on_pr_discovery_failure a)
          in
          let json =
            match Onton.Persistence.patch_agent_to_yojson a with
            | `Assoc fields ->
                `Assoc
                  (("branch_published", `Bool true)
                  :: List.Assoc.remove fields ~equal:String.equal
                       "branch_published")
            | _ -> assert false
          in
          let json =
            if owner_missing then
              match json with
              | `Assoc fields ->
                  `Assoc
                    (List.Assoc.remove fields ~equal:String.equal
                       "branch_reconcile")
              | _ -> assert false
            else json
          in
          match
            Onton.Persistence.patch_agent_of_yojson
              ~main_branch:(Branch.of_string "main")
              ~gameplan:(gameplan_for_agent a) json
          with
          | Error _ -> false
          | Ok restored ->
              (match
                 Onton_core.Branch_reconcile.operation
                   restored.A.branch_reconcile
               with
                | Some op ->
                    Onton_core.Branch_reconcile.equal_purpose
                      op.Onton_core.Branch_reconcile.intent.purpose
                      Onton_core.Branch_reconcile.Verify_publication
                | None -> false)
              && (not (A.branch_published restored))
              && Int.equal restored.A.start_attempts_without_pr failures
              && Bool.equal (A.needs_intervention restored) (failures >= 2)
        with _ -> false)
  in
  let legacy_branch_migration =
    QCheck2.Test.make
      ~name:
        "legacy branch migration observes work and preserves unrelated state"
      ~count:200
      QCheck2.Gen.(
        map
          (fun agent ->
            Onton_core.Patch_agent.increment_no_commits_push_count agent)
          gen_patch_agent_fully_populated)
      (fun agent ->
        try
          let original = Onton.Persistence.patch_agent_to_yojson agent in
          let legacy =
            match original with
            | `Assoc fields ->
                `Assoc
                  (("expected_remote_head_oid", `String "legacy-head")
                  :: List.Assoc.remove fields ~equal:String.equal
                       "branch_reconcile")
            | _ -> assert false
          in
          let decode =
            Onton.Persistence.patch_agent_of_yojson
              ~main_branch:(Onton_core.Types.Branch.of_string "main")
              ~gameplan:(gameplan_for_agent agent)
          in
          match (decode original, decode legacy) with
          | Ok baseline, Ok migrated -> (
              let excluded =
                [
                  "branch_reconcile";
                  "conflict_noop_count";
                  "no_commits_push_count";
                  "push_failure_count";
                  "rebase_failure_count";
                  "expected_remote_head_oid";
                  "intervention_reason";
                ]
              in
              let unrelated a =
                match Onton.Persistence.patch_agent_to_yojson a with
                | `Assoc fields ->
                    `Assoc
                      (List.filter fields ~f:(fun (key, _) ->
                           not (List.mem excluded key ~equal:String.equal)))
                | _ -> assert false
              in
              let existing =
                baseline.has_session
                || Onton_core.Patch_agent.has_pr baseline
                || Option.is_some baseline.worktree_path
              in
              Yojson.Safe.equal (unrelated baseline) (unrelated migrated)
              && (match
                    Onton_core.Branch_reconcile.operation
                      migrated.branch_reconcile
                  with
                | None -> not existing
                | Some op ->
                    String.equal op.intent.base
                      (Onton_core.Types.Branch.to_string
                         (Option.value baseline.base_branch
                            ~default:(Onton_core.Types.Branch.of_string "main"))))
              && migrated.no_commits_push_count = 0
              && Option.is_none
                   (Onton_core.Patch_agent.expected_remote_head_oid migrated)
              && Bool.equal existing
                   (Onton_core.Branch_reconcile.is_pending
                      migrated.branch_reconcile)
              &&
              match
                decode (Onton.Persistence.patch_agent_to_yojson migrated)
              with
              | Ok again -> Onton_core.Patch_agent.equal migrated again
              | Error _ -> false)
          | Error _, _ | _, Error _ -> false
        with _ -> false)
  in
  let readiness_requires_restored_revision_proof =
    QCheck2.Test.make
      ~name:"restored readiness flags cannot replace current head/base proof"
      ~count:200
      QCheck2.Gen.(pair bool bool)
      (fun (confirmed, legacy) ->
        try
          let module A = Onton_core.Patch_agent in
          let initial =
            A.create ~branch:(Branch.of_string "patch")
              (Patch_id.of_string "patch")
          in
          let initial = A.set_pr_number initial (Pr_number.of_int 7) in
          let initial = A.set_base_branch initial (Branch.of_string "main") in
          let initial = A.set_is_draft initial false in
          let initial = A.set_merge_ready initial true in
          let initial = A.set_checks_passing initial true in
          let initial =
            if confirmed then
              Onton_core_test_support.Forge_fixture.readiness_agent initial
            else initial
          in
          let rec legacy_json = function
            | `Assoc fields ->
                `Assoc
                  (List.filter_map fields ~f:(fun (key, value) ->
                       if String.equal key "confirmed_base" then None
                       else Some (key, legacy_json value)))
            | `List values -> `List (List.map values ~f:legacy_json)
            | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as
              json ->
                json
          in
          let json = Onton.Persistence.patch_agent_to_yojson initial in
          let json = if legacy then legacy_json json else json in
          match
            Onton.Persistence.patch_agent_of_yojson ~snapshot_version:2
              ~main_branch:(Branch.of_string "main")
              ~gameplan:(gameplan_for_agent initial)
              json
          with
          | Error _ -> false
          | Ok restored ->
              restored.A.merge_ready && restored.A.checks_passing
              && Bool.equal
                   (A.forge_revision_pair_confirmed restored)
                   (confirmed && not legacy)
              && Bool.equal
                   (A.is_approved restored
                      ~main_branch:(Branch.of_string "main"))
                   (confirmed && not legacy)
              && Bool.equal
                   (A.should_request_review
                      (A.set_review_decision restored (Some "REVIEW_REQUIRED"))
                      ~main_branch:(Branch.of_string "main"))
                   (confirmed && not legacy)
        with _ -> false)
  in
  let legacy_conflict_claim =
    QCheck2.Test.make
      ~name:
        "legacy conflict invalidates readiness without fabricating owner \
         evidence"
      ~count:100 QCheck2.Gen.bool (fun owner_conflict ->
        try
          let module A = Onton_core.Patch_agent in
          let module B = Onton_core.Branch_reconcile in
          let initial =
            A.create ~branch:(Branch.of_string "patch")
              (Patch_id.of_string "patch")
          in
          let initial =
            if owner_conflict then
              Onton_core_test_support.Conflict_fixture.agent initial
            else initial
          in
          let initial = A.set_merge_ready initial true in
          let gameplan = gameplan_for_agent initial in
          let decode json =
            Onton.Persistence.patch_agent_of_yojson ~snapshot_version:2
              ~main_branch:(Branch.of_string "main") ~gameplan json
          in
          match Onton.Persistence.patch_agent_to_yojson initial with
          | `Assoc fields ->
              List.for_all [ false; true ] ~f:(fun legacy_claim ->
                  match
                    decode
                      (`Assoc (("has_conflict", `Bool legacy_claim) :: fields))
                  with
                  | Error _ -> false
                  | Ok restored -> (
                      B.equal initial.branch_reconcile restored.branch_reconcile
                      && Bool.equal (A.has_conflict restored) owner_conflict
                      && (if legacy_claim then
                            (not restored.merge_ready)
                            && restored.mergeability_unknown
                          else
                            Bool.equal restored.merge_ready initial.merge_ready)
                      &&
                      match
                        decode
                          (Onton.Persistence.patch_agent_to_yojson restored)
                      with
                      | Ok again -> A.equal restored again
                      | Error _ -> false))
          | _ -> false
        with _ -> false)
  in
  let base_evidence_roundtrip =
    QCheck2.Test.make
      ~name:"base evidence survives restart but legacy base names grant none"
      ~count:200 QCheck2.Gen.string (fun label ->
        try
          let module A = Onton_core.Patch_agent in
          let module B = Onton_core.Branch_reconcile in
          let module F = Onton_core_test_support.Publication_fixture in
          let base = Branch.of_string ("base/" ^ F.sha label) in
          let initial =
            A.create ~branch:(Branch.of_string "patch")
              (Patch_id.of_string "patch")
          in
          let initial = A.set_pr_number initial (Pr_number.of_int 1) in
          let initial = A.set_base_branch initial base in
          let integrated = F.reconciled_agent ~base initial in
          let gameplan = gameplan_for_agent initial in
          let decode ~snapshot_version json =
            Onton.Persistence.patch_agent_of_yojson ~snapshot_version
              ~main_branch:(Branch.of_string "main") ~gameplan json
          in
          let json = Onton.Persistence.patch_agent_to_yojson integrated in
          let fields =
            match json with `Assoc fields -> fields | _ -> assert false
          in
          let legacy =
            `Assoc
              (("branch_rebased_onto", `String (Branch.to_string base))
              :: List.filter fields ~f:(fun (key, _) ->
                  not (String.equal key "branch_reconcile")))
          in
          List.for_all [ 1; 2 ] ~f:(fun snapshot_version ->
              match decode ~snapshot_version json with
              | Error _ -> false
              | Ok restored ->
                  A.equal integrated restored
                  && Option.equal Branch.equal
                       (A.branch_rebased_onto restored)
                       (Some base))
          &&
          match decode ~snapshot_version:1 legacy with
          | Error _ -> false
          | Ok restored ->
              Option.is_none (A.branch_rebased_onto restored)
              && List.is_empty (B.integrations restored.A.branch_reconcile)
        with _ -> false)
  in
  let obsolete_branch_fields_ignored =
    QCheck2.Test.make
      ~name:"obsolete branch fields cannot alter restored owner or intervention"
      ~count:300
      QCheck2.Gen.(
        triple gen_patch_agent_fully_populated int
          (oneof_list
             [
               "push_failure_count";
               "rebase_failure_count";
               "conflict_noop_count";
               "branch_rebased_onto";
             ]))
      (fun (agent, count, field) ->
        try
          let module A = Onton_core.Patch_agent in
          let module B = Onton_core.Branch_reconcile in
          let pending, _ =
            A.reconcile_branch agent
              (B.Request
                 {
                   base = "main";
                   policy = B.Rewrite;
                   purpose = B.Reconcile_base;
                 })
          in
          let stopped =
            match B.pending pending.A.branch_reconcile with
            | None -> assert false
            | Some command ->
                fst
                  (A.reconcile_branch pending
                     (B.Result
                        {
                          token = command.B.token;
                          at = 100.;
                          result = B.Needs_diagnosis "permission_denied";
                        }))
          in
          List.for_all [ agent; pending; stopped ] ~f:(fun agent ->
              let json = Onton.Persistence.patch_agent_to_yojson agent in
              let gameplan = gameplan_for_agent agent in
              let fields =
                match json with `Assoc fields -> fields | _ -> assert false
              in
              List.for_all [ 1; 2 ] ~f:(fun snapshot_version ->
                  let decode json =
                    Onton.Persistence.patch_agent_of_yojson ~snapshot_version
                      ~main_branch:(Branch.of_string "main") ~gameplan json
                  in
                  List.for_all
                    [
                      `Int count;
                      `String "corrupt";
                      `String (String.make 40 'a');
                      `Null;
                    ]
                    ~f:(fun legacy ->
                      match
                        ( decode json,
                          decode (`Assoc ((field, legacy) :: fields)) )
                      with
                      | Ok baseline, Ok restored ->
                          A.equal baseline restored
                          && (Option.is_none
                                (B.operation agent.A.branch_reconcile)
                             || B.equal agent.A.branch_reconcile
                                  restored.A.branch_reconcile)
                      | Error _, _ | _, Error _ -> false)))
        with _ -> false)
  in
  let v2_requires_branch_checkpoint =
    QCheck2.Test.make ~name:"v2 rejects absent or null branch checkpoint"
      ~count:100 gen_patch_agent_fully_populated (fun agent ->
        let gameplan = gameplan_for_agent agent in
        let json = Onton.Persistence.patch_agent_to_yojson agent in
        let without, null_checkpoint =
          match json with
          | `Assoc fields ->
              let fields =
                List.Assoc.remove fields ~equal:String.equal "branch_reconcile"
              in
              (`Assoc fields, `Assoc (("branch_reconcile", `Null) :: fields))
          | _ -> (json, json)
        in
        let decode json =
          Onton.Persistence.patch_agent_of_yojson ~snapshot_version:2
            ~main_branch:(Branch.of_string "main") ~gameplan json
        in
        Result.is_error (decode without)
        && Result.is_error (decode null_checkpoint))
  in
  let legacy_architecture_snapshot =
    QCheck2.Test.make
      ~name:"legacy string architecture resumes without losing snapshot state"
      ~count:100
      QCheck2.Gen.(
        pair gen_snapshot (oneof [ return ""; return " \n\t"; string ]))
      (fun (snap, text) ->
        try
          let json =
            match Onton.Persistence.snapshot_to_yojson snap with
            | `Assoc fields ->
                `Assoc
                  (List.map fields ~f:(function
                    | "gameplan", `Assoc fields ->
                        ( "gameplan",
                          `Assoc
                            (("architecture_design", `String text)
                            :: List.Assoc.remove fields ~equal:String.equal
                                 "architecture_design") )
                    | field -> field))
            | _ -> assert false
          in
          let architecture_design =
            if String.is_empty (String.strip text) then None
            else Some Architecture_design.{ summary = text; decisions = [] }
          in
          let expected =
            { snap with gameplan = { snap.gameplan with architecture_design } }
          in
          match Onton.Persistence.snapshot_of_yojson json with
          | Error _ -> false
          | Ok restored -> snapshots_equal expected restored
        with _ -> false)
  in
  let legacy_feature_fields_default_false =
    QCheck2.Test.make
      ~name:"legacy snapshots without feature fields resume with false"
      ~count:100 gen_snapshot (fun snap ->
        try
          let json = Onton.Persistence.snapshot_to_yojson snap in
          let json =
            match json with
            | `Assoc fields ->
                `Assoc
                  (List.map fields ~f:(fun (key, value) ->
                       if not (String.equal key "orchestrator") then (key, value)
                       else
                         match value with
                         | `Assoc orch_fields ->
                             let orch_fields =
                               List.filter_map orch_fields ~f:(fun (name, v) ->
                                   if String.equal name "promotion_claimed" then
                                     None
                                   else if String.equal name "agents" then
                                     let agents =
                                       match v with
                                       | `Assoc agents ->
                                           `Assoc
                                             (List.map agents
                                                ~f:(fun (id, agent) ->
                                                  match agent with
                                                  | `Assoc fields ->
                                                      ( id,
                                                        `Assoc
                                                          (List.filter fields
                                                             ~f:(fun
                                                                 (field, _) ->
                                                               not
                                                                 (String.equal
                                                                    field
                                                                    "branch_published")))
                                                      )
                                                  | other -> (id, other)))
                                       | other -> other
                                     in
                                     Some (name, agents)
                                   else Some (name, v))
                             in
                             (key, `Assoc orch_fields)
                         | other -> (key, other)))
            | other -> other
          in
          match Onton.Persistence.snapshot_of_yojson json with
          | Ok restored ->
              (not (Onton.Orchestrator.promotion_claimed restored.orchestrator))
              && List.for_all
                   (Onton.Orchestrator.all_agents restored.orchestrator)
                   ~f:(fun a -> not (Onton_core.Patch_agent.branch_published a))
          | Error _ -> false
        with _ -> false)
  in
  let applied_control_ids_decode =
    QCheck2.Test.make
      ~name:"snapshot rejects malformed control IDs and defaults missing IDs"
      ~count:50 gen_snapshot (fun snap ->
        try
          match Onton.Persistence.snapshot_to_yojson snap with
          | `Assoc fields ->
              let fields =
                List.filter fields ~f:(fun (key, _) ->
                    not (String.equal key "applied_control_ids"))
              in
              let decode value =
                Onton.Persistence.snapshot_of_yojson
                  (`Assoc (("applied_control_ids", value) :: fields))
              in
              (match Onton.Persistence.snapshot_of_yojson (`Assoc fields) with
                | Ok restored -> List.is_empty restored.applied_control_ids
                | Error _ -> false)
              && Result.is_error (decode `Null)
              && Result.is_error (decode (`String "id"))
              && Result.is_error
                   (decode (`List [ `String "committed"; `Int 42 ]))
          | _ -> false
        with _ -> false)
  in
  let applied_control_ids_restore_window =
    QCheck2.Test.make ~name:"snapshot restores only recent control IDs" ~count:1
      gen_snapshot (fun snap ->
        try
          let limit = Onton_core.Control_command.max_retained_ids in
          match Onton.Persistence.snapshot_to_yojson snap with
          | `Assoc fields -> (
              let fields =
                List.filter fields ~f:(fun (key, _) ->
                    not (String.equal key "applied_control_ids"))
              in
              let ids =
                List.init (limit + 2) ~f:(fun n -> `String (Int.to_string n))
              in
              match
                Onton.Persistence.snapshot_of_yojson
                  (`Assoc (("applied_control_ids", `List ids) :: fields))
              with
              | Ok restored ->
                  List.length restored.applied_control_ids = limit
                  && Option.equal String.equal
                       (List.last restored.applied_control_ids)
                       (Some (Int.to_string (limit - 1)))
              | Error _ -> false)
          | _ -> false
        with _ -> false)
  in
  let activity_log_roundtrip =
    QCheck2.Test.make ~name:"activity_log round-trip" ~count:200
      gen_activity_log (fun log ->
        try
          let gameplan =
            Gameplan.
              {
                project_name = "t";
                repo_owner = "";
                repo_name = "";
                problem_statement = "t";
                architecture_design = None;
                solution_summary = "t";
                final_state_spec = "";
                patches = [];
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
          in
          let orchestrator =
            Onton.Orchestrator.create ~patches:[]
              ~main_branch:(Branch.of_string "main")
          in
          let snap =
            {
              Onton.Runtime.orchestrator;
              activity_log = log;
              gameplan;
              transcripts = Base.Hashtbl.create (module Patch_id);
              applied_control_ids = [];
            }
          in
          let json = Onton.Persistence.snapshot_to_yojson snap in
          match Onton.Persistence.snapshot_of_yojson json with
          | Ok snap' -> Onton_core.Activity_log.equal log snap'.activity_log
          | Error _msg -> false
        with _ -> false)
  in
  let snapshot_json_structure =
    QCheck2.Test.make ~name:"snapshot JSON has version field" ~count:100
      gen_snapshot (fun snap ->
        try
          let json = Onton.Persistence.snapshot_to_yojson snap in
          match json with
          | `Assoc fields ->
              List.exists fields ~f:(fun (k, v) ->
                  String.equal k "version" && Yojson.Safe.equal v (`Int 2))
          | _ -> false
        with _ -> false)
  in
  let legacy_backup_bytes =
    QCheck2.Test.make
      ~name:
        "legacy upgrade preserves original backup bytes across repeated loads \
         and v2 save" ~count:30 gen_snapshot (fun snap ->
        try
          let path = Stdlib.Filename.temp_file "onton_legacy_bytes_" ".json" in
          let backup = path ^ ".pre-v2" in
          let write content =
            let channel = Stdlib.open_out_bin path in
            Stdlib.Fun.protect
              ~finally:(fun () -> Stdlib.close_out_noerr channel)
              (fun () -> Stdlib.output_string channel content)
          in
          let read path =
            let channel = Stdlib.open_in_bin path in
            Stdlib.Fun.protect
              ~finally:(fun () -> Stdlib.close_in_noerr channel)
              (fun () -> Stdlib.In_channel.input_all channel)
          in
          Stdlib.Fun.protect
            ~finally:(fun () ->
              List.iter [ path; backup ] ~f:(fun file ->
                  try Stdlib.Sys.remove file with _ -> ()))
            (fun () ->
              let legacy =
                match Onton.Persistence.snapshot_to_yojson snap with
                | `Assoc fields ->
                    `Assoc
                      (("version", `Int 1)
                      :: List.filter fields ~f:(fun (key, _) ->
                          not (String.equal key "version")))
                | _ -> failwith "snapshot must be an object"
              in
              let original =
                " \n" ^ Yojson.Safe.pretty_to_string legacy ^ "\n\n"
              in
              write original;
              let restored =
                match Onton.Persistence.load ~path with
                | Ok value -> value
                | Error reason -> failwith reason
              in
              assert (String.equal (read path) original);
              assert (String.equal (read backup) original);
              (* A second valid legacy input must not replace the first backup. *)
              write (original ^ " \n");
              assert (Result.is_ok (Onton.Persistence.load ~path));
              assert (String.equal (read backup) original);
              assert (
                Result.is_ok (Onton.Persistence.save_snapshot ~path restored));
              assert (
                Poly.equal
                  (Onton_core.Json.int_field "version"
                     (Yojson.Safe.from_string (read path)))
                  (Some 2));
              assert (Result.is_ok (Onton.Persistence.load ~path));
              String.equal (read backup) original)
        with exn -> QCheck2.Test.fail_report (Exn.to_string exn))
  in
  let file_roundtrip =
    QCheck2.Test.make ~name:"snapshot file I/O round-trip" ~count:50
      gen_snapshot (fun snap ->
        try
          let path = Stdlib.Filename.temp_file "onton_test_" ".json" in
          Stdlib.Fun.protect
            ~finally:(fun () -> try Stdlib.Sys.remove path with _ -> ())
            (fun () ->
              match Onton.Persistence.save ~path snap with
              | Error _msg -> false
              | Ok () -> (
                  match Onton.Persistence.load ~path with
                  | Ok snap' -> snapshots_equal snap snap'
                  | Error _msg -> false))
        with _ -> false)
  in
  let patch_agent_roundtrip_fully_populated =
    QCheck2.Test.make
      ~name:"patch_agent round-trip (ci_checks, addressed IDs, fallback)"
      ~count:200 gen_patch_agent_fully_populated (fun agent ->
        try
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          match
            Onton.Persistence.patch_agent_of_yojson
              ~main_branch:(Onton_core.Types.Branch.of_string "main")
              ~gameplan json
          with
          | Ok agent' -> Onton_core.Patch_agent.equal agent agent'
          | Error _msg -> false
        with _ -> false)
  in
  let native_stack_roundtrip =
    QCheck2.Test.make ~name:"native stack membership survives snapshot"
      ~count:100 gen_patch_agent_fully_populated (fun agent ->
        try
          let agent = Onton_core.Patch_agent.set_native_stack agent true in
          let agent = Onton_core.Patch_agent.set_native_stack agent false in
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          match
            Onton.Persistence.patch_agent_of_yojson
              ~main_branch:(Onton_core.Types.Branch.of_string "main")
              ~gameplan json
          with
          | Ok restored ->
              restored.Onton_core.Patch_agent.native_stack
              && restored.native_stack_absent_polls = 1
          | Error _ -> false
        with _ -> false)
  in
  let legacy_anchor_retention =
    QCheck2.Test.make
      ~name:"legacy anchor revisions migrate into owner retention only"
      ~count:100
      QCheck2.Gen.(
        quad gen_patch_agent_fully_populated
          (list_size (int_range 0 20) (int_range 0 100))
          bool (int_range 0 100))
      (fun (agent, values, owner_missing, scalar) ->
        try
          let revisions =
            List.map values ~f:(fun i -> Printf.sprintf "%040x" i)
          in
          let anchors =
            `List
              (List.map revisions ~f:(fun sha ->
                   `Assoc
                     [
                       ("base", `String "legacy-base");
                       ("sha", `String sha);
                       ("observed_at_remote", `Bool true);
                     ]))
          in
          let gameplan = gameplan_for_agent agent in
          let original =
            match Onton.Persistence.patch_agent_to_yojson agent with
            | `Assoc fields ->
                `Assoc
                  (if owner_missing then
                     List.Assoc.remove fields ~equal:String.equal
                       "branch_reconcile"
                   else fields)
            | _ -> failwith "agent object"
          in
          let baseline =
            Result.ok_or_failwith
              (Onton.Persistence.patch_agent_of_yojson
                 ~main_branch:(Branch.of_string "main") ~gameplan original)
          in
          let json =
            match original with
            | `Assoc fields ->
                `Assoc
                  (("anchor_history", anchors)
                  :: ( "branch_rebased_onto_sha",
                       `String (Printf.sprintf "%040x" scalar) )
                  :: fields)
            | _ -> failwith "agent object"
          in
          let restored =
            Result.ok_or_failwith
              (Onton.Persistence.patch_agent_of_yojson
                 ~main_branch:(Branch.of_string "main") ~gameplan json)
          in
          let owner = restored.Onton_core.Patch_agent.branch_reconcile in
          let encoded = Onton.Persistence.patch_agent_to_yojson restored in
          let roundtrip =
            Result.ok_or_failwith
              (Onton.Persistence.patch_agent_of_yojson
                 ~main_branch:(Branch.of_string "main") ~gameplan encoded)
          in
          Onton_core.Patch_agent.equal restored roundtrip
          && Option.is_none (Onton_core.Json.field "anchor_history" encoded)
          && Option.is_none
               (Onton_core.Json.field "branch_rebased_onto_sha" encoded)
          && List.for_all (Printf.sprintf "%040x" scalar :: revisions)
               ~f:(fun sha ->
                 List.exists
                   (Onton_core.Branch_reconcile.required_revisions owner)
                   ~f:(fun revision ->
                     String.equal sha
                       (Onton_core.Branch_reconcile.Commit.to_string revision)))
          && Option.equal Onton_core.Branch_reconcile.equal_operation
               (Onton_core.Branch_reconcile.operation owner)
               (Onton_core.Branch_reconcile.operation baseline.branch_reconcile)
        with _ -> false)
  in
  let pr_number_roundtrip =
    QCheck2.Test.make ~name:"pr_number survives round-trip" ~count:200
      gen_patch_agent_fully_populated (fun agent ->
        try
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          match
            Onton.Persistence.patch_agent_of_yojson
              ~main_branch:(Onton_core.Types.Branch.of_string "main")
              ~gameplan json
          with
          | Ok agent' ->
              Option.equal Pr_number.equal
                (Onton_core.Patch_agent.pr_number agent)
                (Onton_core.Patch_agent.pr_number agent')
          | Error _ -> false
        with _ -> false)
  in
  (* pr_status round-trip: every Patch_pr_status variant (Absent, Present,
     Missing) survives serialize -> deserialize through patch_agent_*_yojson. *)
  let pr_status_roundtrip =
    QCheck2.Test.make ~name:"pr_status survives round-trip (all 3 variants)"
      ~count:300
      QCheck2.Gen.(
        oneof
          [
            return Onton_core.Patch_pr_status.Absent;
            map
              (fun n -> Onton_core.Patch_pr_status.Present (Pr_number.of_int n))
              (int_range 1 9999);
            map
              (fun n -> Onton_core.Patch_pr_status.Missing (Pr_number.of_int n))
              (int_range 1 9999);
          ])
      (fun pr_status ->
        try
          let json = Onton_core.Patch_pr_status.yojson_of_t pr_status in
          match Onton_core.Patch_pr_status.t_of_yojson_compat json with
          | Ok pr_status' ->
              Onton_core.Patch_pr_status.equal pr_status pr_status'
          | Error _ -> false
        with _ -> false)
  in
  (* Legacy pr_number-only snapshot (no pr_status key) decodes correctly:
     null -> Absent, int -> Present. Missing cannot appear from legacy data. *)
  let legacy_pr_number_decodes_correctly =
    QCheck2.Test.make
      ~name:"legacy pr_number field decodes to Absent or Present" ~count:200
      gen_patch_agent_fully_populated (fun agent ->
        try
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          (* Remove pr_status; keep legacy pr_number to simulate an older
             snapshot. *)
          let json =
            match json with
            | `Assoc fields ->
                `Assoc
                  (List.filter fields ~f:(fun (k, _) ->
                       not (String.equal k "pr_status")))
            | other -> other
          in
          match
            Onton.Persistence.patch_agent_of_yojson
              ~main_branch:(Onton_core.Types.Branch.of_string "main")
              ~gameplan json
          with
          | Ok agent' ->
              (* Whatever the agent's original pr_status, the legacy-only
                 decode preserves the pr_number value and avoids Missing. *)
              let original_pr = Onton_core.Patch_agent.pr_number agent in
              let decoded_pr = Onton_core.Patch_agent.pr_number agent' in
              let not_missing =
                not (Onton_core.Patch_agent.is_pr_missing agent')
              in
              Option.equal Pr_number.equal original_pr decoded_pr && not_missing
          | Error _ -> false
        with _ -> false)
  in
  let missing_pr_number_defaults_none =
    QCheck2.Test.make ~name:"missing pr_number defaults to None" ~count:200
      gen_patch_agent_fully_populated (fun agent ->
        try
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          (* Remove both pr_number and pr_status from JSON to simulate a
             legacy snapshot that predates either field. *)
          let json =
            match json with
            | `Assoc fields ->
                `Assoc
                  (List.filter fields ~f:(fun (k, _) ->
                       (not (String.equal k "pr_number"))
                       && not (String.equal k "pr_status")))
            | other -> other
          in
          match
            Onton.Persistence.patch_agent_of_yojson
              ~main_branch:(Onton_core.Types.Branch.of_string "main")
              ~gameplan json
          with
          | Ok agent' ->
              Option.is_none (Onton_core.Patch_agent.pr_number agent')
          | Error _ -> false
        with _ -> false)
  in
  let legacy_merge_state_status_decodes_unknown =
    QCheck2.Test.make
      ~name:"legacy merge_state_status UNKNOWN decodes to mergeability_unknown"
      ~count:200
      QCheck2.Gen.(
        pair gen_patch_agent_fully_populated
          (oneof
             [
               return (Some "UNKNOWN");
               return (Some "CLEAN");
               return (Some "BLOCKED");
               return None;
             ]))
      (fun (agent, legacy_status) ->
        try
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          let json =
            match json with
            | `Assoc fields ->
                let fields =
                  List.filter fields ~f:(fun (k, _) ->
                      not (String.equal k "mergeability_unknown"))
                in
                let fields =
                  match legacy_status with
                  | Some status ->
                      ("merge_state_status", `String status) :: fields
                  | None -> fields
                in
                `Assoc fields
            | other -> other
          in
          match
            Onton.Persistence.patch_agent_of_yojson
              ~main_branch:(Onton_core.Types.Branch.of_string "main")
              ~gameplan json
          with
          | Ok agent' ->
              Bool.equal agent'.mergeability_unknown
                (Option.equal String.equal legacy_status (Some "UNKNOWN"))
          | Error _ -> false
        with _ -> false)
  in
  let missing_branch_falls_back_to_gameplan =
    QCheck2.Test.make
      ~name:"missing branch key falls back to gameplan patch branch" ~count:200
      gen_patch_agent_fully_populated (fun agent ->
        try
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          (* Remove branch from JSON to simulate v1 snapshot *)
          let json =
            match json with
            | `Assoc fields ->
                `Assoc
                  (List.filter fields ~f:(fun (k, _) ->
                       not (String.equal k "branch")))
            | other -> other
          in
          match
            Onton.Persistence.patch_agent_of_yojson
              ~main_branch:(Onton_core.Types.Branch.of_string "main")
              ~gameplan json
          with
          | Ok agent' -> Branch.equal agent.branch agent'.branch
          | Error _ -> false
        with _ -> false)
  in
  (* Ad-hoc snapshot: empty gameplan + agents added via add_agent. Verifies
     that the gameplan/agent mismatch check correctly handles ad-hoc patches. *)
  let adhoc_snapshot_roundtrip =
    QCheck2.Test.make ~name:"ad-hoc snapshot round-trip (no gameplan)" ~count:50
      QCheck2.Gen.(list_size (int_range 1 5) (int_range 1 9999))
      (fun pr_numbers ->
        try
          let gameplan =
            Gameplan.
              {
                project_name = "adhoc";
                repo_owner = "";
                repo_name = "";
                problem_statement = "";
                architecture_design = None;
                solution_summary = "";
                final_state_spec = "";
                patches = [];
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
          in
          let main_branch = Branch.of_string "main" in
          let orchestrator =
            Onton.Orchestrator.create ~patches:[] ~main_branch
          in
          let orchestrator =
            List.fold_left pr_numbers ~init:orchestrator ~f:(fun orch n ->
                let patch_id = Patch_id.of_string (Int.to_string n) in
                let pr_number = Pr_number.of_int n in
                let branch =
                  Branch.of_string ("feature/pr-" ^ Int.to_string n)
                in
                Onton.Orchestrator.add_agent
                  ~complexity:(Some (Int.rem n 3 + 1))
                  orch ~patch_id ~branch ~base_branch:main_branch ~pr_number)
          in
          let snap =
            {
              Onton.Runtime.orchestrator;
              activity_log = Onton_core.Activity_log.empty;
              gameplan;
              transcripts = Base.Hashtbl.create (module Patch_id);
              applied_control_ids = [];
            }
          in
          let json = Onton.Persistence.snapshot_to_yojson snap in
          match Onton.Persistence.snapshot_of_yojson json with
          | Ok snap' -> snapshots_equal snap snap'
          | Error _msg -> false
        with _ -> false)
  in
  (* Restore of a snapshot whose ad-hoc agents encode a stack (B's
     base_branch = A's branch) must re-infer the dep edge B→A so
     detect_rebases can fire on A's merge post-restart. Mirrors the
     real subsetpark-pantagruel PRs 118-on-119 case. *)
  let adhoc_stack_restore_infers_edge =
    QCheck2.Test.make ~name:"restore re-infers ad-hoc stack edge" ~count:1
      (QCheck2.Gen.return ()) (fun () ->
        try
          let gameplan =
            Gameplan.
              {
                project_name = "adhoc-stack";
                repo_owner = "";
                repo_name = "";
                problem_statement = "";
                architecture_design = None;
                solution_summary = "";
                final_state_spec = "";
                patches = [];
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
          in
          let main_branch = Branch.of_string "main" in
          let orch = Onton.Orchestrator.create ~patches:[] ~main_branch in
          (* Add A first (base = main, no edge) *)
          let pid_a = Patch_id.of_string "a" in
          let branch_a = Branch.of_string "feature-a" in
          let orch =
            Onton.Orchestrator.add_agent orch ~patch_id:pid_a ~branch:branch_a
              ~base_branch:main_branch ~pr_number:(Pr_number.of_int 100)
          in
          (* Seed A's persisted base_branch to main so it survives the
             round-trip. Without this A.base_branch stays None and the
             restore has no signal that A is a valid dep target. *)
          let orch =
            Onton.Orchestrator.set_base_branch orch pid_a main_branch
          in
          (* Add B stacked on A *)
          let pid_b = Patch_id.of_string "b" in
          let branch_b = Branch.of_string "feature-b" in
          let orch =
            Onton.Orchestrator.add_agent orch ~patch_id:pid_b ~branch:branch_b
              ~base_branch:branch_a ~pr_number:(Pr_number.of_int 101)
          in
          (* Seed B's persisted base_branch — this is what the poller would
             have done; without it the restore has no basis to infer. *)
          let orch = Onton.Orchestrator.set_base_branch orch pid_b branch_a in
          let snap =
            {
              Onton.Runtime.orchestrator = orch;
              activity_log = Onton_core.Activity_log.empty;
              gameplan;
              transcripts = Base.Hashtbl.create (module Patch_id);
              applied_control_ids = [];
            }
          in
          let json = Onton.Persistence.snapshot_to_yojson snap in
          match Onton.Persistence.snapshot_of_yojson json with
          | Error _ -> false
          | Ok snap' ->
              let g = Onton.Orchestrator.graph snap'.orchestrator in
              let deps_b = Onton_core.Graph.deps g pid_b in
              let dependents_a = Onton_core.Graph.dependents g pid_a in
              List.mem deps_b pid_a ~equal:Patch_id.equal
              && List.mem dependents_a pid_b ~equal:Patch_id.equal
        with _ -> false)
  in
  let intervention_snapshot =
    QCheck2.Test.make
      ~name:
        "snapshot publishes intervention and clears it on operator input or \
         merge"
      ~count:300
      QCheck2.Gen.(triple (int_range 0 6) bool bool)
      (fun (failures, human, merged) ->
        let module Agent = Onton_core.Patch_agent in
        let agent =
          Agent.create_adhoc ~complexity:None
            ~patch_id:(Patch_id.of_string "24")
            ~branch:(Branch.of_string "patch") ~pr_number:(Pr_number.of_int 24)
            ~max_ci_failures:10
        in
        let agent =
          List.fold (List.init failures ~f:Fn.id) ~init:agent ~f:(fun agent _ ->
              Agent.on_context_exhausted agent)
        in
        let agent =
          if human then
            Agent.enqueue
              (Agent.add_human_message agent "continue")
              Onton_core.Types.Operation_kind.Human
          else agent
        in
        let agent = if merged then Agent.mark_merged agent else agent in
        let json = Onton.Persistence.patch_agent_to_yojson agent in
        let expected =
          if failures >= 2 && (not human) && not merged then
            Some (`String "context_exhaustion_count>=2")
          else None
        in
        Poly.equal (Onton_core.Json.field "intervention_reason" json) expected)
  in
  let runnable_snapshot =
    QCheck2.Test.make
      ~name:"snapshot preserves runnable start until terminal merge" ~count:100
      QCheck2.Gen.bool (fun merged ->
        let agent =
          Onton_core.Patch_agent.create ~branch:(Branch.of_string "patch")
            (Patch_id.of_string "1")
        in
        let gameplan = gameplan_for_agent agent in
        let orchestrator =
          Onton.Orchestrator.create ~patches:gameplan.Gameplan.patches
            ~main_branch:(Branch.of_string "main")
        in
        let orchestrator =
          if merged then
            Onton.Orchestrator.mark_merged orchestrator (Patch_id.of_string "1")
          else orchestrator
        in
        let snap =
          {
            Onton.Runtime.orchestrator;
            gameplan;
            activity_log = Onton_core.Activity_log.empty;
            transcripts = Base.Hashtbl.create (module Patch_id);
            applied_control_ids = [];
          }
        in
        Poly.equal
          (Onton_core.Json.bool_field "runnable"
             (Onton.Persistence.snapshot_to_yojson snap))
          (Some (not merged)))
  in
  let exit_code =
    QCheck_base_runner.run_tests
      [
        runnable_snapshot;
        intervention_snapshot;
        snapshot_roundtrip;
        metadata_snapshot_roundtrip;
        legacy_branch_migration;
        legacy_publication_claim;
        legacy_conflict_claim;
        readiness_requires_restored_revision_proof;
        obsolete_branch_fields_ignored;
        base_evidence_roundtrip;
        v2_requires_branch_checkpoint;
        legacy_architecture_snapshot;
        legacy_feature_fields_default_false;
        applied_control_ids_decode;
        applied_control_ids_restore_window;
        activity_log_roundtrip;
        snapshot_json_structure;
        file_roundtrip;
        legacy_backup_bytes;
        patch_agent_roundtrip_fully_populated;
        native_stack_roundtrip;
        legacy_anchor_retention;
        pr_number_roundtrip;
        pr_status_roundtrip;
        legacy_pr_number_decodes_correctly;
        missing_pr_number_defaults_none;
        legacy_merge_state_status_decodes_unknown;
        missing_branch_falls_back_to_gameplan;
        adhoc_snapshot_roundtrip;
        adhoc_stack_restore_infers_edge;
      ]
  in
  if exit_code <> 0 then Stdlib.exit exit_code

let () = Stdlib.print_endline "all persistence property tests passed"
