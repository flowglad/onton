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
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
      functional_changes = [];
      context_resources = [];
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

(* ---------- Snapshot generators ---------- *)

let gen_snapshot =
  QCheck2.Gen.(
    map3
      (fun gameplan main_branch activity_log ->
        let orchestrator =
          Onton.Orchestrator.create ~patches:gameplan.Gameplan.patches
            ~main_branch
        in
        {
          Onton.Runtime.orchestrator;
          activity_log;
          gameplan;
          transcripts = Base.Hashtbl.create (module Patch_id);
        })
      gen_gameplan gen_branch gen_activity_log)

(* ========== Round-trip property tests ========== *)

let () =
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
                solution_summary = "t";
                final_state_spec = "";
                patches = [];
                current_state_analysis = "";
                explicit_opinions = "";
                acceptance_criteria = [];
                open_questions = [];
                functional_changes = [];
                context_resources = [];
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
                  String.equal k "version" && Yojson.Safe.equal v (`Int 1))
          | _ -> false
        with _ -> false)
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
          match Onton.Persistence.patch_agent_of_yojson ~gameplan json with
          | Ok agent' -> Onton_core.Patch_agent.equal agent agent'
          | Error _msg -> false
        with _ -> false)
  in
  (* anchor_history is a new field; exercise it explicitly by pushing 1-8
     synthetic anchors onto an agent before round-tripping. The generic
     gen_patch_agent_fully_populated leaves history empty, which would
     trivially round-trip; this test confirms non-empty histories survive
     yojson_of_t / patch_agent_of_yojson identically. *)
  let gen_hex_sha =
    QCheck2.Gen.string_size
      ~gen:QCheck2.Gen.(oneof [ char_range '0' '9'; char_range 'a' 'f' ])
      (QCheck2.Gen.return 40)
  in
  let gen_anchor =
    QCheck2.Gen.map2
      (fun branch sha ->
        match
          Onton_core.Anchor.make ~base:branch ~sha ~observed_at_remote:true
        with
        | Some a -> a
        | None -> assert false)
      (QCheck2.Gen.map Branch.of_string
         (QCheck2.Gen.string_size
            ~gen:(QCheck2.Gen.char_range 'a' 'z')
            (QCheck2.Gen.int_range 1 20)))
      gen_hex_sha
  in
  let anchor_history_roundtrip =
    QCheck2.Test.make
      ~name:"patch_agent.anchor_history survives JSON round-trip" ~count:100
      QCheck2.Gen.(
        pair gen_patch_agent_fully_populated
          (list_size (int_range 1 8) gen_anchor))
      (fun (agent, anchors) ->
        try
          let agent =
            List.fold anchors ~init:agent
              ~f:Onton_core.Patch_agent.record_anchor
          in
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          match Onton.Persistence.patch_agent_of_yojson ~gameplan json with
          | Ok agent' ->
              Onton_core.Anchor_history.equal
                (Onton_core.Patch_agent.anchor_history agent)
                (Onton_core.Patch_agent.anchor_history agent')
          | Error _msg -> false
        with _ -> false)
  in
  (* Old snapshots predating anchor_history must load as empty history. *)
  let missing_anchor_history_defaults_empty =
    QCheck2.Test.make
      ~name:"missing anchor_history key defaults to Anchor_history.empty"
      ~count:50 gen_patch_agent_fully_populated (fun agent ->
        try
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          let json =
            match json with
            | `Assoc fields ->
                `Assoc
                  (List.filter fields ~f:(fun (k, _) ->
                       not (String.equal k "anchor_history")))
            | other -> other
          in
          match Onton.Persistence.patch_agent_of_yojson ~gameplan json with
          | Ok agent' ->
              Onton_core.Anchor_history.equal
                (Onton_core.Patch_agent.anchor_history agent')
                Onton_core.Anchor_history.empty
          | Error _ -> false
        with _ -> false)
  in
  let pr_number_roundtrip =
    QCheck2.Test.make ~name:"pr_number survives round-trip" ~count:200
      gen_patch_agent_fully_populated (fun agent ->
        try
          let gameplan = gameplan_for_agent agent in
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          match Onton.Persistence.patch_agent_of_yojson ~gameplan json with
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
          match Onton.Persistence.patch_agent_of_yojson ~gameplan json with
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
          match Onton.Persistence.patch_agent_of_yojson ~gameplan json with
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
          match Onton.Persistence.patch_agent_of_yojson ~gameplan json with
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
          match Onton.Persistence.patch_agent_of_yojson ~gameplan json with
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
                solution_summary = "";
                final_state_spec = "";
                patches = [];
                current_state_analysis = "";
                explicit_opinions = "";
                acceptance_criteria = [];
                open_questions = [];
                functional_changes = [];
                context_resources = [];
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
                Onton.Orchestrator.add_agent orch ~patch_id ~branch
                  ~base_branch:main_branch ~pr_number)
          in
          let snap =
            {
              Onton.Runtime.orchestrator;
              activity_log = Onton_core.Activity_log.empty;
              gameplan;
              transcripts = Base.Hashtbl.create (module Patch_id);
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
                solution_summary = "";
                final_state_spec = "";
                patches = [];
                current_state_analysis = "";
                explicit_opinions = "";
                acceptance_criteria = [];
                open_questions = [];
                functional_changes = [];
                context_resources = [];
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
  let exit_code =
    QCheck_base_runner.run_tests
      [
        snapshot_roundtrip;
        activity_log_roundtrip;
        snapshot_json_structure;
        file_roundtrip;
        patch_agent_roundtrip_fully_populated;
        anchor_history_roundtrip;
        missing_anchor_history_defaults_empty;
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
