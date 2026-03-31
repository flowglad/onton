open Base
open Onton.Types
open Onton_test_support.Test_generators

(** QCheck2 property-based tests for persistence round-trips.

    These tests verify that serialize → deserialize is the identity for all
    persisted types, using the generators from [Test_generators]. *)

(** Compare two snapshots field by field. Orchestrator.t is opaque without [eq],
    so we compare agents_map entries and main_branch. *)
let snapshots_equal (a : Onton.Runtime.snapshot) (b : Onton.Runtime.snapshot) =
  let agents_a = Onton.Orchestrator.agents_map a.orchestrator |> Map.to_alist in
  let agents_b = Onton.Orchestrator.agents_map b.orchestrator |> Map.to_alist in
  let agents_eq =
    List.equal
      (fun (ka, va) (kb, vb) ->
        Patch_id.equal ka kb && Onton.Patch_agent.equal va vb)
      agents_a agents_b
  in
  let main_eq =
    Branch.equal
      (Onton.Orchestrator.main_branch a.orchestrator)
      (Onton.Orchestrator.main_branch b.orchestrator)
  in
  let log_eq = Onton.Activity_log.equal a.activity_log b.activity_log in
  agents_eq && main_eq && log_eq

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
          match
            Onton.Persistence.snapshot_of_yojson ~gameplan:snap.gameplan json
          with
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
                problem_statement = "t";
                solution_summary = "t";
                design_decisions = "";
                patches = [];
                current_state_analysis = "";
                explicit_opinions = "";
                acceptance_criteria = [];
                open_questions = [];
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
          match Onton.Persistence.snapshot_of_yojson ~gameplan json with
          | Ok snap' -> Onton.Activity_log.equal log snap'.activity_log
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
                  match
                    Onton.Persistence.load ~path ~gameplan:snap.gameplan
                  with
                  | Ok snap' -> snapshots_equal snap snap'
                  | Error _msg -> false))
        with _ -> false)
  in
  let patch_agent_roundtrip_fully_populated =
    QCheck2.Test.make
      ~name:"patch_agent round-trip (ci_checks, addressed IDs, fallback)"
      ~count:200 gen_patch_agent_fully_populated (fun agent ->
        try
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          match Onton.Persistence.patch_agent_of_yojson json with
          | Ok agent' -> Onton.Patch_agent.equal agent agent'
          | Error _msg -> false
        with _ -> false)
  in
  let pr_number_roundtrip =
    QCheck2.Test.make ~name:"pr_number survives round-trip" ~count:200
      gen_patch_agent_fully_populated (fun agent ->
        try
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          match Onton.Persistence.patch_agent_of_yojson json with
          | Ok agent' ->
              Option.equal Pr_number.equal agent.pr_number agent'.pr_number
          | Error _ -> false
        with _ -> false)
  in
  let missing_pr_number_defaults_none =
    QCheck2.Test.make ~name:"missing pr_number defaults to None" ~count:200
      gen_patch_agent_fully_populated (fun agent ->
        try
          let json = Onton.Persistence.patch_agent_to_yojson agent in
          (* Remove pr_number from JSON to simulate legacy snapshot *)
          let json =
            match json with
            | `Assoc fields ->
                `Assoc
                  (List.filter fields ~f:(fun (k, _) ->
                       not (String.equal k "pr_number")))
            | other -> other
          in
          match Onton.Persistence.patch_agent_of_yojson json with
          | Ok agent' -> Option.is_none agent'.pr_number
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
                problem_statement = "";
                solution_summary = "";
                design_decisions = "";
                patches = [];
                current_state_analysis = "";
                explicit_opinions = "";
                acceptance_criteria = [];
                open_questions = [];
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
                Onton.Orchestrator.add_agent orch ~patch_id ~pr_number)
          in
          let snap =
            {
              Onton.Runtime.orchestrator;
              activity_log = Onton.Activity_log.empty;
              gameplan;
              transcripts = Base.Hashtbl.create (module Patch_id);
            }
          in
          let json = Onton.Persistence.snapshot_to_yojson snap in
          match Onton.Persistence.snapshot_of_yojson ~gameplan json with
          | Ok snap' -> snapshots_equal snap snap'
          | Error _msg -> false
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
        pr_number_roundtrip;
        missing_pr_number_defaults_none;
        adhoc_snapshot_roundtrip;
      ]
  in
  if exit_code <> 0 then Stdlib.exit exit_code

let () = Stdlib.print_endline "all persistence property tests passed"
