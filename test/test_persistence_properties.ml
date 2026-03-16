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
    List.length agents_a = List.length agents_b
    && List.for_all2_exn agents_a agents_b ~f:(fun (ka, va) (kb, vb) ->
        Patch_id.equal ka kb && Onton.Patch_agent.equal va vb)
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
        { Onton.Runtime.orchestrator; activity_log; gameplan })
      gen_gameplan gen_branch gen_activity_log)

let gen_snapshot_with_varied_agents =
  QCheck2.Gen.(
    map3
      (fun gameplan main_branch activity_log ->
        let orchestrator =
          Onton.Orchestrator.create ~patches:gameplan.Gameplan.patches
            ~main_branch
        in
        (* Tick to start root patches, then complete them *)
        let orchestrator, _actions =
          Onton.Orchestrator.tick orchestrator
            ~patches:gameplan.Gameplan.patches
        in
        let orchestrator =
          List.fold gameplan.patches ~init:orchestrator ~f:(fun orch patch ->
              let agent = Onton.Orchestrator.agent orch patch.Patch.id in
              if agent.busy then Onton.Orchestrator.complete orch patch.Patch.id
              else orch)
        in
        { Onton.Runtime.orchestrator; activity_log; gameplan })
      gen_gameplan gen_branch gen_activity_log)

(* ========== Round-trip property tests ========== *)

let () =
  let snapshot_roundtrip =
    QCheck2.Test.make ~name:"snapshot round-trip (fresh agents)" ~count:200
      gen_snapshot (fun snap ->
        let json = Onton.Persistence.snapshot_to_yojson snap in
        match
          Onton.Persistence.snapshot_of_yojson ~gameplan:snap.gameplan json
        with
        | Ok snap' -> snapshots_equal snap snap'
        | Error msg -> failwith msg)
  in
  let snapshot_roundtrip_completed =
    QCheck2.Test.make ~name:"snapshot round-trip (completed agents)" ~count:200
      gen_snapshot_with_varied_agents (fun snap ->
        let json = Onton.Persistence.snapshot_to_yojson snap in
        match
          Onton.Persistence.snapshot_of_yojson ~gameplan:snap.gameplan json
        with
        | Ok snap' -> snapshots_equal snap snap'
        | Error msg -> failwith msg)
  in
  let activity_log_roundtrip =
    QCheck2.Test.make ~name:"activity_log round-trip" ~count:200
      gen_activity_log (fun log ->
        let gameplan =
          Gameplan.
            {
              project_name = "t";
              problem_statement = "t";
              solution_summary = "t";
              patches = [];
            }
        in
        let orchestrator =
          Onton.Orchestrator.create ~patches:[]
            ~main_branch:(Branch.of_string "main")
        in
        let snap =
          { Onton.Runtime.orchestrator; activity_log = log; gameplan }
        in
        let json = Onton.Persistence.snapshot_to_yojson snap in
        match Onton.Persistence.snapshot_of_yojson ~gameplan json with
        | Ok snap' -> Onton.Activity_log.equal log snap'.activity_log
        | Error msg -> failwith msg)
  in
  let snapshot_json_structure =
    QCheck2.Test.make ~name:"snapshot JSON has version field" ~count:100
      gen_snapshot (fun snap ->
        let json = Onton.Persistence.snapshot_to_yojson snap in
        match json with
        | `Assoc fields ->
            List.exists fields ~f:(fun (k, v) ->
                String.equal k "version" && Yojson.Safe.equal v (`Int 1))
        | _ -> false)
  in
  let file_roundtrip =
    QCheck2.Test.make ~name:"snapshot file I/O round-trip" ~count:50
      gen_snapshot (fun snap ->
        let path = Stdlib.Filename.temp_file "onton_test_" ".json" in
        Stdlib.Fun.protect
          ~finally:(fun () -> try Stdlib.Sys.remove path with _ -> ())
          (fun () ->
            match Onton.Persistence.save ~path snap with
            | Error msg -> failwith msg
            | Ok () -> (
                match Onton.Persistence.load ~path ~gameplan:snap.gameplan with
                | Ok snap' -> snapshots_equal snap snap'
                | Error msg -> failwith msg)))
  in
  let exit_code =
    QCheck_base_runner.run_tests
      [
        snapshot_roundtrip;
        snapshot_roundtrip_completed;
        activity_log_roundtrip;
        snapshot_json_structure;
        file_roundtrip;
      ]
  in
  if exit_code <> 0 then Stdlib.exit exit_code

let () = Stdlib.print_endline "all persistence property tests passed"
