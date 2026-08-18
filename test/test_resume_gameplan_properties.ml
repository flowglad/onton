(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton_core
open Types

let pid = Patch_id.of_string

let patch ?(dependencies = []) id title =
  let id = pid id in
  {
    Patch.id;
    title;
    description = title ^ " description";
    branch = Branch.of_string ("resume/patch-" ^ Patch_id.to_string id);
    dependencies;
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

let gameplan patches =
  {
    Gameplan.project_name = "resume";
    repo_owner = "owner";
    repo_name = "repo";
    problem_statement = "problem";
    solution_summary = "solution";
    final_state_spec = "spec";
    patches;
    functional_changes = [];
    context_resources = [];
    reachability_traces = [];
    current_state_analysis = "state";
    explicit_opinions = "opinions";
    acceptance_criteria = [];
    open_questions = [];
  }

let runtime_patch ?(dependencies = []) id title =
  let patch_id = pid id in
  {
    (patch ~dependencies id title) with
    Patch.branch = Gameplan.branch_of_id (gameplan []) patch_id;
  }

let find_patch gameplan patch_id =
  List.find gameplan.Gameplan.patches ~f:(fun patch ->
      Patch_id.equal patch.Patch.id patch_id)

let reconcile ?(missing_patches = []) ?(activity_log = Activity_log.empty)
    ~loaded ~persisted () =
  Resume_gameplan.reconcile ~loaded ~persisted ~missing_patches ~activity_log

let prop_totality =
  QCheck2.Test.make ~name:"resume gameplan reconciliation is total" ~count:500
    QCheck2.Gen.(triple string string (list string))
    (fun (title, message, dependency_names) ->
      try
        let base = patch "1" "base" in
        let added = runtime_patch "add1" title in
        let loaded = gameplan [ base ] in
        let persisted = gameplan [ base; added ] in
        let dependencies = List.map dependency_names ~f:pid in
        let activity_log =
          Activity_log.add_event Activity_log.empty
            (Activity_log.Event.create ~timestamp:1.0 ~patch_id:(pid "add1")
               message)
        in
        ignore
          (reconcile ~loaded ~persisted ~activity_log
             ~missing_patches:
               [
                 {
                   Resume_gameplan.patch_id = pid "add1";
                   branch = added.branch;
                   dependencies;
                 };
               ]
             ()
            : Resume_gameplan.result);
        true
      with _ -> false)

let prop_preserves_snapshot_runtime_patch_exactly =
  QCheck2.Test.make
    ~name:"resume preserves snapshot-only runtime patch metadata exactly"
    ~count:200 QCheck2.Gen.string (fun description ->
      let base = patch "1" "base" in
      let added =
        {
          (runtime_patch ~dependencies:[ pid "1" ] "add1" "follow-up") with
          Patch.description;
        }
      in
      let result =
        reconcile ~loaded:(gameplan [ base ])
          ~persisted:(gameplan [ base; added ])
          ()
      in
      match find_patch result.gameplan (pid "add1") with
      | Some actual ->
          Patch.equal actual added
          && List.equal Resume_gameplan.equal_repair result.repairs
               [ Resume_gameplan.Preserved_snapshot_patch (pid "add1") ]
      | None -> false)

let prop_loaded_patch_wins_on_collision =
  QCheck2.Test.make ~name:"freshly loaded patch wins on id collision" ~count:1
    QCheck2.Gen.unit (fun () ->
      let loaded_add = runtime_patch "add1" "loaded" in
      let stale_add = runtime_patch "add1" "stale snapshot" in
      let result =
        reconcile ~loaded:(gameplan [ loaded_add ])
          ~persisted:(gameplan [ stale_add ]) ()
      in
      match find_patch result.gameplan (pid "add1") with
      | Some actual ->
          String.equal actual.Patch.title "loaded"
          && List.is_empty result.repairs
      | None -> false)

let prop_reconstructs_corrupted_runtime_patch =
  QCheck2.Test.make
    ~name:"corrupted addN patch is reconstructed from activity and graph"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let one = patch "1" "one" in
      let two = patch "2" "two" in
      let loaded = gameplan [ one; two ] in
      let patch_id = pid "add1" in
      let branch = Gameplan.branch_of_id loaded patch_id in
      let activity_log =
        Activity_log.add_event Activity_log.empty
          (Activity_log.Event.create ~timestamp:2.0 ~patch_id
             "Added patch add1 (depends on 1, 2, 2, gone) — More expressive \
              auth composition")
      in
      let result =
        reconcile ~loaded ~persisted:loaded ~activity_log
          ~missing_patches:
            [ { Resume_gameplan.patch_id; branch; dependencies = [ pid "1" ] } ]
          ()
      in
      match find_patch result.gameplan patch_id with
      | Some recovered ->
          String.equal recovered.Patch.title "More expressive auth composition"
          && String.equal recovered.description recovered.title
          && Branch.equal recovered.branch branch
          && List.equal Patch_id.equal recovered.dependencies
               [ pid "1"; pid "2" ]
          && List.equal Resume_gameplan.equal_repair result.repairs
               [ Resume_gameplan.Reconstructed_missing_patch patch_id ]
      | None -> false)

let prop_added_patch_event_message_encodes_patch_metadata =
  QCheck2.Test.make
    ~name:"added patch event encoding preserves id, dependencies, and title"
    ~count:200
    QCheck2.Gen.(pair string (list string))
    (fun (title, dependency_names) ->
      let dependencies = List.map dependency_names ~f:pid in
      let added = runtime_patch ~dependencies "add1" title in
      let encoded_dependencies =
        match dependency_names with
        | [] -> "no dependencies"
        | names -> "depends on " ^ String.concat names ~sep:", "
      in
      String.equal
        (Resume_gameplan.added_patch_event_message added)
        (Printf.sprintf "Added patch add1 (%s) — %s" encoded_dependencies title))

let prop_does_not_heal_adhoc_or_noncanonical_patch =
  QCheck2.Test.make
    ~name:"numeric ad-hoc and noncanonical addN agents are not synthesized"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let loaded = gameplan [ patch "1" "one" ] in
      let result =
        reconcile ~loaded ~persisted:loaded
          ~missing_patches:
            [
              {
                Resume_gameplan.patch_id = pid "123";
                branch = Branch.of_string "resume/patch-123";
                dependencies = [];
              };
              {
                Resume_gameplan.patch_id = pid "add1";
                branch = Branch.of_string "somewhere/else";
                dependencies = [];
              };
            ]
          ()
      in
      List.length result.gameplan.patches = 1 && List.is_empty result.repairs)

let prop_reconstructed_patches_can_depend_on_each_other =
  QCheck2.Test.make
    ~name:"reconstructed runtime patches retain dependencies on each other"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let loaded = gameplan [ patch "1" "one" ] in
      let add1 = pid "add1" in
      let add2 = pid "add2" in
      let result =
        reconcile ~loaded ~persisted:loaded
          ~missing_patches:
            [
              {
                Resume_gameplan.patch_id = add2;
                branch = Gameplan.branch_of_id loaded add2;
                dependencies = [ add1 ];
              };
              {
                Resume_gameplan.patch_id = add1;
                branch = Gameplan.branch_of_id loaded add1;
                dependencies = [ pid "1" ];
              };
            ]
          ()
      in
      match find_patch result.gameplan add2 with
      | Some recovered ->
          List.equal Patch_id.equal recovered.Patch.dependencies [ add1 ]
      | None -> false)

let prop_sanitizes_malformed_dependencies =
  QCheck2.Test.make
    ~name:"resume removes unknown, duplicate, self, and cyclic dependencies"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let one = patch ~dependencies:[ pid "gone" ] "1" "one" in
      let add1 =
        runtime_patch
          ~dependencies:[ pid "add1"; pid "1"; pid "1"; pid "add2" ]
          "add1" "first"
      in
      let add2 = runtime_patch ~dependencies:[ pid "add1" ] "add2" "second" in
      let result =
        reconcile ~loaded:(gameplan [ one ])
          ~persisted:(gameplan [ one; add1; add2 ])
          ()
      in
      match
        ( find_patch result.gameplan (pid "1"),
          find_patch result.gameplan (pid "add1"),
          find_patch result.gameplan (pid "add2") )
      with
      | Some one, Some add1, Some add2 ->
          List.is_empty one.Patch.dependencies
          && List.equal Patch_id.equal add1.dependencies [ pid "1" ]
          && List.is_empty add2.dependencies
      | _ -> false)

let prop_rejects_duplicate_snapshot_branches =
  QCheck2.Test.make
    ~name:"resume rejects snapshot runtime patches with duplicate branches"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let one = patch "1" "one" in
      let duplicate =
        { (runtime_patch "add1" "duplicate") with Patch.branch = one.branch }
      in
      let result =
        reconcile ~loaded:(gameplan [ one ])
          ~persisted:(gameplan [ one; duplicate ])
          ()
      in
      Option.is_none (find_patch result.gameplan (pid "add1"))
      && List.is_empty result.repairs)

let prop_gameplan_reconciliation_is_idempotent =
  QCheck2.Test.make ~name:"resume gameplan reconciliation is idempotent"
    ~count:200 QCheck2.Gen.string (fun title ->
      let base = patch "1" "base" in
      let added = runtime_patch ~dependencies:[ pid "1" ] "add1" title in
      let first =
        reconcile ~loaded:(gameplan [ base ])
          ~persisted:(gameplan [ base; added ])
          ()
      in
      let second =
        reconcile ~loaded:first.gameplan ~persisted:first.gameplan ()
      in
      Gameplan.equal first.gameplan second.gameplan)

let () =
  let tests =
    [
      prop_totality;
      prop_preserves_snapshot_runtime_patch_exactly;
      prop_loaded_patch_wins_on_collision;
      prop_reconstructs_corrupted_runtime_patch;
      prop_added_patch_event_message_encodes_patch_metadata;
      prop_does_not_heal_adhoc_or_noncanonical_patch;
      prop_reconstructed_patches_can_depend_on_each_other;
      prop_sanitizes_malformed_dependencies;
      prop_rejects_duplicate_snapshot_branches;
      prop_gameplan_reconciliation_is_idempotent;
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true tests in
  if exit_code <> 0 then Stdlib.exit exit_code
