(* @archlint.module test
   @archlint.domain orchestrator *)

(* Regression test: Runtime.create with ~snapshot must not touch Eio primitives.

   Running this outside Eio_main.run crashes with Effect.Unhandled if anyone
   reintroduces Eio mutex/effect usage in the constructor. *)

let () =
  let open Onton_core.Types in
  let main_branch = Branch.of_string "main" in
  let gameplan =
    {
      Gameplan.project_name = "test";
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
      reachability_traces = [];
    }
  in
  let snapshot =
    {
      Onton.Runtime.orchestrator =
        Onton.Orchestrator.create ~patches:[] ~main_branch;
      activity_log = Onton_core.Activity_log.empty;
      gameplan;
      transcripts = Base.Hashtbl.create (module Onton_core.Types.Patch_id);
    }
  in
  let _rt = Onton.Runtime.create ~gameplan ~main_branch ~snapshot () in
  Printf.printf "PASS: Runtime.create with snapshot outside Eio\n"

(* Regression test: Runtime.create must override the snapshot's main_branch
   with the config-provided value, so stale snapshots don't silently use the
   wrong branch. *)
let () =
  let open Onton_core.Types in
  let old_branch = Branch.of_string "old-branch" in
  let new_branch = Branch.of_string "new-branch" in
  let gameplan =
    {
      Gameplan.project_name = "test";
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
      reachability_traces = [];
    }
  in
  let snapshot =
    {
      Onton.Runtime.orchestrator =
        Onton.Orchestrator.create ~patches:[] ~main_branch:old_branch;
      activity_log = Onton_core.Activity_log.empty;
      gameplan;
      transcripts = Base.Hashtbl.create (module Onton_core.Types.Patch_id);
    }
  in
  let rt =
    Onton.Runtime.create ~gameplan ~main_branch:new_branch ~snapshot ()
  in
  let actual =
    Onton.Runtime.read rt (fun s ->
        Onton.Orchestrator.main_branch s.Onton.Runtime.orchestrator)
  in
  assert (Branch.equal actual new_branch);
  Printf.printf "PASS: Runtime.create overrides snapshot main_branch\n"

(* Regression test for runtime-added planned patches. The source gameplan is
   intentionally stale because these patches are snapshot-only by design.
   Resume must keep intact metadata and must also reconstruct a legacy snapshot
   that was already saved after the stale source gameplan overwrote it. *)
let () =
  let open Onton_core in
  let open Types in
  let main_branch = Branch.of_string "main" in
  let base_id = Patch_id.of_string "1" in
  let added_id = Patch_id.of_string "add1" in
  let base_patch =
    {
      Patch.id = base_id;
      title = "base";
      description = "base";
      branch = Branch.of_string "resume/patch-1";
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
  in
  let source_gameplan =
    {
      Gameplan.project_name = "resume";
      repo_owner = "";
      repo_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches = [ base_patch ];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
      functional_changes = [];
      context_resources = [];
      reachability_traces = [];
    }
  in
  let added_patch =
    {
      base_patch with
      Patch.id = added_id;
      title = "More expressive auth composition";
      description = "Full runtime-added patch description";
      branch = Gameplan.branch_of_id source_gameplan added_id;
      dependencies = [ base_id ];
    }
  in
  let orchestrator =
    Onton.Orchestrator.create ~patches:[ base_patch ] ~main_branch
    |> fun orchestrator ->
    Onton.Orchestrator.add_planned_patch orchestrator added_patch
      ~deps:[ base_id ]
  in
  let make_snapshot ~gameplan ~activity_log =
    {
      Onton.Runtime.orchestrator;
      activity_log;
      gameplan;
      transcripts = Base.Hashtbl.create (module Patch_id);
    }
  in
  let persisted_gameplan =
    { source_gameplan with Gameplan.patches = [ base_patch; added_patch ] }
  in
  let intact_runtime =
    Onton.Runtime.create ~gameplan:source_gameplan ~main_branch
      ~snapshot:
        (make_snapshot ~gameplan:persisted_gameplan
           ~activity_log:Activity_log.empty)
      ()
  in
  let intact_patch =
    Onton.Runtime.read intact_runtime (fun snapshot ->
        Base.List.find snapshot.Onton.Runtime.gameplan.Gameplan.patches
          ~f:(fun patch -> Patch_id.equal patch.Patch.id added_id))
  in
  assert (Base.Option.equal Patch.equal intact_patch (Some added_patch));
  let activity_log =
    Activity_log.add_event Activity_log.empty
      (Activity_log.Event.create ~timestamp:1.0 ~patch_id:added_id
         "Added patch add1 (depends on 1) — More expressive auth composition")
  in
  let repaired_runtime =
    Onton.Runtime.create ~gameplan:source_gameplan ~main_branch
      ~snapshot:(make_snapshot ~gameplan:source_gameplan ~activity_log)
      ()
  in
  let repaired_snapshot = Onton.Runtime.read repaired_runtime Base.Fn.id in
  let repaired_patch =
    Base.List.find repaired_snapshot.Onton.Runtime.gameplan.Gameplan.patches
      ~f:(fun patch -> Patch_id.equal patch.Patch.id added_id)
  in
  assert (Base.Option.is_some repaired_patch);
  assert (
    Base.List.equal Resume_gameplan.equal_repair
      (Onton.Runtime.resume_repairs repaired_runtime)
      [ Resume_gameplan.Reconstructed_missing_patch added_id ]);
  assert (
    try
      ignore
        (Onton.Patch_controller.plan_messages
           repaired_snapshot.Onton.Runtime.orchestrator
           ~patches:repaired_snapshot.Onton.Runtime.gameplan.Gameplan.patches);
      true
    with Invalid_argument _ -> false);
  Printf.printf "PASS: Runtime.create heals runtime-added patches on resume\n"
