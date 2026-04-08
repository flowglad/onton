(* Regression test: Runtime.create with ~snapshot must not touch Eio primitives.

   Running this outside Eio_main.run crashes with Effect.Unhandled if anyone
   reintroduces Eio mutex/effect usage in the constructor. *)

let () =
  let open Onton.Types in
  let main_branch = Branch.of_string "main" in
  let gameplan =
    {
      Gameplan.project_name = "test";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }
  in
  let snapshot =
    {
      Onton.Runtime.orchestrator =
        Onton.Orchestrator.create ~patches:[] ~main_branch;
      activity_log = Onton.Activity_log.empty;
      gameplan;
      transcripts = Base.Hashtbl.create (module Onton.Types.Patch_id);
    }
  in
  let _rt = Onton.Runtime.create ~gameplan ~main_branch ~snapshot () in
  Printf.printf "PASS: Runtime.create with snapshot outside Eio\n"

(* Regression test: Runtime.create must override the snapshot's main_branch
   with the config-provided value, so stale snapshots don't silently use the
   wrong branch. *)
let () =
  let open Onton.Types in
  let old_branch = Branch.of_string "old-branch" in
  let new_branch = Branch.of_string "new-branch" in
  let gameplan =
    {
      Gameplan.project_name = "test";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }
  in
  let snapshot =
    {
      Onton.Runtime.orchestrator =
        Onton.Orchestrator.create ~patches:[] ~main_branch:old_branch;
      activity_log = Onton.Activity_log.empty;
      gameplan;
      transcripts = Base.Hashtbl.create (module Onton.Types.Patch_id);
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
