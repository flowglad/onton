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
      design_decisions = "";
      patches = [];
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
