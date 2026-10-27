open Base
open Onton_core
open Types

let patch id deps =
  Patch.
    {
      id = Patch_id.of_string id;
      title = id;
      description = "";
      branch = Branch.of_string ("patch/" ^ id);
      dependencies = deps;
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
  let main = Branch.of_string "main" in
  let parent = patch "parent" [] in
  let child = patch "child" [ Patch_id.of_string "parent" ] in
  let graph = Graph.of_patches [ parent; child ] in
  let agents =
    List.map [ parent; child ] ~f:(fun p ->
        Patch_agent.create ~branch:p.Patch.branch p.Patch.id)
  in
  let view : Scheduler_decision.input =
    Scheduler_decision.{ graph; mode = Execution_mode.mainline; agents; main }
  in
  let child_id = child.Patch.id in
  assert (not (Scheduler_decision.dependencies_admitted view child_id));
  assert (Option.is_none (Scheduler_decision.for_patch view child_id));
  assert (Option.is_some (Scheduler_decision.for_patch view parent.Patch.id));
  let absent : Scheduler_decision.input =
    { view with Scheduler_decision.agents = [] }
  in
  assert (Option.is_none (Scheduler_decision.for_patch absent child_id));
  assert (Option.is_none (Scheduler_decision.selected_base absent child_id));
  let unknown = Patch_id.of_string "not-in-graph" in
  assert (Option.is_none (Scheduler_decision.for_patch view unknown));
  assert (Option.is_none (Scheduler_decision.selected_base view unknown));
  assert (not (Scheduler_decision.dependencies_admitted view unknown));
  let property =
    QCheck2.Test.make
      ~name:"planning is total and never changes owner projections" ~count:120
      QCheck2.Gen.(pair (int_range 1 8) (int_range 0 8))
      (fun (count, retained) ->
        try
          let ps =
            List.init count ~f:(fun n ->
                patch (Int.to_string n)
                  (if n = 0 then []
                   else [ Patch_id.of_string (Int.to_string (n - 1)) ]))
          in
          let graph = Graph.of_patches ps in
          let agents =
            List.take ps retained
            |> List.map ~f:(fun p ->
                Patch_agent.create ~branch:p.Patch.branch p.Patch.id)
          in
          let mode = Result.ok_or_failwith (Execution_mode.infer graph) in
          let view = Scheduler_decision.{ graph; mode; agents; main } in
          let before = List.map agents ~f:Patch_agent.sexp_of_t in
          let decisions = Scheduler_decision.plan view in
          List.length decisions = count
          && List.for_all decisions ~f:(fun (id, decision) ->
              Option.equal Poly.equal decision
                (Scheduler_decision.for_patch view id))
          && List.equal Sexp.equal before
               (List.map agents ~f:Patch_agent.sexp_of_t)
        with _ -> false)
  in
  let rc = QCheck_base_runner.run_tests ~verbose:true [ property ] in
  if rc <> 0 then Stdlib.exit rc;
  Stdio.printf "scheduler projection boundary: OK\n"
