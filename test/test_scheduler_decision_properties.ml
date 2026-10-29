(* @archlint.module test
   @archlint.domain scheduler-decision *)

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
  let child_agent = List.nth_exn agents 1 in
  let child_id = child.Patch.id in
  let assert_plan expected input =
    let equal_decision (left_id, left) (right_id, right) =
      Patch_id.equal left_id right_id && Option.equal Poly.equal left right
    in
    let by_patch =
      List.sort ~compare:(fun (left, _) (right, _) ->
          Patch_id.compare left right)
    in
    assert (
      List.equal equal_decision
        (by_patch (Scheduler_decision.plan input))
        (by_patch expected))
  in
  assert (not (Scheduler_decision.dependencies_admitted view child_id));
  assert_plan
    [
      (parent.Patch.id, Some (Scheduler_decision.Start (parent.Patch.id, main)));
      (child_id, None);
    ]
    view;
  let human =
    Patch_agent.create_adhoc ~complexity:None ~patch_id:parent.Patch.id
      ~branch:parent.Patch.branch ~pr_number:(Pr_number.of_int 1)
      ~max_ci_failures:3
    |> fun a -> Patch_agent.enqueue a Operation_kind.Human
  in
  let human_view =
    { view with Scheduler_decision.agents = [ human; child_agent ] }
  in
  assert_plan
    [
      ( parent.Patch.id,
        Some
          (Scheduler_decision.Respond (parent.Patch.id, Operation_kind.Human))
      );
      (child_id, None);
    ]
    human_view;
  let rebasing =
    Patch_agent.create_adhoc ~complexity:None ~patch_id:parent.Patch.id
      ~branch:parent.Patch.branch ~pr_number:(Pr_number.of_int 1)
      ~max_ci_failures:3
    |> fun a ->
    Patch_agent.set_base_branch a main |> fun a ->
    Patch_agent.enqueue a Operation_kind.Rebase
  in
  assert_plan
    [
      (parent.Patch.id, Some (Scheduler_decision.Rebase (parent.Patch.id, main)));
      (child_id, None);
    ]
    { view with Scheduler_decision.agents = [ rebasing; child_agent ] };
  let reconciling, _ =
    Patch_agent.reconcile_branch
      (Patch_agent.create ~branch:parent.Patch.branch parent.Patch.id)
      (Branch_reconcile.Request
         Branch_reconcile.
           { base = "main"; policy = Rewrite; purpose = Reconcile_base })
  in
  let reconcile_decision =
    Option.map
      (Branch_reconcile.operation reconciling.Patch_agent.branch_reconcile)
      ~f:(fun operation ->
        Scheduler_decision.Reconcile_branch
          (parent.Patch.id, operation.Branch_reconcile.id))
  in
  assert (Option.is_some reconcile_decision);
  assert_plan
    [ (parent.Patch.id, reconcile_decision); (child_id, None) ]
    { view with Scheduler_decision.agents = [ reconciling; child_agent ] };
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
      ~name:"generated chains match an independent start/dependency oracle"
      ~count:120
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
          let decisions = Scheduler_decision.plan view in
          let expected =
            List.map (Graph.all_patch_ids graph) ~f:(fun id ->
                let decision =
                  if
                    retained >= count
                    && Patch_id.equal id (Patch_id.of_string "0")
                  then Some (Scheduler_decision.Start (id, main))
                  else None
                in
                (id, decision))
          in
          let by_patch =
            List.sort ~compare:(fun (left, _) (right, _) ->
                Patch_id.compare left right)
          in
          List.equal
            (fun (left_id, left) (right_id, right) ->
              Patch_id.equal left_id right_id
              && Option.equal Poly.equal left right)
            (by_patch decisions) (by_patch expected)
        with _ -> false)
  in
  let rc = QCheck_base_runner.run_tests ~verbose:true [ property ] in
  if rc <> 0 then Stdlib.exit rc;
  Stdio.printf "scheduler projection boundary: OK\n"
