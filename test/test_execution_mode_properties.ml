(* @archlint.module test
   @archlint.domain execution-mode *)
open Base
open Onton_core
open Types

let id n = Patch_id.of_string (Int.to_string n)
let branch n = Branch.of_string ("patch-" ^ Int.to_string n)
let main = Branch.of_string "main"

let patch n deps =
  Patch.
    {
      id = id n;
      branch = branch n;
      dependencies = List.map deps ~f:id;
      title = "patch";
      description = "";
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

let patches = [ patch 1 []; patch 2 [ 1 ]; patch 3 [ 1 ]; patch 4 [ 3 ] ]
let graph = Graph.of_patches patches

let mode =
  match Execution_mode.infer graph with Ok m -> m | Error e -> failwith e

let branch_of p = Branch.of_string ("patch-" ^ Patch_id.to_string p)

module G = QCheck2.Gen

let property name gen f =
  QCheck2.Test.make ~name ~count:500 gen (fun input ->
      try f input with _ -> false)

let ready_agent () =
  let a = Patch_agent.create ~branch:(branch 2) (id 2) in
  Patch_agent.mark_branch_published a |> fun a ->
  Patch_agent.set_automerge_enabled a true |> fun a ->
  Patch_agent.set_base_branch a (branch 1) |> fun a ->
  Patch_agent.set_branch_rebased_onto a (branch 1) |> fun a ->
  Patch_agent.set_head_oid a (Some "checked-head") |> fun a ->
  Patch_agent.set_checks_passing a true

let integration a =
  Execution_mode.integration_ready mode ~construction_open:true
    ~ignore_inflight:false ~max_failures:3 ~terminal:(branch 1) a

let tests =
  [
    property "root and descendant classification" (G.int_range 1 4) (fun n ->
        Bool.equal (Execution_mode.is_root mode (id n)) (Int.equal n 1)
        && Bool.equal
             (Execution_mode.is_descendant mode (id n))
             (not (Int.equal n 1)));
    property "terminal and open dependencies stop at root" G.unit (fun () ->
        Branch.equal
          (Execution_mode.terminal mode (id 2) ~branch_of ~main)
          (branch 1)
        && List.is_empty
             (Execution_mode.open_deps mode graph (id 2) ~has_merged:(fun _ ->
                  false))
        && List.equal Patch_id.equal
             (Execution_mode.open_deps mode graph (id 4) ~has_merged:(fun _ ->
                  false))
             [ id 3 ]);
    property "root readiness requires completed descendants" G.bool
      (fun pending ->
        let root = Patch_agent.create ~branch:(branch 1) (id 1) in
        not
          (Execution_mode.root_ready mode graph
             ~has_merged:(fun _ -> false)
             ~pending_integrations:pending root));
    property "feature publication defers only old or absent heads"
      (G.option G.string) (fun observed ->
        let a =
          ready_agent () |> fun a ->
          Patch_agent.set_head_oid a (Some "before-first-integration")
          |> fun a ->
          Patch_agent.set_expected_remote_head_oid a (Some "final-integration")
        in
        Bool.equal
          (Execution_mode.observation_pending mode a observed)
          (Option.is_none observed
          || Option.equal String.equal observed
               (Some "before-first-integration")));
    property "integration branch cannot be main" G.unit (fun () ->
        Result.is_error
          (Execution_mode.validate_terminal mode
             ~branch_of:(fun _ -> main)
             ~main)
        && Result.is_ok (Execution_mode.validate_terminal mode ~branch_of ~main));
    property "integration readiness excludes every outstanding operation"
      (G.pair G.bool G.bool) (fun (busy, queued) ->
        let a = ready_agent () in
        let a = if queued then Patch_agent.enqueue a Operation_kind.Ci else a in
        let a =
          if busy then
            Patch_agent.respond
              (Patch_agent.enqueue a Operation_kind.Human)
              Operation_kind.Human
          else a
        in
        Bool.equal (integration a) ((not busy) && not queued));
    property "published checked head and settled rebase required"
      (G.pair G.bool G.bool) (fun (pending, settled) ->
        let a =
          ready_agent () |> fun a ->
          Patch_agent.set_expected_remote_head_oid a
            (if pending then Some "new-head" else None)
        in
        let a =
          if settled then a
          else Patch_agent.set_branch_rebased_onto a (branch 3)
        in
        Bool.equal (integration a) ((not pending) && settled));
    property "failure cap and toggle boundaries"
      (G.pair (G.int_range 0 5) G.bool)
      (fun (failures, enabled) ->
        let a =
          List.fold (List.range 0 failures) ~init:(ready_agent ())
            ~f:(fun a _ -> Patch_agent.increment_automerge_failure_count a)
          |> fun a -> Patch_agent.set_automerge_enabled a enabled
        in
        Bool.equal (integration a) (enabled && failures < 3));
    property "sole root inferred independent of order" (G.shuffle_list patches)
      (fun ps ->
        match Execution_mode.infer (Graph.of_patches ps) with
        | Ok m ->
            Option.equal Patch_id.equal (Execution_mode.root m) (Some (id 1))
        | Error _ -> false);
    property "inference total for arbitrary dependency DAGs" (G.list G.bool)
      (fun edges ->
        let ps =
          List.mapi edges ~f:(fun i edge ->
              patch (i + 1) (if i > 0 && edge then [ i ] else []))
        in
        ignore
          (Execution_mode.infer (Graph.of_patches ps)
            : (Execution_mode.t, string) Result.t);
        true);
    property "terminal never main for descendants"
      (G.pair (G.int_range 2 4) (G.list (G.int_range 1 4)))
      (fun (n, merged) ->
        let has_merged p =
          List.mem merged
            (Int.of_string (Patch_id.to_string p))
            ~equal:Int.equal
        in
        let expected =
          Execution_mode.base mode graph (id n) ~has_merged ~branch_of ~main
        in
        Option.value_map expected ~default:true ~f:(fun b ->
            not (Branch.equal b main)));
    property "completion monotone across merge interleavings"
      (G.list (G.int_range 1 4))
      (fun ops ->
        let _, valid =
          List.fold ops
            ~init:(Set.empty (module Patch_id), true)
            ~f:(fun (merged, valid) n ->
              let complete merged =
                Execution_mode.descendants_complete mode graph
                  ~has_merged:(Set.mem merged)
              in
              let after = Set.add merged (id n) in
              (after, valid && ((not (complete merged)) || complete after)))
        in
        valid);
    property "root excluded from fan-in count but still requires publication"
      G.bool (fun root_present ->
        let g =
          Graph.of_patches [ patch 1 []; patch 2 [ 1 ]; patch 3 [ 1; 2 ] ]
        in
        Execution_mode.deps_satisfied mode g (id 3)
          ~has_merged:(fun _ -> false)
          ~has_pr:(fun p -> (not (Patch_id.equal p (id 1))) || root_present)
        |> Bool.equal root_present);
    property "construction freeze excludes every addition"
      (G.list (G.int_range 1 4))
      (fun deps ->
        not
          (Execution_mode.additions_allowed mode graph ~construction_open:false
             ~dependencies:(List.map deps ~f:id)));
    property "invalid roots and legacy mode" G.unit (fun () ->
        Result.is_error (Execution_mode.infer (Graph.of_patches []))
        && Result.is_error
             (Execution_mode.infer
                (Graph.of_patches [ patch 1 []; patch 2 [] ]))
        && Result.is_error (Execution_mode.restore graph (Some (id 2)))
        && Result.is_ok (Execution_mode.restore graph None));
    property "direct base promotes only to root" G.bool (fun parent_merged ->
        Option.equal Branch.equal
          (Execution_mode.base mode graph (id 4)
             ~has_merged:(fun p -> parent_merged && Patch_id.equal p (id 3))
             ~branch_of ~main)
          (Some (branch (if parent_merged then 1 else 3))));
  ]

let () = QCheck_base_runner.run_tests_main tests
