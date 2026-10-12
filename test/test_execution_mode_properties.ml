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

let published_gameplan patches =
  let source =
    Gameplan.
      {
        project_name = "publication-feature";
        repo_owner = "test";
        repo_name = "test";
        problem_statement = "";
        architecture_design = None;
        solution_summary = "";
        final_state_spec = "";
        patches;
        publication = None;
        functional_changes = [];
        context_resources = [];
        reachability_traces = [];
        operational_considerations = "";
        required_changes = "";
        ordering_constraints = [];
        current_state_analysis = "";
        explicit_opinions = "";
        acceptance_criteria = [];
        open_questions = [];
      }
  in
  let get = function Ok x -> x | Error e -> failwith e in
  let publication =
    get
      (Gameplan_publication.create ~directory:"gameplans"
         ~project_name:source.Gameplan.project_name ~yaml:false ~content:"{}")
  in
  get (Gameplan.publish source publication)

module G = QCheck2.Gen

let property name gen f =
  QCheck2.Test.make ~name ~count:500 gen (fun input ->
      try f input with _ -> false)

let ready_agent_without_publication () =
  let a = Patch_agent.create ~branch:(branch 2) (id 2) in
  Patch_agent.set_pr_body_delivered a true |> fun a ->
  Patch_agent.set_automerge_enabled a true |> fun a ->
  Patch_agent.set_base_branch a (branch 1) |> fun a ->
  Patch_agent.set_head_oid a (Some "checked-head") |> fun a ->
  Patch_agent.set_checks_passing a true

let ready_agent () =
  let module F = Onton_core_test_support.Publication_fixture in
  let candidate = F.sha "checked-head" in
  ready_agent_without_publication ()
  |> F.reconciled_agent ~base:(branch 1)
  |> F.agent ~candidate
  |> fun a ->
  Patch_agent.set_head_oid a (Some candidate) |> fun a ->
  Patch_agent.observe_publication_head a (Some candidate)

let integration a =
  Execution_mode.integration_ready mode ~construction_open:true
    ~ignore_inflight:false ~max_failures:3 ~terminal:(branch 1) a

let tests =
  [
    property "root availability follows busy and native-stack interleavings"
      (G.list (G.pair G.bool G.bool))
      (fun steps ->
        let root =
          Patch_agent.create ~branch:(branch 1) (id 1)
          |> Onton_core_test_support.Publication_fixture.agent
               ~candidate:
                 (Onton_core_test_support.Publication_fixture.sha "root")
        in
        let _, valid =
          List.fold steps ~init:(root, true)
            ~f:(fun (root, valid) (busy, native_stack) ->
              let root = Patch_agent.complete root in
              let root =
                if busy then
                  Patch_agent.respond
                    (Patch_agent.enqueue root Operation_kind.Ci)
                    Operation_kind.Ci
                else root
              in
              (* Two absent observations clear the native-stack debounce. *)
              let root = Patch_agent.set_native_stack root native_stack in
              let root = Patch_agent.set_native_stack root native_stack in
              ( root,
                valid
                && Bool.equal
                     (Execution_mode.integration_root_available root)
                     ((not busy) && not native_stack) ))
        in
        valid);
    property "publication bypasses PR creation only for feature descendants"
      (G.int_range 1 4) (fun n ->
        let module F = Onton_core_test_support.Publication_fixture in
        let a = Patch_agent.create ~branch:(branch n) (id n) in
        let a = F.agent ~candidate:(F.sha "published") a in
        Bool.equal (Execution_mode.branch_only_published mode a) (n <> 1)
        && not (Execution_mode.branch_only_published Execution_mode.mainline a));
    property "owner publication authorizes integration without legacy marker"
      G.unit (fun () -> integration (ready_agent ()));
    property "feature integration requires owner publication evidence" G.unit
      (fun () ->
        let a =
          ready_agent_without_publication ()
          |> Onton_core_test_support.Publication_fixture.reconciled_agent
               ~base:(branch 1)
        in
        let positive = integration a in
        let branch_reconcile =
          match Branch_reconcile.yojson_of_t a.Patch_agent.branch_reconcile with
          | `Assoc fields -> (
              Branch_reconcile.decode
                (`Assoc
                   (("publications", `List [])
                   :: ("observed_publication", `Null)
                   :: List.filter fields ~f:(fun (key, _) ->
                       not
                         (List.mem
                            [ "publications"; "observed_publication" ]
                            key ~equal:String.equal))))
              |> function
              | Ok state -> state
              | Error reason -> failwith reason)
          | _ -> failwith "expected checkpoint object"
        in
        let a =
          Patch_agent.restore ~branch_reconcile ~head_oid:a.Patch_agent.head_oid
            ~patch_id:a.Patch_agent.patch_id ~branch:a.Patch_agent.branch
            ~pr_status:a.Patch_agent.pr_status
            ~has_session:a.Patch_agent.has_session ~busy:a.Patch_agent.busy
            ~merged:a.Patch_agent.merged ~queue:a.Patch_agent.queue
            ~satisfies:a.Patch_agent.satisfies ~changed:a.Patch_agent.changed
            ~base_branch:a.Patch_agent.base_branch
            ~notified_base_branch:a.Patch_agent.notified_base_branch
            ~ci_failure_count:a.Patch_agent.ci_failure_count
            ~session_fallback:a.Patch_agent.session_fallback
            ~human_messages:a.Patch_agent.human_messages
            ~inflight_human_messages:a.Patch_agent.inflight_human_messages
            ~ci_checks:a.Patch_agent.ci_checks
            ~merge_ready:a.Patch_agent.merge_ready
            ~mergeability_unknown:a.Patch_agent.mergeability_unknown
            ~merge_queue_required:a.Patch_agent.merge_queue_required
            ~merge_queue_entry:a.Patch_agent.merge_queue_entry
            ~merge_commit_sha:a.Patch_agent.merge_commit_sha
            ~base_contains_merged_siblings:
              a.Patch_agent.base_contains_merged_siblings
            ~is_draft:a.Patch_agent.is_draft
            ~pr_body_delivered:a.Patch_agent.pr_body_delivered
            ~pr_body_artifact_miss_count:
              a.Patch_agent.pr_body_artifact_miss_count
            ~start_attempts_without_pr:a.Patch_agent.start_attempts_without_pr
            ~no_commits_push_count:a.Patch_agent.no_commits_push_count
            ~context_exhaustion_count:a.Patch_agent.context_exhaustion_count
            ~checks_passing:a.Patch_agent.checks_passing
            ~current_op:a.Patch_agent.current_op
            ~current_op_state:a.Patch_agent.current_op_state
            ~current_message_id:a.Patch_agent.current_message_id
            ~generation:a.Patch_agent.generation
            ~worktree_path:a.Patch_agent.worktree_path
            ~branch_blocked:a.Patch_agent.branch_blocked
            ~llm_session_id:a.Patch_agent.llm_session_id
            ~automerge_enabled:a.Patch_agent.automerge_enabled
            ~automerge_deadline:a.Patch_agent.automerge_deadline
            ~automerge_inflight:a.Patch_agent.automerge_inflight
            ~automerge_failure_count:a.Patch_agent.automerge_failure_count
            ~delivered_ci_run_ids:a.Patch_agent.delivered_ci_run_ids ()
        in
        positive
        && Option.equal Branch.equal
             (Patch_agent.branch_rebased_onto a)
             (Some (branch 1))
        && Option.is_none (Patch_agent.expected_remote_head_oid a)
        && (not (Patch_agent.branch_published a))
        && not (integration a));
    property "publication preserves the original root regardless of ID or order"
      (G.pair (G.int_range 1 1000) (G.shuffle_list [ 0; 1; 2 ]))
      (fun (n, order) ->
        let ps = [ patch n []; patch (n + 1) [ n ]; patch (n + 2) [ n + 1 ] ] in
        let ps = List.map order ~f:(List.nth_exn ps) in
        let gp = published_gameplan ps in
        match Execution_mode.infer_gameplan gp with
        | Error _ -> false
        | Ok m ->
            Option.equal Patch_id.equal (Execution_mode.root m) (Some (id n))
            && (not (Execution_mode.is_descendant m (id 0)))
            && (not (Execution_mode.is_root m (id 0)))
            && Execution_mode.is_descendant m (id (n + 1))
            && Branch.equal
                 (Execution_mode.terminal m (id 0) ~branch_of ~main)
                 main
            && Branch.equal
                 (Execution_mode.terminal m (id (n + 2)) ~branch_of ~main)
                 (branch n)
            && Result.equal Execution_mode.equal String.equal
                 (Execution_mode.restore_gameplan gp (Some (id n)))
                 (Ok m)
            && Result.is_error
                 (Execution_mode.restore_gameplan gp (Some (id 0))));
    property "published implementation requires a merged prerequisite"
      (G.pair G.bool G.bool) (fun (publication_merged, publication_present) ->
        let gp = published_gameplan patches in
        let graph = Graph.of_gameplan gp in
        match Execution_mode.infer_gameplan gp with
        | Error _ -> false
        | Ok m ->
            Bool.equal
              (Execution_mode.deps_satisfied m graph (id 1)
                 ~has_merged:(fun p ->
                   publication_merged && Patch_id.equal p (id 0))
                 ~has_pr:(fun _ -> publication_present))
              publication_merged);
    property "publication merge interleavings release only implementation root"
      (G.list G.bool) (fun observations ->
        let gp = published_gameplan patches in
        let graph = Graph.of_gameplan gp in
        match Execution_mode.infer_gameplan gp with
        | Error _ -> false
        | Ok m ->
            let _, valid =
              List.fold observations ~init:(false, true)
                ~f:(fun (merged, valid) observation ->
                  let merged = merged || observation in
                  let allowed n =
                    Execution_mode.deps_satisfied m graph (id n)
                      ~has_merged:(fun p -> merged && Patch_id.equal p (id 0))
                      ~has_pr:(fun _ -> false)
                  in
                  ( merged,
                    valid && Bool.equal (allowed 1) merged && not (allowed 2) ))
            in
            valid);
    property "publication cannot conceal invalid implementation roots" G.unit
      (fun () ->
        List.for_all
          [ []; [ patch 1 []; patch 2 [] ] ]
          ~f:(fun ps ->
            Result.is_error
              (Execution_mode.infer_gameplan (published_gameplan ps))));
    property
      "publication inference is total for malformed implementation graphs"
      (G.list_size (G.int_range 0 20)
         (G.pair (G.int_range 1 5)
            (G.list_size (G.int_range 0 10) (G.int_range 0 5))))
      (fun nodes ->
        let gp =
          published_gameplan (List.map nodes ~f:(fun (n, deps) -> patch n deps))
        in
        ignore
          (Execution_mode.infer_gameplan gp
            : (Execution_mode.t, string) Result.t);
        ignore
          (Execution_mode.restore_gameplan gp (Some (id 1))
            : (Execution_mode.t, string) Result.t);
        true);
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
    property "root readiness requires completed descendants and no integration"
      (G.triple G.bool G.bool G.bool)
      (fun (descendants_merged, pending, body_dirty) ->
        let root =
          Patch_agent.create ~branch:(branch 1) (id 1) |> fun a ->
          Patch_agent.set_pr_number a (Pr_number.of_int 1) |> fun a ->
          Patch_agent.set_head_oid a (Some "checked-head") |> fun a ->
          Patch_agent.set_checks_passing a true |> fun a ->
          Patch_agent.set_pr_body_delivered a true |> fun a ->
          Patch_agent.set_pr_number a (Pr_number.of_int 1) |> fun a ->
          Patch_agent.set_base_branch a main
          |> Onton_core_test_support.Forge_fixture.readiness_agent
        in
        let root =
          if body_dirty then Patch_agent.request_pr_body_refresh root else root
        in
        Bool.equal
          (Execution_mode.root_ready mode graph
             ~has_merged:(fun _ -> descendants_merged)
             ~pending_integrations:pending root)
          (descendants_merged && (not pending) && not body_dirty));
    property "feature publication requires confirmed revision identity"
      (G.option G.string) (fun observed ->
        let module F = Onton_core_test_support.Publication_fixture in
        let candidate = F.sha "final-integration" in
        let observed = Option.map observed ~f:F.sha in
        let a =
          ready_agent () |> fun a ->
          Patch_agent.set_head_oid a (Some (F.sha "before-first-integration"))
          |> F.agent ~candidate
        in
        Bool.equal
          (Execution_mode.observation_pending mode a observed)
          (not (Option.equal String.equal observed (Some candidate))));
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
    property "integration requires delivered implementation notes" G.bool
      (fun delivered ->
        Bool.equal
          (integration
             (Patch_agent.set_pr_body_delivered (ready_agent ()) delivered))
          delivered);
    property "published checked head and settled rebase required"
      (G.pair G.bool G.bool) (fun (pending, settled) ->
        let a =
          ready_agent () |> fun a ->
          if pending then
            let module F = Onton_core_test_support.Publication_fixture in
            F.agent ~candidate:(F.sha "new-head") a
          else a
        in
        let a =
          if settled then a else Patch_agent.set_base_branch a (branch 3)
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
