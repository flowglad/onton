(* @archlint.module test
   @archlint.domain orchestrator *)
open Base
open Onton
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

let gp =
  Gameplan.
    {
      project_name = "feature-test";
      repo_owner = "test";
      repo_name = "test";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches;
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
      functional_changes = [];
      context_resources = [];
      publication = None;
      reachability_traces = [];
    }

let check label value = if not value then failwith label

let ready_branch orch n =
  Orchestrator.mark_branch_published orch (id n) |> fun o ->
  Orchestrator.set_automerge_enabled o (id n) true |> fun o ->
  Orchestrator.set_base_branch o (id n) (branch 1) |> fun o ->
  fst (Orchestrator.apply_rebase_result o (id n) Worktree.Noop (branch 1))
  |> fun o ->
  Patch_controller.apply_branch_observation o (id n)
    ~head_sha:("head" ^ Int.to_string n)
    ~checks:
      [
        Ci_check.
          {
            name = "test";
            conclusion = "success";
            details_url = None;
            description = None;
            started_at = None;
            app_id = None;
            check_suite_id = None;
            id = Some n;
          };
      ]

let ready_root orch =
  Orchestrator.set_pr_number orch (id 1) (Pr_number.of_int 1) |> fun o ->
  Orchestrator.set_is_draft o (id 1) true |> fun o ->
  Orchestrator.set_base_branch o (id 1) main |> fun o ->
  fst (Orchestrator.apply_rebase_result o (id 1) Worktree.Noop main) |> fun o ->
  Orchestrator.set_pr_body_delivered o (id 1) true |> fun o ->
  Orchestrator.set_checks_passing o (id 1) true |> fun o ->
  Orchestrator.set_head_oid o (id 1) (Some "root-head") |> fun o ->
  Orchestrator.set_mergeability_unknown o (id 1) false

let fresh () =
  Orchestrator.create ~patches ~main_branch:main |> fun o ->
  Orchestrator.set_execution_mode o mode |> ready_root

let publication_lifecycle () =
  let get = function Ok x -> x | Error e -> failwith e in
  let publication =
    get
      (Gameplan_publication.create ~directory:"gameplans"
         ~project_name:gp.Gameplan.project_name ~yaml:false ~content:"{}")
  in
  let gameplan = get (Gameplan.publish gp publication) in
  let mode = get (Execution_mode.infer_gameplan gameplan) in
  let runtime = Runtime.create ~gameplan ~main_branch:main () in
  Runtime.update_orchestrator runtime (fun o ->
      Orchestrator.set_execution_mode o mode);
  let orch () = Runtime.read runtime (fun s -> s.Runtime.orchestrator) in
  let starts () =
    Patch_controller.plan_actions (orch ()) ~patches:gameplan.Gameplan.patches
    |> List.filter_map ~f:(function
      | Orchestrator.Start (p, b) -> Some (p, b)
      | Orchestrator.Respond _ | Orchestrator.Rebase _ -> None)
  in
  let check_starts label expected =
    check label
      (List.equal
         (fun (p, b) (p', b') -> Patch_id.equal p p' && Branch.equal b b')
         (starts ()) expected)
  in
  let resume () =
    let snapshot =
      get
        (Persistence.snapshot_of_yojson
           (Runtime.read runtime Persistence.snapshot_to_yojson))
    in
    let mode =
      get
        (Execution_mode.restore_gameplan snapshot.Runtime.gameplan
           (Some (id 1)))
    in
    Runtime.update_orchestrator runtime (fun _ ->
        Orchestrator.set_execution_mode snapshot.Runtime.orchestrator mode)
  in
  check_starts "only publication starts against main" [ (id 0, main) ];
  Runtime.update_orchestrator runtime (fun o ->
      Orchestrator.set_pr_number o (id 0) (Pr_number.of_int 10) |> fun o ->
      Orchestrator.set_base_branch o (id 0) main |> fun o ->
      fst (Orchestrator.apply_rebase_result o (id 0) Worktree.Noop main)
      |> fun o ->
      Orchestrator.set_pr_body_delivered o (id 0) true |> fun o ->
      Orchestrator.set_checks_passing o (id 0) true);
  check "publication ready for review before implementation starts"
    (Patch_controller.ready_for_review (orch ()) (id 0));
  check "publication excluded from feature descendants"
    (not (Orchestrator.is_feature_descendant (orch ()) (id 0)));
  check_starts "ready publication cannot release implementation" [];
  Runtime.update_orchestrator runtime (fun o ->
      Orchestrator.set_worktree_path o (id 1) "/existing-root" |> fun o ->
      Orchestrator.fire o (Orchestrator.Start (id 1, main)));
  check "materialized root cannot bypass publication merge"
    (not (Orchestrator.agent (orch ()) (id 1)).Patch_agent.busy);
  resume ();
  check_starts "resume before publication merge remains blocked" [];
  Runtime.update_orchestrator runtime (fun o ->
      Orchestrator.mark_merged o (id 0));
  check_starts "publication merge releases original root against main"
    [ (id 1, main) ];
  resume ();
  check_starts "resume after publication merge preserves root" [ (id 1, main) ];
  Runtime.update_orchestrator runtime ready_root;
  check "implementation root remains draft during construction"
    (not (Patch_controller.ready_for_review (orch ()) (id 1)));
  check_starts "children start beneath original implementation root"
    [ (id 2, branch 1); (id 3, branch 1) ];
  Runtime.update_orchestrator runtime (fun o -> ready_branch o 2);
  let _, decisions = Patch_controller.reconcile_automerge (orch ()) ~now:0. in
  check "descendant integrates into implementation root rather than publication"
    (List.exists decisions ~f:(function
      | Patch_controller.Git_integrate { root; merge_patch_id; _ } ->
          Patch_id.equal root (id 1) && Patch_id.equal merge_patch_id (id 2)
      | Patch_controller.Github_merge _ -> false));
  Runtime.update_orchestrator runtime (fun o ->
      List.fold [ 2; 3; 4 ] ~init:o ~f:(fun o n ->
          Orchestrator.mark_merged o (id n)));
  check "implementation root promotes once descendants complete"
    (Patch_controller.ready_for_review (orch ()) (id 1))

let () =
  Eio_main.run (fun _ ->
      publication_lifecycle ();
      let orch = fresh () in
      let actions = Patch_controller.plan_actions orch ~patches in
      check "children start on draft root after CI"
        (List.exists actions ~f:(function
          | Orchestrator.Start (p, b) ->
              Patch_id.equal p (id 2) && Branch.equal b (branch 1)
          | Orchestrator.Respond _ | Orchestrator.Rebase _ -> false));
      check "root waits for descendants"
        (not (Patch_controller.ready_for_review orch (id 1)));
      let orch = ready_branch orch 2 in
      check "published descendant has no PR"
        (not (Patch_agent.has_pr (Orchestrator.agent orch (id 2))));
      let counted =
        Orchestrator.increment_ci_failure_count orch (id 2) |> fun o ->
        Orchestrator.increment_ci_failure_count o (id 2)
      in
      let reset =
        Patch_controller.apply_branch_observation counted (id 2)
          ~head_sha:"head2"
          ~checks:
            [
              Ci_check.
                {
                  name = "test";
                  conclusion = "success";
                  details_url = None;
                  description = None;
                  started_at = None;
                  app_id = None;
                  check_suite_id = None;
                  id = Some 2;
                };
            ]
      in
      check "passing branch checks reset CI failure count"
        (Int.equal
           (Orchestrator.agent reset (id 2)).Patch_agent.ci_failure_count 0);
      check "feature mode rejects ad-hoc PRs"
        (Option.is_none
           (Orchestrator.find_agent
              (Orchestrator.add_agent orch ~patch_id:(id 99) ~branch:(branch 99)
                 ~base_branch:(branch 1) ~pr_number:(Pr_number.of_int 99))
              (id 99)));
      check "missing feature root keeps construction predicate total"
        (Orchestrator.construction_open (Orchestrator.remove_agent orch (id 1)));
      let claimed, decisions =
        Patch_controller.reconcile_automerge orch ~now:0.
      in
      check "integration immediate and carries checked head"
        (List.exists decisions ~f:(function
          | Patch_controller.Git_integrate { root; head_sha; merge_patch_id } ->
              Patch_id.equal root (id 1)
              && Patch_id.equal merge_patch_id (id 2)
              && String.equal head_sha "head2"
          | Patch_controller.Github_merge _ -> false));
      check "claimed branch cannot dispatch work"
        (not
           (List.exists
              (Patch_controller.plan_actions
                 (Orchestrator.enqueue claimed (id 2) Operation_kind.Human)
                 ~patches)
              ~f:(function
                | Orchestrator.Respond (p, _) -> Patch_id.equal p (id 2)
                | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)));
      let paused = Orchestrator.set_automerge_enabled orch (id 2) false in
      check "toggle pauses integration"
        (List.is_empty
           (snd (Patch_controller.reconcile_automerge paused ~now:0.)));
      let failed =
        Patch_controller.apply_automerge_failure claimed ~automerge_timeout:10.
          ~now:0. (id 2)
      in
      check "failure backoff"
        (List.is_empty
           (snd (Patch_controller.reconcile_automerge failed ~now:9.)));
      check "retry after backoff"
        (not
           (List.is_empty
              (snd (Patch_controller.reconcile_automerge failed ~now:10.))));
      let orch =
        ready_branch orch 3 |> fun o -> Orchestrator.mark_merged o (id 3)
      in
      check "grandchild stops at root"
        (Option.equal Branch.equal
           (Orchestrator.expected_base orch (id 4))
           (Some (branch 1)));
      let complete =
        List.fold [ 2; 3; 4 ] ~init:orch ~f:(fun o n ->
            Orchestrator.mark_merged o (id n))
      in
      check "root ready when construction complete"
        (Patch_controller.ready_for_review complete (id 1));
      check "old root CI invalidated"
        (not
           (Patch_controller.ready_for_review
              (Orchestrator.invalidate_root_readiness complete)
              (id 1)));
      let awaiting =
        Orchestrator.invalidate_root_readiness complete |> fun o ->
        Orchestrator.set_expected_remote_head_oid o (id 1) (Some "final-head")
      in
      let observe head =
        Patch_controller.
          {
            poll_result =
              Poller.
                {
                  queue = [];
                  merged = false;
                  closed = false;
                  is_draft = true;
                  merge_state = Pr_state.Mergeable;
                  merge_ready = true;
                  head_oid = Some head;
                  review_decision = None;
                  unresolved_comment_count = 0;
                  merge_queue_required = false;
                  merge_queue_entry = None;
                  checks_passing = true;
                  ci_checks = [];
                  merge_commit_sha = None;
                };
            base_branch = Some main;
            native_stack = false;
            branch_in_root = false;
            worktree_path = None;
          }
      in
      let stale, _, _ =
        Patch_controller.apply_poll_result awaiting (id 1) (observe "root-head")
      in
      check "pre-push root CI cannot promote"
        (not (Patch_controller.ready_for_review stale (id 1)));
      check "pre-push observation retains publication marker"
        (Option.equal String.equal
           (Orchestrator.agent stale (id 1))
             .Patch_agent.expected_remote_head_oid (Some "final-head"));
      let distinct, _, _ =
        Patch_controller.apply_poll_result awaiting (id 1)
          (observe "middle-head")
      in
      check "distinct known root head settles publication"
        (Option.equal String.equal
           (Orchestrator.agent distinct (id 1)).Patch_agent.head_oid
           (Some "middle-head")
        && Option.is_none
             (Orchestrator.agent distinct (id 1))
               .Patch_agent.expected_remote_head_oid);
      let publication_state = ref stale in
      let update f = publication_state := f !publication_state in
      ignore
        (Push_publication.with_pending_push ~update ~patch_id:(id 1)
           ~local_sha:(Some "final-head") (fun () -> Worktree.Push_up_to_date)
          : Worktree.push_result);
      check "root no-op publication still waits for exact head"
        (Option.equal String.equal
           (Orchestrator.agent !publication_state (id 1))
             .Patch_agent.expected_remote_head_oid (Some "final-head"));
      ignore
        (Push_publication.with_pending_push ~update ~patch_id:(id 1)
           ~local_sha:(Some "unpublished-head") (fun () ->
             Worktree.Push_error "transport")
          : Worktree.push_result);
      check "failed root push preserves previous published head"
        (Option.equal String.equal
           (Orchestrator.agent !publication_state (id 1))
             .Patch_agent.expected_remote_head_oid (Some "final-head"));
      let observed, _, _ =
        Patch_controller.apply_poll_result stale (id 1) (observe "final-head")
      in
      check "current published root CI allows promotion"
        (Patch_controller.ready_for_review observed (id 1));
      check "feedback blocks promotion"
        (not
           (Patch_controller.ready_for_review
              (Orchestrator.enqueue complete (id 1) Operation_kind.Human)
              (id 1)));
      let claimed = Orchestrator.claim_promotion complete in
      check "promotion freezes additions"
        (not (Orchestrator.additions_allowed claimed ~dependencies:[ id 1 ]));
      check "failed claim reopens"
        (Orchestrator.additions_allowed
           (Orchestrator.release_promotion claimed)
           ~dependencies:[ id 1 ]);
      let persisted_rt = Runtime.create ~gameplan:gp ~main_branch:main () in
      Runtime.update_orchestrator persisted_rt (fun _ -> claimed);
      let json = Runtime.read persisted_rt Persistence.snapshot_to_yojson in
      let restored =
        match Persistence.snapshot_of_yojson json with
        | Ok snap -> snap.Runtime.orchestrator
        | Error e -> failwith e
      in
      check "promotion claim persisted"
        (Orchestrator.promotion_claimed restored);
      check "branch publication persisted"
        (Orchestrator.agent restored (id 2)).Patch_agent.branch_published;
      let rt = Runtime.create ~gameplan:gp ~main_branch:main () in
      Runtime.update_orchestrator rt (fun _ -> fresh ());
      check "unrooted additions refused"
        (Result.is_error
           (Runtime.add_patch rt ~title:"bad" ~description:"" ~dependencies:[]));
      check "rooted additions accepted"
        (Result.is_ok
           (Runtime.add_patch rt ~title:"good" ~description:""
              ~dependencies:[ id 1 ]));
      Runtime.update_orchestrator rt Orchestrator.claim_promotion;
      check "claimed additions refused"
        (Result.is_error
           (Runtime.add_patch rt ~title:"late" ~description:""
              ~dependencies:[ id 1 ]));
      let trace = ref [] in
      Eio.Fiber.both
        (fun () ->
          Runtime.with_patch_write rt ~patch_id:(id 1) (fun () ->
              trace := "session-start" :: !trace;
              Eio.Fiber.yield ();
              trace := "session-end" :: !trace))
        (fun () ->
          Runtime.with_root_write rt (fun () ->
              trace := "integration" :: !trace));
      check "root session and integration serialized"
        (List.equal String.equal !trace
           [ "integration"; "session-end"; "session-start" ]
        || List.equal String.equal !trace
             [ "session-end"; "session-start"; "integration" ]);
      Stdlib.print_endline "PASS feature branch decisions and lifecycle")
