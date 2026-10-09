(* @archlint.module test
   @archlint.domain patch-controller *)
open Base
open Onton
open Onton_core
open Types
open Patch_controller
module F = Onton_core_test_support.Publication_fixture

let publish t candidate =
  F.confirmed ~candidate
    ~step:(fun t event ->
      fst (Orchestrator.reconcile_branch t (Patch_id.of_string "p1") event))
    ~state:(fun t ->
      (Orchestrator.agent t (Patch_id.of_string "p1"))
        .Patch_agent.branch_reconcile)
    t

let make_orchestrator ~patch_id ~main_branch =
  let patch =
    {
      Patch.id = patch_id;
      title = "test";
      description = "test";
      branch = Branch.of_string "test-branch";
      dependencies = [];
      spec = "";
      acceptance_criteria = [];
      changes = [];
      files = [];
      classification = "";
      test_stubs_introduced = [];
      test_stubs_implemented = [];
      complexity = None;
      precedents = [];
      required_context = [];
    }
  in
  (patch, Orchestrator.create ~patches:[ patch ] ~main_branch)

let pid = Patch_id.of_string "p1"
let main = Branch.of_string "main"

let () =
  assert (
    let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
    let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
    let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
    let t = Orchestrator.complete t pid in
    let t = Orchestrator.set_head_oid t pid (Some "old-head") in
    let t = publish t (F.sha "rebased-head") in
    let mergeable =
      {
        Poller.queue = [];
        merged = false;
        closed = false;
        is_draft = false;
        merge_state = Pr_state.Mergeable;
        merge_ready = true;
        base_branch = Some main;
        base_oid = Some (F.sha "base");
        head_oid = Some (F.sha "rebased-head");
        review_decision = None;
        unresolved_comment_count = 0;
        merge_queue_required = false;
        merge_queue_entry = None;
        checks_passing = true;
        ci_checks = [];
        merge_commit_sha = None;
      }
    in
    let t, _, _ =
      Onton_test_support.Forge_poll_fixture.apply t pid
        {
          poll_result = mergeable;
          base_branch = Some main;
          native_stack = false;
          branch_in_root = false;
          worktree_path = None;
        }
    in
    let after_mergeable = Orchestrator.agent t pid in
    Option.is_none (Patch_agent.expected_remote_head_oid after_mergeable)
    && after_mergeable.Patch_agent.merge_ready
    && not (Patch_agent.needs_intervention after_mergeable))

let () =
  assert (
    let _patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
    let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
    let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
    let t = Orchestrator.complete t pid in
    let pushed_head = F.sha "new-rebased-head" in
    let t =
      Orchestrator.set_head_oid t pid (Some (F.sha "old-pre-push-head"))
    in
    let t = publish t pushed_head in
    let poll_result =
      {
        Poller.queue = [ Operation_kind.Merge_conflict ];
        merged = false;
        closed = false;
        is_draft = false;
        merge_state = Pr_state.Conflicting;
        merge_ready = false;
        base_branch = Some main;
        base_oid = Some (F.sha "base");
        head_oid = Some (F.sha "old-pre-push-head");
        review_decision = None;
        unresolved_comment_count = 0;
        merge_queue_required = false;
        merge_queue_entry = None;
        checks_passing = true;
        ci_checks = [];
        merge_commit_sha = None;
      }
    in
    let apply ?confirmed_remote_head t poll_result =
      let t, _, _ =
        (if Option.is_some poll_result.Poller.head_oid then
           Onton_test_support.Forge_poll_fixture.apply
         else apply_poll_result ?previous_conflict:None)
          ?confirmed_remote_head t pid
          {
            poll_result;
            base_branch = Some main;
            native_stack = false;
            branch_in_root = false;
            worktree_path = None;
          }
      in
      t
    in
    let t =
      apply t
        {
          poll_result with
          Poller.queue = [];
          Poller.merge_state = Pr_state.Mergeable;
          Poller.merge_ready = true;
        }
    in
    let agent = Orchestrator.agent t pid in
    assert (not agent.Patch_agent.merge_ready);
    assert agent.Patch_agent.mergeability_unknown;
    assert (
      Option.equal String.equal
        (Patch_agent.expected_remote_head_oid agent)
        (Some pushed_head));
    let unidentified =
      apply t
        {
          poll_result with
          Poller.queue = [];
          Poller.base_branch = None;
          Poller.base_oid = None;
          Poller.head_oid = None;
          Poller.merge_state = Pr_state.Mergeable;
          Poller.merge_ready = true;
          Poller.review_decision = Some "APPROVED";
        }
    in
    let agent = Orchestrator.agent unidentified pid in
    assert (not agent.Patch_agent.merge_ready);
    assert agent.Patch_agent.mergeability_unknown;
    assert (
      Option.equal String.equal agent.Patch_agent.review_decision
        (Some "APPROVED"));
    assert (
      Option.equal String.equal agent.Patch_agent.head_oid
        (Some (F.sha "old-pre-push-head")));
    assert (
      Option.equal String.equal
        (Patch_agent.expected_remote_head_oid agent)
        (Some pushed_head));
    let t = apply unidentified { poll_result with Poller.head_oid = None } in
    let t = apply t poll_result in
    let agent = Orchestrator.agent t pid in
    assert (not (Patch_agent.has_conflict agent));
    assert (not agent.Patch_agent.merge_ready);
    assert (
      not
        (List.mem agent.Patch_agent.queue Operation_kind.Merge_conflict
           ~equal:Operation_kind.equal));
    assert (
      Option.equal String.equal agent.Patch_agent.head_oid
        (Some (F.sha "old-pre-push-head")));
    assert (
      Option.equal String.equal
        (Patch_agent.expected_remote_head_oid agent)
        (Some pushed_head));
    List.for_all
      [ pushed_head; F.sha "newer-remote-head" ]
      ~f:(fun head ->
        let t =
          apply ~confirmed_remote_head:head t
            { poll_result with Poller.head_oid = Some head }
        in
        let agent = Orchestrator.agent t pid in
        Patch_agent.has_conflict agent
        && List.mem agent.Patch_agent.queue Operation_kind.Merge_conflict
             ~equal:Operation_kind.equal
        && Option.is_none (Patch_agent.expected_remote_head_oid agent)
        && Option.equal String.equal agent.Patch_agent.head_oid (Some head)))
