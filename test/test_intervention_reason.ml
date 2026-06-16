(* @archlint.module test
   @archlint.domain patch-agent *)

(** Regression tests for [Tui.human_intervention_reason].

    The misleading-banner bug: a patch pushed into needs-intervention by a high
    CI-failure count rendered "runner: pushed after session" — the most recent
    (and innocuous) activity-log line — instead of the actionable reason. The
    fix makes the banner derive from the agent's own failure counters via
    [Patch_agent.intervention_reason]. These tests pin that counter-based human
    reasons stay aligned with raw intervention, branch-blocked agents still get
    an operator-facing reason, and a CI-stuck agent reports the CI failure,
    never a push event. *)

open Onton
open Onton_core
open Onton_core.Types

let agent () =
  Patch_agent.create ~branch:(Branch.of_string "b")
    (Patch_id.of_string "patch-1")

let rec apply n f x = if n <= 0 then x else apply (n - 1) f (f x)

let contains s sub =
  let s = String.lowercase_ascii s and sub = String.lowercase_ascii sub in
  let n = String.length s and m = String.length sub in
  let rec go i =
    i + m <= n && (String.equal (String.sub s i m) sub || go (i + 1))
  in
  m = 0 || go 0

let () =
  (* A healthy agent needs no intervention and has no banner reason. *)
  let a = agent () in
  assert (not (Patch_agent.needs_intervention a));
  assert (Option.is_none (Tui.human_intervention_reason a));

  (* Three CI failures is the documented threshold for intervention. *)
  let stuck = apply 3 Patch_agent.increment_ci_failure_count a in
  assert (Patch_agent.needs_intervention stuck);
  (match Tui.human_intervention_reason stuck with
  | None -> assert false
  | Some msg ->
      (* The reason names the CI failure and its count... *)
      assert (contains msg "ci");
      assert (contains msg "3");
      (* ...and never the innocuous push event that triggered the bug. *)
      assert (not (contains msg "pushed")));

  (* The human reason is present exactly when intervention is needed: the two
     stay in lockstep with the authoritative predicate, so the banner can't
     show a reason for a healthy patch (or hide one for a stuck patch). *)
  assert (
    Bool.equal
      (Patch_agent.needs_intervention stuck)
      (Option.is_some (Tui.human_intervention_reason stuck)));
  assert (
    Bool.equal
      (Patch_agent.needs_intervention a)
      (Option.is_some (Tui.human_intervention_reason a)));

  (* A repo-root branch collision is not a PatchAgent failure-threshold
     intervention, but it does require operator attention. The TUI must surface
     it as needs-help instead of leaving the row looking queued and inert. *)
  let branch_blocked = Patch_agent.set_branch_blocked a in
  assert (not (Patch_agent.needs_intervention branch_blocked));
  (match Tui.human_intervention_reason branch_blocked with
  | None -> assert false
  | Some msg ->
      assert (contains msg "repo root");
      assert (contains msg "branch"));

  (* Rebase/worktree failures have their own intervention reason and must not
     be reported as repeated LLM session failures. *)
  let rebase_stuck =
    a |> Patch_agent.increment_rebase_failure_count
    |> Patch_agent.increment_rebase_failure_count
  in
  assert (Patch_agent.needs_intervention rebase_stuck);
  (match Tui.human_intervention_reason rebase_stuck with
  | None -> assert false
  | Some msg ->
      assert (contains msg "rebase");
      assert (contains msg "2");
      assert (not (contains msg "session")));

  print_endline "PASS: human_intervention_reason surfaces the actionable reason"

let make_patch ?(deps = []) ~id ~branch ~title () =
  Patch.
    {
      id;
      title;
      description = "";
      branch;
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

let make_gameplan patches =
  Gameplan.
    {
      project_name = "test-project";
      repo_owner = "";
      repo_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches;
      functional_changes = [];
      context_resources = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }

let () =
  let patch_id = Patch_id.of_string "patch-1" in
  let patch =
    make_patch ~id:patch_id
      ~branch:(Branch.of_string "codex/fix")
      ~title:"fix" ()
  in
  let gameplan = make_gameplan [ patch ] in
  let orchestrator =
    Orchestrator.create ~patches:gameplan.Gameplan.patches
      ~main_branch:(Branch.of_string "main")
    |> fun orchestrator -> Orchestrator.set_branch_blocked orchestrator patch_id
  in
  let views =
    Tui.views_of_orchestrator ~orchestrator ~gameplan ~activity:[]
      ~resolve_routing:(fun ~complexity:_ ->
        { Backend_routing.backend = "claude"; model = None })
      ()
  in
  match views with
  | [ view ] ->
      assert (Tui.equal_display_status view.Tui.status Tui.Needs_help);
      assert view.Tui.needs_intervention;
      (match view.Tui.intervention_reason with
      | Some msg ->
          assert (contains msg "repo root");
          assert (contains msg "branch")
      | None -> assert false);
      print_endline "PASS: branch-blocked patches render as needs-help"
  | _ -> assert false

let assert_raw_fields ~merged ~has_pr ~is_pr_missing ~session_given_up
    ~human_in_queue ~ci_failure_count ~start_attempts_without_pr
    ~conflict_noop_count ~no_commits_push_count ~context_exhaustion_count
    ~push_failure_count ~rebase_failure_count ~pr_body_artifact_miss_count
    ~expected =
  let reason =
    Patch_agent.intervention_reason_of_fields ~merged ~has_pr ~is_pr_missing
      ~session_given_up ~human_in_queue ~ci_failure_count
      ~start_attempts_without_pr ~conflict_noop_count ~no_commits_push_count
      ~context_exhaustion_count ~push_failure_count ~rebase_failure_count
      ~pr_body_artifact_miss_count
  in
  assert (Option.equal String.equal reason expected);
  assert (
    Bool.equal
      (Patch_agent.needs_intervention_of_fields ~merged ~has_pr ~is_pr_missing
         ~session_given_up ~human_in_queue ~ci_failure_count
         ~start_attempts_without_pr ~conflict_noop_count ~no_commits_push_count
         ~context_exhaustion_count ~push_failure_count ~rebase_failure_count
         ~pr_body_artifact_miss_count)
      (Option.is_some expected))

let () =
  assert_raw_fields ~merged:false ~has_pr:true ~is_pr_missing:false
    ~session_given_up:false ~human_in_queue:false ~ci_failure_count:0
    ~start_attempts_without_pr:0 ~conflict_noop_count:0 ~no_commits_push_count:0
    ~context_exhaustion_count:0 ~push_failure_count:0 ~rebase_failure_count:2
    ~pr_body_artifact_miss_count:0 ~expected:(Some "rebase_failure_count>=2");
  assert_raw_fields ~merged:false ~has_pr:true ~is_pr_missing:false
    ~session_given_up:false ~human_in_queue:true ~ci_failure_count:3
    ~start_attempts_without_pr:0 ~conflict_noop_count:0 ~no_commits_push_count:0
    ~context_exhaustion_count:0 ~push_failure_count:0 ~rebase_failure_count:0
    ~pr_body_artifact_miss_count:0 ~expected:None;
  print_endline "PASS: raw intervention field decisions stay in lockstep"

(* Exercise the whole Patch_agent decision surface. Some transitions have
   preconditions (e.g. [clear_pr] requires a PR present), so each is applied
   defensively: the property asserts the surface is total — no arbitrary
   transition order crashes the harness — while referencing every decision API.
   Uses the generated [flag] so it counts as a property over real input. *)
let () =
  let anchor =
    match
      Anchor.make ~base:(Branch.of_string "main") ~sha:(String.make 40 'a')
        ~observed_at_remote:false
    with
    | Some anchor -> anchor
    | None -> assert false
  in
  let main = Branch.of_string "main" in
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"patch_agent surface is total under arbitrary transition order"
       ~count:50 QCheck2.Gen.bool (fun flag ->
         let transitions : (Patch_agent.t -> Patch_agent.t) list =
           [
             (fun a -> Patch_agent.set_automerge_enabled a flag);
             (fun a -> Patch_agent.set_automerge_inflight a flag);
             (fun a -> Patch_agent.set_automerge_deadline a 1.0);
             (fun a -> Patch_agent.clear_automerge_deadline a);
             (fun a -> Patch_agent.increment_automerge_failure_count a);
             (fun a -> Patch_agent.reset_automerge_failure_count a);
             (fun a -> Patch_agent.increment_conflict_noop_count a);
             (fun a -> Patch_agent.reset_conflict_noop_count a);
             (fun a -> Patch_agent.increment_no_commits_push_count a);
             (fun a -> Patch_agent.reset_no_commits_push_count a);
             (fun a -> Patch_agent.increment_push_failure_count a);
             (fun a -> Patch_agent.reset_push_failure_count a);
             (fun a -> Patch_agent.increment_rebase_failure_count a);
             (fun a -> Patch_agent.reset_rebase_failure_count a);
             (fun a -> Patch_agent.reset_ci_failure_count a);
             (fun a -> Patch_agent.reset_context_exhaustion_count a);
             (fun a -> Patch_agent.reset_pr_body_artifact_miss_count a);
             (fun a -> Patch_agent.reset_busy a);
             (fun a -> Patch_agent.on_context_exhausted a);
             (fun a -> Patch_agent.on_pre_session_failure a);
             (fun a -> Patch_agent.on_session_failure a ~is_fresh:flag);
             (fun a -> Patch_agent.set_branch_blocked a);
             (fun a -> Patch_agent.clear_branch_blocked a);
             (fun a -> Patch_agent.clear_pr a);
             (fun a -> Patch_agent.clear_session_fallback a);
             (fun a -> Patch_agent.mark_running a);
             (fun a -> Patch_agent.bump_generation a);
             (fun a -> Patch_agent.mark_inflight_human_messages_delivered a);
             (fun a -> Patch_agent.add_human_messages a [ "msg" ]);
             (fun a -> Patch_agent.record_anchor a anchor);
             (fun a -> Patch_agent.resume_current_message a ~op:None);
             (fun a -> Patch_agent.set_base_contains_merged_siblings a flag);
             (fun a -> Patch_agent.set_branch_rebased_onto a main);
             (fun a ->
               Patch_agent.set_branch_rebased_onto_sha a (Some "deadbeef"));
             (fun a -> Patch_agent.set_checks_passing a flag);
             (fun a ->
               Patch_agent.set_current_message_id a
                 (Some (Message_id.of_string "m1")));
             (fun a -> Patch_agent.set_llm_session_id a (Some "session"));
             (fun a -> Patch_agent.set_merge_commit_sha a (Some "deadbeef"));
             (fun a -> Patch_agent.set_merge_queue_entry a None);
             (fun a -> Patch_agent.set_merge_queue_required a flag);
             (fun a -> Patch_agent.set_mergeability_unknown a flag);
             (fun a -> Patch_agent.set_worktree_path a "/tmp/wt");
           ]
         in
         let a =
           List.fold_left
             (fun a step -> try step a with Invalid_argument _ -> a)
             (agent ()) transitions
         in
         let _history = Patch_agent.anchor_history a in
         let _priority = Patch_agent.highest_priority a in
         let _approved =
           Patch_agent.is_approved_modulo_merge_ready a ~main_branch:main
         in
         let _reason = Patch_agent.intervention_reason a in
         let reason_from_fields =
           Patch_agent.intervention_reason_of_fields ~merged:false ~has_pr:false
             ~is_pr_missing:false ~session_given_up:false ~human_in_queue:flag
             ~ci_failure_count:3 ~start_attempts_without_pr:0
             ~conflict_noop_count:0 ~no_commits_push_count:0
             ~context_exhaustion_count:0 ~push_failure_count:0
             ~rebase_failure_count:0 ~pr_body_artifact_miss_count:0
         in
         let needs_from_fields =
           Patch_agent.needs_intervention_of_fields ~merged:false ~has_pr:false
             ~is_pr_missing:false ~session_given_up:false ~human_in_queue:flag
             ~ci_failure_count:3 ~start_attempts_without_pr:0
             ~conflict_noop_count:0 ~no_commits_push_count:0
             ~context_exhaustion_count:0 ~push_failure_count:0
             ~rebase_failure_count:0 ~pr_body_artifact_miss_count:0
         in
         let rebase_reason =
           Patch_agent.intervention_reason_of_fields ~merged:false ~has_pr:true
             ~is_pr_missing:false ~session_given_up:false ~human_in_queue:false
             ~ci_failure_count:0 ~start_attempts_without_pr:0
             ~conflict_noop_count:0 ~no_commits_push_count:0
             ~context_exhaustion_count:0 ~push_failure_count:0
             ~rebase_failure_count:2 ~pr_body_artifact_miss_count:0
         in
         let rebase_needs_intervention =
           Patch_agent.needs_intervention_of_fields ~merged:false ~has_pr:true
             ~is_pr_missing:false ~session_given_up:false ~human_in_queue:false
             ~ci_failure_count:0 ~start_attempts_without_pr:0
             ~conflict_noop_count:0 ~no_commits_push_count:0
             ~context_exhaustion_count:0 ~push_failure_count:0
             ~rebase_failure_count:2 ~pr_body_artifact_miss_count:0
         in
         Bool.equal needs_from_fields (Option.is_some reason_from_fields)
         && Bool.equal rebase_needs_intervention (Option.is_some rebase_reason)));
  print_endline "PASS: patch_agent surface threaded"
