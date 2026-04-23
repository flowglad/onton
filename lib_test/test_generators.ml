open Base
open Onton.Types

(** QCheck2 generators for all core types.

    Shared test infrastructure — all property-based tests should import
    generators from this module rather than defining ad-hoc ones. *)

let gen_patch_id =
  QCheck2.Gen.(
    map Patch_id.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 12)))

let gen_pr_number = QCheck2.Gen.(map Pr_number.of_int (int_range 1 9999))

let gen_session_id =
  QCheck2.Gen.(
    map Session_id.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 8 16)))

let gen_branch =
  QCheck2.Gen.(
    map Branch.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 20)))

let gen_operation_kind =
  QCheck2.Gen.oneof_list
    Operation_kind.
      [ Rebase; Human; Merge_conflict; Ci; Review_comments; Pr_body ]

let gen_feedback_kind =
  QCheck2.Gen.oneof_list
    Operation_kind.[ Human; Merge_conflict; Ci; Review_comments; Pr_body ]

let gen_operation_kind_queue =
  QCheck2.Gen.(
    map
      (List.dedup_and_sort ~compare:Operation_kind.compare)
      (list_small gen_operation_kind))

let gen_comment =
  QCheck2.Gen.(
    let gen_body = string_size ~gen:printable (int_range 1 80) in
    let gen_path =
      option (string_size ~gen:(char_range 'a' 'z') (int_range 3 30))
    in
    let gen_line = option (int_range 1 500) in
    (* Use only synthetic (negative) IDs so content-based dedup governs in
       property tests, matching production behavior where real IDs are unique
       per GitHub comment. Real-ID duplicates with different content can't
       arise in production but would bypass content-match dedup. *)
    let* id = map Comment_id.of_int (int_range (-1_000_000) (-1)) in
    let* body = gen_body in
    let* path = gen_path in
    let* line = gen_line in
    let* outdated = bool in
    return
      Comment.
        {
          id;
          thread_id = None;
          body;
          path;
          line;
          commit_sha = None;
          original_commit_sha = None;
          outdated;
        })

let gen_patch =
  QCheck2.Gen.(
    let gen_deps = list_small gen_patch_id in
    let gen_title = string_size ~gen:printable (int_range 5 50) in
    map4
      (fun id title branch dependencies ->
        Patch.
          {
            id;
            title;
            description = "";
            branch;
            dependencies;
            spec = "";
            acceptance_criteria = [];
            files = [];
            classification = "";
            changes = [];
            test_stubs_introduced = [];
            test_stubs_implemented = [];
          })
      gen_patch_id gen_title gen_branch gen_deps)

let gen_ci_check =
  QCheck2.Gen.(
    let gen_name = string_size ~gen:(char_range 'a' 'z') (int_range 3 15) in
    (* Cover the full GitHub CheckConclusionState / StatusState universe
       the parser can produce, including ambiguous cases ("cancelled",
       "stale", empty string, "pending") that are neither a terminal
       failure nor a terminal success. Properties that conflate any subset
       of these with "failure" will surface via QCheck rather than via
       production incidents. *)
    let gen_conclusion =
      oneof_list
        [
          "success";
          "failure";
          "error";
          "neutral";
          "skipped";
          "cancelled";
          "action_required";
          "timed_out";
          "startup_failure";
          "stale";
          "pending";
          "in_progress";
          "queued";
          "";
        ]
    in
    let gen_url = option (pure "https://ci.example.com/1") in
    let gen_desc = option (string_size ~gen:printable (int_range 5 40)) in
    map4
      (fun name conclusion details_url description ->
        Ci_check.
          {
            name;
            conclusion;
            details_url;
            description;
            started_at = None;
            id = None;
          })
      gen_name gen_conclusion gen_url gen_desc)

let gen_patch_list_linear =
  QCheck2.Gen.(
    map
      (fun n ->
        List.init n ~f:(fun i ->
            let id = Patch_id.of_string (Printf.sprintf "patch-%d" i) in
            let branch = Branch.of_string (Printf.sprintf "branch-%d" i) in
            let dependencies =
              if i = 0 then []
              else [ Patch_id.of_string (Printf.sprintf "patch-%d" (i - 1)) ]
            in
            Patch.
              {
                id;
                title = Printf.sprintf "Patch %d" i;
                description = "";
                branch;
                dependencies;
                spec = "";
                acceptance_criteria = [];
                files = [];
                classification = "";
                changes = [];
                test_stubs_introduced = [];
                test_stubs_implemented = [];
              }))
      (int_range 1 8))

(** Generate a DAG-shaped patch list where patches can depend on multiple
    earlier patches (fan-in). Each patch picks a random subset of earlier
    patches as deps, producing diamonds, wide fans, and mixed shapes. *)
let gen_patch_dag =
  QCheck2.Gen.(
    int_range 2 8 >>= fun n ->
    (* For each patch i>0, generate a random subset of [0..i-1] as deps.
       We do this by generating a bitmask of length i for each patch. *)
    let rec gen_patches i acc =
      if i >= n then return (List.rev acc)
      else
        (* Each earlier patch is independently included as a dep with ~40%
           probability, but ensure at least 1 dep for i>0 when there are
           enough predecessors to form a diamond. *)
        let* dep_bits = list_size (return i) bool in
        let raw_deps =
          List.filter_mapi dep_bits ~f:(fun j included ->
              if included then
                Some (Patch_id.of_string (Printf.sprintf "patch-%d" j))
              else None)
        in
        (* Ensure at least one dep for non-root patches *)
        let* deps =
          if List.is_empty raw_deps && i > 0 then
            map
              (fun j -> [ Patch_id.of_string (Printf.sprintf "patch-%d" j) ])
              (int_range 0 (i - 1))
          else return raw_deps
        in
        let patch =
          Patch.
            {
              id = Patch_id.of_string (Printf.sprintf "patch-%d" i);
              title = Printf.sprintf "Patch %d" i;
              description = "";
              branch = Branch.of_string (Printf.sprintf "branch-%d" i);
              dependencies = deps;
              spec = "";
              acceptance_criteria = [];
              files = [];
              classification = "";
              changes = [];
              test_stubs_introduced = [];
              test_stubs_implemented = [];
            }
        in
        gen_patches (i + 1) (patch :: acc)
    in
    let root =
      Patch.
        {
          id = Patch_id.of_string "patch-0";
          title = "Patch 0";
          description = "";
          branch = Branch.of_string "branch-0";
          dependencies = [];
          spec = "";
          acceptance_criteria = [];
          files = [];
          classification = "";
          changes = [];
          test_stubs_introduced = [];
          test_stubs_implemented = [];
        }
    in
    gen_patches 1 [ root ])

(** Generate patch lists that are a mix of linear chains and DAGs. *)
let gen_patch_list_unique =
  QCheck2.Gen.(oneof [ gen_patch_list_linear; gen_patch_dag ])

let gen_gameplan =
  QCheck2.Gen.(
    map
      (fun patches ->
        Gameplan.
          {
            project_name = "test-project";
            problem_statement = "test problem";
            solution_summary = "test solution";
            final_state_spec = "";
            patches;
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
            open_questions = [];
          })
      gen_patch_list_unique)

let gen_graph = QCheck2.Gen.(map Onton.Graph.of_patches gen_patch_list_unique)

(* -- Pr_state types -- *)

let gen_pr_status =
  QCheck2.Gen.oneof_list Onton.Pr_state.[ Open; Merged; Closed ]

let gen_merge_state =
  QCheck2.Gen.oneof_list Onton.Pr_state.[ Mergeable; Conflicting; Unknown ]

let gen_check_status =
  QCheck2.Gen.oneof_list Onton.Pr_state.[ Passing; Failing; Pending ]

let gen_pr_state =
  QCheck2.Gen.(
    let open Onton.Pr_state in
    let* is_draft = bool in
    let* is_fork = bool in
    let* head_branch = option gen_branch in
    let* base_branch = option gen_branch in
    map5
      (fun (status, merge_state) merge_ready (check_status, ci_checks_truncated)
           ci_checks (comments, unresolved_comment_count) ->
        {
          status;
          is_draft;
          merge_state;
          merge_ready;
          check_status;
          ci_checks;
          ci_checks_truncated;
          comments;
          unresolved_comment_count;
          head_branch;
          head_oid = None;
          base_branch;
          is_fork;
        })
      (pair gen_pr_status gen_merge_state)
      bool
      (pair gen_check_status bool)
      (list_small gen_ci_check)
      (pair (list_small gen_comment) (int_range 0 10)))

let gen_github_error =
  QCheck2.Gen.(
    oneof
      [
        map2
          (fun status body ->
            Onton.Github.Http_error
              { meth = "GET"; path = "/test"; status; body })
          (int_range 400 599)
          (string_size ~gen:printable (int_range 5 30));
        map
          (fun msg -> Onton.Github.Json_parse_error msg)
          (string_size ~gen:printable (int_range 5 30));
        map
          (fun msgs -> Onton.Github.Graphql_error msgs)
          (list_small (string_size ~gen:printable (int_range 5 30)));
      ])

(* -- Poller -- *)

let gen_poller =
  QCheck2.Gen.(
    let* is_draft = bool in
    map5
      (fun queue (merged, closed, has_conflict) merge_ready checks_passing
           ci_checks ->
        Onton.Poller.
          {
            queue;
            merged;
            closed;
            is_draft;
            has_conflict;
            merge_ready;
            checks_passing;
            ci_checks;
          })
      gen_operation_kind_queue (triple bool bool bool) bool bool
      (list_small gen_ci_check))

(* -- Patch_agent -- *)

let gen_patch_agent_fresh =
  QCheck2.Gen.(
    map2
      (fun pid branch -> Onton.Patch_agent.create ~branch pid)
      gen_patch_id gen_branch)

let gen_patch_agent_started =
  QCheck2.Gen.(
    map2
      (fun pid branch ->
        let a = Onton.Patch_agent.create ~branch pid in
        let a = Onton.Patch_agent.start a ~base_branch:branch in
        Onton.Patch_agent.set_llm_session_id a (Some "test-session"))
      gen_patch_id gen_branch)

let gen_patch_agent_with_queue =
  QCheck2.Gen.(
    map3
      (fun pid branch ops ->
        let a = Onton.Patch_agent.create ~branch pid in
        let a = Onton.Patch_agent.start a ~base_branch:branch in
        let a = Onton.Patch_agent.complete a in
        List.fold ops ~init:a ~f:Onton.Patch_agent.enqueue)
      gen_patch_id gen_branch gen_operation_kind_queue)

let gen_patch_agent_with_messages =
  QCheck2.Gen.(
    map4
      (fun pid branch messages ops ->
        let a = Onton.Patch_agent.create ~branch pid in
        let a = Onton.Patch_agent.start a ~base_branch:branch in
        let a =
          List.fold messages ~init:a ~f:(fun a msg ->
              Onton.Patch_agent.add_human_message a msg)
        in
        let a = Onton.Patch_agent.complete a in
        List.fold ops ~init:a ~f:Onton.Patch_agent.enqueue)
      gen_patch_id gen_branch
      (list_small (string_size ~gen:printable (int_range 1 80)))
      gen_operation_kind_queue)

let gen_session_fallback =
  QCheck2.Gen.oneof_list
    Onton.Patch_agent.[ Fresh_available; Tried_fresh; Given_up ]

let gen_patch_agent_fully_populated =
  QCheck2.Gen.(
    let* pid = gen_patch_id in
    let* branch = gen_branch in
    let* messages = list_small (string_size ~gen:printable (int_range 1 80)) in
    let* ops = gen_operation_kind_queue in
    let* ci_checks = list_small gen_ci_check in
    let* fallback = gen_session_fallback in
    let* pr_number = option gen_pr_number in
    let* merge_ready = bool in
    let* checks_passing = bool in
    let* raw_llm_session_id =
      option (string_size ~gen:printable (int_range 8 36))
    in
    let a = Onton.Patch_agent.create ~branch pid in
    let a = Onton.Patch_agent.start a ~base_branch:branch in
    let a =
      List.fold messages ~init:a ~f:(fun a msg ->
          Onton.Patch_agent.add_human_message a msg)
    in
    let a =
      match fallback with
      | Onton.Patch_agent.Fresh_available -> a
      | Onton.Patch_agent.Tried_fresh -> Onton.Patch_agent.set_tried_fresh a
      | Onton.Patch_agent.Given_up -> Onton.Patch_agent.set_session_failed a
    in
    (* When session_fallback is Tried_fresh or Given_up, llm_session_id must
       be None (escalation clears it). Only Fresh_available may have a value. *)
    let llm_session_id =
      match fallback with
      | Onton.Patch_agent.Fresh_available -> raw_llm_session_id
      | Onton.Patch_agent.Tried_fresh | Onton.Patch_agent.Given_up -> None
    in
    let a = Onton.Patch_agent.set_llm_session_id a llm_session_id in
    let a = Onton.Patch_agent.complete a in
    let a = List.fold ops ~init:a ~f:Onton.Patch_agent.enqueue in
    let a = Onton.Patch_agent.set_ci_checks a ci_checks in
    let a =
      match pr_number with
      | Some n -> Onton.Patch_agent.set_pr_number a n
      | None -> a
    in
    let a = Onton.Patch_agent.set_merge_ready a merge_ready in
    let a = Onton.Patch_agent.set_checks_passing a checks_passing in
    return a)

(* -- Reconciler -- *)

let gen_patch_view =
  QCheck2.Gen.(
    map3
      (fun (id, base_branch) (has_pr, merged, busy, branch_blocked)
           (needs_intervention, queue) ->
        Onton.Reconciler.
          {
            id;
            has_pr;
            merged;
            busy;
            needs_intervention;
            branch_blocked;
            queue;
            base_branch;
            (* Conservative default for existing randomized tests: no drift
               (local branch is on the current base). Drift-specific tests
               construct views explicitly rather than relying on this
               generator. *)
            branch_rebased_onto = Some base_branch;
          })
      (pair gen_patch_id gen_branch)
      (quad bool bool bool bool)
      (pair bool gen_operation_kind_queue))

let gen_reconciler_action =
  QCheck2.Gen.(
    oneof
      [
        map (fun pid -> Onton.Reconciler.Mark_merged pid) gen_patch_id;
        map (fun pid -> Onton.Reconciler.Enqueue_rebase pid) gen_patch_id;
        map3
          (fun patch_id kind new_base ->
            Onton.Reconciler.Start_operation { patch_id; kind; new_base })
          gen_patch_id gen_operation_kind (option gen_branch);
      ])

(* -- Orchestrator -- *)

let gen_orchestrator =
  QCheck2.Gen.(
    map2
      (fun patches main_branch ->
        Onton.Orchestrator.create ~patches ~main_branch)
      gen_patch_list_unique gen_branch)

let gen_orchestrator_action =
  QCheck2.Gen.(
    oneof
      [
        map2
          (fun pid branch -> Onton.Orchestrator.Start (pid, branch))
          gen_patch_id gen_branch;
        map2
          (fun pid kind -> Onton.Orchestrator.Respond (pid, kind))
          gen_patch_id gen_feedback_kind;
        map2
          (fun pid branch -> Onton.Orchestrator.Rebase (pid, branch))
          gen_patch_id gen_branch;
      ])

(* -- Invariants -- *)

let gen_violation =
  QCheck2.Gen.(
    map2
      (fun invariant details -> Onton.Invariants.{ invariant; details })
      (string_size ~gen:(char_range 'a' 'z') (int_range 5 30))
      (string_size ~gen:printable (int_range 5 50)))

(* -- Display status / Activity log -- *)

(** Exhaustive identity match ensures a compile error if a variant is added. *)
let all_display_statuses : Onton.Tui.display_status list =
  let open Onton.Tui in
  let id = function
    | Merged -> Merged
    | Needs_help -> Needs_help
    | Approved_idle -> Approved_idle
    | Approved_running -> Approved_running
    | Fixing_ci -> Fixing_ci
    | Addressing_review -> Addressing_review
    | Resolving_conflict -> Resolving_conflict
    | Responding_to_human -> Responding_to_human
    | Writing_pr_body -> Writing_pr_body
    | Rebasing -> Rebasing
    | Starting -> Starting
    | Updating -> Updating
    | Ci_queued -> Ci_queued
    | Review_queued -> Review_queued
    | Awaiting_ci -> Awaiting_ci
    | Awaiting_review -> Awaiting_review
    | Blocked_by_dep -> Blocked_by_dep
    | Pending -> Pending
  in
  List.map ~f:id
    [
      Merged;
      Needs_help;
      Approved_idle;
      Approved_running;
      Fixing_ci;
      Addressing_review;
      Resolving_conflict;
      Responding_to_human;
      Writing_pr_body;
      Rebasing;
      Starting;
      Updating;
      Ci_queued;
      Review_queued;
      Awaiting_ci;
      Awaiting_review;
      Blocked_by_dep;
      Pending;
    ]

let gen_display_status = QCheck2.Gen.oneof_list all_display_statuses

let gen_transition_entry =
  QCheck2.Gen.(
    let* timestamp = float_range 0.0 1e12 in
    let* patch_id = gen_patch_id in
    let* from_status = gen_display_status in
    let* to_status = gen_display_status in
    let* action = string_size ~gen:printable (int_range 3 40) in
    return
      (Onton.Activity_log.Transition_entry.create ~timestamp ~patch_id
         ~from_status ~to_status ~action))

let gen_event =
  QCheck2.Gen.(
    let* timestamp = float_range 0.0 1e12 in
    let* patch_id = option gen_patch_id in
    let* message = string_size ~gen:printable (int_range 3 80) in
    return (Onton.Activity_log.Event.create ~timestamp ?patch_id message))

let gen_activity_log =
  QCheck2.Gen.(
    map2
      (fun transitions events ->
        let log =
          List.fold transitions ~init:Onton.Activity_log.empty ~f:(fun acc e ->
              Onton.Activity_log.add_transition acc e)
        in
        List.fold events ~init:log ~f:(fun acc e ->
            Onton.Activity_log.add_event acc e))
      (list_size (int_range 0 5) gen_transition_entry)
      (list_size (int_range 0 5) gen_event))

(* -- Run_classification -- *)

let gen_run_outcome =
  QCheck2.Gen.(
    let open Onton.Run_classification in
    let* exit_code = int_range (-1) 255 in
    let* got_events = bool in
    let* stderr = string_size ~gen:printable (int_range 0 80) in
    let* stream_errors = string_size ~gen:printable (int_range 0 80) in
    let* timed_out = bool in
    return { exit_code; got_events; stderr; stream_errors; timed_out })

let gen_porcelain_entry =
  QCheck2.Gen.(
    map2
      (fun path branch ->
        Printf.sprintf "worktree %s\nbranch refs/heads/%s\n" path branch)
      (string_size ~gen:(char_range 'a' 'z') (int_range 5 20))
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 15)))

let gen_porcelain_output =
  QCheck2.Gen.(
    map
      (fun entries -> String.concat ~sep:"\n\n" entries)
      (list_small gen_porcelain_entry))

let gen_pr_json_entry =
  QCheck2.Gen.(
    map3
      (fun number state base_ref ->
        Printf.sprintf {|{"number":%d,"state":"%s","baseRefName":"%s"}|} number
          state base_ref)
      (int_range 1 9999)
      (oneof_list [ "OPEN"; "MERGED"; "CLOSED" ])
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 15)))

let gen_pr_json =
  QCheck2.Gen.(
    map
      (fun entries -> "[" ^ String.concat ~sep:"," entries ^ "]")
      (list_small gen_pr_json_entry))

(* -- Printers for QCheck2 shrinking/reporting -- *)

let gen_session_result =
  QCheck2.Gen.(
    oneof
      [
        return Onton.Orchestrator.Session_ok;
        map
          (fun b -> Onton.Orchestrator.Session_process_error { is_fresh = b })
          bool;
        return Onton.Orchestrator.Session_no_resume;
        map (fun b -> Onton.Orchestrator.Session_failed { is_fresh = b }) bool;
        return Onton.Orchestrator.Session_give_up;
        return Onton.Orchestrator.Session_worktree_missing;
        return Onton.Orchestrator.Session_push_failed;
        return Onton.Orchestrator.Session_no_commits;
      ])

let print_session_result = Onton.Orchestrator.show_session_result
let print_patch_id = Patch_id.to_string
let print_branch = Branch.to_string

(* -- Shared test helpers -- *)

let mk_linear_patches n =
  List.init n ~f:(fun i ->
      let id = Patch_id.of_string (Printf.sprintf "p%d" i) in
      let branch = Branch.of_string (Printf.sprintf "b%d" i) in
      let dependencies =
        if i = 0 then []
        else [ Patch_id.of_string (Printf.sprintf "p%d" (i - 1)) ]
      in
      Patch.
        {
          id;
          title = Printf.sprintf "Patch %d" i;
          description = "";
          branch;
          dependencies;
          spec = "";
          acceptance_criteria = [];
          files = [];
          classification = "";
          changes = [];
          test_stubs_introduced = [];
          test_stubs_implemented = [];
        })

let make_test_gameplan patches =
  Gameplan.
    {
      project_name = "test-project";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches;
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }

let pid_of_idx patches i =
  let (p : Patch.t) = List.nth_exn patches i in
  p.Patch.id

let apply_reconcile_actions orch ~main ~branch_of =
  let agents = Onton.Orchestrator.all_agents orch in
  let patch_views =
    List.map agents ~f:(fun (a : Onton.Patch_agent.t) ->
        Onton.Reconciler.
          {
            id = a.Onton.Patch_agent.patch_id;
            has_pr = Onton.Patch_agent.has_pr a;
            merged = a.Onton.Patch_agent.merged;
            busy = a.Onton.Patch_agent.busy;
            needs_intervention = Onton.Patch_agent.needs_intervention a;
            branch_blocked = a.Onton.Patch_agent.branch_blocked;
            queue = a.Onton.Patch_agent.queue;
            base_branch =
              Option.value a.Onton.Patch_agent.base_branch ~default:main;
            branch_rebased_onto = a.Onton.Patch_agent.branch_rebased_onto;
          })
  in
  let merged_patches =
    List.filter_map agents ~f:(fun (a : Onton.Patch_agent.t) ->
        if a.Onton.Patch_agent.merged then Some a.Onton.Patch_agent.patch_id
        else None)
  in
  let actions =
    Onton.Reconciler.reconcile
      ~graph:(Onton.Orchestrator.graph orch)
      ~main ~merged_pr_patches:merged_patches ~branch_of patch_views
  in
  List.fold actions ~init:orch ~f:(fun orch action ->
      match action with
      | Onton.Reconciler.Mark_merged pid ->
          Onton.Orchestrator.mark_merged orch pid
      | Onton.Reconciler.Enqueue_rebase pid ->
          Onton.Orchestrator.enqueue orch pid Operation_kind.Rebase
      | Onton.Reconciler.Start_operation _ -> orch)

let print_operation_kind = Operation_kind.show
let print_comment = Comment.show
let print_patch = Patch.show
let print_ci_check = Ci_check.show
let print_gameplan = Gameplan.show
let print_patch_agent = Onton.Patch_agent.show
let print_poller = Onton.Poller.show
let print_pr_state = Onton.Pr_state.show
let print_github_error = Onton.Github.show_error
