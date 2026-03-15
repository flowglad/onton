open Base
open Types

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
    Operation_kind.[ Rebase; Human; Merge_conflict; Ci; Review_comments ]

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
    map3
      (fun body path line -> Comment.{ body; path; line })
      gen_body gen_path gen_line)

let gen_patch =
  QCheck2.Gen.(
    let gen_deps = list_small gen_patch_id in
    map3
      (fun id branch dependencies ->
        Patch.{ id; title = Patch_id.to_string id; branch; dependencies })
      gen_patch_id gen_branch gen_deps)

let gen_ci_check =
  QCheck2.Gen.(
    let gen_name = string_size ~gen:(char_range 'a' 'z') (int_range 3 15) in
    let gen_conclusion =
      oneof_list [ "success"; "failure"; "neutral"; "skipped" ]
    in
    let gen_url = option (pure "https://ci.example.com/1") in
    let gen_desc = option (string_size ~gen:printable (int_range 5 40)) in
    map4
      (fun name conclusion details_url description ->
        Ci_check.{ name; conclusion; details_url; description })
      gen_name gen_conclusion gen_url gen_desc)

let gen_patch_list_unique =
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
              { id; title = Printf.sprintf "Patch %d" i; branch; dependencies }))
      (int_range 1 8))

let gen_gameplan =
  QCheck2.Gen.(
    map
      (fun patches ->
        Gameplan.
          {
            project_name = "test-project";
            problem_statement = "test problem";
            solution_summary = "test solution";
            patches;
          })
      gen_patch_list_unique)

let gen_graph = QCheck2.Gen.(map Graph.of_patches gen_patch_list_unique)

(* -- Github types -- *)

let gen_merge_state =
  QCheck2.Gen.oneof_list Github.Pr_state.[ Mergeable; Conflicting; Unknown ]

let gen_check_status =
  QCheck2.Gen.oneof_list Github.Pr_state.[ Passing; Failing; Pending ]

let gen_pr_state =
  QCheck2.Gen.(
    let open Github.Pr_state in
    map3
      (fun (merged, merge_state) check_status
           (comments, unresolved_comment_count) ->
        {
          merged;
          merge_state;
          check_status;
          comments;
          unresolved_comment_count;
        })
      (pair bool gen_merge_state)
      gen_check_status
      (pair (list_small gen_comment) (int_range 0 10)))

let gen_github_error =
  QCheck2.Gen.(
    oneof
      [
        map2
          (fun code msg -> Github.Http_error (code, msg))
          (int_range 400 599)
          (string_size ~gen:printable (int_range 5 30));
        map
          (fun msg -> Github.Json_parse_error msg)
          (string_size ~gen:printable (int_range 5 30));
        map
          (fun msgs -> Github.Graphql_error msgs)
          (list_small (string_size ~gen:printable (int_range 5 30)));
      ])

(* -- Poller -- *)

let gen_poller =
  QCheck2.Gen.(
    map3
      (fun queue (merged, has_conflict) (mergeable, checks_passing) ->
        Poller.{ queue; merged; has_conflict; mergeable; checks_passing })
      gen_operation_kind_queue (pair bool bool) (pair bool bool))

(* -- Patch_agent -- *)

let gen_pending_comment =
  QCheck2.Gen.(map2 (fun comment valid -> (comment, valid)) gen_comment bool)

let gen_patch_agent_fresh = QCheck2.Gen.(map Patch_agent.create gen_patch_id)

let gen_patch_agent_started =
  QCheck2.Gen.(
    map2
      (fun pid branch ->
        Patch_agent.create pid |> fun a ->
        Patch_agent.start a ~base_branch:branch)
      gen_patch_id gen_branch)

let gen_patch_agent_with_queue =
  QCheck2.Gen.(
    map3
      (fun pid branch ops ->
        let a = Patch_agent.create pid in
        let a = Patch_agent.start a ~base_branch:branch in
        let a = Patch_agent.complete a in
        List.fold ops ~init:a ~f:Patch_agent.enqueue)
      gen_patch_id gen_branch gen_operation_kind_queue)

(* -- Reconciler -- *)

let gen_patch_view =
  QCheck2.Gen.(
    map3
      (fun (id, base_branch) (has_pr, merged, busy) (needs_intervention, queue)
         ->
        Reconciler.
          { id; has_pr; merged; busy; needs_intervention; queue; base_branch })
      (pair gen_patch_id gen_branch)
      (triple bool bool bool)
      (pair bool gen_operation_kind_queue))

let gen_reconciler_action =
  QCheck2.Gen.(
    oneof
      [
        map (fun pid -> Reconciler.Mark_merged pid) gen_patch_id;
        map (fun pid -> Reconciler.Enqueue_rebase pid) gen_patch_id;
        map3
          (fun patch_id kind new_base ->
            Reconciler.Start_operation { patch_id; kind; new_base })
          gen_patch_id gen_operation_kind (option gen_branch);
      ])

(* -- Orchestrator -- *)

let gen_orchestrator =
  QCheck2.Gen.(
    map2
      (fun patches main_branch -> Orchestrator.create ~patches ~main_branch)
      gen_patch_list_unique gen_branch)

let gen_orchestrator_action =
  QCheck2.Gen.(
    oneof
      [
        map2
          (fun pid branch -> Orchestrator.Start (pid, branch))
          gen_patch_id gen_branch;
        map2
          (fun pid kind -> Orchestrator.Respond (pid, kind))
          gen_patch_id gen_operation_kind;
      ])

(* -- Invariants -- *)

let gen_violation =
  QCheck2.Gen.(
    map2
      (fun invariant details -> Invariants.{ invariant; details })
      (string_size ~gen:(char_range 'a' 'z') (int_range 5 30))
      (string_size ~gen:printable (int_range 5 50)))

(* -- Printers for QCheck2 shrinking/reporting -- *)

let print_patch_id = Patch_id.to_string
let print_branch = Branch.to_string
let print_operation_kind = Operation_kind.show
let print_patch_agent = Patch_agent.show
let print_poller = Poller.show
let print_pr_state = Github.Pr_state.show
