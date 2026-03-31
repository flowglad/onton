open Base
open Onton
open Onton.Types

let make_stream_entry =
  let open QCheck2.Gen in
  let gen_kind =
    oneof
      [
        map2
          (fun name input -> Activity_log.Stream_entry.Tool_use (name, input))
          (string_size ~gen:printable (int_range 1 12))
          (string_size ~gen:printable (int_range 0 20));
        map
          (fun text -> Activity_log.Stream_entry.Text_chunk text)
          (string_size ~gen:printable (int_range 0 20));
        map
          (fun reason -> Activity_log.Stream_entry.Finished reason)
          (string_size ~gen:printable (int_range 0 20));
        map
          (fun msg -> Activity_log.Stream_entry.Stream_error msg)
          (string_size ~gen:printable (int_range 0 20));
      ]
  in
  let open Onton_test_support.Test_generators in
  map3
    (fun timestamp patch_id kind ->
      Activity_log.Stream_entry.create ~timestamp ~patch_id ~kind)
    (float_range 0.0 1e12) gen_patch_id gen_kind

let state_with_patch patch_id ~busy ~has_session ~ci_failure_count =
  State.empty
  |> State.update_patch_ctx ~f:(fun ctx ->
         ctx
         |> State.Patch_ctx.set_busy ~patch_id ~value:busy
         |> State.Patch_ctx.set_has_session ~patch_id ~value:has_session
         |> State.Patch_ctx.set_ci_failure_count ~patch_id ~count:ci_failure_count)

let add_resolved_and_pending state ~comment ~patch_id =
  state
  |> State.update_comments ~f:(fun comments ->
         comments
         |> State.Comments.set_resolved ~comment ~value:true
         |> State.Comments.set_pending ~comment ~patch_id ~value:true)

let valid_parser_json ~project_name ~patches_json ~dependency_graph_json =
  Printf.sprintf
    {|{
  "projectName": %S,
  "problemStatement": "",
  "solutionSummary": "",
  "finalStateSpec": "",
  "currentStateAnalysis": "",
  "explicitOpinions": [],
  "acceptanceCriteria": [],
  "dependencyGraph": %s,
  "patches": %s
}|}
    project_name dependency_graph_json patches_json

let () =
  let open QCheck2 in
  let open Onton_test_support.Test_generators in

  let prop_activity_log_trim_preserves_prefixes =
    Test.make ~name:"activity_log: trim preserves recent prefixes" ~count:300
      Gen.(
        triple gen_activity_log (int_range 0 5)
          (list_size (int_range 0 5) make_stream_entry))
      (fun (log, max_entries, stream_entries) ->
        let log =
          List.fold stream_entries ~init:log ~f:Activity_log.add_stream_entry
        in
        let trimmed = Activity_log.trim log ~max:max_entries in
        List.equal Activity_log.Transition_entry.equal
          (Activity_log.recent_transitions trimmed ~limit:(max_entries + 1))
          (Activity_log.recent_transitions log ~limit:max_entries)
        && List.equal Activity_log.Event.equal
             (Activity_log.recent_events trimmed ~limit:(max_entries + 1))
             (Activity_log.recent_events log ~limit:max_entries)
        && List.equal Activity_log.Stream_entry.equal
             (Activity_log.recent_stream_entries trimmed ~limit:(max_entries + 1))
             (Activity_log.recent_stream_entries log ~limit:max_entries))
  in

  let prop_activity_log_recent_limits =
    Test.make ~name:"activity_log: recent accessors respect limit" ~count:300
      Gen.(pair gen_activity_log (int_range 0 5))
      (fun (log, limit) ->
        List.length (Activity_log.recent_transitions log ~limit) <= limit
        && List.length (Activity_log.recent_events log ~limit) <= limit)
  in

  let prop_state_patch_ctx_roundtrips =
    Test.make ~name:"state.patch_ctx: setters round-trip through getters"
      ~count:300
      Gen.(triple gen_patch_id gen_operation_kind bool)
      (fun (patch_id, kind, value) ->
        let branch = Branch.of_string "base" in
        let ctx =
          State.Patch_ctx.empty
          |> State.Patch_ctx.set_queued ~patch_id ~kind ~value
          |> State.Patch_ctx.set_busy ~patch_id ~value
          |> State.Patch_ctx.set_has_pr ~patch_id ~value
          |> State.Patch_ctx.set_has_session ~patch_id ~value
          |> State.Patch_ctx.set_needs_intervention ~patch_id ~value
          |> State.Patch_ctx.set_merged ~patch_id ~value
          |> State.Patch_ctx.set_approved ~patch_id ~value
          |> State.Patch_ctx.set_ci_failure_count ~patch_id ~count:2
          |> State.Patch_ctx.set_base_branch ~patch_id ~branch
        in
        Bool.equal (State.Patch_ctx.is_queued ctx ~patch_id ~kind) value
        && Bool.equal (State.Patch_ctx.is_busy ctx ~patch_id) value
        && Bool.equal (State.Patch_ctx.has_pr ctx ~patch_id) value
        && Bool.equal (State.Patch_ctx.has_session ctx ~patch_id) value
        && Bool.equal (State.Patch_ctx.needs_intervention ctx ~patch_id) value
        && Bool.equal (State.Patch_ctx.is_merged ctx ~patch_id) value
        && Bool.equal (State.Patch_ctx.is_approved ctx ~patch_id) value
        && Int.equal (State.Patch_ctx.ci_failure_count ctx ~patch_id) 2
        && Option.equal Branch.equal (State.Patch_ctx.base_branch ctx ~patch_id)
             (Some branch)
        && List.mem (State.Patch_ctx.known_patch_ids ctx) patch_id
             ~equal:Patch_id.equal)
  in

  let prop_state_comments_roundtrip =
    Test.make ~name:"state.comments: resolved and pending round-trip"
      ~count:300 Gen.(pair gen_comment gen_patch_id)
      (fun (comment, patch_id) ->
        let comments =
          State.Comments.empty
          |> State.Comments.set_resolved ~comment ~value:true
          |> State.Comments.set_pending ~comment ~patch_id ~value:true
        in
        State.Comments.is_resolved comments ~comment
        && State.Comments.is_pending comments ~comment ~patch_id
        && List.mem (State.Comments.all_resolved comments) comment
             ~equal:Comment.equal
        && List.mem (State.Comments.all_pending comments) (comment, patch_id)
             ~equal:(fun (c1, p1) (c2, p2) ->
               Comment.equal c1 c2 && Patch_id.equal p1 p2))
  in

  let prop_priority_queue_is_sorted_and_unique =
    Test.make ~name:"priority: enqueue keeps sorted unique queue" ~count:300
      gen_operation_kind_queue (fun ops ->
        let q = List.fold ops ~init:Priority.empty ~f:Priority.enqueue in
        let l = Priority.to_list q in
        List.is_sorted l ~compare:(fun a b ->
            Int.compare (Priority.priority a) (Priority.priority b))
        && List.equal Operation_kind.equal l
             (List.dedup_and_sort ops ~compare:Operation_kind.compare
             |> List.sort ~compare:(fun a b ->
                    Int.compare (Priority.priority a) (Priority.priority b))))
  in

  let prop_priority_dequeue_returns_peeked_highest =
    Test.make ~name:"priority: dequeue returns current highest" ~count:300
      gen_operation_kind_queue (fun ops ->
        let q = List.fold ops ~init:Priority.empty ~f:Priority.enqueue in
        match (Priority.peek_highest q, Priority.dequeue_highest q) with
        | None, None -> true
        | Some top, Some (deq, rest) ->
            Operation_kind.equal top deq
            && not (Priority.mem rest deq)
        | _ -> false)
  in

  let prop_pr_state_predicates =
    Test.make ~name:"pr_state: predicates reflect fields" ~count:500 gen_pr_state
      (fun pr ->
        Bool.equal (Pr_state.merged pr)
          (Pr_state.equal_pr_status pr.status Pr_state.Merged)
        && Bool.equal (Pr_state.closed pr)
             (Pr_state.equal_pr_status pr.status Pr_state.Closed)
        && Bool.equal (Pr_state.is_draft pr) pr.is_draft
        && Bool.equal (Pr_state.mergeable pr)
             (Pr_state.equal_pr_status pr.status Pr_state.Open
             && Pr_state.equal_merge_state pr.merge_state Pr_state.Mergeable)
        && Bool.equal (Pr_state.merge_ready pr)
             (Pr_state.equal_pr_status pr.status Pr_state.Open
             && pr.merge_ready)
        && Bool.equal (Pr_state.checks_passing pr)
             (Pr_state.equal_check_status pr.check_status Pr_state.Passing)
        && Bool.equal (Pr_state.no_unresolved_comments pr)
             (Int.equal pr.unresolved_comment_count 0)
        && Bool.equal (Pr_state.has_conflict pr)
             (Pr_state.equal_merge_state pr.merge_state Pr_state.Conflicting)
        && Bool.equal (Pr_state.ci_failed pr)
             (Pr_state.equal_check_status pr.check_status Pr_state.Failing))
  in

  let prop_invariants_empty_state =
    Test.make ~name:"invariants: empty state is valid" ~count:1 Gen.unit
      (fun () -> List.is_empty (Invariants.check_invariants State.empty))
  in

  let prop_invariants_negative_ci_detected =
    Test.make ~name:"invariants: negative ci_failure_count detected" ~count:100
      gen_patch_id (fun patch_id ->
        let state = state_with_patch patch_id ~busy:false ~has_session:false ~ci_failure_count:(-1) in
        List.exists (Invariants.check_invariants state) ~f:(fun v ->
            String.equal v.invariant "ci_failure_count_non_negative"))
  in

  let prop_invariants_busy_without_session_detected =
    Test.make ~name:"invariants: busy without session detected" ~count:100
      gen_patch_id (fun patch_id ->
        let state = state_with_patch patch_id ~busy:true ~has_session:false ~ci_failure_count:0 in
        List.exists (Invariants.check_invariants state) ~f:(fun v ->
            String.equal v.invariant "busy_implies_has_session"))
  in

  let prop_invariants_resolved_pending_overlap_detected =
    Test.make
      ~name:"invariants: resolved and pending overlap detected" ~count:100
      Gen.(pair gen_patch_id gen_comment)
      (fun (patch_id, comment) ->
        let state = add_resolved_and_pending State.empty ~comment ~patch_id in
        List.exists (Invariants.check_invariants state) ~f:(fun v ->
            String.equal v.invariant "resolved_not_pending"))
  in

  let prop_invariants_valid_state_stays_clean =
    Test.make ~name:"invariants: valid constructed state has no violations"
      ~count:200
      Gen.(pair gen_patch_id (int_range 0 5))
      (fun (patch_id, ci_failure_count) ->
        let state =
          state_with_patch patch_id ~busy:true ~has_session:true
            ~ci_failure_count
        in
        List.is_empty (Invariants.check_invariants state))
  in

  let prop_gameplan_parser_parses_single_patch =
    Test.make ~name:"gameplan_parser: parses valid single patch JSON" ~count:100
      Gen.(string_size ~gen:printable (int_range 1 20))
      (fun project_name ->
        let json =
          valid_parser_json ~project_name
            ~patches_json:
              {|[
  {"number": 1, "title": "Patch 1", "changes": ["one"]}
]|}
            ~dependency_graph_json:"[]"
        in
        match Gameplan_parser.parse_json_string json with
        | Error _ -> false
        | Ok parsed ->
            List.length parsed.gameplan.patches = 1
            && Map.is_empty parsed.dependency_graph
            && Patch_id.equal (List.hd_exn parsed.gameplan.patches).id
                 (Patch_id.of_string "1"))
  in

  let prop_gameplan_parser_rejects_cycle =
    Test.make ~name:"gameplan_parser: rejects cyclic dependency graph" ~count:1
      Gen.unit (fun () ->
        let json =
          valid_parser_json ~project_name:"cyclic"
            ~patches_json:
              {|[
  {"number": 1, "title": "Patch 1", "changes": []},
  {"number": 2, "title": "Patch 2", "changes": []}
]|}
            ~dependency_graph_json:
              {|[
  {"patch": 1, "dependsOn": [2]},
  {"patch": 2, "dependsOn": [1]}
]|}
        in
        match Gameplan_parser.parse_json_string json with
        | Ok _ -> false
        | Error msg -> String.is_substring msg ~substring:"Cycle detected")
  in

  let prop_gameplan_parser_rejects_missing_dependency_target =
    Test.make
      ~name:"gameplan_parser: rejects dependency on nonexistent patch" ~count:1
      Gen.unit (fun () ->
        let json =
          valid_parser_json ~project_name:"missing-dep"
            ~patches_json:
              {|[
  {"number": 1, "title": "Patch 1", "changes": []}
]|}
            ~dependency_graph_json:
              {|[
  {"patch": 1, "dependsOn": [99]}
]|}
        in
        match Gameplan_parser.parse_json_string json with
        | Ok _ -> false
        | Error msg -> String.is_substring msg ~substring:"nonexistent patch")
  in

  let prop_prompt_substitute_single_pass_and_unknown_preserved =
    Test.make
      ~name:
        "prompt: substitute_variables is single-pass and preserves unknowns"
      ~count:200
      Gen.(pair (string_size ~gen:printable (int_range 1 12))
             (string_size ~gen:printable (int_range 1 12)))
      (fun (_known, replacement) ->
        let template = "{{known}} {{unknown}}" in
        let output =
          Prompt.substitute_variables template
            [ ("known", replacement); ("replacement", "SHOULD_NOT_EXPAND") ]
        in
        String.is_substring output ~substring:replacement
        && String.is_substring output ~substring:"{{unknown}}"
        && not (String.is_substring output ~substring:"SHOULD_NOT_EXPAND"))
  in

  let prop_tui_input_apply_move_stays_in_range =
    Test.make ~name:"tui_input: apply_move stays within valid selection range"
      ~count:500
      Gen.(
        triple (int_range 0 20) (int_range (-1) 25)
          (oneof_list
             [
               Tui_input.Move_up;
               Tui_input.Move_down;
               Tui_input.Page_up;
               Tui_input.Page_down;
               Tui_input.Noop;
               Tui_input.Select;
             ]))
      (fun (count, selected, cmd) ->
        let next = Tui_input.apply_move ~count ~selected cmd in
        if count <= 0 then Int.equal next (-1)
        else next >= -1 && next < count)
  in

  let prop_markdown_render_collapses_blank_lines =
    Test.make
      ~name:"markdown_render: render_to_lines collapses consecutive blanks"
      ~count:200
      Gen.(string_size ~gen:printable (int_range 0 200))
      (fun body ->
        let lines = Markdown_render.render_to_lines body in
        let rec no_double_blank = function
          | a :: b :: rest ->
              let blank s = String.is_empty (String.strip s) in
              if blank a && blank b then false else no_double_blank (b :: rest)
          | _ -> true
        in
        no_double_blank lines)
  in

  let suite =
    [
      prop_activity_log_trim_preserves_prefixes;
      prop_activity_log_recent_limits;
      prop_state_patch_ctx_roundtrips;
      prop_state_comments_roundtrip;
      prop_priority_queue_is_sorted_and_unique;
      prop_priority_dequeue_returns_peeked_highest;
      prop_pr_state_predicates;
      prop_invariants_empty_state;
      prop_invariants_negative_ci_detected;
      prop_invariants_busy_without_session_detected;
      prop_invariants_resolved_pending_overlap_detected;
      prop_invariants_valid_state_stays_clean;
      prop_gameplan_parser_parses_single_patch;
      prop_gameplan_parser_rejects_cycle;
      prop_gameplan_parser_rejects_missing_dependency_target;
      prop_prompt_substitute_single_pass_and_unknown_preserved;
      prop_tui_input_apply_move_stays_in_range;
      prop_markdown_render_collapses_blank_lines;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode
