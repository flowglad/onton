open Base
open Onton.Types
open Onton.Patch_agent

let all_ops =
  Operation_kind.[ Rebase; Human; Merge_conflict; Ci; Review_comments ]

let gen_pid =
  QCheck2.Gen.(
    map Patch_id.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 12)))

let gen_branch =
  QCheck2.Gen.(
    map Branch.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 20)))

let gen_op = QCheck2.Gen.oneof_list all_ops
let feedback_ops = Operation_kind.[ Human; Merge_conflict; Ci; Review_comments ]
let gen_feedback_op = QCheck2.Gen.oneof_list feedback_ops

(** Simulate the full start+PR-confirmed flow for tests that need has_pr=true.
*)
let start_with_pr t ~base_branch =
  let t = start t ~base_branch in
  set_pr_number t (Pr_number.of_int 1)

let () =
  let open QCheck2 in
  let pid0 = Patch_id.of_string "p" in
  let br0 = Branch.of_string "main" in
  let tests =
    [
      (* -- create -- *)
      Test.make ~name:"create yields clean initial state" gen_pid (fun pid ->
          let t = create ~branch:br0 pid in
          (not (has_pr t))
          && (not t.has_session) && (not t.busy) && (not t.merged)
          && (not (needs_intervention t))
          && List.is_empty t.queue && (not t.satisfies) && (not t.changed)
          && (not t.has_conflict)
          && Option.is_none t.base_branch
          && t.ci_failure_count = 0
          && equal_session_fallback t.session_fallback Fresh_available
          && t.start_attempts_without_pr = 0
          && List.is_empty t.human_messages
          && List.is_empty t.ci_checks && (not t.merge_ready)
          && not t.checks_passing);
      (* -- enqueue is idempotent -- *)
      Test.make ~name:"enqueue is idempotent"
        Gen.(triple gen_pid gen_branch gen_op)
        (fun (pid, br, k) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a1 = enqueue a k in
          let a2 = enqueue a1 k in
          equal a1 a2);
      (* -- enqueue adds operation -- *)
      Test.make ~name:"enqueue adds operation to queue"
        Gen.(pair (pair gen_pid gen_branch) gen_op)
        (fun ((pid, br), k) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = enqueue a k in
          List.mem a.queue k ~equal:Operation_kind.equal);
      (* -- start postconditions -- *)
      Test.make ~name:"start sets has_session, busy, satisfies (not has_pr)"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create ~branch:br pid |> fun a -> start a ~base_branch:br in
          (not (has_pr a))
          && a.has_session && a.busy && a.satisfies
          && Option.equal Branch.equal a.base_branch (Some br));
      (* -- start twice raises -- *)
      Test.make ~name:"start on already-started raises (busy)"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create ~branch:br pid |> fun a -> start a ~base_branch:br in
          match start a ~base_branch:br with
          | exception Invalid_argument _ -> true
          | _ -> false);
      Test.make ~name:"start on has_pr raises"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create ~branch:br pid |> fun a -> start a ~base_branch:br in
          let a = set_pr_number a (Pr_number.of_int 1) in
          let a = complete a in
          match start a ~base_branch:br with
          | exception Invalid_argument _ -> true
          | _ -> false);
      (* -- complete clears busy -- *)
      Test.make ~name:"complete clears busy"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          not a.busy);
      (* -- complete on non-busy is idempotent no-op -- *)
      Test.make ~name:"complete on non-busy is no-op" gen_pid (fun pid ->
          let a = create ~branch:br0 pid in
          let a' = complete a in
          equal a a');
      (* -- respond requires has_pr -- *)
      Test.make ~name:"respond requires has_pr"
        Gen.(pair gen_pid gen_op)
        (fun (pid, k) ->
          let a = create ~branch:br0 pid in
          match respond a k with
          | exception Invalid_argument msg ->
              String.is_substring msg ~substring:"no PR"
          | _ -> false);
      (* -- respond requires not busy -- *)
      Test.make ~name:"respond requires not busy"
        Gen.(triple gen_pid gen_branch gen_op)
        (fun (pid, br, k) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          match respond a k with
          | exception Invalid_argument msg ->
              String.is_substring msg ~substring:"busy"
          | _ -> false);
      (* -- respond requires op in queue -- *)
      Test.make ~name:"respond requires op in queue"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          match respond a Operation_kind.Ci with
          | exception Invalid_argument _ -> true
          | _ -> false);
      (* -- respond sets busy and has_session -- *)
      Test.make ~name:"respond sets busy and has_session"
        Gen.(triple gen_pid gen_branch gen_feedback_op)
        (fun (pid, br, k) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = enqueue a k in
          let a = respond a k in
          a.busy && a.has_session);
      (* -- respond removes op from queue -- *)
      Test.make ~name:"respond removes op from queue"
        Gen.(triple gen_pid gen_branch gen_feedback_op)
        (fun (pid, br, k) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = enqueue a k in
          let a = respond a k in
          not (List.mem a.queue k ~equal:Operation_kind.equal));
      (* -- mark_merged sets merged -- *)
      Test.make ~name:"mark_merged sets merged" gen_pid (fun pid ->
          let a = create ~branch:br0 pid in
          let a = mark_merged a in
          a.merged);
      (* -- priority: low rejected AND high accepted for all pairs -- *)
      Test.make
        ~name:"priority enforced: low rejected and high accepted for all pairs"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let rank = Onton.Priority.priority in
          let dispatch a k =
            if Operation_kind.equal k Operation_kind.Rebase then
              rebase a ~base_branch:(Branch.of_string "new-base")
            else respond a k
          in
          List.for_all all_ops ~f:(fun high ->
              List.for_all all_ops ~f:(fun low ->
                  if rank high >= rank low then true
                  else
                    let a =
                      create ~branch:br pid |> fun a ->
                      start_with_pr a ~base_branch:br
                    in
                    let a = complete a in
                    let a = enqueue a high in
                    let a = enqueue a low in
                    let low_rejected =
                      match dispatch a low with
                      | exception Invalid_argument _ -> true
                      | _ -> false
                    in
                    let high_accepted =
                      match dispatch a high with
                      | exception _ -> false
                      | a' ->
                          a'.busy
                          && (not
                                (List.mem a'.queue high
                                   ~equal:Operation_kind.equal))
                          && List.mem a'.queue low ~equal:Operation_kind.equal
                    in
                    low_rejected && high_accepted)));
      (* -- full lifecycle -- *)
      Test.make ~name:"full lifecycle"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create ~branch:br0 pid in
          let a = start_with_pr a ~base_branch:br in
          let busy_after_start = a.busy in
          let a = complete a in
          let idle_after_complete = not a.busy in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:(Branch.of_string "new-base") in
          let busy_after_rebase = a.busy in
          let a = complete a in
          busy_after_start && idle_after_complete && busy_after_rebase
          && (not a.busy)
          && not (needs_intervention a));
      (* -- respond Human clears satisfies -- *)
      Test.make ~name:"respond Human clears satisfies" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = enqueue a Operation_kind.Human in
          let a = respond a Operation_kind.Human in
          not a.satisfies);
      (* -- respond Ci sets changed -- *)
      Test.make ~name:"respond Ci sets changed" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          a.changed);
      Test.make ~name:"respond Ci sets busy with current_op Ci" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = enqueue a Operation_kind.Ci in
            let a = respond a Operation_kind.Ci in
            a.busy
            && Option.equal Operation_kind.equal a.current_op
                 (Some Operation_kind.Ci)
          with _ -> false);
      Test.make ~name:"complete Ci clears busy and current_op" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = enqueue a Operation_kind.Ci in
            let a = respond a Operation_kind.Ci in
            let a = complete a in
            (not a.busy) && Option.is_none a.current_op
          with _ -> false);
      (* -- respond Merge_conflict preserves has_conflict -- *)
      Test.make ~name:"respond Merge_conflict preserves has_conflict" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = set_has_conflict a in
          let a = enqueue a Operation_kind.Merge_conflict in
          let a = respond a Operation_kind.Merge_conflict in
          a.has_conflict);
      (* -- respond Review_comments always sets changed (lazy fetch) -- *)
      Test.make ~name:"respond Review_comments always sets changed (lazy fetch)"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = enqueue a Operation_kind.Review_comments in
          let a = respond a Operation_kind.Review_comments in
          a.changed);
      (* -- add_human_message prepends to list -- *)
      Test.make ~name:"add_human_message prepends to list"
        Gen.(pair gen_pid (list_size (int_range 1 5) (pure "msg")))
        (fun (pid, msgs) ->
          let a = create ~branch:br0 pid in
          let a =
            List.fold msgs ~init:a ~f:(fun a m -> add_human_message a m)
          in
          List.length a.human_messages = List.length msgs);
      (* -- respond Human moves messages to inflight -- *)
      Test.make ~name:"respond Human moves inbox to inflight" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = add_human_message a "hello" in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            List.is_empty a.human_messages
            && not (List.is_empty a.inflight_human_messages)
          with _ -> false);
      (* -- complete clears inflight_human_messages -- *)
      Test.make ~name:"complete clears inflight_human_messages" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = add_human_message a "hello" in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            let a = complete a in
            List.is_empty a.human_messages
            && List.is_empty a.inflight_human_messages
          with _ -> false);
      (* -- messages enqueued during busy are preserved -- *)
      Test.make ~name:"messages enqueued during busy survive complete" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = add_human_message a "first" in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            (* message arrives while busy *)
            let a = add_human_message a "second" in
            let a = complete a in
            List.length a.human_messages = 1
            && List.is_empty a.inflight_human_messages
          with _ -> false);
      (* -- respond Review_comments does not clear human_messages -- *)
      Test.make ~name:"respond Review_comments preserves human_messages"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = add_human_message a "hello" in
          let a = enqueue a Operation_kind.Review_comments in
          let a = respond a Operation_kind.Review_comments in
          List.length a.human_messages = 1);
      (* -- property: respond Human partitions messages into inflight -- *)
      Test.make ~name:"respond Human: total messages = inbox + inflight"
        Gen.(
          pair gen_pid
            (list_size (int_range 1 10) (string_size ~gen:printable (pure 5))))
        (fun (pid, msgs) ->
          try
            let a =
              create ~branch:br0 pid |> fun a ->
              start_with_pr a ~base_branch:br0
            in
            let a = complete a in
            let a =
              List.fold msgs ~init:a ~f:(fun a m -> add_human_message a m)
            in
            let n_before = List.length a.human_messages in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            List.length a.inflight_human_messages = n_before
            && List.is_empty a.human_messages
          with _ -> false);
      (* -- property: messages added during busy are disjoint from inflight -- *)
      Test.make
        ~name:
          "inflight disjoint: busy-time messages survive, pre-respond cleared"
        Gen.(
          triple gen_pid
            (list_size (int_range 1 5) (string_size ~gen:printable (pure 4)))
            (list_size (int_range 1 5) (string_size ~gen:printable (pure 4))))
        (fun (pid, before_msgs, during_msgs) ->
          try
            let a =
              create ~branch:br0 pid |> fun a ->
              start_with_pr a ~base_branch:br0
            in
            let a = complete a in
            let a =
              List.fold before_msgs ~init:a ~f:(fun a m ->
                  add_human_message a m)
            in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            let a =
              List.fold during_msgs ~init:a ~f:(fun a m ->
                  add_human_message a m)
            in
            let a = complete a in
            List.length a.human_messages = List.length during_msgs
            && List.is_empty a.inflight_human_messages
          with _ -> false);
      (* -- property: inbox is always empty after respond Human, so any
         empty-delivery guard must check inflight, not inbox -- *)
      Test.make
        ~name:
          "respond Human: inbox always empty (guard must use inflight, not \
           inbox)"
        Gen.(
          pair gen_pid
            (list_size (int_range 1 10) (string_size ~gen:printable (pure 5))))
        (fun (pid, msgs) ->
          try
            let a =
              create ~branch:br0 pid |> fun a ->
              start_with_pr a ~base_branch:br0
            in
            let a = complete a in
            let a =
              List.fold msgs ~init:a ~f:(fun a m -> add_human_message a m)
            in
            let a = enqueue a Operation_kind.Human in
            let a = respond a Operation_kind.Human in
            (* A guard that checks human_messages here would always skip
               delivery — the bug we're guarding against. *)
            List.is_empty a.human_messages
            && not (List.is_empty a.inflight_human_messages)
          with _ -> false);
      (* -- property: CI checks remain accessible during Ci respond for
         fresh-fetch filtering at delivery time -- *)
      Test.make
        ~name:
          "respond Ci: ci_checks remain on agent for delivery-time filtering"
        Gen.(
          pair gen_pid
            (list_size (int_range 1 5)
               (pair
                  (string_size ~gen:printable (pure 8))
                  (oneof_list
                     [ "failure"; "success"; "error"; "neutral"; "timed_out" ]))))
        (fun (pid, check_specs) ->
          try
            let a =
              create ~branch:br0 pid |> fun a ->
              start_with_pr a ~base_branch:br0
            in
            let a = complete a in
            let checks =
              List.map check_specs ~f:(fun (name, conclusion) ->
                  Ci_check.
                    {
                      name;
                      conclusion;
                      details_url = None;
                      description = None;
                      started_at = None;
                    })
            in
            let a = set_ci_checks a checks in
            let a = enqueue a Operation_kind.Ci in
            let a = respond a Operation_kind.Ci in
            (* ci_checks must still be readable so the runner can filter
               for failures at delivery time *)
            let failure_conclusions =
              Onton.Patch_decision.failure_conclusions
            in
            let has_any_failure =
              List.exists a.ci_checks ~f:(fun c ->
                  List.mem failure_conclusions c.Ci_check.conclusion
                    ~equal:String.equal)
            in
            let input_has_failure =
              List.exists check_specs ~f:(fun (_name, conc) ->
                  List.mem failure_conclusions conc ~equal:String.equal)
            in
            Bool.equal has_any_failure input_has_failure
          with _ -> false);
      (* -- property: CI with all-passing checks at delivery time yields
         empty failure list (delivery should be skipped) -- *)
      Test.make
        ~name:
          "respond Ci: all-passing checks -> empty failure filter (skip \
           delivery)"
        Gen.(
          pair gen_pid
            (list_size (int_range 1 5) (string_size ~gen:printable (pure 8))))
        (fun (pid, check_names) ->
          try
            let a =
              create ~branch:br0 pid |> fun a ->
              start_with_pr a ~base_branch:br0
            in
            let a = complete a in
            let checks =
              List.map check_names ~f:(fun name ->
                  Ci_check.
                    {
                      name;
                      conclusion = "success";
                      details_url = None;
                      description = None;
                      started_at = None;
                    })
            in
            let a = set_ci_checks a checks in
            let a = enqueue a Operation_kind.Ci in
            let a = respond a Operation_kind.Ci in
            let failure_conclusions =
              Onton.Patch_decision.failure_conclusions
            in
            not
              (List.exists a.ci_checks ~f:(fun c ->
                   List.mem failure_conclusions c.Ci_check.conclusion
                     ~equal:String.equal))
          with _ -> false);
      (* -- property: non-Human respond preserves inbox, inflight unchanged -- *)
      Test.make ~name:"non-Human respond leaves human_messages untouched"
        Gen.(
          triple gen_pid
            (list_size (int_range 0 5) (string_size ~gen:printable (pure 4)))
            (oneof_list Operation_kind.[ Ci; Review_comments; Merge_conflict ]))
        (fun (pid, msgs, op) ->
          try
            let a =
              create ~branch:br0 pid |> fun a ->
              start_with_pr a ~base_branch:br0
            in
            let a = complete a in
            let a =
              List.fold msgs ~init:a ~f:(fun a m -> add_human_message a m)
            in
            let n = List.length a.human_messages in
            let a = enqueue a op in
            let a = respond a op in
            List.length a.human_messages = n
            && List.is_empty a.inflight_human_messages
          with _ -> false);
      (* -- 2 ci failures does NOT trigger needs_intervention (boundary) -- *)
      Test.make ~name:"2 ci failures no intervention (boundary)" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          not (needs_intervention a));
      (* -- 3 ci failures triggers needs_intervention -- *)
      Test.make ~name:"3 ci failures triggers intervention" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          needs_intervention a);
      (* -- session_failed triggers needs_intervention -- *)
      Test.make ~name:"session_failed triggers intervention" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = set_session_failed a in
          let a = set_tried_fresh a in
          (* session_fallback = Given_up and Human not in queue →
             needs_intervention *)
          needs_intervention a);
      (* -- Human queued suppresses intervention from session_failed -- *)
      Test.make ~name:"Human queued suppresses intervention (session_failed)"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = set_session_failed a in
          let a = set_tried_fresh a in
          let a = enqueue a Operation_kind.Human in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:(Branch.of_string "new-base") in
          let a = complete a in
          not (needs_intervention a));
      (* -- Human queued suppresses intervention from 3 ci failures -- *)
      Test.make ~name:"Human queued suppresses intervention (3 ci failures)"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = enqueue a Operation_kind.Human in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:(Branch.of_string "new-base") in
          let a = complete a in
          not (needs_intervention a));
      (* -- reset_intervention_state clears derived intervention -- *)
      Test.make ~name:"reset_intervention_state clears intervention" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = set_session_failed a in
          let a = set_tried_fresh a in
          (* session_fallback = Given_up → needs_intervention *)
          let triggered = needs_intervention a in
          let a = reset_intervention_state a in
          triggered && not (needs_intervention a));
      (* -- create has no pr_number -- *)
      Test.make ~name:"create has no pr_number" gen_pid (fun pid ->
          let a = create ~branch:br0 pid in
          Option.is_none a.pr_number);
      (* -- set_pr_number stores pr_number -- *)
      Test.make ~name:"set_pr_number stores pr_number"
        Gen.(pair gen_pid (map Pr_number.of_int (int_range 1 9999)))
        (fun (pid, pr) ->
          let a = create ~branch:br0 pid in
          let a = set_pr_number a pr in
          Option.equal Pr_number.equal a.pr_number (Some pr));
      (* -- start clears ci_checks -- *)
      Test.make ~name:"start clears ci_checks"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create ~branch:br0 pid in
          let a = start a ~base_branch:br in
          let check =
            Ci_check.
              {
                name = "build";
                conclusion = "success";
                details_url = None;
                description = None;
                started_at = None;
              }
          in
          let a = set_ci_checks a [ check ] in
          let a = complete a in
          let a = mark_merged a in
          let a =
            Onton.Patch_agent.restore ~patch_id:a.patch_id ~branch:br
              ~pr_number:None ~has_session:false ~busy:false ~merged:false
              ~queue:[] ~satisfies:false ~changed:false ~has_conflict:false
              ~base_branch:None ~notified_base_branch:None ~ci_failure_count:0
              ~session_fallback:Fresh_available ~human_messages:[]
              ~inflight_human_messages:[] ~ci_checks:a.ci_checks
              ~merge_ready:false ~is_draft:false ~pr_body_delivered:false
              ~start_attempts_without_pr:0 ~conflict_noop_count:0
              ~no_commits_push_count:0 ~branch_rebased_onto:None
              ~checks_passing:false ~current_op:None ~current_message_id:None
              ~generation:0 ~worktree_path:None ~branch_blocked:false
              ~llm_session_id:None
          in
          let a = start a ~base_branch:br in
          List.is_empty a.ci_checks);
      (* -- set_ci_checks stores checks -- *)
      Test.make ~name:"set_ci_checks stores checks" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create ~branch:br pid |> fun a -> start a ~base_branch:br in
          let check =
            Ci_check.
              {
                name = "build";
                conclusion = "success";
                details_url = None;
                description = None;
                started_at = None;
              }
          in
          let a = set_ci_checks a [ check ] in
          List.length a.ci_checks = 1);
      (* -- set_tried_fresh from Fresh_available -> Tried_fresh -- *)
      Test.make ~name:"set_tried_fresh from Fresh_available" gen_pid (fun pid ->
          let a = create ~branch:br0 pid in
          let a = set_tried_fresh a in
          equal_session_fallback a.session_fallback Tried_fresh);
      (* -- set_tried_fresh is no-op from Tried_fresh or Given_up -- *)
      Test.make ~name:"set_tried_fresh advances Tried_fresh to Given_up" gen_pid
        (fun pid ->
          let a = create ~branch:br0 pid in
          let a = set_tried_fresh a in
          let a = set_tried_fresh a in
          equal_session_fallback a.session_fallback Given_up);
      (* -- set_tried_fresh is no-op from Given_up -- *)
      Test.make ~name:"set_tried_fresh no-op from Given_up" gen_pid (fun pid ->
          let a = create ~branch:br0 pid in
          let a = set_tried_fresh a in
          let a = set_tried_fresh a in
          let a = set_tried_fresh a in
          equal_session_fallback a.session_fallback Given_up);
      (* -- rebase sets busy, preserves has_session, updates base_branch,
           drains rebase queue -- *)
      Test.make ~name:"rebase postconditions (has_session=true)"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let new_base = Branch.of_string "new-base" in
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:new_base in
          a.busy && a.has_session
          && Option.equal Branch.equal a.base_branch (Some new_base)
          && not
               (List.mem a.queue Operation_kind.Rebase
                  ~equal:Operation_kind.equal));
      Test.make ~name:"rebase postconditions (has_session=false -> true)"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let new_base = Branch.of_string "new-base" in
          (* Construct agent with has_pr=true but has_session=false via
             restore — rebase should promote has_session to true. *)
          let a =
            restore ~patch_id:pid ~branch:br
              ~pr_number:(Some (Pr_number.of_int 1))
              ~has_session:false ~busy:false ~merged:false ~queue:[]
              ~satisfies:true ~changed:false ~has_conflict:false
              ~base_branch:(Some br) ~notified_base_branch:(Some br)
              ~ci_failure_count:0 ~session_fallback:Fresh_available
              ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
              ~merge_ready:false ~is_draft:false ~pr_body_delivered:false
              ~start_attempts_without_pr:0 ~conflict_noop_count:0
              ~no_commits_push_count:0 ~branch_rebased_onto:None
              ~checks_passing:false ~current_op:None ~current_message_id:None
              ~generation:0 ~worktree_path:None ~branch_blocked:false
              ~llm_session_id:None
          in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:new_base in
          a.busy && a.has_session
          && Option.equal Branch.equal a.base_branch (Some new_base)
          && not
               (List.mem a.queue Operation_kind.Rebase
                  ~equal:Operation_kind.equal));
      (* -- rebase preserves other queues -- *)
      Test.make ~name:"rebase preserves other queues"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = enqueue a Operation_kind.Rebase in
          let a = enqueue a Operation_kind.Ci in
          let a = rebase a ~base_branch:(Branch.of_string "new-base") in
          List.mem a.queue Operation_kind.Ci ~equal:Operation_kind.equal
          && not
               (List.mem a.queue Operation_kind.Rebase
                  ~equal:Operation_kind.equal));
      (* -- respond rejects Rebase -- *)
      Test.make ~name:"respond rejects Rebase"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a =
            create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
          in
          let a = complete a in
          let a = enqueue a Operation_kind.Rebase in
          match respond a Operation_kind.Rebase with
          | exception Invalid_argument msg ->
              String.is_substring msg ~substring:"not a feedback"
          | _ -> false);
      (* -- is_approved true when all conditions met -- *)
      Test.make ~name:"is_approved true when all conditions met" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = set_is_draft a false in
            let a = set_merge_ready a true in
            is_approved a ~main_branch:br0
          with _ -> false);
      (* -- is_approved false without has_pr -- *)
      Test.make ~name:"is_approved false without has_pr" ~count:1
        Gen.(pure pid0)
        (fun pid ->
          let a = create ~branch:br0 pid in
          let a = set_merge_ready a true in
          not (is_approved a ~main_branch:br0));
      (* -- is_approved false when busy -- *)
      Test.make ~name:"is_approved false when busy" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = set_merge_ready a true in
            not (is_approved a ~main_branch:br0)
          with _ -> false);
      (* -- is_approved false when not merge_ready -- *)
      Test.make ~name:"is_approved false when not merge_ready" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            not (is_approved a ~main_branch:br0)
          with _ -> false);
      (* -- is_approved false when needs_intervention -- *)
      Test.make ~name:"is_approved false when needs_intervention" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = increment_ci_failure_count a in
            let a = increment_ci_failure_count a in
            let a = increment_ci_failure_count a in
            let a = set_merge_ready a true in
            not (is_approved a ~main_branch:br0)
          with _ -> false);
      (* -- is_approved false when base_branch is not main -- *)
      Test.make ~name:"is_approved false when base_branch is not main" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = set_merge_ready a true in
            let other = Branch.of_string "feature/dep" in
            not (is_approved a ~main_branch:other)
          with _ -> false);
      Test.make ~name:"is_approved false when draft" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = set_merge_ready a true in
            let a = set_is_draft a true in
            not (is_approved a ~main_branch:br0)
          with _ -> false);
      Test.make ~name:"set_pr_number resets bootstrap lifecycle facts" ~count:1
        Gen.(pure pid0)
        (fun pid ->
          let a = create ~branch:br0 pid in
          let a = increment_start_attempts_without_pr a in
          let a = set_pr_body_delivered a true in
          let a = set_pr_number a (Pr_number.of_int 7) in
          has_pr a && a.is_draft && (not a.pr_body_delivered)
          && a.start_attempts_without_pr = 0);
      Test.make ~name:"on_pr_discovery_failure increments durable attempt count"
        ~count:1
        Gen.(pure pid0)
        (fun pid ->
          let a = create ~branch:br0 pid in
          let a = on_pr_discovery_failure a in
          a.start_attempts_without_pr = 1);
      (* -- respond invalidates merge_ready -- *)
      Test.make ~name:"respond invalidates merge_ready" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = set_merge_ready a true in
            let a = enqueue a Operation_kind.Ci in
            let a = respond a Operation_kind.Ci in
            let a = complete a in
            not a.merge_ready
          with _ -> false);
      (* -- rebase invalidates merge_ready -- *)
      Test.make ~name:"rebase invalidates merge_ready" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = set_merge_ready a true in
            let a = enqueue a Operation_kind.Rebase in
            let a = rebase a ~base_branch:br in
            not a.merge_ready
          with _ -> false);
      (* -- clear_has_conflict clears flag -- *)
      Test.make ~name:"clear_has_conflict clears flag" ~count:1
        Gen.(pure pid0)
        (fun pid ->
          let a = create ~branch:br0 pid in
          let a = set_has_conflict a in
          let before = a.has_conflict in
          let a = clear_has_conflict a in
          before && not a.has_conflict);
      (* -- base_branch_changed false after start -- *)
      Test.make ~name:"base_branch_changed false after start"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let a = create ~branch:br pid |> fun a -> start a ~base_branch:br in
            not (base_branch_changed a)
          with _ -> false);
      (* -- base_branch_changed true after rebase to different base -- *)
      Test.make ~name:"base_branch_changed true after rebase to different base"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let new_base = Branch.of_string "rebased-target" in
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = enqueue a Operation_kind.Rebase in
            let a = rebase a ~base_branch:new_base in
            base_branch_changed a
          with _ -> false);
      (* -- set_notified_base_branch clears base_branch_changed -- *)
      Test.make ~name:"set_notified_base_branch clears base_branch_changed"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let new_base = Branch.of_string "rebased-target" in
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = enqueue a Operation_kind.Rebase in
            let a = rebase a ~base_branch:new_base in
            let changed_before = base_branch_changed a in
            let a = set_notified_base_branch a new_base in
            changed_before && not (base_branch_changed a)
          with _ -> false);
      (* -- rebase to same base does not trigger base_branch_changed -- *)
      Test.make ~name:"rebase to same base: base_branch_changed false"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let a =
              create ~branch:br pid |> fun a -> start_with_pr a ~base_branch:br
            in
            let a = complete a in
            let a = enqueue a Operation_kind.Rebase in
            let a = rebase a ~base_branch:br in
            not (base_branch_changed a)
          with _ -> false);
      (* -- set_base_branch before start does not trigger base_branch_changed -- *)
      Test.make ~name:"set_base_branch before start: base_branch_changed false"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          try
            let a = create ~branch:br pid in
            let a = set_base_branch a br in
            not (base_branch_changed a)
          with _ -> false);
      (* -- create_adhoc stores real branch -- *)
      Test.make ~name:"create_adhoc stores real branch"
        Gen.(
          triple gen_pid gen_branch (map Pr_number.of_int (int_range 1 9999)))
        (fun (pid, br, pr) ->
          let a = create_adhoc ~patch_id:pid ~branch:br ~pr_number:pr in
          Branch.equal a.branch br);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t);
  Stdlib.print_endline "patch_agent: all tests passed"
