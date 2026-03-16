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

let () =
  let open QCheck2 in
  let pid0 = Patch_id.of_string "p" in
  let br0 = Branch.of_string "main" in
  let c_valid =
    Comment.{ id = Comment_id.of_int 1; body = "fix"; path = None; line = None }
  in
  let c_invalid =
    Comment.{ id = Comment_id.of_int 2; body = "nit"; path = None; line = None }
  in
  let tests =
    [
      (* -- create -- *)
      Test.make ~name:"create yields clean initial state" gen_pid (fun pid ->
          let t = create pid in
          (not t.has_pr) && (not t.has_session) && (not t.busy)
          && (not t.merged) && (not t.needs_intervention)
          && List.is_empty t.queue && (not t.satisfies) && (not t.changed)
          && (not t.has_conflict)
          && Option.is_none t.base_branch
          && t.ci_failure_count = 0
          && equal_session_fallback t.session_fallback Fresh_available
          && List.is_empty t.pending_comments
          && List.is_empty t.ci_checks
          && Set.is_empty t.addressed_comment_ids);
      (* -- enqueue is idempotent -- *)
      Test.make ~name:"enqueue is idempotent"
        Gen.(triple gen_pid gen_branch gen_op)
        (fun (pid, br, k) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a1 = enqueue a k in
          let a2 = enqueue a1 k in
          equal a1 a2);
      (* -- enqueue adds operation -- *)
      Test.make ~name:"enqueue adds operation to queue"
        Gen.(pair (pair gen_pid gen_branch) gen_op)
        (fun ((pid, br), k) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = enqueue a k in
          List.mem a.queue k ~equal:Operation_kind.equal);
      (* -- start postconditions -- *)
      Test.make ~name:"start sets has_pr, has_session, busy, satisfies"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          a.has_pr && a.has_session && a.busy && a.satisfies
          && Option.equal Branch.equal a.base_branch (Some br));
      (* -- start twice raises -- *)
      Test.make ~name:"start on already-started raises"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          match start a ~base_branch:br with
          | exception Invalid_argument _ -> true
          | _ -> false);
      (* -- complete clears busy -- *)
      Test.make ~name:"complete clears busy"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          not a.busy);
      (* -- complete on non-busy raises -- *)
      Test.make ~name:"complete on non-busy raises" gen_pid (fun pid ->
          let a = create pid in
          match complete a with
          | exception Invalid_argument _ -> true
          | _ -> false);
      (* -- respond requires has_pr -- *)
      Test.make ~name:"respond requires has_pr"
        Gen.(pair gen_pid gen_op)
        (fun (pid, k) ->
          let a = create pid in
          match respond a k with
          | exception Invalid_argument msg ->
              String.is_substring msg ~substring:"no PR"
          | _ -> false);
      (* -- respond requires not busy -- *)
      Test.make ~name:"respond requires not busy"
        Gen.(triple gen_pid gen_branch gen_op)
        (fun (pid, br, k) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          match respond a k with
          | exception Invalid_argument msg ->
              String.is_substring msg ~substring:"busy"
          | _ -> false);
      (* -- respond requires op in queue -- *)
      Test.make ~name:"respond requires op in queue"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          match respond a Operation_kind.Ci with
          | exception Invalid_argument _ -> true
          | _ -> false);
      (* -- respond sets busy and has_session -- *)
      Test.make ~name:"respond sets busy and has_session"
        Gen.(triple gen_pid gen_branch gen_op)
        (fun (pid, br, k) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = enqueue a k in
          let a = respond a k in
          a.busy && a.has_session);
      (* -- respond removes op from queue -- *)
      Test.make ~name:"respond removes op from queue"
        Gen.(triple gen_pid gen_branch gen_op)
        (fun (pid, br, k) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = enqueue a k in
          let a = respond a k in
          not (List.mem a.queue k ~equal:Operation_kind.equal));
      (* -- mark_merged sets merged -- *)
      Test.make ~name:"mark_merged sets merged" gen_pid (fun pid ->
          let a = create pid in
          let a = mark_merged a in
          a.merged);
      (* -- priority: low rejected AND high accepted for all pairs -- *)
      Test.make
        ~name:"priority enforced: low rejected and high accepted for all pairs"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let rank = Onton.Priority.priority in
          List.for_all all_ops ~f:(fun high ->
              List.for_all all_ops ~f:(fun low ->
                  if rank high >= rank low then true
                  else
                    let a = create pid |> fun a -> start a ~base_branch:br in
                    let a = complete a in
                    let a = enqueue a high in
                    let a = enqueue a low in
                    let low_rejected =
                      match respond a low with
                      | exception Invalid_argument _ -> true
                      | _ -> false
                    in
                    let high_accepted =
                      match respond a high with
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
          let a = create pid in
          let a = start a ~base_branch:br in
          let busy_after_start = a.busy in
          let a = complete a in
          let idle_after_complete = not a.busy in
          let a = enqueue a Operation_kind.Rebase in
          let a = respond a Operation_kind.Rebase in
          let busy_after_respond = a.busy in
          let a = complete a in
          busy_after_start && idle_after_complete && busy_after_respond
          && (not a.busy) && not a.needs_intervention);
      (* -- respond Human clears satisfies -- *)
      Test.make ~name:"respond Human clears satisfies" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = enqueue a Operation_kind.Human in
          let a = respond a Operation_kind.Human in
          not a.satisfies);
      (* -- respond Ci sets changed -- *)
      Test.make ~name:"respond Ci sets changed" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          a.changed);
      (* -- respond Merge_conflict clears has_conflict -- *)
      Test.make ~name:"respond Merge_conflict clears has_conflict" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = set_has_conflict a in
          let a = enqueue a Operation_kind.Merge_conflict in
          let a = respond a Operation_kind.Merge_conflict in
          not a.has_conflict);
      (* -- respond Review_comments clears pending_comments -- *)
      Test.make ~name:"respond Review_comments clears pending_comments" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = add_pending_comment a c_valid ~valid:true in
          let a = enqueue a Operation_kind.Review_comments in
          let a = respond a Operation_kind.Review_comments in
          List.is_empty a.pending_comments);
      (* -- respond Review_comments sets changed with valid comment -- *)
      Test.make ~name:"respond Review_comments sets changed with valid comment"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = add_pending_comment a c_valid ~valid:true in
          let a = enqueue a Operation_kind.Review_comments in
          let a = respond a Operation_kind.Review_comments in
          a.changed);
      (* -- respond Review_comments does not set changed without valid comment -- *)
      Test.make ~name:"respond Review_comments no changed without valid comment"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = add_pending_comment a c_invalid ~valid:false in
          let a = enqueue a Operation_kind.Review_comments in
          let a = respond a Operation_kind.Review_comments in
          not a.changed);
      (* -- 2 ci failures does NOT trigger needs_intervention (boundary) -- *)
      Test.make ~name:"2 ci failures no intervention (boundary)" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          let a = complete a in
          not a.needs_intervention);
      (* -- 3 ci failures triggers needs_intervention -- *)
      Test.make ~name:"3 ci failures triggers intervention" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          let a = complete a in
          a.needs_intervention);
      (* -- session_failed triggers needs_intervention -- *)
      Test.make ~name:"session_failed triggers intervention" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = set_session_failed a in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          let a = complete a in
          a.needs_intervention);
      (* -- Human queued suppresses intervention from session_failed -- *)
      Test.make ~name:"Human queued suppresses intervention (session_failed)"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = set_session_failed a in
          let a = enqueue a Operation_kind.Human in
          let a = enqueue a Operation_kind.Rebase in
          let a = respond a Operation_kind.Rebase in
          let a = complete a in
          not a.needs_intervention);
      (* -- Human queued suppresses intervention from 3 ci failures -- *)
      Test.make ~name:"Human queued suppresses intervention (3 ci failures)"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = enqueue a Operation_kind.Human in
          let a = enqueue a Operation_kind.Rebase in
          let a = respond a Operation_kind.Rebase in
          let a = complete a in
          not a.needs_intervention);
      (* -- clear_needs_intervention resets flag -- *)
      Test.make ~name:"clear_needs_intervention resets flag" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          let a = set_session_failed a in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          let a = complete a in
          let triggered = a.needs_intervention in
          let a = clear_needs_intervention a in
          triggered && not a.needs_intervention);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t);
  Stdlib.print_endline "patch_agent: all tests passed"
