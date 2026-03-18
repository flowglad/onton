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
  let c_valid =
    Comment.
      {
        id = Comment_id.of_int 1;
        thread_id = None;
        body = "fix";
        path = None;
        line = None;
      }
  in
  let c_invalid =
    Comment.
      {
        id = Comment_id.of_int 2;
        thread_id = None;
        body = "nit";
        path = None;
        line = None;
      }
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
          && Set.is_empty t.addressed_comment_ids
          && (not t.mergeable) && (not t.merge_ready) && (not t.checks_passing)
          && not t.no_unresolved_comments);
      (* -- enqueue is idempotent -- *)
      Test.make ~name:"enqueue is idempotent"
        Gen.(triple gen_pid gen_branch gen_op)
        (fun (pid, br, k) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a1 = enqueue a k in
          let a2 = enqueue a1 k in
          equal a1 a2);
      (* -- enqueue adds operation -- *)
      Test.make ~name:"enqueue adds operation to queue"
        Gen.(pair (pair gen_pid gen_branch) gen_op)
        (fun ((pid, br), k) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = enqueue a k in
          List.mem a.queue k ~equal:Operation_kind.equal);
      (* -- start postconditions -- *)
      Test.make ~name:"start sets has_session, busy, satisfies (not has_pr)"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          (not a.has_pr) && a.has_session && a.busy && a.satisfies
          && Option.equal Branch.equal a.base_branch (Some br));
      (* -- start twice raises -- *)
      Test.make ~name:"start on already-started raises (busy)"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          match start a ~base_branch:br with
          | exception Invalid_argument _ -> true
          | _ -> false);
      Test.make ~name:"start on has_pr raises"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = set_pr_number a (Pr_number.of_int 1) in
          let a = complete a in
          match start a ~base_branch:br with
          | exception Invalid_argument _ -> true
          | _ -> false);
      (* -- complete clears busy -- *)
      Test.make ~name:"complete clears busy"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          not a.busy);
      (* -- complete on non-busy is idempotent no-op -- *)
      Test.make ~name:"complete on non-busy is no-op" gen_pid (fun pid ->
          let a = create pid in
          let a' = complete a in
          equal a a');
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
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          match respond a k with
          | exception Invalid_argument msg ->
              String.is_substring msg ~substring:"busy"
          | _ -> false);
      (* -- respond requires op in queue -- *)
      Test.make ~name:"respond requires op in queue"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          match respond a Operation_kind.Ci with
          | exception Invalid_argument _ -> true
          | _ -> false);
      (* -- respond sets busy and has_session -- *)
      Test.make ~name:"respond sets busy and has_session"
        Gen.(triple gen_pid gen_branch gen_feedback_op)
        (fun (pid, br, k) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = enqueue a k in
          let a = respond a k in
          a.busy && a.has_session);
      (* -- respond removes op from queue -- *)
      Test.make ~name:"respond removes op from queue"
        Gen.(triple gen_pid gen_branch gen_feedback_op)
        (fun (pid, br, k) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
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
                      create pid |> fun a -> start_with_pr a ~base_branch:br
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
          let a = create pid in
          let a = start_with_pr a ~base_branch:br in
          let busy_after_start = a.busy in
          let a = complete a in
          let idle_after_complete = not a.busy in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:(Branch.of_string "new-base") in
          let busy_after_rebase = a.busy in
          let a = complete a in
          busy_after_start && idle_after_complete && busy_after_rebase
          && (not a.busy) && not a.needs_intervention);
      (* -- respond Human clears satisfies -- *)
      Test.make ~name:"respond Human clears satisfies" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = enqueue a Operation_kind.Human in
          let a = respond a Operation_kind.Human in
          not a.satisfies);
      (* -- respond Ci sets changed -- *)
      Test.make ~name:"respond Ci sets changed" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          a.changed);
      (* -- respond Merge_conflict clears has_conflict -- *)
      Test.make ~name:"respond Merge_conflict clears has_conflict" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = set_has_conflict a in
          let a = enqueue a Operation_kind.Merge_conflict in
          let a = respond a Operation_kind.Merge_conflict in
          not a.has_conflict);
      (* -- respond Review_comments clears pending_comments -- *)
      Test.make ~name:"respond Review_comments clears pending_comments" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
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
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
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
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = add_pending_comment a c_invalid ~valid:false in
          let a = enqueue a Operation_kind.Review_comments in
          let a = respond a Operation_kind.Review_comments in
          not a.changed);
      (* -- 2 ci failures does NOT trigger needs_intervention (boundary) -- *)
      Test.make ~name:"2 ci failures no intervention (boundary)" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
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
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
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
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = set_session_failed a in
          let a = set_tried_fresh a in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          let a = complete a in
          a.needs_intervention);
      (* -- Human queued suppresses intervention from session_failed -- *)
      Test.make ~name:"Human queued suppresses intervention (session_failed)"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = set_session_failed a in
          let a = set_tried_fresh a in
          let a = enqueue a Operation_kind.Human in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:(Branch.of_string "new-base") in
          let a = complete a in
          not a.needs_intervention);
      (* -- Human queued suppresses intervention from 3 ci failures -- *)
      Test.make ~name:"Human queued suppresses intervention (3 ci failures)"
        ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = increment_ci_failure_count a in
          let a = enqueue a Operation_kind.Human in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:(Branch.of_string "new-base") in
          let a = complete a in
          not a.needs_intervention);
      (* -- clear_needs_intervention resets flag -- *)
      Test.make ~name:"clear_needs_intervention resets flag" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = set_session_failed a in
          let a = set_tried_fresh a in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          let a = complete a in
          let triggered = a.needs_intervention in
          let a = clear_needs_intervention a in
          triggered && not a.needs_intervention);
      (* -- create has no pr_number -- *)
      Test.make ~name:"create has no pr_number" gen_pid (fun pid ->
          let a = create pid in
          Option.is_none a.pr_number);
      (* -- set_pr_number stores pr_number -- *)
      Test.make ~name:"set_pr_number stores pr_number"
        Gen.(pair gen_pid (map Pr_number.of_int (int_range 1 9999)))
        (fun (pid, pr) ->
          let a = create pid in
          let a = set_pr_number a pr in
          Option.equal Pr_number.equal a.pr_number (Some pr));
      (* -- start clears ci_checks and addressed_comment_ids -- *)
      Test.make ~name:"start clears ci_checks and addressed_comment_ids"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid in
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
          let cid = Comment_id.of_int 1 in
          let a = set_ci_checks a [ check ] in
          let a = add_addressed_comment_id a cid in
          let a = complete a in
          let a = mark_merged a in
          let a =
            Onton.Patch_agent.restore ~patch_id:a.patch_id ~has_pr:false
              ~pr_number:None ~has_session:false ~busy:false ~merged:false
              ~needs_intervention:false ~queue:[] ~satisfies:false
              ~changed:false ~has_conflict:false ~base_branch:None
              ~ci_failure_count:0 ~session_fallback:Fresh_available
              ~pending_comments:[] ~ci_checks:a.ci_checks
              ~addressed_comment_ids:a.addressed_comment_ids ~removed:false
              ~mergeable:false ~merge_ready:false ~checks_passing:false
              ~no_unresolved_comments:false ~worktree_path:None
              ~head_branch:None
          in
          let a = start a ~base_branch:br in
          List.is_empty a.ci_checks && Set.is_empty a.addressed_comment_ids);
      (* -- set_ci_checks stores checks -- *)
      Test.make ~name:"set_ci_checks stores checks" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
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
      (* -- add_addressed_comment_id is idempotent -- *)
      Test.make ~name:"add_addressed_comment_id is idempotent"
        Gen.(pair gen_pid (map Comment_id.of_int (int_range 1 100)))
        (fun (pid, cid) ->
          let a = create pid in
          let a = add_addressed_comment_id a cid in
          let a = add_addressed_comment_id a cid in
          Set.length a.addressed_comment_ids = 1);
      (* -- is_comment_addressed reflects add -- *)
      Test.make ~name:"is_comment_addressed reflects add"
        Gen.(pair gen_pid (map Comment_id.of_int (int_range 1 100)))
        (fun (pid, cid) ->
          let a = create pid in
          let before = is_comment_addressed a cid in
          let a = add_addressed_comment_id a cid in
          let after = is_comment_addressed a cid in
          (not before) && after);
      (* -- set_tried_fresh from Fresh_available -> Tried_fresh -- *)
      Test.make ~name:"set_tried_fresh from Fresh_available" gen_pid (fun pid ->
          let a = create pid in
          let a = set_tried_fresh a in
          equal_session_fallback a.session_fallback Tried_fresh);
      (* -- set_tried_fresh is no-op from Tried_fresh or Given_up -- *)
      Test.make ~name:"set_tried_fresh advances Tried_fresh to Given_up" gen_pid
        (fun pid ->
          let a = create pid in
          let a = set_tried_fresh a in
          let a = set_tried_fresh a in
          equal_session_fallback a.session_fallback Given_up);
      (* -- set_tried_fresh is no-op from Given_up -- *)
      Test.make ~name:"set_tried_fresh no-op from Given_up" gen_pid (fun pid ->
          let a = create pid in
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
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
          let a = complete a in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:new_base in
          a.busy && a.has_session
          && Option.equal Branch.equal a.base_branch (Some new_base)
          && not
               (List.mem a.queue Operation_kind.Rebase
                  ~equal:Operation_kind.equal));
      Test.make ~name:"rebase postconditions (has_session=false)"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let new_base = Branch.of_string "new-base" in
          (* Construct agent with has_pr=true but has_session=false via
             restore *)
          let a =
            restore ~patch_id:pid ~has_pr:true ~pr_number:None
              ~has_session:false ~busy:false ~merged:false
              ~needs_intervention:false ~queue:[] ~satisfies:true ~changed:false
              ~has_conflict:false ~base_branch:(Some br) ~ci_failure_count:0
              ~session_fallback:Fresh_available ~pending_comments:[]
              ~ci_checks:[]
              ~addressed_comment_ids:(Set.empty (module Comment_id))
              ~removed:false ~mergeable:false ~merge_ready:false
              ~checks_passing:false ~no_unresolved_comments:false
              ~worktree_path:None ~head_branch:None
          in
          let a = enqueue a Operation_kind.Rebase in
          let a = rebase a ~base_branch:new_base in
          a.busy && (not a.has_session)
          && Option.equal Branch.equal a.base_branch (Some new_base)
          && not
               (List.mem a.queue Operation_kind.Rebase
                  ~equal:Operation_kind.equal));
      (* -- rebase preserves other queues -- *)
      Test.make ~name:"rebase preserves other queues"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
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
          let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
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
            let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
            let a = complete a in
            let a = set_merge_ready a true in
            is_approved a
          with _ -> false);
      (* -- is_approved false without has_pr -- *)
      Test.make ~name:"is_approved false without has_pr" ~count:1
        Gen.(pure pid0)
        (fun pid ->
          let a = create pid in
          let a = set_merge_ready a true in
          not (is_approved a));
      (* -- is_approved false when busy -- *)
      Test.make ~name:"is_approved false when busy" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
            let a = set_merge_ready a true in
            not (is_approved a)
          with _ -> false);
      (* -- is_approved false when not merge_ready -- *)
      Test.make ~name:"is_approved false when not merge_ready" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
            let a = complete a in
            not (is_approved a)
          with _ -> false);
      (* -- is_approved false when needs_intervention -- *)
      Test.make ~name:"is_approved false when needs_intervention" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
            let a = complete a in
            let a = increment_ci_failure_count a in
            let a = increment_ci_failure_count a in
            let a = increment_ci_failure_count a in
            let a = enqueue a Operation_kind.Ci in
            let a = respond a Operation_kind.Ci in
            let a = complete a in
            let a = set_merge_ready a true in
            not (is_approved a)
          with _ -> false);
      (* -- respond invalidates merge_ready -- *)
      Test.make ~name:"respond invalidates merge_ready" ~count:1
        Gen.(pure (pid0, br0))
        (fun (pid, br) ->
          try
            let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
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
            let a = create pid |> fun a -> start_with_pr a ~base_branch:br in
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
          let a = create pid in
          let a = set_has_conflict a in
          let before = a.has_conflict in
          let a = clear_has_conflict a in
          before && not a.has_conflict);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t);
  Stdlib.print_endline "patch_agent: all tests passed"
