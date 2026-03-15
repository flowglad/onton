open Base
open Onton.Types
open Onton.Patch_agent

let gen_pid =
  QCheck2.Gen.(
    map Patch_id.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 12)))

let gen_branch =
  QCheck2.Gen.(
    map Branch.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 20)))

let gen_op =
  QCheck2.Gen.oneof_list
    Operation_kind.[ Rebase; Human; Merge_conflict; Ci; Review_comments ]

let _print_agent = Onton.Patch_agent.show

let () =
  let open QCheck2 in
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
          && t.ci_failure_count = 0 && (not t.session_failed)
          && List.is_empty t.pending_comments);
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
          | exception Invalid_argument _ -> true
          | _ -> false);
      (* -- respond requires not busy -- *)
      Test.make ~name:"respond requires not busy"
        Gen.(triple gen_pid gen_branch gen_op)
        (fun (pid, br, k) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          match respond a k with
          | exception Invalid_argument _ -> true
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
      (* -- respond with multiple ops: only highest priority accepted -- *)
      Test.make ~name:"respond rejects non-highest-priority op"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid |> fun a -> start a ~base_branch:br in
          let a = complete a in
          (* Ci has lower priority than Rebase *)
          let a = enqueue a Operation_kind.Rebase in
          let a = enqueue a Operation_kind.Ci in
          match respond a Operation_kind.Ci with
          | exception Invalid_argument _ -> true
          | _ -> false);
      (* -- full lifecycle -- *)
      Test.make ~name:"full lifecycle"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = create pid in
          let a = start a ~base_branch:br in
          assert a.busy;
          let a = complete a in
          assert (not a.busy);
          let a = enqueue a Operation_kind.Rebase in
          let a = respond a Operation_kind.Rebase in
          assert a.busy;
          let a = complete a in
          assert (not a.busy);
          assert (not a.needs_intervention);
          true);
    ]
  in
  let () =
    (* Specific unit-style property tests *)

    (* respond Human clears satisfies *)
    let pid = Patch_id.of_string "p" in
    let br = Branch.of_string "main" in
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let a = enqueue a Operation_kind.Human in
    let a = respond a Operation_kind.Human in
    assert (not a.satisfies);

    (* respond Ci sets changed *)
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let a = enqueue a Operation_kind.Ci in
    let a = respond a Operation_kind.Ci in
    assert a.changed;

    (* respond Merge_conflict clears has_conflict *)
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let a = set_has_conflict a in
    let a = enqueue a Operation_kind.Merge_conflict in
    let a = respond a Operation_kind.Merge_conflict in
    assert (not a.has_conflict);

    (* respond Review_comments clears pending_comments *)
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let c = Comment.{ body = "fix"; path = None; line = None } in
    let a = add_pending_comment a c ~valid:true in
    let a = enqueue a Operation_kind.Review_comments in
    let a = respond a Operation_kind.Review_comments in
    assert (List.is_empty a.pending_comments);

    (* respond Review_comments sets changed with valid comment *)
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let a = add_pending_comment a c ~valid:true in
    let a = enqueue a Operation_kind.Review_comments in
    let a = respond a Operation_kind.Review_comments in
    assert a.changed;

    (* respond Review_comments does not set changed without valid comment *)
    (* After start+complete, changed is false. Adding only invalid comments
       should keep changed false after responding to Review_comments. *)
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let c_invalid = Comment.{ body = "nit"; path = None; line = None } in
    let a = add_pending_comment a c_invalid ~valid:false in
    let a = enqueue a Operation_kind.Review_comments in
    let a = respond a Operation_kind.Review_comments in
    assert (not a.changed);

    (* complete sets needs_intervention after 3 ci failures *)
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let a = increment_ci_failure_count a in
    let a = increment_ci_failure_count a in
    let a = increment_ci_failure_count a in
    let a = enqueue a Operation_kind.Ci in
    let a = respond a Operation_kind.Ci in
    let a = complete a in
    assert a.needs_intervention;

    (* complete sets needs_intervention on session_failed *)
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let a = set_session_failed a in
    let a = enqueue a Operation_kind.Ci in
    let a = respond a Operation_kind.Ci in
    let a = complete a in
    assert a.needs_intervention;

    (* complete does not set needs_intervention if Human queued *)
    (* Human must still be in the queue at complete time to suppress *)
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let a = set_session_failed a in
    let a = enqueue a Operation_kind.Human in
    (* enqueue Rebase (higher priority than Human) so we can respond to it *)
    let a = enqueue a Operation_kind.Rebase in
    let a = respond a Operation_kind.Rebase in
    (* Human is still in queue *)
    let a = complete a in
    assert (not a.needs_intervention);

    (* clear_needs_intervention resets flag *)
    let a = create pid |> fun a -> start a ~base_branch:br in
    let a = complete a in
    let a = set_session_failed a in
    let a = enqueue a Operation_kind.Ci in
    let a = respond a Operation_kind.Ci in
    let a = complete a in
    assert a.needs_intervention;
    let a = clear_needs_intervention a in
    assert (not a.needs_intervention)
  in
  let ok =
    List.for_all tests ~f:(fun t ->
        match QCheck2.Test.check_exn t with () -> true)
  in
  if ok then Stdlib.print_endline "patch_agent: all tests passed"
