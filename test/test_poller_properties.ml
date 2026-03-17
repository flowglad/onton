open Base
open Onton
open Onton.Types

(** Poller property tests derived from the Pantagruel spec:

    {v
    all p: Patch | world-merged p -> merged' p.
    all p: Patch | merged p -> merged' p.
    all p: Patch | world-has-conflict p -> has-conflict' p.
    all p: Patch | world-has-conflict p -> queue' p merge-conflict.
    all p: Patch | world-ci-failed p -> queue' p ci.
    all c: Comment, p: Patch |
        world-has-comment c p and ~resolved c -> queue' p review-comments.
    all p: Patch | mergeable' p = world-mergeable p.
    all p: Patch | checks-passing' p = world-checks-passing p.
    v} *)

let no_addressed = Set.empty (module Comment_id)

let () =
  let open QCheck2 in
  let gen = Onton_test_support.Test_generators.gen_pr_state in
  (* Spec: merged p -> merged' p (sticky) *)
  let prop_merged_sticky =
    Test.make ~name:"poller: was_merged=true implies merged=true in result" gen
      (fun pr ->
        let result =
          Poller.poll ~was_merged:true ~addressed_ids:no_addressed pr
        in
        result.merged)
  in
  (* Spec: world-ci-failed p -> queue' p ci *)
  let prop_ci_failed_implies_ci_queue =
    Test.make ~name:"poller: ci_failed implies Ci in result queue" gen
      (fun pr ->
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
        let in_queue =
          List.mem result.queue Operation_kind.Ci ~equal:Operation_kind.equal
        in
        (not (Github.ci_failed pr)) || in_queue)
  in
  (* Spec: world-has-conflict p -> queue' p merge-conflict *)
  let prop_conflict_implies_merge_conflict_queue =
    Test.make
      ~name:"poller: has_conflict implies Merge_conflict in result queue" gen
      (fun pr ->
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
        let in_queue =
          List.mem result.queue Operation_kind.Merge_conflict
            ~equal:Operation_kind.equal
        in
        (not (Github.has_conflict pr)) || in_queue)
  in
  (* Spec: addressed comments are excluded from new_comments *)
  let prop_addressed_excluded =
    Test.make ~name:"poller: new_comments excludes addressed_ids" gen (fun pr ->
        let all_ids =
          List.map pr.Github.Pr_state.comments ~f:(fun (c : Comment.t) -> c.id)
        in
        let addressed = Set.of_list (module Comment_id) all_ids in
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:addressed pr
        in
        List.is_empty result.new_comments)
  in
  (* Spec: mergeable' p = world-mergeable p *)
  let prop_mergeable_passthrough =
    Test.make ~name:"poller: mergeable passed through from PR state" gen
      (fun pr ->
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
        Bool.equal result.mergeable (Github.mergeable pr))
  in
  (* Spec: checks-passing' p = world-checks-passing p *)
  let prop_checks_passing_passthrough =
    Test.make ~name:"poller: checks_passing passed through from PR state" gen
      (fun pr ->
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
        Bool.equal result.checks_passing (Github.checks_passing pr))
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_merged_sticky;
      prop_ci_failed_implies_ci_queue;
      prop_conflict_implies_merge_conflict_queue;
      prop_addressed_excluded;
      prop_mergeable_passthrough;
      prop_checks_passing_passthrough;
    ];
  Stdlib.print_endline "poller properties: all passed"
