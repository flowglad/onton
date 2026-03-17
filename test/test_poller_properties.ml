open Base
open Onton.Types
open Onton.Github

let gen_merge_state =
  QCheck2.Gen.oneof_list Pr_state.[ Mergeable; Conflicting; Unknown ]

let gen_check_status =
  QCheck2.Gen.oneof_list Pr_state.[ Passing; Failing; Pending ]

let gen_comment_id = QCheck2.Gen.(map Comment_id.of_int (int_range 1 10_000))

let gen_comment =
  QCheck2.Gen.(
    map2
      (fun id body -> Comment.{ id; body; path = None; line = None })
      gen_comment_id
      (string_size ~gen:(char_range 'a' 'z') (int_range 1 20)))

let gen_ci_check =
  QCheck2.Gen.(
    map2
      (fun name conclusion ->
        Ci_check.{ name; conclusion; details_url = None; description = None })
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 10))
      (oneof_list [ "success"; "failure"; "neutral"; "skipped" ]))

let gen_pr_state =
  QCheck2.Gen.(
    let* merged = bool in
    let* merge_state = gen_merge_state in
    let* check_status = gen_check_status in
    let* ci_checks = list_small gen_ci_check in
    let* comments = list_small gen_comment in
    let unresolved_comment_count = List.length comments in
    return
      Pr_state.
        {
          merged;
          merge_state;
          check_status;
          ci_checks;
          ci_checks_truncated = false;
          comments;
          unresolved_comment_count;
        })

let gen_addressed_ids =
  QCheck2.Gen.(
    map
      (fun ids ->
        Set.of_list (module Comment_id) (List.map ids ~f:Comment_id.of_int))
      (list_small (int_range 1 10_000)))

let () =
  let open QCheck2 in
  let tests =
    [
      (* was_merged=true implies merged=true in result *)
      Test.make ~name:"was_merged implies result merged" ~count:500
        (Gen.pair gen_pr_state gen_addressed_ids) (fun (pr, addressed_ids) ->
          let result = Onton.Poller.poll ~was_merged:true ~addressed_ids pr in
          result.Onton.Poller.merged);
      (* PR merged implies result merged *)
      Test.make ~name:"pr.merged implies result merged" ~count:500
        (Gen.pair gen_pr_state gen_addressed_ids) (fun (pr, addressed_ids) ->
          let pr = { pr with Pr_state.merged = true } in
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          result.Onton.Poller.merged);
      (* ci_failed implies Ci in result queue *)
      Test.make ~name:"ci_failed implies Ci in queue" ~count:500
        (Gen.pair gen_pr_state gen_addressed_ids) (fun (pr, addressed_ids) ->
          let pr = { pr with Pr_state.check_status = Pr_state.Failing } in
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          List.mem result.Onton.Poller.queue Operation_kind.Ci
            ~equal:Operation_kind.equal);
      (* checks not failing implies no Ci in queue *)
      Test.make ~name:"ci not failed implies no Ci in queue" ~count:500
        (Gen.pair gen_pr_state gen_addressed_ids) (fun (pr, addressed_ids) ->
          let pr = { pr with Pr_state.check_status = Pr_state.Passing } in
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          not
            (List.mem result.Onton.Poller.queue Operation_kind.Ci
               ~equal:Operation_kind.equal));
      (* has_conflict implies Merge_conflict in result queue *)
      Test.make ~name:"has_conflict implies Merge_conflict in queue" ~count:500
        (Gen.pair gen_pr_state gen_addressed_ids) (fun (pr, addressed_ids) ->
          let pr = { pr with Pr_state.merge_state = Pr_state.Conflicting } in
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          List.mem result.Onton.Poller.queue Operation_kind.Merge_conflict
            ~equal:Operation_kind.equal);
      (* not conflicting implies no Merge_conflict in queue *)
      Test.make ~name:"not conflicting implies no Merge_conflict in queue"
        ~count:500 (Gen.pair gen_pr_state gen_addressed_ids)
        (fun (pr, addressed_ids) ->
          let pr = { pr with Pr_state.merge_state = Pr_state.Mergeable } in
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          not
            (List.mem result.Onton.Poller.queue Operation_kind.Merge_conflict
               ~equal:Operation_kind.equal));
      (* new comments (not in addressed_ids) implies Review_comments in queue *)
      Test.make ~name:"unaddressed comments implies Review_comments in queue"
        ~count:500 gen_pr_state (fun pr ->
          let comment =
            Comment.
              {
                id = Comment_id.of_int 99999;
                body = "fix this";
                path = None;
                line = None;
              }
          in
          let pr =
            { pr with Pr_state.comments = comment :: pr.Pr_state.comments }
          in
          let addressed_ids = Set.empty (module Comment_id) in
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          List.mem result.Onton.Poller.queue Operation_kind.Review_comments
            ~equal:Operation_kind.equal);
      (* all comments addressed implies no Review_comments in queue *)
      Test.make ~name:"all comments addressed implies no Review_comments"
        ~count:500 gen_pr_state (fun pr ->
          let addressed_ids =
            Set.of_list
              (module Comment_id)
              (List.map pr.Pr_state.comments ~f:(fun c -> c.Comment.id))
          in
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          not
            (List.mem result.Onton.Poller.queue Operation_kind.Review_comments
               ~equal:Operation_kind.equal));
      (* new_comments excludes addressed_ids *)
      Test.make ~name:"new_comments excludes addressed_ids" ~count:500
        (Gen.pair gen_pr_state gen_addressed_ids) (fun (pr, addressed_ids) ->
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          List.for_all result.Onton.Poller.new_comments ~f:(fun c ->
              not (Set.mem addressed_ids c.Comment.id)));
      (* mergeable is passed through from PR state *)
      Test.make ~name:"mergeable passed through" ~count:500
        (Gen.pair gen_pr_state gen_addressed_ids) (fun (pr, addressed_ids) ->
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          Bool.equal result.Onton.Poller.mergeable (Onton.Github.mergeable pr));
      (* checks_passing is passed through from PR state *)
      Test.make ~name:"checks_passing passed through" ~count:500
        (Gen.pair gen_pr_state gen_addressed_ids) (fun (pr, addressed_ids) ->
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          Bool.equal result.Onton.Poller.checks_passing
            (Onton.Github.checks_passing pr));
      (* ci_checks are passed through from PR state *)
      Test.make ~name:"ci_checks passed through" ~count:500
        (Gen.pair gen_pr_state gen_addressed_ids) (fun (pr, addressed_ids) ->
          let result = Onton.Poller.poll ~was_merged:false ~addressed_ids pr in
          List.equal Ci_check.equal result.Onton.Poller.ci_checks
            pr.Pr_state.ci_checks);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t)
