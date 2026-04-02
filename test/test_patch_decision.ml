open Base
open Onton.Types
open Onton.Patch_agent
open Onton.Patch_decision

(* -- Generators -- *)

let gen_pid =
  QCheck2.Gen.(
    map Patch_id.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 12)))

let gen_branch =
  QCheck2.Gen.(
    map Branch.of_string
      (string_size ~gen:(char_range 'a' 'z') (int_range 3 20)))

let feedback_ops =
  Operation_kind.
    [ Human; Merge_conflict; Ci; Review_comments; Implementation_notes ]

let gen_feedback_op = QCheck2.Gen.oneof_list feedback_ops

(** Start + set PR so the agent is in has_pr=true, busy=false state. *)
let with_pr pid br =
  let a = create pid |> fun a -> start a ~base_branch:br in
  let a = set_pr_number a (Pr_number.of_int 1) in
  complete a

let () =
  let open QCheck2 in
  let tests =
    [
      (* ---- disposition: merged always Skip ---- *)
      Test.make ~name:"disposition: merged -> Skip"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> mark_merged in
          equal_disposition (disposition a) Skip);
      (* ---- disposition: needs_intervention -> Blocked ---- *)
      Test.make ~name:"disposition: needs_intervention -> Blocked"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          (* Drive ci_failure_count to 3 via respond (which increments on
             Ci) to trigger needs_intervention after complete *)
          let a = with_pr pid br in
          let a = increment_ci_failure_count a |> increment_ci_failure_count in
          let a = enqueue a Operation_kind.Ci in
          (* respond Ci increments ci_failure_count to 3 *)
          let a = respond a Operation_kind.Ci in
          let a = complete a in
          equal_disposition (disposition a) Blocked);
      (* ---- disposition: busy -> Busy ---- *)
      Test.make ~name:"disposition: busy -> Busy"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Human in
          let a = respond a Operation_kind.Human in
          (* a is now busy *)
          equal_disposition (disposition a) Busy);
      (* ---- disposition: no PR -> Ready_start ---- *)
      Test.make ~name:"disposition: no PR -> Ready_start" gen_pid (fun pid ->
          let a = create pid in
          equal_disposition (disposition a) Ready_start);
      (* ---- disposition: idle (has_pr, empty queue) -> Idle ---- *)
      Test.make ~name:"disposition: has_pr, empty queue -> Idle"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          equal_disposition (disposition a) Idle);
      (* ---- disposition: queued feedback -> Ready_respond ---- *)
      Test.make ~name:"disposition: queued feedback -> Ready_respond"
        Gen.(triple gen_pid gen_branch gen_feedback_op)
        (fun (pid, br, k) ->
          let a = with_pr pid br |> fun a -> enqueue a k in
          try
            let d = disposition a in
            equal_disposition d (Ready_respond k)
          with _ -> false);
      (* ---- disposition: queued Rebase -> Ready_rebase ---- *)
      Test.make ~name:"disposition: queued Rebase -> Ready_rebase"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> fun a -> enqueue a Operation_kind.Rebase in
          equal_disposition (disposition a) Ready_rebase);
      (* ---- disposition: Rebase + feedback -> Ready_rebase (Rebase wins) ---- *)
      Test.make ~name:"disposition: Rebase wins over feedback"
        Gen.(triple gen_pid gen_branch gen_feedback_op)
        (fun (pid, br, k) ->
          let a =
            with_pr pid br |> fun a ->
            enqueue a k |> fun a -> enqueue a Operation_kind.Rebase
          in
          equal_disposition (disposition a) Ready_rebase);
      (* ---- on_ci_failure: below cap -> Enqueue ---- *)
      Test.make ~name:"on_ci_failure: count < 3 -> Enqueue"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          (* count = 0 *)
          equal_ci_decision (on_ci_failure a) Enqueue_ci);
      (* ---- on_ci_failure: at cap -> Cap_reached ---- *)
      Test.make ~name:"on_ci_failure: count >= 3 -> Cap_reached"
        Gen.(triple gen_pid gen_branch (int_range 3 20))
        (fun (pid, br, n) ->
          let a = with_pr pid br in
          let rec apply_n n f x =
            if n <= 0 then x else apply_n (n - 1) f (f x)
          in
          let a = apply_n n increment_ci_failure_count a in
          equal_ci_decision (on_ci_failure a) Cap_reached);
      (* ---- on_ci_failure: exactly 2 -> Enqueue ---- *)
      Test.make ~name:"on_ci_failure: count = 2 -> Enqueue"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = increment_ci_failure_count (increment_ci_failure_count a) in
          equal_ci_decision (on_ci_failure a) Enqueue_ci);
      (* ---- on_ci_failure: Ci already queued -> Already_queued ---- *)
      Test.make ~name:"on_ci_failure: Ci in queue -> Already_queued"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Ci in
          equal_ci_decision (on_ci_failure a) Ci_already_queued);
      (* ---- on_ci_failure: active Ci fix -> Ci_fix_in_progress ---- *)
      Test.make ~name:"on_ci_failure: active Ci fix -> Ci_fix_in_progress"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          equal_ci_decision (on_ci_failure a) Ci_fix_in_progress);
      Test.make
        ~name:
          "on_ci_failure: completed failed CI attempt re-enqueues on next poll"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Ci in
          let a = respond a Operation_kind.Ci in
          let a = complete a in
          equal_ci_decision (on_ci_failure a) Enqueue_ci);
      (* ---- on_human_message: fresh queue -> Enqueue_human ---- *)
      Test.make ~name:"on_human_message: no Human in queue -> Enqueue_human"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          equal_human_decision (on_human_message a) Enqueue_human);
      (* ---- on_human_message: already queued -> Already_queued ---- *)
      Test.make ~name:"on_human_message: Human in queue -> Already_queued"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> fun a -> enqueue a Operation_kind.Human in
          equal_human_decision (on_human_message a) Already_queued);
      (* ---- on_merge_conflict: no conflict -> Enqueue_conflict ---- *)
      Test.make ~name:"on_merge_conflict: no conflict -> Enqueue_conflict"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          equal_conflict_decision (on_merge_conflict a) Enqueue_conflict);
      (* ---- on_merge_conflict: already conflicting -> Already_conflicting ---- *)
      Test.make ~name:"on_merge_conflict: has_conflict -> Already_conflicting"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> set_has_conflict in
          equal_conflict_decision (on_merge_conflict a) Already_conflicting);
      (* ---- on_checks_passing: failures + passing -> Reset ---- *)
      Test.make
        ~name:"on_checks_passing: ci_failure_count > 0 + passing -> Reset"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> increment_ci_failure_count in
          equal_checks_passing_decision
            (on_checks_passing a ~checks_passing:true)
            Reset_ci_failure_count);
      (* ---- on_checks_passing: no failures + passing -> No_ci_reset ---- *)
      Test.make
        ~name:"on_checks_passing: ci_failure_count = 0 + passing -> No_ci_reset"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          equal_checks_passing_decision
            (on_checks_passing a ~checks_passing:true)
            No_ci_reset);
      (* ---- on_checks_passing: failures + not passing -> No_ci_reset ---- *)
      Test.make
        ~name:
          "on_checks_passing: ci_failure_count > 0 + not passing -> No_ci_reset"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> increment_ci_failure_count in
          equal_checks_passing_decision
            (on_checks_passing a ~checks_passing:false)
            No_ci_reset);
      (* ---- should_clear_conflict: no active conflict op -> true ---- *)
      Test.make ~name:"should_clear_conflict: idle agent -> true"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          should_clear_conflict a);
      (* ---- should_clear_conflict: Merge_conflict queued -> false ---- *)
      Test.make ~name:"should_clear_conflict: Merge_conflict queued -> false"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> set_has_conflict in
          let a = enqueue a Operation_kind.Merge_conflict in
          not (should_clear_conflict a));
      (* ---- should_clear_conflict: Merge_conflict in-flight -> false ---- *)
      Test.make ~name:"should_clear_conflict: Merge_conflict in-flight -> false"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br |> set_has_conflict in
          let a = enqueue a Operation_kind.Merge_conflict in
          let a = respond a Operation_kind.Merge_conflict in
          not (should_clear_conflict a));
      (* ---- should_clear_conflict: other op in-flight -> true ---- *)
      Test.make ~name:"should_clear_conflict: non-conflict op in-flight -> true"
        Gen.(pair gen_pid gen_branch)
        (fun (pid, br) ->
          let a = with_pr pid br in
          let a = enqueue a Operation_kind.Human in
          let a = respond a Operation_kind.Human in
          should_clear_conflict a);
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t)
