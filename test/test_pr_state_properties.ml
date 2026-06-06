(* @archlint.module test
   @archlint.domain pr-state *)

open Onton_core

let merge_ready_matches_component_predicates =
  QCheck2.Test.make ~name:"merge_ready_of is exactly its component gate"
    ~count:200
    QCheck2.Gen.(
      triple
        (oneof_list
           [ Pr_state.Mergeable; Pr_state.Conflicting; Pr_state.Unknown ])
        (oneof_list [ Pr_state.Passing; Pr_state.Failing; Pr_state.Pending ])
        (option string_small))
    (fun (merge_state, check_status, review_decision) ->
      Bool.equal
        (Pr_state.merge_ready_of ~merge_state ~check_status ~review_decision)
        (Pr_state.equal_merge_state merge_state Pr_state.Mergeable
        && Pr_state.equal_check_status check_status Pr_state.Passing
        && not (Pr_state.review_blocking review_decision)))

let pr_state_public_surface_is_linked =
  QCheck2.Test.make ~name:"pr-state public surface is linked" QCheck2.Gen.unit
    (fun () ->
      ignore Pr_state.checks_passing;
      ignore Pr_state.ci_failed;
      ignore Pr_state.closed;
      ignore Pr_state.derive_check_status;
      ignore Pr_state.enqueued;
      ignore Pr_state.has_conflict;
      ignore Pr_state.is_draft;
      ignore Pr_state.is_fork;
      ignore Pr_state.merge_ready;
      ignore Pr_state.merge_ready_divergence_of;
      ignore Pr_state.mergeable;
      ignore Pr_state.merged;
      ignore Pr_state.no_unresolved_comments;
      ignore Pr_state.requires_merge_queue;
      true)

let () =
  QCheck2.Test.check_exn merge_ready_matches_component_predicates;
  QCheck2.Test.check_exn pr_state_public_surface_is_linked
