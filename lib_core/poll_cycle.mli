open Types

(** Pure planning layer between the effectful per-patch poll driver and the
    orchestrator state machine.

    Encodes the per-patch isolation guarantee in the type system: [classify]
    consumes a single [input] and produces a single [classification], so the
    output for patch X cannot depend on any other patch's outcome. Property
    tests assert the corresponding behavioural invariant — substituting any
    subset of inputs with [Timed_out] leaves every other patch's classification
    unchanged. *)

type input = {
  patch_id : Patch_id.t;
  pr_number : Pr_number.t;
  was_merged : bool;
  outcome : Poll_outcome.t;
}
[@@deriving show, eq]

type classification =
  | Apply_pr_state of {
      pr_state : Pr_state.t;
      poll_result : Poller.t;
      ci_checks_truncated : bool;
    }
  | Skip_fork of { head_branch : Branch.t option }
  | Rediscover_pr of { head_branch : Branch.t option }
  | Log_error of { message : string }
[@@deriving show, eq]

val classify : input -> classification
(** [classify input] is total: it returns a [classification] for every possible
    [outcome], never raises, never observes any state outside [input]. *)

val plan : input list -> (Patch_id.t * Pr_number.t * classification) list
(** [plan inputs] applies [classify] to each input in order. Pure;
    order-preserving; per-patch independent. *)
