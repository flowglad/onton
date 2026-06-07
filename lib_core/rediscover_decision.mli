(* @archlint.module interface
   @archlint.domain rediscover-decision *)

open Types

(** Pure decision layer between [StartupReconciler.discover_pr]'s effectful PR
    lookup and the orchestrator state change. The poller drives this when an
    in-memory PR is reported closed by a normal poll and we need to decide what
    to do with the agent. *)

type replacement = {
  new_pr : Pr_number.t;
  base_branch : Branch.t;
  merged : bool;
}
[@@deriving show, eq]

type input = {
  patch_id : Patch_id.t;
  pr_number : Pr_number.t;
      (** The PR number that was closed and triggered the rediscovery. Used only
          for log composition by the effectful caller; the classification itself
          does not depend on it. *)
  in_gameplan : bool;
      (** [true] iff [patch_id] currently appears in the gameplan. Drives the
          split between [Clear_pr_for_recreate] (gameplan patch — let the next
          tick re-Start with a fresh PR) and [Mark_pr_missing] (ad-hoc patch —
          there is nothing to recreate; surface for intervention). *)
  result : (replacement option, string) Result.t;
      (** Outcome of [StartupReconciler.discover_pr]:
          - [Ok (Some r)]: a replacement PR was found on the same branch.
          - [Ok None]: no PR exists on the branch.
          - [Error msg]: the lookup itself failed. *)
}
(* [show] omitted: [Result.t] has no derived pretty-printer in this codebase
   and the classification carries enough context for logs. *)

type classification =
  | Switch_to_pr of replacement
      (** Adopt the replacement PR. Effectful caller should
          [Patch_controller.apply_replacement_pr] and re-register the PR number
          in any external map. *)
  | Clear_pr_for_recreate
      (** Gameplan patch with no PR on the remote — reset the agent so the next
          [Start] tick opens a fresh one. Pre-existing behavior. *)
  | Mark_pr_missing
      (** Ad-hoc patch with no PR on the remote — the user-added PR vanished.
          Transition the agent to [Missing] (preserving the recorded number) so
          it surfaces as [needs_intervention]; the operator can either re-open
          the PR on GitHub or remove the agent via [-N]. *)
  | Log_error of { message : string }
      (** Transport / GitHub / GraphQL error. Log and leave the agent state
          unchanged this tick — the next poll will retry. *)
[@@deriving show, eq]

val classify : input -> classification
(** Total. Pure. Same input always yields the same classification. *)

(** {2 Log dedup} *)

type log_decision = Log_emit | Log_skip [@@deriving show, eq]

val classify_vanish_log : classification -> already_logged:bool -> log_decision
(** Pure decision for "should the effectful handler emit the 'PR vanished from
    remote' log line on this tick?". The handler maintains the [already_logged]
    flag per-patch (mirroring [mark_skip_logged] in the poller fiber) and resets
    it whenever a successful recovery transition fires (e.g. [Switch_to_pr]).
    Returns [Log_skip] for non-[Mark_pr_missing] classifications regardless of
    [already_logged]. *)
