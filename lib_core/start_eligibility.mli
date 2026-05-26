(** Pure decision: is a [Start] action eligible to fire right now, given the
    freshness of its base branch relative to the orchestrator's known
    [origin/main] sha?

    Why this exists: the orchestrator schedules [Start(c, base=b)] as soon as
    [c] is dispatchable, but [b]'s local branch can lag [origin/main] when an
    upstream dependency merges to main while [b] is open (busy responding to
    CI/review). Cutting [c]'s worktree from a stale [b] silently elides the
    just-merged ancestor's commits. The reconciler's poll-tick detectors
    (`detect_rebases`, `detect_stale_bases`, `detect_notified_base_drift`)
    eventually catch this, but a [Start] can race past the next tick. This
    module is the synchronous gate that closes that race: when the base is
    stale, [Start] is deferred (not consumed) until the rebase pipeline brings
    [b] up to date.

    {1 Authority}

    The orchestrator tracks [origin/main]'s sha (`Orchestrator.main_sha`,
    updated by the poller). Each open patch records the sha of the base it was
    last rebased onto (`Patch_agent.branch_rebased_onto_sha`, written by the
    rebase pipeline on success). When the two agree for [c]'s base patch, the
    base contains main's tip and [Start] is safe. *)

type defer_reason =
  | Main_sha_unknown
      (** The poller has not yet observed [origin/main] (e.g. very first tick
          after startup) and no rebase is in flight. Fail closed — defer rather
          than risk cutting from a base whose freshness we cannot verify. *)
  | Base_patch_busy_with_rebase of { base_branch : string }
      (** The base patch already has a [Rebase] in flight (busy with op
          [Rebase], or [Rebase] is queued and highest-priority). Wait for it
          rather than racing it. *)
  | Base_not_rebased_since_main_advanced of {
      base_branch : string;
      base_rebased_onto_sha : string option;
      main_sha : string;
    }
      (** The base patch's last-recorded rebase sha differs from the
          orchestrator's known main sha. The base needs to be rebased before
          [Start] can fire. *)
[@@deriving show, eq, sexp_of, compare]

type decision = Allow | Defer of defer_reason
[@@deriving show, eq, sexp_of, compare]

val decide :
  base_is_main:bool ->
  base_branch:string ->
  base_patch_merged:bool ->
  base_patch_rebased_onto_sha:string option ->
  base_patch_busy_rebasing:bool ->
  main_sha:string option ->
  decision
(** [decide] is total and deterministic. Pre-emption order, applied top-down:

    + [base_is_main = true] → [Allow]. Starting from main directly is always
      eligible — main is the freshness reference itself.
    + [base_patch_merged = true] → [Allow]. The base was a dep but it merged;
      callers must treat this as "base is effectively main" (the orchestrator
      will refresh [base_branch] before the [Start] actually executes via
      [refresh_base_branch] in [mark_merged]).
    + [base_patch_busy_rebasing = true] → [Defer Base_patch_busy_with_rebase].
      Checked before [main_sha = None] so an active rebase pre-empts the
      unknown-main verdict (and the arm is not dead when main is unpublished).
    + [main_sha = None] → [Defer Main_sha_unknown]. Fail closed.
    + [base_patch_rebased_onto_sha = Some s] and [Some s = main_sha] → [Allow].
    + Otherwise → [Defer Base_not_rebased_since_main_advanced { … }].

    Note: this decision does not look at the dep graph. The caller resolves
    [c]'s base to a single base patch (or main) and passes the relevant facts.
    For multi-dep patches, the caller is expected to wait for the dep graph to
    collapse to ≤1 open dep (the existing [refresh_base_branch] contract) before
    constructing a [Start] action; this module enforces freshness of that single
    chosen base. *)

val short_label : decision -> string
(** A short, lowercase, snake_case identifier for the decision arm, suitable for
    activity logging. Always non-empty and ≤ 32 characters. Examples: ["allow"],
    ["defer_main_unknown"], ["defer_base_busy_rebasing"], ["defer_base_stale"].
*)
