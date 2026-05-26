(** Pure decision: is a [Start] action eligible to fire right now, given whether
    its base patch's local branch has already incorporated the merged form of
    its dependencies?

    Why this exists: the orchestrator schedules [Start(c, base=b)] as soon as
    [c] is dispatchable, but [b]'s local branch can lag its dependencies when an
    upstream dependency merges to main while [b] is open (busy responding to
    CI/review). Cutting [c]'s worktree from such a [b] silently elides the
    just-merged ancestor's commits — and under squash merges [b] still carries
    the dep's pre-squash commits, so a later rebase of [b] would leave [c]
    stranded on history that no longer exists. The reconciler's poll-tick
    detectors ([detect_rebases], [detect_stale_bases],
    [detect_notified_base_drift]) enqueue the rebase that fixes this, but a
    [Start] can race past the next tick. This module is the synchronous gate
    that closes that race: when the base is not yet fresh, [Start] is deferred
    (not consumed) until the rebase pipeline brings [b] up to date.

    {1 Freshness is dependency-scoped, not main-scoped}

    The gate does NOT require [b] to be current with the latest [origin/main].
    It only requires [b]'s local branch to sit on its structurally-correct base
    given the current merge state — i.e. [Patch_agent.branch_rebased_onto]
    equals [Graph.initial_base b]. A [b] based directly on main is always fresh,
    even if main has since advanced for unrelated reasons; an unrelated merge to
    main never makes [b] stale for the purpose of cutting [c]. The caller
    computes the [base_structurally_fresh] flag from the dep graph and the base
    patch's recorded rebase anchor. *)

type defer_reason =
  | Base_patch_busy_with_rebase of { base_branch : string }
      (** The base patch already has a [Rebase] in flight (busy with op
          [Rebase], or [Rebase] is queued and highest-priority). Wait for it
          rather than racing it. *)
  | Base_not_fresh_for_cut of { base_branch : string }
      (** The base patch's local branch is not yet rebased onto its
          structurally-correct base — a dependency has merged but the rebase
          that absorbs it has not landed. The base needs that rebase before
          [Start] can fire. *)
[@@deriving show, eq, sexp_of, compare]

type decision = Allow | Defer of defer_reason
[@@deriving show, eq, sexp_of, compare]

val decide :
  base_is_main:bool ->
  base_branch:string ->
  base_patch_merged:bool ->
  base_patch_busy_rebasing:bool ->
  base_structurally_fresh:bool ->
  decision
(** [decide] is total and deterministic. Pre-emption order, applied top-down:

    + [base_is_main = true] → [Allow]. Starting from main directly is always
      eligible — main is the freshness reference itself.
    + [base_patch_merged = true] → [Allow]. The base was a dep but it merged;
      callers must treat this as "base is effectively main" (the orchestrator
      will refresh [base_branch] before the [Start] actually executes via
      [refresh_base_branch] in [mark_merged]).
    + [base_patch_busy_rebasing = true] → [Defer Base_patch_busy_with_rebase].
      Checked before the freshness flag so an active rebase pre-empts.
    + [base_structurally_fresh = false] → [Defer Base_not_fresh_for_cut].
    + Otherwise → [Allow].

    Note: this decision does not look at the dep graph itself. The caller
    resolves [c]'s base to a single base patch (or main) and passes the relevant
    facts, including whether that base is structurally fresh. *)

val short_label : decision -> string
(** A short, lowercase, snake_case identifier for the decision arm, suitable for
    activity logging. Always non-empty and ≤ 32 characters. Examples: ["allow"],
    ["defer_base_busy_rebasing"], ["defer_base_stale"]. *)
