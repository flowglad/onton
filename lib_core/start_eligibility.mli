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
    patch's recorded rebase anchor.

    {1 Sibling containment for fan-in patches}

    Structural freshness alone is blind to a second hazard. When [c] fans in on
    several dependencies (e.g. [c -> {1,2,3}] where [2] is a stacked child of
    [1] and [3] is an independent sibling), [c]'s base resolves to the sole
    still-open dep [b]. [b]'s branch transitively carries its own lineage, but
    NOT a *sibling* dependency of [c] that merged to main independently — that
    sibling's squash commit lives on main, never an ancestor of [b]. Cutting [c]
    from such a [b] starts it from a base missing one of its own dependencies.
    The [base_contains_merged_siblings] flag — computed by the caller against
    [c]'s full merged-dep set via [Worktree.is_ancestor] over each merged dep's
    [merge_commit_sha] — closes this. It never trips on unrelated main movement
    (only [c]'s actual deps are checked), so it preserves the dependency-scoped
    (not main-scoped) discipline above. The same gate is consulted for [Rebase]
    actions, which is what makes a fan-in/chain rebase cascade block on its
    dependency layers in topological order.

    {1 A conflicted rebase keeps the gate closed}

    The busy-rebase arm covers a [Rebase] that is queued-next or running, but a
    rebase that hits conflicts {e completes} as an op — the continuation is a
    queued [Merge_conflict] respond, and the base branch's tip is only rewritten
    when that resolution lands and force-pushes. During that window every other
    arm reads fresh: a same-name freshen rebase (main → newer main) leaves
    [branch_rebased_onto] equal to the structural base, and a pure-chain child
    has no merged deps so sibling containment is vacuous. Cutting then takes the
    doomed pre-rebase tip and the child's PR is born stale (the
    connector-adapter-shape-unification patch-5 / PR #3811 failure mode). The
    [base_patch_has_conflict] input — the base patch's [has_conflict], set on
    every path that enqueues [Merge_conflict] — holds the gate closed while the
    unresolved conflict is known locally. Successful conflict resolution clears
    the flag before the rewritten branch can launch dependents; a conflict
    rebase [Noop] also clears it so the flag continues to track GitHub conflict
    state, with the next poll re-setting it and re-enqueueing [Merge_conflict]
    if the conflict persists. *)

type defer_reason =
  | Base_patch_busy_with_rebase of { base_branch : string }
      (** The base patch already has a [Rebase] in flight (busy with op
          [Rebase], or [Rebase] is queued and highest-priority). Wait for it
          rather than racing it. *)
  | Base_resolving_conflict of { base_branch : string }
      (** The base patch has an unresolved conflict ([has_conflict]): a rebase
          of the base conflicted mid-flight, or GitHub reports its PR
          conflicting with its own base. Either way the base branch's tip is
          pending a rewrite by the conflict-resolution pipeline, so a cut taken
          now would build on commits about to be force-pushed away. *)
  | Base_not_fresh_for_cut of { base_branch : string }
      (** The base patch's local branch is not yet rebased onto its
          structurally-correct base — a dependency has merged but the rebase
          that absorbs it has not landed. The base needs that rebase before
          [Start] can fire. *)
  | Base_missing_merged_sibling of { base_branch : string }
      (** The base is structurally settled on its own lineage, but the launching
          patch fans in on a sibling dependency that merged to main
          independently and whose squash commit the base does not yet contain.
          The base must be rebased to absorb that sibling before
          [Start]/[Rebase] can fire. Computed against the launching patch's full
          merged-dep set, so it also covers arbitrarily deep fan-in nesting. *)
[@@deriving show, eq, sexp_of, compare]

type decision = Allow | Defer of defer_reason
[@@deriving show, eq, sexp_of, compare]

val decide :
  base_is_main:bool ->
  base_branch:string ->
  base_patch_merged:bool ->
  base_patch_busy_rebasing:bool ->
  base_patch_has_conflict:bool ->
  base_structurally_fresh:bool ->
  base_contains_merged_siblings:bool ->
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
    + [base_patch_has_conflict = true] → [Defer Base_resolving_conflict]. The
      conflicted-rebase continuation of the previous arm: the [Rebase] op
      completed by conflicting, and until the [Merge_conflict] resolution lands
      the base's tip is pending a rewrite.
    + [base_structurally_fresh = false] → [Defer Base_not_fresh_for_cut].
    + [base_contains_merged_siblings = false] →
      [Defer Base_missing_merged_sibling]. Checked last: only fires when the
      base is otherwise settled on its own lineage but still lacks a merged
      sibling dep's squash commit. The first two arms
      ([base_is_main]/[base_patch_merged]) already imply containment.
    + Otherwise → [Allow].

    Note: this decision does not look at the dep graph itself. The caller
    resolves [c]'s base to a single base patch (or main) and passes the relevant
    facts, including whether that base is structurally fresh and whether it
    contains [c]'s merged siblings. *)

val short_label : decision -> string
(** A short, lowercase, snake_case identifier for the decision arm, suitable for
    activity logging. Always non-empty and ≤ 32 characters. Examples: ["allow"],
    ["defer_base_busy_rebasing"], ["defer_base_conflicted"],
    ["defer_base_stale"], ["defer_base_missing_sibling"]. *)
