(** Pure decision: does a patch's resolved base branch already contain the
    squash commit of every *merged* dependency of that patch?

    This closes the fan-in blind spot: when a patch [p] depends on several
    patches and exactly one is still open, [p]'s base is that sole open dep [b];
    [b]'s branch carries its own lineage but not a *sibling* dependency of [p]
    that merged to main independently (its squash commit lives on main, never an
    ancestor of [b]). Cutting/rebasing [p] from such a [b] would start it from a
    base missing one of its dependencies.

    Git ancestry is supplied as an [ancestor_oracle] (effectfully backed by
    [git merge-base --is-ancestor]), keeping this function pure and testable —
    the same pattern as {!Rebase_decision.plan}. The result feeds the
    [base_contains_merged_siblings] cache and the Start/Rebase eligibility gate.
*)

val contains_merged_siblings :
  graph:Graph.t ->
  patch_id:Types.Patch_id.t ->
  has_merged:(Types.Patch_id.t -> bool) ->
  merge_sha:(Types.Patch_id.t -> string option) ->
  branch_of:(Types.Patch_id.t -> Types.Branch.t) ->
  main:Types.Branch.t ->
  ancestor_oracle:(string -> descendant:string -> bool) ->
  bool
(** [contains_merged_siblings ~graph ~patch_id ~has_merged ~merge_sha ~branch_of
     ~main ~ancestor_oracle] is:

    - [true] when the patch's structurally-correct base ([Graph.initial_base])
      is [main] (every merged dep is already a squash commit on main), or when
      every merged dependency's [merge_sha] is an ancestor of the base branch;
    - [false] (fail-closed) when any merged dependency's [merge_sha] is unknown
      ([None]) or is not yet an ancestor of the base branch;
    - [false] when the patch has more than one open dependency (not at the
      last-but-one-merge edge; the value is irrelevant because the patch defers
      on dependency satisfaction anyway, and [Graph.initial_base] is undefined
      there).

    Only the patch's own dependencies are consulted, so an unrelated advance of
    main never flips this to [false]. *)

val stale_chain_rebase_target :
  graph:Graph.t ->
  patch_id:Types.Patch_id.t ->
  has_merged:(Types.Patch_id.t -> bool) ->
  merge_sha:(Types.Patch_id.t -> string option) ->
  branch_of:(Types.Patch_id.t -> Types.Branch.t) ->
  main:Types.Branch.t ->
  ancestor_oracle:(string -> descendant:string -> bool) ->
  Types.Patch_id.t option
(** Which patch in [patch_id]'s base chain should be rebased next so that the
    base branch absorbs [patch_id]'s merged siblings — the demand target for
    [Reconciler.detect_sibling_stale_bases].

    When [contains_merged_siblings] is false for a fan-in patch [P] whose sole
    open dep is [B], the missing squash commits live on main and can only enter
    [B]'s branch by flowing {e up} [B]'s chain of still-open ancestors, each
    layer rebasing onto its structural base, bottom-up. Rebasing [B] alone is
    not enough — when [B] is itself stacked, its rebase target lacks the squash
    just like [B]'s branch does, so the rebase completes without absorbing
    anything and the demand re-fires forever (the FLI-3 seed-440463877/seed-7
    livelocks; in production, a pointless force-push per poll tick). Conversely,
    demanding every chain layer each tick starves the upper layers: the freshly
    re-queued lower rebase keeps the layer above it deferred on
    [Base_patch_busy_with_rebase].

    This returns the {e frontier}: the deepest stale layer of [B]'s chain whose
    own structural base already contains every merged-dep squash of [P] (main
    contains all of them by definition; a layer above a fresh layer rebases onto
    that fresh branch). Healing the frontier moves it up one layer; once [B]
    itself absorbs the squashes, [contains_merged_siblings] flips true and the
    demand stops. Exactly one rebase per stale layer, no starvation, no
    redundant rebase of already-fresh layers.

    Returns [None] when: [patch_id] is not at the last-but-one-merge edge (no
    open dep, or more than one); there are no merged deps; a merged dep's
    [merge_sha] is unknown (fail-closed); the whole chain already contains the
    squashes; or the chain is interrupted by a layer with more than one open dep
    (the squash cannot flow past that fan-in until its own merges land —
    fail-closed, re-derived on a later tick). Whether the returned target is
    actually rebasable (holds a PR, no [Rebase] already queued) is the caller's
    concern ([base_rebasable] in the detector). *)
