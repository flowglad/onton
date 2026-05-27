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
