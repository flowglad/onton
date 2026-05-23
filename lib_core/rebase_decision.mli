(** Pure decision: what value should the orchestrator pass as the [<upstream>]
    argument to [git rebase --onto <target> <upstream>] when it re-bases a patch
    branch?

    Git's three-arg rebase replays exactly the commits in [upstream..HEAD] onto
    [target]. When [upstream] is the {e current} tip of the previous base
    branch, that range is precisely "the patch's own work" — every commit the
    patch contributed since the last rebase. When [upstream] is stale (or set to
    the local main), the range can include commits that were absorbed into a
    squash-merge on origin/main and now live there as a single squash commit —
    leaving the original un-squashed commits stuck in the rebased branch's
    history (the case we saw on patch-6).

    {!Patch_agent.branch_rebased_onto_sha} records the SHA the base ref resolved
    to at the moment of the last successful rebase / Start. [upstream] returns
    that SHA when it is present; otherwise it falls back to the supplied default
    (the branch name, mirroring today's behavior). The handler
    [Worktree.rebase_onto] consults this decision before invoking [git rebase].
*)

val upstream : prev_base_sha:string option -> fallback:string -> string
(** [upstream ~prev_base_sha ~fallback] returns the upstream argument for
    [git rebase --onto NEW_BASE <result>]. When [prev_base_sha = Some sha],
    [sha] is returned (the recorded old-base tip). When [None] or the SHA is
    empty / whitespace, [fallback] is returned — typically the previous
    base-branch ref name, preserving the legacy 2-arg rebase semantics so an
    agent whose [branch_rebased_onto_sha] was never recorded still rebases onto
    something sensible. Pure and total. *)
