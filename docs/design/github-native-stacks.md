# GitHub native stacks and Onton dependencies

GitHub's native stack is a forge object. Onton's patch graph is the authority for
work ordering and expected direct PR bases. A chain in Onton's graph does not
prove that GitHub linked the PRs into a native stack; an ad-hoc GitHub stack may
also include PRs outside Onton's gameplan. Poll `PullRequest.stack` to identify
native membership. Do not infer it from `baseRefName`.

## GitHub contracts

- A stack is linear and confined to one repository. Each PR above the bottom
  targets the head branch of the PR below it. `PullRequest.baseRefName` is this
  **direct base**; `PullRequest.stack.baseRefName` is the **trunk** used for
  branch rules and checks. These can differ. A stack can retain merged entries,
  so a PR's absolute `stackEntry.position` is not necessarily its position
  among open PRs. [Stack rules][rules], [GraphQL fields][api]
- GitHub owns the linked bases. The ordinary PR base PATCH rejects a stack
  member. After a native stack merge, GitHub rebases and retargets the remaining
  open PRs. Reordering or dissolving a stack is a distinct stack operation, not
  an ordinary base update. [Managing stacks][manage], [merging stacks][merge]
- Merging a stack PR selects every unmerged PR below it as one atomic request,
  possibly into a merge queue. The legacy synchronous PR merge endpoint cannot
  merge a stack; GitHub requires `merge-async` and exposes a result UUID for
  polling. Native auto-merge is unavailable for stacks. [Stack API][api],
  [async merge API][async], [merging stacks][merge]

## Onton composition

- Onton's graph still decides when a patch can start and which **direct** base
  its PR should target. It allows fan-in, while GitHub's native stack is a
  linear chain. Do not replace graph edges with GitHub stack membership.
- For an unstacked PR, Onton can reconcile a stale direct base with the
  ordinary PR PATCH. For a native stack, Onton observes the direct base but
  never sends that PATCH. If GitHub's direct base differs from the graph's
  unambiguous expected base, Onton pauses patch work until GitHub retargets it
  or the stack is deliberately changed. Draft promotion also waits for this
  alignment. This avoids pushing a branch rebased against one base while its
  GitHub PR still targets another.
- Onton's existing automerge operation uses the legacy synchronous endpoint.
  It is disabled for native stack members. Supporting unattended native stack
  merges requires a separate operation that selects the intended open bottom
  PR, calls `merge-async`, persists its UUID, polls the terminal result, and
  maps both merge-queue and failure outcomes back to Onton's per-patch state.
  A higher PR must never be selected solely because it is locally ready: that
  would merge every unmerged PR below it, including patches whose Onton gates
  have not passed.

[rules]: https://docs.github.com/en/pull-requests/reference/stacked-pull-requests
[api]: https://docs.github.com/en/pull-requests/reference/stacked-pull-requests-apis-and-webhooks
[manage]: https://docs.github.com/en/pull-requests/how-tos/create-pull-requests/managing-stacked-pull-requests
[merge]: https://docs.github.com/en/pull-requests/how-tos/merge-and-close-pull-requests/merging-stacked-pull-requests
[async]: https://docs.github.com/en/rest/pulls/pulls#merge-a-pull-request-asynchronously
