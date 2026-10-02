# Feature-branch integration

How a descendant branch reaches the root in the
[feature-branch sample](feature-branch-sample.md). Readiness is covered in
[Branch HEAD checks](feature-branch-checks.md).

## Direct intermediate bases

Every descendant is based on its direct dependency's branch, not on `main`.
Patches 2 and 3 are based on patch 1; patch 4 is based on patch 3. The root
(patch 1) is the terminal boundary: integration stops there, and nothing a
descendant does writes to `main`.

## Integrating into the root

Once a descendant's HEAD is ready, the supervisor integrates it with ordinary
Git, not a hosting-provider merge:

1. Create a temporary worktree on the root branch, so the user's checkout is
   never touched.
2. Take the shared root-write lock, so only one integration writes the root at
   a time.
3. Merge the descendant with a normal `--no-ff` merge commit.
4. Push the root with a normal push (no force) and remove the worktree.

The lock is shared by every integration into the same root, including those
from different descendants.

## Rebasing descendants

When a patch integrates, its own dependents are rebased onto the new base. A
dependent of an integrated patch is re-based on that patch's own base, using
the usual rebase machinery (see `lib_core/rebase_decision.mli`).

## Nested example

Patch 4 depends on patch 3.

```
before:  main <- 1 <- 3 <- 4     (patch 4 base: patch 3)
after:   main <- 1 <- 4          (patch 4 base: patch 1)
```

Until patch 3 integrates, patch 4 is based on patch 3. When patch 3 is merged
into patch 1, patch 4 is rebased and its base becomes patch 1. Its base is
never `main`, because the root is the terminal boundary.

## Recovery and review

Integration is designed to be retried safely. See the
[root guide](feature-branch-sample.md) for the topology and
[Branch HEAD checks](feature-branch-checks.md) for readiness.

- **Dirty or diverged root checkout:** the supervisor refuses to integrate
  while the root checkout has uncommitted changes or has diverged from its
  remote, rather than overwriting work. Clean or reconcile it, and the next
  attempt proceeds.
- **Already-contained heads:** if the root already contains the descendant's
  HEAD, integration is a no-op success. Repeating it is idempotent.
- **Conflicts:** a merge conflict is routed back to the descendant, whose agent
  resolves it on its own branch. The root is never left half-merged.
- **Retry backoff and cap:** transient failures are retried with increasing
  delay, up to a fixed cap, after which the patch needs intervention.
- **Readiness invalidation:** every update to the root invalidates root
  readiness, so earlier check results never carry over.
- **Root promotion:** the root is promoted for review only after all
  descendants have integrated and fresh checks pass on its current HEAD. The
  root PR is the only one, and it stops there for human review.
