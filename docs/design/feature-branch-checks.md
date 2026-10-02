# Branch HEAD checks

Part of the [feature-branch sample](feature-branch-sample.md). Next:
[Git integration](feature-branch-integration.md).

## Descendants have no PRs

A descendant publishes its branch and nothing else: no PR is opened, so there
is no review, approval, or PR status to wait for. Only the root has a PR.

## What "ready" means

Readiness is read from the checks attached to the exact commit at the
descendant branch HEAD, not from a PR.

- Checks must be attached to the current HEAD SHA. Results for an earlier
  commit do not count, so a new push restarts the wait.
- At least one check is required. A HEAD with no checks is not ready; it is
  never treated as vacuously passing.
- Every check must have a passing conclusion; neutral and skipped checks count
  as passing. Pending checks mean keep waiting; any failure blocks
  integration.

## Push-triggered workflows are required

Without a PR, `pull_request` workflows do not run for these branches, so this
sample relies on workflows triggered by `push` to attach checks to a
descendant HEAD. The
base `main` CI is push-limited to `main`, so this sample adds
`.github/workflows/feature-branch-sample.yml`, which runs on pushes to
`feature-branch-sample-20261002/patch-*`.

## Integration is immediate

Once the HEAD has at least one check and all checks pass, the supervisor
integrates the descendant into its base right away. No further approval or
delay is added. See [Git integration](feature-branch-integration.md) for how
the merge happens.

## Pausing a descendant

Each patch has an automerge toggle. Turning it off for a descendant pauses
integration: checks still run and may pass, but the supervisor holds the
branch until the toggle is turned back on. The root's toggle stays disabled so
the final result stops for review.
