# Feature-branch sample field guide

A small live run of feature-branch mode. The sample is documentation only; its
purpose is to exercise branch-only descendants end to end.

Guides in this sample:

- This guide: topology and commands.
- [Branch HEAD checks](feature-branch-checks.md): how descendants become ready.
- [Git integration](feature-branch-integration.md): how descendants reach the root.

## Root versus descendant

- The **root** (patch 1) is based on `main`. It is the only patch with a PR,
  and its automerge stays disabled so the result stops for review.
- A **descendant** (patches 2, 3, 4) is a branch-only patch. It is pushed
  without a PR and is integrated into the root by the local supervisor.

## Topology

```
main <- 1 <- {2, 3 <- 4}
```

Patches 2 and 3 are siblings based on patch 1. Patch 4 is nested: it is based
on patch 3 until patch 3 integrates, and then on patch 1.

## Running and resuming

Start the run from the checkout that holds the gameplan:

```
onton --gameplan gameplans/feature-branch-sample-20261002.json --feature-branch
```

Resume it by project name:

```
onton feature-branch-sample-20261002
```

The gameplan JSON belongs to the invoking checkout. It need not already exist
on the target `main`.
