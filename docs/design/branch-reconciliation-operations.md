# Operating branch reconciliation

The patch detail view shows the operation, phase, source, target, candidate,
remote lease, replay evidence, retry deadline and reason. Snapshots and agent
events retain the same reconciliation state. A retry stays pending in the
outbox while worker capacity and Git ownership are released. An intervention
retains work but does not retry through polling or restart.

## Intervention and deliberate resumption

Stop the project before manually changing its checkout. Preserve its snapshot,
managed repository and worktrees together, including untracked files, the index,
sequencer directories and private `refs/onton/reconcile/` refs. Do not delete those
refs to clear an error: they retain source, target and recovery revisions.

Inspect the operation's captured SHAs and the checkout's `git status`, index,
sequencer and reflogs. Correct the reported problem without replacing the
captured source or target with moving branch names. The repair agent completes the entire Git repair, including repository-wide edits,
merge or rebase completion, commits and applicable checks. Onton trusts a successful
report, pins and checkpoints the resulting HEAD, and publishes that exact SHA with
an explicit lease. It does not independently approve the repaired tree or history.
The detail view distinguishes agent-reported integration evidence from deterministic
verification. Failed HEAD capture retries without rerunning a successful turn.
Lease movement retains the accepted candidate and requests work against the newly
observed remote. Captured revisions and observed recovery revisions remain pinned.

| Reason | Recovery action |
|---|---|
| `dirty_worktree` / `history_recovery_dirty_worktree` | Preserve and reconcile staged, unstaged and untracked work. Keep the captured branch tip intact; preserve intended commits on a separate branch or worktree. Resume with a clean checkout. Onton does not automatically stash or discard it. |
| `publication_during_integration` | A legacy or publication-only checkpoint encountered a live sequencer. Inspect and complete or deliberately abort that integration while retaining its work before resuming publication. |
| `repair_attempts_exhausted` | Two failed repair turns exhausted the recovery budget. Inspect retained source/candidate/remote refs and reconstruct the intended result before resuming. |
| Permission or repository-policy rejection | Correct credentials or policy, then resume. Do not rerun successful implementation solely because publication failed. |
| `multiple_push_destinations` | Retain the checkpoint and configure one intended push destination before resuming. Onton cannot represent partial publication across distinct push URLs as one confirmed remote ref. |
| `publication_destination_changed` / `publication_destination_unrecorded` | Review the configured push URL and the retained candidate. Restore the original destination, or deliberately approve the intended destination by resuming. Resume inspects that endpoint before rebinding authority; ordinary restart never silently redirects publication. |
| Checkpoint failure | Restore writable, durable snapshot storage before resuming. Git may have completed the last command; let recovery inspect it. |
| `patch_merged` / `patch_wontdo` | Terminal disposition suppresses new Git and repair work. Review the terminal state before deliberately reopening work. |

Restart Onton and use the existing **bump** action (`b` on the selected patch),
or send deliberate human guidance. This resets intervention and repair-attempt
limits while retaining the operation identity and captured revisions. The next
step inspects actual Git state and starts a complete repair turn when needed. An
existing migration candidate can be accepted without a new commit when that turn
validates it and reports success. Old attempt counters do not grant acceptance. Duplicate resume events do
not dispatch duplicate repair. Bumping without correcting the cause can return
to the same intervention.

Repeated forge conflict reports for an already verified and published head/base
pair show an observation wait. Direct remote-ref evidence can establish a real
head reversion even when it equals a previously seen SHA. Failed remote probes
do not establish absence or authorize mutation.

## Upgrade and downgrade

Version-1 snapshots are read conservatively. Existing sessions, worktrees,
anchors and unrelated intervention reasons survive migration; obsolete branch
retry counters and expected-head markers are cleared. Existing work enters
observation-first reconciliation. A live legacy sequencer is retained for
intervention rather than blindly continued as a publication request.

Before replacing a version-1 snapshot, loading preserves its original bytes at
`snapshot.json.pre-v2`. New checkpoints use version 2. The backup is an upgrade
recovery artifact, not a current checkpoint after subsequent Git mutations.

For downgrade, stop all local/cloud runners for the project and retain the
current version-2 snapshot, repository and worktrees. Reconcile the actual local
and remote Git histories with the version-1 state to be restored, preserving
all newer commits and uncommitted work separately. Restore a compatible snapshot
only after that reconciliation, then start the older binary. Never change the
version field or point an older binary at a live version-2 checkpoint. Simply
copying the pre-upgrade snapshot over current state can repeat completed work.

Project pruning reclaims recovery refs only when surviving dependent checkpoints
no longer need them as their sole recovery anchors. It checks captured ref tips
before deletion and retains data when inventory or ancestry probes fail.
Unfinished reconciliation prevents terminal-project pruning. A shared managed
repository remains until every stored project using it is eligible for removal.

Pruning requires exclusive project lifetime ownership, including when a worker
was started with `--no-lock`. Project-directory removal uses a private retirement
journal: it renames the captured directory before deleting that payload. Restart
can finish interrupted cleanup without deleting a replacement project created at
the original path. Preserve unrecognized journals for inspection.

Forge reports carry the PR identity, head/base revisions and request identity.
Stale or duplicate replies cannot authorize new Git work. A repeated conflict
report for a satisfied pair remains a visible observation wait; a failed remote
probe does not prove that a branch disappeared. Git reconciliation remains the
publication authority even when forge metadata reports a mergeable result.

## Publication acceptance checks

`dune runtest` runs the owner model, pure properties, and focused Git fixtures.
The exhaustive real-Git publication race suite runs separately with
`dune build @publication-acceptance`. It uses isolated local repositories and
does not contact GitHub. Run it when changing publication, remote leases,
rewrite preservation, or restart handling, and before releases that touch those
contracts.

The exhaustive real-Git checkpoint interruption matrix runs with
`dune build @reconciliation-checkpoint-acceptance`, also outside the default gate.

The **Git reconciliation acceptance** GitHub Actions workflow is manually triggered
with `workflow_dispatch` on the selected branch. It has no push, pull-request,
or scheduled trigger, bounds the test command to 25 minutes, and uploads its
log even on failure. Normal CI does not run either exhaustive suite.
