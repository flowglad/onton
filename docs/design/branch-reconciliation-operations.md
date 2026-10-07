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
captured source or target with moving branch names. During normal content repair,
stage resolutions and let Onton continue the sequencer; an agent commit is not
required. A history-recovery agent may reconstruct integration, but Onton still
verifies preservation and performs publication with an explicit lease. Every local
and remote revision observed during history recovery is pinned and checkpointed
as required preservation evidence, including work arriving between agent turns.

| Reason | Recovery action |
|---|---|
| `dirty_worktree` / `history_recovery_dirty_worktree` | Preserve and reconcile staged, unstaged and untracked work. Commit intended patch work or move unrelated work to a separately retained checkout. Resume with a clean checkout. Onton does not automatically stash or discard it. |
| `publication_during_integration` | A legacy or publication-only checkpoint encountered a live sequencer. Inspect and complete or deliberately abort that integration while retaining its work before resuming publication. |
| `recovery_preservation_unproven` | Two completed recovery turns failed to establish preservation. Inspect retained source/candidate/remote refs and reconstruct the intended result before resuming. |
| Permission or repository-policy rejection | Correct credentials or policy, then resume. Do not rerun successful implementation solely because publication failed. |
| `multiple_push_destinations` | Retain the checkpoint and configure one intended push destination before resuming. Onton cannot represent partial publication across distinct push URLs as one confirmed remote ref. |
| `publication_destination_changed` / `publication_destination_unrecorded` | Review the configured push URL and the retained candidate. Restore the original destination, or deliberately approve the intended destination by resuming. Resume inspects that endpoint before rebinding authority; ordinary restart never silently redirects publication. |
| Checkpoint failure | Restore writable, durable snapshot storage before resuming. Git may have completed the last command; let recovery inspect it. |
| `patch_merged` / `patch_wontdo` | Terminal disposition suppresses new Git and repair work. Review the terminal state before deliberately reopening work. |

Restart Onton and use the existing **bump** action (`b` on the selected patch),
or send deliberate human guidance. This resets intervention and repair-attempt
limits while retaining the operation identity and captured revisions. The next
step inspects actual Git state before any mutation. Duplicate resume events do
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

Recovery refs are retained conservatively for the project lifetime. Automatic
dependency-aware reclamation and the complete forge-context model remain M3 work.
