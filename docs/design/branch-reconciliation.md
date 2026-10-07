# Branch reconciliation

Delivery is sequenced in the [branch reconciliation workstream](../workstreams/branch-reconciliation.md):
M1 records and stabilizes the existing foundation, M2 qualifies a useful release,
and M3 completes this design. The intermediate release does not waive safety
requirements or mark the full cutover complete.

Branch reconciliation moves a managed checkout toward a captured base revision
and publishes a captured result. The pure owner is `Branch_reconcile`; Git
observations and mutations belong to `Branch_reconcile_executor`.
`Branch_reconcile_runner` checkpoints every decision through
`Runtime.update_persisting` before dispatching its command.

## Checkout observations

`Git_observation` is the shared representation of branch identity, HEAD, staged
paths, unstaged paths, untracked paths, unmerged index entries, and sequencer
state. It parses porcelain v1 with NUL delimiters, including rename source
records. Filenames containing whitespace, quotes, or newlines remain literal.
Malformed status output cannot establish a clean checkout. Failed probes are
errors, distinct from successfully observing absent refs or absent sequencers.

A staged conflict resolution is ready for continuation when the sequencer is
active, the index has no unmerged entries, and tracked files have no unstaged
changes. It does not require an agent commit. The sequencer identity contains
its pinned target and replay step; incidental HEAD changes do not establish
repair progress. A rebase that is recreating a merge also records its active
merge heads. An empty staged diff at that step still requires a merge commit to
retain its parents; only an empty ordinary replay step is skipped. The core
checkpoints a dedicated merge-completion command before Onton commits that
resolution, then checkpoints the resulting head before continuing the rebase.
Recovery verifies the exact parents, unchanged tree identity, and remaining
sequencer context to recognize a completed merge after lost acknowledgement.
It never asks the repair agent to manufacture this commit.

## Durable protocol

Each operation has its own identity and command sequence, independent of patch
message generations. The owner produces one private command at a time. Results
must match both identity components; duplicates and predecessor results cannot
advance the current command. Intent changes wait for the active operation to
finish rather than changing its pinned target. An intent distinguishes base
integration from publication of a captured session revision. Session publication
requires the observed branch to match that revision and does not integrate a
moving base as a prerequisite. An interrupted session whose final HEAD probe
was unavailable instead supplies its session identity; preparation captures and
pins the actual branch revision. A later session is a distinct intent. Dirty
files are retained while publishing the captured commit; an active sequencer
requires recovery rather than publication.

The worktree capability supplies Git execution and encodes project and branch
names into disjoint recovery-ref namespaces. The runner acquires patch write
ownership, or accepts a scoped ownership handle from an enclosing session.
Expired ownership handles cannot authorize execution. Checkpointing and command
dispatch use the runtime and patch carried by that handle.

Provisioning resolves a new branch's source to a commit before creation and
checks that the materialized checkout still has that revision. Its first
materialization receipt is retained in the project/branch recovery namespace.
A new branch proves a replay boundary; an adopted branch records its actual
starting revision without claiming ownership of earlier commits. The receipt
is checkpointed before creation hooks or agent work. Retry can recover it from
Git if the snapshot write failed. Initial legacy anchors are derived from that
receipt, so later base movement does not change the recorded starting boundary.

Preparation observes Git, records source and target revisions, and pins them
under `refs/onton/reconcile/<project-and-branch>/<operation>/<command>/`.
The captured remote lease and any completed candidate are retained there too,
including before integration or repair can change the checkout. Background fetches
cannot remove the only ref keeping that remote evidence reachable.
Integration checks the checkout identity, captured HEAD, index, and sequencer
before using the captured target. Recorded replay boundaries precede plain
rebase. History-preserving integration uses merge. Recovery refs are retained;
project pruning must remove the project namespace only after its dependents no
longer need its boundaries.

An existing rebase or merge is adopted from its sequencer metadata. Its source
and original target are checkpointed and pinned before continuation or a repair
turn. Staged resolutions remain in place. Adoption records the target without
inventing an original base branch name. After confirming publication, the owner
starts a fresh observation of the requested base, so later base movement cannot
replace the active integration's target. Restart inspection restores captured
recovery refs before authorizing further repair or continuation.

Conflicts produce a repair token. A second checkpoint claims that token before
an agent is dispatched; repeating the claim cannot authorize another turn.
The dedicated repair handler starts an isolated backend turn with instructions
to edit and stage resolutions. It does not invoke ordinary implementation-session
completion or publication. The repair claim captures the checkout HEAD, which
can differ from the named branch during a rebase. The handler checks that HEAD
before dispatch; subsequent inspection checks the captured HEAD and sequencer
even after a lost probe or restart. Unexpected agent commits, resets or
sequencer changes stop continuation while retaining work. A completed
agent turn triggers inspection, then Onton continues the sequencer. Two consecutive completed turns at the same
step without reducing the number of index conflicts exhaust content repair and
enter history recovery with reason `repair_no_structural_progress`. Advancing a step or resolving additional
conflicts resets this budget. Transport failures and restart do not spend it.

Publication retains the candidate after failures and uses its immutable SHA
and an explicit expected remote SHA, including the empty lease for an absent
remote branch. PR #482's content-checked `Rewrite_lineage` proof is shared by
the existing publisher and the new executor through
`Git_publication_evidence`. A refreshed lease alone never grants rewrite
permission. The release contract requires rewrite branches to replay newly
identified remote work into the preserved candidate, and history-preserving
branches to merge that work. A rewrite race first captures the incoming remote,
original candidate, previous lease, and recorded base boundaries in a planning
command. It selects a reachable recorded boundary whose preceding work is
preserved in the candidate. If none qualifies, a unique Git common ancestor
provides an explicitly inferred boundary. Probe failures retry rather than
masquerading as missing ancestry. The selected boundary and its inputs are
checkpointed before checkout; planning never replaces the captured pair with a
later remote tip.

The executor pins the captured revisions before a separately checkpointed
checkout, then replays incoming commits onto the preserved candidate. Checkout
uses Git's keep mode so late staged changes can prevent the transition without
being discarded. Restart distinguishes an untouched checkout, a completed
checkout, and a completed replay, including a replay whose changes were already
present. Remote replay preserves merge topology through Git's merge-preserving
rebase sequencer; its content conflicts use the same staged-resolution protocol.
Managed rebases explicitly disable automatic ref updates and autostash, so user
Git configuration cannot silently move other branches or change dirty-work
handling.
History-preserving races retain merge integration. No common ancestor and
ambiguous common ancestors currently enter agent recovery; further deterministic
reconstruction and patch-equivalence recovery
remain required before cutover. A completed candidate must never be discarded
to refresh a lease.

Remote replay records a separate durable integration receipt before publication,
including source, preserved candidate, chosen boundary, resulting revision, and
execution or recovery evidence. This receipt survives later intents without
claiming that the patch's own preserved revision is a base boundary.

Local base integration records a receipt before publication, retaining the
original source, target, replay boundary, policy, and resulting revision. A
publication race cannot replace this base pair with its remote integration
pair. Subsequent operations capture the receipt targets newest-first, followed
by the proven materialization boundary. Observation selects the first boundary
verified reachable from the captured source. A failed ancestry probe retries;
it does not discard a boundary as unreachable. Restart during observation
retains the complete captured candidate list.

Confirmation directly observes the remote and its topology. Equality or a
remote descendant containing the candidate completes publication. Lost push
acknowledgements therefore do not repeat implementation. PR #483's
agent-published resolution with a stale remote-tracking ref is the same
confirmation contract: direct remote equality settles publication without
another push, rebase, or repair turn. If the remote still matches the captured
lease after a failed push, publication retries the preserved candidate against
that same lease, even when the rewrite made their histories diverge. A failed
remote probe retains the candidate and waits without spending repair budget. Transport and uncertain
outcomes wait with exponential backoff starting at five seconds and capped at
five minutes, honoring longer provider delays. Waiting and repair return control
to the caller rather than retaining a worker slot.

Recovery inspects Git before authorizing another mutation. The pure inspection
boundary checks the sequencer's original source, actual target, managed branch,
and policy against the captured operation. A matching active integration with
no unresolved index entries or unstaged tracked changes continues without
claiming an agent repair turn, including after interruption before its first
command result was checkpointed. An unexpected integration context enters
history recovery without continuing that sequencer. Malformed or contradictory
checkout observations retry as failed probes. An active sequencer after a failed
Git command does not itself establish a content conflict: when no unresolved
index entries or unstaged tracked changes remain, the operation waits and
inspects before continuing, without dispatching or charging an agent turn. Preparation recovery refreshes
remote refs before resolving an as-yet-uncaptured target. Each operation records
its local action (publish the captured source, or integrate with a captured
policy) independently of its initiating intent. A publication race can require
integration even though its original intent was session publication. Restart
resumes that recorded action and policy; it cannot skip remote integration or
silently substitute a rewrite for a recorded merge. An unchanged source can
resume its captured integration. A changed source needs a completed
named-branch reflog receipt for the captured source and target; merely containing
the target is insufficient. Other changes stop with a specific intervention
reason while retaining recovery refs.

## Agent recovery fallback

The release endpoint includes agent-driven Git recovery after deterministic
recovery strategies are exhausted. This is a distinct durable recovery phase,
not another implementation session. Its input includes pinned source, target,
candidate and remote revisions, provenance, and the strategies already tried.
The agent inspects history and repairs a candidate while preserving valid patch
and remote work. Transport outages and permanent permission failures do not
trigger content-repair attempts.

Onton retains publication authority. The recovery agent requests publication;
Onton inspects the actual checkout and sequencer, verifies preservation evidence,
and publishes a captured candidate with an explicit lease. Neither an agent's
success report nor a refreshed lease establishes that remote work was retained.
Unverifiable preservation produces a specific intervention reason with recovery
refs retained. Recovery turns have durable identities and a bounded progress
budget, so restart and duplicate results cannot dispatch or charge a turn twice.
History recovery is a distinct mode of the durable agent-turn protocol. It is
entered after exhausted staged conflict repair, invalidated Git preconditions,
unexpected agent Git mutations, uncertain local integration and publication
state, or a completed publication check that cannot establish preservation of
remote work. A negative preservation result is distinct from a failed probe:
only the failed probe retries as infrastructure. A checkpointed verification command pins the captured
revisions and every observed recovery HEAD, named branch tip and remote tip before
offering a turn. These revisions become durable preservation obligations across
later turns and restarts. Recovery may change
HEAD; staged content repair may not. Both modes share duplicate-safe claims,
interruption handling and structural-progress accounting.

After a recovery turn, Onton verifies a clean named checkout, the required
integration target, preservation of the original candidate (or source), and
preservation of both the captured and freshly observed remote revisions. The
first recovery inspection also captures the current checkout HEAD as an immutable
recovery baseline: newer commits discovered after interruption must survive too.
Ancestry or revision-bound rewrite-lineage evidence must establish preservation
of every retained revision. Capacity acquisition is followed by another owned
inspection before an agent runs. Dirty history-recovery checkouts stop with
`history_recovery_dirty_worktree`, retaining staged, unstaged and untracked work.
A lost backend acknowledgement or failed probe resumes verification rather than
repeating successful agent work. Two completed recovery turns without structural
progress stop with `recovery_preservation_unproven`, retaining the recovery refs.
Remaining deterministic cutover paths must route exhausted recovery through this
owner before release.

Publication observes and confirms the actual configured push URL, even when it
differs from the fetch URL. Multiple distinct push destinations stop specifically
before mutation; partial multi-destination publication is not represented by the
single-remote operation model. Each operation checkpoints a BLAKE256 fingerprint
of its resolved destination, without storing URL credentials. A changed endpoint
stops with `publication_destination_changed`; explicit human resume permits
inspection and rebinding, while ordinary recovery retains the original authority.
A recorded replay grants publication authority
only for its captured candidate and lease, and only when the leased revision lies
between the recorded upstream and captured source. This permits legitimate
conflict-resolution content changes without treating inferred boundaries as
proven ownership. Other publication cases retain the ancestry/lineage safeguards.

An ancestry-preserving integration also requires source ancestry before a
reflog can establish completion; an external rebase cannot impersonate an adopted
merge. Direct Git evidence can confirm external head reversions. Repeated forge
conflicts for a settled, verified head/base pair become an observation wait rather
than new reconciliation requests.

## Scheduling

Scheduled rebases and conflict reports submit a distinct reconciliation request keyed by the
scheduler message identity. Repeated delivery cannot restart that request;
a later request observes the base again. An already satisfied and published
source/target pair settles without a Git mutation. The integration root selects
history-preserving policy; other managed patch branches select rewrite policy.
The scripted gameplan patch can reconcile deterministically but stops with a
specific intervention reason if agent repair would be required.

Each unfinished operation owns a `Reconcile_branch` outbox message whose identity
is independent of patch generations. Command checkpoints create that message in
the same snapshot as pending Git work. Ordinary message reconciliation cannot
obsolete it, including while a backend session still owns its separate message.
Finishing a Git dispatch returns a retrying operation's message to `Pending` and
releases patch ownership. The supplied clock determines when Git work is due;
backoff and content-repair waits remain visible without taking a worker slot.
Readiness is separate from worker occupancy: unfinished reconciliation, including
an intervention retaining work, blocks review promotion, merge eligibility, and
dependent cuts even when legacy CI and base observations still look ready.

The runner resumes checkpointed Git work under scoped patch ownership. It checks
the operation identity again after acquiring ownership. Content repair uses the separate staged-resolution agent interaction under
patch ownership and the session concurrency limit. Waiting for an agent slot
holds neither patch nor root Git ownership. After capacity becomes available,
the runner acquires ownership and revalidates the durable turn claim before
invoking the backend. Ordinary Start and Respond actions use the same resource
order, and cancellation while waiting still completes their busy lifecycle.
Each agent turn releases capacity and ownership before another turn is queued.
The handler inspects the
index and sequencer after the turn and lets the core authorize continuation.
An interrupted backend leaves pending work; restart inspects Git before a
replacement turn. Infrastructure failures back off without spending repair
budget.

## Session completion

The session driver checkpoints a typed backend-completion receipt before any
post-session push or cancellation exit. The receipt retains the session UUID,
Start/Respond context, operation kind, originating message, backend outcome and
observed local revision. An unavailable revision remains unknown evidence.
Subsequent publication failure cannot overwrite this receipt with a push error.
Cancellation checkpoints a session-scoped publication request and its outbox
message atomically with that receipt, without probing Git in a cancelled scope.
The scheduler can therefore publish commits left by an interrupted backend.
Normal session publication runs through the checkpoint runner under the session's
scoped write ownership. Publication failures remain pending reconciliation work
and do not become backend failures. A successful Start receipt can resume
publication and PR creation without another backend invocation when its
accepted human guidance is unchanged. New guidance requires a new turn.
The scripted gameplan publisher uses the same completion checkpoint and
publication protocol; transport retries preserve its artifact commit and never
select an implementation backend.
The core recognizes a never-published branch still at its recorded new-branch
materialization boundary as no committed patch work; that outcome permits a new
implementation attempt rather than claiming publication succeeded.

## Destination-root integration

A contribution request belongs to the destination root and names the captured
contributor revision. This intent enforces ancestry-preserving integration,
regardless of the caller's supplied policy. Git resolves and fetches that commit;
a moving contributor branch name cannot replace it. Post-command observations
must prove that both the captured target and existing root ancestry survive
before the core authorizes publication. A failed postcondition enters history
recovery while retaining the pinned revisions.

The root records a publication receipt only after direct remote confirmation.
Local integration and push acknowledgement alone cannot complete a descendant.
The application consumes the receipt in the same checkpoint that settles
publication, completing only a descendant whose observed head still matches the
captured revision. A known newer descendant head remains unintegrated. Sibling
integrations wait while root reconciliation is pending, and repair runs in the
root checkout through the shared repair protocol. Lost acknowledgements resume
confirmation without repeating the merge or implementation.

## Persistence

Snapshots carry per-agent reconciliation checkpoints. Writers emit version 2;
readers accept versions 1 and 2. Loading a version-1 file preserves its bytes at
`<snapshot>.pre-v2` before an upgrade can replace it. Legacy sessions, worktrees,
anchors, and other agent state remain readable.

Writes flush and synchronize the temporary file, atomically replace the
snapshot, then synchronize the containing directory. A failed checkpoint stops
command dispatch. Git and the snapshot are not one transaction: recovery
inspects the actual checkout and remote when the last command's outcome was
not durably acknowledged.

Downgrade requires reconciling Git and restoring compatible version-1 state.
An older binary must not execute a live version-2 checkpoint.

## Algebraic contracts and evidence

Pure properties exercise duplicate-result idempotence, predecessor isolation,
settled fixed points, serialization/recovery equivalence, compatible
materialization/intent ordering, retry boundaries, repair-budget isolation, and
convergence with duplicate results. Both pure test executables link only
`onton_core`. Real Git tests inspect recovery refs, remote trees, staged
resolutions, multiple replay steps, lost acknowledgements, and checkpoint
failure behavior.

## Cutover status

This change establishes the core, executor, checkpoint runner, snapshot storage,
and unified status parser. The existing ordinary dirty-check path uses the shared
parser, and both publication implementations share lineage validation.

Scheduled rebases, conflict delivery, session publication, and destination-root
integration now use the checkpoint runner. Conflict delivery no longer runs a
separate implementation session or publisher. Deterministic conflict reconciliation requires no agent
slot; the dedicated repair dispatcher acquires capacity only for a repair turn.
The complete release cutover remains outstanding: remove legacy execution APIs
and replace independently writable legacy fields with derived views; add forge
request/head/base identities and replace legacy forge eligibility retry counters;
migrate legacy branch counters; complete interruption recovery across provisioning boundaries; connect project pruning;
and run the complete interruption, stack, forge, and SourceHut acceptance matrix.
The current change is not the completed release described by that cutover.
