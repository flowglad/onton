# Workstream: Branch reconciliation

## Vision

Make branch reconciliation a first-class Onton subsystem: a pure functional core
owns decisions and durable operation state, while imperative handlers observe
Git, execute captured commands, and checkpoint results. Preserve valid patch and
remote work through conflicts, races, interruption, and publication failure.
Deliver useful reliability improvements before completing the architectural
cutover, retaining agent-driven recovery as the final fallback.

This workstream sequences delivery of [the architecture](../design/branch-reconciliation.md).
It supersedes the original single-release delivery constraint, not the terminal
behavioral requirements. M1 records the foundation already built; M2 makes that
foundation a useful, supportable release; M3 completes the full design.

## Current State

M1 is complete, M2 is qualified as a release candidate, and M3 is implemented
and locally verified in the current worktree. Merge, release publication and
representative live-provider trials are not claimed. The
[M3 completion audit](branch-reconciliation-m3-audit.md) records A01–A24,
formal-contract review, final gates and both isolated patch builds. The
[M2 release evidence](branch-reconciliation-m2-release-evidence.md) retains the
separate release/operator handoff.

All enabled reconciliation mutations use the durable owner and checkpoint
runner. The endpoint includes contextual forge observations, the complete
applicable deterministic fallback chain and bounded verified agent recovery,
owner-derived branch views, provisioning/repair restart handling, dependent-aware
recovery-ref reclamation and project lifetime/retirement authority. Independent
models and real-Git/runtime fixtures qualify the original acceptance contract.
No endpoint guarantee is deferred to another milestone.

### M3 implementation history

The following progress notes describe intermediate states and are retained as
history. Their references to remaining M3 work are superseded by the current
state and completion audit above. The terminal acceptance suite remains the
unchanged completion contract.

The implementation removes `Worktree_plan` and its anchor
side-channel executor, retains repair-turn identity across inspection and
restart, and rejects in-flight PR/branch-check results after their local request
context changes, including PR rediscovery. A typed forge envelope binds returned
PR identity to the client request; open PRs require an identified head/base SHA
pair and base branch. GitHub parsing and the real SourceHut fixture cover provider
identity, while property tests cover missing and mismatched context. Poll events
retain request identity. These guards do not establish provider cache freshness;
the complete observation algebra and A18 audit remain outstanding. Terminal
history-repair diagnostics retain both exhausted turns and their final identity
across snapshot restoration.

The preservation-policy audit separates required ancestry from the adopted Git
strategy. Existing rebases cannot weaken ancestry-preserving requests or root
contributions. The executor checks the captured requirement at integration,
publication, confirmation and completed-candidate restart inspection; remote
successors retain original obligations. Legacy downgraded checkpoints recover
requirements through exact recorded receipt links and reopen prior settlement
once. Properties cover policy combinations, migration idempotence, explicit
weaker-policy rejection and receipt-chain isolation. Real Git fixtures cover
staged adopted rebases, restart, restored ancestry, late legacy publication
checks and failed ancestry probes. The broader fallback and lifecycle audits
below remain required.

The duplicate writable anchor history, its setter APIs and the `Anchor` /
`Anchor_history` modules are removed. Start uses owned provisioning directly,
and pruning reads the owner's revision inventory exclusively. Snapshot migration
retains valid old anchor SHAs in the owner without granting replay/publication
authority or changing command identity. Generated properties cover totality,
commutative/idempotent import, retention beyond the old cap, malformed entries,
owner-transition interleavings and serialization. Persistence covers snapshots
with and without an owner checkpoint; real Git tests cover retained starting
boundaries after base movement and migrated revisions preventing reclamation.
The independently writable conflict marker still needs removal; the base-name
marker has since been replaced with an owner projection (details below).

Removing the old anchor tests exposed incorrectly assigned architecture metadata:
`Types` claimed the anchor domain and relied on an unrelated state test. Its
domain now matches its interface; the synthetic comment-ID restore decision is
extracted into total `Comment_id_allocation` logic. Generated pure properties
and real allocation/restore sequences cover that state without an exemption.
The other tests previously assigned to the retired anchor domain now name the
modules they exercise.

The writable publication marker and its setter APIs are removed. Publication
expectations derive exclusively from the owner's pending identity; old snapshot
markers are ignored. Runtime restart preserves the owner wait and acknowledgement.
The startup fetch-ref shortcut is removed, so a fetch endpoint cannot establish
publication at a different push destination. Migrated pure and feature-root tests
require direct Git confirmation before accepting a differing forge head.

The separate rebase/conflict post-push policy APIs are removed. Generated
orchestrator tests now apply token-bound publication outcomes, retain the candidate
through infrastructure/lease retries, and reject stale push completions during
history recovery. Satisfied conflict pairs wait without spending repair budget;
a changed base invalidates that satisfaction. The synchronous controller tick now
accepts/resumes outbox messages instead of firing actions without message ownership.
The legacy rebase-result and conflict-result APIs and their unused telemetry
facade are now removed. Replacement properties require owner integration receipts,
confirmed publication, retained repair and session-budget isolation. The legacy
rebase-failure and conflict-no-op counters, mutators, poll resets and intervention
rules are removed. Their old snapshot values are ignored without changing owner
state or unrelated intervention; new snapshots omit the fields. The writable
base-revision scalar and setters are removed as well; integration tests inspect
the owner's captured target directly. Legacy scalar values cannot change restored
owner state, including pending work and permanent intervention. The unused
session/push combiner is also removed. Session-result properties cover
local-work classification, while timeout interleavings retain the owner candidate
and release the worker without spending a legacy push budget. The obsolete
`Session_push_failed` and `Session_worktree_missing` outcomes are removed, along
with the push-failure counter, its mutators, restoration input, intervention rule,
and status projections. Old counter values are ignored in v1/v2 snapshots and
archived event projections; new snapshots omit the field. Generated publication
and provisioning interleavings retain work beyond the old retry cap without
consuming implementation budgets. Permanent publication denial is an owner
intervention and preserves the backend session; a successful implementation
cannot clear that intervention. Independently writable compatibility fields
still need cleanup.
The activity-log status projection now decodes the owner checkpoint and calls
the same intervention decision as the live agent. Generated owner histories
cover queued Human guidance, merged terminal state, retries, permanent refusal
and explicit resume. A telemetry regression verifies intervention/resume status
transitions even when archived payloads contain an exhausted obsolete push counter.
The owner now has a distinct `Provision_checkout` intent with a durable request
identity and a token-bound `Checkout_ready` result. Repeating one request is
idempotent; a later session's request rechecks a previously ready checkout. Its retry/intervention phases survive restart; successful
provisioning cannot authorize integration, publication or agent repair. The ordinary
Git executor refuses this intent without performing IO, reserving it for the checkout
handler. Generated histories cover retries, restart, stale/duplicate completions and
wrong result kinds; checkpoint validation rejects invented publication authority.
Those tests also exposed empty retry diagnostics producing unreadable checkpoints;
the owner now supplies a stable fallback reason. Production Start and session
setup now use this protocol under the existing patch ownership capability.
Scheduled retries use the same checkout executor. Probe and checkpoint failures
defer without changing implementation-session budgets; contradictory materialization
evidence enters owner intervention. A creation hook cannot run before its
materialization checkpoint succeeds. Completed sessions bypass a new provisioning
request and resume their captured publication, preserving its settled fixed point.
Real-Git coverage checks pre-command checkpoint failure, cancellation, persisted
backoff, restart, restored Human guidance, budget isolation, and a fresh inspection
without recreation or publication. Provisioning now adopts locally ahead or
diverged branches unchanged, including patch-equivalent rewritten histories;
only proven equal/remote-ahead histories authorize resetting to the remote.
Failed ancestry observations back off. Ref reads distinguish verified absence
from failed probes, symbolic refs, and unreadable/non-commit objects, so an
unavailable ref cannot authorize new-branch creation.
Initial publication with unincorporated remote work now attempts the existing
deterministic remote integration before agent recovery. Real Git tests exercise
adoption through verified publication for ahead, diverged and unrelated histories
under both policies; unrelated histories reach the actual recovery-session
adapter, and duplicate claims cannot invoke its backend twice. Generated owner
tests cover restart, stale completion and a failed preservation check after the
deterministic attempt, which must reach recovery rather than repeat integration.
Remote integration now uses an explicit durable attempt capture across publication,
scheduled/scoped base reconciliation, adopted conflict continuation and contributor
integration. Both policies retain remote receipts separately from base evidence;
restart and duplicate publication retries cannot reset the attempt or duplicate
receipts for the same pair/candidate. A generated lost-completion/retry case exposed
recovered evidence being relabelled as executed; retries now retain the original
receipt and its evidence. Old checkpoints reconstruct only recorded replay/receipt evidence.
Generated tests cover all intent families, invalid captures, legacy decoding and
unexpected inspection targets. The latter exposed a transition that could replace
pinned inputs; it now retains them for recovery, and a verification for another
target cannot publish. Real Git fixtures interrupt before/after remote mutation and
publication, preserve an adopted staged merge resolution, and exercise unrelated
histories through the actual recovery-session adapter. Diagnostics and prompts expose
the attempted pair and policy. The full hook interruption matrix, broader fallback
strategy audit and full stack matrix remain open. The preservation-policy audit
above covers adoption of an existing rebase under an ancestry-preserving request,
including older checkpoints.
Owned provisioning reports success/failure directly; Start stops on failure
before constructing an implementation session. The obsolete rebase result algebra, unused status classifier and unused
rebase/conflict/push telemetry facades are removed. Refused provisioning is tested
to leave owner provenance unchanged and avoid running a creation hook.

The unused `Rebase_decision` planner and combined legacy rebase/anchor-result
wrappers are removed. Replacement pure tests drive owner materialization and
integration receipts across checkpoint restoration, duplicate targets, no-op
completion, failed attempts, and older reachable boundaries. Scenario coverage
includes retained stack-start boundaries after dependency rewrites and rejection
of a newer target refresh while an integration is in flight. This is provenance
coverage; the full real-Git stack/tree acceptance matrix remains outstanding.

Fan-in, stale-base and worktree-start interleaving fixtures now use owner
integration and publication receipts for successful/no-op rebases. The stale-base
conflict fixture drives a retained repair, claim, completion, continuation and
publication instead of synthetic legacy outcomes. Its PR #3811 witness checks
that an idle worker with outstanding owner repair still blocks a dependent cut.
Generated controller interleavings, outbox readiness and feature-root lifecycle
fixtures now use the same owner transitions. Root tests distinguish transport
backoff from repair progress and require confirmed publication before completing
a contribution; CI observations use the confirmed revision. Generated conflict
completion can resume retained repair after capacity is released. Orchestrator
and controller fixtures now follow this protocol too; obsolete synthetic result
handlers no longer provide a second way to mutate reconciliation state.

The legacy top-level and functor `Worktree.rebase_onto` APIs, their separate
protected-root merge implementation, and unused conflict-state readers are now
removed. Existing rebase and protected-root Git fixtures drive checkpointed owner
commands, retaining history/tree, dirty-state, failed-probe, staged-repair and
restart assertions. The functor publication API and construction-time protected
branch switch are also removed; the owner's intent carries publication policy.
Publication fixtures for explicit destinations, stale tracking, missing tracking
refs, fresh-clone rewrites, conflict resolutions and concurrent writers now drive
the owner. This migration exposed and fixed blanket-fetch side effects during
publication and receive-side ref races being mistaken for permanent hook failures.
The standalone push API is also removed. Initial publication, timeout and
publication-lineage guard fixtures now use the owner. Confirmation and restart
inspection complete initial upstream configuration after an acknowledged or
unacknowledged first push; partial local setup resumes without repeating the
successful push. The standalone and functor descendant-integration APIs are
also removed. Their real-Git fixtures now drive the owner under the root lock,
covering sibling serialization, captured-revision identity, lost confirmation,
staged conflict repair, remote races, and cancellation/restart. Cancelling Git
now allows a bounded cleanup window before forced process-tree reaping; this
prevents the supervisor itself from stranding Git index locks during merge.
The supervisor now keeps the exited leader unreaped until descendant cleanup
finishes, avoiding group signals or probes after that identity can be reused.
Process tests cover graceful and forced cancellation, successful/failed leaders
with live descendants, and concurrent rapid exits. The native Linux branch was
also exercised in a container for rapid exits, orphan reaping and cancellation.
Publication observation now belongs to the owner: a durable operation/candidate
identity records acknowledgement before or after Git confirmation, survives
restart, and rejects delayed acknowledgements for older candidates. Live polling
reads that projection, requires direct Git confirmation for head reversions, and
suppresses stale CI/conflict work and readiness. Direct Git confirmations now
reject changed or unconfirmed push destinations before probing; only owned
resume can checkpoint a replacement endpoint. The obsolete `Push_publication`
wrapper is removed; its interleaving test drives owner commands instead. The
publication marker field and setters are removed; other writable legacy fields
and control paths still need a complete ownership audit.

The poller entrypoint now has 25 local acceptance scenarios using SourceHut's
real Git adapter and parsed GitHub payloads. They cover durable request dispatch,
stale and missing revision context, PR/request replacement, directly confirmed
head reversion, and native-stack absence thresholds. Contradictory positive
mergeability metadata cannot bypass unresolved conflict evidence at the approval
gate. Already-satisfied conflict pairs retain their settled operation, enqueue no
new conflict repair, and visibly wait for forge refresh. These fixtures do not
exercise GitHub HTTP transport; final aggregate qualification remains required.

`test_branch_reconcile_model.exe` interprets commands against independent
repository models for rewrite and ancestry-preserving publication. The linear
model covers interruption, lost acknowledgements, delayed/duplicate results,
transport failures, repeated requests and restart. Generated distinct requests
for the same source/base pair additionally verify unchanged active command
authority, latest-request settlement, and exactly one integration and publication
under both policies, including lost acknowledgements and restart. The remote-writer model tracks
commit ancestry and content separately, races external commits and
content-preserving remote history rewrites with publication,
and supplies successful history-recovery turns only after a durable claim. It
checks content preservation independently of leases, requires ancestry when the
execution policy promises it, and verifies convergence and
settlement after external writes stop. A generated retargeting extension switches
between two disjoint base revisions while remote writes, history rewrites and
interruption continue. It checks that queued requests preserve active command
authority, the latest requested target settles, prior integrated content remains,
and ancestry is retained when required. Hostile recovery fixtures replace the
agent's candidate with an empty tree while reporting a completed turn. Independent
preservation checks prevent publication, two invalid completed turns exhaust the
budget, and ordinary ticks, duplicate results and restart cannot resume it.
Generated histories mix those outcomes with remote races and retargeting; an
explicitly authorized healthy suffix recovers retained work and settles.
Destructive remote rewrites additionally erase content already incorporated
locally, without assuming recovery of never-observed commits. Generated histories
retain that captured content and required ancestry through later races. A delayed
confirmation/force-push counterexample clarified the model's fairness assumption:
a healthy suffix must provide a fresh remote observation after external writes
stop, since an earlier successful result cannot establish the present remote.
The exact sequence remains a regression under both policies.
A shrunk recovery/remote-write race exposed
successful repairs being charged as no-progress turns; the owner now retains the
captured remote obligation and captures new remote work for deterministic
integration after proved recovery. Real Git fixtures exercise new remote work
arriving between successful recovery and verification under both policies.
Conflicting sequencers, competing intents with different purposes,
additional hostile agent mutations and the full stack matrix still need model coverage. Those extensions, the complete
fallback audit, remaining legacy-state removal, the complete pruning interruption audit,
formal endpoint specifications and final requirement-by-requirement acceptance
remain M3 work.

Recovery-ref reclamation now has an owner-provided, sorted revision inventory
covering materialization, queued intent, active commands, replay and repair
state, integration/publication receipts, and acknowledged publication. Generated
interleavings compare that inventory with serialized checkpoint revisions and
check restart equivalence, including both supported object hash widths. This is
the dependency input for pruning, not a reachability proof. Project pruning now
locks surviving projects sharing the canonical common Git directory, probes
which recovery refs retain each required revision, and compare-and-deletes only
the completed project's captured direct commit refs. Compatibility anchors remain
protected during migration. Missing/failed probes, symbolic or non-commit refs,
changed tips, and late refs cannot authorize directory removal. Managed clones
remain while another stored project uses them; merged/closed patches with
unfinished reconciliation also remain. `test_recovery_ref_prune.exe` covers
dependent retention and release through the actual command, independent anchors,
shared clones, prefix isolation, failed probes, changed tips, partial deletion
restart and late writes. Startup and pruning now share a registration lease
outside project directories. Startup claims the project before managed checkout
preparation, snapshot/configuration reads and writes; it retains an exclusive
lifetime mutation lease even under `--no-lock`. Pruning needs exclusive lifetime leases
for the target and shared-repository evidence owners. Completed project directories
move atomically into versioned retirement journals before cleanup. Restart cleanup
traverses only journal payloads, never a fresh project at the original pathname.
`test_project_lifecycle.exe` covers process contention, inherited/repeated release,
real CLI exclusion before configuration access, worker death, death after retirement
rename, same-name recreation, symlink containment and unknown-journal preservation.
The command-level pruning fixture also covers live targets and live shared peers
without exclusive supervisor locks.
`test_project_lifecycle_state_machine.exe` additionally generates mixed shared/
exclusive acquisition, release, duplicate release and stale-handle histories,
then checks the resulting kernel locks from an independent process.
Startup now resolves case, spacing and punctuation aliases to the exact stored
project name before checkout preparation and config writes. The store rejects
misfiled configurations whose names select a different storage directory. Pure
properties exercise alias histories and recovery-namespace stability; the real
CLI fixture resumes through aliases, verifies persisted identity and existing Git
recovery refs, and rejects a copied configuration. This prevents new namespace
drift; it does not recover historical names already overwritten by older versions.
The final A22 audit still needs to cover
historical identity/namespace migrations and all interruption boundaries together
with the remaining M3 lifecycle matrix.

Initial base reconciliation now attempts explicitly labelled patch-equivalence
recovery for linear histories lacking a reachable recorded boundary. The Git
fixture `test_branch_replay_evidence.exe` verifies precedence, failed-probe
classification, serialized strategy evidence and the resulting published tree.
Scoped scheduled requests additionally retain the project/dependency set and
can select a best-effort dependency-subject prefix after patch equivalence is
unavailable. The fixture covers changed patch IDs after squash merging and empty
replays for both strategies. Exact-tree reconstruction now precedes these heuristics when a recorded boundary
is unreachable: first-parent topology is verified and both boundary revisions
are pinned. A real unrelated-history merge refusal also reaches claimed agent
recovery and publishes only after independent ancestry verification. These
fixtures cover the local chain; the complete A19 scenario audit, including
remote replay and unsupported topology combinations, remains outstanding.

The replay-evidence suite also composes a four-patch stack across three dependency
merges and mainline advancements. Two-commit patches are squash-merged or
cherry-picked, with Git confirming changed or equivalent patch IDs respectively.
Each round reconciles every remaining descendant and eventually retargets it to
main. Exact cumulative file contents and commit counts prove dependency work is
not duplicated; an empty-tip variant publishes no local commits. Owner state is
serialized after every command result and repair claim. Both stack variants force
two consecutive staged content repairs in a dependency patch; the simulated agent
never commits, and later transformations retain both resolutions and the upstream
edit. Exactly two repair turns execute. The final aggregate A20 audit remains
outstanding; these fixtures exercise owner/executor composition directly.

Publication state is now derived exclusively from owner receipts. The writable
`branch_published` field and its setters are removed. Scheduling and live display
combine confirmed publication with execution mode: mainline and feature-root
patches still require PR creation. Only first confirmation clears bootstrap
failures; subsequent receipts cannot erase failed PR-creation attempts.

Old publication claims import as durable, verification-only owner work. They
preserve the active command and desired intent; existing confirmed receipts
satisfy the claim, otherwise verification follows pending work. Verification
pins captured evidence and confirms the clean named checkout against the remote.
It cannot push, integrate, repair content, provision or clean up a checkout, or
run creation hooks. The checkout wrapper uses read-only ownership validation and
can recover a stale stored path from the linked-checkout registry. Missing or
unconfirmed branches remain specific interventions instead of authorizing new
implementation. A verified legacy mainline branch without a saved backend
completion resumes PR creation directly, unless new guidance requires a session.
New snapshots omit the old field. Pure and real-Git fixtures
cover restart, hostile completion results, missing/different remotes, dirty and
changed checkouts, replay-boundary isolation, and checkout-wrapper behavior.

The writable `branch_rebased_onto` field, setter, restore argument, and snapshot
output are removed. The accessor derives the most recent base integration from
owner receipts, skipping contributor receipts. Start and retargeting preserve
historical evidence; old base-name claims cannot fabricate it. Published branches
without base evidence enqueue reconciliation. This base context is historical,
not proof of arbitrary later head ancestry; the A18 freshness audit remains open.

Forge reads now allocate owner-held request tickets in a single checkpoint before
network dispatch. Tickets bind PR, branch, generation, head and base-name context;
accepted facts retain the returned head/base revisions, mergeability and direct
head confirmation. Apply-time checks reject replaced tickets and changed context.
Ticket bookkeeping preserves implementation-message generations and Git command
identity. Unknown/mergeable reports and infrastructure failures retain unresolved
conflict evidence for later exact Git verification. Checked history decoding and
legacy defaults preserve restart compatibility. Pure interleaving tests and a real
snapshot fixture cover duplicate/late responses, PR replacement, interruption,
failed saves, sequence bounds and restart.

The writable conflict field, setters, restore argument and snapshot output are
removed. The owner projects unresolved local repairs and current unsatisfied
forge conflicts; contradictory same-pair mergeability and successful agent
responses cannot clear them. A changed revision pair supersedes an old report,
while verified integration satisfies the exact pair. Pending publications apply
the same head-identity gate to conflict projection as poll ingestion. Legacy
conflict flags invalidate readiness without manufacturing owner evidence. Focused
owner, persistence, outbox, controller and real-Git publication tests cover this
cutover.

Open-PR application now requires matching direct head and base observations in the
accepted owner fact. Head confirmation uses the bound publication destination;
base confirmation uses origin's fetch destination, including configurations with a
different push URL. These probes do not modify tracking refs. Unconfirmed pairs
suspend readiness and queued CI/conflict work while retaining owner operations and
conflict evidence. Old checkpoints cannot manufacture base proof. Focused pure
restart properties and a real Git fixture cover this distinction; the complete
provider-timing and A18 acceptance matrix remains open.

PR-based merge approval, review requests, draft promotion, root promotion and
open-dependency readiness now require a confirmed owner pair matching the current
PR, branch, head and base. A missing PR, replacement PR, changed head or retargeted
base invalidates that readiness; unrelated message-generation changes do not.
Saved readiness/CI flags cannot substitute for proof after restart. Branch-only
feature descendants retain their separate publication-confirmation path, and a
dependency awaiting publication acknowledgement is not ready. Pure context-mutation
and persistence properties, controller tests and inline tests exercise these gates.

Request admission and checkpoint decoding now share intent validation. Empty
reconciliation bases or required request/session/contributor identities cannot
replace active intent or create unrestorable owner state. A generated property
covers all eight purposes against empty and active owners, including restart
after the first reconciliation observation. The broader A23 model remains open.
Provisioning rejects empty bases before requesting owner work, preventing its
advance loop from repeatedly submitting an inadmissible request. A generated
property requires every provisioning request to change owner state and emit work.

Legacy scalar anchor SHAs now enter the same retention inventory as anchor-history
entries. They survive restart and protect required recovery refs without granting
replay or publication authority. Scalar/history imports deduplicate; malformed
values are ignored. Pure and persistence properties cover both representations,
and the real pruning fixture exercises a scalar-only checkpoint.

Supervisor startup now requires an exclusive OS-held project lifetime lease,
including with `--no-lock` or `ONTON_NO_LOCK=1`. Those switches bypass only the
legacy PID-file lock. Competing readers, supervisors and retirement cannot enter
while the writer is alive; process exit releases authority without stale-PID
inference. Real CLI tests cover both bypass forms before config reads or checkout
creation, and fork/crash plus generated lease histories exercise kernel ownership.
The broader A24 competing-trigger matrix remains open.

Creation hooks now have durable plans, attempt identities and acknowledged outcomes
inside the reconciliation checkpoint. Setup records the selected executable path
before creation, claims an attempt after materialization and capacity acquisition,
and records completion before readiness. Unstarted hooks resume after checkout
creation is interrupted; uncertain executions require explicit resume. Completed
hooks do not repeat after restart, and stale attempt results cannot settle a retry.
Canonical checkout identity handles filesystem aliases without weakening branch
binding. Pure generated histories and real Git restart fixtures cover these paths;
checkpoint-failure and capacity/terminal-state interleavings remain open.

Hook execution now uses the shared process-tree supervisor. Real PID assertions
verify that descendants are reaped before return after success, failure, timeout
and cancellation, including children that ignore termination signals. Timeout
output remains available, external cancellation propagates, relative supervisor
paths survive checkout working-directory changes, and a missing supervisor cannot
launch a hook. Existing process-runner, spawn-retry and hook restart fixtures also
exercise this integration.

Hook plans and attempt claims now recheck terminal patch state atomically with
their checkpoint. Real Git fixtures hold hook capacity, mark the patch merged or
WONTDO, then release capacity: neither path launches a script or consumes the
unstarted attempt. The broader competing-trigger and checkpoint-failure matrices
remain open.

Git and hook supervisors now bind dispatch to the spawning process ID, refuse a
stale parent before execution, and tear down their command group after parent
disappearance. A real crash fixture kills the caller with SIGKILL and verifies
that the supervisor, command and termination-resistant descendant all disappear.
Streaming agent commands now use the same parent-bound supervisor, with separate
grace periods for terminal-event shutdown and persistence helpers. The streaming
crash fixture reproduced surviving commands before this change and now verifies
their disappearance after SIGKILL of the caller. Cancellation fixtures assert
that both graceful and termination-resistant descendants are reaped before
return; backend smoke and idle-timeout tests cover persistence flushing, terminal
events, blocked callbacks, nested failures and output activity. Production startup
requires an available supervisor. Group signaling stays inside the supervisor,
which retains the unreaped command leader until descendant cleanup finishes.
Project leases now contribute shared command locks held by the supervisor until
its command group has been reaped. Writer, reader and retirement admission check
those locks exclusively after acquiring the primary lifetime lock. A helper
rechecks its parent's lifetime after acquiring its command locks, so a delayed
helper cannot launch for a dead runtime after replacement admission. Crash tests
pause cleanup before killing the caller: before this change a new writer entered
while the old command was alive; now both protected projects refuse admission,
an unrelated project remains available, and admission succeeds after cleanup.
Generated lifecycle histories verify the guard inventory across acquisition,
release, stale handles and forked processes.

Worktree-backend provisioning, worktree Git helpers (including fetch and
gameplan commits), and SourceHut now use the shared runner. Independent crash
fixtures for provisioning, fetch and SourceHut each reproduced replacement
admission while the old command was alive; all now retain both project guards
through cleanup and allow replacement afterward. SourceHut retains native
exit/signal status and timeout classification. Default streaming discovers the
supervisor instead of bypassing it, and an empty override cannot dispatch a
command. The unused non-streaming Claude launcher has been removed. Native
status, backend smoke, idle-timeout, worktree lifecycle, start-point and gameplan
publication tests exercise this cutover.

Synchronous startup capture now uses the same supervisor and command guards.
Repository discovery, startup fetch, clone, remote inspection and credential
lookup share this capture path; the independent Unix Git launchers are removed.
Clone, startup-fetch and discovery crash fixtures each reproduced replacement
admission while an old command remained live and now pass the two-project guard
and cleanup assertions. Native capture tests run without an Eio scheduler and
cover stdin EOF, native signal status, missing-supervisor refusal and output
larger than either pipe's capacity. Repository-root and real CLI tests cover
normal startup behavior. This closes the identified subprocess handoff paths;
the broader checkpoint, competing-trigger and content-preservation matrices
remain part of the terminal acceptance audit below.

Provisioning now performs a read-only checkout inspection after executing a
creation hook, before acknowledging checkout readiness. Real Git fixtures first
reproduced false readiness after a hook switched branches, and now also cover a
hook moving the checkout. Hook completion remains durably acknowledged so an
inspection failure does not repeat execution; missing paths stop, while failed
inspection probes remain retryable. Real filesystem failures at the hook claim
and completion checkpoints now exercise the owned provisioning entrypoint:
failed claims cannot execute the hook; failed completion acknowledgments retain
an unknown outcome across restart and require explicit resume. The fixtures
check the durable attempt state, exact checkout writes and eventual readiness,
then restart again to prove acknowledged hooks do not repeat. Together with the
plan-save and cancellation fixtures this covers these hook boundaries. The
interrupted-execution fixture also changes the stored checkout path before
explicit resume: the captured hook cannot execute in the new context or create
that checkout, and restoring its original path allows recovery to settle. Pure
properties additionally reject changed branch context. The full A21 mutation
matrix remains under audit.

## Key Challenges

- Git and snapshots are separate systems. Restart must distinguish commands that
  did not happen, completed, or remain uncertain without repeating unsafe work.
- A valid lease does not prove preservation. Recovery must retain both patch work
  and newly observed remote/local work before granting publication authority.
- Legacy scheduling and mutable views still surround the new owner. A transitional
  release needs one mutation owner per path, even while old detectors remain.
- Forge mergeability may be delayed or contradictory. Adding identifiers alone
  does not prove a cached report is fresh.
- A broad green test suite is weaker than a complete requirement-to-scenario map.
  Missing safety evidence on an enabled path is an M2 blocker; additional automation
  coverage may remain in M3 if M2 stops safely with recoverable work.

## Established Precedents

- **documentation — Git explicit force-with-lease contract** —
  https://git-scm.com/docs/git-push
  Use the explicit expected ref value, including the absent-ref expectation.
  Preserve and independently verify remote work before issuing the push; the
  lease only detects a concurrent ref change. Applies to M1–M3 publication.
- **library — QCheck2** — https://github.com/c-cube/qcheck
  Use generated scenarios and shrinking for pure protocol laws and the independent
  model. Existing property suites are reused; M3 supplies the missing independent
  behavioral oracle rather than duplicating the transition implementation.

The repository's existing `lib_core` / `lib` split, Eio resource ownership, and
`Runtime.update_persisting` are established local foundations to reuse, not new
primitives to design in later milestones.

## Milestones

### Milestone 1: branch-reconciliation-foundation

**Scope: the work done so far.** Retrospective implementation inventory plus the
minimum stabilization needed to leave a consistent checkout. Status: complete;
the subsequent M2 qualification is recorded separately.

**Definition of Done**:

- Preserve and document the implemented owner, executor, unified observations,
  materialization provenance, recovery refs, checkpoint protocol, v2 storage,
  session receipts, explicit leases, remote race recovery, destination-root
  integration, and both repair modes listed in Current State.
- Resolve or coherently withdraw the unfinished forge-context edit without losing
  the implemented base-SHA evidence. Do not broaden M1 into the remaining cutover.
- Establish a reviewable green baseline with `opam exec -- dune build`,
  `dune runtest`, `dune build @fmt`, `scripts/check-no-raw-yojson.sh`,
  `git diff --check`, `dune build @check`, and the repository's pinned architecture
  check, all with recorded exit codes. Preserve their evidence in the milestone
  review record, rather than relying on temporary logs.
- Explicitly record incomplete requirements and test coverage. Existing scheduled
  rebase/conflict/publication/integration paths remain wired coherently; no claim
  of production qualification or full design completion is made.

**Why this is a safe pause point**:

A buildable, tested development baseline can be reviewed and retained without
shipping it. This statement becomes true only after stabilization; the current
compile failure is not a safe pause point disguised as completed work.

**Unlocks**: A bounded release-hardening gameplan that reuses the foundation.

**Operator Actions Before Next Milestone**:

Review the baseline diff and evidence, select its immutable revision, and accept
M2's release scope. Do not deploy M1 as a production release. Any discovered
preservation or restart defect becomes an explicit M2 item.

### Milestone 2: branch-reconciliation-useful-release

**Scope: the work necessary to ship the reliability gains.** Keep the current
product modes and forge support; do not silently drop stacked PRs, feature-root
integration, or SourceHut to make the gate easier. No rollout flag or alternative
runtime owner is required by this workstream. Status: release candidate qualified;
review, publication and representative operator trials remain the handoff below.

**Definition of Done**:

1. **Single mutation authority on enabled paths.** Audit Start/session publication,
   scheduled rebase, conflict response, remote race recovery, agent fallback, and
   root integration. Remove or disconnect competing writers on these paths.
   Existing scheduling detectors and compatibility views may remain if they cannot
   authorize conflicting mutations or overwrite durable progress.
2. **Work preservation and restart safety.** Close defects in dirty/staged/untracked
   handling, unexpected agent/external Git mutations, provisioning interruption,
   checkpoint failure, cancellation, and missing checkout handling that affect
   enabled paths. Pin newly discovered recovery work before offering an agent turn.
   An unsupported topology must retain evidence and stop specifically; it must not
   silently lose commits or blindly refresh a lease.
3. **Useful automatic recovery.** A failed push resumes the captured candidate
   without rerunning successful implementation. Transient failures back off and
   release capacity. Staged repair continues against the original target. Exhausted
   available deterministic strategies enter the existing bounded agent recovery
   protocol; Onton verifies preservation and performs safe publication. Permission
   failures and infrastructure outages do not spend content-repair budget.
4. **Safe transitional scheduling and forge handling.** Prevent repeated stale
   conflict reports from repeatedly rewriting a satisfied branch or exhausting
   repair counters. Confirm external head changes using direct Git evidence.
   Complete only the forge-context work necessary for this guarantee; the fully
   unified observation algebra belongs to M3. Fix WONTDO/merged/cancellation and
   legacy-counter interactions where they could launch unsafe or duplicate work.
5. **Upgrade and operations.** Exercise v1 migration with real sessions, anchors,
   worktrees, and unrelated intervention reasons. Do not trust old pending-push
   markers or exhausted branch counters as current facts. Preserve the pre-upgrade
   snapshot; verify older binaries reject v2. Document recovery and downgrade,
   durable pending outbox behavior, and actionable phase/retry/intervention details.
   Retain recovery refs conservatively until M3 adds dependency-aware reclamation.
6. **Release evidence.** Run safety acceptance for every enabled entry point and
   supported mode/provider, including loss of acknowledgement and interruption at
   relevant mutation/checkpoint boundaries. Include PR #482 lineage behavior,
   PR #483 direct-remote confirmation, stacks/squash merges, and concurrent root
   contributions. Preserve unrelated review, CI, WONTDO, backend-session, native
   GitHub stack restrictions, and cloud-facing agent/outbox behavior. Run the full
   source gate and attach a requirement-to-evidence map to the release candidate.

**May remain for M3**: broader deterministic heuristic coverage; complete removal
of inert legacy APIs/fields; unified revision-scoped forge model beyond the M2
safety requirements; independent abstract-model checking; complete exhaustive
scenario matrix; dependency-aware ref pruning; full formal specifications and
richer diagnostics. No known safety defect on an enabled path is deferrable.

**Why this is a safe pause point**:

A useful release can run indefinitely. It preserves work and resumes durable
operations; unfamiliar cases produce an actionable intervention rather than
requiring future implementation to make the current execution safe.

**Unlocks**: Real use of durable publication and repair, with operational evidence
for completing the model. Users need not wait for the entire architecture cleanup.

**Operator Actions Before Next Milestone**:

- Review and publish the qualified release candidate through the normal release
  process; publication is a human handoff, not an inter-patch gameplan step.
- Select backed-up representative projects for stacked PRs, feature-root mode,
  and SourceHut. Run each through a complete implementation/publication cycle and
  at least one controlled restart. Observe until each settles or reaches a stable,
  explained intervention; no arbitrary wall-clock soak duration substitutes for
  those outcomes.
- Inspect remote refs/trees, pending outbox work, repair counts, and recovery refs.
  Halt further rollout on lost work, unsafe overwrite, duplicate implementation,
  duplicate repair, or premature cloud quiescence. Retain snapshots and Git refs;
  do not open live v2 state with an older binary.
- Record release revision and outcomes. Resolve safety regressions before broader
  rollout; use the evidence to refine the M3 gameplan without shrinking its endpoint.

### Milestone 3: branch-reconciliation-complete

**Status: implemented and locally verified.** A01–A24 and the final validation
gates pass; see the [completion audit](branch-reconciliation-m3-audit.md).
Merge, release publication and live-provider rollout remain separate handoffs.

**Scope: the complete picture.** Finish the original design after the useful
release; this milestone is not a second competing runtime implementation.

**Definition of Done**:

- Complete the fallback chain: recorded provenance, verified topology and
  reconstruction, patch-equivalence, subject-based recovery, and plain-rebase
  fallback where applicable. Record selected strategy and inputs; inferred patch
  ownership stays explicitly best effort. Probe failures remain distinct from
  negative evidence. Agent recovery remains the final bounded fallback.
- Replace independently writable branch/conflict/publication state with accessors
  and derived views of the owner. Remove obsolete execution APIs and redundant
  control paths. Scheduling submits reconciliation intent/evidence; the core
  decides rebase, merge, repair, continuation, publication, or no action.
- Carry PR identity, head SHA, base branch, base SHA, and request identity through
  forge/poller boundaries. Handle duplicate, delayed, contradictory, missing, and
  stale observations; already satisfied conflict pairs visibly wait. Direct remote
  confirmation can establish a head reversion. GitHub and SourceHut obey equivalent
  revision/outcome contracts, with native-stack restrictions preserved.
- Finish provisioning recovery, cancellation/restart semantics, revision-scoped
  descendant integration, legacy migration cleanup, and project/dependent-aware
  recovery-ref lifetime. Finish durable repair identity/telemetry and detail views.
- Record architecture laws and formal gameplan specifications. Build an independent
  abstract model with generated sequences and shrinking, covering totality,
  idempotence, stale-result isolation, compatible-observation order independence,
  fixed points, serialization/restart equivalence, exclusive authority, budget
  isolation, and eventual convergence under the stated recovery assumptions.
- Complete the original Git/forge acceptance matrix: deep stacks and retargeting;
  squash/rebase merges; dirty and staged states; multiple conflicting steps and
  empty outcomes; root/sibling races; changed patch IDs; fresh clones and missing
  tracking refs; background fetch and force-push; lost push bookkeeping; termination
  around every mutation/checkpoint boundary; legacy snapshots; and unexpected
  agent mutations. Verify outcomes, trees, refs, index, and persisted state.
- Audit every original requirement against authoritative evidence and pass final
  build/test/format/source/architecture gates. No feature flag, second runtime
  implementation, or unowned mutable branch state remains at this endpoint.

**Why this is a safe pause point**: The complete design is implemented, verified,
operable, and documented; no later milestone is needed to satisfy its promises.

**Unlocks**: One extensible reconciliation model instead of accumulating fixes
for separate Git-status shapes.

## Dependency Graph

```text
1 (branch-reconciliation-foundation) → []
2 (branch-reconciliation-useful-release) → [1]
3 (branch-reconciliation-complete) → [2]
```

The baseline review and release/operational observation are explicit milestone
boundaries. Patches inside each future gameplan execute atomically without hidden
manual pauses. M3 details may be refined from M2 evidence; its terminal scope stays
intact.

## Open Questions

| Question | Resolve by |
|---|---|
| Which projects and operator own the representative release trials? | M2 release handoff; this is an operational choice, not inferable from source. |
| Which release version/channel and rollout cohort should receive M2 first? | M2 release handoff through the normal release process. |
| Which unfamiliar topologies should safely intervene in M2 rather than gain additional automatic recovery before release? | M2 gameplan, using the call-path and acceptance audit; preservation is non-negotiable. |

## Decisions Made

- Keep the user's three milestones. M1 is retrospective and requires minimal
  stabilization, M2 is the useful release, M3 is full completion.
- The delivery sequence replaces one atomic release with a qualified intermediate
  release. The full architecture goal is not marked complete at M2.
- Reuse the owner, executor, snapshot protocol, repair handler, and lineage proof
  already implemented. Do not plan to rebuild them.
- M2 may retain legacy demand detectors and compatibility views, but every enabled
  mutation path has a single owner. Safety boundaries cannot be postponed as cleanup.
- Agent-driven recovery belongs in the useful release and the final endpoint.
  Recovery agents may repair history; Onton retains verification and publication
  authority. A success report or new lease alone never proves preservation.
- This is a planning record, not authorization to publish a release. Existing
  source changes are preserved; this planning task does not repair the build.

## Definition of Done (Acceptance Suite)

This is the terminal suite, not a claim that all assertions currently pass.
Each scenario starts from an independent fixture. M1 ownership identifies existing
behavior; M2 owns stronger release qualification; M3 owns the completed model.
Verification must inspect runtime results and persisted/Git state rather than
implementation text. Extend the named tests where coverage is missing. Proposed
scenario labels below are requirements, not claims that named tests already exist.

| ID / Assert | Verify by | Expected | Traces to |
|---|---|---|---|
| A01 — Duplicate command results cannot dispatch twice. | `cmd`: generated duplicate/stale result sequences in `test_branch_reconcile_properties.exe`; inspect emitted commands and tokens. | One dispatch per authorized token; predecessor results do not advance successor. | M1 — `lib_core/branch_reconcile.ml` |
| A02 — Failed checkpoint prevents the next mutation. | `cmd`: inject snapshot failures before commands/results in `test_branch_reconcile_git.exe`; count Git mutations. | No successor mutation after failed persistence; captured work remains reachable. | M1 — `lib/branch_reconcile_runner.ml` |
| A03 — Initial boundary survives base movement. | `cmd`: create checkout, move/fetch base before agent work, inspect materialization snapshot and private refs. | Recorded boundary is the actual starting commit. | M1 — `lib/worktree_setup.ml`, `lib_core/branch_reconcile.ml` |
| A04 — Lost push acknowledgement does not rerun implementation. | `cmd`: accept push remotely, lose result, restart runner; inspect backend invocations and remote ref. | One implementation session; direct remote confirmation completes publication. | M1 — `lib/session_driver.ml`, `lib/branch_reconcile_executor.ml` |
| A05 — Staged resolution needs no agent commit. | `cmd`: stage a conflicted replay resolution, restart and continue; inspect index and resulting tree. | Resolution survives; Onton continues against the captured target. | M1 — `lib/branch_repair_session.ml`, executor |
| A06 — Remote races retain both sides. | `cmd`: advance remote between observation and push for rewrite and preserve-ancestry policies. | Both captured patch work and incoming work survive; refresh alone never overwrites remote work. | M1 — executor, `lib/git_publication_evidence.ml` |
| A07 — Root publication controls descendant completion. | `cmd`: race sibling contributions and lose push acknowledgement in feature-mode fixtures; inspect remote ancestry and snapshots. | Child completes only after confirmed publication containing its captured revision. | M1 — `lib/runner_fiber_impl.ml` |
| A08 — Infrastructure waits do not spend repair budget. | `cmd`: inject fetch/probe/ref-lock failures and a sequencer stop without content conflicts; advance supplied clock. | Backoff starts at 5 seconds, caps at 300 unless provider delay is longer; no repair turn charged. | M1 — `lib_core/branch_reconcile.ml` |
| A09 — Empty recreated merges retain ancestry. | `cmd`: resolve a recreated merge to its first-parent tree, terminate around merge completion, resume. | Required merge parents remain; no duplicate completion. | M1 — executor merge-completion protocol |
| A10 — Every enabled path preserves unexpected work. | `cmd`: independently introduce staged, unstaged, untracked, and externally committed work before mutation/repair on each entry point. | Work remains recoverable in checkout or retained refs; uncertain preservation prevents publication. | M2 — executor, `lib/worktree_setup.ml` |
| A11 — Agent fallback is bounded and verified. | `cmd`: exhaust deterministic recovery; supply valid and invalid recovery-agent outcomes, duplicate completion, and restart. | Valid candidate safely publishes; unverifiable preservation stops specifically; two no-progress completed turns exhaust budget without duplicate dispatch. | M2 — `lib/branch_repair_session.ml`, core recovery protocol |
| A12 — Legacy upgrade preserves recoverable state. | `db` + `cmd`: load v1 fixtures containing sessions, worktrees, anchors, branch counters, and unrelated interventions; restart, inspect backup/v2 JSON and refs. | Original bytes backed up; unrelated state preserved; branch facts reobserved; old binary rejects v2. | M2 — `lib/persistence.ml` |
| A13 — Waiting remains visible without monopolizing capacity. | `db` + `cmd`: hold publication in retry while another patch is runnable; inspect outbox and worker/lock activity. | Retry remains Pending; other patch progresses; cloud does not report quiescence. | M2 — runner, `lib/spawn_logic.ml`, `lib/orchestrator.ml` |
| A14 — Cancellation/WONTDO does not authorize unintended work. | `cmd`: cancel or mark WONTDO around queued/running publication and repair; restart and inspect commands/session outcomes. | No new unintended implementation/repair; retained work is observed before any authorized recovery mutation. | M2 — `lib/runner_fiber_impl.ml`, scheduler |
| A15 — Repeated satisfied conflict reports do not create rewrite loops. | `cmd`: replay the same conflict report after successful local verification/publication; inspect mutation and repair counts. | No repeated rewrite or repair-budget spending for unchanged satisfied facts. | M2 — `lib/patch_controller.ml`, reconciliation owner |
| A16 — Existing product modes retain behavior. | `cmd`: run independent stacked-PR, feature-root, SourceHut, native-stack restriction, review/CI/WONTDO and backend-session acceptance fixtures. | Existing supported outcomes remain; restricted operations remain rejected; cloud field/outbox contracts hold. | M2 — runtime/forge integration |
| A17 — Operators can diagnose retained work. | `ux` + `db`: induce transient and permanent failures, open detail view and inspect snapshot/event. | Phase, operation ID, relevant revisions, evidence source and specific reason agree; pending retry differs from intervention. | M2 — `lib/event_log.ml`, persistence and detail views |
| A18 — Forge observations remain bound to context. | `cmd`: inject delayed, duplicate, contradictory and unidentified PR/head/base/request observations, plus directly confirmed head reversion. | Stale context cannot authorize mutation; satisfied pairs wait visibly; confirmed external reversion is accepted as new evidence. | M3 — `lib_core/poller.ml`, `lib/poller_fiber.ml`, forge interfaces |
| A19 — Fallback provenance is truthful. | `cmd`: fixtures independently requiring recorded, reconstructed, patch-equivalent, subject and plain strategies, plus failed probes; inspect snapshot strategy/evidence and resulting trees. | Recorded evidence takes precedence; inferred ownership is labelled; failed probes retry; exhausted strategies reach agent recovery. | M3 — core and executor replay planning |
| A20 — Full stack transformations preserve patch work. | `cmd`: deep stacks with squash/rebase merges, retargeting, changed patch IDs, multiple repair steps, and empty outcomes. | Expected patch trees survive without duplicated dependency work; publication settles or yields a specific recoverable intervention. | M3 — core/executor, `lib_core/rewrite_lineage.ml` |
| A21 — Interruption decisions are restart-equivalent. | `cmd`: terminate before/after every mutation/checkpoint boundary, including provisioning and agent turns; compare resumed public outcomes with uninterrupted fixtures. | No lost work, duplicate repair, repeated implementation, or unauthorized push. | M3 — runner, persistence, provisioning |
| A22 — Recovery refs respect dependent lifetimes. | `cmd`: prune a completed project with and without dependents retaining old boundaries; inspect `git for-each-ref refs/onton/reconcile/`. | Required boundaries remain; eligible project refs are eventually reclaimed without touching unrelated refs. | M3 — project pruning and worktree ownership integration |
| A23 — The model converges and settles. | `cmd`: run independent-model generated sequences with shrinking, then stop external writes, restore services and provide successful repairs. | Runtime decisions match the model; branch settles, then emits no new mutations until intent/relevant evidence changes. | M3 — independent model suite and `lib_core/branch_reconcile.ml` |
| A24 — Exactly one mutation owner acts across competing triggers. | `cmd`: interleave polling, scheduled rebase, session completion, cancellation, root contributions and retries; inspect captured command trace and refs. | One revision-bound authority at a time; compatible observation ordering leaves equivalent decisions. | M3 — core, runtime ownership and unified scheduling |
