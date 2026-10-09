# Branch reconciliation M3 completion audit

The corrected M3 recovery boundary is locally qualified. Ordinary reconciliation
failures reach agent recovery or observation-only diagnosis before an exhausted
recovery hold. The current evidence is recorded in the M3 audit; earlier
qualification did not cover this complete boundary. No PR has been opened.

The earlier recovery correction was qualified only for its covered cases. Dirty interrupted
work and repeated validation timeouts exposed missing obligations in the prior
model; the corrected planner and current terminal-route audit are recorded below.
The historical assessments are retained for provenance and are superseded where
those oracles conflict with the current recovery contract. No PR has been opened;
merge, release publication and live-provider trials remain separate handoffs.

## Historical qualification before reopening

The requirement assessments below cover the preserved M1/M2 contract and all
M3-specific acceptance items. Formal contracts and checklist review are recorded
near the end. Two workflow checklist exceptions remain intentional: the atomic
behavior cutover comes first and includes its regression tests.

| Gate | Result |
|---|---|
| `opam exec -- dune build` | Pass |
| `opam exec -- dune runtest` | Pass, including current outbox and migration tests |
| `opam exec -- dune build @fmt @check` | Pass |
| `bash scripts/check-no-raw-yojson.sh` | Pass |
| `git diff --check` | Pass |
| Pinned archlint OCaml adapter | Pass |
| Gameplan schema, Pant, reference, dependency and frame validation | Pass |
| Isolated atomic-cutover prefix build | Pass; only the independent-model stanza deferred |
| Isolated final-prefix build | Pass with complete test wiring |
| Forced final-prefix model/outbox/persistence runs | 27 / 10 / 30 properties pass |

Final-prefix seeds were `516245780` (model), `411366070` (outbox), and `227408857`
(persistence). The fresh projection included the runbook and latest test changes.
All 211 changed non-cache paths have declared frames; the excluded paths are two
Python bytecode cache files. Trace files and named symbols resolve. Temporary
logs are `/tmp/onton-m3-final-regression.log`,
`/tmp/onton-m3-final-architecture.log`, and `/tmp/onton-m3-final-prefix-model.log`;
this record preserves their results without requiring those local files.

The gates and A01–A24 assessments in this historical section describe the earlier
revision. They do not qualify the reopened recovery implementation below. Abstract models, real Git, simulated backend outcomes, rendered frames and
live runtime fixtures are distinguished where their evidence is used.

## Preserved foundation acceptance: A01–A09

The following assessment checks current assertions rather than treating the M2
release record as evidence that unchanged guarantees still hold. The aggregate
regression and focused owner run cited above include these executable surfaces.

| Requirement | Current evidence and completion assessment |
|---|---|
| A01 — duplicate and stale results | `test_branch_reconcile_properties.ml` applies duplicate results at successive protocol phases and asserts identical state with no effects; predecessor tokens cannot advance a successor. Claimed repair duplicates likewise produce no second backend dispatch. The independent model adds generated stale/duplicate delivery interleavings. Qualified for the stated identity/idempotence contract. |
| A02 — checkpoint before mutation | `test_branch_reconcile_git.ml` refuses the initial durable write and asserts zero executions and unchanged runtime state. The real-Git boundary suite adds command/result persistence refusal across the mutation inventory described under A21. Qualified for checkpoint-gated dispatch; the complete interruption inventory is assessed separately under A21. |
| A03 — actual materialization boundary | `run_ensure` in `test_worktree_setup_base_fetch_integration.ml` advances the remote and tracking base after creation, invokes owned provisioning again, and checks the original owner boundary. The Git fixture verifies repeated receipt recovery after later local commits, exact new-branch evidence and adopted-branch non-authority. The provisioning checkpoint fixtures cover receipt loss/recovery. Qualified for retaining the starting revision across base movement. |
| A04 — publication without repeated implementation | The Git fixture accepts the push, substitutes a lost acknowledgement, advances the remote, loads the checkpoint and verifies one total push, unchanged local candidate, retained newer remote work and settlement. `runner_without_backend` in `test_gameplan_publication.ml` exercises publication/retry in normal and feature modes with a backend selector that fails if invoked and asserts zero backend calls. These complementary runtime and real-Git checks qualify publication recovery without a repeated implementation session. |
| A05 — staged-only repair | The real-Git staged backend writes and stages resolutions without committing, checks the persisted claim before invocation, and drives owner continuation to the final published tree with no active sequencer. Staged restart/adoption and multi-step checkpoint fixtures independently check preserved index work. Qualified for staged resolution and owner-controlled continuation. |
| A06 — remote race preservation | The real-Git remote-replay matrix verifies retained candidate identity, incoming work, single replay and both parents for recreated merges; the preserving-merge fixtures check captured ancestry and new remote work after repair. Lease and direct-confirmation fixtures retain evidence through remote advancement and restart. Qualified under both preservation policies, including refusal when preservation cannot be verified. |
| A07 — root completion authority | The captured-contribution Git fixture verifies no publication receipt while acknowledgement is missing, captured child and existing root ancestry, exclusion of a newer uncaptured child revision, then exactly one push and a revision-specific receipt after restart. Concurrent sibling fixtures serialize root writes and preserve all contributing ancestors. Runtime integration consumes the confirmed receipt through the common owner path documented under A24. Qualified for revision-bound descendant completion. |
| A08 — infrastructure retries | Owner retry decisions retain the repair context, apply bounded exponential delay and honor longer finite provider delays. Properties check early-wake rejection, infrastructure interruption without budget consumption and checkpoint restoration; real-Git probe failures never push. The strengthened provider-deadline assertion is documented below. Qualified for retry timing and budget isolation. |
| A09 — empty recreated merge | The Git fixture stages a resolution equal to the first-parent tree, verifies no agent commit, and checks both merge parents and published content. Before/after/result-checkpoint failures around merge creation and subsequent continuation assert one merge commit and one continuation after recovery. Qualified for retained ancestry and nonduplicated completion. |

This closes the foundation assessment; it does not independently close A10–A24,
formal specification review or final gate qualification.

## Preserved release acceptance: A10, A12, A16 and A17

| Requirement | Current evidence and completion assessment |
|---|---|
| A10 — unexpected work on enabled paths | Scheduled/conflict reconciliation, repair handoff, session publication and root contributions reach the same scoped runner/executor (A24 map). Real-Git fixtures independently preserve staged, unstaged, untracked and mixed work for publication and root integration; provisioning/adoption and repair fixtures preserve index/status/bytes and pin newly observed commits before intervention. The history-recovery fixture adds dirty work before and between turns, refuses backend dispatch and retains the external commit plus all three uncommitted work forms. Unexpected successful agent rewrites remain subject to independent verification. Qualified through these entrypoint fixtures and the common mutation boundary; an unverified result cannot publish. |
| A12 — upgrade and retained state | Current persistence properties compare unrelated populated agent state before/after legacy import, retain anchor revisions in the owner, preserve original backup bytes across repeated loads and write version 2. Real-Git legacy fixtures migrate checkpoints with dirty files and active sequencers, retain their work, and require observation before deliberate recovery. The explicit-resume handoff fixture retains a captured remote revision even after its remote ref disappears. Current writer code still emits version 2; the immutable v0.66.0 binary's rejection of that version is recorded with its hash in the M2 evidence. That historical compatibility check remains applicable to the version contract, and is not represented as a new run of the currently installed binary. Qualified for the migration contract. |
| A16 — supported modes and providers | The aggregate regression covers stacked PRs, feature-root integration/CLI, review/CI/WONTDO, backend/session behavior and native-stack restrictions. The deep-stack real-Git fixtures check transformed histories and exact resulting content; feature-root fixtures check sibling ancestry and captured contribution publication. The poller runtime fixture uses SourceHut's Git adapter and parsed GitHub payloads, with both providers subject to the same contextual acceptance rules. Native-stack cases retain restrictions until confirmed absence permits release. Qualified for repository-owned product contracts. External service trials and release publication remain outside this implementation acceptance, as in the M2 handoff. |
| A17 — diagnosable retained work | The TUI directly calls owner diagnostics; twelve rendered-frame cases restore checkpoints and assert identity, revisions, evidence, reason and retry/intervention distinctions. Event logging serializes before/after agents through the persistence encoder. Activity-log tests consume owner checkpoints and assert Pending → Needs_help → Pending transitions. The repair telemetry fixture reads the event from disk before backend execution and checks the exact operation/command and backend session identity for success and exception. Qualified for rendered details and snapshot/event consistency, with the in-memory rendering limit stated below. |

These assessments preserve the M1/M2 acceptance contract at the M3 endpoint.
They do not close the independent-model, complete-boundary and formal-specification
obligations that are specific to M3.

## A08: provider retry deadline

The owner computes a five-second initial retry, exponential growth capped at
300 seconds, and a longer finite provider delay when supplied. The existing
provider-delay property previously checked only an early tick at 104 seconds and
a late tick at the expected deadline; that did not detect waking too early for a
longer provider delay. It now checks unchanged state, no effects, no executable
Git command and no wake event immediately before that deadline, then recovery
at the deadline. The focused owner suite passes all 118 properties (seed
`523138341`). Separate existing properties cover infrastructure interruption
without repair-budget consumption and persistence across restart. This closes
the provider-deadline assertion gap; it does not replace the real-Git probe and
transport failure fixtures in the M2 evidence map.

## A11: bounded, verified agent fallback

The fallback is qualified by the current core budget property and real-Git
backend fixture. The core property serializes arbitrary infrastructure
interruptions, rejects duplicate claims, completes two unsuccessful turns and
checks the persisted `recovery_preservation_unproven` intervention with count
two. The Git fixture returns successful backend results that discard required
work, asserts exactly two backend invocations and no remote publication, and
contrasts this with a verified repair that preserves captured ancestry and the
published patch, extra and upstream file contents. Losing the post-backend probe
then restoring the checkpoint verifies the completed repair with one backend
invocation. Deterministic exhaustion into this path is covered by A19's unrelated
history fixture.

The budget is scoped to repair mode and structural progress: two unsuccessful
content-repair turns escalate to history recovery; two unsuccessful completed
history-recovery turns stop. It is not a two-invocation lifetime limit across
all conflicts, changing replay steps and infrastructure interruptions. This
matches the explicit distinction in the patch and endpoint specifications.

The full regression after the provider-deadline strengthening passes
(`opam exec -- dune runtest`, exit 0; captured in
`/tmp/onton-m3-foundation-audit-regression.log`). The source-policy gate and
`git diff --check` also pass. The temporary log records this workspace run and
is not a portable release artifact.

## Push-result API cleanup

The obsolete `Push_no_commits` and `Push_worktree_missing` variants are removed
from `Worktree_parser.push_result` and the public worktree facade, together with
the executor's unreachable cases. Publication eligibility and checkout presence
remain owner decisions; the push classifier reports only outcomes of an executed
Git command. Existing success, up-to-date, rejection and error properties remain.
The tautological test for impossible variants is replaced by 1,000 arbitrary
exit-code/stdout/stderr totality cases in the core-only worktree parser test.
The parser, general property and real-Git replay targets pass after the removal.
Legacy recovery helper and prompt cleanup is described below.

## Legacy recovery prompt and helper removal

The source caller audit found no live consumer of the merge/rebase conflict
renderers or their optional `conflict_info` reconstruction payload. These APIs,
the old root-merge renderer, their strategy/payload types, and the old anchor and
oldest-commit reconstruction helpers are removed. The pure subject matcher is
retained: `Branch_reconcile.subject_boundary` still uses it for explicitly scoped
best-effort inference. The real-Git dependency fixture now asserts the owner's
actual `Subject_inferred` receipt instead of an obsolete helper's calculation.

Tests specific to the deleted prompt reconstruction API are removed, including
the isolated legacy state machine that exercised its own simulated transitions.
Live uncommitted-change and implementation prompt tests remain, and shared prompt
context tests now use that live surface. The independent owner model, claimed
repair protocol properties and real-Git recovery/checkpoint fixtures continue to
verify recovery behavior. The gameplan marks the old state-machine file deleted
and owns the updated prompt interface/tests. The complete `dune runtest` regression
passes after these removals, including the updated owner-receipt assertion and
live prompt-context tests. Gameplan and source-policy validation also pass.
The architecture gate initially exposed missing direct property references for
the retained fetch classifiers and subject matcher. Core-only parser properties
now cover arbitrary-output totality, success precedence over stale stderr,
missing-ref versus transport failure, and exact project/patch matching. These
properties pass, followed by `dune build @check` and the pinned architecture gate.

## A19: fallback provenance

The named strategies have executable fixtures. Initial replay and remote replay
have different applicable strategies; inferred boundaries do not acquire the
publication authority of recorded boundaries.

| Required behavior | Evidence and assertions |
|---|---|
| Recorded evidence precedes inference | `test/test_branch_replay_evidence.ml` supplies recorded materialization while making inference probes fail. The selected boundary is still recorded. `test/test_branch_reconcile_git.ml` selects an older reachable integration receipt after a source reversion and retains it through a failed newer-boundary probe and restart. |
| Exact-tree reconstruction | `test/test_branch_replay_evidence.ml` reconstructs an unreachable recorded boundary from first-parent history, makes patch-equivalence probing fail to prove precedence, retains both revisions in private refs/checkpoints, and verifies the published tree. Failed reconstruction probes retry. |
| Patch-equivalent prefix | The replay-evidence fixture cherry-picks dependency work onto an advanced base. Only local patch work is replayed; both nonempty and empty outcomes retain their selected strategy after serialization. Failed probes retry. |
| Scoped subject prefix | The replay-evidence fixture squash-merges multiple dependency commits so patch IDs differ. Scoped reconciliation selects the dependency-subject boundary, including an empty outcome. Unscoped requests select plain replay instead. Subject-probe failures retry. |
| Plain replay | `plain_replay` in the replay-evidence fixture exercises both linear and merged local histories without recorded or inferred boundaries. It serializes every command result, verifies the final `Plain` label, and checks published file contents and captured-target ancestry. |
| Verified remote topology | The remote-replay matrix in `test/test_branch_reconcile_git.ml` checks recorded and inferred boundaries, failed common-ancestor probes, interrupted checkout/rebase, persisted receipt evidence, exactly one replay, retained local candidate identity, and incoming/side-branch content. Recreated merge parents are checked separately from tree contents. |
| Agent fallback when applicable deterministic recovery is exhausted | The replay-evidence fixture uses real unrelated histories under ancestry-preserving integration. Deterministic refusal reaches one claimed history-recovery turn; a simulated agent merge publishes only after independent verification of both histories. The Git/checkpoint suite separately covers invalid agent mutations and failed preservation probes. |

Validation: `dune runtest` passed after the plain-replay, poller and composed
deep-stack repair additions. The first aggregate run exposed a missing sandbox
executable dependency in the poller test; its action now uses
`%{bin:onton-setsid-exec}`, and the subsequent aggregate run passed. Final-state
qualification remains required after the outstanding milestone work.
This map establishes the named A19 scenarios; it is not evidence for the separate
full-stack, every-boundary interruption, or competing-trigger requirements.

## Remaining acceptance audit

| Requirement | Current evidence | What remains to establish |
|---|---|---|
| A18 — context-bound forge observations | Forge observation properties, checkpoint tests, revision confirmation tests, and provider fixtures exercise tickets and direct head/base evidence. `test/test_poller_revision_context.ml` runs `Poller_fiber.run` with SourceHut's real Git adapter and parsed GitHub response payloads against local repositories: checkpoint-before-dispatch, accepted pairs, stale/missing head/base, missing PR identity, and PR replacement during an in-flight response. Native-stack cases verify blocked dispatch/base updates and the two-confirmed-absence release threshold. Multi-cycle fixtures reject stale older-head metadata, accept that head after a real remote reversion, and reject an in-flight response after a newer durable ticket replaces it. Both providers also exercise contradictory mergeability and already-satisfied conflict pairs: positive provider metadata cannot bypass retained conflict evidence at the approval gate, while a satisfied pair preserves the settled operation, queues no conflict repair, and emits a visible wait. The checkpoint fixture rejects duplicate delivery and old tickets after restart. | All 25 local poller scenarios pass. Final aggregate validation and the cross-requirement audit remain; GitHub HTTP transport is outside this local payload harness. |
| A20 — full stack transformations | `deep_stack` in `test/test_branch_replay_evidence.ml` exercises four patches with two commits each, alternating squash and cherry-picked dependency merges, three mainline advancements, repeated descendant reconciliation, and retargeting to main. It checks changed versus equivalent patch IDs with Git, exact cumulative file contents, exactly two local commits above each new target, and zero commits for an empty tip. Every command result is serialized and restored. Both variants now force two consecutive content repairs in a dependency patch, restore each claimed turn, stage without committing, and verify both resolutions plus upstream content after all later transformations. Exactly two turns execute; Git continuation and publication stay with the owner. Existing Git fixtures separately cover the backend adapter and merge recreation. | Finish the cross-requirement and final aggregate audit. The new fixture tests owner/executor composition with simulated staged agent outcomes, not scheduler-driven stack discovery or a live model backend. |
| A21 — restart-equivalent interruption | Git/checkpoint fixtures, provisioning interruption tests, durable hook plan/claim/completion failures, context restoration, and process-tree crash tests pass. | Enumerate each live mutation/checkpoint boundary and its before/after evidence, including agent turns and composed provisioning recovery. |
| A22 — dependent recovery-ref lifetimes | Pruning, identity, retirement, lifetime-lock and generated lifecycle tests exercise retention, competing workers and restart. The recovery-ref suite now interrupts before and after each deletion in a three-ref namespace (six cases). Without an independent anchor all owned refs remain; with a protected dependent anchor, every interrupted prefix resumes to complete cleanup while retaining that anchor. Repeated cleanup emits no deletion, and every delete checks its captured tip. The suite also drives the actual prune command with persisted dependent checkpoints. | The inspected retirement and identity map below covers the named A22 scenarios. Final cross-requirement and aggregate qualification remain; ref-deletion interruption tests are distinct from filesystem retirement tests. |
| A23 — independent model | Independent repository models cover rewrite/merge publication, remote writers, retargeting, delayed results, lost acknowledgments, invalid agent completions, destructive rewrites of locally captured work, explicit resume, and settled fixed points. | The linear model now generates base/scoped/identified reconciliation, session-publication, captured-revision publication/integration, verification-only and checkout-provisioning intents under both policies, with restart, lost acknowledgements, outages and duplicates. It checks unchanged active authority, latest-intent settlement, one integration, and at most one push per operation; new publication intents may authorize additional pushes. Verification observations describe the captured checkout candidate and every verification command must preserve local head, remote ref and mutation count. Provisioning accepts only checkout observation commands and preserves repository head, remote and mutation count. Fixed histories cover provisioning queued before publication, requested after publication, and followed by publication, including lost acknowledgements, restart and stale delivery. A separate independent sequencer model generates one to six conflicting steps under both policies, with staged-only agent resolutions, backend interruptions, command outages, lost acknowledgements, stale results and checkpoint restart. It checks one integration start, exactly one continuation per step, preservation of staged progress across restart, no early publication, and a settled fixed point. Paired runs now reverse compatible initial materialization/intent observations under identical generated fault histories and compare final repository state and mutation count (1,000 histories per policy). All 27 model properties pass. This adds independent evidence for that ordering pair; forge/publication ordering remains covered separately by owner properties and runtime fixtures. A multi-owner stack model now generates three to six owners sharing a commit graph and separate checkout/remote refs, with interleaved commands, base advances, competing requests, outages, lost acknowledgements, stale delivery and restart. A dependency-ordered healthy suffix observes each parent publication and checks exact cumulative content, parent inclusion, original ancestry under merge policy, publication and mutation fixed points (500 histories per policy). Its disjoint content-set interpreter does not model patch IDs, squash/cherry-pick equivalence or duplicate replay commits; those transformations still require the real-Git stack fixtures. The sequencer model likewise represents Git facts abstractly and does not replace real-Git acceptance. Captured integration here targets the already represented base revision; it does not establish new stack transformations. Check all stated algebraic/model guarantees against independent evidence. Fair convergence requires fresh observations after external writes cease. |
| A24 — exclusive mutation authority | Scoped patch ownership, project lifetime and drain guards, terminal/capacity tests, and competing-request models exercise specific authority boundaries. The ownership suite now also runs a real concurrent `Branch_reconcile_runner.run` request while a repair fiber holds patch/root ownership. Four added cases cross patch/root mode with staged/unresolved cancellation. The request cannot acquire ownership before cancellation releases the backend, then persists only the new intent without executing Git. Snapshot reload completes the old repair exactly once, observes the queued successor once, and emits no repeated integration or push. | The runtime trigger/authority map below connects all named entrypoints to the common scoped runner and identifies the executable composition evidence. Final endpoint qualification and the remaining formal review still apply. |

## M3 acceptance decisions: A18–A24

The detailed maps above and below identify fixture scope and assertions. The
completion decisions are:

| Requirement | Assessment |
|---|---|
| A18 — context-bound forge observations | Qualified by the 25 poller runtime scenarios, durable ticket/checkpoint properties and live acceptance path. Both provider adapters reject missing/stale/replaced identities, preserve contradictory conflict evidence, wait on satisfied pairs and accept directly confirmed reversion. Native-stack release requires the confirmed-absence threshold. The GitHub fixture boundary is parsed HTTP payloads; it does not claim a live service trial. |
| A19 — truthful fallback provenance | Qualified by the strategy map: recorded precedence, reconstruction, patch equivalence, scoped subjects, plain replay and verified remote topology each have independent real-Git outcomes and serialized evidence assertions. Probe failures retry. Unrelated histories exercise deterministic exhaustion followed by independently verified agent recovery. |
| A20 — stack transformations | Qualified by the deep-stack Git fixture and root/remote race fixtures. Four patches, two commits each, three base advancements, changed/equivalent patch IDs, squash/cherry-pick transformations, retargeting, two staged repair steps and an empty tip are checked through exact content and replay counts. Abstract stack tests supply generated ordering/fault histories; they are supplementary to these actual Git outcomes. |
| A21 — restart-equivalent boundaries | Qualified by the finite command inventory and shared runner checkpoint protocol, with before/after/result-refusal cases for each command kind. `Commit_merge` and post-merge continuation have separate empty-tree/recreated-parent cases. Repair claim/completion, provisioning plan/materialization/hook boundaries, pruning compare-deletion and retirement journals have distinct fixtures. Process-tree cancellation tests establish that an old command tree cannot overlap a resumed owner. These are boundary-class coverage plus specialized Git-state cases, not a claim that every topology was crossed with every interruption. |
| A22 — dependent ref lifetimes | Qualified by actual prune-command fixtures, dependent snapshots, before/after deletion cases, compare-and-delete checks, shared-repository lifetime locks, alias histories and retirement-journal recovery. Unknown inventories and failed probes retain data; eligible resumed cleanup reclaims only the captured namespace/payload. |
| A23 — independent model | Qualified by 27 properties over independent commit/content, sequencer and multi-owner stack interpreters. Generated fault histories shrink; both policies check preservation, captured authority, stale/duplicate isolation, restart behavior, bounded repair contexts, settled fixed points and healthy-suffix convergence. Compatible initial evidence ordering is compared between paired runs; publication/forge ordering has additional owner/runtime evidence. Total decoding and malformed-input boundaries are covered by core-only properties. Convergence explicitly requires serviced commands, fresh observations, restored services, successful repairs and authorized resumes where an intervention requires them. |
| A24 — one mutation authority | Qualified by the runtime trigger map, scoped owner capability, common checkpoint runner, project lifetime/drain locks and concurrent cancellation/request fixtures. Scheduler histories retain one message identity across poll/restart interleavings. Root contributions serialize siblings and match completion to the captured revision. Compatibility is constrained to equivalent evidence; distinct publication intents are not assumed to commute. |

The boundary inventory's earlier “remaining proof” notes record how the matrix
was assembled. Their referenced consolidation is supplied by these decisions and
the provisioning, lifecycle and trigger maps; no additional command kind is
outside the inspected common checkpoint dispatcher. Formal-specification review, final patch projection and final gates are recorded
separately in this audit; those checks now pass.

## A24: runtime trigger and authority map

The inspected runtime routes converge on one scoped command runner. This map
connects scheduling tests to the actual handlers, rather than treating generated
owner requests alone as evidence that a runtime caller uses the owner.

| Trigger | Live route and authority | Composed executable evidence |
|---|---|---|
| Poll / conflict evidence | `Poller_fiber` accepts a durable context-bound ticket; `Patch_controller` plans messages. The `Reconcile_branch` dispatcher checks the captured operation ID under `Runtime.with_patch_ownership` before `wake_event` and `run_owned`. | Poller revision-context and forge-checkpoint cases reject stale tickets; outbox histories interleave polling, ticks, duplicate requests and session receipts without a second accepted action. |
| Scheduled base reconciliation | The `Rebase` dispatcher enters `With_busy_guard`, which acquires scoped patch ownership, then submits an identified/scoped request to `run_owned`. | Owner-driven real-Git rebase fixtures inspect final trees, ancestry and scoped boundary receipts; scheduler/outbox properties retain active authority and pending delivery through changed base observations. |
| Session publication | `Session_driver.run` acquires ownership; nested `run_owned` uses the same handle. `publish_completion` submits the session completion's publication intent and reconfirms before reporting publication. | Publication integration checks exact captured revisions, dirty-work retention and remote races; outbox properties preserve the independent implementation receipt through retries and completion. Ownership tests cancel an accepted session during its capacity wait, prevent backend start and verify both locks remain available. |
| Conflict repair | The feedback dispatcher submits reconciliation under `With_busy_guard`. `run_repair` acquires capacity first, then ownership, rejects terminal/stale claims, re-inspects and invokes the claimed turn. | Ownership fixtures interleave cancellation and a concurrent new request in patch/root modes, restore on-disk checkpoints, and verify continuation/push/backend counts. Real-Git staged and unexpected-agent-mutation cases separately prove content preservation and publication checks. |
| Root contributions | `execute_integration` holds the contributing patch lock and destination root ownership, revalidates the captured head/checks, then submits `Integrate_revision` under ancestry policy. | Feature-root Git fixtures race siblings, lose confirmation, cancel commands, and verify exact published ancestry. Revision-bound completion tests prevent a newer child head from consuming an older receipt. |
| Retry / restart | Reconciliation delivery keeps its original message ID. Startup clears stale busy state; the dispatcher validates the operation and uses `wake_event`, then the same owned runner. | Generated scheduler restart histories accept once per runtime; command/checkpoint cases inspect Git before resuming lost acknowledgements; terminal/capacity fixtures prevent stale backend dispatch. |

`Runtime.with_patch_write` acquires the root mutex for the integration-root patch;
`with_patch_ownership` expires its handle on every exit. `run_owned` derives the
runtime and patch from that handle, and the runner checkpoints before command
execution and before successor dispatch. Repair keeps capacity and ownership
through the backend and releases them on cancellation. `With_busy_guard` clears
only the matching busy message on exceptional exit. These shared mechanisms are
the composition boundary between the entrypoint tests above and the real-Git
command tests. The evidence does not claim a single live-provider test executes
every trigger permutation or that arbitrary order-sensitive intents commute.
Compatible materialization/intent ordering is checked by paired independent-model
runs; candidate observation/publication confirmation ordering is checked at all
three confirmation positions by owner properties with checkpoint restoration.

## Formal convergence assumptions

The patch and endpoint contracts now explicitly require that pending commands and
retries are serviced, and that an operator authorizes any required resume, before
asserting fair convergence. Stable refs, available services and successful repair
outcomes alone cannot make an unscheduled command execute or authorize recovery
from a deliberate intervention. The independent model's healthy suffix already
supplies ticks/commands and explicit resume after exhausted history repair; the
core tests separately require interventions to remain stopped without that input.
This makes the formal assumptions match the existing recovery contract. It does
not turn an intervention into an automatic retry or weaken stale-result,
exclusive-ownership, preservation or bounded-repair requirements.

## A24: scheduler acceptance evidence

`test_branch_reconcile_outbox.ml` now drives the actual `Patch_controller.tick`
acceptance path through 1,000 generated histories in patch and root modes. Each
history first dispatches one reconciliation, then interleaves poll-style base
updates/message planning, duplicate owner requests, repeated scheduler ticks,
session-completion receipt recording, and snapshot encode/decode. The active
owner stays unchanged, the patch remains busy, receipts survive, old message
acceptance is refused and no second action is dispatched. A further 500 histories restore through `Runtime.create` and apply the startup stale-busy reset, interleaved with ticks, poll planning and duplicate requests. Each runtime accepts the same original message/action exactly once, preserves the owner checkpoint and rejects repeated dispatch. All nine outbox
properties pass. This complements the concurrent runner cancellation cases;
it is an orchestrator/scheduler fixture, not a live provider or backend session.
Root sibling races and real process cancellation are separately exercised by
`test_feature_branch_git.ml`; the runtime trigger map above identifies how these
fixtures compose at the shared authority boundaries.

## A21: live boundary inventory

`Branch_reconcile.command_kind` is the authoritative command inventory.
`Branch_reconcile_runner` checkpoints intent before dispatch and command results
before subsequent effects; repair claim is a separate checkpoint. The following
map distinguishes observed fixtures from boundaries still needing explicit proof.
Serialization after every result in the stack fixture is useful evidence, but it
does not simulate a process dying between a Git effect and its acknowledgement.

`test_branch_reconcile_boundaries.ml` now supplies 42 real-Git command cases plus
six repair-completion checkpoint cases. For each of
`Observe`, `Pin`, `Integrate`, `Publish`, `Confirm`, `Inspect`, `Continue`,
`Verify_recovery`, `Plan_remote_replay`, and `Checkout_remote`, stop before execution,
stop after execution but before acknowledgement, or refuse the result checkpoint.
Every case reloads an on-disk runtime snapshot, verifies in-memory rollback to
that durable state, resumes, and checks the exact published files and replayed
commit list. Ordinary-path counters require exactly one integration and one push
across the entire interrupted/resumed run; remote-race counts are stated below. Inspection and continuation cases use real
rebase and plain-merge conflicts with staged resolutions; after restart they preserve both local
and upstream content and execute exactly one continuation, without a second
repair turn. History-recovery cases begin with real unrelated histories, reach a
claimed agent turn after deterministic integration fails, and simulate a preserving
merge. Interrupted verification cannot push; restart repeats verification of the
completed agent result and publishes with both original histories reachable. The
verification counter excludes the separate pre-agent baseline check. Remote-replay
cases create a real lease race, interrupt planning, checkout, integration or publication, and retain incoming
work plus the exact captured local candidate. They require one checkout, one initial
integration plus one remote replay, and exactly two push attempts (one rejected,
one successful). Recreated-merge continuation is exercised separately in the Git/checkpoint
suite, and provisioning boundaries are mapped below.

The repair-completion cases stage real rebase and merge resolutions, then stop before its
completion checkpoint, save that checkpoint but lose acknowledgement, or refuse
the write. Before restart there is no continuation or push, and the index retains
the resolution. A fresh runtime distinguishes the newer saved completion from a
refused write, inspects the checkout and publishes without returning another
repair turn. Exactly one continuation and one push occur across recovery. All 48
cases passed under Dune's sandbox. Merge cases additionally verify both captured
parents in order and exactly one merge commit.

| Command or boundary | Inspected evidence | Remaining proof |
|---|---|---|
| Initial request / dispatch checkpoint | `test_branch_reconcile_git.ml` refuses persistence and asserts zero executions plus unchanged runtime. | Ordinary-path result refusal is covered by the new matrix; specialized result checkpoints remain. |
| `Observe` / `Inspect` / `Confirm` | Revision confirmation, publication recovery and restart fixtures inspect actual refs and retained candidate identity. | `Observe`, `Confirm`, and staged-repair `Inspect` are covered by the new matrix; specialized observation paths remain. |
| `Pin` | Adopted-rebase fixtures interrupt before pinning, restore from disk and preserve staged resolutions; private refs and receipts are checked throughout the Git suite. | The ordinary `Pin` before/after/refusal cases pass; consolidate special adopted/recovery pinning paths. |
| `Plan_remote_replay` | Common-ancestor probe failure persists backoff, restarts, and leaves checkout/reset/rebase counts at zero. | Planning before/after/refusal cases now pass in the real lease-race matrix; consolidate with failed-probe evidence. |
| `Checkout_remote` / `Integrate` | Remote replay matrix interrupts before and after both commands, restores snapshots, checks exact replay counts, and preserves late staged edits when checkout is refused. Unclaimed rebase tests cover lost acknowledgement and wrong-target continuation. | Ordinary `Integrate` and remote `Checkout_remote` before/after/refusal cases pass. Remote replay integration before/after/refusal now passes with exactly one initial integration, one remote replay and one checkout. |
| `Commit_merge` | Recreated-merge fixtures interrupt before and after the merge commit, including a resolution with no staged tree changes, then restore and converge. The added history-9 case executes the commit but rejects its result checkpoint. All three cases assert exactly one merge commit, unchanged runtime versus the saved checkpoint after acknowledgement loss, both merge parents, captured-candidate ancestry, and preserved tree contents. The complete Git/checkpoint executable passed. | Consolidate the completed command-boundary evidence with agent/provisioning and final aggregate qualification. |
| `Continue` | Staged two-step repair, adopted sequencers, interrupted rebases, and deep-stack repairs exercise continuation and retained target identity. | Staged rebase and plain-merge `Continue` before/after/refusal cases pass with exactly one continuation. Plain merges retain both parents in order and exactly one merge commit; The recreated-merge fixture now also interrupts the post-commit continuation before execution, after execution without acknowledgement, and at result-checkpoint refusal. Snapshot reload retains both merge parents, executes exactly one merge commit and one subsequent rebase continuation, and publishes preserved content. The complete Git/checkpoint suite passes. |
| `Publish` | Lost push acknowledgement, changed remote destinations, lease races and later remote advancement retain the captured candidate and avoid duplicate publication. | Ordinary and remote-replay `Publish` before/after/refusal cases pass. The replay cases retain incoming and captured local work, with exactly two push attempts across restart (the initial rejected lease and successful publication). |
| `Verify_recovery` | Invalid agent outcomes, preservation probe failures and recovery/remote-write races are covered by Git and independent-model fixtures. | History-recovery verification before/after/refusal cases pass, prohibit push before acknowledged verification, and reverify after restart. Consolidate this with remote-write and invalid-repair outcomes. |
| Agent claim / invocation / completion | Runtime adapter tests inspect the on-disk claim before invocation; capacity/ownership tests prevent stale or terminal dispatch. `failed_claim` in the ownership suite uses real snapshot persistence with simulated Git observations: for both patch and root modes it refuses the claim checkpoint or saves it then interrupts acknowledgement. Neither path returns dispatch permission; disk distinguishes the unclaimed offer from the saved claim. A fresh runtime re-inspects and invokes the backend exactly once with a durable claim. Deep-stack tests restore claims and completion state around staged outcomes. | Repair-completion before-save, after-save/lost-acknowledgement and refusal cases now pass with real staged Git state and no repeated turn. The ownership suite also cancels an actual running Eio repair fiber in patch and root modes, before and after a simulated staged resolution. Real snapshot reload preserves the uncompleted claim and zero spent budget; capacity and both locks are released. Staged restart continues/publishes exactly once without another backend turn; unresolved restart obtains one fresh turn. Old-claim redelivery invokes neither Git nor the backend. Consolidate this with process-lifetime evidence; the Git adapter in this cancellation fixture is simulated, and process-kill evidence remains separate. |
| Provisioning and creation hooks | Provisioning retry/restart and hook plan, claim, completion, unknown-outcome, context and post-hook inspection fixtures are present. | The provisioning/hook map below now identifies request, plan, materialization, claim and completion boundaries, with exact create/hook counts and durable receipt checks. Final cross-requirement qualification remains. |

## A21: provisioning and hook checkpoint evidence

The inspected `test_worktree_setup_base_fetch_integration.ml` fixtures exercise
these distinct boundaries using real Git checkouts and on-disk snapshots:

| Boundary | Fixture and verified outcome |
|---|---|
| Owner request checkpoint before any checkout effect | `scenario_owned_provisioning` makes the checkpoint unwritable and asserts zero probes/creates, unchanged owner state and preserved implementation-session budgets. |
| Probe cancellation, retry and restart | The same fixture cancels a claimed inspection, verifies the pending operation survives, exercises backoff without new probes, reloads a snapshot and creates exactly once. Subsequent requests re-inspect without publication. |
| Hook plan before creation | `scenario_checkpoint_failure` rejects the plan checkpoint before checkout creation or hook dispatch. |
| Creation effect before materialization receipt | The materialization variant of `scenario_hook_checkpoint_failure` creates the real checkout, then refuses its receipt checkpoint. Memory and disk retain no receipt; the checkout remains. Reload recovers its materialization evidence, creates no second checkout and runs the pending hook once. |
| Hook claim before dispatch | The claim variant blocks the checkpoint after checkout materialization but before the claim write. No hook runs until a durable claim is possible. |
| Hook effect before completion checkpoint | The completion variant runs the hook then refuses completion persistence. Restart stops on unknown outcome; only explicit resume permits another attempt. A later restart does not repeat the acknowledged hook. |
| Interrupted creation/hook and changed context | `scenario_hook_restart` interrupts both boundaries, retains the captured script despite configuration changes, refuses a substituted checkout path, and converges after authorized resume with exact hook-write counts. |

The complete provisioning/base-fetch target passes after the added materialization
case. These checkpoint refusals complement the process-tree kill fixtures;
they establish distinct durable boundaries rather than claiming every case kills
the test process itself.

## A22: retirement and identity evidence

| Required behavior | Authoritative fixture and scope |
|---|---|
| Retain dependent replay evidence | `test_recovery_ref_prune.ml` drives the actual prune command from persisted owner/dependent snapshots. A live dependent retains the owner's refs; a finished dependent releases them; a protected independent anchor allows eligible reclamation without deleting that anchor. Shared managed repositories remain until aliases finish. |
| Resume partial ref deletion safely | Six before/after deletion cases cover every ref in a three-ref namespace, guarded by captured tips. Restart finishes cleanup; unrelated/protected namespaces survive; repeated cleanup is mutation-free. Existing tests reject failed probes, changed tips, symbolic/non-commit inventories, and newly appearing refs. |
| Exclude live users and old command trees | `test_project_lifecycle.ml` tests actual kernel leases, killed writers and startup exclusion. `test_process_tree_cancellation.ml` exercises command-drain guards across process death. Recovery-ref command tests refuse pruning a live shared repository even without an exclusive supervisor lock. |
| Preserve the next project incarnation | The lifecycle fixture retires by rename, recreates the original path, then cleans the captured payload. A separate child is killed after the rename; subsequent cleanup still preserves the replacement. Payload symlinks cannot redirect deletion outside the journal. |
| Recover prepared or partially removed journals | The new lifecycle case calls retirement with a missing source: the manifest is saved but rename fails. A project subsequently created at the original path survives cleanup, and the empty prepared journal is reclaimed. Existing cases reclaim an empty journal after marker removal and preserve unrecognized data. The pure retirement suite checks total decoding, manifest normalization and path/schema boundaries. |
| Keep historical aliases in the same namespace | `test_project_identity_properties.ml` generates alias histories and checks exact stored-name preservation. `test_feature_branch_cli.ml` resumes through case, underscore, whitespace and punctuation aliases, checks the exact recovery prefix and existing Git ref, and rejects a copied/misfiled configuration before it can redirect startup. |

The lifecycle, identity, retirement and CLI targets passed after the prepared-journal
addition. These fixtures establish concrete lifetime boundaries; they do not claim
power-loss durability beyond the process-interruption semantics exercised here.

## Legacy verification recovery evidence

`test_legacy_publication.ml` now persists the wrapper's missing-checkout
intervention, restores a registered checkout externally with a feature commit,
and reloads the actual snapshot into a fresh runtime. Explicit resume settles
verification without checkout creation, metadata repair or creation hooks. A new
normal reconciliation request after main advances then preserves both the feature
and upstream commits (verified with real Git ancestry), publishes the resulting
head, and saves the settled normal intent. The complete legacy-publication target
passes. The subsequent liveness audit found that queued reconciliation could not
escape changed-source or unconfirmed-publication verification intervention.
The owner now hands a queued mutation-capable intent to a new operation only on
explicit resume, retaining all captured revision obligations and the stronger
preservation policy. The new property regression failed before the change and
passes afterward, including restart, stale-result isolation, and verification-only
behavior without both prerequisites. Real-Git additions exercise missing remote,
different remote and late local-source changes, with a lost observation result
after the handoff. The full `dune runtest` regression passed after the handoff change, including the real-Git legacy cases. A subsequent real-Git case captures an independent remote commit during verification, replaces the remote ref before explicit handoff, and confirms that the retained obligation survives restart and is pinned in the recovery namespace. The successor cannot publish against the now-easier lease until a claimed history-recovery turn merges that retained revision. Independent verification then settles publication with the original local commit, captured remote commit and base all reachable. The complete legacy-publication target passes with this adversarial case. Formatting and architecture gates remain part of final-state qualification.

## A10: root integration and session publication work preservation

`test_feature_branch_git.ml` independently injects staged, unstaged, untracked
and mixed changes into the root checkout before submitting a captured descendant
revision. All four cases require `dirty_worktree`, exact index-tree and status
retention, unchanged tracked/untracked bytes, and unchanged local/remote heads.
After the fixture removes only its injected edits, explicit resume publishes the
retained contribution. The complete feature-root Git suite passes.

`test_push_plan_integration.ml` adds the same four shapes under both publication
policies. These eight fixtures exercise `Publish_session` through the production
owner/executor with checkpoint serialization before every command. Publication
settles with the exact committed candidate while preserving the index, status,
checkout head and all dirty bytes; the remote tree contains neither the staged
changes nor untracked work. The complete publication integration target passes.
An initial test incorrectly required dirty publication to intervene; the design
explicitly permits pushing the captured commit without changing dirty files, so
the corrected oracle verifies that preservation contract. These fixtures cover
Git protocol entrypoints, not a live implementation backend or every scheduler
interleaving; root and publication evidence must be combined with the runner and
scheduler ownership fixtures for the full acceptance audit.

## A12: legacy backup byte preservation

The M2 evidence named original snapshot backups, but the current persistence
suite had no explicit `.pre-v2` byte assertion. A new property generates 30
snapshots, writes version-1 JSON with deliberate whitespace, and checks that
loading preserves both the original input and an exact-byte backup. Loading a
second valid legacy input leaves the first backup unchanged. Saving the migrated
snapshot produces version 2, and subsequent loading still leaves the original
backup intact. All 30 persistence properties pass. This supplies durable regression
evidence for backup retention; the older installed-binary rejection result remains
historical M2 evidence and is not a newly executed check.

## A13: independent scheduling during publication backoff

The new outbox property uses two independent patches with distinct branch names.
One submits a captured-revision publication intent and encounters a transport
failure during preparation. Repeated scheduler planning leaves its owner state
and retry deadline unchanged while the second patch's Start is accepted. The
first patch remains non-busy, cannot execute before its deadline, and retains its
original Pending message through snapshot encoding and decoding. With the second
patch already busy, the exported snapshot still reports `runnable: true` for the
remaining reconciliation work. All 10 outbox properties pass (seed `66858909`).

This closes the missing direct scheduler/export assertion in A13. The ownership
suite separately acquires patch/root locks during capacity waits, prevents stale
or changed claims from invoking a backend, and checks capacity release on exit.
Together these qualify pending visibility and independent scheduling without
claiming that the new pure scheduler property ran a second live backend. The
cloud-facing contract inspected here is the exported snapshot and outbox; there
is no cloud service implementation in this repository.

## A14–A15: terminal work and repeated conflict reports

A14 is qualified by the scheduler terminal-delivery property and runtime
ownership cases. The latter change a patch to merged or WONTDO during capacity
acquisition, then assert that neither Git nor the repair backend runs, the owner
checkpoint remains identical, and direct recovery is also refused. Actual repair
fiber cancellation in patch/root modes releases capacity and ownership, retains
the claim without spending budget, and re-inspects before continuation or a new
turn after restart. The corresponding staged and unresolved outcomes are mapped
under A21/A24. A cancelled session capacity wait cannot start implementation.

A15 is qualified by the outbox and provider-context fixtures. Repeated conflicting
reports for an independently confirmed published pair preserve operation,
integration and publication receipts, queue no conflict repair and emit visible
wait logs. Directly confirmed head reversion is accepted as new evidence;
unconfirmed older metadata and failed probes cannot establish that reversion.
Both supported provider adapters exercise satisfied-pair waiting. These tests
check retained owner authority and suppressed mutations, not just report parsing.

## Migration test and operator documentation review

The migration properties retain live assertions for publication verification,
owner preservation, unrelated state and interventions, base receipt recovery,
and round-trip behavior under malformed legacy inputs. Five incidental
assertions checking absence of retired JSON fields were removed: the surviving
behavior is the regression contract. All 30 persistence properties pass after
that cleanup (seed `28757287`).

The operator runbook now describes implemented dependent-aware pruning,
exclusive lifetime ownership, interrupted retirement and contextual forge waits,
replacing its obsolete statement that these remain future M3 work. Its file is
assigned to the qualification patch, bringing the current non-cache path/frame
inventory to 211. Earlier prefix-build counts above describe the prior 210-path
projection; final prefix qualification must include the runbook as well.

## A17: captured publication diagnostics

`test_tui_render_frame.ml` now drives observation, pinning and integration before
injecting a publication failure, round-trips the owner checkpoint, and renders
the actual detail frame. Twelve cases cross retry/intervention with recorded,
inferred, patch-equivalent, subject, reconstructed and plain replay evidence.
They assert the exact source, target, candidate and remote-lease revisions,
operation identity, evidence label and specific failure reason in the rendered
text. Only the retry state displays the expected deadline. These supplement the
existing unknown-revision rendering checks and owner diagnostic properties; the
complete frame test passes. The fixture renders and strips ANSI in memory rather
than driving an interactive terminal or asserting a full runtime event sequence.

## Repair-session telemetry correlation

The detail view already derives repair mode, last claimed operation/command pair,
completion state and no-progress count from `Branch_reconcile.diagnostics`.
Existing owner properties verify those fields after serialization and budget
exhaustion. The repair adapter now emits `branch_repair_session_started` before
backend dispatch, connecting the durable operation/command identity to the exact
`onton_session_uuid` used by backend telemetry. The telemetry integration fixture
restores a claimed owner checkpoint, reads the event from disk inside the backend
callback, and checks correlation for both success and a backend exception. This
adds the missing explicit link; it does not treat a started event as completion.
The focused telemetry target and the full `dune runtest` regression pass with the correlation event.

## Formal contract and checklist review

The atomic-cutover spec groups owner, forge and lifecycle laws; the qualification
spec states the independent-model obligations. The endpoint spec repeats those
laws together, rather than relying on imports or a retired intermediate API.
Their predicates describe valid protocol observations: a completed no-progress
event means the verified result of a completed turn in the same repair context,
not merely backend exit; structural progress and infrastructure-only events are
distinct classifications. Stale result tokens cannot count as completed turns.
Verification-only forbids branch/history publication mutation while allowing
retention refs needed to preserve captured evidence. Publication preservation
includes all retained revisions, not only the latest remote lease.

The source review maps these laws to `Branch_reconcile.step`/`repair`, the common
checkpoint dispatcher, contextual forge validation, hook plan/claim/completion,
project lifetime/drain ownership and compare-guarded retirement. The A01–A24 maps
provide the executable evidence. Model fairness includes serviced commands and
authorized resumes as well as restored services, successful repairs, fresh
observations and finite work; intervention does not silently become automatic
retry. The specs are declarative contracts with total predicates, not an
executable Git model or a machine proof of implementation correctness. Pant
parsing checks syntax; generated models and real-Git/runtime tests check behavior.

All eight functional changes have one implementation owner and routed evidence;
the atomic cutover supplies the qualification patch's real API preconditions.
The original four-patch split was rejected by an isolated compilation failure,
and the two-prefix replacement compiled. No new service, flag, runtime owner or
manual action between patches is introduced. Operational considerations identify
actual snapshots, leases, provider inputs and downgrade constraints. The runbook
and architecture-domain correction are now accurately described in the frames.

The checklist is reviewed rather than uniformly green: `earlyPatchesNonFunctional`
and `testStubsInInfraPatches` remain false because the first patch is the necessary
atomic behavior cutover with its regression tests. The other checklist items are
supported by the inspected contract, call-path/test maps, scoped dependencies and
validation. These two documented workflow exceptions do not defer endpoint
behavior or require an operator action between patches. Final projection and build/test/format/source/architecture gates pass on the
implementation state, as recorded above.

## Endpoint closure

The owner-state/API cutover, legacy migration and explicit-resume liveness,
repair telemetry and detail views, architecture laws, inline patch/endpoint
specifications and preserved M1/M2 guarantees are assessed above. The source gate
includes non-ignored untracked OCaml modules and skips removed tracked files;
its isolated fixture checks forbidden uses, ignored generated output and stale
allowlist entries. The architecture gate verifies the pure/effect boundary.

The gameplan has two reviewable patches. The first supplies mutually dependent
owner, forge and lifecycle interfaces with migrated consumers and tests. The
second supplies the independent model, documentation and final qualification.
Both compile over the M2 base. Implementation acceptance leaves no endpoint
behavior deferred to a later milestone. The approved workstream status update completes the documentation handoff.
Release and live-provider trials remain the separate operator handoff already
defined by the workstream.

## Reopened recovery implementation and terminal-route audit

The owner now asks the pure obligation planner what work remains and whether a
deterministic or agent strategy is available. This replaces early Git-state
rejections with ordered preservation, local completion, integration and
publication obligations. The owner still verifies observations and emits durable
commands; handlers perform effects. The planner never treats a diagnostic string
as a reason to omit fallback.

| Outcome at the owner boundary | Current route |
|---|---|
| Dirty work, changed source/target, unexpected sequencer, detached/wrong branch | Preserve captured and observed revisions, offer owned full recovery; finish local work before base integration |
| Failed deterministic merge/rebase continuation, merge commit, replay checkout or push | Inspect after failure; full recovery after two failed mutation attempts |
| Exhausted content-only conflict repair | Escalate to full history recovery |
| Clean history but rejected publication | Publication repair; attempt budget survives rejected pushes |
| Accepted agent timeout/failure | Independently verify; count no progress toward the full-recovery budget |
| Failed observation or unavailable transport probe | Backoff, then bounded diagnosis after repeated failure; no mutation attempt is charged |
| Backend failure before accepting a turn | Retain the owned turn for retry without charging an agent attempt |
| Lease race or merge-queue lock | Confirm fresh remote state; repeated unavailable evidence reaches diagnosis |
| Write permission/workflow scope | Bounded publication recovery; prompt prohibits credential/scope changes or removing work to evade authorization |
| Changed or unsupported destination | Bounded observation-only diagnosis; captured destination remains binding |
| Legacy verification-only intent | Observation-only diagnosis before a hold; queued mutation intent still needs explicit resume to transfer authority |
| Stale command identity | Ignore without altering the current claim |
| Impossible result type for the current command | Bounded diagnosis; the result cannot authorize mutation |
| Provisioning ownership/receipt mismatch or unacknowledged creation hook | Bounded diagnosis, including without HEAD; no ownership transfer or blind hook replay |
| Gameplan-publication patch | Same owned repair dispatcher and backend path as other patches |
| Merged/WONTDO patch | Scheduling and dispatch remain held |
| Exhausted full agent recovery | Hold with work retained; explicit resume renews the budget |

The audit covers `Branch_reconcile.step`, `verification_result`, inspection and
recovery planning; every executor `Needs_diagnosis` result; provisioning handoffs;
`Branch_reconcile_runner` dispatch; `Patch_agent.reconciliation_hold_reason`; and
`Patch_controller.plan_action`. Pending owner work precedes legacy session/CI
intervention counters. These ordinary Git recovery paths no longer require the
user to clear a diagnostic stop before the first full agent attempt.

Upgrade decoding re-evaluates old diagnostic-only interventions once without a
reason allowlist, retaining destination authority and exhausted full-recovery
budgets. Recovery prompts carry patch context and human guidance. Source and
remote preservation remain independent requirements before publication; dirty
bytes remain in the checkout and the agent is instructed to preserve them.
Commit refs do not constitute a byte-level backup or semantic proof of uncommitted
edits.

Focused evidence includes pure obligation and owner properties, the independent
repository model, and real Git/checkpoint fixtures. The new fixture covers two
existing commits plus three dirty files across restart, bump and human guidance;
two `merge --continue` timeouts; two push timeouts; local pre-push and remote
pre-receive validation; persistent publication rejection; and switched/detached
HEAD recovery. Agent callbacks make actual Git edits. Exit 124 is injected for
the timeout cases; this is not a live backend or a 120-second wall-clock trial.

## Historical qualification — first recovery correction

The earlier full rerun exposed an obsolete expectation that wrong-branch
observations wait indefinitely. That fixture now requires owned history recovery,
and real-Git fixtures independently prove recovery to publication. The final full
regression run passes after that correction.

| Gate | Current result |
|---|---|
| `opam exec -- dune build` | Pass |
| `opam exec -- dune runtest` | Pass, including the full publication matrix and Git/checkpoint suite |
| `opam exec -- dune build @fmt @check` | Pass |
| Source-policy gate and `git diff --check` | Pass |
| Pinned archlint OCaml adapter | Pass, including pure API property coverage |
| Gameplan schema, Pant, dependency, reference and frame validation | Pass |
| Atomic-cutover prefix over `7452ee3` | Builds; independent model and recovery Git test stanzas deferred until their sources exist |
| Final prefix | Builds with no deferred executable stanzas |
| Final-prefix properties | Planner 12, owner 130, independent model 27, scheduler/outbox 10; all pass |

The final-prefix seeds are `255091383` (planner), `254983415` (owner), `60565969`
(model), and `532761785` (outbox). The complete non-cache change inventory has
217 paths, with implementation changes covered by declared frames and the
gameplan itself treated as metadata. The final verification logs are
`/tmp/onton-m3-reopened-qualified.log`,
`/tmp/onton-m3-reopened-architecture-qualified.log`,
`/tmp/onton-m3-qualified-cutover-build.log`,
`/tmp/onton-m3-qualified-final-prefix.log`, and
`/tmp/onton-m3-qualified-prefix-properties.log`.

The two checklist exceptions remain the documented atomic behavior cutover and
colocated tests; they do not defer recovery behavior or require an operator action
between patches. That earlier completion claim was insufficient: it did not test the authority and gameplan bypasses. Those results did not qualify the second reopening; its final evidence follows.
The evidence does not claim live-model success rates or release deployment.

## Current qualification — complete fallback boundary

The unrestricted terminal result and repair-denial event have been removed.
The obligation planner distinguishes evidence reads, deterministic mutation,
content repair, full recovery and observation-only diagnosis. Lack of mutation
authority limits the agent's scope; it no longer suppresses agent assistance.

The only owner intervention constructor is reached from the three exhausted-budget
decisions (diagnosis, repair, independently verified recovery). Handler refusals
now enter the owner as diagnostic obligations. The gameplan-publication dispatcher
uses the common backend path. External dispatch still honors merged/WONTDO state.
A failed or missing durable checkpoint is reported as a checkpoint failure: no
backend may claim an operation whose identity was not durably established.
Malformed provisioning intents are rejected before a valid owner can be created.

Evidence reads have a separate persisted failure count. Complete observations
reset it; failed mutation counts and publication recovery attempts remain scoped
to their outstanding work. Backoff saturation is never recovery exhaustion.
Upgrade restores every unexhausted diagnostic-only intervention, including
intermediate checkpoints with planning counters; exhausted agent budgets survive.

The independent repository model now interprets diagnostic assistance without
changing Git content. Healthy suffixes explicitly resume exhausted recovery;
the model checks that a real completed-agent budget exists before doing so.
A generated owner property changes diagnostic strings, restarts and duplicates
completion events while requiring exactly two turns before intervention.
Backend acceptance fixtures cover diagnosis without a checkout HEAD and verify
event forwarding. The live dispatcher logs final recovery findings.

The corrected implementation passes the following gates:

| Gate | Result |
|---|---|
| Build and fatal warnings | Pass |
| Full `dune runtest` | Pass, including gameplan-publication backend recovery, the complete Git/checkpoint suite and publication-interleaving matrix |
| `dune build @fmt @check` | Pass |
| Source-policy gate and `git diff --check` | Pass |
| Pinned archlint OCaml adapter | Pass at `855690b1d0f821a177af0ea68e8eb6ae6a263ecc` |
| Gameplan schema, Pant and frame validation | Pass; 218 non-cache paths, with the gameplan treated as metadata |
| Atomic-cutover prefix over `7452ee3049322d0070666782c7b25af485034946` | Builds; only the two qualification-only test executables are deferred |
| Final prefix | Builds with no deferred executable stanzas |
| Final-prefix properties | Planner 13, owner 134, independent model 27, outbox 10; all pass |

The final-prefix seeds are `44089028` (planner), `308989935` (owner),
`333191074` (independent model), and `380070466` (outbox). Current logs are
`/tmp/onton-m3-final-build.log`, `/tmp/onton-m3-final-runtest.log`,
`/tmp/onton-m3-final-check.log`, `/tmp/onton-m3-final-architecture.log`,
`/tmp/onton-m3-final-cutover-build.log`,
`/tmp/onton-m3-final-prefix-build.log`, and
`/tmp/onton-m3-final-prefix-properties.log`.

The two checklist exceptions remain early non-functional patches and separate
infrastructure test stubs: the verified API cutover is atomic and includes its
producer regressions. They defer no recovery behavior. The reopened local
implementation goal is complete. No PR, release or deployment was performed.
Agent fixtures demonstrate dispatch and verified Git outcomes; they do not
claim live-model success rates or OS enforcement of observation-only prompts.

## Rebase onto #486 — behavioral compatibility

M3 is rebased onto `0c93fb099384636c958da7e7410565090b9006d8`, the merge
of [#486](https://github.com/flowglad/onton/pull/486). The preceding qualification
records the pre-rebase tree; the checks below qualify the combined implementation.

| #486 behavior | M3 disposition and regression evidence |
|---|---|
| Finish interrupted staged and unstaged work before base reconciliation, with or without a PR | Preserved through owned `Finish_local_work` recovery rather than a separate `Awaiting_session` scheduler phase. `test_branch_reconcile_git` restores the checkpoint, exercises both PR states, commits the retained work through a repair turn, and checks remote content. |
| Reconcile the deferred base after the session, including a base that advances during recovery | Successful, independently verified completion of dirty work marks the operation for fresh observation after publication. The retained #486 Git regression advances `main` while recovery is pending and requires the final tree to contain both the session changes and the new base file. |
| Retain recovery across missing/rediscovered PRs and automerge races | Local owner recovery remains available without a PR. Planning and outbox acceptance defer it while automerge is in flight; a failed merge releases the same message, while a winning merge retains evidence without dispatch. Covered by Git and outbox tests. |
| Resume old dirty-work and capped-retry checkpoints | The decoder migrates M2 `Awaiting_session` into owned reinspection, preserving the operation and deferred intent. Legacy failure counts conservatively seed both new failure budgets. Properties cover repeated decoding, deferred retargets, stopped operations and waiting operations at the old cap. |
| Stop infinite retries without successful inspections replenishing the budget | The five-failure terminal stop is intentionally superseded by M3's bounded deterministic attempts followed by agent recovery/diagnosis. Separate durable mutation, evidence and completed-agent counters preserve the liveness guarantee; intervention requires exhausted agent recovery, not a saturated backoff exponent. |
| Disable local hooks only for Onton-owned Git commands | #486's `Git_env.clean_env` override and environment-scrubbing tests are retained unchanged. Real Git tests verify generated commits, merge continuation, protected-root integration and local pre-push bypass; ordinary Git retains configured hooks. Remote URL probes continue through the supervised clean Git environment. |
| Keep the two-minute Git timeout and name the timed-out command | The production timeout and command diagnostic are retained. Observation, fetch and push timeout/cancellation/retry tests use blocking transports; publication-race fixtures inject server transports explicitly because M3 binds a destination URL. Independent remote hook fixtures explicitly configure their server-side hooks. |
| Refresh empty or nonterminal descendant CI on every configured polling cycle | The #486 planner, terminal-check predicate, poller call and polling properties are retained. Pending/mixed/empty observations bypass both throttles; cancelled/stale and other terminal checks retain the normal cache cadence. |

The separate M2 session queue and its PR-dependent dispatch rules are superseded;
reintroducing them would recreate a second owner of the same Git work. The fresh
base observation and automerge guards were genuine gaps found by retaining #486's
behavioral tests and are fixed in this rebase.

Validation on the rebased implementation passes `dune build`, full `dune runtest`,
`dune build @fmt @check`, the source-policy gate, `git diff --check`, and the pinned
archlint OCaml adapter. Logs: `/tmp/onton-m3-rebase-runtest-final.log` and
`/tmp/onton-m3-rebase-architecture.log`. The focused timeout suite also passes
observation, fetch and push cancellation/retry; the focused real-Git/checkpoint
and dirty-recovery suites pass. The earlier isolated-prefix results retain their
recorded pre-rebase baseline and are not claimed as new prefix qualification.
