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

M1 is complete and M2 is qualified as a release candidate in this change set.
The release has not been published. The [M2 release evidence](branch-reconciliation-m2-release-evidence.md)
records the acceptance map, passing checks and operator handoff; the
[M2 audit](branch-reconciliation-m2-audit.md) records closure against the six
milestone requirements.

The checkout contains the pure owner, typed executor, durable snapshot-v2
checkpoints, exact materialization receipts, session completion receipts,
explicit-lease publication, destination-bound recovery, staged content repair,
verified agent history recovery, and destination-root integration. All enabled
branch mutations use the owned checkpoint runner. Legacy scheduling detectors
and compatibility views remain for M3 cleanup.

The final source gate passed with 54 reconciliation properties and real Git
acceptance. SourceHut's actual Git observation handler was exercised with injected
build results; installed Simgit lifecycle and old-binary v2 rejection were checked
separately. These are local acceptance results, not live-provider rollout evidence.
Full PR/request-scoped forge observations, the independent abstract model,
exhaustive scenario matrix and formal endpoint specification remain M3 work.

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
| A03 — Initial boundary survives base movement. | `cmd`: create checkout, move/fetch base before agent work, inspect materialization snapshot and private refs. | Recorded boundary is the actual starting commit. | M1 — `lib/worktree_setup.ml`, `lib_core/worktree_plan.ml` |
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
