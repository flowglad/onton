---
title: Distributed Onton execution in the cloud
status: draft
updated_at: 2026-10-22
---

# Workstream: Distributed Onton execution in the cloud

## Vision

Run one gameplan across multiple containers managed by Onton in the Cloud (OITC),
with a headless Onton coordinator deciding which patches may execute. OITC owns
container placement, connectivity, credentials, and durable storage. If a patch
container disappears, replace it automatically with a fresh checkout and agent
conversation, using the branch on origin and current forge observations to
recover context and continue the outstanding work.

The user has selected cloud-only deployment, headless operation, automatic worker
replacement, and origin-based recovery without restoring agent sessions. The
milestone sequence and implementation boundaries below are a review draft.

## Current State

Verified against the Onton and sibling `provisioning-agent` checkouts on
2026-10-22. These are source observations, not claims about deployed versions.

- `lib/orchestrator.mli` exposes durable patch messages with stable identities
  and Pending/Acked/Completed/Obsolete states. `lib/runner_fiber_impl.ml` executes
  them in local fibers behind a process-local concurrency semaphore.
- `lib/session_driver.ml` owns provisioning, agent conversation lifecycle,
  transcript handling, and publication. Its backend and worktree interfaces use
  local filesystem capabilities. `lib/runtime.mli` supplies process-local patch
  and integration-root write ownership.
- `lib_core/branch_reconcile.ml`, `lib/branch_reconcile_runner.ml`, and
  `lib/branch_reconcile_executor.ml` already own captured Git operations,
  checkpoint-before-execution, publication receipts, direct remote observation,
  and repair. Their retained revisions and recovery refs can include objects
  available only in the local clone. A fresh clone cannot simply resume every
  pending local command from that checkpoint.
- `lib_core/execution_mode.ml` and the runner support feature construction:
  descendant branches publish independently, and integration into the root is
  serialized. Descendant publication does not imply readiness; applicable checks
  and revision evidence still govern integration.
- In `provisioning-agent/tools/issue-runner/main.go`, `launch` starts one Fargate
  task per source session, and the worker starts one headless Onton process.
  Multi-patch gameplans select feature-branch mode. The current image and task
  definition provide the coding environment; container provisioning is not new
  work for this project.
- OITC already has conditional DynamoDB claims, idempotent ECS launch requests,
  transactional session transitions, authorized control delivery, CI/review
  wakeups, transcript storage, and cancellation that observes task termination.
  See `tools/issue-runner/{main,sessions,coordination_store,supervisor}.go` and
  `tools/issue-runner/internal/coordination/` in that repository.
- Existing OITC archive recovery stops Onton before saving the workspace,
  worktrees, and conversation files. `checkpoint.go` validates restores. That
  whole-session archive is useful for the existing mode, but is not the new
  distributed recovery contract. Expired running claims currently become
  operator attention rather than automatic fresh-worker recovery.
- No patch-level remote execution interface was found in the inspected runner,
  backend, worktree, or OITC execution paths. Existing local control sockets carry
  user controls, not distributed patch execution.

## Key Challenges

- **Separate durable intent from disposable execution.** Origin is the code
  checkpoint. The gameplan, user guidance, accepted controls, outstanding logical
  operations, execution attempts, and coordinator state still need durable
  storage. A branch alone does not encode them.
- **Recover without stale local authority.** Worker replacement must explicitly
  invalidate checkout paths, conversation handles, local-only candidate claims,
  and pending commands that belong to the lost workspace. Preserve outstanding
  intent and re-observe the remote through the existing reconciliation owner;
  do not wait forever for an unavailable local object or declare it published.
- **Prevent two writers during replacement.** A missed heartbeat is not proof of
  termination. The platform must stop and confirm the previous task before a
  replacement receives execution authority. Attempt identities also reject late
  messages. Git expected-head checks protect publication races but are not a
  substitute for worker ownership. Already-issued remote effects may need
  reconciliation even after the old task has stopped.
- **Keep existing scheduling semantics across separate clones.** Dependencies,
  descendant checks, root integration, and publication must consume confirmed
  remote revisions. No coordinator path may assume another worker's checkout,
  notes file, or recovery refs are locally accessible.
- **Resume safely after ambiguous results.** A push or PR operation can succeed
  before its acknowledgement is lost. Recovery must inspect actual state before
  deciding what remains. A fresh agent receives uncertainty explicitly; it does
  not receive invented success, test results, or conversation history.
- **Bound operational churn.** Repeated infrastructure failures need backoff and
  eventual attention. Placement limits, task startup overhead, transcript
  attribution, and aggregate sleep/wake behavior need cloud qualification.

## Established Precedents

- **documentation — Amazon ECS RunTask idempotency and eventual consistency** —
  <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html>
  OITC already uses `ClientToken`. Extend that approach to explicit attempts:
  retries of one launch retain their identity, and uncertain responses are
  reconciled through task observation before authorizing another launch.
- **documentation — Amazon ECS task lifecycle** —
  <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle-explanation.html>
  Replacement and cancellation distinguish desired stop from observed `STOPPED`.
  This is the platform evidence used to retire a previous writer.
- **documentation — DynamoDB transactions and conditional writes** —
  <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/transaction-apis.html>
  Reuse OITC's guarded transition approach for assignment, coordinator ownership,
  and durable result acknowledgement. Application operation IDs survive beyond
  service-level request deduplication windows.
- **documentation — Git push with an explicit expected revision** —
  <https://git-scm.com/docs/git-push>
  Keep publication in `Branch_reconcile` with its captured destination and
  expected revision. Preserve the root's ancestry policy. These checks protect
  remote history; they do not authorize an expired execution attempt.

## Milestones

### Milestone 1: headless-patch-execution-contract

**Repository**: Onton. **Dependencies**: none.

**Status**: Planned as of 2026-10-22; not implemented or released.
Artifact: planned at `gameplans/headless-patch-execution-contract.yaml`
(six patches proposed; authoring the gameplan is outside this workstream PR).
The following names describe the planned implementation, not landed APIs.

**Definition of Done**:

- Define protocol v1 in `lib_core/worker_protocol.ml` and
  `docs/design/headless-worker-protocol.md`: bounded JSON Lines over stdin/stdout,
  with stderr reserved for diagnostics. Bind requests, checkpoints, results and
  cancellation to run, patch, logical operation, attempt and sequence/version.
- Add explicit `onton-worker` (`bin/worker.ml`), reusable for sequential operations
  on one run/patch. Execute patch implementation/feedback and patch-local
  reconciliation/publication; reject unsupported operations. Distributed scheduling,
  root integration, scripted gameplan publication and general forge-management
  dispatch remain outside M1. The host supplies transport, credentials and authority;
  Onton adds no AWS client or container lifecycle implementation.
- The assigned worker alone advances reconciliation state. `Worker_checkpoint`
  exports a versioned patch-execution capsule, not mutable whole-gameplan state.
  `Execution_checkpoint` and checkpoint-aware setup/session functors await exact
  host acknowledgement before dependent effects, including nested setup, repair,
  session identity, guidance consumption and terminal paths. Next assignment
  requires acknowledged terminal result and completed process cleanup. Existing
  local `Make` wrappers retain their success and error semantics.
- Host EOF, broken output or failed/timed-out checkpoint acknowledgement cancels
  execution and terminates after bounded cleanup. The host must still confirm
  predecessor termination before replacement; a missed ACK may hide a committed
  newer capsule and already-issued remote effects must be observed.
- Implement explicit lost-workspace recovery through `Branch_reconcile`,
  `Worktree_setup`, and `Session_driver`. With a reachable branch, adopt origin's
  observed revision; before first publication, use the currently valid base.
  A failed remote probe is not an absent branch. Patch-head observation retains
  the bound push destination; base observation uses origin's fetch destination.
  A possibly published branch that disappears requires fresh forge reconciliation:
  report confirmed merge without backend work, otherwise return retryable evidence
  failure or explicit attention. Recreation requires a new explicitly renewed
  instruction referencing the durable attention receipt (`renewal_id`), followed
  by fresh branch/base/forge observation. Ordinary retry or replacement is not
  renewal; publication history remains intact.
- Render a fresh recovery prompt containing the patch requirements, outstanding
  operation and guidance, observed branch/base revisions, and the fact that local
  edits and the prior conversation may be lost. The agent must inspect remaining
  work and validate it. Local-only progress may be recomputed.
- Extend the owner properties and real-Git acceptance tests for workspace loss,
  including a lost push acknowledgement, a missing local candidate, and an
  interrupted rebase. Destroy the old clone/object database in recovery tests,
  not merely a worktree directory. Typed recovery results preserve retryable,
  attention and merged observations through the worker wire response. Existing
  local operation remains functional.
- Qualify the actual executable with `dune build @worker-protocol-smoke` and
  `test/test_worker_protocol.ml`, including a binary override for image testing.
  Cover sequential reuse, stale/duplicate messages, ACK loss/rejection, cancellation,
  all recovery outcomes and explicit renewal versus ordinary retries.

**Why this is a safe pause point**: The existing runner remains usable. The new
worker can be exercised directly through a local harness before cloud admission.

**Unlocks**: A concrete contract that the Go cloud wrapper can implement.

**Operator Actions Before Next Milestone**:

- Publish an Onton release containing the qualified protocol and worker mode.
- Run the `worker-protocol-smoke` harness using its explicit binary override for
  the released `onton-worker` in the OITC image; record binary/image identities
  and the result. Follow `docs/design/headless-worker-protocol.md` for the concrete
  invocation once implemented. Do not advance if shipped and tested protocols differ.

### Milestone 2: cloud-execution-attempt-adapter

**Repository**: provisioning-agent. **Dependencies**: M1 and its release gate.

**Definition of Done**:

- Extend `tools/issue-runner/` and `infra/issue-runner.ts` to host the new protocol,
  launch assigned workers, and durably retain coordinator state and execution
  records. OITC owns storage and delivery; Onton owns the semantic state payload.
- Reuse the image, private networking, credential renewal, logs, and existing
  coordination facilities. Scope claims and records to run/patch/attempt rather
  than treating each patch as an independent GitHub issue session.
- Make launch retries idempotent and retain ambiguous launches for observation.
  A replacement is admitted only after its predecessor is confirmed stopped;
  an unavailable observation causes bounded retry and then explicit attention.
  Do not infer death solely from an expired heartbeat or missing API response.
- Support conditional coordinator ownership and state versions, acknowledged
  result delivery, attempt-scoped cancellation, and transcript attribution.
  Stale tasks cannot replace durable state or discharge a newer operation.
- A contract fixture can start a worker, terminate it, and request a fresh
  replacement without restoring a workspace archive. The normal issue-start
  route continues using the established execution mode at this pause point.

**Why this is a safe pause point**: The adapter is available for controlled
qualification; existing issue sessions retain their existing route.

**Unlocks**: Real cloud attempts behind the platform-neutral Onton contract.

**Operator Actions Before Next Milestone**:

- Deploy the adapter and scoped infrastructure through the OITC release path.
- Run the contract fixture in an isolated test repository. Interrupt launch
  acknowledgement and worker connectivity separately, then request replacement.
  Record task identities and prove no successor starts while its predecessor
  remains live. Proceed only when state versions and result deduplication hold.

### Milestone 3: distributed-gameplan-coordinator

**Repository**: Onton. **Dependencies**: M2 and its cloud contract gate.

**Definition of Done**:

- Add opt-in headless coordinator execution through the host protocol, retaining
  existing dependency and eligibility decisions. Durable outbox messages become
  execution requests with stable operation identity; attempts are subordinate
  to those operations, not new user requests.
- Persist coordinator transitions through the acknowledged host storage boundary
  before dispatch or acceptance that relies on them. On coordinator replacement,
  reconcile existing attempts and pending results before dispatching new work.
- Run eligible patches in separate workers under a gameplan-wide concurrency
  limit. Loss of one worker leaves unrelated eligible work runnable. Automatic
  recovery uses the M1 origin-based path, with bounded retries and diagnostics.
- Preserve both ordinary branch/PR behavior and the feature-branch mode used by
  OITC multi-patch runs. Fetch published contributor revisions into the root
  executor and serialize root mutations through the existing owner. Transfer or
  regenerate required patch notes; do not read another container's local files.
- Keep revision-specific checks and forge evidence gates. Duplicate or stale
  results cannot complete another attempt, unlock a dependent, or integrate a
  contribution twice. Worker stdout and successful agent exit are not durable
  publication evidence.
- Emit aggregate headless state for execution, recovery, pending work, and safe
  sleeping. Fresh recovery uses current guidance and controls, not a frozen
  prompt from before the interruption.
- Qualify local multi-process execution with real Git, an independent operation
  model, and fault injection for coordinator death, worker death, duplicate
  messages, lost push acknowledgements, and root integration interruption.

**Why this is a safe pause point**: Distributed orchestration is opt-in and
qualified against the contract; the cloud's ordinary issue route has not changed.

**Unlocks**: A complete distributed gameplan engine for OITC to expose.

**Operator Actions Before Next Milestone**:

- Release and pin the new Onton binary in a qualification image.
- Run a diamond dependency gameplan through the M2 adapter, with independent
  siblings on distinct tasks and a feature root. Kill a sibling, then separately
  kill the coordinator. Proceed only after each run completes with the expected
  repository content, current checks, and no duplicate integration.

### Milestone 4: oitc-distributed-run-lifecycle

**Repository**: provisioning-agent. **Dependencies**: M3 and its release gate.

**Definition of Done**:

- Expose distributed execution as an opt-in OITC run mode with one cloud
  coordinator and per-patch execution capacity. Retain source-session identity,
  run configuration, plan, accepted guidance, and controls across replacements.
- Connect automatic task-loss detection and attempt recovery to the coordinator.
  Restart a lost coordinator from durable control state; do not restore patch
  workspaces or coding-agent sessions to recover distributed attempts.
- Route authorized start/cancel/automerge controls and existing CI/review wakes
  to the owning run. Cancellation retires all active and ambiguous child launches
  before cleanup completes; late events cannot resurrect the cancelled run.
- Sleep only when the aggregate run has no executable or in-flight obligations.
  Persist the control state before stopping compute. Wake resumes the run from
  durable control state plus current origin/forge observations. Preserve the
  existing wake-counter protection during the sleep transition.
- Present logs and transcripts by run, patch, and attempt through existing
  headless telemetry and the OITC operational surface. Show replacement reasons,
  failed recovery, capacity waits, and aggregate run status.
- Add an executable distributed acceptance driver under `tools/issue-runner/`
  covering the terminal suite below. Record operator evidence, pinned versions,
  task timings, resource use, and rollout/rollback instructions.

**Why this is a safe pause point**: The complete mode is available by explicit
selection and has a qualification record. Existing single-task sessions can
finish under their original mode and release.

**Unlocks**: Routine cloud distribution of one gameplan across multiple workers.

**Operational acceptance**: Run the terminal suite in an isolated repository,
then observe representative opt-in runs for at least 24 hours, including a
CI/review sleep/wake cycle. Stop expansion for overlapping writers, lost accepted
guidance, incorrect root content, duplicate PRs, or unrecoverable control-state
loss. Measure task startup time and wasted recomputation before setting default
worker sizing and concurrency. Rollback disables new distributed admission and
drains or explicitly cancels active distributed runs; it must not relaunch the
same active branches through the old runner. No default-on cutover is implied.

## Dependency Graph

```text
1 (headless-patch-execution-contract) -> []
2 (cloud-execution-attempt-adapter) -> [1]
3 (distributed-gameplan-coordinator) -> [2]
4 (oitc-distributed-run-lifecycle) -> [3]
```

The release and observation gates above are the reasons for separate gameplans.
Each milestone changes one repository and leaves a usable system. A qualification
failure is resolved before the dependent gameplan starts; it is not hidden as a
conditional patch inside that gameplan.

## Open Questions

| Question | Current position | Resolve by |
| --- | --- | --- |
| Loss detection, backoff, and retry limits | Automatic bounded recovery; unavailable task-stop evidence must become attention rather than permit a second writer. | M2/M3 gameplans |
| Worker sizing and cold-start overhead | Start with explicit bounded capacity; measure with the maintained image rather than multiplying today's whole-gameplan resources blindly. | M4 qualification |
| Final default-on policy | Opt-in delivery is sufficient for this workstream. | Separate product decision |

## Decisions Made

| Decision | Rationale and status |
| --- | --- |
| Cloud-only and headless | Confirmed by the user; no local coordinator deployment or distributed TUI is required. |
| OITC owns infrastructure | User-directed reuse of provisioning-agent for networking and containers; AWS-specific coordination adapters stay there. |
| Automatic replacement | Confirmed by the user; a lost worker must not require an operator to restore its archive. |
| Origin is the code recovery point | Confirmed by the user. Unpushed commits, edits, paused sequencers, and conversation progress may be lost. There is no time-bounded recovery-point guarantee or requirement to publish partial work periodically. |
| Fresh recovery conversation | Confirmed by the user. The new agent is told it is recovering, receives current task context, and inspects and validates the remaining work. |
| Durable control state remains necessary | Design consequence: branch contents cannot recover accepted user controls, pending operations, plan identity, or ownership. OITC stores these separately from disposable workspaces. |
| Single patch execution owner | Approved in M1 planning: worker alone advances its patch reconciliation state; coordinator receives projections. OITC stores exact capsules and acknowledges authoritative checkpoints. Root integration ownership remains M3 work. |
| Reusable run/patch worker | Approved in M1 planning: one process accepts successive operations with distinct attempt identities only after terminal ACK and cleanup. No concurrent operations or cross-patch reassignment. |
| Host protocol | Selected under engineer delegation in M1: dedicated `onton-worker`, JSON Lines on stdin/stdout, diagnostics on stderr. The host owns cloud transport/authentication; local IDs fence messages but do not prove task death. |
| Patch capsules and ACK barriers | Approved in M1: versioned patch-scoped payloads, not mutable whole-gameplan snapshots. Host ACK precedes dependent effects; session sidecars cannot supersede host state. M2 must condition storage writes on version and current authority. |
| Host disconnection | Approved in M1: cancel and terminate after bounded cleanup; no reconnect protocol. Readmission uses actual host-committed state and re-observes ambiguous effects. |
| Remote disappearance and renewed intent | Approved in M1: confirmed never-published absence can use current base. Possibly published disappearance requires forge reconciliation; confirmed merge is reported, otherwise explicit renewed instruction referencing the attention receipt is needed to recreate. Ordinary retries cannot recreate; renewal preserves publication history and rechecks remote/forge evidence. |
| Feature construction is in scope | The inspected OITC multi-patch path uses it today; supporting only independent PR branches would miss the actual target workflow. |
| Reuse branch reconciliation | Required repository architecture. No alternate remote-only rebase or publication path. Workspace loss becomes an explicit owner transition. |
| Recover effects by observation | A repeated execution may re-run tests or agent work. The system does not promise exactly-once arbitrary shell/API side effects; it must reconcile owned Git, PR, and operation state before repeating them. |
| Planning status | M1 proposes a future six-patch gameplan; M2–M4 remain milestone sketches. No finalized gameplan, implementation, release, image qualification or deployment is claimed. |

## Definition of Done (Acceptance Suite)

Each scenario is independent and runs against isolated repositories and run IDs.
The M4 driver invokes the public headless protocol, OITC admission, task APIs,
Git, forge APIs, and durable state observations. Failure injection acts at those
boundaries. Source text is never the behavioral oracle. Proposed fixture command
names are finalized in the owning gameplans; owner files below identify the
implementation boundary. Each assertion has one owning milestone. M1 names are
now planned: `dune build @worker-protocol-smoke` drives `onton-worker` through
`test/test_worker_protocol.ml`; focused recovery fixtures are
`test/test_worker_recovery_git.ml` and `test/test_worker_recovery_prompt.ml`.
These fixtures are not implemented yet; no acceptance run is claimed.

- **A01 — Fresh recovery starts from published code**
  - **Assert**: Replacing a worker without its workspace starts from the observed origin patch revision.
  - **Verify by** `cmd`: In `test_worker_recovery_git` and the public `worker-protocol-smoke`, push a marker, add an unpushed edit, destroy the old clone/object database, and admit `lost_workspace` recovery through `onton-worker`.
  - **Expected**: Before new edits, HEAD matches the fetched patch revision and contains the pushed marker. Recovery requires no previous session files; the unpushed edit is not promised.
  - **Traces to**: M1 — `lib/worktree_setup.ml`, `lib/session_driver.ml`, and the recovery transition in `lib_core/branch_reconcile.ml`.
- **A02 — Recovery is explicit to the fresh agent**
  - **Assert**: The first replacement conversation identifies recovery and the outstanding patch work.
  - **Verify by** `cmd`: `test_worker_recovery_prompt` and `worker-protocol-smoke` capture the actual `Llm_backend.run_streaming` prompt and `resume_session=None` after `Session_driver.run_recovery_owned`.
  - **Expected**: Fresh invocation with recovery notice, patch requirements, current guidance, observed revisions, and instruction to inspect and validate remaining work; no claim of restored conversation or unobserved success.
  - **Traces to**: M1 — `lib/session_driver.ml` and `lib_core/session_prompt.ml`.
- **A03 — Recovery before first publication works**
  - **Assert**: A patch whose first worker died before creating its remote branch can resume automatically.
  - **Verify by** `cmd`: `test_worker_recovery_git` and `worker-protocol-smoke` admit a patch, interrupt before any possible push, destroy its clone, and request fresh recovery against the current observed base.
  - **Expected**: The replacement uses a confirmed valid base and proceeds with the outstanding patch rather than waiting for an old local candidate.
  - **Traces to**: M1 — `lib/worktree_setup.ml` and `lib_core/branch_reconcile.ml`.
- **A04 — Unavailable origin is not treated as an empty branch**
  - **Assert**: A failed remote observation cannot authorize recreating a supposedly absent patch branch.
  - **Verify by** `cmd`: `test_worker_recovery_git` and `worker-protocol-smoke` fail or corrupt remote reads, then restore them; inspect emitted retryable results and actual Git effects.
  - **Expected**: No branch-recreation mutation during the outage; retry or explicit attention is visible, and subsequent recovery uses the actual branch.
  - **Traces to**: M1 — `lib/branch_reconcile_executor.ml` and `lib_core/branch_reconcile.ml`.
- **A05 — Lost local reconciliation state does not deadlock recovery**
  - **Assert**: A pending operation referencing a lost local candidate or sequencer can reach a fresh repair turn.
  - **Verify by** `cmd`: `test_worker_recovery_git` and `worker-protocol-smoke` interrupt a conflicting rebase or lose a successful push acknowledgement, destroy the old clone, retain logical intent, and request owner `Workspace_lost` recovery.
  - **Expected**: A fresh recovery turn runs from available remote history; no command waits indefinitely for the missing checkout or claims the lost candidate was published.
  - **Traces to**: M1 — `lib_core/branch_reconcile.ml` and `lib/branch_reconcile_runner.ml`.
- **A20 — Worker acknowledgement and sequential reuse are fenced**
  - **Assert**: The public worker cannot execute dependent effects before exact host ACK or accept overlapping operations.
  - **Verify by** `cmd`: `worker-protocol-smoke` delays/rejects ACKs, replays stale messages, runs two sequential operations, cancels and disconnects the host while checkpointing.
  - **Expected**: No premature effect or stale completion; next assignment waits for terminal ACK and cleanup. Host loss terminates within the documented bound. These local checks do not certify cloud task exclusivity.
  - **Traces to**: M1 — `lib_core/worker_protocol.ml`, `lib/worker_checkpoint.ml`, `lib/worker.ml`, `test/test_execution_checkpoint.ml` and `test/test_worker_protocol.ml`.
- **A21 — Disappeared publication requires explicit renewed intent**
  - **Assert**: Ordinary retries cannot recreate a possibly published deleted branch; a matching explicit renewed instruction can authorize recovery after fresh evidence.
  - **Verify by** `cmd`: Recovery fixtures and `worker-protocol-smoke` delete a published branch, test merged/nonmerged/unavailable forge outcomes, then submit plain retry, stale renewal and a matching new-operation `renewal_id`.
  - **Expected**: Distinct typed retryable/attention/merged results, no backend for those outcomes, and no implicit recreation. Valid renewal with confirmed nonmerged history can start from the confirmed base without erasing prior publication history; merge or branch reappearance takes precedence.
  - **Traces to**: M1 — owner `Renew_workspace_intent`, `Worktree_setup.recover_owned`, `Session_driver.run_recovery_owned` and the worker protocol harness.
- **A06 — Ambiguous launches do not duplicate a worker**
  - **Assert**: Retrying an accepted launch whose response was lost yields one active task for that attempt.
  - **Verify by** `api`: Drop the first successful launch response and redeliver the identical execution request; observe ECS and OITC attempt records.
  - **Expected**: One associated task identity and one active worker for the attempt.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/main.go` and execution records in `sessions.go`.
- **A07 — Replacement waits for predecessor termination**
  - **Assert**: A replacement cannot execute while its predecessor remains live or termination is unconfirmed.
  - **Verify by** `api`: Partition worker communication, request replacement, and hold task observation at RUNNING/STOPPING before allowing STOPPED.
  - **Expected**: No successor execution before confirmed termination; afterwards a new attempt may execute. Unavailable evidence produces retry/attention, not concurrent writers.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/internal/coordination/` and ECS adapter in `main.go`.
- **A08 — Durable state has one current writer**
  - **Assert**: A stale coordinator or worker attempt cannot overwrite a newer state version.
  - **Verify by** `api` + `db`: Retain an old authenticated request, advance ownership, then replay its state write and result acknowledgement.
  - **Expected**: The stale request is rejected and current state/version remains unchanged.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/coordination_store.go`.
- **A09 — Independent patches execute on separate tasks**
  - **Assert**: Two eligible independent patches can run concurrently on distinct workers.
  - **Verify by** `cmd`: Submit a diamond gameplan with capacity two and hold both sibling agents at a runtime barrier.
  - **Expected**: Two distinct worker identities are concurrently executing the siblings; the dependent remains ineligible until existing dependency conditions hold.
  - **Traces to**: M3 — `lib/runner_fiber_impl.ml` and `lib/orchestrator.ml`.
- **A10 — Capacity remains bounded during retries**
  - **Assert**: Active authorized patch executions never exceed the configured gameplan limit.
  - **Verify by** `cmd`: Run five eligible patches at capacity two while duplicating dispatch messages and replacing one worker.
  - **Expected**: At most two active authorized patch executions at every observed transition; waiting replacement does not create an extra writer.
  - **Traces to**: M3 — distributed dispatch ownership in `lib/runner_fiber_impl.ml` and `lib/orchestrator.ml`.
- **A11 — An old completion cannot complete a new attempt**
  - **Assert**: Delayed or duplicate attempt results cannot advance a replacement's logical state.
  - **Verify by** `cmd`: Start a replacement, deliver its predecessor's success result, and inspect public run state and dependent dispatches.
  - **Expected**: No replacement completion or dependent release is caused by the stale result.
  - **Traces to**: M3 — message/result acceptance in `lib/orchestrator.ml`.
- **A12a — Feature content survives interrupted integration**
  - **Assert**: Distributed feature construction preserves each accepted contribution through an interrupted root integration.
  - **Verify by** `cmd`: Publish two independent content fixtures, interrupt root integration after a remote write but before acknowledgement, then recover from origin.
  - **Expected**: The final root contains the expected content of both fixtures.
  - **Traces to**: M3 — integration orchestration in `lib/runner_fiber_impl.ml` through `Branch_reconcile_runner`.
- **A12b — Integration recovery does not duplicate contribution commits**
  - **Assert**: Recovery does not integrate an already published contribution a second time.
  - **Verify by** `cmd`: Run the A12a interruption scenario and inspect the recovered root's commit history for each fixture contribution.
  - **Expected**: Each fixture contribution has exactly one integration into root history.
  - **Traces to**: M3 — integration orchestration in `lib/runner_fiber_impl.ml` through `Branch_reconcile_runner`.
- **A12c — Integration recovery preserves root history**
  - **Assert**: Recovery retains the root history that preceded integration.
  - **Verify by** `cmd`: Record the root tip before the A12a interruption scenario, then check its ancestry against the recovered root tip.
  - **Expected**: The recorded pre-integration root tip remains an ancestor of the recovered root tip.
  - **Traces to**: M3 — integration orchestration in `lib/runner_fiber_impl.ml` through `Branch_reconcile_runner`.
- **A12d — Recovered integration requires current revision checks**
  - **Assert**: Recovering a published integration does not bypass revision-specific readiness checks.
  - **Verify by** `cmd`: Run the A12a interruption scenario with passing checks only for the old revision; withhold required checks for the recovered revision, then supply passing checks for that revision while other readiness conditions hold.
  - **Expected**: Readiness remains blocked until the recovered revision's required checks pass.
  - **Traces to**: M3 — integration orchestration in `lib/runner_fiber_impl.ml` through `Branch_reconcile_runner`.
- **A13 — Coordinator replacement preserves pending intent**
  - **Assert**: A new coordinator resumes durable operations without redispatching live accepted work.
  - **Verify by** `cmd`: Kill the coordinator after dispatch acceptance, retain a live worker and a pending human instruction, then restart through the host protocol.
  - **Expected**: The same logical operation is reconciled, its accepted guidance remains pending/delivered as appropriate, and no second active execution is created.
  - **Traces to**: M3 — `lib/runtime.ml`, `lib/persistence.ml`, and `lib/orchestrator.ml` host persistence integration.
- **A14 — A lost push acknowledgement preserves published work**
  - **Assert**: Recovery recognizes an already successful push rather than reporting lost code or blindly recreating it.
  - **Verify by** `cmd`: Accept a push at origin, suppress its result, remove the worker, and run the replacement.
  - **Expected**: Recovery observes the published revision and continues the outstanding intent without duplicating its contribution or creating a second owned PR.
  - **Traces to**: M3 — distributed result/recovery wiring in `lib/runner_fiber_impl.ml` and `lib/session_driver.ml`.
- **A15 — Patch loss recovers automatically in OITC**
  - **Assert**: Stopping one active patch task causes automatic fresh recovery while independent work can continue.
  - **Verify by** `api` + `cmd`: Stop a patch task in an opt-in cloud gameplan without a manual restart command; observe tasks, transcripts, and final branches.
  - **Expected**: A replacement attempt starts after stop confirmation, emits the recovery context, and completes from origin. No workspace archive is required; an independent patch can progress.
  - **Traces to**: M4 — `provisioning-agent/tools/issue-runner/supervisor.go` and distributed run routing in `main.go`.
- **A16 — Sleep/wake retains the distributed run**
  - **Assert**: A quiescent run can stop compute and resume on an authorized CI/review wake without losing pending controls.
  - **Verify by** `api` + `db`: Let a test run enter sleeping state, deliver a wake and an automerge control across the sleep transition, and inspect the resumed run.
  - **Expected**: The same run and patch identities resume, the accepted control is applied, and the wake is not lost or duplicated into another run.
  - **Traces to**: M4 — `provisioning-agent/tools/issue-runner/sessions.go`, `onton_control.go`, and `internal/coordination/`.
- **A17 — Cancellation retires the entire run**
  - **Assert**: Accepted source-session cancellation prevents further distributed execution after cleanup completes.
  - **Verify by** `api`: Cancel with active and launch-pending children; delay one launch response, then replay old wakes and completions.
  - **Expected**: All associated tasks are retired before completion is recorded, and late events cannot restart the cancelled run.
  - **Traces to**: M4 — distributed cancellation in `provisioning-agent/tools/issue-runner/`.
- **A18 — Recovery failures are bounded and attributable**
  - **Assert**: Repeated failed replacements eventually reach explicit attention rather than launching forever.
  - **Verify by** `api` + `cmd`: Repeatedly fail replacement startup beyond the configured budget; inspect run state and telemetry.
  - **Expected**: The run identifies the patch, attempt history, reason, and attention state; unrelated patches retain their own state. Logs/transcripts identify their run, patch, and attempt.
  - **Traces to**: M4 — `provisioning-agent/tools/issue-runner/supervisor.go` and `transcript_logs.go`.
- **A19 — Existing execution stays usable**
  - **Assert**: An ordinary OITC start without distributed selection still completes through the established mode.
  - **Verify by** `api` + `cmd`: Start an independent single-task fixture after the distributed release and exercise its existing sleep/restore path.
  - **Expected**: Its normal branch/PR result and archive restore succeed; no distributed attempt is admitted for that run.
  - **Traces to**: M4 — mode selection and compatibility in `provisioning-agent/tools/issue-runner/main.go`.
