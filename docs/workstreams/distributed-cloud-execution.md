---
title: Distributed Onton execution in the cloud
status: draft
updated_at: 2026-10-23
---

# Workstream: Distributed Onton execution in the cloud

## Vision

Run one gameplan across multiple containers managed by Onton in the Cloud (OITC),
with one active headless Onton coordinator owning scheduling and communication
with its patch workers, whether they run on the same box or different boxes.
OITC provides container placement, connectivity, credentials, and durable storage;
it does not send or interpret Onton patch commands. If a patch container disappears,
Onton requests replacement resources and recovers with a fresh checkout and agent
conversation, using origin and current forge observations to continue the work.

The user has selected cloud-only deployment, headless operation, automatic worker
replacement, and origin-based recovery without restoring agent sessions. The
milestone sequence and implementation boundaries below track delivery. M1 passed
local qualification; the engineer confirmed completion of its release and exact-binary
OITC image qualification on 2026-10-23. That release-gate status is engineer-reported,
not independently verified here.

## Responsibility Boundary

| Concern | Onton owns | OITC provides |
| --- | --- | --- |
| Patch execution | One active coordinator schedules work, sends commands, accepts results, and communicates with local or remote workers. Workers retain patch-local reconciliation ownership. | Requested compute and authenticated connectivity; any transport relay carries bytes without interpreting Onton commands. |
| Durability | Coordinator state, admission receipts, exact checkpoint capsules, protocol versions, replay rules, and checkpoint/result acknowledgements. | Opaque durable storage with conditional writes, scoped access, ownership fencing, and confirmed storage outcomes. A storage success is not a worker protocol ACK. |
| Replacement | Decisions to retry or recover patch work, and restoration of protocol state before dispatch. | Idempotent resource launches, stop requests, observed task status, and positive predecessor-termination evidence. |
| Run lifecycle | Aggregate execution/sleep readiness and interpretation of accepted user guidance and controls. | Authorized source-session admission/control delivery, coordinator process hosting/restart, wake routing, and resource cleanup. |
| Telemetry | Semantic execution/recovery events and transcript production. | Collection, retention, and attribution to the supplied run/patch/resource identities. |

The M1 protocol's **host** is the Onton-side protocol peer: the coordinator in
M3, or a test harness during qualification. It is not an OITC implementation of
the worker state machine. Onton waits for durable storage confirmation before
sending the required protocol ACK. OITC enforces resource and storage authority,
not patch-operation semantics; a resource identity must not be confused with a
protocol operation/attempt identity.

This boundary supersedes the exploratory proposal for an OITC command/receipt
API and a separate Onton receipt-helper prerequisite. Neither is required by
this workstream. M3 can reuse Onton's existing receipt-transition logic directly.
M2 planning selected a local Go resource helper beside the coordinator, an opaque
server-TLS/bearer-authenticated stream, and immutable S3 state published through
conditional DynamoDB pointers. No new network control API service is required.
Onton's integration with these capabilities remains M3 work.

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
- M2 planning rechecked provisioning-agent at
  `00bb41c3851f7bbe65b99f709b6107cc7dc34eb2`: `startWork` releases failed launch
  claims, and `stopSessionTask` treats empty/missing task observations as stopped.
  Those legacy paths do not establish the new resource exclusivity contract;
  M2 adds separate resource adapters rather than silently changing issue sessions.
- Existing OITC archive recovery stops Onton before saving the workspace,
  worktrees, and conversation files. `checkpoint.go` validates restores. That
  whole-session archive is useful for the existing mode, but is not the new
  distributed recovery contract. Expired running claims currently become
  operator attention rather than automatic fresh-worker recovery.
- Onton now exposes the opt-in `onton-worker` patch execution interface described
  in `docs/design/headless-worker-protocol.md` (protocol v2).
  `lib_core/worker_protocol.mli` exposes `step` and `admission_receipt` for Onton
  to reuse; OITC does not need a second receipt state machine. Resource capabilities
  remain M2 work and coordinator-to-worker protocol integration remains M3 work;
  existing local control sockets carry user controls.

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
  OITC already uses `ClientToken`. M2 retains physical launch identity but authorizes
  only one submission; caller retries observe the retained launch rather than
  resubmit. See also the
  [token retention contract](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_Idempotency.html):
  tokens expire, so they cannot fence a delayed resubmission indefinitely.
- **documentation — Amazon ECS task lifecycle** —
  <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle-explanation.html>
  Replacement and cancellation distinguish desired stop from observed `STOPPED`.
  This is the platform evidence used to retire a previous writer.
- **documentation — DynamoDB transactions and conditional writes** —
  <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/transaction-apis.html>
  Reuse OITC's guarded transition approach for resource ownership and conditional
  writes of opaque state. Onton uses confirmed writes to implement its own durable
  admission and result acknowledgements. Resource launch IDs and Onton operation
  IDs have distinct lifetimes and survive service-level deduplication windows.
- **documentation — Git push with an explicit expected revision** —
  <https://git-scm.com/docs/git-push>
  Keep publication in `Branch_reconcile` with its captured destination and
  expected revision. Preserve the root's ancestry policy. These checks protect
  remote history; they do not authorize an expired execution attempt.

## Milestones

### Milestone 1: headless-patch-execution-contract

**Repository**: Onton. **Dependencies**: none.

**Status**: Implemented and locally qualified as of 2026-10-22. The engineer
confirmed release and exact-binary OITC image qualification completed on
2026-10-23; release/image identities and smoke evidence were not independently
inspected during this workstream update.
Planning artifact: `gameplans/headless-patch-execution-contract.yaml`.
At source revision `215889d6536dbc201fa8cf8d2bf69f81af07fade`, `opam exec -- dune build`,
`opam exec -- dune runtest`, `opam exec -- dune build @worker-protocol-smoke`
and a forced smoke with `ONTON_WORKER_BINARY` pointing to the built worker all
exited 0. These local runs do not qualify a release or OITC image.

**Definition of Done**:

- Define protocol v2 in `lib_core/worker_protocol.ml` and
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

**Unlocks**: A concrete worker contract for the Onton coordinator to consume,
using cloud resource capabilities supplied by M2.

**Operator Actions Before Next Milestone** (engineer-confirmed complete):

- Publish an Onton release containing the qualified protocol and worker mode.
- Run the `worker-protocol-smoke` harness using its explicit binary override for
  the released `onton-worker` in the OITC image; record binary/image identities
  and the result. Follow `docs/design/headless-worker-protocol.md` for the concrete
  invocation. Do not advance if shipped and tested protocols differ.

### Milestone 2: cloud-execution-attempt-adapter

**Repository**: provisioning-agent. **Dependencies**: M1 and its release gate.

**Status**: Planned and finalized on 2026-10-23; not implemented or cloud-qualified.
Planning artifact: `provisioning-agent/gameplans/cloud-execution-attempt-adapter.yaml`
(six patches, eight engineer-approved decisions). Plan validation and bounded
Pantagruel checks passed; these are not runtime or deployed-service evidence.

**Definition of Done**:

- Add the local `issue-runner-bin resources` helper, its `resource-v1` contract in
  `tools/issue-runner/resource-contract.md`, and pure guarded resource policy in
  `internal/coordination/resource_types.go` and `resources.go`. Explicit `request`,
  `connect`, and `fixture` commands expose resources; Go never implements Onton
  Execute, admission receipts, checkpoint/result ACKs, or logical patch retries.
- `resource_ecs.go` durably authorizes one `RunTask` submission per physical launch
  identity, with SDK resubmission disabled. Duplicate caller requests observe the
  retained record. A crash after authorization but before send can remain ambiguous
  and reach attention: this deliberately sacrifices automatic retry liveness to
  avoid duplicate tasks after provider token expiry. Only an atomic abort before
  send authorization proves an intent never submitted.
- Require positive matching-task `STOPPED` evidence before replacing a submitted
  resource or transferring coordinator ownership. Missing tasks, failed observations,
  `STOPPING`, desired status and expired leases are not proof. Persist at most
  12 observations over 10 minutes before attention; explicit later observation can
  resolve evidence but cannot authorize another submission for that identity.
- `resource_store.go` retains immutable opaque S3 bytes behind atomically published
  DynamoDB versioned pointers and write receipts in dedicated coordinator-only
  storage. Support payloads up to 8 MiB and request envelopes up to 16 MiB; reject
  excess rather than truncate. Resolve lost commit responses by strong readback.
  Validate current authority before replay: exact retained requests can recover
  receipts despite newer revisions; former-owner epochs and changed reuse reject.
- Retain identities/state without live TTL. Explicit close fences new work, retires
  every possible resource, permits retryable cleanup while still closing, then
  records a terminal tombstone. Cleanup failure stays closing; old requests cannot
  resurrect a closed run. Preserve all ambiguous intents through takeover/closure.
- `resource_host.go` supplies one exclusive opaque stdin/stdout stream using pinned
  server TLS and a per-resource bearer, with stderr separate. Bootstrap is an
  immutable object-specific expiring download, not shared storage access. There is
  no reconnect or replay. Disconnect initiates bounded process-tree cleanup and
  host exit within 65 seconds; local exit is not a substitute for ECS stop evidence.
- A bounded manifest selects configured image executables/arguments, immutable files,
  and optional preparation profiles. `resource_workspace.go` owns fresh workspaces,
  coding setup and renewable credentials independently of issue-session polling.
  `resource_logs.go` attributes supplied logs/transcripts without interpreting them;
  bounded telemetry failures cannot indefinitely block cleanup.
- `infra/issue-runner-resources.ts`, composed by `infra/issue-runner.ts` and exposed
  through `sst.issue-runner.config.ts`, supplies separate coordinator/process roles,
  private ingress, dedicated state storage and digest-pinned task templates. Wire
  both source permissions and target development/log role trusts; patch tasks get
  no resource-state or ECS launch authority. Reuse the maintained image/network.
- `resource_cli.go`, `resource_runtime.go` and `resource_fixture.go` compose the real
  resource path and an opaque echo-process fixture, including fresh replacement and
  fault injection. New `RUNNER_MODE=resource-host` and explicit CLI commands are the
  only entrypoints; ordinary issue starts and archive restore stay unchanged.

**Why this is a safe pause point**: The adapter is available for controlled
qualification; existing issue sessions retain their existing route.

**Unlocks**: Cloud compute, connectivity, and storage capabilities for Onton to
use while retaining all worker-protocol and orchestration semantics.

**Operator Actions Before Next Milestone**:

- Deploy the helper and scoped infrastructure through the OITC release path. Obtain
  the non-secret resource descriptor from the actual stack outputs and use the
  dedicated qualifier task; the stack's current dev branch provisions only a bastion
  and is not a resource qualification environment.
- Run `issue-runner-bin resources fixture` in its documented cloud mode, using
  `tools/issue-runner/resource-contract.md` for configuration and invocation details.
  Use an isolated run and the image-local echo child, not an Onton scheduler.
- Interrupt a successful launch response, pause a sender after send authorization,
  and partition connectivity in separate scenarios. Repeat caller requests and
  verify only one API submission; prove no successor starts before positive stop.
  Unknown outcomes must remain pending/attention, not silently acquire a new token.
- Exercise stale-owner writes, lost successful commits, wrong bearer/TLS trust,
  simultaneous attachment, process cleanup and interrupted close/cleanup. Record
  image/task identities, observation timelines, versions, byte digests and cleanup
  outcomes without credentials or signed URLs. Proceed only when A06–A08 and the
  M2 resource acceptance checks below pass against actual deployed services.
- Close qualification runs before rollback; do not delete active/ambiguous resource
  storage or relaunch their branches through the legacy route. This gate does not
  qualify Onton ACKs, scheduling or command replay; those remain M3 work.

### Milestone 3: distributed-gameplan-coordinator

**Repository**: Onton. **Dependencies**: M2 and its cloud contract gate.

**Definition of Done**:

- Add one active opt-in headless Onton coordinator that owns communication with
  all patch workers, local or remote, retaining existing dependency and eligibility
  decisions. It sends execution commands and consumes worker events over connectivity
  supplied by the platform; OITC does not synthesize those commands.
- Reuse `Worker_protocol` receipt and transition logic on the Onton side. Own durable
  admission, checkpoint validation, result delivery/ACKs, cancellation messages,
  replay rejection, and recovery binding. Persist the exact capsule and admission
  facts through M2's opaque conditional storage before sending dependent protocol
  ACKs. A storage commit and an Onton protocol acknowledgement are distinct events.
- Map durable outbox work to protocol identities without conflating logical work,
  protocol assignments, and physical container launches. In particular, replacing
  a container must preserve M1's unfinished-assignment receipt rather than reset
  its sequence or invent a fresh operation to bypass recovery.
- Persist coordinator transitions before dispatch or acceptance that relies on
  them. On coordinator replacement, restore committed state and reconcile existing
  resources, worker connections, and pending results before dispatching new work.
  OITC stores bytes; Onton decides what those bytes mean and what remains runnable.
  Consume M2's local helper and single-attachment stream contract: a lost stream
  cannot be reattached, and an unresolved physical launch cannot be retried under a
  new identity. Read committed state and resource evidence before choosing recovery;
  preserve pending/attention rather than bypassing platform fencing.
- Run eligible patches in separate workers under a gameplan-wide concurrency
  limit. Loss of one worker leaves unrelated eligible work runnable. Automatic
  recovery uses the M1 origin-based path, with bounded retries and diagnostics.
  Onton requests resource stop/replacement and awaits M2's positive termination
  evidence before assigning a replacement; an expired lease alone is insufficient.
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
- Deliver task-loss observations to the coordinator, which decides patch recovery
  and requests replacement resources. Host and restart a lost coordinator only
  after predecessor termination is confirmed; the new Onton process restores and
  interprets its durable control state. OITC does not send patch commands or restore
  patch workspaces/coding-agent sessions to recover distributed attempts.
- Route authorized start/cancel/automerge controls and existing CI/review wakes
  to the owning run. Cancellation retires all active and ambiguous child launches
  before cleanup completes; late events cannot resurrect the cancelled run.
- Stop compute for sleep only on Onton's durable aggregate sleep-ready state;
  OITC does not infer runnable work from patch receipts. Wake starts the coordinator,
  which restores durable control state and obtains current origin/forge observations.
  Preserve the existing wake-counter protection during the sleep transition.
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
| Onton integration and logical recovery policy | M2's helper/stream/storage choices and bounded resource observation policy are resolved below. M3 must map Onton identities and recovery onto them without resubmitting ambiguous launches or reattaching consumed streams. | M3 gameplan |
| Worker sizing and cold-start overhead | Start with explicit bounded capacity; measure with the maintained image rather than multiplying today's whole-gameplan resources blindly. | M4 qualification |
| Final default-on policy | Opt-in delivery is sufficient for this workstream. | Separate product decision |

## Decisions Made

| Decision | Rationale and status |
| --- | --- |
| Cloud-only and headless | Confirmed by the user; no local coordinator deployment or distributed TUI is required. |
| OITC owns infrastructure, not patch orchestration | Clarified by the user on 2026-10-23: OITC provides boxes, connectivity, credentials, storage, and resource lifecycle evidence. One active Onton coordinator communicates with patch workers on the same or different boxes; it owns commands, receipts, ACKs, and result interpretation. AWS resource adapters stay in provisioning-agent. |
| No receipt-helper prerequisite | The exploratory OITC command API and separate Onton receipt-helper proposal are superseded by the clarified boundary. M3 reuses Onton's existing protocol logic; no extra prerequisite milestone is introduced. |
| Local resource helper | Approved in M2 planning: Onton calls a local Go helper with a bounded resource-v1 interface and coordinator-scoped AWS authority. No new network control service or AWS client in Onton is required. |
| Opaque single-attachment stream | Approved in M2: pinned server TLS plus a per-resource bearer protects an opaque stdin/stdout stream; stderr is separate. One attachment per resource, no reconnect or replay. Onton alone interprets bytes. |
| Isolated bootstrap and generic process manifest | Approved in M2: object-specific expiring bootstrap download; coordinator-only client capabilities/state. Configured image executables, arguments, immutable files and optional preparation profiles keep launch configuration generic rather than encoding Onton commands in Go. |
| Opaque state commit boundary | Approved in M2: immutable S3 payloads, conditional DynamoDB pointer/version and durable write identity committed together. Strong readback resolves lost responses; storage success is not an Onton protocol ACK. |
| Explicit resource lifetime | Approved in M2: no live TTL; takeover needs predecessor termination. Close fences, retires resources, cleans while closing, then retains a terminal tombstone. M4 must drive explicit closure and must not destroy unresolved resource state. |
| Single-submission launch safety | Approved in M2: one durable send authorization and one RunTask submission per physical identity, SDK retries disabled. Caller retries observe. A crash before the actual send can remain ambiguous and reach attention; availability is deliberately sacrificed rather than risk a late duplicate beyond ECS token retention. |
| Bounded resource observations | M2 starts with 12 observations over 10 minutes before attention. Later explicit observation may resolve evidence but never grants another submission. M3 owns logical patch recovery limits separately. |
| Automatic replacement | Confirmed by the user; a lost worker must not require an operator to restore its archive. |
| Origin is the code recovery point | Confirmed by the user. Unpushed commits, edits, paused sequencers, and conversation progress may be lost. There is no time-bounded recovery-point guarantee or requirement to publish partial work periodically. |
| Fresh recovery conversation | Confirmed by the user. The new agent is told it is recovering, receives current task context, and inspects and validates the remaining work. |
| Durable control state remains necessary | Design consequence: branch contents cannot recover accepted user controls, pending operations, plan identity, or ownership. OITC stores these separately from disposable workspaces. |
| Single patch execution owner | Approved in M1 planning: worker alone advances its patch reconciliation state; coordinator receives projections. Clarified boundary: Onton persists exact capsules using OITC storage and sends protocol ACKs after confirmed storage. Root integration ownership remains M3 work. |
| Reusable run/patch worker | Approved in M1 planning: one process accepts successive operations with distinct attempt identities only after terminal ACK and cleanup. No concurrent operations or cross-patch reassignment. |
| Host protocol | Selected under engineer delegation in M1: dedicated `onton-worker`, JSON Lines on stdin/stdout, diagnostics on stderr. Its protocol host is the Onton coordinator (or qualification harness), not OITC. OITC provides authenticated connectivity and task-termination evidence; IDs alone do not prove task death. |
| Patch capsules and ACK barriers | Approved in M1: versioned patch-scoped payloads, not mutable whole-gameplan snapshots. Onton owns capsule/receipt pairing and protocol ACK barriers; sidecars cannot supersede committed state. M2 supplies authority/version-fenced opaque storage, and M3 uses confirmed writes before ACKs or dependent dispatch. |
| Host disconnection | Approved in M1: cancel and terminate after bounded cleanup; no reconnect protocol. Readmission uses actual host-committed state and re-observes ambiguous effects. |
| Remote disappearance and renewed intent | Approved in M1: confirmed never-published absence can use current base. Possibly published disappearance requires forge reconciliation; confirmed merge is reported, otherwise explicit renewed instruction referencing the attention receipt is needed to recreate. Ordinary retries cannot recreate; renewal preserves publication history and rechecks remote/forge evidence. |
| Feature construction is in scope | The inspected OITC multi-patch path uses it today; supporting only independent PR branches would miss the actual target workflow. |
| Reuse branch reconciliation | Required repository architecture. No alternate remote-only rebase or publication path. Workspace loss becomes an explicit owner transition. |
| Recover effects by observation | A repeated execution may re-run tests or agent work. The system does not promise exactly-once arbitrary shell/API side effects; it must reconcile owned Git, PR, and operation state before repeating them. |
| Delivery status | M1 passed local build/tests/smoke; release/image qualification is engineer-confirmed, not independently verified here. M2's six-patch gameplan was finalized and validated on 2026-10-23; implementation and cloud qualification remain outstanding. M3–M4 remain milestone sketches. |

## Definition of Done (Acceptance Suite)

Each scenario is independent and runs against isolated repositories and run IDs.
The M4 driver invokes the public headless protocol, OITC admission, task APIs,
Git, forge APIs, and durable state observations. Failure injection acts at those
boundaries. Source text is never the behavioral oracle. Proposed fixture command
names are finalized in the owning gameplans; owner files below identify the
implementation boundary. M2's planned resource commands and files below are not yet
implemented. Each assertion has one owning milestone. M1 fixtures are
implemented: `dune build @worker-protocol-smoke` drives `onton-worker` through
`test/test_worker_protocol.ml`; focused recovery fixtures are
`test/test_worker_recovery_git.ml` and `test/test_worker_recovery_prompt.ml`.
The local M1 qualification passed as recorded above; its release/image gate is
engineer-confirmed. No distributed cloud acceptance run is claimed.

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
  - **Assert**: Repeating a resource request after a lost launch response never resubmits RunTask for that physical identity.
  - **Verify by** `cmd` + `api`: `TestResourceLaunchAmbiguityAndIdempotency` and the cloud `resources fixture` drop a successful response, repeat requests, and inspect the captured API submissions, resource records and ECS tasks; separately pause after send authorization before dispatch.
  - **Expected**: At most one submission and one associated task. Unresolved sends stay pending/attention, including the pre-send crash window; absence never authorizes a second submission.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/resource_ecs.go`, `resource_ecs_test.go` and `resource_fixture.go`.
- **A07 — Resource replacement waits for predecessor termination**
  - **Assert**: Resource replacement or coordinator takeover cannot proceed on unconfirmed predecessor termination.
  - **Verify by** `cmd` + `api`: `TestResourceStopTransferAndClosure` and the cloud fixture exercise RUNNING/STOPPING, missing tasks and failed observations before supplying matching STOPPED evidence.
  - **Expected**: No successor before positive retirement; bounded observation reaches attention without releasing authority. A never-submitted intent requires atomic abort before send authorization, not a lease timeout.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/resource_ecs.go` and `internal/coordination/resources.go`.
- **A08 — Opaque durable state has one current writer**
  - **Assert**: Stale owners/versions cannot overwrite state, and a lost successful write response remains recoverable.
  - **Verify by** `cmd` + `api` + `db`: `TestResourceMetadataFencesAndRetention`, `TestResourceOpaqueStateCommitAndReadback` and the cloud fixture store bytes, advance authority/version, replay old writes, and lose a successful commit response before readback.
  - **Expected**: Exact committed bytes/version/receipt remain available. Former-owner epochs and changed reuse reject; current-owner exact retries recover receipts without repeating writes. No Onton interpretation is involved.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/resource_store.go` and `resource_store_test.go`.
- **A23 — Resource streams preserve opaque bytes with exclusive admission**
  - **Assert**: Only one authenticated attachment can start the resource process and exchange its stdin/stdout bytes.
  - **Verify by** `cmd` + `api`: `TestResourceStreamAuthenticationAndCleanup` and the cloud fixture try wrong TLS trust/bearer, concurrent valid attachments, arbitrary non-JSON bytes, normal EOF and disconnected child/grandchild processes.
  - **Expected**: Unauthorized/duplicate attachments start no extra child; accepted bytes remain ordered and exact, EOF drains output, and disconnect initiates cleanup within the 65-second bound. No stream reattachment or automatic replay occurs.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/resource_host.go` and `resource_host_test.go`.
- **A24 — Patch resources cannot acquire coordinator authority**
  - **Assert**: A patch task cannot write resource state, launch another task or read another resource's bootstrap/capability.
  - **Verify by** `api`: In the cloud fixture, attempt these operations with the resource-process role; verify permitted own bootstrap and task-scoped development/log access separately.
  - **Expected**: Cross-resource/authority operations are denied; legitimate role assumptions require matching source grants, target trusts and task identity. Only private coordinator-to-process ingress is allowed.
  - **Traces to**: M2 — `provisioning-agent/infra/issue-runner-resources.ts` and `issue-runner-resources.unit.test.ts`.
- **A25 — Explicit resource closure is resumable and terminal**
  - **Assert**: Interrupted cleanup cannot reopen a run or prematurely report it closed.
  - **Verify by** `cmd` + `api`: The resource fixture closes a run with a delayed authorized send, then interrupts object cleanup after resource retirement; resume cleanup and replay old open/launch/write requests.
  - **Expected**: Unresolved resources block closure; cleanup failure remains fenced-closing. Completed cleanup leaves a terminal tombstone rejecting resurrection; repeated close is harmless.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/resource_ecs.go`, `resource_store.go` and `resource_fixture.go`.
- **A26 — Fresh process preparation owns renewable credentials**
  - **Assert**: A valid coding resource prepares a fresh workspace and renews credentials without an issue-session poller.
  - **Verify by** `cmd`: `TestResourceBootstrapAndFreshWorkspace` observes input materialization and token rotation without legacy session polling; test expired/invalid bootstrap and escaping paths independently.
  - **Expected**: Fresh owned files/clone and independent renewal; invalid bootstrap starts no child, and cleanup releases the profile, timers and temporary files. No workspace archive is required.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/resource_workspace.go` and `resource_workspace_test.go`.
- **A27 — Resource telemetry is attributed and bounded**
  - **Assert**: Supplied stderr/transcript records retain resource attribution without contaminating stdout or blocking cleanup indefinitely.
  - **Verify by** `cmd`: `TestResourceLogAttributionAndRedaction` saturates the sink and injects sensitive bootstrap values while the echo fixture exchanges bytes.
  - **Expected**: Run/patch/resource attribution, redacted credentials, explicit telemetry-drop reporting and bounded cleanup; process stdout stays an opaque byte stream.
  - **Traces to**: M2 — `provisioning-agent/tools/issue-runner/resource_logs.go` and `resource_logs_test.go`.
- **A22 — Onton owns durable worker acknowledgements**
  - **Assert**: The Onton coordinator sends checkpoint/result ACKs only after the corresponding protocol state is durably committed.
  - **Verify by** `cmd`: In the M3 multi-process harness, delay/reject storage commits and lose commit responses; restart the coordinator and inspect worker protocol traffic and restored admission state.
  - **Expected**: No premature ACK or dependent work; restoration uses the actual committed capsule/receipt, and duplicate delivery cannot advance another assignment. The platform fixture stores opaque bytes and does not generate protocol ACKs.
  - **Traces to**: M3 — `lib_core/worker_protocol.ml` with coordinator persistence integration in `lib/persistence.ml` and `lib/runner_fiber_impl.ml`.
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
  - **Verify by** `cmd`: Kill the coordinator after dispatch acceptance, retain a live worker task and a pending human instruction, then restart the Onton coordinator using committed state and resource observations.
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
