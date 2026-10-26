---
title: Distributed Onton execution in the cloud
status: active
updated_at: 2026-10-26
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

The original source inventory below was verified against the Onton and sibling
`provisioning-agent` checkouts on 2026-10-22. M2 deployment and qualification
were independently verified on 2026-10-26; deployed identities and results are
recorded below. The engineer accepted M2 as complete on 2026-10-26 and explicitly
waived the template-retention replacement drill. M3 is the next milestone.

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
  are implemented and dev echo-qualified as recorded below; coordinator-to-worker
  protocol integration remains M3 work;
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

**Status**: Complete — engineer-accepted on 2026-10-26. Isolated dev echo,
cloud fault/permission, connectivity, observation-recovery and production
coding-worker qualification passed as recorded below. The coding evidence
distinguishes the native renewal cycle from accelerated real-token rotation.
The engineer explicitly waived the template-retention replacement drill and
accepted M2 without that additional evidence. The drill was not performed and
is not claimed as passed; it no longer gates M3. The deployed retention setting
remains in place.
Planning artifact: `provisioning-agent/gameplans/cloud-execution-attempt-adapter.yaml`
(six patches, eight engineer-approved decisions). Plan validation and bounded
Pantagruel checks passed. Deployment and runtime evidence follow separately.

**Deployment and qualification evidence (2026-10-26)**:

- Qualification fixes merged in provisioning-agent
  [PR #11789](https://github.com/flowglad/provisioning-agent/pull/11789).
  [PR #11790](https://github.com/flowglad/provisioning-agent/pull/11790) added the
  isolated dev resources and merged as `dea74e393d1e79f90fd62c7dd2c3bb0e5c2669da`.
- [OITC deployment 40357547735](https://github.com/flowglad/provisioning-agent/actions/runs/40357547735)
  succeeded for `97a0f6b2df6b930351965ff27dd805437abb0168`, whose ancestry includes
  that merge. Both maintained-release and dev-resource jobs succeeded; dev release
  tag: `deploy-issue-runner-dev-20261026T162107Z`.
- Deployed coordinator/process task definitions: `oitc-rc-dev:1` and
  `oitc-rp-dev:1`, cluster `payclaw-issue-runner-dev`, account `576847308568`,
  region `us-east-1`. Both use image
  `576847308568.dkr.ecr.us-east-1.amazonaws.com/payclaw-issue-runner-production@sha256:e2686b9d77baa3ec73ea90a7a929aaa65ed3079ce716626369a085af29f0dccc`.
  Dev has separate lifecycle storage, roles and security groups in the dev private
  network, direct CloudWatch logs, and no coding secrets or development-role authority.
- Ran `resources fixture --cloud` in fresh coordinator task
  `fc5805ec0533459889a4db8253130759`, using the descriptor from deployed dev state.
  Run: `fixture-eb85256b697cfd5d987550594b6e4abf`. Child tasks:
  `356f3ef93e1e4bb3aabb1819b7dc8480` and `fb7fcdce493c4f97be87b13aba77e701`.
- Retrieved the immutable report directly from bucket
  `oitc-res-dev-20261026162053300800000003`, key
  `qualification/a18a6d2aa89989b6cf0b02664eecef54dacfd6c912ce465c1d100b1b2616bb00/b28a04cfd08c2e9d0a27b7d6a9f7927618a9688f99f4c1d3c09f30b48b561980/cloud.json`.
  It records `passed: true`, `cleanup: closed`, a rejected stale-epoch write,
  successful state readback across replacement, and matching echo/state SHA-256
  `8258fe1968d38f7d875f0a4a180913e83fcd3f561e3f0f305f2c440f22c77915`.
- Independent ECS reads confirmed the coordinator and both children `STOPPED`,
  each with exit code 0 and the expected image digest. A strongly consistent read
  from `oitc-res-dev` confirmed revision 29, state version 1, `closing: true`,
  `closed: true`, and `cleanup_complete: true`.
- The earlier failed production run
  `fixture-8c26e3306b9486f9e5c5ae41e4a0a56c` remains a separate cleanup obligation:
  its tasks were confirmed stopped, but its last observed record was closing with
  cleanup incomplete. The successful dev drill does not discharge that record.

**Additional cloud qualification (2026-10-26)**:

- The operator harness's six fault checks passed in dev run
  `fixture-7299d1e8e0c191347c87e14d3000c981`: lost successful launch and commit
  responses, replacement fencing while send authorization was paused, exactly
  two launch submissions, rejected TLS/bearer/duplicate attachment with EOF and
  disconnect, and an interrupted cleanup upload followed by successful retry.
  The immutable report is in the dev bucket above at
  `qualification/8309be3bd0aadca61aed3d1431bfef9f171e603cb6f1f7d02da3159b83437fe0/7ed123c654b7713332f9a58d0d5cea9295a474d50d72b4935eab6cdb723d9ab6/cloud-faults.json`.
  `passed` is true, cleanup is closed, and the echo/state digests match the prior
  successful drill. CloudTrail independently confirmed exactly two successful
  RunTask submissions by coordinator `95d72f1019c449d993bf922bc99dc328`.
- This was an instrumented coordinator harness using the shipped process host,
  not a claim that the ordinary cloud CLI injects faults. Its SHA-256 is
  `8396cfc7c4507a2203080383a5b5c920846d4cccb96ab42030fc5162505e0c5f`.
  The tested resource Go sources match the earlier deployed release; retained
  copies of the task templates prevented concurrent releases from invalidating
  the test. The coordinator and both children were confirmed stopped.
- Process-role task `bd56a71c056746d28445af090cd81c60` exited 0 after actual AWS
  AccessDenied responses for coordinator-state reads/writes, another bootstrap
  object, ECS RunTask, and another task's transcript stream. Its own task-scoped
  transcript role assumption and write succeeded. Credential-free evidence is
  retained at `qualification/operator/permissions-bd56a71c056746d28445af090cd81c60.json`
  in the dev bucket. This qualifies the echo process role; coding preparation and
  credential renewal remain separate.
- Actual coordinator-loss cleanup recovery also passed. Run
  `fixture-fad2de3fc58d5355a0b9f829d9eda212` reached `closing: true` with cleanup
  incomplete while uploads to only its payload prefix were temporarily denied.
  Coordinator `c20a190716a64f14b109be14152673ce` was stopped; recovery was launched
  only after positive ECS STOPPED evidence and removal of the temporary denial.
  Successor `3f698f1d0de246d19ec32dbee96f0d87` took ownership at epoch 2 and closed
  revision 32 with `cleanup_complete: true`, then exited 0. Its immutable report is
  `qualification/b5add6e1c3c66af4dab44d39e5083a24d974b0b4e06eaa65745c0f21eddd65ff/159c59b4324961ed3a8bc39b3b5b0790153581c65fa820886932a7eff0947eca/cloud-close.json`.
  The report correctly retains `passed: false` for the interrupted original
  fixture; the independently verified result is successful cleanup recovery.
- The aggregate, credential-free evidence was stored and read back exactly at
  `qualification/operator/m2-followup-20261026-d0cf2be56e.json` in the dev bucket,
  SHA-256 `d90f2efbfc436b2324975449e8011b1c71cb5571ddfa96caa6a21b54e03c6465`.
  It includes CloudTrail, task states, permission evidence, run inventory and
  recovery timeline. All drill tasks are stopped, the temporary IAM denial is
  removed, and temporary task definitions were deregistered only after their
  runs could no longer launch work. The dev cluster had no active tasks at the
  final check.
- A concurrent release deregistered `oitc-rp-dev:1` while two earlier attempts
  still had that revision captured. ECS returned `TaskDefinition is inactive`;
  the helper retained uncertainty and did not resubmit. The infrastructure fix
  in [PR #11798](https://github.com/flowglad/provisioning-agent/pull/11798) retains
  earlier resource task revisions with `skipDestroy`. The post-merge deployment
  and rerun below verified the deployed setting; a subsequent template replacement
  crossing an active run still needs verification.
- Failed or interrupted attempts retain their authority records:
  `fixture-6971fad4e69ebe024f46a2dd845a58d3` (the first harness assertion aborted
  after send authorization), `fixture-17ef0622b04e418b886461de82af1d83`, and
  `fixture-2d4ec39cd06f4d24c2422b357593544d` (deactivated captured template).
  Their coordinators were stopped; the first and third records are closing,
  while the second remains open with its consumed send authorization retained.
  No resubmission or successful cleanup is claimed for these attempts.
  The first attempt's safe projection is retained at
  `qualification/operator/first-fault-attempt-5be77ed07acb460f8c4c8d5528c5104e.json`.
- Retrying the earlier production cleanup produced a durable `cloud-close.json`
  report with `cleanup: pending`: ECS no longer returns the old coordinator,
  so takeover cannot obtain positive STOPPED evidence. Recovery task
  `93645cee818c4c21846c52b58398911f` stopped with helper exit 1. Missing provider
  history does not authorize clearing the record or declaring cleanup complete.

**Post-merge rerun of PR #11798 (2026-10-26)**:

- PR #11798 merged as `c2b8a7a074d070972aa21fc4ce3a1e002fa21c1f`.
  [Deployment 40373740579](https://github.com/flowglad/provisioning-agent/actions/runs/40373740579)
  succeeded for that exact commit, including maintained-release and dev-resource
  jobs. The drill started only after deployment completed.
- Deployed state confirmed `skipDestroy: true` in both inputs and outputs for
  `oitc-rc-dev:4` and `oitc-rp-dev:4`; before deployment both were false. The task
  revisions stayed unchanged during this deployment, so this verifies configuration
  but does not yet exercise retention through a subsequent template replacement.
  Both templates remained ACTIVE after the drill.
- Both templates use image
  `576847308568.dkr.ecr.us-east-1.amazonaws.com/payclaw-issue-runner-production@sha256:cc26be052fd4182cb3a8a54ac3861743232ac8dfa0d5149be8f91756d7f5a034`.
  The instrumented harness was built from the exact merge commit, SHA-256
  `b5b651c2fcffcbf0e202c3090441160ac8963a0384578f59cf17c720fb5b52de`.
- Fault run `fixture-a4b9eae028e0a657e68bb6ca8b96456d` passed all six checks and
  closed at revision 30. Ordinary deployed CLI echo run
  `fixture-99bb41a4c3a936740621d4f6676db289` also passed and closed at revision 31.
  Both reports record `passed: true`, `cleanup: closed`, and matching echo/state
  digests. Strong reads confirmed `closed: true` and `cleanup_complete: true`.
- Process-role probe `46179370fb5f4a9e95dee7e8b8ed3442` passed the real AWS
  permission checks, including rejection of another task's transcript-role
  assumption and stream, while its own transcript write succeeded. All seven
  coordinator/probe/child tasks were independently confirmed STOPPED with exit 0
  and the expected image digest. CloudTrail confirmed exactly two successful
  child RunTask submissions per run, four total.
- The aggregate report was stored and read back exactly from the dev bucket at
  `qualification/operator/m2-rerun-11798-8a2afed6.json`, SHA-256
  `4639e4a943de94bfce13a124b47eb46348e976f618d072117197b815fb912b62`.
  It includes source/deployment identity, retention observations, both immutable
  fixture reports, process-role evidence, final task/run state and CloudTrail.
  Temporary probe task definitions were deregistered; managed templates remain
  active. Earlier unresolved records were not altered by this rerun.

**Dev stream interruption drill (2026-10-26)**:

- Run `fixture-ea6e7210f42f2075a066eb375cb9316a` passed the dedicated stream
  interruption probe using existing `oitc-rc-dev:4` and `oitc-rp-dev:4` templates.
  No task-template registration, replacement, or deployment was initiated for
  this drill. Both template revisions remained ACTIVE.
- The operator probe ran through ECS Exec inside coordinator task
  `9410972c65aa45edac5d55a4292042ca`, retaining its real task identity and resource
  authority. Its temporary dev coordinator session-channel grant was removed
  after cleanup. The ordinary fixture was deliberately paused and interrupted;
  its own `cloud.json` is not the passing evidence for this probe.
- After exact arbitrary-byte echo on an established stream, a premature replacement
  returned `pending / predecessor_live` without reserving a successor. The probe
  killed the local connection-helper process with stdin still open. The remote
  host's containers were observed stopped after 10,329 ms, within the 65-second
  cleanup bound. AWS recorded `EssentialContainerExited`; no StopTask request
  was made for this interrupted host.
- The successor was admitted only after positive predecessor STOPPED evidence,
  then connected and echoed its expected bytes. Independent AWS timestamps
  confirmed successor creation followed predecessor termination. CloudTrail
  confirmed three successful child submissions: the ordinary fixture's retired
  initial child, the interruption target, and its successor.
- The run closed at revision 41, state version 2, with `closed: true` and
  `cleanup_complete: true`. All four task instances were confirmed STOPPED.
  The coordinator's exit 1 reflects the deliberately interrupted ordinary
  fixture; the dedicated probe report records `passed: true` and all four
  checks passed. The two probe worker hosts exited 0.
- The probe source is retained at
  `qualification/operator-tools/13341f23d99a665d037bf7dbf59806bdb493eb61ac414e8b8c55f68ae13e2b5c/connectivity.mjs`
  in the dev bucket. Aggregate evidence was stored and read back exactly at
  `qualification/operator/m2-connectivity-9410972c.json`, SHA-256
  `16992d7298d13ebd90c1210f21f38a8f9700269c12c3a4e012d3f1f71ab0413f`.
  It contains the immutable probe report, task/run observations, CloudTrail,
  unchanged template identities, and permission-removal confirmation.
- This qualifies abrupt client-process/stream loss and bounded host cleanup.
  The bounded packet-loss qualification is recorded below. The template
  replacement drill remains deferred at the engineer's request.

**Dev packet-loss drill (2026-10-26)**:

- Run `fixture-7154a7e25e76ef504d81da7a920b63b6` passed all seven dedicated
  packet-loss checks using existing coordinator/process template revisions 4.
  No task-template registration, replacement, or deployment was initiated.
  The operator probe ran inside the real dev coordinator through ECS Exec;
  the deliberately interrupted ordinary fixture's `cloud.json` is not this
  drill's passing evidence.
- Coordinator and worker were placed in separate private subnets. Two temporary
  ingress ACL denies targeted only their established TCP connection's source
  addresses and destination ports. They dropped traffic for 64.076 seconds;
  removal restored the complete original ACL entry set. An independent watchdog
  could remove only matching owned rules. No ACL associations were changed.
- The operator's 15,015 ms read deadline expired while the helper and worker
  remained running with 51 unacknowledged bytes. This is an operator-probe
  timeout, not a native helper idle timeout. Replacement returned
  `pending / predecessor_live` both before and during the partition, without
  reserving a successor. A transient partition did not grant replacement authority.
- After restoration, the original socket delivered all 29 buffered bytes exactly,
  with unchanged inode and client port, without reattachment or replay. Recovery
  took 48,546 ms from observation of the restoration marker, or approximately
  51.698 seconds from actual ACL removal. TCP samples showed a pending
  retransmission timer while recovery waited; no native idle timeout was observed.
- Normal EOF then stopped the host containers in 10,337 ms measured after the
  local helper exited (10,340 ms from the recovered-byte event). AWS recorded
  `EssentialContainerExited`, with no StopTask request for that host. Successor
  creation followed positive predecessor STOPPED evidence and independent AWS
  termination timestamps; the successor connected and echoed successfully.
- The run closed at revision 45, state version 2, with `closed: true` and
  `cleanup_complete: true`. All four tasks were independently confirmed STOPPED;
  CloudTrail confirmed three successful child launches. The coordinator exited 1
  because its ordinary fixture was intentionally interrupted. The temporary dev
  coordinator session-channel policy was removed, the owned deny rules are absent,
  and both managed task definitions remain ACTIVE at revision 4.
- Preserve the earlier failed attempt: run
  `fixture-49c8af1db243746f345ab758e26ff5a9` blocked replacement during a real
  partition but failed its 45-second post-restoration recovery observation window.
  It was closed and cleaned up, with its coordinator stopped and temporary policy
  removed. The successful second attempt used a 180-second observation budget;
  it does not erase the first result or establish a 45-second recovery guarantee.
- Aggregate evidence, including both immutable probe reports, safe task/run and
  CloudTrail observations, ACL snapshots, source hashes and permission cleanup,
  was stored and read back exactly from the dev bucket at
  `qualification/operator/m2-packet-loss-87e658e5.json`, SHA-256
  `0831023a7c6fa47ada80c2f712fa35cc06c15ab7d5d4c7ab0aecb3d1b630658d`.
  This qualifies a bounded transient packet-loss partition and recovery. A
  sustained blackhole and provider-observation qualification follow below.
  Template replacement remains deferred by request.

**Dev provider-observation drill (2026-10-26)**:

- Two real dev coordinators ran the dedicated operator probe using existing
  `oitc-rc-dev:4` and `oitc-rp-dev:4` templates. Leader run
  `fixture-4d0f5e09ba550728fbe972be880bee5a` passed seven checks; contender run
  `fixture-3af3eaa5880b398c2133c5dcc0e6cc9a` passed three takeover checks.
  No task-template registration, replacement, or deployment was initiated.
- The probe used real task metadata, roles, ECS calls and durable AWS storage.
  An operator-only Go source overlay withheld successful DescribeTasks responses
  at the client boundary, returning either an error or no matching task. These
  are injected observation faults, not evidence of a native AWS outage. No clock,
  STOPPED evidence, owner identity, or task success was fabricated. The compiled
  harness/source and SHA-256 identities are retained with the aggregate evidence.
- A distinct coordinator attempted takeover under an injected description error,
  a missing result, and restored visibility confirming the owner remained RUNNING.
  Every attempt returned `pending / predecessor_live`; owner and epoch remained
  unchanged. The contender submitted no resource launch during these checks.
- The leader launched one target worker and independently confirmed it actually
  STOPPED before withholding that evidence from the helper. Alternating error and
  missing-result observations never retired the resource. Replacement remained
  `pending / predecessor_live`, without a successor reservation or another launch.
  At persisted observation count 12, including earlier readiness observations,
  the helper returned `attention / observation_exhausted` on the real clock and
  backoff schedule. Close stayed pending while stop evidence was unavailable.
- Restoring the real matching STOPPED response resolved attention through an
  explicit observation. Cleanup then completed without another launch. Strong
  reads confirmed both runs closed with cleanup complete: leader revision 32 and
  contender revision 15, both state version 2 and owner epoch 1.
- Independent AWS checks confirmed all five tasks STOPPED and both temporary
  coordinator session-channel policies removed. CloudTrail showed exactly three
  successful child launches: two retired ordinary-fixture children and the single
  observation target. Coordinator exit 1 reflects deliberate interruption of the
  ordinary fixtures; dedicated observation reports passed. Both managed task
  definitions remained ACTIVE at revision 4.
- Preserve the initial harness failure: the first source-overlay mapping used
  `/tmp` instead of canonical `/private/tmp`, so both invocations selected no
  tests. They provide no qualification evidence. Their two runs closed, all four
  tasks stopped, and both temporary policies were removed before the fresh rerun.
  The corrected invocation explicitly listed and executed the selected test.
- Aggregate evidence was stored and read back exactly from the dev bucket at
  `qualification/operator/m2-observations-c6046e43.json`, SHA-256
  `5a7a2d0cf2adf7a0176278442fc96c7766ca4b7ca58af521fe5559d8b842489f`.
  It includes both passing reports, the empty invocation's disposition, safe
  task/run records, CloudTrail, template identities and permission cleanup.
  The corrected overlay passed the native race suite and vet. Architecture
  checking reproduced the four previously recorded baseline violations;
  repository runtime source was unchanged.

**Fenced qualification-run disposition (2026-10-26)**:

- Review and explicit disposition are complete for the four earlier failed runs.
  Each is retained as a failed qualification with cleanup unresolved, excluded
  from passing acceptance evidence. This operator disposition preserves the
  existing runtime records and consumed send authorizations. No run was marked
  closed or cleanup complete, and no resource was relaunched to bypass uncertainty.
- Strong reads and current ECS descriptions found:
  - Production `fixture-8c26e3306b9486f9e5c5ae41e4a0a56c`: closing, revision 13.
    Owner `c37d7a53250d46be83e92bbc7d33934f` and known worker
    `8c27e55eb2364e9d8ea450e166daa390` both return MISSING. A retained earlier
    local AWS-read artifact records both STOPPED, but the supported takeover path
    cannot import that historical evidence. Preserve the closing record and payloads.
  - Dev `fixture-6971fad4e69ebe024f46a2dd845a58d3`: closing, revision 5.
    Send authorization is consumed without a task ID. The paginated CloudTrail
    search found no child submission; absence does not prove never submitted.
    The owner returns MISSING. Preserve the ambiguous launch and its fence.
  - Dev `fixture-17ef0622b04e418b886461de82af1d83`: open, revision 27.
    Its first child has persisted STOPPED evidence. The successor consumed send
    authorization without a task ID; CloudTrail records a rejected RunTask against
    captured `oitc-rp-dev:1`. The owner returns MISSING. Preserve the open record
    and consumed successor authorization; no replacement or administrative reset.
  - Dev `fixture-2d4ec39cd06f4d24c2422b357593544d`: closing, revision 13.
    CloudTrail records a rejected RunTask against captured `oitc-rp-dev:1`, with
    consumed authorization and no task ID. The owner returns MISSING. Preserve
    the closing record and submission fence.
- Paginated RunTask/StopTask history was checked from 2026-10-24 onward. Cluster
  inventories covered desired RUNNING, PENDING and STOPPED, inspecting returned
  current states and tags so a task transitioning to STOPPED was not overlooked.
  No nonterminal task matched these runs. Dev had no nonterminal tasks; production
  had two unrelated nonterminal tasks, which were untouched. Empty inventories,
  provider rejection, and StopTask requests do not substitute for accepted stop
  evidence or revoke a consumed send authorization.
- No supported automated cleanup recovery was available during this read-only
  audit: takeover requires a current, positively matching STOPPED owner, and every
  owner returned MISSING. The engineer subsequently authorized manual cleanup;
  its completion is recorded below. No automated recovery change was needed.
- Resource metadata, opaque storage, task templates, tasks and IAM policies were
  unchanged. Production access was read-only. A final strong read confirmed all
  four owner epochs, revisions and cleanup flags unchanged. The safe disposition
  report was stored and read back exactly in the dev qualification bucket at
  `qualification/operator/m2-fenced-run-disposition-20261026.json`, SHA-256
  `eedafb391a22dacef20973ef6112b210132e345230aa04882cb60264513dafd1`.
  It retains current safe projections, CloudTrail evidence, prior local stop
  observations, per-run dispositions and the limits of those observations.

**Manual cleanup of failed qualification runs (2026-10-26)**:

- The engineer explicitly instructed manual cleanup after the read-only disposition
  above. All four failed runs are now manually retired with `closing: true`,
  `closed: true`, and `cleanup_complete: true`. This supersedes the earlier
  unresolved-cleanup disposition; no new automated recovery mechanism was needed.
- Fresh ECS inventories found no matching nonterminal tasks before the changes.
  Two unrelated production tasks were untouched. Administrative metadata writes
  used conditions against the exact previously read metadata, first fencing all
  runs closed with cleanup pending, then completing cleanup after object readback.
  Both the JSON metadata and DynamoDB guard columns were updated consistently.
- Fourteen run-scoped state/bootstrap objects were replaced with the existing
  `resource-v1:closed` seal marker and read back exactly. This removes their payload
  bytes while preventing delayed conditional uploads from recreating them. Both
  buckets have versioning disabled; no older payload versions were retained.
  Qualification reports and unrelated run objects were preserved.
- Final strongly read metadata revisions are:
  - Production `fixture-8c26e3306b9486f9e5c5ae41e4a0a56c`: revision 15.
  - Dev `fixture-6971fad4e69ebe024f46a2dd845a58d3`: revision 7.
  - Dev `fixture-17ef0622b04e418b886461de82af1d83`: revision 29.
  - Dev `fixture-2d4ec39cd06f4d24c2422b357593544d`: revision 15.
- Every non-metadata DynamoDB record is unchanged, verified by exact record-set
  digests. Launch fences, receipts, owner identities/epochs, and original unknown
  task observations are retained. Metadata explicitly records the operator,
  manual authorization, completion time, and audit reference. No STOPPED evidence
  was invented, and manual retirement does not turn these failed tests into passes.
  No task, IAM policy, or task definition was created or changed.
- The completion report was stored and read back exactly in the dev qualification
  bucket at `qualification/operator/m2-manual-cleanup-20261026.json`, SHA-256
  `b194127a7316068a0b136efa43d8d871bc796515381dfa5b9ac473cd3f4b5347`.
  It records the pre-cleanup inventories, exact object scopes, final metadata,
  preserved-record digests, and explicit administrative authority.

**Dev sustained-blackhole drill (2026-10-26)**:

- Run `fixture-f96774d43e14b5c81258dbbc7102afb2` completed all eight behavior
  checks using existing `oitc-rc-dev:4` and `oitc-rp-dev:4` templates. The probe
  established exact arbitrary-byte echo, left the client's stdin open, then held
  the same narrowly scoped, cross-subnet packet drop through autonomous client
  failure and positive worker STOPPED evidence. No task templates were registered,
  replaced or deployed, and no socket/kernel settings were changed.
- Two owned ACL denies blocked the established connection for 979,116 ms
  (16 minutes 19 seconds). The independent watchdog was armed for 22 minutes;
  the operator observation budget was 20 minutes. Restoration happened only after
  the original helper had exited and the worker had stopped. Final AWS reads
  confirmed the entire original ACL entry set restored, with both deny rules absent.
- The initial 15,003 ms read deadline was operator-owned. During the outage,
  replacement remained `pending / predecessor_live` while the worker was live;
  no successor was reserved. Socket samples retained the same inode/client port,
  51 unacknowledged bytes and advancing retransmission counters/timers.
- The worker container exited 147,088 ms after the block began. ECS recorded the
  task STOPPED at 171,616 ms (about 2 minutes 52 seconds), with
  `EssentialContainerExited`, exit 1. It was observed STOPPED by the probe at
  178,973 ms. Paginated CloudTrail evidence found no StopTask request for that
  target from any principal in the inspected window.
- The original client helper independently exited 1 at 970,776 ms (16 minutes
  11 seconds), without stdin EOF, operator kill, reconnect or replay. Its CLI
  diagnostic is generic and does not identify a timeout cause. Autonomous closure
  plus TCP timer observations support a native transport-failure inference;
  no application idle timeout or 65-second blackhole-detection guarantee is claimed.
  This separates outage-detection delay from the previously qualified cleanup
  bound after an observed disconnect.
- After network restoration, the helper persisted positive predecessor STOPPED
  evidence, admitted a successor, and verified successor echo. Independent AWS
  timestamps confirmed successor creation followed predecessor termination.
  The run closed at revision 41, state version 2, with cleanup complete. All four
  tasks are STOPPED; CloudTrail confirmed three child launches (retired initial
  fixture child, blackhole target, successor). The successor exited 0. The
  temporary coordinator session-channel policy was removed, and both managed
  template revisions remain ACTIVE at revision 4.
- Preserve a harness reporting failure: after terminal closure, the probe resumed
  the original fixture before publishing its dedicated `cloud-blackhole.json`.
  The primary exited and the container terminated, interrupting publication. That
  dedicated report is absent. The coordinator exited 1; the local ECS Exec client
  remained hung and was terminated only after AWS independently confirmed the
  coordinator STOPPED. No such termination caused the original connection failure.
- The passing behavior evidence was recovered from the captured public event
  stream, then independently checked against strong DynamoDB reads, ECS task
  states/timestamps, ACL snapshots and paginated CloudTrail. The aggregate records
  `original_probe_report_publication_passed: false` alongside the eight verified
  behavior checks; it does not claim the interrupted publication succeeded. Store
  the report before resuming the primary in future operator probes.
- Aggregate evidence was stored and read back exactly in the dev bucket at
  `qualification/operator/m2-blackhole-e6a94474.json`, SHA-256
  `4f037528b70219d11973d19e114770dbc9b989a63eea35fcce93c8bea9733485`.
  Operator Python/Bun syntax checks and all three rollback-ownership tests passed.
  This qualifies the sustained blackhole through native closure, replacement
  fencing, successor connectivity and terminal cleanup on the deployed dev path.

**Production coding-worker qualification (2026-10-26)**:

- The engineer selected an isolated drill on existing production coding resources
  and authorized subsequent qualification tests in production. Run
  `fixture-8eada264726502aa4133a62ba5f70112` passed all seven top-level checks using
  existing `oitc-rc-production:32` and `oitc-rp-production:32`, image
  `sha256:02a600be5ad7d5336e712529bd23f5ae4e571388cd666c36216c3a53c3042d81`.
  No task definitions were registered/replaced and no coding permissions or secrets
  were added. Existing live issue runners were not modified.
- A real `preparation: coding` resource prepared its private workspace and manifest
  input, logged into Codex, initialized its GitHub token file and development-role
  profile, started the image-local services, and completed an exact echo check.
  Operator checks ran as the same `runner` user as the resource child. Only the
  isolated coding launch added ECS Exec's diagnostic flag; role, environment,
  bootstrap, resource identity and one-send dispatch semantics were unchanged.
- The native child used the expected development role, with a session bound to
  task `8a5e2d91c7f147c4aa2ca84f99d8a011`, through the normal ECS-backed AWS profile.
  No static AWS keys were installed. Workspace mode was 0700 and token-file mode
  0600. Host bootstrap and raw API-key configuration were absent from child env.
  Postgres, MinIO, local DynamoDB and local KMS readiness checks passed.
- Codex CLI 0.156.1 performed a small actual coding task in a fresh local directory:
  implement an ESM slug function and run an independent seven-case verifier.
  Codex exited 0, the independent cases passed, and the verifier remained unchanged.
  Generated-file and captured-output hashes are retained in the evidence. No commit,
  push, issue update, or application-data change was part of the test.
- The unmodified image host's default five-minute renewal loop atomically replaced
  its token file after 300,001 ms; an authenticated GitHub read then succeeded.
  The cached installation token may be reused before expiry. A separate real
  `PrepareResourceWorkspace` check accelerated renewal to five seconds and created
  a fresh GitHub client per renewal: both the original and a different newly minted
  token authenticated successfully. No token or expiry was fabricated. This proves
  actual rotation and the native scheduling cycle; a full natural token-lifetime
  expiry was not awaited.
- All seven additional workspace checks passed, including private files, native AWS
  role assumption, child-env filtering, real token rotation without an issue poller,
  and removal of the workspace, AWS profile and renewal task on close. Three
  supplementary read-only checks also passed: private Git HTTPS authenticated via
  the native token-file askpass helper, and both the source process role and the
  assumed development role received actual AccessDenied responses for coordinator
  state reads against this run.
- Normal EOF completed cleanly and the coding worker exited 0. Strong reads
  confirmed the run closed at revision 34, state version 2, with cleanup complete.
  All three test tasks are STOPPED; CloudTrail confirmed exactly two child launches
  (retired ordinary fixture child and coding resource), with no resubmission.
  Both temporary session-channel policies were removed. Managed revisions 32
  remained ACTIVE. The interrupted ordinary coordinator fixture exited 1; its
  result is separate from the passing coding report.
- An initial ECS Exec attachment was rejected before remote execution because a
  sidecar agent became ready before the application agent. The operator readiness
  check was scoped to `IssueRunnerWorker` and retried on the same coordinator/run.
  The dedicated coding report was successfully published and read back before
  resuming the primary, avoiding the earlier blackhole reporting race.
- Aggregate evidence was stored and read back exactly in the production resource
  bucket at `qualification/operator/m2-coding-dca657ee.json`, SHA-256
  `10fe312553a6410ef48eb7b0ad9fed45cbcda576340dcff557e87751548e0572`.
  It includes the immutable coding report, worker/workspace/authority checks,
  final tasks and metadata, paginated CloudTrail, template/image identities and
  permission cleanup. Operator race tests, vet and Python/Bun syntax checks passed;
  architecture checking retained its four previously recorded baseline violations.

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

- Completed: deploy the helper and isolated dev infrastructure through the OITC
  release path, obtain the actual deployed descriptor, and run the image-local
  echo fixture in a dedicated dev coordinator. Evidence is recorded above.
- Completed: coding-worker preparation, an actual coding task, native renewal-cycle
  observation and accelerated real-token rotation on isolated existing production
  resources, with task-bound development access and coordinator-state denial checks.
  Dev remains echo-only; future qualification tests may use production by explicit
  engineer instruction. The template-replacement retention drill is waived by the
  completion decision above.
- Completed in the additional dev suite: lost successful launch/commit responses,
  paused-send replacement fencing, wrong TLS/bearer, duplicate attachment, EOF and
  disconnect, interrupted cleanup-upload retry, actual coordinator-loss takeover
  and cleanup recovery, and real process-role permission checks. The normal fixture also rejected a stale-owner write.
- Completed: bounded transient connectivity partition, same-socket recovery, and
  replacement fencing during packet loss, as recorded above.
- Completed: injected failed/missing provider observations against real dev tasks,
  bounded attention, replacement/takeover fencing and recovery on positive stop
  evidence, as recorded above. No native AWS outage was induced.
- Completed: sustained blackhole through autonomous client failure and positive
  worker stop, with replacement fencing and terminal cleanup. Behavior evidence
  was independently recovered after the operator report-publication race above.
- Accepted: the engineer closed the M2 cloud contract gate on 2026-10-26 with
  the explicit retention-drill waiver below. Unknown launch outcomes must still
  remain pending/attention; milestone acceptance grants no resubmission authority.
- Completed: review and engineer-authorized manual cleanup of all four earlier
  failed-run records. All are closed with cleanup complete; payloads are sealed,
  and authority records and submission fences are retained. The failed tests remain
  excluded from passing qualification evidence.
- Waived at the engineer's explicit request on 2026-10-26: the template-retention
  replacement drill. No template replacement was initiated and no pass is claimed.
  This supersedes the earlier deferral and removes the remaining M2 acceptance gate.
- Close qualification runs before rollback; do not delete active/ambiguous resource
  storage or relaunch their branches through the legacy route. This gate does not
  qualify Onton ACKs, scheduling or command replay; those remain M3 work.

### Milestone 3: distributed-gameplan-coordinator

**Repository**: Onton. **Dependencies**: M2 and its cloud contract gate — satisfied
by the engineer's M2 completion decision above.

**Status**: Ready for planning; implementation has not started.

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
| M2 acceptance and retention waiver | The engineer accepted M2 as complete on 2026-10-26 and explicitly waived the template-retention replacement drill. Existing deployed retention settings remain; the unperformed drill is not reported as a pass and does not gate M3. |
| Delivery status | M1 passed local build/tests/smoke; release/image qualification is engineer-confirmed, not independently verified here. M2 is complete by engineer acceptance on 2026-10-26, with dev echo, cloud fault/permission, connectivity, observation-recovery and production coding evidence recorded above, and the retention drill explicitly waived. Earlier failed test runs were manually closed and cleaned up by explicit engineer instruction. M3 is ready for planning; M3–M4 are not implemented. |

## Definition of Done (Acceptance Suite)

Each scenario is independent and runs against isolated repositories and run IDs.
The M4 driver invokes the public headless protocol, OITC admission, task APIs,
Git, forge APIs, and durable state observations. Failure injection acts at those
boundaries. Source text is never the behavioral oracle. Proposed fixture command
names are finalized in the owning gameplans; owner files below identify the
implementation boundary. M2's resource commands and files are implemented;
its qualification evidence and explicit retention-drill waiver are recorded above.
M2 acceptance does not claim a full distributed Onton acceptance run. Each
assertion has one owning milestone. M1 fixtures are
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
