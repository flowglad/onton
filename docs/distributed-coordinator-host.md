# Distributed coordinator: executable and durable host boundary

The original repair supplied the missing Patch 6 worker-stream prerequisite.
The follow-up closes resource execution, durable forge-result, worker-input and
serialized control/status seams; see [the composition contract](design/distributed-coordinator.md).
The opt-in mainline executable and local multi-process acceptance now consume
these boundaries. Full FC-15/FC-16 and deployed-cloud qualification are not
implied. The frozen gameplan is unchanged.

## Owned guarantees

`Worker_stream` is the pure inbound-frame host reducer for the current v3 wire
contract. It reuses the shared worker transition semantics, but receives decoded
worker frames rather than accepting caller assertions about worker events.
Checkpoint/result identities, versions and exact generated messages must match.
Caller-supplied ACKs cannot enter the command boundary.

The durable stream representation contains the exact capsule, derived protocol
receipt and validated replay history. Import recomputes the pair and rejects any
disagreement. There is one current coordinator envelope schema, with no legacy
schema import or protocol compatibility shim.

`Coordinator_host` commits the stream and coordinator intent in the same run
transaction, confirmed by known write receipt **and exact current-byte readback**:

1. Bind and Execute admission are committed before their frames are returned.
   Execute's operation/attempt must match the reserved coordinator assignment.
   The worker's first checkpoint remains the barrier before checkout/backend work.
2. Inbound checkpoints are projected through `Worker_checkpoint`, checked against
   the bound scope, operation and exact delivered input, and committed with their
   derived receipt before Checkpoint ACK is returned.
3. Result must exactly match the retained terminal capsule, including status,
   diagnostics and renewal identity. Result is the worker protocol's cleanup
   attestation: the real worker emits it only after its cleanup barrier.
   Terminal receipt/result adoption is committed separately from the post-ACK
   receipt and reservation release. Result ACK is returned only after both writes.
4. Failed, rejected or uncertain writes expose no frames. Exact same-owner retries
   retain their original write identity. Duplicate checkpoints/results/input are
   idempotent; a conflicting duplicate or stale/retired/superseded owner is refused.
5. Recovery derives Bind from the committed predecessor stream, never supplied
   receipt bytes. Replacement requires matching proven retirement. Unfinished
   operation/attempt identities are retained; acknowledged-idle replacement uses
   a fresh operation. A consumed attachment is not reconnected.

`Coordinator_state.capsules` advances the receipt for unchanged capsule bytes at
terminal and post-ACK phases using an exact predecessor receipt. The envelope
loader also checks that stream and coordinator artifacts agree.

All recognized result statuses are retained durably. Failed/retryable work keeps
intent within its allowance, Attention remains intervention work, and Cancelled
only completes intent under persisted cancellation context. No result invents
forge readiness, publication or merge evidence. Maintenance assignments reserve
capacity, cannot request a coding session, and retain their allowance across
completion/restart/repeated idle loss. Ordinary growth preserves emergency space
for stop/retirement and lifecycle settlement; oversized prospective envelopes do
not install a pending write that would prevent that repair work.

## Consumer API

- `Coordinator_host.submit`: coordinator intent only. `Dispatch` means a capacity
  reservation, **not** permission to bypass protocol admission or send Execute.
- `Coordinator_host.command`: exact current-wire Bind/Execute/Input/control;
  only returned frames may be sent.
- `Coordinator_host.receive`: actual inbound worker frame plus current time and
  gameplan context; only returned ACKs may be sent.
- `Coordinator_host.recover`: committed predecessor identity plus replacement
  assignment/worker/workspace IDs, after retirement proof.
- `Coordinator_host.retired`: exact persisted Stop request and successful matching
  helper response, never observe-ok, stream EOF or an arbitrary cleanup assertion.

The former caller-receipt `checkpoint`, bare `result` and assertion-only `cleanup`
entrypoints are removed. Unconsumed coordinator inputs survive recovery and must
be redelivered; only the worker's checkpoint can mark them consumed.

`Coordinator.run` supports raw single-use Attach, Command, Read and Recover events
alongside coordinator Intent events. The attachment callback supplies
nonblocking readiness so quiet workers do not monopolize control/event selection.
Sequential assignments can reuse their existing attachment.
`Coordinator_stream_io` bounds frame reads and writes,
retains buffered complete frames, and reports partial/timeout sends as uncertain.
The loop closes attachments on errors/exit. Provision drives exact persisted
resource intent through helper execution, settlement, attachment and optional
recovery; resource loss drives persisted Stop and requires positive retirement.
Observe/Forge_create/Forge_change execute through the same host. The optional
serve composition runs the actual control socket, event source and worker
readability with committed headless output. Provider capabilities/configuration
remain injected; resource lifecycle and forge policy are no longer left for
packaging to implement. Cloud launch/takeover qualification is not inferred from
these local fixtures.

Two worker-side boundary fixes accompany the host:

- An active turn retains its captured base/guidance until the terminal safe
  checkpoint; pending input cannot change the base behind an old capsule input.
- Checkpoint receipts retain the capsule's Attention projection, so replacement
  can bind an interrupted Attention checkpoint before Result arrives. A recovery
  checkpoint without a terminal status clears that provisional Attention field.

## Opt-in executable

The default `onton` local path is unchanged. Install `onton-coordinator`,
`onton-worker` and `onton-setsid-exec` together. The coordinator needs a read
checkout with directly observable origin refs, a public resource-v1 helper and
GitHub credentials (or an explicit command forge adapter). The helper supplies
worker repository/authentication/backend environments; this executable does not
install or qualify a cloud image.

Use the exact current, nonsecret configuration schema:

```json
{
  "version": 3,
  "capacity": 2,
  "repo_root": "/srv/onton/read-checkout",
  "main_branch": "main",
  "worker_executable": "onton-worker",
  "backend": "codex",
  "execution_mode": "mainline"
}
```

For a confirmed new run:

```sh
onton-coordinator --gameplan gameplan.yaml --config coordinator.json \
  --run-id "$RUN_ID" --config-id "$CONFIG_DIGEST" \
  --coordinator-id "$ACTUAL_TASK_ID" --resource-helper /usr/local/bin/resource-helper \
  --control-socket /srv/onton/private/control.sock --fresh
```

The supervisor supplies real run/config/task identities, not fixture identities.
For restart use the same exact plan/config bytes and replace `--fresh` with
`--takeover-epoch N --takeover-revision N`, using current public helper metadata.
Unknown ownership, incompatible bytes, uncertain bootstrap and unsupported
execution modes fail closed. Feature-root execution is explicitly unsupported by
this entrypoint. The control socket parent must be owner-only; crash residue is
not blindly unlinked.

The runtime restores owner projections, uses shared scheduler/reconciler decisions,
persists selected work before provisioning and retains completed idle workers.
Intentional wake uses bounded maintenance operations after positively retired,
acknowledged owners; it does not replay implementation. Queued human work is
executable and remains ahead of rebase work. Merge idle windows are journaled
through shared policy, including loss of eligibility. Forge mutations still
recompute policy at the owning boundary; source selection is not permission.

The server backpressures tick/readability producers until the preceding event
has completed; duplicate readiness cannot fill the control queue. Regular forge
polling is bounded to one poll per patch per 30 seconds. Worker checkpoint waits
are 60 seconds for this composition. Cancellation sends one committed request to
active turns, retires unstarted reservations through actual helper Stop evidence,
and exits only after confirmed helper Close. A late cancellation request cannot
rewrite a terminal result captured before it; Result must still match the exact
committed terminal capsule. No ACK or reservation release is inferred from Cancel.

The single current opaque envelope is `onton-coordinator-3\n` followed by the
lossless bounded-window archive codec. It retains every checkpoint, receipt,
journal entry and protocol event; compression is not history eviction. The
physical helper payload remains bounded to 8 MiB; decoded envelopes to 64 MiB;
each decoded stream and logical coordinator state to 8 MiB. Prospective growth
preserves emergency headroom and is rejected before installing an unrecoverable
pending write. Old opaque envelope formats are not imported.

`ONTON_COORDINATOR_TRACE_POLICY=1` emits changed per-patch readiness diagnostics,
not credential or authority bytes. Headless status remains bounded and attributed.

## Verification

- `test_worker_stream_properties`: totality, version boundaries, generated
  checkpoint/stale/restart interleavings, exact receipt/capsule roundtrips,
  duplicate idempotence, rejected caller ACKs and interrupted Attention recovery.
  Links `onton_core` only.
- `test_coordinator_result_properties`: all result statuses, cancellation context,
  retry exhaustion, receipt predecessor binding, maintenance allowance across
  restart, and emergency stop capacity. Links `onton_core` only.
- `test_coordinator_stream`: real worker/backend/Git execution through the durable
  host; rejected/lost/unconfirmed writes and missing readback at Bind, admission,
  input, checkpoint, terminal and post-ACK barriers; stale and tampered evidence;
  sequential reuse; idle replacement; and replacement after a checkpoint commit
  whose ACK was deliberately not delivered. Also exercises `Coordinator.run`
  through a raw public-helper attachment to the real worker.
- `test_coordinator_stream_io`: buffered framing, EOF, partial-frame timeout and
  bounded uncertain sends under backpressure.
- Resource lifecycle, durable forge/effect, cumulative input and actual socket
  coverage is listed in the composition contract above.
- `test_coordinator_archive_properties`: byte roundtrip, deterministic encoding,
  strict decode bounds, malformed/truncated histories, trailing bytes, overlapping
  copies and 64 KiB window boundaries. Links `onton_core` only.
- `test_coordinator_config_properties`: strict schema/decoder totality, durable
  work roundtrips, fail-closed body receipts and shared reconciliation idempotence.
  Links `onton_core` only.
- `test_distributed_coordinator` / `@distributed-coordinator-smoke`: actual
  coordinator and worker processes, separate repositories/HOMEs, a fake backend
  making real commits, and public resource/forge adapters. Covers five independent
  patches at capacity two, lost successful bootstrap-write response, actual
  coordinator kill/takeover, dead-predecessor workspace removal, replacement
  without reconnect, guidance duplicate/conflict, unique owned PRs, origin
  contribution content, physical backend overlap, idle retention, sleep/wake
  maintenance and cancellation/Close. No pending-success path remains.
- Release packaging includes all three distributed executables and runs that
  composition against extracted paths. To reproduce, set
  `ONTON_COORDINATOR_BINARY`, `ONTON_WORKER_BINARY`, and
  `ONTON_SETSID_EXEC_BINARY` to the extracted executables and build
  `@distributed-coordinator-smoke --force`.
- This is local public-contract acceptance, not the full provider timing/root-loss
  matrix, independent executable proof of every A09–A14/A22 case, or deployed
  cloud M3 qualification. Do not infer FC-15/FC-16 closure or advance M4 from it.
- Existing coordinator storage/state-machine, worker v3 and public-worker suites
  remain regression coverage. Passing pending stubs is not acceptance evidence.
