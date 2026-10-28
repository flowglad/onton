# Distributed coordinator: host boundary for Patch 7

This repairs the missing Patch 6 worker-stream prerequisite. It is not
qualification of the Patch 7 forge/control suite or the Patch 8 composed/cloud
suite. Those suites remain explicitly pending. The frozen gameplan is unchanged.

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
The loop closes attachments on errors/exit. Resource provisioning and event
selection remain injected capabilities; cloud launch/takeover qualification is
not inferred from the local stream fixture.

Two worker-side boundary fixes accompany the host:

- An active turn retains its captured base/guidance until the terminal safe
  checkpoint; pending input cannot change the base behind an old capsule input.
- Checkpoint receipts retain the capsule's Attention projection, so replacement
  can bind an interrupted Attention checkpoint before Result arrives. A recovery
  checkpoint without a terminal status clears that provisional Attention field.

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
- Existing coordinator storage/state-machine, worker v3 and public-worker suites
  remain regression coverage. Passing pending stubs is not acceptance evidence.
