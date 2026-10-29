# Opt-in coordinator: resource, forge and control composition

The frozen gameplan is unchanged. The default local runner is unchanged. These
are executable upstream library boundaries for the Patch 8 entrypoint, not a
claim that its pending public/cloud qualification has passed.

## One serialized host

`Coordinator.run` owns one `Coordinator_host`. Intent, Provision, Observe,
Forge_create, Forge_change, authorized Control_line, worker Command/Read,
Recover and Close_run all pass through that host. `Coordinator.serve` composes
the actual owner-only, same-UID Unix socket with external events and worker
readability in that same loop. Socket responses wait for the committed
transition; transport success is not acceptance. The socket and attachments
close on exit. A quiet stream cannot monopolize control intake.

The entrypoint supplies configuration, immutable IDs/manifests, initial helper
open/takeover authority, a clock, Forge.S and destination-explicit Git I/O.
Scheduling views must be decoded from committed owner capsules with
`Worker_checkpoint.validate_binding`, together with initial known planned
patches. `Coordinator_state.scheduling_view` overlays committed observations
and controls; it is not permission to synthesize missing peer/owner state.
Packaging must use these boundaries rather than implement another resource,
Git-owner, polling-policy or ACK path.

## Provisioning and replacement

`Coordinator_resource.provision` atomically commits the exact launch/replace
request, resource-to-patch binding, execution reservation and attempt before
calling the public resource helper. It captures the guard for the metadata
revision immediately after that intent write. Replays use the original persisted
request and guards, not a newly generated launch identity. Read-only exact
envelope refresh reconciles helper metadata revisions before response settlement.

A ready response must identify the matching run/resource and its settlement must
commit before attachment. An ambiguous launch retains its reservation. Three
unresolved attempts, including attempts interrupted by restart, reach durable
attention. A new resource ID cannot bypass a live/ambiguous predecessor.
An unrelated eligible capacity slot can still progress.

Replacement requires a bound, positively retired predecessor, current lifecycle
permission and the operation's persisted allowance/backoff. The existing host
reconstructs recovery Bind from committed stream/capsule/receipt history.
Unfinished operation/attempt identity is preserved. ACKed idle loss uses bounded
maintenance, not a replayed coding session. EOF and uncertain sends record Loss
and attempt Stop; they are never retirement evidence or reconnection permission.
Matching Stop success is required. On restore, retained inventory is fenced
before any new dispatch. Sleeping/cancelling runs cannot launch fresh work.

## Forge observations and policy

Each read commits an immutable run/patch/generation/PR/head/base/destination
ticket before provider I/O. Open PR evidence requires direct head confirmation
at the bound push destination and base confirmation at origin's fetch
destination. Closed/merged PRs require matching provider PR identity and do not
require deleted refs to remain available. Failed probes are not missing refs.

The result, its bounded diagnostic, and its versioned worker input commit in one
transaction. A failed result write returns no confirmed observation. Replaying
the same ticket returns its committed result, not a new provider read; a fresh
poll needs a fresh ticket. Superseded or changed tickets cannot adopt results.
Repeated unavailable/unconfirmed reads reach operation attention after three
failures; bump cannot renew that coordinator allowance. Explicit matching renewal
is required. Successful observations reset the failure counter, not a previously
raised intervention.

`Coordinator_observation.apply` is shared pure owner ingestion: revision and
generation mismatches defer readiness, conflict history remains sticky, and an
identified current discovery may adopt an absent PR but cannot replace another
PR. Snapshots carry full CI/review/queue/native-stack inputs. Scheduling and
workers use the same projection, including Merge_queue_decision removal evidence
and the shared Scheduler_recovery clock/waits. A vanished queue entry cannot
silently become merge-ready; missing removal lookups retain executable repair.
Already checkpoint-consumed observation inputs
are not replayed over newer owner state.

Merge/queue writes do not accept a caller readiness boolean.
`Coordinator_forge.change` takes the shared scheduling view and current clock,
commits fresh pair evidence, and commits the coordinator's 120-second readiness
window. Loss of eligibility removes the window; restart preserves an unchanged
window. The shared scheduler's dependency, approval, conflict, CI, review,
native-stack and queue gates still apply. Metadata/review changes use the
corresponding shared base/review predicates. A provider change after the committed
proof prevents a first mutation. A worker merged-observed result alone cannot
terminate a patch: `terminal_from_forge` requires current committed merge evidence
and settled coordinator intent.

## Forge effect reconciliation

PR create/discovery and body/base/draft/review/queue/merge effects commit exact
immutable scopes and parameters before provider writes. First creation also
directly confirms the captured remote publication pair. Successful results commit
before returning success and survive restart. A replay of a pending effect reads
actual provider state; absence/unchanged eventual state never authorizes another
mutation. Body/review calls without readable post-state remain pending if their
success response was lost. Three unresolved attempts become durable attention.
A fresh effect ID cannot bypass another unresolved mutation on that patch;
explicit matching renewal is separate from bump.

Notes come from validated revision-bound worker checkpoint projections and are
rendered through Feature_pr_body/Pr_body_limit, never another container's paths.
Integration credit still requires the captured current contributor/root pair and
matching owner receipt. Forge candidate flags are not contribution credit.

## Controls and versioned delivery

Controls are exact-schema v3 envelopes, bounded and run/config scoped. Accepted
IDs and exact payloads remain for the run; changed reuse rejects. Guidance,
automerge and bump also create unconsumed inputs atomically with acceptance.
Renewal names existing durable attention; bump never renews publication or a
coordinator recovery/observation allowance.

`Coordinator_inputs.pending` builds a deterministic cumulative packet containing
every unconsumed identity/version, ordered guidance and controls, and committed
forge evidence. The current v3 input schema carries this explicit coverage.
The worker applies it at a safe boundary and checkpoints the input and owner
together. Only that validated checkpoint consumes the covered IDs. New input
arriving during a turn cannot retarget its captured work, overwrite older
guidance or be consumed by an old result. Recovery restores exact input bytes.
Host delivery checks evidence/control sources against committed history.

## Sleep, cancellation and status

Sleep commits a generation-bound drain fence, settles active protocol work,
requests Stop and confirms retirement. It does not close/seal the run. Wake
invalidates the prior generation and sleep-ready projection; a sleeping restore
awaits authorized wake. Cancellation dominates late wakes, sends matching active
Cancel commands, retires resources and persists exact terminal Close intent.

Successful Close seals opaque storage. Its matching helper response/tombstone is
the durable cleanup evidence: the host projects completion without trying to
write another checkpoint to sealed storage. The last opaque envelope remains the
pre-close intent. A sealed-run restart must be resolved through the public helper
tombstone, not by manufacturing an empty/new run when state-read is closed.

Headless JSONL is emitted after committed lifecycle settlement. It contains
run/config, lifecycle/sleep generation, aggregate counts, and up to 128 attributed
patch/operation/assignment/resource rows with executing, idle, capacity wait,
recovery or attention status. It never includes capsule, receipt, request,
control payload, notes or credentials and grants no authority.

## Verification boundary

Executable coverage includes:
- `test_coordinator_resource`: commit rejection, exact replay after lost launch
  response/restart, post-intent metadata guards, retirement-gated replacement,
  preserved unfinished identity, bounded ambiguity, unrelated slot progress,
  explicit renewal and terminal close without writes to sealed storage.
- `test_coordinator_controls_forge`: observation result barriers, durable policy
  adoption, versioned owner evidence, merge idle-window boundaries/restart,
  successful/ambiguous effect settlement, immutable identities and bounded waits.
- `test_coordinator_control_socket`: real same-UID intake, serialized durable
  responses, duplicate/changed controls, sleep/wake status, uncertain storage and
  socket teardown.
- Pure-only journal/observation/input properties, plus existing host, stream,
  recovery, state-model, worker and real Git suites. The real coordinator loop
  provisions and attaches the real worker and verifies checkpointed automerge
  delivery.

The opt-in `onton-coordinator` entrypoint now composes the mainline path, and
`test_distributed_coordinator` runs real coordinator/worker processes rather than
a pending stub. Release packaging includes the coordinator and qualifies the
extracted coordinator, worker and session supervisor through
`@distributed-coordinator-smoke`. See [operation and qualification](../distributed-coordinator-host.md).
Feature-root execution is explicitly rejected by this entrypoint. The full
provider-timing/root-loss matrix, FC-15/FC-16 closure and deployed-cloud M3 proof
remain unqualified; the local process fixture is not permission to advance M4.
