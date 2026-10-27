# Headless patch worker protocol v2 (default) and v3 (opt-in)

This is the local, trusted stdin/stdout contract for the opt-in `onton-worker` executable. Each direction carries one UTF-8 JSON object per line. Stderr is diagnostics only. The existing `onton --headless` scheduler keeps its local execution route.

## Invocation and local qualification

```sh
dune build @worker-protocol-smoke
ONTON_WORKER_BINARY=/absolute/path/to/onton-worker dune build @worker-protocol-smoke --force
```

The first command builds and drives `bin/worker.exe` through the public JSONL entry point. The override drives an explicitly supplied binary with the same harness, for the later exact-image gate. The smoke uses local bare Git repositories, separate clones, a fake Codex executable, a local forge observation fixture, and a host durability simulator; it does not require AWS or a GitHub credential. `dune build && dune runtest` runs the source-change gate. Ordinary `onton` is a separate executable.

Release archives contain `onton`, `onton-setsid-exec`, and `onton-worker`.
The release workflow extracts each archive and runs the protocol smoke against
that extracted worker before publishing. Linux builds additionally check worker
startup in Ubuntu 22.04. The Homebrew formula template installs all three tools.

OITC image qualification is a separate gate: its Dockerfile must extract and
install `onton-worker` from the pinned release alongside the existing tools.
Run the harness in the qualification environment with `ONTON_WORKER_BINARY`
pointing to that installed binary, and record the source revision, release,
archive checksum, image digest, installed worker checksum and command exit status.
An archive smoke does not qualify a downstream image.

The worker CLI is:

```sh
onton-worker --gameplan FILE --repo-root DIR --project-name NAME \
  [--main-branch BRANCH] [--backend codex|claude] [--ack-timeout SECONDS]
```

The spawning wrapper provides the immutable gameplan file, managed repository root and workspace configuration. Defaults are `main`, `codex`, and a ten-second checkpoint acknowledgement timeout; the timeout must be greater than zero and at most 60 seconds. The worker reads Git credentials from the existing Git transport environment. Forge observations use `ONTON_GITHUB_TOKEN_FILE` (read again on each observation) or `GITHUB_TOKEN`. Missing or failed forge evidence yields a typed retryable result where merge status matters. No credentials are carried in protocol frames or capsules. The executable does not start the scheduler, poller, local control socket, or periodic persistence fiber.

The public test uses `ONTON_WORKER_BINARY_DEFAULT` only as Dune's built-binary path. An explicit `ONTON_WORKER_BINARY` takes precedence. The test supplies `ONTON_SETSID_EXEC` for process-tree supervision.

`ONTON_WORKER_FORGE_FIXTURE` is a local qualification hook supplied only to the smoke subprocess. Its file contains exactly `{"branch":"<assigned branch>","state":"merged|not_merged|unavailable"}`. The worker re-reads it for each forge observation, allowing the harness to change provider evidence between operations. Do not set it in a production host; absent this variable, the worker uses the GitHub client and renewable token path.

## Capsule v1

The host stores the exact UTF-8 `capsule` bytes from each acknowledged checkpoint, along with the protocol admission receipt in one conditional storage envelope. The capsule is an object with exactly `version:1`, `run_id`, `patch_id`, `workspace_id`, `context`, decimal-string `state_version`, nullable `operation`, `guidance`, `agent`, nullable `terminal`, and nullable `attention_id`. `operation` contains `operation_id`, `attempt_id`, and decimal-string `assignment_seq`; `terminal` contains `status`, `diagnostics`, and nullable `renewal_id`. `agent` is the assigned patch's serialized `Patch_agent` record, including the reconciliation owner and session state. It is not a whole Runtime snapshot or gameplan authority. The gameplan file supplies immutable patch context at process start. The adapter rejects mismatched run, patch, version, context, attention receipt, patch branch, or missing required owner/session fields before constructing Runtime. It uses patch codecs directly and never calls `Persistence.load`, so no local sidecar overlay or archived conversation can become host authority.

The workspace ID names the worker's current checkout authority. A changed workspace requires `lost_workspace` recovery; checkpointed loss clears the old path, conversation ID, and captured session completion before backend work. The host must restore its last actually committed envelope, including a newer capsule after a lost ACK. The adapter observes Git and forge evidence from that state; it never rolls the host state back to the last ACK it happened to see.

## Framing and validation

A complete frame, including `\n`, is at most 1,048,576 bytes. EOF with a partial frame, invalid UTF-8, embedded newline, duplicate object keys at any depth, nonfinite numbers, unknown keys, malformed shapes, and NUL text are rejected. Identifiers are opaque, nonempty UTF-8 strings of at most 256 bytes; they are neither paths nor credentials. Counter values are canonical, nonnegative base-10 strings in signed int64 range (`"0"`, `"1"`, …; no leading zeros). Text and capsule string fields are bounded to 256 KiB. Version is the JSON number `2`; an otherwise parseable frame with another version is incompatible. A complete malformed request yields `protocol_error` without effects. Broken framing, incompatible version or binding terminates the connection after cleanup.

Every frame has `version`, `type`, `run_id`, and `patch_id` except an unbound `protocol_error`, which has no run or patch. Operation frames additionally have `operation_id`, `attempt_id`, and `assignment_seq`. IDs never prove host authority. The trusted spawning wrapper must authenticate cloud messages, serialize assignments, and confirm the predecessor process and descendants have terminated before replacement. Heartbeat absence and Git leases do not prove that.

## Host to worker

Required fields are shown in the examples. Optional values are represented explicitly by JSON `null`; omitted fields are invalid. Unknown fields are invalid. `capsule` is an opaque, canonical serialized patch-execution capsule supplied and interpreted by the worker adapter; semantic capsule validation is outside the pure codec. `attention_id` is the outstanding disappearance receipt extracted from that accepted capsule, if any.

```json
{"version":2,"type":"bind","run_id":"r","patch_id":"p","worker_id":"w","workspace_id":"ws","context":"requirements and configuration","capsule":"{}","state_version":"0","attention_id":null,"admission":{"mode":"fresh"}}
{"version":2,"type":"execute","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","expected_state_version":"0","work":{"type":"session","phase":"start","kind":null,"turn":"Implement this patch"},"guidance":"current guidance","recovery_mode":"retained_workspace","renewal_id":null,"renewed_intent":false}
{"version":2,"type":"execute","run_id":"r","patch_id":"p","operation_id":"o2","attempt_id":"a2","assignment_seq":"2","expected_state_version":"3","work":{"type":"reconcile"},"guidance":"reconcile patch branch","recovery_mode":"lost_workspace","renewal_id":"receipt-7","renewed_intent":true}
{"version":2,"type":"cancel","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1"}
{"version":2,"type":"checkpoint_ack","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","proposal_id":"cp-1","state_version":"1","capsule":"frozen capsule bytes","accepted":true}
{"version":2,"type":"result_ack","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","state_version":"3"}
{"version":2,"type":"shutdown","run_id":"r","patch_id":"p"}
```

Session phases are `start` and `respond`; `kind` is optional text represented as `null` or string, and `turn` is required. The pure decoder accepts only the current `Types.Operation_kind` labels (`uncommitted-changes`, `rebase`, `human`, `merge-conflict`, `ci`, `review-comments`, `pr-body`, `findings`). Only assigned patch sessions and patch-local reconciliation are supported. Root integration, scripted gameplan publication, general forge management, and whole-gameplan dispatch are unsupported. Unknown work shapes fail decoding. `recovery_mode` is `retained_workspace` or `lost_workspace`.

## Worker to host

```json
{"version":2,"type":"ready","run_id":"r","patch_id":"p","worker_id":"w","state_version":"0"}
{"version":2,"type":"accepted","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","replay":false}
{"version":2,"type":"observation","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","text":"fetch started"}
{"version":2,"type":"checkpoint","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","proposal_id":"cp-1","previous_state_version":"0","state_version":"1","capsule":"frozen capsule bytes"}
{"version":2,"type":"result","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","state_version":"3","status":"completed_ok","diagnostics":"done","renewal_id":null}
{"version":2,"type":"protocol_error","run_id":"r","patch_id":"p","reason":"assignment not admissible"}
```

The result `status` is one of `completed_ok`, `completed_no_commits`, `failed`, `cancelled`, `retryable`, `attention`, or `merged_observed`. Only `attention` carries a nonnull `renewal_id`. `attention` means unexplained disappearance of possibly published work requires an explicit renewed instruction. `retryable` means evidence or execution is temporarily unavailable; `merged_observed` reports a confirmed merge. `observation` and transcript chunks are bounded, non-authoritative telemetry. A pre-bind `protocol_error` omits `run_id` and `patch_id`.

## Lifecycle and durability

The first `bind` fixes the run, patch, worker and workspace identity, context, initial capsule, version and attention receipt. An exact bind replay is harmless; any changed bind terminates the connection. Assignment sequence starts at `"1"` and increases by exactly one after each acknowledged result. An `execute` is admitted only when its run/patch binding matches, expected version equals the acknowledged version, the prior result is acknowledged, and cleanup completed. Repeating an identical execute replays acceptance (and a retained terminal result) without beginning execution again. Reusing an operation, attempt, or sequence with changed content is rejected, including after cached results expire. The process remains bound to the same run and patch across sequential operations.

A checkpoint freezes its proposal ID, version and capsule bytes. The host conditionally stores those exact bytes under current authority and replies with an ACK matching the full operation identity, proposal ID, version and capsule. Only an accepted exact ACK advances the pure state version. Duplicate exact ACKs are harmless. Rejection cancels and stops the worker; a lost ACK may have committed at the host, so readmission must retrieve the host's actual stored version and observe remote effects. The adapter must not run any dependent effect before the ACK. Its ACK mailbox must not acquire the Runtime mutex while a persistence callback waits for ACK.

The worker emits a terminal result only after acknowledged state and completed descendant cleanup. The next assignment waits for `result_ack`; stale ACKs, cancels and changed replays cannot affect a successor. Active shutdown cancels and stops; idle shutdown exits. Host EOF, broken stdout, checkpoint rejection or ACK timeout cancels execution and terminates active backend descendants before the worker exits. The spawning host must enforce a hard deadline of the configured ACK timeout plus 5 seconds after disconnect or rejection (at most 65 seconds, since the CLI caps ACK timeout at 60 seconds), then kill the worker process group if necessary and confirm all descendants have exited before replacement. The local smoke uses a 1-second ACK timeout. No arbitrary agent shell or API side effect is promised exactly once.

For a possibly published branch that disappeared, plain retry and old guidance cannot recreate it. The host must supply `renewed_intent:true` and the exact outstanding `renewal_id` in a new logical operation and attempt after the prior result ACK. A wrong, consumed, or superseded receipt is rejected. Exact replay of the accepted renewal is idempotent. Wire admission is not Git mutation authority: the owner re-observes branch, base, and forge state; a merge or reappeared branch takes precedence. Publication history remains possibly published.

Pure properties exercise the codec and transition algebra. The exact-binary smoke exercises checkpoint barriers, sequential use, replacement, malformed framing, and backend descendant cancellation. The local simulator models conditional storage and ACK delivery; cloud authentication, conditional writes and task-stop evidence remain host obligations.

## Required replacement-worker contract (FC-1 amendment)

The pure `Worker_protocol` owner implements durable admission in protocol v2.
Version 1 is incompatible: a replacement must not silently discard durable
admission evidence. The worker adapter supplies persistence and subprocess integration;
receipt validation does not itself perform storage or fence a previous process.

Every bind must include an `admission` object selecting `fresh` or `resume`. `fresh` is
valid only when the host has durably established that this run/patch has never
admitted an assignment. A nonzero capsule version alone is neither a sequence
receipt nor permission to start again at sequence 1. `resume` must carry a
host-committed, patch-scoped predecessor receipt. Missing, malformed,
inconsistent, or unsupported receipts fail binding before `ready` or execution.
The authenticated host supplies the receipt after fencing the previous process
and descendants; the receipt does not itself establish exclusive ownership.

The receipt must retain:

- The run/patch identity and last admitted assignment sequence.
- The exact decoded Execute request for each admitted identity, including
  operation ID, attempt ID, sequence, expected state version, work, guidance,
  recovery mode and renewal intent. Retain identity reservations for the whole
  run/patch lifetime; result eviction must not permit identity reuse.
- The last host-committed capsule and state version, together with outstanding
  attention and consumed renewal receipts.
- The predecessor disposition: `unfinished`, `terminal_unacknowledged`, or
  `terminal_acknowledged`. A terminal disposition includes the exact terminal
  result, full operation identity and committed state version. Terminal results
  attest completed descendant cleanup. An acknowledged disposition additionally
  includes the host's durable Result ACK receipt for that exact result.

The wire represents unfinished as `terminal:null,result_acknowledged:false`,
unacknowledged terminal as a terminal object with `result_acknowledged:false`,
and acknowledged terminal as a terminal object with `result_acknowledged:true`.
An ACK without a terminal result is invalid. The ACK flag is an explicit
authenticated host attestation for the enclosing request and exact terminal
result, not an inference from the capsule version. The pure owner validates
identity, sequence, state-version and renewal consistency before readiness. The capsule codec
must persist these owner facts without reconstructing them from inferred Git
state or treating a successful bind as acknowledgement.

After restoration, an exact Execute replay never emits `Begin`. It replays
acceptance and the retained terminal result, if present. Changed content or
reuse of a reserved operation, attempt or sequence is rejected. An unfinished
predecessor remains ambiguous: neither exact replay nor a new assignment may
start effects. The adapter explicitly supplies `Recover_unfinished` to obtain one `Recover`
output carrying the interrupted request. This authorizes observation and repair
from the committed capsule, never rerunning the original request. Recovery must
commit a new checkpoint and complete cleanup before finishing that same
assignment. Only its acknowledged terminal result releases the next sequence.
Replacement during recovery restores an unfinished receipt again.

A terminal unacknowledged predecessor permits result replay and an exact
Result ACK, but blocks the next assignment. The host must commit its ACK receipt
before sending Result ACK; a replacement supplied that receipt can restore the
acknowledged state even if the prior process never received the ACK. A terminal
acknowledged predecessor permits only sequence N+1 with the current committed
state version and valid renewal intent. Sequence exhaustion admits no successor.
Duplicate exact ACKs are harmless and stale ACKs cannot release a successor.

The host must durably reserve an admitted request before allowing its first
effect, commit terminal state before acknowledging its result, and preserve
those facts when replacing a process. A worker-side in-memory acceptance is not
a durable reservation. Failure between reservation and effect remains unfinished
and ambiguous, preserving safety without claiming exactly-once arbitrary effects.

The public pure-interface properties cover restoring after at
least three assignments, admitting sequence 4 only after durable ACK, rejecting
sequence 1 reset and altered replay, replaying a lost-ACK terminal result without
`Begin`, blocking unfinished attempts, rejecting invalid receipts, preserving
renewal consumption and identity reservations, and handling sequence exhaustion.
The subprocess smoke withholds the first checkpoint ACK, terminates that worker,
and verifies that a replacement enters observation recovery from the committed
admission receipt. It also tests exact checkpoint rejection and subsequent
same-process assignments. The pure interface tests cover result ACK delivery
interleavings and terminal replay.

### Receipt schema and adapter API

`Worker_protocol.admission_receipt` exports the required `admission` JSON object
from an immutable protocol state. An unbound state returns `None`; a bound state
with no admissions exports `{"mode":"fresh"}`. Fresh denotes a new admission
sequence, which may start from an existing execution capsule and state version.
The host must not use it once any assignment has been reserved for this run/patch.

A resume object has exactly `mode`, `initial_state_version`,
`initial_attention_id`, `state_version`, `capsule`, `attention_id`, and `entries`.
The last three state fields must equal the outer bind. Entries are chronological
and nonempty; they start at assignment sequence 1 and increase without gaps.
Every entry has exactly `request`, `terminal`, and `result_acknowledged`.
`request` is the complete v2 Execute JSON object. A terminal object has exactly
`state_version`, `status`, `diagnostics`, and `renewal_id`; its identity is the
entry's request identity. Its version must exceed the request's expected version,
and a successor must expect that terminal version. Only the final entry may be
unfinished or unacknowledged. Result status and renewal constraints are the same
as the live result contract. Initial attention plus accepted requests and terminal
results reconstruct consumed renewals and outstanding attention; these cannot
be reset independently on replacement.

For example, the bind above can resume one completed operation with:

```json
{"mode":"resume","initial_state_version":"0","initial_attention_id":null,"state_version":"1","capsule":"committed capsule","attention_id":null,"entries":[{"request":{"version":2,"type":"execute","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","expected_state_version":"0","work":{"type":"reconcile"},"guidance":"reconcile","recovery_mode":"retained_workspace","renewal_id":null,"renewed_intent":false},"terminal":{"state_version":"1","status":"completed_ok","diagnostics":"done","renewal_id":null},"result_acknowledged":true}]}
```

`Begin` is internal dispatch only. The worker immediately proposes an admission
checkpoint and waits for its exact accepted ACK before running any owner event,
checkout, hook, backend or Git effect. The host commits the post-admission
receipt and proposed capsule together before issuing that ACK. For a
checkpoint or result ACK, the host can compute the prospective immutable
`step` transition and export its receipt, commit that receipt and the matching
capsule atomically, then deliver the ACK. Computing a prospective transition is
not execution or proof of a storage commit. If commit fails, do not deliver the
ACK or perform dependent effects. On replacement always bind the last actually
committed pair, including when the old process never received its ACK.

Receipts share the bind frame's 1 MiB bound. History cannot be truncated to fit;
an oversized receipt prevents readmission. The adapter must check prospective
bind size before committing admission and report capacity exhaustion rather than
start an operation it cannot durably restore. Retained requests and terminal
results preserve exact replay for the run/patch lifetime.

Persist the admission object alongside the opaque execution capsule in the host
storage envelope. Do not insert it recursively into the capsule string supplied
to the protocol; the resume object already binds those exact capsule bytes.

## Opt-in v3 pure contract (M3)

The default worker and explicit v2 continue to use the wire and receipt above.
V3 is a distinct, explicitly selected schema: a v2 line sent to the v3 decoder
(or a v3 line sent to the v2 decoder) fails closed. A worker process must not
switch protocols after binding. `Worker_protocol_v3` provides the *pure* library
seam; effectful v3 worker CLI selection/execution belongs to the subsequent
worker-execution patch. It does not make the current v2 executable a v3 worker.

V3 frames retain the v2 identity, counter, UTF-8, duplicate-key, text and 1 MiB
line rules, with numeric `version:3`. Bind additionally requires
`execution_context`, a canonical JSON *string* (up to 256 KiB) containing exactly
`version:3`, `run_id`, `patch_id`, `config_id`, `plan_id`, `message_id`,
`generation` (decimal counter), `dependencies` (list of objects with `patch_id`,
`admitted`, `revision`), `selected_base`, `execution_mode` (`feature|mainline`),
`root_role` (`root|patch`), `admitted`, `evidence_id`, `guidance_ids` (list of IDs),
and `input_version` (decimal counter). Root role requires feature mode. Bind
and execute scopes must agree with the context. Execute repeats the exact
execution context and supplies `input_version`; its work is either the existing
session/reconcile shape or `{type:"integrate_revision",contributor,revision}`.
Missing/unknown fields, bad scopes and changed request identities reject.

V3 input is a separate `{version:3,type:"input",run_id,patch_id,input_id,
input_version,guidance,selected_base}` frame. Versions start at 1 and increase
by one. A matching exact replay is harmless; changed ID reuse, gaps and counter
exhaustion reject. `Input_committed(id,version)` is a *host attestation*, not a
storage write: the host must atomically commit the exact input before emitting
it. Only a matching committed input updates the desired base or authorizes
application. An older unfinished Execute retains its original binding. Notes
are bounded to 256 KiB, with distinct missing, empty, unreadable and oversized
outcomes; acceptance requires exact assignment and publication binding. Storage
confirmation for notes and their bytes is a later effectful host obligation.

The pure v3 host delegates admission, checkpoints, terminal-versus-ACK and
unfinished recovery to `Worker_protocol.step`. Its shared receipt records a
canonical v3 Execute frame in the v2 request's guidance field, so all extended
work, context and input bindings participate in replay equality and are
reconstructed on replacement. The shadow request is never backend guidance.
Receipts and capsules must be committed together; a prospective pure `step`
transition, a transmitted checkpoint, or an unconfirmed storage write cannot
justify an ACK or worker effect. A context too large to fit the v2 guidance
bound is rejected before admission rather than truncated. V3 resource identities
remain outside protocol assignment identities and receipts.
