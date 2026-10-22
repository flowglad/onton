# Headless patch worker protocol v1

This is the local, trusted stdin/stdout contract for the opt-in `onton-worker` executable (wired in Patch 6). Each direction carries one UTF-8 JSON object per line. Stderr is diagnostics only. This document fixes the wire contract; it does not activate the existing `onton --headless` scheduler.

## Framing and validation

A complete frame, including `\n`, is at most 1,048,576 bytes. EOF with a partial frame, invalid UTF-8, embedded newline, duplicate object keys at any depth, nonfinite numbers, unknown keys, malformed shapes, and NUL text are rejected. Identifiers are opaque, nonempty UTF-8 strings of at most 256 bytes; they are neither paths nor credentials. Counter values are canonical, nonnegative base-10 strings in signed int64 range (`"0"`, `"1"`, …; no leading zeros). Text and capsule string fields are bounded to 256 KiB. Version is the JSON number `1`; an otherwise parseable frame with another version is incompatible. A complete malformed request yields `protocol_error` without effects. Broken framing, incompatible version or binding terminates the connection after cleanup.

Every frame has `version`, `type`, `run_id`, and `patch_id` except an unbound `protocol_error`, which has no run or patch. Operation frames additionally have `operation_id`, `attempt_id`, and `assignment_seq`. IDs never prove host authority. The trusted spawning wrapper must authenticate cloud messages, serialize assignments, and confirm the predecessor process and descendants have terminated before replacement. Heartbeat absence and Git leases do not prove that.

## Host to worker

Required fields are shown in the examples. Optional values are represented explicitly by JSON `null`; omitted fields are invalid. Unknown fields are invalid. `capsule` is an opaque, canonical serialized patch-execution capsule supplied and interpreted by the worker adapter; semantic capsule validation is outside the pure codec. `attention_id` is the outstanding disappearance receipt extracted from that accepted capsule, if any.

```json
{"version":1,"type":"bind","run_id":"r","patch_id":"p","worker_id":"w","workspace_id":"ws","context":"requirements and configuration","capsule":"{}","state_version":"0","attention_id":null}
{"version":1,"type":"execute","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","expected_state_version":"0","work":{"type":"session","phase":"start","kind":null,"turn":"Implement this patch"},"guidance":"current guidance","recovery_mode":"retained_workspace","renewal_id":null,"renewed_intent":false}
{"version":1,"type":"execute","run_id":"r","patch_id":"p","operation_id":"o2","attempt_id":"a2","assignment_seq":"2","expected_state_version":"4","work":{"type":"reconcile"},"guidance":"reconcile patch branch","recovery_mode":"lost_workspace","renewal_id":"receipt-7","renewed_intent":true}
{"version":1,"type":"cancel","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1"}
{"version":1,"type":"checkpoint_ack","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","proposal_id":"cp-1","state_version":"1","capsule":"frozen capsule bytes","accepted":true}
{"version":1,"type":"result_ack","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","state_version":"3"}
{"version":1,"type":"shutdown","run_id":"r","patch_id":"p"}
```

Session phases are `start` and `respond`; `kind` is optional text represented as `null` or string, and `turn` is required. The pure decoder accepts only the current `Types.Operation_kind` labels (`uncommitted-changes`, `rebase`, `human`, `merge-conflict`, `ci`, `review-comments`, `pr-body`, `findings`). Only assigned patch sessions and patch-local reconciliation are supported. Root integration, scripted gameplan publication, general forge management, and whole-gameplan dispatch are unsupported. Unknown work shapes fail decoding. `recovery_mode` is `retained_workspace` or `lost_workspace`.

## Worker to host

```json
{"version":1,"type":"ready","run_id":"r","patch_id":"p","worker_id":"w","state_version":"0"}
{"version":1,"type":"accepted","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","replay":false}
{"version":1,"type":"observation","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","text":"fetch started"}
{"version":1,"type":"checkpoint","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","proposal_id":"cp-1","previous_state_version":"0","state_version":"1","capsule":"frozen capsule bytes"}
{"version":1,"type":"result","run_id":"r","patch_id":"p","operation_id":"o1","attempt_id":"a1","assignment_seq":"1","state_version":"3","status":"completed_ok","diagnostics":"done","renewal_id":null}
{"version":1,"type":"protocol_error","run_id":"r","patch_id":"p","reason":"assignment not admissible"}
```

The result `status` is one of `completed_ok`, `completed_no_commits`, `failed`, `cancelled`, `retryable`, `attention`, or `merged_observed`. Only `attention` carries a nonnull `renewal_id`. `attention` means unexplained disappearance of possibly published work requires an explicit renewed instruction. `retryable` means evidence or execution is temporarily unavailable; `merged_observed` reports a confirmed merge. `observation` and transcript chunks are bounded, non-authoritative telemetry. A pre-bind `protocol_error` omits `run_id` and `patch_id`.

## Lifecycle and durability

The first `bind` fixes the run, patch, worker and workspace identity, context, initial capsule, version and attention receipt. An exact bind replay is harmless; any changed bind terminates the connection. Assignment sequence starts at `"1"` and increases by exactly one after each acknowledged result. An `execute` is admitted only when its run/patch binding matches, expected version equals the acknowledged version, the prior result is acknowledged, and cleanup completed. Repeating an identical execute replays acceptance (and a retained terminal result) without beginning execution again. Reusing an operation, attempt, or sequence with changed content is rejected, including after cached results expire. The process remains bound to the same run and patch across sequential operations.

A checkpoint freezes its proposal ID, version and capsule bytes. The host conditionally stores those exact bytes under current authority and replies with an ACK matching the full operation identity, proposal ID, version and capsule. Only an accepted exact ACK advances the pure state version. Duplicate exact ACKs are harmless. Rejection cancels and stops the worker; a lost ACK may have committed at the host, so readmission must retrieve the host's actual stored version and observe remote effects. The adapter must not run any dependent effect before the ACK. Its ACK mailbox must not acquire the Runtime mutex while a persistence callback waits for ACK.

The worker emits a terminal result only after acknowledged state and completed descendant cleanup. The next assignment waits for `result_ack`; stale ACKs, cancels and changed replays cannot affect a successor. Active shutdown cancels; idle shutdown exits. Host EOF, broken stdout, checkpoint rejection or ACK timeout cancels execution and terminates the process tree within the adapter's documented bound. It does not authorize spawning a replacement; that is the host wrapper's responsibility. No arbitrary agent shell or API side effect is promised exactly once.

For a possibly published branch that disappeared, plain retry and old guidance cannot recreate it. The host must supply `renewed_intent:true` and the exact outstanding `renewal_id` in a new logical operation and attempt after the prior result ACK. A wrong, consumed, or superseded receipt is rejected. Exact replay of the accepted renewal is idempotent. Wire admission is not Git mutation authority: the owner re-observes branch, base, and forge state; a merge or reappeared branch takes precedence. Publication history remains possibly published.

The local Patch 2 properties exercise the pure codec and transition algebra. Patch 6 adds the CLI invocation, exact-binary subprocess harness, and end-to-end cleanup deadline evidence.

## Required replacement-worker contract (FC-1 amendment)

The single-process implementation does not yet satisfy FC-1 across replacement.
Patch 2 owns the following admission API and wire change; Patch 6 must depend on
that implementation rather than infer authority from capsule bytes or `bind`.
The v1 examples above describe the current implementation, not safe readmission.
The replacement contract must use a new protocol version so an older worker
cannot silently discard durable admission evidence.

A bind in that version must explicitly select `fresh` or `resume`. `fresh` is
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

These dispositions are a tagged union: unfinished cannot contain an ACK,
and acknowledged cannot omit its terminal result or matching ACK. The pure
protocol owner must validate identity, sequence, state-version and renewal
consistency before constructing restored admission state. The capsule codec
must persist these owner facts without reconstructing them from inferred Git
state or treating a successful bind as acknowledgement.

After restoration, an exact Execute replay never emits `Begin`. It replays
acceptance and the retained terminal result, if present. Changed content or
reuse of a reserved operation, attempt or sequence is rejected. An unfinished
predecessor remains ambiguous: neither exact replay nor a new assignment may
start effects. It requires an explicit owner recovery disposition based on the
committed capsule and observed effects; process replacement alone never turns
it into a fresh attempt.

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

Patch 2 acceptance requires public pure-interface tests for restoring after at
least three assignments, admitting sequence 4 only after durable ACK, rejecting
sequence 1 reset and altered replay, replaying a lost-ACK terminal result without
`Begin`, blocking unfinished attempts, rejecting invalid receipts, preserving
renewal consumption and identity reservations, and handling sequence exhaustion.
Patch 6 then supplies real subprocess tests for death before effects, after
checkpoint commit but before ACK delivery, after result delivery but before ACK
commit, and after ACK commit but before delivery. Each must assert sequence
continuity, explicit recovery or terminal replay, and no duplicate execution.
