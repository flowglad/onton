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
