# Resource v1 consumer contract (pinned M2 public surface)

This document records the public interface Onton may consume from provisioning-agent at **`d7c82ad33ebd603800e19ee2cefec3c0bccce936`**. The inspected sources were `tools/issue-runner/resource-contract.md`, `internal/coordination/resource_types.go`, `internal/coordination/resources.go`, `resource_cli.go`, `resource_runtime.go`, `resource_store.go`, `resource_ecs.go`, and `resource_host.go`. This is a consumer contract, not an Onton implementation of provider internals. No sibling-repository changes or cloud qualification are implied.

## Entrypoints and trust boundary

- The opt-in command is `issue-runner resources <operation>`. Ordinary issue-worker and webhook paths are separate.
- A request is one UTF-8 JSON object read from stdin through EOF. Close stdin after writing it. The helper writes one JSON response to stdout; diagnostics are separate. Runtime failures can return a nonzero exit status even when a `pending` JSON response was produced.
- `resources request` accepts any supported `op`; alternatively, the operation name can be the command. `resources connect <run-id> <coordinator-id> <owner-epoch> <revision> <request-id> <resource-id>` is a different, raw bidirectional stdin/stdout stream, not JSON framing. Its stdout carries only child bytes; stderr is separate.
- The helper derives the caller and ECS task identity from trusted task credentials/metadata and checks its deployed `RESOURCE_DESCRIPTOR` against task configuration. Request JSON cannot assert AWS authority, task identity, provider observations, TLS pin, or bearer. Secrets and bootstrap capabilities are not supplied in request fields or connect arguments.
- The coordinator keeps its own resource/request identity inventory. There is no public resource-inventory or historical-state read API. `inspect` returns the generic current-run response, not a list of resources or receipts. A known write receipt is queried by its `write_id`; current state bytes are obtained only through `state-read`.

## Request JSON

The exact common fields are:

| Field | Type | Presence / meaning |
| --- | --- | --- |
| `version` | string | Required; exactly `"resource-v1"`. |
| `op` | string | Required; one of the operations below. |
| `run_id` | string | Required run identity. |
| `request_id` | string | Required idempotency identity for this request. |
| `coordinator_id` | string | Required; must match the trusted current coordinator task ARN. |
| `owner_epoch` | string counter | Required for every operation except `open`. |
| `revision` | string counter | Required for every operation except `open`. |

All identifiers are opaque UTF-8 strings of 1–256 bytes, and may not contain NUL, CR, or LF. They are not paths. Counters are canonical, nonnegative signed-int64 decimal **strings** (`"0"`, `"1"`, …; no leading zeroes). `open` has no owner/revision guards and requires `config_digest`, which is an identifier; that digest and coordinator identity fix the run configuration.

Operation-specific fields (no other fields are allowed):

| `op` | Additional fields |
| --- | --- |
| `open` | `config_digest` |
| `inspect`, `takeover`, `close`, `state-read` | none |
| `launch` | `chain_id`, `patch_id`, `resource_id`, `launch_digest`, `manifest` |
| `observe`, `stop` | `resource_id` |
| `replace` | `chain_id`, `patch_id`, `resource_id`, `predecessor_id`, `launch_digest`, `manifest` |
| `state-write` | `write_id`, `state_version`, `payload` |
| `write-status` | `write_id` |
| `log` | `message`, and optional `patch_id`, `resource_id` |

`state_version` is also a canonical decimal-string counter. `payload`, when present, is canonical standard base64 for at most 8 MiB of opaque bytes; omission is accepted as the empty payload for `state-write`. `manifest` has `executable`, an optional `args` list (at most 128 arguments, each at most 4096 bytes), optional `preparation`, and optional `inputs` (at most 256 `{path,data}` entries). Input paths must be clean, relative, unique, and nonescaping; aggregate decoded input bytes are at most 8 MiB. Each `data` value is canonical base64. A `log.message` is 1–4096 bytes.

The complete request envelope is limited to 16 MiB. Unknown fields, unknown operations/versions, duplicate JSON keys at any depth, malformed UTF-8, trailing JSON, explicit top-level `null` fields, noncanonical counters/base64, invalid paths, and excess sizes are rejected; input is never truncated. Exact retries preserve request ID and all original fields/guards. Reusing an ID with changed content rejects. Current authority is checked before replay; an old owner cannot use an old epoch to retrieve receipts.

## Response JSON

Every response has these required fields:

```json
{"status":"ok|rejected|pending|attention","reason":"stable-reason"}
```

The remaining fields are optional (omitted when their Go zero value is empty/zero):

| Field | JSON type | Meaning |
| --- | --- | --- |
| `run_id` | string | Run identity, when present. |
| `revision` | JSON number | Current metadata revision. |
| `owner_epoch` | JSON number | Current ownership epoch. |
| `resource_id` | string | Named physical resource, when present. |
| `state_version` | JSON number | Current opaque state version. |
| `receipt_state_version` | JSON number | Version committed by the known write/request receipt. |
| `payload` | string | Canonical base64 **only** for successful `state-read`; never historical bytes. |

Response counters are JSON integer numbers backed by Go `int64`, not decimal strings. Consumers must parse them without floating-point conversion. On successful `state-read`, `payload` is present even when the exact committed payload is empty. Failed/absent state is not equivalent to empty bytes.

Statuses are `ok`, `rejected`, `pending`, and `attention`. Stable reasons are `none`, `invalid_request`, `not_found`, `not_owner`, `stale_guard`, `changed_request`, `closed`, `closing`, `chain_busy`, `predecessor_live`, `launch_uncertain`, `observation_exhausted`, `state_version`, `storage_uncertain`, `corrupt_object`, `cleanup_pending`, and `invariant_violation`. `pending` means evidence may settle; `attention` requires operator inspection or later explicit observation. Provider errors are not implicit authority to release a claim.

`state-read` returns only the current committed bytes and current state version. `write-status` returns a known write receipt (including `receipt_state_version` and current `state_version`) but never its payload. An exact state-write retry can report the original receipt version while the current state version has advanced. Uploaded bytes alone are not a committed write; the published pointer/version and write receipt are committed together after immutable-object verification. Cleanup can make state-read reject while receipt metadata remains retained.

## Effect semantics that callers must preserve

- `open` creates or returns the matching open run. `takeover` changes owner only after positive STOPPED evidence for the recorded predecessor coordinator task. Missing tasks, failed observation, STOPPING, desired STOPPED, or lease expiry are not retirement proof.
- A `launch`/`replace` physical identity has one durable send authorization and at most one `RunTask` submission; SDK retries are disabled. An ambiguous send is observation-only. Missing task discovery does not mean no effect. `replace` is guarded by positive predecessor retirement, or by the helper's proven abort-before-send path; a new ID cannot bypass uncertainty.
- `observe` returning `status:"ok"` is **not** retirement evidence: it may mean the exact task is currently RUNNING. Matching last-status `STOPPED` from a successful observation is retirement evidence. A failed or missing observation is not.
- A successful `stop` operation reports `ok` only when the matching resource is retired (positive STOPPED) or the helper atomically proves abort-before-send. A bare StopTask request is not proof. Generic observe success, stream EOF, and helper process exit are not proof either.
- Routine observation is bounded to 12 observations over 10 minutes before `attention`; a later explicit observation can still establish positive STOPPED evidence. Backoff and provider observations are helper semantics; Onton does not infer retirement from elapsed time.
- `close` fences new work, waits for all possible resources to be retired, seals owned opaque objects, and retains a tombstone. `close` is terminal cleanup, not coordinator sleep. Sleep must not call it.
- `state-write` is conditional on owner/revision/state-version. Upload success or an uncertain response is not commit; resolve uncertainty with current-state readback and known write-receipt lookup before acknowledging dependent Onton work.

## Stream semantics

A resource has one authenticated attachment, not a reconnectable stream. The helper rechecks current coordinator/run/resource authority and the observed matching RUNNING task before connecting; TLS server pin and bearer material are retained privately by the helper and are not part of request/argv JSON. Stdout and stdin carry arbitrary ordered bytes, with bounded backpressure. A disconnect initiates bounded process cleanup; the helper's exit does not establish ECS STOPPED. Do not reconnect, replay consumed stream bytes, or treat EOF as task retirement.

## Qualification boundary

This pinned contract allows local fixtures to simulate only the public request/response and single-attachment stream behavior. Tests must not depend on helper-private inventory, historical state bytes, task enumeration, or reconnect. `observe`-ok/RUNNING, matching `stop`-ok/retirement (including proven abort-before-send), and guarded replacement must be separately exercised. Native/local fixtures are not cloud deployment qualification.
