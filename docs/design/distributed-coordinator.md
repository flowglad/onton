# Opt-in coordinator: control and forge seams (Patch 7)

The local runner and default v2 worker do not use these modules. The Patch 8
entrypoint must serialize `Coordinator_control.handle` with `Coordinator.run`
worker-stream events and use the same `Coordinator_host` epoch. The socket
requires same-UID peer credentials and an owner-only directory; transport
success is not an acceptance. `accepted` means the exact host envelope was
confirmed through the M2 receipt/current-bytes barrier. Failed or uncertain
writes return `not_accepted`; the original write identity must be resolved
before another transition. Control frames are version 3, exact-schema,
run/config-scoped and bounded. Known IDs retain their payload for the run;
changed reuse is rejected. Sleep and wake generation IDs persist in the same
envelope, not in a local timer. Guidance is also recorded as a new unconsumed
input in the *same* transition; only the checkpointed worker input consumption
can mark it delivered. Bump never renews publication. A renewal must identify
a durable attention operation on that patch. No IDs are evicted.

`Coordinator_forge.observe` requires current provider PR/head/base identity plus
direct read-only `ls-remote` confirmation of the exact provider revisions.
Head goes to the bound push destination, base to origin's fetch destination;
missing ref differs from failed probe. `ready` also requires caller-supplied
scheduler policy and unchanged ticket/scope; neither stdout nor a previous
revision's checks substitute. `contribution_credit` requires the current
captured pair and owner receipt, not a candidate or session log. Note rendering
consumes validated worker checkpoint projections matching the current patch and
publication binding and truncates through `Pr_body_limit`, never a worker path.

`create_or_discover` commits an exact run-scoped create identity and parameters
before provider IO. A later call, including after restart, only discovers;
an empty eventual-consistency read is *not* evidence to submit again. Changed
reuse and a new effect identity without a matching explicit renewal are
blocked. Ambiguous create remains pending. The first successful provider reply
can identify the created PR; readiness still requires current direct proof.
This does not claim exactly-once backend shell/API effects. The configured M2
8 MiB envelope and emergency headroom apply to control/forge intent; attempts
that cannot fit are rejected without evicting history.

`Coordinator_headless` emits a bounded JSONL projection (run/config, active,
idle, attention, lifecycle and sleep generation) without capsule, request,
notes, secret or authority bytes. It never changes scheduling state. On explicit
drain/cancellation, the coordinator attempts matching stop for non-executing
resources; an active assignment and a generic observe-ok cannot prove
retirement. A sleeping run is unsealed and awaits authorized wake. Terminal
cancel cleanup still needs the external close/cleanup authority; absence of a
close response must not emit `Cancel_cleanup_confirmed`.

## Not yet qualified

This patch's local seams do **not** complete FC-11–FC-14: the Patch 8 entrypoint
has not wired the control socket/committed telemetry into the serialized loop;
forge polling tickets, metadata/review/queue/merge reconciliation and versioned
worker evidence delivery are not implemented here. Cancellation has no terminal
close driver. The implemented test verifies immutable control receipts, uncertain
writes, sleep/wake/cancel dominance, changed create identity and ambiguous
creation without blind resubmission, scoped credit and direct probe semantics.
It is not a cloud deployment test or a passing claim for those missing clauses.
