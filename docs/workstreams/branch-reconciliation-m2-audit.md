# Branch reconciliation M2 release audit

M2 is complete as a qualified release candidate. The [release evidence](branch-reconciliation-m2-release-evidence.md)
records the final executable acceptance map, command outcomes and verification
limits. Review, release publication and representative operator trials remain the
explicit handoff in the [workstream](branch-reconciliation.md).

| M2 requirement | Closure |
|---|---|
| 1. Single mutation authority | Session publication, scheduled rebase, conflict repair and root integration use the owned checkpoint runner. Captured integration policy governs recovery and publication, including adopted merges. Handoff revalidates after capacity acquisition; legacy mutation APIs have no enabled runtime callers. |
| 2. Preservation and restart | Private refs retain source, target, candidate, leases and newly observed recovery revisions. Dirty/staged/untracked work survives; late commits remain preservation obligations. Missing checkout waits without recreation. Checkpoint, interruption, unexpected-agent-mutation and retained-resolution fixtures pass. |
| 3. Useful recovery | Failed pushes retain candidates independently of implementation outcomes. Explicit leases and direct confirmation handle races/lost acknowledgements. Bounded agent history recovery is the final fallback, with independent preservation verification before Onton publishes. Transport failures do not charge repair; explicit write denial intervenes. |
| 4. Transitional scheduling/forge | Terminal dispositions suppress queued and owned dispatch while retaining checkpoints. Duplicate satisfied conflicts wait, and direct remote evidence confirms real head reversions. Deliberate human resume inspects first; cancelled capacity waits do not launch a backend. |
| 5. Upgrade/operations | v1 migration retains sessions, worktrees, anchors and unrelated interventions while clearing obsolete branch counters. Pre-upgrade bytes and v2 durability are tested. Installed v0.66.0 rejects v2 without changing it. Pending outbox, derived detail views, manual resume and downgrade runbook are covered. |
| 6. Release evidence | Full source gate passes, including 57 pure reconciliation properties and real Git fixtures for publication, repair, root races and provider handling. Installed Simgit lifecycle passes separately. A01–A17 and destination authority are mapped to executable evidence. |

The final publication review also closed two authority gaps: legitimate recorded
replays can publish conflict-resolved content without claiming heuristic ownership,
and direct confirmation queries the actual push endpoint. Operations checkpoint a
credential-free destination fingerprint; changed or multiple endpoints stop before
mutation. Explicit human resume can adopt a reviewed endpoint after inspection.

The [operator runbook](../design/branch-reconciliation-operations.md) explains
retained refs, specific intervention reasons, deliberate resumption and v2 downgrade.
No known enabled-path safety defect is deferred. M3 retains broader deterministic
recovery coverage, the independent abstract model, complete formal/exhaustive
specifications, full forge-context algebra, inert compatibility cleanup and
dependency-aware ref reclamation. Live-provider trials and release publication
have not been performed or claimed.
