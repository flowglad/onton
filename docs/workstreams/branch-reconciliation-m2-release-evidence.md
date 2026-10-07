# Branch reconciliation M2 release evidence

M2 qualifies the branch-reconciliation release candidate. The containing PR's head
identifies the candidate; the implementation base is
`cb47dfdab0dc0047826d5c9893346c02957bc854` (PR #483). This record covers the
implemented M2 contract, not completion of M3 or a published production release.

## Requirement-to-evidence map

Paths below are relative to the repository root. Tests assert public transitions,
Git refs/trees/index state, persisted snapshots, or runtime outcomes.

| Acceptance | Executable evidence | Observed contract |
|---|---|---|
| A01: identity/idempotence | `test/test_branch_reconcile_properties.ml` | Generated duplicate/stale results, retry and repair events, restart, fixed points and captured authority; 54 properties. |
| A02: durable dispatch | `test/test_branch_reconcile_git.ml`, `test/test_branch_reconcile_ownership.ml` | Failed checkpoint prevents mutation; interrupted commands recover by inspection; capacity/ownership handoff revalidates the current turn. |
| A03: materialization | `test/test_worktree_setup_base_fetch_integration.ml`, `test/test_branch_reconcile_git.ml` | Starting commit is retained before implementation; later fetch/base movement cannot replace the proven replay boundary. |
| A04: publication independent of implementation | `test/test_session_result_properties.ml`, `test/test_branch_reconcile_git.ml`, `test/test_gameplan_publication.ml` | Session receipt survives publication failure; lost push acknowledgement is confirmed directly without another push or implementation session. |
| A05: staged repair | `test/test_branch_reconcile_git.ml` | Staged resolutions survive restart; Onton continues the original integration without requiring an agent commit, then observes newer base movement. |
| A06: remote races | `test/test_branch_reconcile_git.ml`, `test/test_feature_branch_git.ml` | Rewrite replay and preserving merge retain both sides. Recorded replay permits legitimate conflict-resolution changes; inferred boundaries do not grant the same authority. PR #482/#483 regression scenarios retain work and confirm the actual remote. |
| A07: root ownership | `test/test_feature_branch_git.ml`, `test/test_feature_branch_mode.ml`, `test/test_branch_reconcile_git.ml` | Concurrent contributions preserve ancestry; descendant receipt waits for confirmed publication containing its captured source. |
| A08: retry isolation | `test/test_branch_reconcile_properties.ml`, `test/test_branch_reconcile_git.ml`, `test/test_push_reject_classify_properties.ml` | Probe/transport failures back off without charging repair; explicit permission denial intervenes. Missing checkout waits without recreation and resumes the same candidate when restored. |
| A09: empty merge | `test/test_branch_reconcile_git.ml` | Recreated merge with first-parent tree retains required parents through interruption around merge completion. |
| A10: unexpected work | `test/test_branch_reconcile_git.ml`, `test/test_worktree_setup_base_fetch_integration.ml`, `test/test_branch_reconcile_ownership.ml` | Dirty/staged/untracked work survives; newly observed committed work is pinned and becomes a durable preservation obligation, including work arriving between repair turns. Unexpected agent commits cannot silently authorize continuation/publication. |
| A11: final agent fallback | `test/test_branch_reconcile_properties.ml`, `test/test_branch_reconcile_git.ml` | Exhausted deterministic recovery dispatches a distinct history-recovery turn. Verified recovery publishes; invalid outcomes retain refs and stop after two completed no-progress attempts. Lost backend acknowledgement resumes verification. |
| A12: upgrade | `test/test_persistence_properties.ml`, `test/test_branch_reconcile_outbox.ml`, `test/test_branch_reconcile_git.ml` | v1 migration retains unrelated state and original backup, clears obsolete branch counters, restores pending work and reobserves real Git. Active legacy sequencer stops specifically and deliberate resume inspects first. Installed old-reader rejection was checked separately below. |
| A13: waiting/outbox | `test/test_branch_reconcile_outbox.ml`, `test/test_branch_reconcile_ownership.ml` | Waiting reconciliation remains Pending and releases capacity/ownership; cloud-visible work is not mistaken for quiescence. |
| A14: terminal/cancellation | `test/test_branch_reconcile_ownership.ml`, `test/test_branch_reconcile_outbox.ml`, `test/test_wontdo_session.ml`, `test/test_session_timeout.ml` | Terminal disposition is checked at scheduling and dispatch/handoff boundaries; no new backend after a cancelled wait. Work/checkpoints remain retained; explicit reset can resume through inspection. |
| A15: stale conflict reports | `test/test_patch_controller.ml`, `test/test_branch_reconcile_properties.ml`, `test/test_branch_reconcile_git.ml` | Repeated conflicts for an already satisfied pair wait without another rewrite or repair charge. Positive direct remote evidence permits real head reversions; failed probes do not establish absence. |
| A16: supported modes/providers | Full `dune runtest`; `test/test_branch_reconcile_git.ml`, `test/test_feature_branch_git.ml`, `test/test_feature_branch_cli.ml`, `test/test_worktree_backend_integration.ml` | Stacks/base retargeting/squash fixtures, feature-root races, SourceHut's actual Git probe plus injected build observation, native-stack restrictions and unrelated review/CI/backend behavior pass. Installed Simgit lifecycle also passed separately. |
| A17: diagnostics | `test/test_tui_render_frame.ml`, `test/test_branch_reconcile_properties.ml`, persistence/outbox fixtures | Detail views distinguish phase, identity, captured SHAs, lease, evidence, retry and intervention. Snapshot/event state retains the operation. |
| Destination authority | `test/test_branch_reconcile_properties.ml`, `test/test_branch_reconcile_git.ml` | Confirmation uses the actual push endpoint, including split fetch/push URLs. Multiple destinations stop. A checkpointed destination fingerprint prevents redirection after restart; explicit resume reobserves the reviewed destination before publishing the retained candidate. |

These scenarios close M2 requirements 1–6: one enabled mutation owner (A02/A07/
A14), preservation (A03/A05/A06/A09/A10), useful recovery (A04/A08/A11), safe
transitional scheduling (A13–A15), upgrade/operations (A12/A17), and executable
release evidence (this record/A16). Runtime call-path review found scheduled
rebase, conflict repair, session publication and root integration using the
checkpoint runner. The remaining worktree-plan runtime caller supplies checkout
setup (`for_start`); legacy mutation APIs are not alternate enabled owners.

## Verification

Local environment: OCaml 5.5.1, dune 3.24.2, Git 2.56.0. Final gate results are
recorded after the destination-binding change and formatting:

| Command | Exit |
|---|---|
| `opam exec -- dune build` | 0 |
| `opam exec -- dune runtest` | 0 |
| `opam exec -- dune build @fmt` | 0 |
| `bash scripts/check-no-raw-yojson.sh` | 0 |
| `git diff --check` | 0 |
| `opam exec -- dune build @check` | 0 |
| Pinned archlint OCaml adapter (commit `855690b1d0f821a177af0ea68e8eb6ae6a263ecc`) | 0 |

Architecture invocation: `opam exec -- uv run --project <archlint> python
<archlint>/evaluate.py --repo-root <onton> --adapter ocaml --ocaml-root .`.
The local final log is `/tmp/onton-m2-final-gate.log`; this committed map preserves
the results without requiring that temporary file.

Additional completed checks:

- Installed Simgit 0.4.0: `env ONTON_TEST_SIMGIT=/opt/homebrew/bin/simgit opam exec
  -- dune exec test/test_worktree_backend_integration.exe`, exit 0, lifecycle OK.
  This is provisioning/lifecycle evidence, not a claim that every Git fixture ran
  against Simgit.
- Installed Onton 0.66.0, binary SHA-256
  `c7664fa108348c526710ff4af84c8b86f4851e53637419b14403841aacbab57d`:
  `onton --prune --no-refresh` against an isolated `ONTON_DATA_DIR` containing a
  v2 snapshot exited 1 with `snapshot load failed: unsupported version: 2`.
  Snapshot bytes remained unchanged; no project execution or forge request.

## Release handoff and remaining scope

Review/merge and release publication have not occurred. Operators select backed-up
representative stacked-PR, feature-root and SourceHut projects and exercise a full
implementation/publication cycle plus controlled restart before broader rollout.
The SourceHut acceptance fixture uses real local Git and the provider handler with
an injected builds response; it is not a live SourceHut service trial. Follow the
[operator runbook](../design/branch-reconciliation-operations.md), including its
v2 downgrade procedure and retention of snapshots, worktrees and private refs.

M3 retains the independent abstract model, complete formal specifications and
exhaustive interruption/topology matrix, full PR/request-scoped forge observation
algebra, broader deterministic heuristics, removal of inert compatibility APIs and
dependency-aware recovery-ref reclamation. Existing generated tests are not an
independent abstract model. Unproven preservation reaches a stable intervention;
M2 does not claim automatic recovery for every possible Git history.
