# Workstream Format Reference

## Output Template

```markdown
# Workstream: [Name]

## Vision
[2-4 sentences describing the end state and why it matters]

## Current State
[Brief description of where the codebase is today relative to this vision. Ground this in the actual code — read the subsystems the workstream touches and state what you verified, not how you assume the architecture is wired. This section is the foundation every milestone rests on; a wrong claim here ("there is no X", "every Y works via Z") propagates into invented milestones. See "Ground the Workstream in the Real Codebase" in SKILL.md.]

## Key Challenges
[Bullet list of the hardest parts or biggest unknowns]

## Established Precedents

Cross-cutting prior art (libraries, algorithms, patterns, papers, RFCs, canonical docs) this workstream adopts. Each entry is consumed by `write-gameplan` when a milestone is unpacked into patches — the relevant precedents are then attached to the specific patches that depend on them. Use the same four-field shape as per-patch precedents.

- **[kind] Name** — [URL if a stable one exists]
  [1-2 sentences: how this precedent applies across milestones and the concrete shape it imposes on implementation.]

- **[kind] Name** — [URL]
  [Why applicable.]

`kind` is one of: `library`, `algorithm`, `pattern`, `paper`, `rfc-spec`, `documentation`, `blog-post`.

## Milestones

### Milestone 1: [gameplan-name-kebab-case]
**Definition of Done**: ...
**Why this is a safe pause point**: ...
**Unlocks**: ...

### Milestone 2: [gameplan-name-kebab-case]
**Definition of Done**: ...
**Why this is a safe pause point**: ...
**Unlocks**: ...
**Operator Actions Before Next Milestone** (only if a human action / observation window / decision is required before the next milestone can begin):
- [Step-by-step instructions: what to flip, what to observe (and for how long, watching which signals), what to decide (with criteria), what abort conditions apply.]
**Established Precedents** (milestone-scoped only):
- **[kind] Name** — [URL] — [Why applicable for this milestone specifically.]
**Open Questions** (if any): ...

## Dependency Graph
- Milestone 1 -> []
- Milestone 2 -> [1]
- Milestone 3 -> [1]
- Milestone 4 -> [2, 3]

## Open Questions
[Questions that apply to the workstream as a whole, not yet resolved]

## Decisions Made
[Key technical or product decisions made during planning, with rationale]

## Definition of Done (Acceptance Suite)

The terminal acceptance suite for the whole workstream. This section always comes last. Every assertion is independently verifiable by a coding agent with the codebase, a running copy of the software, and the database — without reading source to decide whether it passed. Together these assertions subsume every milestone's Definition of Done: every behavioral promise made anywhere in the workstream reappears here as at least one observable check.

Each assertion follows a Given-When-Then shape: `Verify by` is the given/when, `Expected` is the then. Keep them atomic (one aspect each) and independent (no ordering dependency), so a failure localizes to a single `Traces to` target. `Verify by` kind is one of `api`, `db`, `ux`, `cmd` (run a command / CI job / build and observe exit code, output, artifact, or git/filesystem state — the workhorse for infra and tooling workstreams).

- **DoD-1 — [short name]**
  - **Assert**: [an unambiguously true/false statement about observable behavior or state]
  - **Verify by** `api` | `db` | `ux` | `cmd`: [exact procedure — endpoint + method + payload, the SQL/state query, the UI steps, or the command + expected exit code/output]
  - **Expected**: [the observable result that confirms the assertion]
  - **Traces to**: Milestone N — [module / file / migration / endpoint / table that implements it]

- **DoD-2 — [short name]**
  - **Assert**: ...
  - **Verify by** `db`: ...
  - **Expected**: ...
  - **Traces to**: ...
```

---

## Complete Example: Onton Orchestrator

Below is a real-world workstream demonstrating all sections. Onton is an OCaml orchestrator that manages parallel coding agents executing gameplan patches.

# Workstream: Onton Orchestrator

## Vision

Build a standalone OCaml binary that parses gameplans, builds dependency graphs, and spawns concurrent coding agents in isolated git worktrees. It polls GitHub for PR status and reacts to merges, reviews, and CI. The type system and a formal specification (Pantagruel) serve as correctness tools, with property-based tests derived from the spec.

## Current State

The OCaml project has a build skeleton (dune 3.21, OCaml 5.4, Jane Street ppx ecosystem) but no implementation beyond a placeholder module. A formal specification defines the functional correctness requirements.

## Key Challenges

- **Concurrency model**: Mapping structured concurrency primitives (fibers, semaphores, mutex-protected state) to the orchestration requirements.
- **No TUI library**: Raw ANSI + Eio instead of a TUI framework. Must handle terminal modes, key input, alternate screen, and rendering without library support.
- **Property test coverage**: Comprehensive QCheck2 coverage from the start, with properties derived from the formal spec.
- **Spec parity**: The formal spec defines invariants that must be explicit and machine-checkable in the implementation.
- **Decision logic testability**: Pure decision logic must be separated from I/O for property testing.

## Established Precedents

- **library — Eio** — https://github.com/ocaml-multicore/eio
  Structured-concurrency primitives (fibers, switches, cancellation, capability-based I/O) for the orchestrator main loop, poller, runner, and patch_agent. Prefer `Eio.Fiber.fork` / `Switch.run` over manual thread plumbing; pass capabilities (`#Eio.Stdenv.process_mgr`, `#Eio.Net.t`) explicitly rather than reaching for global state.

- **pattern — Property-based testing (QuickCheck-style)** — https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
  Each Pantagruel rule produces at least one QCheck2 property — `forall x: T | P x` becomes a generator + property pair with shrinking. Derive properties from the spec, never from current OCaml behavior, so a divergence between code and spec surfaces as a test failure rather than being locked in.

- **library — QCheck2** — https://github.com/c-cube/qcheck
  OCaml property-based testing library. Use `QCheck2.Test.make ~name ~count gen prop`; reach for `QCheck2.Gen.{bool, int_bound, oneofl}` as composable generators. Shared generators live in `lib_test/test_generators.ml`.

- **pattern — Functional core, imperative shell** — https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell
  Pure decision modules (`Patch_decision`, `Spawn_logic`, `Reconciler`) sit beneath an imperative I/O shell in `main.ml`. Property-test the core; keep the shell minimal, dumb, and trivially auditable. Every milestone that touches decision logic extends the pure core, not the shell.

## Milestones

### Milestone 1: onton-port

**Definition of Done**:
- All pure logic modules exist: types, gameplan parser, dependency graph, priority queue, prompt renderer, patch agent state machine, orchestrator, reconciler, poller
- Core algebraic types match the formal spec domains
- Gameplan parser handles markdown gameplans
- Graph module computes dependency satisfaction, open PR deps, branch names
- `dune build` succeeds with all warnings fatal

**Why this is a safe pause point**: All pure logic is implemented and compiles. No I/O modules yet, but the computational core is complete and can be reviewed against the spec.

**Unlocks**: Property testing of pure modules, I/O wiring, TUI development.

---

### Milestone 2: onton-complete

**Definition of Done**:
- QCheck2 property tests for patch_agent, graph, poller, orchestrator, reconciler
- GitHub HTTP wiring (cohttp-eio), backend subprocess spawning (Eio.Process)
- Full TUI with status table, detail pane, ANSI rendering
- Three-fiber Eio event loop (TUI renderer, poller, runner)
- JSON state persistence
- CI pipeline (GitHub Actions)

**Why this is a safe pause point**: The system is runnable end-to-end. Agents can be spawned, PRs polled, status displayed. Core modules have property tests.

**Unlocks**: Real-world usage, feature parity work, TUI interactivity.

---

### Milestone 3: onton-completeness

**Definition of Done**:
- Property tests for reconciler, persistence round-trips, orchestrator tick/spawn, state machine sequences
- TUI input loop with keyboard handling
- Concurrency cap (--max-concurrent), session resume fallback, comment dedup, startup reconciliation

**Why this is a safe pause point**: Test coverage is comprehensive. Interactive TUI commands work. The tool handles edge cases (restart recovery, comment dedup, CI cap).

**Unlocks**: Decision logic extraction, advanced TUI features.

---

### Milestone 4: onton-completeness-pt-4

**Definition of Done**:
- Backend stream parsing for live activity and PR auto-detection
- PR number persistence (survives restarts)
- Persistence migration for forward-compatible snapshots
- GitHub comment ID tracking for dedup
- Session resume fallback chain with PR context
- TUI polish: timeline view, input history, signal handling

**Why this is a safe pause point**: All target features implemented. Live streaming means the TUI shows real-time agent activity.

**Unlocks**: Decision logic refactoring, spec violation fixes.

---

### Milestone 5: onton-completeness-pt-5

**Definition of Done**:
- Pure Patch_decision and Spawn_logic modules extracted from main.ml
- QCheck2 property tests for all decision functions, derived from the formal spec
- Spec violations fixed: approved? tracking, orchestrator-executed rebase, conflict clearing, CI failure cap
- TUI: activity log in detail view, help overlay, scrollable detail, visible input prompt

**Why this is a safe pause point**: Decision logic is fully testable. All known spec violations are resolved. The TUI is polished.

**Unlocks**: The orchestrator is production-ready and spec-compliant.

## Dependency Graph

```text
1 (onton-port) → []
2 (onton-complete) → [1]
3 (onton-completeness) → [2]
4 (onton-completeness-pt-4) → [3]
5 (onton-completeness-pt-5) → [4]
```

**Note**: This workstream is strictly sequential — each milestone builds on the previous. In a larger project, milestones could be parallelized (e.g., test infrastructure and feature work in parallel tracks).

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| TUI library vs raw ANSI | Decided: raw ANSI. No OCaml TUI library met requirements. | Milestone 1 (resolved) |
| bisect_ppx compatibility | Incompatible with OCaml 5.4. Revisit when updated. | Deferred |
| JSON gameplan format | v1 is markdown. v2 JSON format designed later. | Post-workstream |

## Decisions Made

These are decisions that did NOT produce a citable precedent — rejected library choices, framing principles, and project philosophy. Accepted precedents (Eio, QCheck2, functional-core-imperative-shell, property-based testing) live in the `Established Precedents` section above.

| Decision | Rationale |
|----------|-----------|
| Lwt rejected in favor of Eio | Lwt is callback-based and harder to reason about than Eio's structured fibers. Recorded here as a rejected alternative — the chosen precedent is documented in `Established Precedents`. |
| Raw ANSI TUI (no library) | No OCaml TUI library supports the rendering model we need (incremental updates, alternate screen, raw key input). A conscious decision to roll our own — no precedent applies. Revisit if a suitable library emerges. |
| Spec is source of truth | When the code disagrees with the formal spec, the code is wrong. Tests are derived from the spec, not from current behavior. Project philosophy rather than an external precedent. |

## Definition of Done (Acceptance Suite)

Onton has no relational DB and no HTTP API of its own, so the `db` method maps to its **persisted JSON state store** and `api` maps to the **GitHub API** it polls. The suite is still black-box: each assertion is confirmed by observing running behavior, the state file, GitHub, or the TUI — not by reading orchestrator source.

- **DoD-1 — Concurrency cap is never exceeded**
  - **Assert**: With `--max-concurrency N`, the number of patch agents in `Running` at any instant is ≤ N.
  - **Verify by** `ux`: Launch against a gameplan with ≥ 5 dependency-free patches and `--max-concurrency 2`; watch the status table and count agents showing `Running` at each refresh. (Don't use `git worktree list` as a proxy — worktrees persist after an agent finishes and are reused across runs, so the listing reflects lifetime worktrees, not concurrent execution.)
  - **Expected**: Never more than 2 agents in `Running` simultaneously.
  - **Traces to**: Milestone 3 — concurrency gate in `Spawn_logic` / orchestrator tick.

- **DoD-2 — A patch never starts before its dependencies merge**
  - **Assert**: No agent is in `Running`/`Done` while any of its graph-dependency PRs is unmerged.
  - **Verify by** `db`: Query the persisted state JSON for any agent whose status ≠ `Blocked` but whose dependency PRs are not all `merged`.
  - **Expected**: Empty result set.
  - **Traces to**: Milestone 2 — the orchestrator's eligibility gate that consults `Graph` dependency-satisfaction before marking an agent runnable.

- **DoD-3 — State survives a restart**
  - **Assert**: Killing and relaunching onton restores in-flight PR numbers and per-agent statuses.
  - **Verify by** `ux` + `db`: Start a run, note PR numbers and statuses in the table, send SIGINT, relaunch; diff the state JSON before and after.
  - **Expected**: Identical agent set, PR numbers, and statuses after relaunch.
  - **Traces to**: Milestone 4 — PR-number persistence + persistence migration.

- **DoD-4 — Review-feedback comments are de-duplicated**
  - **Assert**: The orchestrator posts each unique review-feedback comment to a PR at most once across poll cycles.
  - **Verify by** `api`: After two poll cycles with no new review activity, fetch the PR's comments via the GitHub API.
  - **Expected**: No duplicate comment bodies; the tracked comment IDs in the state JSON match the comments present on the PR.
  - **Traces to**: Milestone 4 — GitHub comment-ID tracking.

- **DoD-5 — Rebases are orchestrator-executed, not agent-executed**
  - **Assert**: When a dependency PR merges, the orchestrator rebases the dependent branches onto the new main.
  - **Verify by** `ux` + `git`: Merge a dependency PR; inspect a dependent branch's `git log` / merge-base.
  - **Expected**: The dependent branch's base equals the merged main SHA, and the rebase appears in orchestrator activity (not agent) logs.
  - **Traces to**: Milestone 5 — orchestrator-executed rebase spec fix.
