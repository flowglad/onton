# Workstream Format Reference

## Output Template

```markdown
# Workstream: [Name]

## Vision
[2-4 sentences describing the end state and why it matters]

## Current State
[Brief description of where the codebase is today relative to this vision]

## Key Challenges
[Bullet list of the hardest parts or biggest unknowns]

## Milestones

### Milestone 1: [gameplan-name-kebab-case]
**Definition of Done**: ...
**Why this is a safe pause point**: ...
**Unlocks**: ...

### Milestone 2: [gameplan-name-kebab-case]
**Definition of Done**: ...
**Why this is a safe pause point**: ...
**Unlocks**: ...
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
```

---

## Complete Example: Onton Orchestrator

Below is a real-world workstream demonstrating all sections. Onton is an OCaml orchestrator that manages parallel Claude Code agents executing gameplan patches.

# Workstream: Onton Orchestrator

## Vision

Build a standalone OCaml binary that parses gameplans, builds dependency graphs, and spawns concurrent Claude Code agents in isolated git worktrees. It polls GitHub for PR status and reacts to merges, reviews, and CI. The type system and a formal specification (Pantagruel) serve as correctness tools, with property-based tests derived from the spec.

## Current State

The OCaml project has a build skeleton (dune 3.21, OCaml 5.4, Jane Street ppx ecosystem) but no implementation beyond a placeholder module. A formal specification defines the functional correctness requirements.

## Key Challenges

- **Concurrency model**: Mapping structured concurrency primitives (fibers, semaphores, mutex-protected state) to the orchestration requirements.
- **No TUI library**: Raw ANSI + Eio instead of a TUI framework. Must handle terminal modes, key input, alternate screen, and rendering without library support.
- **Property test coverage**: Comprehensive QCheck2 coverage from the start, with properties derived from the formal spec.
- **Spec parity**: The formal spec defines invariants that must be explicit and machine-checkable in the implementation.
- **Decision logic testability**: Pure decision logic must be separated from I/O for property testing.

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
- GitHub HTTP wiring (cohttp-eio), Claude subprocess spawning (Eio.Process)
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
- Claude stream-JSON parsing for live activity and PR auto-detection
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

```
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

| Decision | Rationale |
|----------|-----------|
| Eio over Lwt | Eio provides structured concurrency (fibers, scopes). Lwt is callback-based and harder to reason about. |
| Raw ANSI TUI | No OCaml TUI library supports the rendering model we need (incremental updates, alternate screen, raw key input). Rolling our own with Eio is simpler than fighting a library. |
| QCheck2 from the start | Property tests derived from the formal spec catch more bugs than example-based tests and serve as machine-readable spec compliance evidence. |
| Separate pure and I/O | Pure decision modules (Patch_decision, Spawn_logic, Reconciler) are extracted and tested independently. I/O code in main.ml is a thin wiring layer. |
| Spec is source of truth | When the code disagrees with the formal spec, the code is wrong. Tests are derived from the spec, not from current behavior. |
