# Gameplan: onton-completeness

## Problem Statement

Onton has a solid pure-logic core and spec-aligned state machine, but the audit against the Elixir reference implementation reveals three categories of gaps: (1) missing property-based tests for reconciler, persistence, spawning logic, and state-machine command sequences; (2) missing features for runtime interaction (TUI input, human messaging, ad-hoc PRs, concurrency cap, session resume fallback); (3) a display-only TUI with no interactivity. These gaps prevent onton from being used as a drop-in replacement for the Elixir orchestrator.

## Solution Summary

Close all 12 gaps identified in the audit, organized into three tracks: **testability** (property tests, invariant oracles, persistence round-trips), **feature parity** (TUI input, human messaging, concurrency cap, session resume fallback, addressed comment dedup, startup reconciliation), and **TUI polish** (keyboard input loop, scrollable patch list, detail view, headless mode). Patches are ordered to maximize early testability wins (pure logic, no I/O changes) before adding interactive features.

## Mergability Strategy

### Feature Flagging Strategy

**No feature flags needed.** This is an internal tool, not a library. All changes are additive (new modules, new tests, new CLI options) or extend existing interfaces. No behavior changes to existing merged-and-working logic.

### Patch Ordering Strategy

**Early** (`[INFRA]`): Property test generators, QCheck properties for existing pure modules, invariant oracle wiring, persistence round-trip tests.
**Middle** (`[BEHAVIOR]`): TUI input handling, concurrency cap, session resume fallback, addressed comment dedup, startup reconciliation.
**Late** (`[BEHAVIOR]`): Human messaging end-to-end, ad-hoc PR addition, scrollable/selectable TUI, headless mode.

## Current State Analysis

### What exists and works
- Pure logic core: `Patch_agent`, `Graph`, `Priority`, `Reconciler`, `Poller`, `Orchestrator` — all spec-aligned
- QCheck2 properties for `Patch_agent` (~30 tests), `Graph` and `Poller` (in `test_properties.ml`)
- Shared generators in `lib_test/test_generators.ml`
- TUI rendering with 16 display statuses, activity feed, ANSI color
- Three Eio fibers: TUI renderer, poller, runner
- `Persistence` module with `save`/`load` and JSON round-trip
- `Invariants` module with runtime checking (env-var gated)

### What's missing
- **Tests**: No properties for `Reconciler`, `Persistence`, `Orchestrator.tick`/spawn logic, or arbitrary command sequences (Elixir P1–P7)
- **TUI input**: Display-only — no keyboard handling, no commands, no scrolling
- **Human messaging**: No way to send messages to running/idle agents
- **Concurrency cap**: Runner spawns all unblocked patches simultaneously
- **Session resume fallback**: No retry on `--resume` failure
- **Addressed comment dedup**: `pending_comments` grows unboundedly, agents re-address same comments
- **Startup reconciliation**: No recovery of worktrees/PRs on restart
- **Ad-hoc PR management**: Can't add/remove PRs at runtime
- **Headless mode**: TUI is mandatory

## Required Changes

### Patch 1: Reconciler properties
- `test/test_reconciler_properties.ml` — new file
- Generators: `reconciler_patch_view` (in `lib_test/test_generators.ml`)

### Patch 2: Persistence round-trip properties
- `test/test_persistence_properties.ml` — new file
- Generators: `runtime_snapshot` (in `lib_test/test_generators.ml`)

### Patch 3: Orchestrator tick/spawn properties
- `test/test_orchestrator_properties.ml` — new file

### Patch 4: State machine sequence properties (P1–P7)
- `test/test_state_machine_properties.ml` — new file
- Uses `Invariants.check_invariants` as postcondition oracle after every transition

### Patch 5: TUI input loop
- `lib/tui_input.ml` / `lib/tui_input.mli` — new module
- `bin/main.ml` — add stdin reading fiber, dispatch to `Tui_input`

### Patch 6: Concurrency cap
- `bin/main.ml` — add `--max-concurrent` CLI arg, limit runner_fiber spawning

### Patch 7: Session resume fallback
- `lib/claude_process.ml` / `.mli` — track resume_fallback flag
- `bin/main.ml` — two-tier retry in `run_claude_and_handle`

### Patch 8: Addressed comment dedup
- `lib/patch_agent.ml` / `.mli` — add `addressed_comment_ids` set
- `lib/poller.ml` — filter by addressed set before enqueueing

### Patch 9: Startup reconciliation
- `lib/startup_reconciler.ml` / `.mli` — new module
- `bin/main.ml` — call before entering fiber loop

### Patch 10: Human messaging and ad-hoc PRs
- `lib/tui_input.ml` — `N> message` parsing, `+123`/`+#123`/`+URL` parsing, `-N` parsing
- `bin/main.ml` — wire input commands to `Runtime`/`Orchestrator`

### Patch 11: Scrollable/selectable TUI with detail view
- `lib/tui.ml` / `.mli` — add `tui_state` with scroll offset, selected patch, view mode
- `lib/tui_input.ml` — arrow keys, mouse scroll, patch selection

### Patch 12: Headless mode
- `bin/main.ml` — `--no-tui` flag, log-to-stdout fiber instead of TUI fiber

## Acceptance Criteria

- [ ] QCheck2 properties exist for Reconciler (merge detection, rebase triggering, liveness)
- [ ] QCheck2 properties exist for Persistence (save/load round-trip identity)
- [ ] QCheck2 properties exist for Orchestrator.tick (start biconditional, pending actions)
- [ ] State machine sequence tests cover: no silent drops, session continuity, no infinite retry, queue drain completeness, terminal state absorbing
- [ ] Invariants module is used as QCheck postcondition oracle
- [ ] TUI accepts keyboard input (quit, patch selection, messaging)
- [ ] Concurrency cap limits simultaneous Claude processes (default 4)
- [ ] Session resume failure triggers fresh session with fallback guard
- [ ] Comments are deduplicated by ID before delivery to agents
- [ ] On restart, existing worktrees and PRs are detected and state is recovered
- [ ] Human messages can be sent to patches via `N> message` syntax
- [ ] Ad-hoc PRs can be added via `+123` and removed via `-N`
- [ ] TUI supports scrolling and patch selection with detail view
- [ ] `--no-tui` runs headless with structured log output
- [ ] All existing tests continue to pass
- [ ] `dune build` produces zero warnings

## Open Questions

1. **Ad-hoc PR numbering**: Elixir uses PR# + 10000 offset on collision. Is this the right approach, or should we use a separate namespace (e.g. `adhoc-123`)?
2. **Startup reconciliation scope**: Should we attempt full GitHub API reconciliation (detect open PRs, sync worktrees) or just load persisted state and let the poller catch up?
3. **Headless output format**: Plain text logs vs. structured JSON (for piping to other tools)?

## Explicit Opinions

1. **Tests before features.** Patches 1–4 are all `[INFRA]` test-only changes. This ensures the existing logic is thoroughly verified before we start modifying it for new features. A regression caught by a property test is worth 10x a regression caught in production.

2. **Invariants as oracle, not just assertion.** The `Invariants` module should be wired into every QCheck property as a postcondition check, not just used behind an env var. This turns the formal spec into an executable contract.

3. **TUI input as separate module.** `tui_input.ml` should be pure (parse bytes → command variant), tested independently of terminal state. The Eio stdin-reading fiber is the only I/O boundary.

4. **Concurrency cap in the runner, not the orchestrator.** The orchestrator's `tick` should remain pure (fire all actions whose preconditions hold). The runner fiber applies the concurrency limit when deciding how many to actually spawn. This preserves the spec's liveness property at the logic layer.

5. **Session fallback is a two-shot mechanism.** First try: `--resume`. If that fails and we haven't already tried fresh: start fresh session. If fresh also fails: `needs_intervention`. No infinite retry loops.

## Patches

### Patch 1 [INFRA]: Reconciler QCheck2 properties

**Files to modify:**
- `lib_test/test_generators.ml`
- `test/test_reconciler_properties.ml` (new)
- `test/dune`

**Changes:**
1. Add `reconciler_patch_view` generator to `test_generators.ml` — generates `Reconciler.patch_view` values with valid field combinations
2. Add `newly_merged_subset` generator — picks a subset of patch_views to mark as newly merged
3. Properties to test:
   - `detect_merges` only emits `Mark_merged` for patches not already merged
   - `detect_merges` emits exactly one action per newly-merged patch
   - `detect_rebases` only targets dependents with `has_pr` and `~merged`
   - `detect_rebases` never targets the merged patch itself
   - `plan_operations` only emits actions for idle patches (has_pr, ~merged, ~busy, ~needs_intervention)
   - `plan_operations` respects priority ordering (rebase < human < merge_conflict < ci < review_comments)
   - `reconcile` output is deterministic (same input → same output)
   - `reconcile` = `detect_merges` ++ `detect_rebases` ++ `plan_operations` (composition identity)

### Patch 2 [INFRA]: Persistence round-trip QCheck2 properties

**Files to modify:**
- `lib_test/test_generators.ml`
- `test/test_persistence_properties.ml` (new)
- `test/dune`

**Changes:**
1. Add `runtime_snapshot` generator to `test_generators.ml` — builds valid `Runtime.snapshot` with consistent orchestrator, activity_log, and gameplan
2. Properties to test:
   - `snapshot_of_yojson (snapshot_to_yojson snap) = Ok snap` (round-trip identity)
   - `save` then `load` produces equivalent snapshot
   - Malformed JSON returns `Error` (not exception)
   - Agent IDs mismatching gameplan patches returns `Error`

### Patch 3 [INFRA]: Orchestrator tick/spawn QCheck2 properties

**Files to modify:**
- `test/test_orchestrator_properties.ml` (new)
- `test/dune`

**Changes:**
1. Properties to test:
   - `tick` only produces `Start` for patches where `in_gameplan`, `deps_satisfied`, `~has_pr`
   - `tick` only produces `Respond` for patches where `has_pr`, `~merged`, `~busy`, `~needs_intervention`, non-empty queue
   - `Start` is biconditional: `in_gameplan ∧ deps_satisfied ∧ ~has_pr ↔ has_pr' ∧ satisfies'` (spec line 118–119)
   - After `tick`, all fired agents are `busy`
   - `pending_actions` returns the same list as the actions from `tick` (consistency)
   - `fire` then `tick` produces no duplicate actions for already-fired patches
   - Max concurrency: number of `Start`+`Respond` actions ≤ number of eligible patches

### Patch 4 [INFRA]: State machine command sequence properties (P1–P7)

**Files to modify:**
- `test/test_state_machine_properties.ml` (new)
- `test/dune`

**Changes:**
1. Define command type: `Start | Respond of Operation_kind.t | Complete | Enqueue of Operation_kind.t | Mark_merged | Set_has_conflict | Increment_ci | Add_comment`
2. Generate random command sequences, apply each to `Patch_agent.t`, check invariants after every step
3. Properties:
   - **P1 (no silent drops)**: every enqueued operation eventually appears in a `Respond` or remains in queue
   - **P2 (session continuity)**: `has_session` never transitions from `true` to `false` (spec: "Sessions are never lost")
   - **P3 (no infinite retry)**: `needs_intervention` is set after 3 CI failures (spec: ci_failure_count >= 3)
   - **P4 (queue drain)**: after `Respond k`, `k` is no longer in queue
   - **P5 (terminal absorbing)**: once `merged`, all subsequent operations leave `merged = true`
   - **P6 (busy invariant)**: `busy` is true iff between a `Start`/`Respond` and a `Complete`
   - **P7 (invariants oracle)**: `Invariants.check_invariants` returns `[]` after every valid transition

### Patch 5 [BEHAVIOR]: TUI keyboard input loop

**Files to modify:**
- `lib/tui_input.ml` (new)
- `lib/tui_input.mli` (new)
- `lib/dune`
- `bin/main.ml`
- `test/test_tui_input.ml` (new)
- `test/dune`

**Changes:**
1. New `Tui_input` module with pure command parsing:
   ```ocaml
   type command =
     | Quit
     | Select_patch of Patch_id.t
     | Scroll_up | Scroll_down
     | Send_message of Patch_id.t * string
     | Add_pr of int
     | Remove_patch of Patch_id.t
     | Refresh
     | Noop

   val parse_key : char -> command
   val parse_line : string -> command
   ```
2. In `main.ml`, add a fourth Eio fiber that reads stdin in raw mode, dispatches parsed commands
3. `Quit` → set shutdown flag, exit fiber loop
4. `Refresh` → force TUI redraw
5. Unit tests for `parse_key` and `parse_line` covering all command variants

### Patch 6 [BEHAVIOR]: Max concurrency cap for Claude processes

**Files to modify:**
- `bin/main.ml`

**Changes:**
1. Add `--max-concurrent` CLI arg (default 4) via Cmdliner
2. In `runner_fiber`, count currently-busy agents via `Runtime.read`
3. Take only `max_concurrent - busy_count` actions from `pending_actions`
4. Preserve topological ordering when truncating (earlier patches first)

### Patch 7 [BEHAVIOR]: Session resume fallback with guard

**Files to modify:**
- `lib/patch_agent.ml` / `.mli`
- `bin/main.ml`

**Changes:**
1. Add `resume_attempted : bool` field to `Patch_agent.t` (default false)
2. Add `set_resume_attempted : t -> t` and `resume_attempted : t -> bool` to `.mli`
3. In `run_claude_and_handle`, when session_id is `Some _`:
   - First attempt: run with `--resume`
   - On failure: if `~resume_attempted`, call `set_resume_attempted`, try fresh (no session_id)
   - On second failure: mark `session_failed`, set `needs_intervention`
4. Add `Orchestrator.set_resume_attempted` pass-through

### Patch 8 [BEHAVIOR]: Addressed comment deduplication

**Files to modify:**
- `lib/patch_agent.ml` / `.mli`
- `bin/main.ml` (runner_fiber Respond handler)

**Changes:**
1. Add `addressed_comment_ids : Set.M(String).t` field to `Patch_agent.t`
2. Add `mark_comments_addressed : t -> string list -> t` — adds IDs to the set
3. Add `is_comment_addressed : t -> string -> bool` — checks membership
4. In poller's comment handling: filter out comments whose IDs are in `addressed_comment_ids` before calling `add_pending_comment`
5. In runner's `Respond Review_comments` handler: after successful Claude run, call `mark_comments_addressed` with the IDs of delivered comments
6. Update `restore` and persistence serialization for the new field

### Patch 9 [BEHAVIOR]: Startup reconciliation

**Files to modify:**
- `lib/startup_reconciler.ml` (new)
- `lib/startup_reconciler.mli` (new)
- `lib/dune`
- `bin/main.ml`

**Changes:**
1. New `Startup_reconciler` module:
   ```ocaml
   val reconcile :
     persisted:Runtime.snapshot option ->
     gameplan:Types.Gameplan.t ->
     main_branch:Types.Branch.t ->
     worktree_exists:(Types.Patch_id.t -> bool) ->
     Runtime.snapshot
   ```
2. Logic:
   - If no persisted state: create fresh from gameplan
   - If persisted: validate agent IDs match gameplan patches
   - Clear all `busy` flags (no Claude process survives restart)
   - Clear `session_failed` flags (allow retry)
   - For patches with `has_pr` and a worktree: preserve state
   - For patches with `has_pr` but no worktree: clear `has_pr` (will be re-started)
3. In `main.ml`: attempt `Persistence.load`, pass to `Startup_reconciler.reconcile`, use result as initial runtime state

### Patch 10 [BEHAVIOR]: Human messaging and ad-hoc PR wiring

**Files to modify:**
- `lib/tui_input.ml`
- `lib/orchestrator.ml` / `.mli`
- `bin/main.ml`

**Changes:**
1. Wire `Send_message` command: enqueue `Human` operation on target patch, add message to pending_comments
2. Wire `Add_pr` command:
   - Create a new `Patch_agent.t` for the PR number
   - Add to orchestrator (need new `Orchestrator.add_adhoc_patch` function)
   - Discover branch from GitHub, create worktree
3. Wire `Remove_patch` command:
   - Remove from orchestrator (new `Orchestrator.remove_patch` function)
   - Clean up worktree
4. Add confirmation flow for `Remove_patch` (requires TUI state for pending confirmation)

### Patch 11 [BEHAVIOR]: Scrollable and selectable TUI with detail view

**Files to modify:**
- `lib/tui.ml` / `.mli`
- `lib/tui_input.ml`
- `bin/main.ml`

**Changes:**
1. Add `tui_state` record:
   ```ocaml
   type tui_state = {
     scroll_offset : int;
     selected : Patch_id.t option;
     visible_rows : int;  (* derived from terminal height *)
   }
   ```
2. `render_frame` takes `tui_state` — slices `views` by `scroll_offset..scroll_offset+visible_rows`
3. Highlight selected patch row with reverse video
4. When a patch is selected, render detail panel below the table:
   - Full activity log for that patch
   - Current queue contents
   - Session ID, base branch, CI failure count
5. Wire `Scroll_up`/`Scroll_down`/`Select_patch` commands to update `tui_state`
6. Share `tui_state` via `Eio.Mutex` between input fiber and render fiber

### Patch 12 [BEHAVIOR]: Headless mode

**Files to modify:**
- `bin/main.ml`

**Changes:**
1. Add `--no-tui` flag via Cmdliner
2. When `--no-tui`: replace TUI fiber with a log fiber that prints state changes to stdout
3. Log fiber: subscribe to activity log changes, print each entry as a formatted line
4. No raw terminal mode, no ANSI codes, no cursor manipulation
5. Ctrl+C terminates cleanly (default signal handling)

## Test Map

| Test Name | File | Stub Patch | Impl Patch |
|-----------|------|------------|------------|
| detect_merges only emits for non-merged | test/test_reconciler_properties.ml | — | 1 |
| detect_rebases targets dependents only | test/test_reconciler_properties.ml | — | 1 |
| plan_operations respects priority | test/test_reconciler_properties.ml | — | 1 |
| reconcile = compose(detect, rebase, plan) | test/test_reconciler_properties.ml | — | 1 |
| snapshot round-trip identity | test/test_persistence_properties.ml | — | 2 |
| malformed JSON returns Error | test/test_persistence_properties.ml | — | 2 |
| tick Start biconditional | test/test_orchestrator_properties.ml | — | 3 |
| tick Respond preconditions | test/test_orchestrator_properties.ml | — | 3 |
| pending_actions = tick actions | test/test_orchestrator_properties.ml | — | 3 |
| P2: session continuity | test/test_state_machine_properties.ml | — | 4 |
| P3: CI failure cap | test/test_state_machine_properties.ml | — | 4 |
| P4: queue drain after Respond | test/test_state_machine_properties.ml | — | 4 |
| P5: merged is absorbing | test/test_state_machine_properties.ml | — | 4 |
| P7: invariants oracle | test/test_state_machine_properties.ml | — | 4 |
| parse_key quit | test/test_tui_input.ml | — | 5 |
| parse_line N> message | test/test_tui_input.ml | — | 5 |
| parse_line +123 | test/test_tui_input.ml | — | 5 |

Note: Patches 1–4 are test-only (`[INFRA]`), so stubs and implementations are in the same patch. The OCaml test convention uses `%test` / `%expect_test` / QCheck2 directly — no `.skip` marker pattern.

## Dependency Graph

```
- Patch 1 [INFRA] -> []
- Patch 2 [INFRA] -> []
- Patch 3 [INFRA] -> []
- Patch 4 [INFRA] -> []
- Patch 5 [BEHAVIOR] -> []
- Patch 6 [BEHAVIOR] -> []
- Patch 7 [BEHAVIOR] -> []
- Patch 8 [BEHAVIOR] -> []
- Patch 9 [BEHAVIOR] -> [2]
- Patch 10 [BEHAVIOR] -> [5]
- Patch 11 [BEHAVIOR] -> [5]
- Patch 12 [BEHAVIOR] -> [6]
```

**Mergability insight**: 4 of 12 patches are `[INFRA]` (test-only) and can ship without changing observable behavior. Patches 1–4 are fully independent and can be parallelized. Patches 5–8 are independent of each other. Patches 9–12 have minimal dependencies.

## Mergability Checklist

- [x] Feature flag strategy documented (not needed — internal tool, all changes additive)
- [x] Early patches contain only non-functional changes (`[INFRA]`)
- [x] Test stubs are N/A (OCaml uses inline tests / QCheck, no `.skip` pattern)
- [x] Test implementations are co-located with the code they test (same patch)
- [x] Test Map is complete
- [x] `[BEHAVIOR]` patches are as small as possible
- [x] Dependency graph shows `[INFRA]` patches early, `[BEHAVIOR]` patches late
- [x] Each `[BEHAVIOR]` patch is clearly justified (adds user-facing capability or enables downstream patches)
