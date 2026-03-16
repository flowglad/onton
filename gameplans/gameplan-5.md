# Gameplan: onton-completeness-pt-5

## Problem Statement

The OCaml onton codebase has three categories of gaps relative to the Elixir reference implementation and Pantagruel spec: (1) decision logic is tangled with I/O in `main.ml`, making it untestable; (2) several spec features are missing or incorrect (approved? tracking, orchestrator-executed rebase, conflict clearing, CI failure cap in poller); (3) the TUI detail view is sparse and lacks help/scroll/input-prompt UX. These gaps undermine the project's goals of maximal mechanical feedback, spec parity, and TUI polish.

## Solution Summary

Extract pure decision modules (`Patch_decision`, `Spawn_logic`) from `main.ml` to mirror the Elixir architecture. Add `mergeable`, `checks_passing`, `no_unresolved_comments` fields to `Patch_agent` so the spec's `approved?` derivation works. Implement orchestrator-executed `git rebase` in `Worktree`. Add poller property tests and decision logic property tests. Fix spec violations (clear `has_conflict`, CI cap in poller, `check_resolved_not_pending` invariant). Polish the TUI with detail view activity, help overlay, scroll in detail view, and visible input prompt.

## Mergability Strategy

### Feature Flagging Strategy

No feature flag needed. All changes are internal refactors, spec-correctness fixes, or TUI improvements with no external-facing API changes.

### Patch Ordering Strategy

**Early** (`[INFRA]`): New types/fields, pure logic modules with tests, test stubs.
**Middle** (`[BEHAVIOR]`): Wire new modules into `main.ml`, fix spec violations.
**Late** (`[BEHAVIOR]`): TUI rendering changes.

## Current State Analysis

### Decision logic in main.ml (~1500 lines)

`main.ml` contains `runner_fiber` (lines 941–1171), `poller_fiber` (lines 801–937), and `input_fiber` (lines 415–740). Each mixes pure decision logic with Eio I/O. The Elixir codebase separates these into `PatchDecision`, `PatchSupervisor.Logic`, `QueueDelivery.Logic`, and `GitHubPoller.Logic` — all tested with property tests.

### Missing Patch_agent fields

`Patch_agent.t` lacks `mergeable`, `checks_passing`, and `no_unresolved_comments`. `Poller.poll` returns these values but `main.ml` discards them. The spec's `approved?` derivation (`approved? p <-> has-pr p, mergeable p, checks-passing p, no-unresolved-comments p, ~busy p, ~needs-intervention p`) cannot be computed.

### Rebase is Claude-prompted, not orchestrator-executed

The spec (line 87) says "Rebase is performed by the orchestrator, not the agent." The OCaml code sends a merge-conflict prompt to Claude for rebases (`main.ml:1110-1112`). The Elixir `Rebase.execute/3` runs `git rebase` directly via `Worktree.rebase_onto_main`.

### has_conflict never cleared

`main.ml:859` sets `has_conflict` on poll when GitHub reports a conflict, but never clears it when the conflict is resolved. The spec says `k = merge-conflict -> ~has-conflict' p` (Respond postcondition).

### CI failure cap not enforced in poller

The spec says `ci-failure-count >= 3 -> needs-intervention`. The `Complete` action checks this, but the poller continues enqueuing CI operations even after 3 failures. Elixir's `PatchDecision.on_ci_failure` returns `:cap_reached` at 3.

### check_resolved_not_pending is a no-op

`invariants.ml:35-37` returns `[]` with a TODO comment. The `Comments.t` type lacks enumeration, so the invariant `resolved c → ¬pending c p` can't be verified.

### TUI gaps

- Detail view shows only static fields — no activity log, no pending comment content, no CI check details.
- Help command is wired but ignored (`main.ml:735`).
- No scroll in detail view (`main.ml:692` skips it).
- Text-mode input has no visible prompt indicator.

## Required Changes

### New module: `lib/patch_decision.ml` / `.mli`

```ocaml
type disposition = Skip | Queue | Deliver

val disposition : merged:bool -> busy:bool -> disposition

val on_human_message : merged:bool -> busy:bool -> [ `Queue | `Resume | `Reject ]

val on_review_comments :
  merged:bool -> busy:bool ->
  addressed_ids:Set.M(Types.Comment_id).t ->
  Types.Comment.t list ->
  [ `Skip | `Queue of Types.Comment.t list | `Deliver of Types.Comment.t list ]

val on_ci_failure :
  merged:bool -> busy:bool -> ci_failure_count:int ->
  [ `Queue | `Deliver | `Cap_reached | `Reject ]

val on_merge_conflict :
  merged:bool -> busy:bool -> [ `Queue | `Deliver | `Noop ]
```

### New module: `lib/spawn_logic.ml` / `.mli`

```ocaml
type spawn_action =
  | Start of { patch_id : Types.Patch_id.t; base_branch : Types.Branch.t }
  | Respond of { patch_id : Types.Patch_id.t; kind : Types.Operation_kind.t }

val plan_spawns :
  agents:Patch_agent.t list ->
  patches:Types.Patch.t list ->
  graph:Graph.t ->
  main:Types.Branch.t ->
  spawn_action list
```

### Extended fields on Patch_agent.t (`lib/patch_agent.ml`, `lib/patch_agent.mli`)

Add fields: `mergeable : bool`, `checks_passing : bool`, `no_unresolved_comments : bool`.
Add setters: `set_mergeable`, `set_checks_passing`, `set_no_unresolved_comments`.
Add query: `is_approved : t -> bool` (derives `approved?` from spec).
Add setter: `clear_has_conflict : t -> t`.
Update `restore` with new fields.

### Worktree rebase (`lib/worktree.ml`, `lib/worktree.mli`)

```ocaml
type rebase_result = Ok | Noop | Conflict | Error of string

val rebase_onto : process_mgr:_ Eio.Process.mgr -> path:string -> target:string -> rebase_result
```

### Comments enumeration (`lib/state.ml`, `lib/state.mli`)

Add to `Comments` module:
```ocaml
val all_resolved : t -> Types.Comment.t list
val all_pending : t -> (Types.Comment.t * Types.Patch_id.t) list
```

### TUI detail view activity (`lib/tui.ml`, `lib/tui.mli`)

Extend `patch_view` with:
```ocaml
  activity : activity_entry list;
  pr_number : Types.Pr_number.t option;
```

Add `render_help_overlay : width:int -> height:int -> string list`.

### Persistence (`lib/persistence.ml`)

Update `patch_agent_to_yojson`/`patch_agent_of_yojson` for new fields (`mergeable`, `checks_passing`, `no_unresolved_comments`). Backward-compat: default `false` when missing.

## Acceptance Criteria

- [ ] `Patch_decision` module exists with property tests covering all branches
- [ ] `Spawn_logic` module exists with property tests
- [ ] `Poller` module has property tests
- [ ] `Patch_agent.t` has `mergeable`, `checks_passing`, `no_unresolved_comments` fields
- [ ] `is_approved` correctly derives the spec's `approved?` predicate
- [ ] `main.ml` poller stores `mergeable`, `checks_passing`, `no_unresolved_comments` on each poll
- [ ] Rebase action executes `git rebase` via `Worktree.rebase_onto`, not Claude prompt
- [ ] `has_conflict` is cleared when Respond completes a merge-conflict operation
- [ ] CI failure cap (>=3) prevents poller from enqueuing further CI operations
- [ ] `check_resolved_not_pending` invariant is implemented (non-no-op)
- [ ] TUI detail view shows per-patch activity entries
- [ ] TUI help overlay renders on `h` keypress
- [ ] TUI detail view supports scroll (up/down navigation)
- [ ] TUI text-mode shows visible input prompt at bottom
- [ ] All existing tests pass; `dune build` clean

## Open Questions

1. **Rebase conflict handling**: When `Worktree.rebase_onto` returns `Conflict`, should we abort the rebase and enqueue a merge-conflict operation for Claude, or leave the conflict in-tree for Claude to resolve? (Elixir aborts and flags.)

## Explicit Opinions

1. **Extract, don't rewrite**: `main.ml` fibers keep their structure — we extract pure decision functions and call them from the same locations. This minimizes risk.
2. **approved? is derived, not stored**: Like the spec says, `approved?` is a derivation, not mutable state. `Patch_agent.is_approved` computes it from the constituent fields.
3. **Rebase abort on conflict**: Follow the Elixir pattern — `git rebase --abort` on conflict, set `has_conflict`, let Claude handle it via merge-conflict prompt. The orchestrator does the `git rebase`, but Claude resolves conflicts.
4. **CI cap at poller level**: Don't just check at Complete — also skip enqueuing CI operations when `ci_failure_count >= 3`. This prevents wasted Claude sessions.
5. **Detail view activity is filtered**: Show only entries for the selected patch, not the global log.

## Patches

### Patch 1 [INFRA]: Add Patch_decision module with property tests

**Files to create:**
- `lib/patch_decision.ml`
- `lib/patch_decision.mli`
- `test/test_patch_decision.ml`

**Changes:**
1. Implement `disposition`, `on_human_message`, `on_review_comments`, `on_ci_failure`, `on_merge_conflict` as pure functions mirroring `Anton.PatchDecision`
2. Write QCheck2 property tests for all branches:
   - `merged` always produces `Skip`/`Reject`/`Noop`
   - `busy` always produces `Queue`
   - `on_ci_failure` returns `Cap_reached` when `ci_failure_count >= 3`
   - `on_review_comments` filters by `addressed_ids`
3. Register test in `test/dune`

### Patch 2 [INFRA]: Add Spawn_logic module with property tests

**Files to create:**
- `lib/spawn_logic.ml`
- `lib/spawn_logic.mli`
- `test/test_spawn_logic.ml`

**Changes:**
1. Extract the action-planning logic from `runner_fiber` (currently `Orchestrator.pending_actions` + fire loop at `main.ml:953-970`) into a pure `plan_spawns` function
2. Write QCheck2 property tests:
   - Only non-busy, non-merged, non-intervention agents with queued operations produce actions
   - Start only for patches without PRs where deps are satisfied
   - Respond only for patches with PRs, respecting priority

### Patch 3 [INFRA]: Add poller property tests

**Files to create:**
- `test/test_poller_properties.ml`

**Changes:**
1. Write QCheck2 property tests for `Poller.poll`:
   - `was_merged = true` implies `merged = true` in result
   - `ci_failed` implies `Ci` in result queue
   - `has_conflict` implies `Merge_conflict` in result queue
   - New comments excludes `addressed_ids`
   - `mergeable` and `checks_passing` are passed through from PR state
2. Register test in `test/dune`

### Patch 4 [INFRA]: Add mergeable/checks_passing/no_unresolved_comments to Patch_agent

**Files to modify:**
- `lib/patch_agent.ml` (~line 15, type definition)
- `lib/patch_agent.mli` (~line 15, type definition)
- `lib/persistence.ml` (serialization)

**Changes:**
1. Add fields `mergeable : bool`, `checks_passing : bool`, `no_unresolved_comments : bool` to `Patch_agent.t`, defaulting to `false` in `create`
2. Add setters: `set_mergeable`, `set_checks_passing`, `set_no_unresolved_comments`
3. Add `is_approved : t -> bool` that computes `has_pr && mergeable && checks_passing && no_unresolved_comments && not busy && not needs_intervention`
4. Add `clear_has_conflict : t -> t`
5. Update `restore` signature with new fields
6. Update `persistence.ml` to serialize/deserialize new fields with `false` defaults for backward compat
7. Update `test/test_patch_agent.ml` with tests for `is_approved` derivation
8. Update `test/test_persistence_properties.ml` for roundtrip of new fields

### Patch 5 [INFRA]: Add Worktree.rebase_onto

**Files to modify:**
- `lib/worktree.ml`
- `lib/worktree.mli`

**Changes:**
1. Add `type rebase_result = Ok | Noop | Conflict | Error of string`
2. Implement `rebase_onto ~process_mgr ~path ~target`:
   - Run `git -C path merge-base --is-ancestor target HEAD` to detect noop
   - Run `git -C path rebase target`
   - On conflict, run `git -C path rebase --abort`, return `Conflict`
   - Parse exit codes for each case

### Patch 6 [INFRA]: Add Comments enumeration for invariant checking

**Files to modify:**
- `lib/state.ml`
- `lib/state.mli`
- `lib/invariants.ml`

**Changes:**
1. Add `all_resolved : t -> Comment.t list` to `Comments` module — enumerates all comments marked resolved
2. Add `all_pending : t -> (Comment.t * Patch_id.t) list` — enumerates all pending comment/patch pairs
3. Implement `check_resolved_not_pending` in `invariants.ml` using `Comments.all_resolved` and `Comments.all_pending` — verify no comment appears in both sets

### Patch 7 [BEHAVIOR]: Wire Patch_decision into main.ml poller

**Files to modify:**
- `bin/main.ml` (poller_fiber, ~lines 826-894)

**Changes:**
1. After `Poller.poll`, use `Patch_decision.on_ci_failure` to gate CI enqueuing:
   - Read `ci_failure_count` from agent
   - If `Cap_reached`, skip enqueuing `Ci` and log
2. After poll, call `Orchestrator.set_mergeable`, `set_checks_passing`, `set_no_unresolved_comments` from poll result
3. When poll shows no conflict, call `Orchestrator.clear_has_conflict` (new function) — spec: `mergeable' p = world-mergeable p`
4. Add `clear_has_conflict` to `Orchestrator` (delegates to `Patch_agent.clear_has_conflict`)

### Patch 8 [BEHAVIOR]: Wire Patch_decision into main.ml runner

**Files to modify:**
- `bin/main.ml` (runner_fiber, ~lines 941-1171)

**Changes:**
1. Replace inline prompt-selection logic (lines 1088-1112) with `Patch_decision` calls for operation dispatch
2. For `Operation_kind.Rebase`: call `Worktree.rebase_onto` instead of prompting Claude:
   - Compute target branch via `Reconciler.merge_target`
   - Call `Worktree.rebase_onto ~process_mgr ~path ~target`
   - On `Ok`: update base_branch, complete, clear has_conflict
   - On `Conflict`: set has_conflict, enqueue merge-conflict, complete
   - On `Noop`: complete
   - On `Error`: log, mark session failed
3. After Respond completes for `Merge_conflict`, clear `has_conflict` — spec: `k = merge-conflict -> ~has-conflict' p`

### Patch 9 [BEHAVIOR]: TUI detail view with activity and scroll

**Files to modify:**
- `lib/tui.ml` (~line 512, `render_detail`)
- `lib/tui.mli` (`patch_view` type)
- `bin/main.ml` (input_fiber detail view scroll, ~line 692; `views_of_orchestrator` call)

**Changes:**
1. Add `activity : activity_entry list` and `pr_number : Pr_number.t option` to `patch_view`
2. Populate `activity` in `views_of_orchestrator` — caller passes activity log, filtered to matching patch_id
3. Extend `render_detail` to show:
   - PR number (if present)
   - Recent activity entries (last 10 for this patch)
4. Add scroll support in detail view:
   - Track `detail_scroll : int ref` alongside `selected`
   - In input_fiber, handle `Move_up`/`Move_down` for `Detail_view` by adjusting scroll offset
   - In `render_detail`, apply scroll window to the rendered lines

### Patch 10 [BEHAVIOR]: TUI help overlay and input prompt

**Files to modify:**
- `lib/tui.ml`
- `lib/tui.mli`
- `bin/main.ml` (input_fiber help handling, ~line 735; tui_fiber text_mode rendering)

**Changes:**
1. Add `render_help_overlay ~width ~height -> string list` showing all keyboard shortcuts grouped by mode (list, detail, timeline, text)
2. Add `show_help : bool ref` in input_fiber; toggle on `h`, dismiss on any key
3. In tui_fiber, when `show_help` is true, overlay help on top of current frame
4. When `text_mode` is true, render a visible input prompt at the bottom of the frame:
   - Show `:` prefix followed by buffer contents and a cursor block
   - Requires passing `text_mode` and `buf` contents to tui_fiber (via shared ref)

## Test Map

| Test Name | File | Stub Patch | Impl Patch |
|-----------|------|------------|------------|
| disposition: merged always Skip | test/test_patch_decision.ml | 1 | 1 |
| disposition: busy always Queue | test/test_patch_decision.ml | 1 | 1 |
| on_ci_failure: cap at 3 | test/test_patch_decision.ml | 1 | 1 |
| on_review_comments: filters addressed | test/test_patch_decision.ml | 1 | 1 |
| plan_spawns: respects preconditions | test/test_spawn_logic.ml | 2 | 2 |
| plan_spawns: Start only without PR | test/test_spawn_logic.ml | 2 | 2 |
| poll: was_merged implies merged | test/test_poller_properties.ml | 3 | 3 |
| poll: ci_failed implies Ci queued | test/test_poller_properties.ml | 3 | 3 |
| poll: addressed_ids excluded | test/test_poller_properties.ml | 3 | 3 |
| is_approved: derives correctly | test/test_patch_agent.ml | 4 | 4 |
| persistence roundtrip: new fields | test/test_persistence_properties.ml | 4 | 4 |

## Dependency Graph

- Patch 1 [INFRA] -> []
- Patch 2 [INFRA] -> []
- Patch 3 [INFRA] -> []
- Patch 4 [INFRA] -> []
- Patch 5 [INFRA] -> []
- Patch 6 [INFRA] -> []
- Patch 7 [BEHAVIOR] -> [1, 4]
- Patch 8 [BEHAVIOR] -> [1, 4, 5]
- Patch 9 [BEHAVIOR] -> []
- Patch 10 [BEHAVIOR] -> [9]

**Mergability insight**: 6 of 10 patches are `[INFRA]` and can ship without changing observable behavior. Patches 1-6 are all independent and can be parallelized. Patches 7 and 8 can run in parallel once their deps land. Patches 9 and 10 are independent of the logic patches.

## Mergability Checklist

- [x] Feature flag strategy documented (not needed — internal refactors and fixes)
- [x] Early patches contain only non-functional changes (`[INFRA]`)
- [x] Test stubs with `.skip` markers are in early `[INFRA]` patches (tests co-located with new modules)
- [x] Test implementations are co-located with the code they test (same patch)
- [x] Test Map is complete: every test has Stub Patch and Impl Patch assigned
- [x] Test Map Impl Patch matches the patch that implements the tested code
- [x] `[BEHAVIOR]` patches are as small as possible
- [x] Dependency graph shows `[INFRA]` patches early, `[BEHAVIOR]` patches late
- [x] Each `[BEHAVIOR]` patch is clearly justified (cannot be gated or deferred)
