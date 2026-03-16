# Gameplan: onton-complete

## Problem Statement

The first gameplan (`onton-port`) built all the pure logic modules and infrastructure for the OCaml Anton port. However, the system is not yet runnable end-to-end: GitHub HTTP is stubbed, Claude subprocess spawning is missing, the TUI shows only status derivation with no rendering or input, and `main.ml` is a hardcoded demo. Additionally, spec-critical modules (patch_agent, orchestrator, reconciler, graph, poller) have **zero tests** despite being pure and ideal for property-based verification. The application needs to be feature-complete with a polished TUI, comprehensive automated testing, and CI that surfaces maximally useful feedback.

## Solution Summary

Three parallel workstreams:

1. **Feature completion**: Wire cohttp-eio for GitHub API, Eio.Process for Claude CLI, full raw-ANSI TUI, cmdliner CLI, three-fiber Eio event loop, JSON state persistence.
2. **Test suite**: QCheck2 property-based tests for all spec invariants (patch_agent state machine, orchestrator liveness, reconciler merge logic, graph dependency satisfaction, poller correctness). Unit/expect tests for all new modules. QCheck2 generators in a shared test helpers module.
3. **CI pipeline**: GitHub Actions with `ocaml/setup-ocaml@v3`, dune caching, multi-job workflow (build, test, format check, property tests, coverage via bisect_ppx → Codecov, compiler warnings as annotations).

## Mergability Strategy

### Feature Flagging Strategy

**No feature flag needed.** Greenfield codebase — each patch adds new capability.

### Patch Ordering Strategy

**Early (INFRA):** QCheck2 generators + test helpers, property tests for existing pure modules, CI workflow, activity log types, terminal helpers, enriched status display.
**Middle (INFRA):** GitHub HTTP wiring, Claude subprocess runner, shared runtime state, coverage gating.
**Late (BEHAVIOR):** TUI render engine, main event loop, state persistence.

## Current State Analysis

All 15 patches from `onton-port` are complete. Test coverage is heavily skewed:

| Module | Tests | Purity | Gap |
|--------|-------|--------|-----|
| `patch_agent.ml` | 0 | Pure | CRITICAL — state machine untested |
| `orchestrator.ml` | 0 | Pure | CRITICAL — liveness untested |
| `reconciler.ml` | 0 | Pure | CRITICAL — merge/rebase logic untested |
| `graph.ml` | 0 | Pure | HIGH — dependency satisfaction untested |
| `gameplan_parser.ml` | 0 | Pure | HIGH — parsing robustness untested |
| `poller.ml` | 0 | Pure | HIGH — poll action correctness untested |
| `tui.ml` | 0 | Pure | MEDIUM — status derivation untested |
| `prompt.ml` | 2 | Pure | MEDIUM — only 2 of 5 prompt types tested |
| `priority.ml` | 11 | Pure | Low — well-tested |
| `claude_process.ml` | 12 | Pure | Low — well-tested |
| `term.ml` | 8 | Pure | Low — well-tested |
| `markdown_render.ml` | 6 | Pure | Low — good coverage |

**Available opam packages:** `eio`, `eio_main`, `cohttp-eio`, `cmdliner`, `yojson`, `base`, `qcheck-core` (0.91)

**Not yet installed (needed):** `bisect_ppx` (coverage — supports OCaml 5.x as of 2.8.3)

## Required Changes

### New modules

| Module | Purpose |
|--------|---------|
| `lib/test_generators.ml` | Shared QCheck2 generators for all property tests |
| `lib/activity_log.ml` | Per-patch event feed for TUI detail view |
| `lib/claude_runner.ml` | Bridge `claude_process.ml` state machine with `Eio.Process` |
| `lib/runtime.ml` | Mutex-protected shared state |
| `lib/persistence.ml` | JSON serialization to disk |

### New CI files

| File | Purpose |
|------|---------|
| `.github/workflows/ci.yml` | Main CI: build, test, format, coverage |
| `.ocamlformat` | Pin ocamlformat version for CI reproducibility |

### Modifications

| File | Change |
|------|--------|
| `lib/github.ml` | Wire cohttp-eio HTTP POST into `pr_state` stub |
| `lib/github.mli` | Add `~net` parameter to `pr_state` |
| `lib/tui.ml` | Complete rewrite: render engine, input handling, status table, detail pane |
| `lib/term.ml` | Add raw mode, key input parsing, terminal size query, alternate screen |
| `bin/main.ml` | Cmdliner CLI + Eio orchestration loop |
| `lib/dune` | Add `cohttp-eio` to libraries |
| `bin/dune` | Add `cmdliner unix` to libraries |
| `lib/onton.ml` | Re-export new modules |
| `dune-project` | Add `(instrumentation (backend bisect_ppx))` |

## Acceptance Criteria

### Testing
- [ ] QCheck2 property tests verify all 7 spec properties (P1-P7) for the patch_agent state machine
- [ ] Orchestrator liveness property tested: all respondable patches fire within one tick
- [ ] Reconciler merge-absorbing property tested: once merged, never unmarked
- [ ] Graph dependency satisfaction tested: `deps_satisfied` matches spec definition
- [ ] Poller queue derivation tested: queue contents match PR state signals
- [ ] Gameplan parser validated: rejects cycles, self-deps, missing targets, duplicates; never crashes on arbitrary input
- [ ] All new modules (activity_log, claude_runner, runtime, persistence, TUI) have inline tests
- [ ] `dune runtest` runs all tests (inline + QCheck2) in under 30s
- [ ] Coverage report generated via bisect_ppx; all spec-critical modules > 90% line coverage

### CI
- [ ] GitHub Actions CI runs on every push and PR
- [ ] Build step: `dune build` with zero warnings — compiler errors appear as file/line annotations
- [ ] Test step: `dune runtest` — failures appear in CI logs
- [ ] Format step: `dune fmt --check` — formatting violations fail the check
- [ ] Property test step: QCheck2 with 10,000 iterations — flaky properties caught
- [ ] Coverage step: bisect_ppx report uploaded to Codecov — coverage trends visible on PRs
- [ ] CI caches opam switch and dune build artifacts for fast runs (<5min target)

### Feature-complete
- [ ] `onton --gameplan FILE` parses gameplan file and starts orchestration
- [ ] GitHub poller fetches PR state via GraphQL every 30s
- [ ] Claude agents are spawned in worktrees with proper prompts
- [ ] Reconciler detects merges, queues rebases, spawns unblocked patches
- [ ] State persists to `.onton/state.json` and survives restarts

### TUI quality (matching Elixir reference)
- [ ] Title bar with project name (cyan bg, bold)
- [ ] Status table: `#`, `Status` (colored, context-sensitive), `PR`, `Deps` (color-coded by dep status), `Title`
- [ ] Detail pane: per-patch activity log with markdown rendering, or project overview/timeline
- [ ] Input line with `> ` prompt, `N> msg` to send messages, `+PR#N` to add PRs
- [ ] Navigation: arrow keys, number to select patch, `0` for overview, `q` to quit
- [ ] Scrolling in both table and detail pane
- [ ] Context-sensitive status: `>> running`, `>> fixing CI`, `>> addressing`, `merge conflict`, `CI failed`, `has comments`, `✓ merged`, `✓ approved`, `⚠ needs help`, `awaiting review`, `pending`, `rebasing`
- [ ] Clean terminal restore on exit (alternate screen, raw mode cleanup)

### Compile-time
- [ ] `dune build` with zero warnings after each patch
- [ ] `dune runtest` passes after each patch

## Open Questions

None — architecture decisions carried over from first gameplan.

## Explicit Opinions

1. **Raw ANSI TUI, no library**: `term.ml` already has ANSI primitives. No notty/minttea — none support Eio + OCaml 5.4 well.
2. **Single Eio.Mutex for shared state**: Coarse but sufficient.
3. **One fiber per Claude process**: Natural backpressure — busy patches don't get new work.
4. **`~net` parameter on `pr_state` only**: Keeps `Github.t` serializable and testable.
5. **Status display is richer than current `derive_display_status`**: Match the Elixir reference's ~15 context-sensitive statuses.
6. **QCheck2 generators as shared test infrastructure**: A single `test_generators.ml` module provides arbitrary instances for all types (Patch_id, Operation_kind, Patch_agent, Orchestrator, patch_view, etc.). All property tests import from this module.
7. **CI surfaces feedback at maximum resolution**: Compiler warnings as line annotations, test failures as check failures, format violations as check failures, coverage as PR comments. An LLM editing this codebase should get all feedback from CI without human intervention.
8. **bisect_ppx for coverage**: Compatible with OCaml 5.x (v2.8.3). Gated behind `(instrumentation ...)` so it doesn't affect production builds. Coverage report sent to Codecov for PR-level feedback.

## Patches

### Patch 1 [INFRA]: QCheck2 generators and test helpers

**Files to create:**
- `lib/test_generators.ml`

**Changes:**
1. QCheck2 generators for all core types:
   ```ocaml
   val gen_patch_id : Patch_id.t QCheck2.Gen.t
   val gen_operation_kind : Operation_kind.t QCheck2.Gen.t
   val gen_branch : Branch.t QCheck2.Gen.t
   val gen_comment : Comment.t QCheck2.Gen.t
   val gen_operation_queue : Operation_kind.t list QCheck2.Gen.t
   ```
2. Composite generators for state machine testing:
   ```ocaml
   val gen_patch_agent : Patch_agent.t QCheck2.Gen.t
   val gen_patch_agent_with_pr : Patch_agent.t QCheck2.Gen.t
   val gen_respondable_agent : Patch_agent.t QCheck2.Gen.t
   val gen_busy_agent : Patch_agent.t QCheck2.Gen.t
   ```
3. Graph and orchestrator generators:
   ```ocaml
   val gen_dag : int -> (Patch_id.t list * (Patch_id.t * Patch_id.t list) list) QCheck2.Gen.t
   val gen_patch_view : Reconciler.patch_view QCheck2.Gen.t
   val gen_pr_state : Github.Pr_state.t QCheck2.Gen.t
   ```
4. Helper functions:
   ```ocaml
   val make_agent : ?has_pr:bool -> ?busy:bool -> ?merged:bool ->
     ?needs_intervention:bool -> ?queue:Operation_kind.t list ->
     ?ci_failure_count:int -> ?has_conflict:bool ->
     ?pending_comments:Patch_agent.pending_comment list ->
     Patch_id.t -> Patch_agent.t
   (** Smart constructor for tests — builds agent in specific state
       by applying the right sequence of operations *)
   ```
5. Inline tests validating generator invariants (generated agents satisfy basic type constraints)

### Patch 2 [INFRA]: Property tests for patch_agent state machine

**Files to modify:**
- `lib/patch_agent.ml` (add tests at end of file)

**Changes — QCheck2 property tests encoding spec (anton.pant) properties:**

1. **P1 "Sessions never lost"**: After any valid transition on an agent with `has_session=true`, `has_session` remains true.
2. **P2 "Merged is absorbing"**: After `mark_merged`, no transition changes `merged` back to false.
3. **P3 "Queue isolation"**: `respond k` removes only `k` from queue; all other entries preserved.
4. **P4 "CI failure cap"**: After `complete` with `ci_failure_count >= 3` and no Human in queue, `needs_intervention` is true.
5. **P5 "Human clears intervention"**: After `complete` with Human in queue, `needs_intervention` is false regardless of ci_failure_count.
6. **P6 "Enqueue idempotent"**: `enqueue k (enqueue k agent) = enqueue k agent`.
7. **P7 "Respond preconditions"**: `respond` on an agent violating any precondition (no PR, merged, busy, needs_intervention, kind not in queue, not highest priority) raises `Invalid_argument`.
8. **P8 "Start postconditions"**: After `start`, agent has `has_pr=true`, `has_session=true`, `busy=true`, `satisfies=true`, `base_branch=Some base`.
9. **P9 "Complete postconditions"**: After `complete`, `busy=false`.
10. **P10 "Respond effects by kind"**: CI → `changed=true`. Merge_conflict → `has_conflict=false`. Review_comments → `pending_comments=[]`. Human → `satisfies=false`.

**Unit/expect tests (deterministic edge cases):**
- Start on agent that already has_pr → raises
- Respond on merged agent → raises
- Complete on non-busy agent → raises
- CI failure count 2→3 boundary (no intervention at 2, intervention at 3)
- Pending comments with mixed valid/invalid flags

### Patch 3 [INFRA]: Property tests for orchestrator, reconciler, graph, poller

**Files to modify:**
- `lib/orchestrator.ml` (add tests)
- `lib/reconciler.ml` (add tests)
- `lib/graph.ml` (add tests)
- `lib/poller.ml` (add tests)
- `lib/gameplan_parser.ml` (add tests)

**Changes:**

**Orchestrator properties:**
1. "Liveness": For every agent with `has_pr && !merged && !busy && !needs_intervention && queue non-empty`, `pending_actions` includes a `Respond` for that agent.
2. "Startable completeness": For every agent with `!has_pr && deps_satisfied`, `pending_actions` includes a `Start`.
3. "Tick fires all": After `tick`, no pending_actions remain (tick is idempotent).
4. "Sessions preserved through tick": All agents with `has_session` before tick still have it after.

**Reconciler properties:**
1. "Merged is absorbing": `detect_merges` never emits `Mark_merged` for already-merged patches.
2. "Rebase targets are dependents": Every `Enqueue_rebase pid` in `detect_rebases` result has `pid` as a dependent of some newly_merged patch.
3. "No duplicate rebases": `detect_rebases` returns at most one `Enqueue_rebase` per patch.
4. "Plan respects priority": Every `Start_operation` picks the `highest_priority_op` from the view's queue.

**Graph properties:**
1. "deps_satisfied iff open_pr_deps <= 1 and all deps have PR or merged": Equivalence with spec definition.
2. "initial_base = main when 0 open deps": No open deps → base is main.
3. "initial_base = branch_of(sole_open_dep) when 1 open dep".
4. "dependents is reverse of deps": If `d in deps(p)`, then `p in dependents(d)`.
5. "deps are deduplicated": No duplicate entries in `deps p`.

**Poller properties:**
1. "Queue reflects PR signals": `Review_comments in queue iff unresolved_count > 0`, `Merge_conflict in queue iff has_conflict`, `Ci in queue iff ci_failed`.
2. "Merged is monotonic": If `was_merged=true`, result `merged=true` regardless of PR state.
3. "Mergeable/checks observed passively": `result.mergeable = Github.mergeable pr` and `result.checks_passing = Github.checks_passing pr`.

**Gameplan parser properties:**
1. "Never crashes on arbitrary input": `parse_string` returns `Ok _` or `Error _`, never raises.
2. "Valid gameplans pass validation": Generated valid gameplans (no cycles, no self-deps, all targets exist) pass `parse_string`.
3. "Slugify is idempotent": `slugify (slugify name) = slugify name`.

**Unit tests (deterministic):**
- Orchestrator: empty graph, single patch lifecycle, missing branch_map entry
- Reconciler: merge cascade, diamond dependency rebase, busy patch not rebased
- Graph: no deps, single dep, diamond, all merged
- Poller: no signals, all signals, was_merged true
- Parser: valid gameplan, cyclic deps, self-deps, missing target, duplicate IDs

### Patch 4 [INFRA]: GitHub Actions CI workflow

**Files to create:**
- `.github/workflows/ci.yml`
- `.ocamlformat` (if not present — pin version for CI reproducibility)

**Changes:**

```yaml
name: CI
on:
  push:
    branches: [main]
  pull_request:

jobs:
  build:
    name: Build
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: ocaml/setup-ocaml@v3
        with:
          ocaml-compiler: '5.4.0'
          dune-cache: true
      - run: opam install . --deps-only --with-test
      - run: opam exec -- dune build 2>&1 | tee build.log
      - name: Annotate build errors
        if: failure()
        run: |
          grep -E '^File "' build.log | while IFS= read -r line; do
            file=$(echo "$line" | sed -n 's/^File "\([^"]*\)", line \([0-9]*\).*/\1/p')
            lineno=$(echo "$line" | sed -n 's/^File "\([^"]*\)", line \([0-9]*\).*/\2/p')
            echo "::error file=$file,line=$lineno::$line"
          done

  test:
    name: Test
    needs: build
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: ocaml/setup-ocaml@v3
        with:
          ocaml-compiler: '5.4.0'
          dune-cache: true
      - run: opam install . --deps-only --with-test
      - run: opam exec -- dune runtest

  property-tests:
    name: Property Tests (10k iterations)
    needs: build
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: ocaml/setup-ocaml@v3
        with:
          ocaml-compiler: '5.4.0'
          dune-cache: true
      - run: opam install . --deps-only --with-test
      - run: opam exec -- dune runtest
        env:
          QCHECK_LONG: "true"

  format:
    name: Format Check
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: ocaml/setup-ocaml@v3
        with:
          ocaml-compiler: '5.4.0'
      - uses: ocaml/setup-ocaml/lint-fmt@v3

  coverage:
    name: Coverage
    needs: build
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: ocaml/setup-ocaml@v3
        with:
          ocaml-compiler: '5.4.0'
          dune-cache: true
      - run: opam install . --deps-only --with-test bisect_ppx
      - run: opam exec -- dune runtest --instrument-with bisect_ppx --force
      - run: opam exec -- bisect-ppx-report send-to Codecov
        env:
          CODECOV_TOKEN: ${{ secrets.CODECOV_TOKEN }}
```

Key features:
- **Build errors as line annotations**: Parses `dune build` output for `File "...", line N` format, emits `::error` annotations that appear on PR diffs
- **Separate property test job**: Runs with `QCHECK_LONG=true` for thorough 10k-iteration testing
- **Format check via official lint action**: `ocaml/setup-ocaml/lint-fmt@v3`
- **Coverage via bisect_ppx → Codecov**: Coverage diff appears as PR comment
- **Dune caching**: `dune-cache: true` for fast rebuilds

Also create/verify `.ocamlformat` pins the version:
```
version = 0.28.1
```

### Patch 5 [INFRA]: Activity log types

**Files to create:**
- `lib/activity_log.ml`
- `lib/activity_log.mli`

**Changes:**
1. Define `entry_kind` variant:
   ```ocaml
   type entry_kind =
     | Spawned | Completed | Failed
     | Human_input | Ai_response | Tool_use | Tool_result
     | System_event
   ```
2. Define `entry` record: `{ timestamp : float; kind : entry_kind; patch_id : Patch_id.t; summary : string }`
3. Define `t` — list, most-recent-first, capped at 200 entries per patch
4. Functions: `empty`, `append`, `for_patch`, `recent`, `all`
5. Inline tests: append respects cap, for_patch filters correctly, empty returns empty list

### Patch 6 [INFRA]: Terminal raw mode, key input, and terminal size

**Files to modify:**
- `lib/term.ml`

**Changes:**
1. `val with_raw_mode : (unit -> 'a) -> 'a` — `Unix.tcsetattr stdin TCSANOW {raw settings}`, restore via `Fun.protect`
2. `val enter_alt_screen : unit -> unit` / `val leave_alt_screen : unit -> unit` — `\027[?1049h` / `\027[?1049l`
3. `val terminal_size : unit -> int * int` — parse `stty size` output (rows, cols)
4. Key input parsing:
   ```ocaml
   type key = Char of char | Up | Down | Left | Right | Enter | Backspace
            | Escape | Ctrl of char | PageUp | PageDown
   val read_key : unit -> key
   ```
   Parse escape sequences: `\027[A` → Up, `\027[B` → Down, `\027[5~` → PageUp, etc.
5. Inline tests for key sequence parsing (using `parse_escape` on known byte sequences)

### Patch 7 [INFRA]: Enriched status display

**Files to modify:**
- `lib/tui.ml`

**Changes:**
1. Replace simple `display_status` with context-sensitive status matching the Elixir reference:
   ```ocaml
   type display_status = {
     label : string;
     style : string list;  (* Term.Sgr codes *)
   }
   val derive_display_status :
     agent:Patch_agent.t ->
     current_op:Operation_kind.t option ->
     display_status
   ```
2. Status mapping (from Elixir `tui.ex:835-891`):
   - `merged` → `"✓ merged"`, dim
   - `needs_intervention` → `"⚠ needs help"`, red bold
   - `busy + Merge_conflict op` → `">> merge conflict"`, red
   - `busy + Ci op` → `">> fixing CI"`, red
   - `busy + Review_comments op` → `">> addressing"`, yellow
   - `busy + Rebase op` → `"rebasing"`, magenta
   - `busy + other feedback` → `">> running"`, green
   - `has_pr + has_conflict` → `"merge conflict"`, red
   - `has_pr + ci_failure_count >= 3` → `"⚠ needs help (CI)"`, red bold
   - `has_pr + ci_failure_count > 0 + Ci queued` → `"CI failed"`, red
   - `has_pr + pending_comments non-empty` → `"has comments"`, yellow
   - `has_pr + ~busy` → `"awaiting review"`, blue
   - `~has_pr` → `"pending"`, dim
3. QCheck2 property: `derive_display_status` is total (never raises for any valid agent state)
4. Inline expect tests for each status derivation path

### Patch 8 [INFRA]: GitHub HTTP client wiring

**Files to modify:**
- `lib/github.ml`
- `lib/github.mli`
- `lib/dune`

**Changes:**
1. Add `cohttp-eio` to `lib/dune` libraries list
2. Update `pr_state` signature to accept `~sw:Eio.Switch.t -> ~net:_ Eio.Net.t`:
   ```ocaml
   val pr_state : sw:Eio.Switch.t -> net:_ Eio.Net.t ->
     t -> Pr_number.t -> (Pr_state.t, error) Result.t
   ```
3. Implement HTTP call: `Cohttp_eio.Client.post` to `https://api.github.com/graphql`, `Authorization: bearer {token}`, pipe through existing `parse_response`
4. Keep all pure functions untouched — existing `parse_response` tests still pass
5. Add inline tests for `build_request_body` output structure and `parse_response` with additional edge cases (null fields, empty review threads, missing rollup)

### Patch 9 [INFRA]: Claude subprocess runner

**Files to create:**
- `lib/claude_runner.ml`
- `lib/claude_runner.mli`

**Changes:**
1. Define type `t` holding `Eio.Process.t` handle + stdout pipe + session tracking
2. `val spawn : sw:Eio.Switch.t -> process_mgr:_ Eio.Process.mgr -> cwd:string -> prompt:string -> ?session_id:Session_id.t -> ?max_turns:int -> unit -> t`
   - Runs: `claude --print --output-format stream-json [--continue --session-id ID] -p PROMPT [--max-turns N]`
3. `val read_events : t -> (Yojson.Safe.t -> unit) -> unit` — NDJSON line reader
4. `val wait : t -> [ `Ok of Session_id.t option | `Failed of int ]`
5. `val cancel : t -> unit` — SIGTERM
6. Pure helper: `val parse_ndjson_line : string -> Yojson.Safe.t option` — handles blank lines, malformed JSON gracefully
7. Pure helper: `val extract_session_id : Yojson.Safe.t -> Session_id.t option` — looks for `session_id` in result events
8. Inline tests for `parse_ndjson_line` and `extract_session_id` with sample stream-json output

### Patch 10 [INFRA]: Shared runtime state

**Files to create:**
- `lib/runtime.ml`
- `lib/runtime.mli`

**Changes:**
1. Define:
   ```ocaml
   type t = {
     mutable orch : Orchestrator.t;
     mutable activity : Activity_log.t;
     mutable pr_numbers : Pr_number.t Map.M(Patch_id).t;
     mutable worktrees : Worktree.t Map.M(Patch_id).t;
     mutable sessions : Claude_process.t Map.M(Patch_id).t;
     gameplan : Gameplan.t;
     patches : Patch.t list;
     github : Github.t;
     main_branch : Branch.t;
     mutex : Eio.Mutex.t;
   }
   ```
2. `val create`, `val with_lock`, `val snapshot`, `val update_orch`, `val log_activity`
3. Tests: snapshot returns consistent state, update_orch applies transformation

### Patch 11 [BEHAVIOR]: TUI render engine

**Files to modify:**
- `lib/tui.ml` (complete rewrite, keeping `derive_display_status` from Patch 7)

**Changes:**

1. **Model** (Elm-style MVU):
   ```ocaml
   type model = {
     selected_patch : Patch_id.t option;
     table_scroll : int;
     detail_scroll : [ `Bottom | `Offset of int ];
     input_buf : string;
     input_history : string list;
     history_idx : int option;
     pending_quit : bool;
     terminal_size : int * int;
   }
   ```

2. **Layout** (matching Elixir `tui.ex:293-325`):
   - Title bar (1 line): `" Onton: {project} "` — cyan bg, bold, black fg, padded to full width
   - Separator: `hrule` in dim
   - Status table header: `#` (4w), `Status` (22w), `PR` (8w), `Deps` (12w), `Title` — dim bold
   - Status rows (max 8 visible, scrollable): columns aligned via `fit_width`, status colored per `derive_display_status`, deps colored per dep status, selected row in cyan bold
   - Scroll indicator if rows > 8: `▲ N/M ▼`
   - Separator
   - Detail pane (remaining height - chrome): patch activity log or project timeline. `Ai_response`/`Tool_result` entries rendered via `Markdown_render.render`. Scrollable.
   - Separator
   - Help line (1 line): keybindings in dim
   - Input line (1 line): `"> {buf}█"` in cyan

3. **Render function**: `val render_frame : model -> runtime_snapshot -> string` — build in `Buffer.t`, `Cursor.move_to`, per-line `Clear.line_to_end`, hide cursor during render

4. **Input dispatch**: `val handle_key : model -> Term.key -> model * side_effect list`
   - Number keys: select patch. `0`: overview. Arrow keys: scroll. Enter: submit. `q`: quit confirm. Backspace: trim. Left/Right: navigate patches. Printable: append to buffer.

5. **Side effects**: `Send_message of Patch_id.t * string | Add_pr of int | Quit | Log of string`

6. **Parse input**: `val parse_input : string -> [ `Message of Patch_id.t * string | `Add_pr of int | `Select of Patch_id.t | `Unknown ]`

7. **Main TUI loop**: `val run : clock:_ Eio.Time.t -> runtime:Runtime.t -> on_effect:(side_effect -> unit) -> unit`
   - Enter alt screen + raw mode. Two fibers: render (2s timer) + input reader. `Fun.protect` for clean exit.

8. **Tests:**
   - Expect tests for `render_frame` (ANSI stripped) with mocked snapshot
   - Unit tests for `handle_key` state transitions
   - Unit tests for `parse_input` (N> msg, +PR#N, number, unknown)
   - QCheck2: `handle_key` never raises for any key + model combination

### Patch 12 [BEHAVIOR]: Main event loop and CLI

**Files to modify:**
- `bin/main.ml` (complete rewrite)
- `bin/dune`

**Changes:**

1. `bin/dune`: Add `cmdliner unix`

2. Cmdliner CLI: `onton [--gameplan FILE] [--token TOKEN] [--max-concurrent N] [--repo OWNER/REPO]`
   - `--token` default: `GITHUB_TOKEN` env or `gh auth token`
   - `--repo` default: from `.git/config`

3. `Eio_main.run` with `Eio.Fiber.all` running 3 fibers:
   - **Reconciler fiber** (30s tick): snapshot → build `patch_view` list → `Reconciler.reconcile` → apply actions → `Orchestrator.tick` → for each Start/Respond, spawn child fiber (gated by `Eio.Semaphore`) that creates worktree, builds prompt, runs Claude, logs activity, calls complete
   - **Poller fiber** (30s tick): for each patch with PR, `Github.pr_state` → `Poller.poll` → apply state updates
   - **TUI fiber**: `Tui.run` with effect handler dispatching side effects

4. Effect handler: `Send_message` → enqueue Human op, `Add_pr` → add external PR, `Quit` → cancel switch

5. Graceful shutdown: TUI quit cancels switch, all fibers terminate, terminal restored

### Patch 13 [INFRA]: State persistence

**Files to create:**
- `lib/persistence.ml`
- `lib/persistence.mli`

**Changes:**
1. `val save : Runtime.t -> filepath:string -> unit` — serialize to JSON via Yojson.Safe
2. `val load : filepath:string -> patches:Patch.t list -> github:Github.t -> main_branch:Branch.t -> (Runtime.t, string) result`
3. Default path: `.onton/state.json`
4. Wire into main loop: save every 60s + on graceful shutdown
5. On startup: check for existing state, load and reconcile with GitHub
6. Inline tests: round-trip (save then load produces equivalent state), handles missing file gracefully, handles corrupt JSON gracefully

## Test Map

| Test Name | File | Stub Patch | Impl Patch |
|-----------|------|------------|------------|
| **QCheck2 generators** | | | |
| gen_patch_agent produces valid agents | lib/test_generators.ml | — | 1 |
| gen_respondable_agent satisfies preconditions | lib/test_generators.ml | — | 1 |
| **Patch agent properties** | | | |
| P1: sessions never lost | lib/patch_agent.ml | — | 2 |
| P2: merged is absorbing | lib/patch_agent.ml | — | 2 |
| P3: queue isolation on respond | lib/patch_agent.ml | — | 2 |
| P4: CI failure cap triggers intervention | lib/patch_agent.ml | — | 2 |
| P5: human clears intervention | lib/patch_agent.ml | — | 2 |
| P6: enqueue idempotent | lib/patch_agent.ml | — | 2 |
| P7: respond precondition violations raise | lib/patch_agent.ml | — | 2 |
| P8: start postconditions | lib/patch_agent.ml | — | 2 |
| P9: complete postconditions | lib/patch_agent.ml | — | 2 |
| P10: respond effects by kind | lib/patch_agent.ml | — | 2 |
| start on has_pr raises | lib/patch_agent.ml | — | 2 |
| respond on merged raises | lib/patch_agent.ml | — | 2 |
| ci_failure boundary 2→3 | lib/patch_agent.ml | — | 2 |
| **Orchestrator properties** | | | |
| liveness: all respondable fire | lib/orchestrator.ml | — | 3 |
| startable completeness | lib/orchestrator.ml | — | 3 |
| tick is idempotent | lib/orchestrator.ml | — | 3 |
| sessions preserved through tick | lib/orchestrator.ml | — | 3 |
| **Reconciler properties** | | | |
| merged never unmarked | lib/reconciler.ml | — | 3 |
| rebase targets are dependents | lib/reconciler.ml | — | 3 |
| no duplicate rebases | lib/reconciler.ml | — | 3 |
| plan respects priority | lib/reconciler.ml | — | 3 |
| **Graph properties** | | | |
| deps_satisfied matches spec | lib/graph.ml | — | 3 |
| initial_base correctness | lib/graph.ml | — | 3 |
| dependents is reverse of deps | lib/graph.ml | — | 3 |
| deps deduplicated | lib/graph.ml | — | 3 |
| **Poller properties** | | | |
| queue reflects PR signals | lib/poller.ml | — | 3 |
| merged is monotonic | lib/poller.ml | — | 3 |
| mergeable/checks observed | lib/poller.ml | — | 3 |
| **Parser properties** | | | |
| never crashes on arbitrary input | lib/gameplan_parser.ml | — | 3 |
| slugify idempotent | lib/gameplan_parser.ml | — | 3 |
| **Activity log** | | | |
| append respects cap | lib/activity_log.ml | — | 5 |
| for_patch filters correctly | lib/activity_log.ml | — | 5 |
| **Terminal** | | | |
| key parsing: escape sequences | lib/term.ml | — | 6 |
| **Status display** | | | |
| enriched status: all 14 paths | lib/tui.ml | — | 7 |
| derive_display_status is total | lib/tui.ml | — | 7 |
| **TUI** | | | |
| render_frame overview | lib/tui.ml | — | 11 |
| handle_key select patch | lib/tui.ml | — | 11 |
| handle_key quit confirm | lib/tui.ml | — | 11 |
| parse_input: N> msg | lib/tui.ml | — | 11 |
| parse_input: +PR#N | lib/tui.ml | — | 11 |
| handle_key never raises | lib/tui.ml | — | 11 |
| **GitHub** | | | |
| parse_response edge cases | lib/github.ml | — | 8 |
| build_request_body structure | lib/github.ml | — | 8 |
| **Claude runner** | | | |
| parse_ndjson_line | lib/claude_runner.ml | — | 9 |
| extract_session_id | lib/claude_runner.ml | — | 9 |
| **Persistence** | | | |
| round-trip save/load | lib/persistence.ml | — | 13 |
| handles missing file | lib/persistence.ml | — | 13 |
| handles corrupt JSON | lib/persistence.ml | — | 13 |

## Dependency Graph

- Patch 1 [INFRA] -> []
- Patch 2 [INFRA] -> [1]
- Patch 3 [INFRA] -> [1]
- Patch 4 [INFRA] -> []
- Patch 5 [INFRA] -> []
- Patch 6 [INFRA] -> []
- Patch 7 [INFRA] -> []
- Patch 8 [INFRA] -> []
- Patch 9 [INFRA] -> []
- Patch 10 [INFRA] -> [5]
- Patch 11 [BEHAVIOR] -> [6, 7, 10]
- Patch 12 [BEHAVIOR] -> [8, 9, 10, 11]
- Patch 13 [INFRA] -> [10]

**Mergability insight**: 11 of 13 patches are `[INFRA]` and can ship without changing observable behavior. Patches 1-9 are almost entirely independent and can be heavily parallelized. The critical test patches (1, 2, 3) should land first to establish the mechanical feedback loop before feature work proceeds.

## Mergability Checklist

- [x] Feature flag strategy documented (not needed — greenfield port)
- [x] Early patches contain only non-functional changes (`[INFRA]`)
- [x] Test stubs are co-located with implementations (OCaml inline tests)
- [x] Test implementations are co-located with the code they test (same patch)
- [x] Test Map is complete
- [x] Test Map Impl Patch matches the patch that implements the tested code
- [x] `[BEHAVIOR]` patches are as small as possible (2 of 13)
- [x] Dependency graph shows `[INFRA]` patches early, `[BEHAVIOR]` patches late
- [x] Each `[BEHAVIOR]` patch is clearly justified (TUI render engine requires wiring; main loop requires all components)
- [x] CI workflow surfaces build errors as line annotations, test failures as check failures, format violations as check failures, and coverage as PR comments
- [x] Property-based tests cover all spec invariants (P1-P10 for patch_agent, liveness for orchestrator, absorbing for reconciler, satisfaction for graph, signals for poller)
- [x] QCheck2 generators are shared infrastructure in Patch 1, enabling all subsequent property tests
