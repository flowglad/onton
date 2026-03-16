# Gameplan: onton-completeness-pt-2

## Problem Statement

Onton is a functional OCaml port of the Anton Elixir orchestrator, but several features are missing or incomplete relative to the Elixir implementation. The most impactful gaps are: no live Claude event streaming (the TUI is blind while agents run), no automatic PR detection from Claude output, incomplete session fallback logic, no comment deduplication by GitHub ID, and missing TUI features (timeline view, input history, signal handling). These gaps make onton unsuitable as a drop-in replacement for Anton.

## Solution Summary

Close all feature parity gaps in priority order: (1) PR number persistence — without it, restarts break polling entirely, (2) persistence migration for forward-compatible snapshots, (3) Claude stream-JSON parsing for live activity and PR auto-detection, (4) startup reconciler completeness (worktree recovery, stale-agent reset), (5) GitHub comment ID tracking for dedup, (6) session resume fallback chain with PR context, (7) CI check detail storage, (8) worktree operations for ad-hoc flows, (9) TUI polish (timeline, history, signals, prompt overrides). Infrastructure patches (types, test stubs) land first; behavior patches follow with tests co-located.

## Mergability Strategy

### Feature Flagging Strategy

**No feature flag needed.** All changes are internal improvements to an orchestrator tool. Each patch is independently useful and backward-compatible with existing persisted state (new fields get defaults on deserialization).

### Patch Ordering Strategy

**Early** (`[INFRA]`): New types (comment IDs, CI check storage, stream events), test property stubs, `.mli` additions.
**Middle** (`[BEHAVIOR]`): Stream parser, comment dedup, session fallback, CI detail propagation.
**Late** (`[BEHAVIOR]`): TUI features (timeline, history, signals), ad-hoc management commands.

## Current State Analysis

### What exists
- Full state machine with QCheck2 property tests and runtime invariant checking
- GitHub GraphQL polling with merge/conflict/CI/comment detection
- Claude subprocess spawning with `--output-format text` (captures final stdout/stderr only)
- TUI with 17 display statuses, list/detail views, keyboard nav, text input mode
- Persistence with JSON snapshots and crash recovery (periodic 5s saves + exit-time save)
- Project data directory (`~/.local/share/onton/<slug>/`) with config, gameplan, and snapshot persistence
- CLI: `onton [PROJECT] --gameplan GAMEPLAN [OPTIONS]` for new projects; `onton PROJECT` to resume from saved state
- `tried_fresh`/`session_failed` flags in `Patch_agent.t` (but no automatic retry logic)
- `Ci_check.t` has `name`, `conclusion`, `details_url`, `description` but patch agent only stores `ci_failure_count`
- Comments tracked by `Comment.t { body; path; line }` — no GitHub database ID

### What's missing
- Stream-JSON NDJSON parsing from Claude CLI (currently `--output-format text`)
- PR number extraction from Claude tool output
- **PR number persistence** — `Pr_registry` is an in-memory hashtable, lost on restart; Elixir stores `pr_number` in `PatchState`
- **Persistence migration** — `persistence.ml` fails on missing fields instead of defaulting; Elixir's `migrate_entries/1` adds missing fields
- Addressed comment tracking by GitHub ID
- Automatic session fallback: resume → fresh → needs_intervention
- **Session resume prompt lacks PR context** — Elixir injects PR #, branch name, URL into fresh-session prompts
- CI check detail storage per patch (only count stored)
- **Startup reconciler gaps** — no worktree recovery for stale/crashed agents, no reset-to-pending for agents that died mid-run
- **Worktree operations** — missing `add_existing` (attach to existing branch), `detect_branch`, `list_with_branches`; needed for ad-hoc flows and startup recovery
- Ad-hoc worktree addition, patch removal TUI commands
- **Prompt override system** — Elixir supports project-specific prompt overrides from `~/.local/share/anton/prompts/`
- Cross-patch timeline view
- TUI input history, SIGTSTP/SIGCONT handling

## Required Changes

### Patch Agent — PR number persistence (`lib/patch_agent.ml`)

Add `pr_number : Pr_number.t option` field. Eliminate in-memory-only `Pr_registry` hashtable from `bin/main.ml`. PR numbers survive restarts via the existing snapshot persistence.

### Persistence — Migration logic (`lib/persistence.ml`)

Replace hard failures on missing JSON fields with `Option.value ~default` patterns. Specifically: `pr_number` defaults to `None`, `session_fallback` defaults to `Fresh_available`, `ci_checks` defaults to `[]`, `addressed_comment_ids` defaults to `Set.empty`. This matches Elixir's `migrate_entries/1`.

### Types (`lib/types.ml`, `lib/types.mli`)

Add `Comment_id` private type wrapper:
```ocaml
module Comment_id : sig
  type t [@@deriving sexp, compare, hash, equal]
  val of_string : string -> t
  val to_string : t -> string
end
```

Add `id : Comment_id.t` field to `Comment.t`.

Add `Stream_event` type for Claude NDJSON events:
```ocaml
module Stream_event : sig
  type t =
    | Assistant of { text : string }
    | Tool_use of { tool : string; input_preview : string }
    | Tool_result of { tool : string; output_preview : string }
    | Result of { text : string }
  [@@deriving sexp]
end
```

### Worktree operations (`lib/worktree.ml`)

Add `detect_branch`, `list_with_branches`, `add_existing` — needed by startup reconciler for worktree recovery and by the `+path` TUI command.

### Startup reconciler (`lib/startup_reconciler.ml`)

Add worktree recovery (call `Worktree.add_existing` for PRs missing their worktree) and stale-agent reset (`busy=true` in snapshot → `busy=false` so orchestrator can re-fire).

### Claude Runner (`lib/claude_runner.ml`, `lib/claude_runner.mli`)

Change `--output-format text` to `--output-format stream-json`. Parse NDJSON lines. Call an event callback per line. Extract session ID and PR URLs from tool results.

```ocaml
val run :
  process_mgr:_ Eio.Process.mgr ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  patch_id:Patch_id.t ->
  session_id:Session_id.t option ->
  prompt:string ->
  on_event:(Stream_event.t -> unit) ->
  run_result
```

### Patch Agent — extended fields (`lib/patch_agent.ml`)

Add `ci_checks`, `addressed_comment_ids`, replace `tried_fresh`/`session_failed` bool flags with `session_fallback` enum.

### Poller (`lib/poller.ml`)

Filter comments by `addressed_comment_ids` before queuing. Include `Comment_id.t` in `Comment.t` from GitHub response.

### Prompt — session fallback context and overrides (`lib/prompt.ml`)

Add `render_session_fallback_prompt` with PR context (number, branch, URL). Add prompt override system loading templates from `~/.local/share/onton/prompts/<project>/`.

### TUI (`lib/tui.ml`, `lib/tui_input.ml`)

Add `Timeline_view` mode showing interleaved events across all patches. Add input history ring buffer. Add stream event display in detail view.

### Main (`bin/main.ml`)

Wire `on_event` callback from Claude runner to activity log. Extract PR numbers from `Tool_result` events. Implement session fallback chain in runner fiber. Add SIGTSTP/SIGCONT signal handlers. Wire `-N` removal and `+path` worktree commands.

## Acceptance Criteria

- [ ] PR numbers are persisted in Patch_agent and survive restarts (no in-memory-only registry)
- [ ] Persistence migration: loading a snapshot with missing new fields succeeds with defaults
- [ ] Claude stream-JSON events are parsed and displayed live in TUI detail view
- [ ] PR numbers are auto-detected from Claude tool output (no manual `+N` needed)
- [ ] Review comments are deduplicated by GitHub database ID
- [ ] Session resume failure automatically tries fresh session (with PR context) before needs_intervention
- [ ] CI check details (name, URL, conclusion) are stored per patch and included in prompts
- [ ] Startup reconciler recovers worktrees for PRs that exist but whose worktree is missing
- [ ] Startup reconciler resets `busy=true` agents from crashed sessions
- [ ] `-N` command removes a patch and its worktree
- [ ] `+path` command adds an existing worktree directory (using `detect_branch` + `add_existing`)
- [ ] Cross-patch timeline view shows interleaved events from all patches
- [ ] TUI input history navigable with ↑/↓
- [ ] Ctrl+Z suspends cleanly and resumes without terminal corruption
- [ ] Project-specific prompt overrides can be loaded from disk
- [ ] All new logic has QCheck2 property tests
- [ ] `dune build` and `dune runtest` pass

## Open Questions

1. **Stream event buffer size**: How many events to retain per patch in memory? (Suggest: 200, matching Elixir's activity log trim.)
2. **PR detection regex**: Should we parse GitHub URLs specifically, or also detect `Created pull request #N` text patterns?

## Explicit Opinions

1. **PR persistence is the most critical gap.** Without it, restarting onton loses all PR discovery — the poller can't poll, agents can't respond. This blocks the entire resume flow.
2. **Stream-JSON is the highest-impact new feature.** Without it, onton is flying blind — no live feedback, no PR auto-detection. Everything else is secondary.
3. **Comment IDs over content matching.** Content-based dedup is fragile (edits break it). GitHub `databaseId` is stable.
4. **Session fallback should be a 3-state enum, not two bools.** `Fresh_available | Tried_fresh | Given_up` is clearer and prevents invalid states like `tried_fresh=true, session_failed=false` being misinterpreted.
5. **CI check details belong in Patch_agent, not just in prompts.** Storing them enables richer TUI display and better test assertions.
6. **Persistence migration is non-optional.** Any new field added to `Patch_agent.t` will break loading old snapshots without migration defaults. This must land alongside or before the fields it supports.
7. **Startup reconciler completeness is required for reliable resume.** A crashed onton session leaves agents in `busy=true` state and possibly missing worktrees. Without recovery logic, resumed sessions deadlock.
8. **Timeline view is a new view mode, not a replacement.** List → Detail → Timeline, all accessible via keyboard.
9. **Prompt overrides are low priority but high value for power users.** Template files on disk are simpler than a full EEx system — just load-and-substitute.

## Patches

### Patch 1 [INFRA]: Persist PR numbers in Patch_agent and add persistence migration

**Files to modify:**
- `lib/patch_agent.ml`
- `lib/patch_agent.mli`
- `lib/persistence.ml`

**Changes:**
1. Add `pr_number : Pr_number.t option` field to `Patch_agent.t`
2. Add `set_pr_number : t -> Pr_number.t -> t` operation
3. Add `pr_number` to `patch_agent_to_yojson` / `patch_agent_of_yojson`
4. Add migration logic to `patch_agent_of_yojson`: use `Option.value ~default` for any missing fields instead of hard failure — specifically `pr_number` (default `None`), `tried_fresh` (already has default), and future fields
5. Remove `Pr_registry` hashtable from `bin/main.ml` — read PR numbers from `Patch_agent.t` instead
6. Update poller fiber to read `agent.pr_number` instead of `Pr_registry.find`
7. Update PR discovery and auto-detection to call `Orchestrator.set_pr_number` instead of `Pr_registry.register`

### Patch 2 [INFRA]: Add Comment_id, Stream_event types

**Files to modify:**
- `lib/types.ml`
- `lib/types.mli`

**Changes:**
1. Add `Comment_id` module with private `t`, `of_string`, `to_string`, sexp/compare/hash/equal derivations
2. Add `id : Comment_id.t` field to `Comment.t`
3. Add `Stream_event.t` variant type: `Assistant`, `Tool_use`, `Tool_result`, `Result`
4. Add `Pr_url` module (private string wrapper) for extracted PR URLs
5. Update all existing `Comment.t` construction sites to include `id`

### Patch 3 [INFRA]: Extend Patch_agent with CI checks, addressed comments, session fallback enum

**Files to modify:**
- `lib/patch_agent.ml`
- `lib/patch_agent.mli`
- `lib/persistence.ml`

**Changes:**
1. Replace `tried_fresh : bool` and `session_failed : bool` with `session_fallback : session_fallback` where type is `Fresh_available | Tried_fresh | Given_up`
2. Add `ci_checks : Ci_check.t list` field (stores latest check details)
3. Add `addressed_comment_ids : Set.M(Comment_id).t` field
4. Add `set_ci_checks : t -> Ci_check.t list -> t`
5. Add `add_addressed_comment_id : t -> Comment_id.t -> t`
6. Add `is_comment_addressed : t -> Comment_id.t -> bool`
7. Add `advance_session_fallback : t -> t` (Fresh_available → Tried_fresh → Given_up)
8. Update `complete` to use `session_fallback = Given_up` for needs_intervention check
9. Update persistence serialization for new fields (with defaults for migration)

### Patch 4 [INFRA]: QCheck2 properties for Patch_agent new fields and PR persistence

**Files to modify:**
- `test/test_patch_agent.ml`
- `test/test_persistence_properties.ml`
- `lib_test/test_generators.ml`

**Changes:**
1. Add generator for `Comment_id.t`, `Ci_check.t` lists, `session_fallback`, `Pr_number.t option`
2. Property: `add_addressed_comment_id` then `is_comment_addressed` returns true
3. Property: `advance_session_fallback` transitions Fresh_available → Tried_fresh → Given_up monotonically
4. Property: `set_ci_checks` preserves all other fields
5. Property: `complete` with `Given_up` sets `needs_intervention`; `Fresh_available`/`Tried_fresh` do not
6. Property: persistence roundtrip preserves `pr_number`, `ci_checks`, `addressed_comment_ids`, `session_fallback`
7. Property: loading a snapshot missing new fields succeeds with defaults (migration test)

### Patch 5 [BEHAVIOR]: Worktree operations — add_existing, detect_branch, list_with_branches

**Files to modify:**
- `lib/worktree.ml`
- `lib/worktree.mli`

**Changes:**
1. Add `detect_branch : process_mgr:_ Eio.Process.mgr -> worktree_path:string -> (Branch.t, string) result` — runs `git -C <path> branch --show-current`
2. Add `list_with_branches : process_mgr:_ Eio.Process.mgr -> repo_root:string -> (Branch.t * string) list` — parses `git worktree list --porcelain` into (branch, path) pairs
3. Add `add_existing : process_mgr:_ Eio.Process.mgr -> repo_root:string -> branch:Branch.t -> worktree_path:string -> (unit, string) result` — attaches to an existing remote branch, fetches if needed

### Patch 6 [BEHAVIOR]: Startup reconciler — worktree recovery and pending reset

**Files to modify:**
- `lib/startup_reconciler.ml`
- `lib/startup_reconciler.mli`

**Changes:**
1. After discovering existing PRs, also call `Worktree.list_with_branches` to find existing worktrees
2. For patches with a PR but no worktree on disk: recover by calling `Worktree.add_existing` with the PR's branch
3. For patches that were `busy=true` in the persisted snapshot (crashed mid-run): reset to `busy=false` so the orchestrator can re-fire them
4. Add `stale_resets : Patch_id.t list` to the reconciler result for logging

### Patch 7 [BEHAVIOR]: Stream-JSON parser for Claude NDJSON output

**Files to modify:**
- `lib/claude_runner.ml`
- `lib/claude_runner.mli`

**Changes:**
1. Change `--output-format text` to `--output-format stream-json`
2. Add `Stream_parser` submodule: `parse_line : string -> Stream_event.t option` (parses JSON, extracts type/text/tool fields)
3. Add `extract_pr_url : string -> Pr_url.t option` (regex for `github.com/.*/pull/[0-9]+`)
4. Change `run` signature to accept `on_event : Stream_event.t -> unit` callback
5. Buffer stdout line-by-line, parse each as JSON, dispatch to `on_event`
6. Extract and return detected PR URL from tool results
7. Extract session ID from stream output (if available)

### Patch 8 [INFRA]: QCheck2 properties for stream parser

**Files to modify:**
- `test/test_stream_parser.ml` (new)

**Changes:**
1. Property: `parse_line` on valid JSON with `type: "assistant"` returns `Assistant`
2. Property: `parse_line` on malformed input returns `None`
3. Property: `extract_pr_url` on string containing GitHub PR URL extracts it
4. Property: `extract_pr_url` on string without PR URL returns None
5. Expect tests with real Claude NDJSON samples

### Patch 9 [BEHAVIOR]: Comment dedup by GitHub ID in poller

**Files to modify:**
- `lib/poller.ml`
- `lib/poller.mli`
- `lib/github.ml`

**Changes:**
1. Update `Github.Pr_state.t` to include `Comment_id.t` in comment records
2. Parse `databaseId` from GitHub GraphQL comment response into `Comment_id.t`
3. Update `Poller.poll` to accept `addressed_ids : Set.M(Comment_id).t`
4. Filter out comments whose ID is in `addressed_ids` before queuing `Review_comments`
5. After responding to comments, add their IDs to `addressed_comment_ids` via patch agent

### Patch 10 [INFRA]: QCheck2 properties for comment dedup

**Files to modify:**
- `test/test_properties.ml`

**Changes:**
1. Property: comments with IDs in addressed set are not queued
2. Property: comments with IDs NOT in addressed set ARE queued
3. Property: addressed set grows monotonically (IDs are never removed)

### Patch 11 [BEHAVIOR]: Session fallback chain with PR context in prompts

**Files to modify:**
- `bin/main.ml`
- `lib/prompt.ml`

**Changes:**
1. In runner fiber, after Claude exits with failure:
   - If `session_fallback = Fresh_available`: call `advance_session_fallback`, retry with fresh session (no `--resume`)
   - If `session_fallback = Tried_fresh`: call `advance_session_fallback` (→ Given_up), set needs_intervention
   - If `session_fallback = Given_up`: already in needs_intervention, skip
2. On successful completion: call `clear_session_fallback` (reset to `Fresh_available`)
3. Log fallback transitions to activity log
4. When rendering a fresh-session fallback prompt, include PR context: PR number, branch name, PR URL (matching Elixir's `session.ex` lines 40-50)
5. Add `render_session_fallback_prompt : pr_number:Pr_number.t option -> branch:Branch.t -> string` to `prompt.ml`

### Patch 12 [BEHAVIOR]: CI check detail storage and prompt enrichment

**Files to modify:**
- `bin/main.ml`
- `lib/prompt.ml`

**Changes:**
1. In poller integration (main.ml), when CI failure detected, store full `Ci_check.t` list via `Patch_agent.set_ci_checks`
2. Update `Prompt.render_ci_failure_prompt` to accept `Ci_check.t list` and include check names, URLs, conclusions
3. In runner fiber, pass stored CI checks to prompt renderer instead of just count

### Patch 13 [BEHAVIOR]: Wire stream events to activity log and PR auto-detection

**Files to modify:**
- `bin/main.ml`
- `lib/activity_log.ml`
- `lib/activity_log.mli`

**Changes:**
1. Add `Stream_event` variant to `Activity_log.event` type
2. In runner fiber, pass `on_event` callback that:
   - Appends stream events to activity log for the patch
   - Checks each `Tool_result` for PR URLs via `extract_pr_url`
   - Auto-registers detected PR number via `Orchestrator.set_pr_number` (persisted, not hashtable)
3. Log "Auto-detected PR #N" to activity log

### Patch 14 [BEHAVIOR]: Cross-patch timeline view in TUI

**Files to modify:**
- `lib/tui.ml`
- `lib/tui.mli`
- `lib/tui_input.ml`

**Changes:**
1. Add `Timeline_view` to `view_mode` type
2. Add `t` keybinding to toggle timeline view from list view
3. Render timeline: merge all patches' activity log entries, sort by timestamp descending, display with patch ID prefix and color coding
4. Timeline shows last 50 entries across all patches
5. Navigation: `j`/`k` scroll, `Esc` returns to list view

### Patch 15 [BEHAVIOR]: TUI input history

**Files to modify:**
- `lib/tui_input.ml`
- `lib/tui_input.mli`

**Changes:**
1. Add `history : string list` and `history_index : int option` to `Tui_input.t`
2. On successful text submit, prepend to history (max 50 entries)
3. `Up` arrow in text mode: recall previous entry, set history_index
4. `Down` arrow in text mode: navigate forward in history
5. Entering text clears history_index

### Patch 16 [BEHAVIOR]: SIGTSTP/SIGCONT signal handling

**Files to modify:**
- `bin/main.ml`
- `lib/term.ml`

**Changes:**
1. Add `Term.save_state` / `Term.restore_state` for terminal mode save/restore
2. Install `Sys.signal Sys.sigtstp` handler that: saves terminal state, resets to cooked mode, re-raises SIGTSTP
3. Install `Sys.signal Sys.sigcont` handler that: restores raw mode, restores terminal state, triggers TUI redraw
4. Ensure cursor visibility is restored on suspend

### Patch 17 [BEHAVIOR]: Ad-hoc worktree addition and patch removal

**Files to modify:**
- `lib/tui_input.ml`
- `bin/main.ml`

**Changes:**
1. Parse `+/path/to/worktree` in `Tui_input.parse_line` → `Add_worktree of string` command
2. Parse `-N` in `Tui_input.parse_line` → `Remove_patch of Patch_id.t` command
3. `Add_worktree` handler: use `Worktree.detect_branch` to find branch, check for open PR on branch, register PR, start polling
4. `Remove_patch` handler: stop agent if running, remove from orchestrator, optionally call `Worktree.remove`, log removal

### Patch 18 [INFRA]: TUI detail view enrichment with CI checks and stream events

**Files to modify:**
- `lib/tui.ml`

**Changes:**
1. In detail view, show stored CI check details: name, conclusion, URL (if available)
2. Show recent stream events (last 20) with tool names and text previews
3. Show addressed vs pending comment counts

### Patch 19 [BEHAVIOR]: Prompt override system

**Files to modify:**
- `lib/prompt.ml`
- `lib/prompt.mli`

**Changes:**
1. Add `load_overrides : project_name:string -> overrides` that reads prompt templates from `~/.local/share/onton/prompts/<project-slug>/` if they exist
2. Override files: `patch_prompt.md`, `review_prompt.md`, `ci_failure_prompt.md`, `merge_conflict_prompt.md`
3. Each render function accepts optional `overrides` parameter; if present and file exists, use file contents as template instead of hardcoded string
4. Template variables substituted with `Printf`-style: `{{patch_title}}`, `{{base_branch}}`, `{{project_name}}`, etc.

## Test Map

| Test Name | File | Stub Patch | Impl Patch |
|-----------|------|------------|------------|
| pr_number persisted in snapshot | test/test_persistence_properties.ml | 4 | 4 |
| migration loads snapshot with missing fields | test/test_persistence_properties.ml | 4 | 4 |
| addressed comment add then check | test/test_patch_agent.ml | 4 | 4 |
| session fallback monotonic advance | test/test_patch_agent.ml | 4 | 4 |
| set_ci_checks preserves fields | test/test_patch_agent.ml | 4 | 4 |
| complete with Given_up sets intervention | test/test_patch_agent.ml | 4 | 4 |
| persistence roundtrip new fields | test/test_patch_agent.ml | 4 | 4 |
| parse_line valid assistant JSON | test/test_stream_parser.ml | 8 | 8 |
| parse_line malformed returns None | test/test_stream_parser.ml | 8 | 8 |
| extract_pr_url finds GitHub URL | test/test_stream_parser.ml | 8 | 8 |
| extract_pr_url no URL returns None | test/test_stream_parser.ml | 8 | 8 |
| addressed comments filtered from queue | test/test_properties.ml | 10 | 10 |
| non-addressed comments queued | test/test_properties.ml | 10 | 10 |

## Dependency Graph

- Patch 1 [INFRA] -> []
- Patch 2 [INFRA] -> []
- Patch 3 [INFRA] -> [1, 2]
- Patch 4 [INFRA] -> [3]
- Patch 5 [BEHAVIOR] -> []
- Patch 6 [BEHAVIOR] -> [1, 5]
- Patch 7 [BEHAVIOR] -> [2]
- Patch 8 [INFRA] -> [7]
- Patch 9 [BEHAVIOR] -> [2, 3]
- Patch 10 [INFRA] -> [9]
- Patch 11 [BEHAVIOR] -> [3]
- Patch 12 [BEHAVIOR] -> [3]
- Patch 13 [BEHAVIOR] -> [1, 7]
- Patch 14 [BEHAVIOR] -> []
- Patch 15 [BEHAVIOR] -> []
- Patch 16 [BEHAVIOR] -> []
- Patch 17 [BEHAVIOR] -> [5]
- Patch 18 [INFRA] -> [3, 13]
- Patch 19 [BEHAVIOR] -> []

**Mergability insight**: 6 of 19 patches are `[INFRA]` and can ship without changing observable behavior. Patches 5, 7, 11, 12, 14, 15, 16, 19 can begin immediately or after minimal dependencies, giving high fan-out.

## Mergability Checklist

- [x] Feature flag strategy documented (not needed — internal tooling)
- [x] Early patches contain only non-functional changes (`[INFRA]`)
- [x] Test stubs are co-located with implementations (OCaml inline tests + QCheck in test/)
- [x] Test implementations are co-located with the code they test (same patch)
- [x] Test Map is complete
- [x] `[BEHAVIOR]` patches are as small as possible
- [x] Dependency graph shows `[INFRA]` patches early, `[BEHAVIOR]` patches late
- [x] Each `[BEHAVIOR]` patch is clearly justified (cannot be deferred without leaving a gap)
