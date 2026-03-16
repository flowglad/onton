# Gameplan: onton-completeness

## Problem Statement

Onton is a functional OCaml port of the Anton Elixir orchestrator, but several features are missing or incomplete relative to the Elixir implementation. The most impactful gaps are: no live Claude event streaming (the TUI is blind while agents run), no automatic PR detection from Claude output, incomplete session fallback logic, no comment deduplication by GitHub ID, and missing TUI features (timeline view, input history, signal handling). These gaps make onton unsuitable as a drop-in replacement for Anton.

## Solution Summary

Close all feature parity gaps in priority order: (1) Claude stream-JSON parsing to enable live activity display and PR auto-detection, (2) GitHub comment ID tracking for dedup, (3) session resume fallback chain, (4) CI check detail storage, (5) ad-hoc PR/worktree/removal flows, (6) cross-patch timeline view, (7) TUI polish (input history, SIGTSTP, detail view enrichment). Infrastructure patches (types, test stubs) land first; behavior patches follow with tests co-located.

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
- Persistence with JSON snapshots and crash recovery
- `tried_fresh`/`session_failed` flags in `Patch_agent.t` (but no automatic retry logic)
- `Ci_check.t` has `name`, `conclusion`, `details_url`, `description` but patch agent only stores `ci_failure_count`
- Comments tracked by `Comment.t { body; path; line }` — no GitHub database ID

### What's missing
- Stream-JSON NDJSON parsing from Claude CLI (currently `--output-format text`)
- PR number extraction from Claude tool output
- Addressed comment tracking by GitHub ID
- Automatic session fallback: resume → fresh → needs_intervention
- CI check detail storage per patch (only count stored)
- Ad-hoc worktree addition, patch removal commands
- Cross-patch timeline view
- TUI input history, SIGTSTP/SIGCONT handling
- `.mli` files for modules that lack them

## Required Changes

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

### Claude Runner (`lib/claude_runner.ml`, `lib/claude_runner.mli`)

Change `--output-format text` to `--output-format stream-json`. Parse NDJSON lines. Call an event callback per line. Extract session ID and PR URLs from tool results.

```ocaml
val run :
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  session_id:Session_id.t option ->
  prompt:string ->
  on_event:(Stream_event.t -> unit) ->
  run_result
```

### Patch Agent (`lib/patch_agent.ml`, `lib/patch_agent.mli`)

Add `ci_checks : Ci_check.t list` and `addressed_comment_ids : Set.M(Comment_id).t` fields. Add `session_fallback` variant type replacing bool flags:

```ocaml
type session_fallback = Fresh_available | Tried_fresh | Given_up
```

Add `add_addressed_comment_id`, `is_comment_addressed`, `set_ci_checks`, `advance_session_fallback` operations.

### State (`lib/state.ml`)

Update comment tracking to use `Comment_id.t` for dedup. Add `is_addressed` check using the patch agent's addressed set.

### Poller (`lib/poller.ml`)

Filter comments by `addressed_comment_ids` before queuing. Include `Comment_id.t` in `Comment.t` from GitHub response.

### TUI (`lib/tui.ml`, `lib/tui_input.ml`)

Add `Timeline_view` mode showing interleaved events across all patches. Add input history ring buffer. Add stream event display in detail view.

### Main (`bin/main.ml`)

Wire `on_event` callback from Claude runner to activity log. Extract PR numbers from `Tool_result` events. Implement session fallback chain in runner fiber. Add SIGTSTP/SIGCONT signal handlers. Wire `-N` removal command.

## Acceptance Criteria

- [ ] Claude stream-JSON events are parsed and displayed live in TUI detail view
- [ ] PR numbers are auto-detected from Claude tool output (no manual `+N` needed)
- [ ] Review comments are deduplicated by GitHub database ID
- [ ] Session resume failure automatically tries fresh session before needs_intervention
- [ ] CI check details (name, URL, conclusion) are stored per patch and included in prompts
- [ ] `-N` command removes a patch and its worktree
- [ ] `+path` command adds an existing worktree directory
- [ ] Cross-patch timeline view shows interleaved events from all patches
- [ ] TUI input history navigable with ↑/↓
- [ ] Ctrl+Z suspends cleanly and resumes without terminal corruption
- [ ] All new logic has QCheck2 property tests
- [ ] `dune build` and `dune runtest` pass

## Open Questions

1. **Stream event buffer size**: How many events to retain per patch in memory? (Suggest: 200, matching Elixir's activity log trim.)
2. **PR detection regex**: Should we parse GitHub URLs specifically, or also detect `Created pull request #N` text patterns?

## Explicit Opinions

1. **Stream-JSON is the highest priority gap.** Without it, onton is flying blind — no live feedback, no PR detection. Everything else is secondary.
2. **Comment IDs over content matching.** Content-based dedup is fragile (edits break it). GitHub `databaseId` is stable.
3. **Session fallback should be a 3-state enum, not two bools.** `Fresh_available | Tried_fresh | Given_up` is clearer and prevents invalid states like `tried_fresh=true, session_failed=false` being misinterpreted.
4. **CI check details belong in Patch_agent, not just in prompts.** Storing them enables richer TUI display and better test assertions.
5. **Timeline view is a new view mode, not a replacement.** List → Detail → Timeline, all accessible via keyboard.

## Patches

### Patch 1 [INFRA]: Add Comment_id, Stream_event types and extend Ci_check storage

**Files to modify:**
- `lib/types.ml`
- `lib/types.mli`

**Changes:**
1. Add `Comment_id` module with private `t`, `of_string`, `to_string`, sexp/compare/hash/equal derivations
2. Add `id : Comment_id.t` field to `Comment.t`
3. Add `Stream_event.t` variant type: `Assistant`, `Tool_use`, `Tool_result`, `Result`
4. Add `Pr_url` module (private string wrapper) for extracted PR URLs
5. Update all existing `Comment.t` construction sites to include `id`

### Patch 2 [INFRA]: Extend Patch_agent with CI checks, addressed comments, session fallback enum

**Files to modify:**
- `lib/patch_agent.ml`
- `lib/patch_agent.mli`

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

### Patch 3 [INFRA]: QCheck2 properties for new Patch_agent fields

**Files to modify:**
- `test/test_patch_agent.ml`
- `lib_test/test_generators.ml`

**Changes:**
1. Add generator for `Comment_id.t`, `Ci_check.t` lists, `session_fallback`
2. Property: `add_addressed_comment_id` then `is_comment_addressed` returns true
3. Property: `advance_session_fallback` transitions Fresh_available → Tried_fresh → Given_up monotonically
4. Property: `set_ci_checks` preserves all other fields
5. Property: `complete` with `Given_up` sets `needs_intervention`; `Fresh_available`/`Tried_fresh` do not
6. Property: persistence roundtrip preserves new fields

### Patch 4 [BEHAVIOR]: Stream-JSON parser for Claude NDJSON output

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

### Patch 5 [INFRA]: QCheck2 properties for stream parser

**Files to modify:**
- `test/test_stream_parser.ml` (new)

**Changes:**
1. Property: `parse_line` on valid JSON with `type: "assistant"` returns `Assistant`
2. Property: `parse_line` on malformed input returns `None`
3. Property: `extract_pr_url` on string containing GitHub PR URL extracts it
4. Property: `extract_pr_url` on string without PR URL returns None
5. Expect tests with real Claude NDJSON samples

### Patch 6 [BEHAVIOR]: Comment dedup by GitHub ID in poller

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

### Patch 7 [INFRA]: QCheck2 properties for comment dedup

**Files to modify:**
- `test/test_properties.ml`

**Changes:**
1. Property: comments with IDs in addressed set are not queued
2. Property: comments with IDs NOT in addressed set ARE queued
3. Property: addressed set grows monotonically (IDs are never removed)

### Patch 8 [BEHAVIOR]: Session fallback chain in runner fiber

**Files to modify:**
- `bin/main.ml`

**Changes:**
1. In runner fiber, after Claude exits with failure:
   - If `session_fallback = Fresh_available`: call `advance_session_fallback`, retry with fresh session (no `--resume`)
   - If `session_fallback = Tried_fresh`: call `advance_session_fallback` (→ Given_up), set needs_intervention
   - If `session_fallback = Given_up`: already in needs_intervention, skip
2. On successful completion: call `clear_session_fallback` (reset to `Fresh_available`)
3. Log fallback transitions to activity log

### Patch 9 [BEHAVIOR]: CI check detail storage and prompt enrichment

**Files to modify:**
- `bin/main.ml`
- `lib/prompt.ml`

**Changes:**
1. In poller integration (main.ml), when CI failure detected, store full `Ci_check.t` list via `Patch_agent.set_ci_checks`
2. Update `Prompt.render_ci_failure_prompt` to accept `Ci_check.t list` and include check names, URLs, conclusions
3. In runner fiber, pass stored CI checks to prompt renderer instead of just count

### Patch 10 [BEHAVIOR]: Wire stream events to activity log and PR auto-detection

**Files to modify:**
- `bin/main.ml`
- `lib/activity_log.ml`
- `lib/activity_log.mli`

**Changes:**
1. Add `Stream_event` variant to `Activity_log.event` type
2. In runner fiber, pass `on_event` callback that:
   - Appends stream events to activity log for the patch
   - Checks each `Tool_result` for PR URLs via `extract_pr_url`
   - Auto-registers detected PR number in `Pr_registry`
3. Log "Auto-detected PR #N" to activity log

### Patch 11 [BEHAVIOR]: Cross-patch timeline view in TUI

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

### Patch 12 [BEHAVIOR]: TUI input history

**Files to modify:**
- `lib/tui_input.ml`
- `lib/tui_input.mli`

**Changes:**
1. Add `history : string list` and `history_index : int option` to `Tui_input.t`
2. On successful text submit, prepend to history (max 50 entries)
3. `Up` arrow in text mode: recall previous entry, set history_index
4. `Down` arrow in text mode: navigate forward in history
5. Entering text clears history_index

### Patch 13 [BEHAVIOR]: SIGTSTP/SIGCONT signal handling

**Files to modify:**
- `bin/main.ml`
- `lib/term.ml`

**Changes:**
1. Add `Term.save_state` / `Term.restore_state` for terminal mode save/restore
2. Install `Sys.signal Sys.sigtstp` handler that: saves terminal state, resets to cooked mode, re-raises SIGTSTP
3. Install `Sys.signal Sys.sigcont` handler that: restores raw mode, restores terminal state, triggers TUI redraw
4. Ensure cursor visibility is restored on suspend

### Patch 14 [BEHAVIOR]: Ad-hoc worktree addition and patch removal

**Files to modify:**
- `lib/tui_input.ml`
- `bin/main.ml`

**Changes:**
1. Parse `+/path/to/worktree` in `Tui_input.parse_line` → `Add_worktree of string` command
2. Parse `-N` in `Tui_input.parse_line` → `Remove_patch of Patch_id.t` command
3. `Add_worktree` handler: detect branch from path, check for open PR on branch, register PR, start polling
4. `Remove_patch` handler: stop agent if running, remove from orchestrator, optionally remove worktree, log removal

### Patch 15 [INFRA]: TUI detail view enrichment with CI checks and stream events

**Files to modify:**
- `lib/tui.ml`

**Changes:**
1. In detail view, show stored CI check details: name, conclusion, URL (if available)
2. Show recent stream events (last 20) with tool names and text previews
3. Show addressed vs pending comment counts

## Test Map

| Test Name | File | Stub Patch | Impl Patch |
|-----------|------|------------|------------|
| addressed comment add then check | test/test_patch_agent.ml | 3 | 3 |
| session fallback monotonic advance | test/test_patch_agent.ml | 3 | 3 |
| set_ci_checks preserves fields | test/test_patch_agent.ml | 3 | 3 |
| complete with Given_up sets intervention | test/test_patch_agent.ml | 3 | 3 |
| persistence roundtrip new fields | test/test_patch_agent.ml | 3 | 3 |
| parse_line valid assistant JSON | test/test_stream_parser.ml | 5 | 5 |
| parse_line malformed returns None | test/test_stream_parser.ml | 5 | 5 |
| extract_pr_url finds GitHub URL | test/test_stream_parser.ml | 5 | 5 |
| extract_pr_url no URL returns None | test/test_stream_parser.ml | 5 | 5 |
| addressed comments filtered from queue | test/test_properties.ml | 7 | 7 |
| non-addressed comments queued | test/test_properties.ml | 7 | 7 |

## Dependency Graph

- Patch 1 [INFRA] -> []
- Patch 2 [INFRA] -> [1]
- Patch 3 [INFRA] -> [2]
- Patch 4 [BEHAVIOR] -> [1]
- Patch 5 [INFRA] -> [4]
- Patch 6 [BEHAVIOR] -> [1, 2]
- Patch 7 [INFRA] -> [6]
- Patch 8 [BEHAVIOR] -> [2]
- Patch 9 [BEHAVIOR] -> [2]
- Patch 10 [BEHAVIOR] -> [4, 2]
- Patch 11 [BEHAVIOR] -> []
- Patch 12 [BEHAVIOR] -> []
- Patch 13 [BEHAVIOR] -> []
- Patch 14 [BEHAVIOR] -> [1]
- Patch 15 [INFRA] -> [2, 10]

**Mergability insight**: 5 of 15 patches are `[INFRA]` and can ship without changing observable behavior. Patches 4, 6, 8, 9, 11, 12, 13, 14 can all proceed in parallel after their dependencies are met, giving high fan-out.

## Mergability Checklist

- [x] Feature flag strategy documented (not needed — internal tooling)
- [x] Early patches contain only non-functional changes (`[INFRA]`)
- [x] Test stubs are co-located with implementations (OCaml inline tests + QCheck in test/)
- [x] Test implementations are co-located with the code they test (same patch)
- [x] Test Map is complete
- [x] `[BEHAVIOR]` patches are as small as possible
- [x] Dependency graph shows `[INFRA]` patches early, `[BEHAVIOR]` patches late
- [x] Each `[BEHAVIOR]` patch is clearly justified (cannot be deferred without leaving a gap)
