# onton

![onton](onton.png)

A methodology and orchestrator for breaking complex codebase changes into
parallel, spec-verified patches executed by AI agents.

## The gameplan approach

Large changes are hard to review, easy to get wrong, and risky to merge. The
gameplan approach decomposes them into small, ordered patches — each with a
formal specification (written in
[Pantagruel](https://github.com/subsetpark/pantagruel)) that states what must
be true after the patch is applied. A dependency graph identifies which patches
can run in parallel. An orchestrator spawns AI agents in isolated git worktrees
and manages the full lifecycle: PR creation, code review, CI, rebasing, and
merge.

The approach has three layers:

| Layer | What | Where |
|-------|------|-------|
| **Scoping** | Break a large project into sequenced gameplans (milestones) | `skills/write-workstream/` |
| **Planning** | Structure a change into patches with specs, tests, and a dependency graph | `skills/write-gameplan/` |
| **Execution** | Orchestrate parallel agents, poll GitHub, react to events | `onton` (this binary) |

### Skills

The `skills/` directory contains Claude Code skills for the planning layers:

- **write-workstream** — Define a large project as a sequence of gameplans
  (milestones), each a safe stopping point. Guided discovery process: vision,
  challenges, milestones, dependencies.

- **write-gameplan** — Create a structured JSON gameplan with typed
  sections, patch classifications (INFRA/GATED/BEHAVIOR), formal specs,
  test maps, and a dependency graph. Designed so it's 5-10x easier to review
  the gameplan than the code it produces.

## Onton: the orchestrator

Onton parses a structured gameplan, builds a dependency graph, and spawns
concurrent Claude Code agents in git worktrees — one per patch. It polls GitHub
for PR status, detects merges, triggers rebases, and reacts to CI failures and
review comments. A terminal UI shows live status with full markdown-rendered
agent transcripts.

## Install

Pick **one** of the following methods:

### Option A: Homebrew (macOS)

```sh
brew tap flowglad/onton https://github.com/flowglad/onton
brew install onton
```

### Option B: GitHub Releases

Download a prebuilt binary from
[Releases](https://github.com/flowglad/onton/releases) (macOS ARM64 and
x86_64).

### Option C: From source

Only needed if you want to modify onton or are on a platform without prebuilt
binaries. Requires OCaml 5.4.0, dune 3.21, and opam:

```sh
git clone https://github.com/flowglad/onton.git
cd onton
opam switch create . ocaml.5.4.0 --deps-only
eval $(opam env)
opam install . --deps-only
dune build
```

## Requirements

- Claude Code CLI (`claude` on PATH)
- GitHub CLI (`gh` on PATH) for PR discovery

## Usage

```sh
onton --gameplan GAMEPLAN [OPTIONS]       # Start a new project from a gameplan
onton PROJECT [OPTIONS]                  # Resume a saved project
onton --repo ../my-repo [OPTIONS]        # Ad-hoc mode (no gameplan)
```

| Flag | Default | Description |
|------|---------|-------------|
| `PROJECT` | (derived from gameplan) | Project name (positional). Required to resume, optional with `--gameplan` |
| `--gameplan` | — | Path to the gameplan markdown file |
| `--repo` | `.` | Path to the git repository. GitHub owner/repo are inferred from `git remote` |
| `--token` | `$GITHUB_TOKEN` or `gh auth token` | GitHub API token |
| `--main-branch` | `main` | Main branch name |
| `--poll-interval` | `30.0` | GitHub polling interval in seconds |
| `--max-concurrency` | `5` / `$ONTON_MAX_CONCURRENCY` | Maximum concurrent Claude processes |
| `--headless` | off | Run without TUI (plain log output to stdout) |

Project config and state are persisted to `~/.local/share/onton/<project>/`.
Resuming a project reloads the saved snapshot (including agent transcripts) and
reconciles against GitHub.

### User configuration

Per-repo configuration lives at `~/.config/onton/<github-owner>/<github-repo>/`.

| File | Description |
|------|-------------|
| `on_worktree_create` | Executable hook run after a new git worktree is created |

The `on_worktree_create` hook receives these environment variables:

| Variable | Description |
|----------|-------------|
| `ONTON_WORKTREE_PATH` | Absolute path to the created worktree |
| `ONTON_PATCH_ID` | Patch identifier |
| `ONTON_BRANCH` | Branch name |

Example — install dependencies in every new worktree:

```sh
mkdir -p ~/.config/onton/myorg/myrepo
cat > ~/.config/onton/myorg/myrepo/on_worktree_create << 'EOF'
#!/bin/bash
cd "$ONTON_WORKTREE_PATH"
npm install
EOF
chmod +x ~/.config/onton/myorg/myrepo/on_worktree_create
```

Worktrees are discovered from `git worktree list`. If no existing worktree is
found for a patch's branch, one is created at
`~/worktrees/<project>/patch-<id>`.

### Ad-hoc mode

When launched without `PROJECT` or `--gameplan`, onton starts with an empty
patch list. Add PRs at runtime with `+N` in text mode (`:` then `+123`). Each
`+N` creates a new agent that polls and responds to the given PR. Branch, base,
and worktree are auto-discovered from GitHub and local git.

State is persisted across restarts — ad-hoc agents survive session restarts and
resume where they left off.

## Build & test

```sh
dune build          # compile with strict warnings (most warnings are fatal)
dune runtest        # inline tests + property tests (QCheck2)
dune build @check   # type-check only (no linking), faster for quick feedback
dune exec bin/main.exe -- --gameplan path/to/gameplan.md
dune fmt            # auto-format via ocamlformat
```

## Architecture

```
gameplan.md ──> Gameplan_parser ──> Graph + Patches
                                         │
                    Orchestrator ────────┤
                    ├── Patch_agent (per-patch state machine)
                    ├── Patch_decision (pure decision logic)
                    ├── Spawn_logic (action planning)
                    ├── Poller (GitHub PR status via GraphQL)
                    ├── Reconciler (merge detection, stale base detection)
                    └── TUI (terminal display + markdown transcript)

          ┌─────────────────────────────────────────────┐
          │              Eio_main.run                   │
          │  ┌────────┐ ┌───────┐ ┌───────┐ ┌────────┐  │
          │  │  TUI   │ │Poller │ │Runner │ │Persist │  │
          │  │ fiber  │ │ fiber │ │ fiber │ │ fiber  │  │
          │  └───┬────┘ └───┬───┘ └───┬───┘ └───┬────┘  │
          │      └──────────┼─────────┼─────────┘       │
          │            Runtime (Eio.Mutex)              │
          └─────────────────────────────────────────────┘
```

### Claude session management

Claude is invoked via `-p` (prompt mode, not `--print`) which saves sessions,
enabling `--continue` to resume the most recent session in a worktree. This
enables session resumption across restarts.

Since `-p` mode requires a TTY for streaming output, each Claude process is
wrapped in `/usr/bin/script -q /dev/null` to allocate a pseudo-TTY. ANSI
escapes from the PTY are stripped before JSON parsing.

The session fallback chain: `--continue` (resume worktree session) -> fresh
session (no `--continue`) -> give up (needs intervention). If `--continue`
produces no events, it's treated as a resume failure and falls back to fresh.

Additional flags: `--dangerously-skip-permissions`, `--max-turns 200`,
`--output-format stream-json`, `--verbose`.

### Runner concurrency

Action fibers are spawned independently (`fork_daemon`) rather than batched —
the runner loop picks up newly-queued operations on each 1-second cycle without
waiting for running sessions to finish. Backpressure is provided by a
`max_concurrency` semaphore.

### Modules

| Module | Purpose |
|--------|---------|
| `types` | Core types: `Patch_id`, `Branch`, `Operation_kind`, `Patch`, `Comment`, `Gameplan` |
| `priority` | Operation priority queue — single source of truth for ordering |
| `graph` | Dependency graph: unblocked detection, base branch resolution |
| `gameplan_parser` | Markdown gameplan to structured `Gameplan.t` |
| `patch_agent` | Per-patch state machine: start, respond, complete, rebase transitions (private type). Tracks `current_op` for active operation |
| `patch_decision` | Pure decision logic: disposition, CI cap, review comment filtering, merge conflict handling. Extracted from main.ml for testability |
| `spawn_logic` | Pure action planning: which patches need Start/Respond/Rebase |
| `claude_process` | Claude CLI session state machine (No_session -> Has_session) |
| `claude_runner` | Claude subprocess spawning with PTY wrapping, NDJSON streaming, ANSI stripping, `got_events` resume-failure detection |
| `orchestrator` | Top-level tick loop: fires all actions whose preconditions hold |
| `reconciler` | Pure merge detection, rebase cascading, stale base detection, liveness enforcement |
| `startup_reconciler` | PR discovery, worktree recovery, stale busy reset at startup |
| `poller` | GitHub polling: comments, CI, merge conflicts, merge/approval state |
| `state` | Spec context maps (PatchCtx, Comments) for invariant checking |
| `runtime` | Mutex-protected shared snapshot across fibers (orchestrator + activity log + gameplan + transcripts) |
| `activity_log` | Per-patch event, transition, and stream entry feed |
| `invariants` | Runtime spec invariant checker (gated via `ONTON_CHECK_INVARIANTS`) |
| `persistence` | JSON snapshot save/load with backward-compatible migration, transcript persistence |
| `project_store` | Project config and gameplan storage at `~/.local/share/onton/` |
| `user_config` | Per-repo user configuration and hook execution from `~/.config/onton/<owner>/<repo>/` |
| `prompt` | Agent prompt rendering with per-project template override support |
| `worktree` | Git worktree CRUD, branch detection, orchestrator-executed `git rebase` |
| `github` | GitHub GraphQL API client (HTTPS via Eio) |
| `term` | ANSI terminal primitives (raw mode, key input, size, SIGTSTP/SIGCONT) |
| `tui_input` | Keyboard -> command translation, text-mode parsing, history buffer |
| `tui` | Terminal UI: list/detail/timeline views, status derivation, frame rendering, gameplan-ordered display |
| `markdown_render` | CommonMark to ANSI terminal renderer via cmarkit |

### Design principles

- **Eio for structured concurrency** — four fibers (TUI, poller, runner,
  persistence), with independently-spawned Claude action fibers bounded by a
  semaphore
- **Pure logic core** — parser, graph, priority, state machine, decision logic,
  spawn planning are pure functions with no I/O
- **Strict compiler feedback** — all warnings fatal (except 44/70), `.mli`
  files enforce module boundaries
- **Pantagruel spec alignment** — state machine transitions match the formal
  spec
- **Single source of truth** — priority ordering defined once in `Priority`;
  sorted patch display via shared `sorted_patch_ids` ref; `current_op` tracks
  active operation for both status display and log suppression
- **Property-based testing** — QCheck2 tests for graph, patch agent,
  orchestrator liveness, reconciler, state machine, persistence roundtrip,
  stream parsing, TUI input, poller, patch decision, and spawn logic

## CI

GitHub Actions runs on every push and PR:

- **Build & Test** — `dune build` + `dune runtest` with compiler error annotations on PR diffs
- **Property tests** — QCheck2 with 10,000 iterations
- **Format check** — `ocamlformat` via `ocaml/setup-ocaml/lint-fmt`

Pushing a `v*` tag builds macOS binaries (ARM64 + x86_64), creates a GitHub
release, and updates the Homebrew formula.

## TUI

Three view modes:
- **List view** — patch table with status badge, PR number, title (branch
  name for ad-hoc), current operation, CI failures
- **Detail view** — single patch: status, branch, base, worktree path, PR,
  dependencies, conflict, pending comments, CI checks. Scrollable
  markdown-rendered transcript with timestamped prompt delivery and Claude
  responses. Info section pinned at top; transcript auto-follows new content
- **Timeline view** — scrollable activity log (transitions, events, stream
  entries)

Key bindings:

| Key | List view | Detail view | Timeline |
|-----|-----------|-------------|----------|
| `j`/`k`, arrows | Navigate patches | Scroll transcript | Scroll log |
| `Enter` | Open detail | Enter text mode | — |
| `Esc`/`Backspace` | — | Back to list | Back to list |
| `t` | Timeline | Timeline | List |
| `h` | Help overlay | Help overlay | Help overlay |
| `q` | Quit | Quit | Quit |

Text mode (Enter in detail view, or `:` in list):
- Type a message and press Enter — sent as human message to the currently
  viewed patch (clears `needs_intervention`)
- `N> message` — send human message to patch N
- `+123` — register ad-hoc PR #123 for the selected patch
- `w /path` — register existing worktree directory
- `-` — remove the selected patch from orchestration

The input prompt is visible in the footer as `: <text>`.

Headless mode (`--headless`) outputs plain timestamped log lines to stdout with
dedup-based entry tracking.

## Formal spec

The state machine is specified in
[Pantagruel](https://github.com/subsetpark/pantagruel). Key
properties:

- Sessions are never lost (`has_session p -> has_session' p`)
- Merged is absorbing (terminal state)
- Queue isolation (responding to `k` only removes `k`)
- CI failure cap (3 failures triggers intervention)
- Liveness (all fireable actions fire)
- `approved?` is derived: `has_pr && merge_ready && not busy && not
  needs_intervention && base_branch = main` (where `merge_ready` reflects
  GitHub's `mergeStateStatus = CLEAN`). Patches targeting a dependency branch
  show `blocked-by-dep` instead

## License

BSD-3
