# onton

An OCaml orchestrator for parallel Claude Code agents executing gameplan patches. Port of the [Anton](https://github.com/flowglad/orchestrate-gameplan) Elixir/OTP system.

Onton parses a structured gameplan (markdown), builds a dependency graph, and spawns concurrent Claude Code agents in git worktrees — one per patch. It polls GitHub for PR status, detects merges, triggers rebases, and reacts to CI failures and review comments. A terminal UI shows live status.

## Status

**Functional.** Core orchestration loop works end-to-end: pure logic core, GitHub GraphQL polling, Claude subprocess spawning with streaming, four-fiber Eio event loop (TUI, poller, runner, persistence), session fallback chain, snapshot persistence/restore, startup reconciliation, and Cmdliner CLI. Property-based tests (QCheck2) cover spec invariants for graph, patch agent, orchestrator, reconciler, persistence, and stream parsing.

## Requirements

- OCaml 5.4.0
- dune 3.21
- opam (local switch included)

## Setup

```sh
git clone https://github.com/flowglad/onton.git
cd onton
opam switch create . ocaml.5.4.0 --deps-only
eval $(opam env)
opam install . --deps-only
```

## Usage

```sh
onton [PROJECT] --gameplan GAMEPLAN [OPTIONS]   # Start a new project
onton PROJECT [OPTIONS]                         # Resume a saved project
```

| Flag | Default | Description |
|------|---------|-------------|
| `PROJECT` | (derived from gameplan) | Project name (positional). Required to resume, optional with `--gameplan` |
| `--gameplan` | — | Path to the gameplan markdown file |
| `--token` | `$GITHUB_TOKEN` | GitHub API token |
| `--owner` | `$GITHUB_OWNER` | GitHub repository owner |
| `--repo` | `$GITHUB_REPO` | GitHub repository name |
| `--main-branch` | `main` | Main branch name |
| `--poll-interval` | `30.0` | GitHub polling interval in seconds |
| `--repo-root` | `.` | Path to the git repository root |
| `--max-concurrency` | `5` / `$ONTON_MAX_CONCURRENCY` | Maximum concurrent Claude processes |
| `--headless` | off | Run without TUI (plain log output to stdout) |

Project config and state are persisted to `~/.local/share/onton/<project>/`. Resuming a project reloads the saved snapshot and reconciles against GitHub.

## Build & test

```sh
dune build          # compile with strict warnings (most warnings are fatal)
dune runtest        # inline tests + property tests (QCheck2)
dune build @check   # type-check only (no linking), faster for quick feedback
dune exec bin/main.exe -- --gameplan GAMEPLAN --token TOKEN --owner OWNER --repo REPO
dune fmt            # auto-format via ocamlformat
```

## Architecture

```
gameplan.md ──> Gameplan_parser ──> Graph + Patches
                                        │
                    Orchestrator ────────┤
                    ├── Patch_agent (per patch, state machine)
                    ├── Poller (GitHub PR status via GraphQL)
                    ├── Reconciler (merge detection, rebases)
                    └── TUI (terminal display)

          ┌─────────────────────────────────────────────┐
          │              Eio_main.run                    │
          │  ┌────────┐ ┌───────┐ ┌───────┐ ┌────────┐ │
          │  │  TUI   │ │Poller │ │Runner │ │Persist │ │
          │  │ fiber  │ │ fiber │ │ fiber │ │ fiber  │ │
          │  └───┬────┘ └───┬───┘ └───┬───┘ └───┬────┘ │
          │      └──────────┼─────────┼─────────┘      │
          │            Runtime (Eio.Mutex)              │
          └─────────────────────────────────────────────┘
```

### Modules

| Module | Purpose |
|--------|---------|
| `types` | Core types: `Patch_id`, `Branch`, `Operation_kind`, `Patch`, `Comment`, `Gameplan` |
| `priority` | Operation priority queue — single source of truth for ordering |
| `graph` | Dependency graph: unblocked detection, base branch resolution |
| `gameplan_parser` | Markdown gameplan to structured `Gameplan.t` |
| `patch_agent` | Per-patch state machine: start, respond, complete transitions (private type) |
| `claude_process` | Claude CLI session state machine (No_session → Has_session) |
| `orchestrator` | Top-level tick loop: fires all actions whose preconditions hold |
| `reconciler` | Pure merge detection, rebase cascading, liveness enforcement |
| `startup_reconciler` | PR discovery, worktree recovery, stale busy reset at startup |
| `poller` | GitHub polling: comments, CI, merge conflicts, merge/approval state |
| `state` | Spec context maps (PatchCtx, Comments) |
| `runtime` | Mutex-protected shared snapshot across fibers |
| `activity_log` | Per-patch event, transition, and stream entry feed |
| `invariants` | Runtime spec invariant checker (gated via `ONTON_CHECK_INVARIANTS`) |
| `persistence` | JSON snapshot save/load with backward-compatible migration |
| `project_store` | Project config and gameplan storage at `~/.local/share/onton/` |
| `prompt` | Agent prompt rendering with per-project override support |
| `worktree` | Git worktree CRUD and branch detection |
| `github` | GitHub GraphQL API client (HTTPS via Eio) |
| `claude_runner` | Claude subprocess spawning with NDJSON streaming |
| `term` | ANSI terminal primitives (raw mode, key input, size, SIGTSTP/SIGCONT) |
| `tui_input` | Keyboard → command translation, text-mode parsing, history buffer |
| `tui` | Terminal UI: list/detail/timeline views, status derivation, frame rendering |
| `markdown_render` | Markdown subset to ANSI terminal renderer |

### Design principles

- **Eio for structured concurrency** — four fibers (TUI, poller, runner, persistence), concurrent Claude spawning via `Eio.Fiber.all` with semaphore-bounded concurrency
- **Pure logic core** — parser, graph, priority, state machine are pure functions with no I/O
- **Strict compiler feedback** — all warnings fatal (except 44/70), `.mli` files enforce module boundaries
- **Pantagruel spec alignment** — state machine transitions match the formal spec in `anton.pant`
- **Single source of truth** — priority ordering and `is_feedback` defined once in `Priority`, used everywhere
- **Property-based testing** — QCheck2 tests for graph, patch agent, orchestrator liveness, reconciler, state machine, persistence roundtrip, stream parsing, and TUI input

## CI

GitHub Actions runs on every push and PR:

- **Build** — `dune build` with compiler error annotations on PR diffs
- **Test** — `dune runtest` with failure annotations
- **Property tests** — QCheck2 with 10,000 iterations
- **Format check** — `ocamlformat` via `ocaml/setup-ocaml/lint-fmt`

## TUI

Three view modes:
- **List view** — patch table with status badges, queue depth, CI failures, current operation
- **Detail view** — single patch: status, branch, PR, dependencies, conflict, pending comments
- **Timeline view** — scrollable activity log (transitions, events, stream entries)

Key bindings: `j`/`k` or arrows to navigate, `Enter` for detail, `Esc`/`Backspace` to go back, `t` for timeline, `q` to quit, `:` for text mode. Text mode supports `N> message` (human message to patch N), `+123` (register ad-hoc PR), `w /path` (register worktree), `-` (remove patch).

Headless mode (`--headless`) outputs plain timestamped log lines to stdout.

## Formal spec

The state machine is specified in [Pantagruel](https://github.com/subsetpark/pantagruel) (`anton.pant`). Key properties:

- Sessions are never lost (`has_session p -> has_session' p`)
- Merged is absorbing (terminal state)
- Queue isolation (responding to `k` only removes `k`)
- CI failure cap (3 failures triggers intervention)
- Liveness (all fireable actions fire)

## License

MIT
