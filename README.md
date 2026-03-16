# onton

An OCaml orchestrator for parallel Claude Code agents executing gameplan patches. Port of the [Anton](https://github.com/flowglad/orchestrate-gameplan) Elixir/OTP system.

Onton parses a structured gameplan (markdown), builds a dependency graph, and spawns concurrent Claude Code agents in git worktrees — one per patch. It polls GitHub for PR status, detects merges, triggers rebases, and reacts to CI failures and review comments. A terminal UI shows live status.

## Status

**Feature-complete.** All modules are implemented: pure logic core, GitHub GraphQL polling, Claude subprocess spawning, three-fiber Eio event loop (TUI, poller, runner), and Cmdliner CLI. Property-based tests (QCheck2) cover all spec invariants.

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
onton GAMEPLAN --token TOKEN --owner OWNER --repo REPO [OPTIONS]
```

| Flag | Default | Description |
|------|---------|-------------|
| `GAMEPLAN` | (required) | Path to the gameplan markdown file |
| `--token` | `$GITHUB_TOKEN` | GitHub API token |
| `--owner` | `$GITHUB_OWNER` | GitHub repository owner |
| `--repo` | `$GITHUB_REPO` | GitHub repository name |
| `--main-branch` | `main` | Main branch name |
| `--poll-interval` | `30.0` | GitHub polling interval in seconds |
| `--repo-root` | `.` | Path to the git repository root |

## Build & test

```sh
dune build          # compile with strict warnings (most warnings are fatal)
dune runtest        # inline tests + property tests (QCheck2)
dune exec bin/main.exe -- GAMEPLAN --token TOKEN --owner OWNER --repo REPO
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

          ┌──────────────────────────────────┐
          │          Eio_main.run            │
          │  ┌────────┐ ┌───────┐ ┌───────┐ │
          │  │  TUI   │ │Poller │ │Runner │ │
          │  │ fiber  │ │ fiber │ │ fiber │ │
          │  └───┬────┘ └───┬───┘ └───┬───┘ │
          │      └──────────┼─────────┘     │
          │           Runtime (Eio.Mutex)    │
          └──────────────────────────────────┘
```

### Modules

| Module | Purpose |
|--------|---------|
| `types` | Core types: `Patch_id`, `Branch`, `Operation_kind`, `Patch`, `Comment` |
| `priority` | Operation priority queue — single source of truth for ordering |
| `graph` | Dependency graph: unblocked detection, base branch resolution |
| `gameplan_parser` | Markdown gameplan to structured `Gameplan.t` |
| `patch_agent` | Per-patch state machine: start, respond, complete transitions |
| `orchestrator` | Top-level tick loop: fires all actions whose preconditions hold |
| `reconciler` | Merge detection, rebase cascading, spawn planning |
| `poller` | GitHub polling: comments, CI, merge conflicts, approvals |
| `state` | Mutable state store with context maps |
| `runtime` | Mutex-protected shared state across fibers |
| `activity_log` | Per-patch event and status transition feed |
| `invariants` | Runtime spec invariant checker |
| `prompt` | Agent prompt rendering |
| `worktree` | Git worktree CRUD |
| `github` | GitHub GraphQL API client (cohttp-eio) |
| `claude_process` | Claude CLI session state machine |
| `claude_runner` | Claude subprocess spawning via Eio.Process |
| `term` | ANSI terminal primitives (raw mode, key input, size) |
| `markdown_render` | Markdown to ANSI renderer |
| `tui` | Terminal UI: status table, activity feed, context-sensitive status |

### Design principles

- **Eio for structured concurrency** — three fibers (TUI, poller, runner), concurrent Claude spawning via `Eio.Fiber.all`
- **Pure logic core** — parser, graph, priority, state machine are pure functions with no I/O
- **Strict compiler feedback** — all warnings fatal (except 44/70), `.mli` files enforce module boundaries
- **Pantagruel spec alignment** — state machine transitions match the formal spec in `anton.pant`
- **Single source of truth** — priority ordering and `is_feedback` defined once in `Priority`, used everywhere
- **Property-based testing** — QCheck2 tests verify all spec invariants (patch_agent P1-P10, orchestrator liveness, reconciler absorbing, graph satisfaction, poller signals)

## CI

GitHub Actions runs on every push and PR:

- **Build** — `dune build` with compiler error annotations on PR diffs
- **Test** — `dune runtest` with failure annotations
- **Property tests** — QCheck2 with 10,000 iterations
- **Format check** — `ocamlformat` via `ocaml/setup-ocaml/lint-fmt`

## Formal spec

The state machine is specified in [Pantagruel](https://github.com/subsetpark/pantagruel) (`anton.pant`). Key properties:

- Sessions are never lost (`has_session p -> has_session' p`)
- Merged is absorbing (terminal state)
- Queue isolation (responding to `k` only removes `k`)
- CI failure cap (3 failures triggers intervention)
- Liveness (all fireable actions fire)

## License

MIT
