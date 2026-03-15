# onton

An OCaml orchestrator for parallel Claude Code agents executing gameplan patches. Port of the [Anton](https://github.com/flowglad/orchestrate-gameplan) Elixir/OTP system.

Onton parses a structured gameplan (markdown), builds a dependency graph, and spawns concurrent Claude Code agents in git worktrees — one per patch. It polls GitHub for PR status, detects merges, triggers rebases, and reacts to CI failures and review comments. A terminal UI shows live status.

## Status

**Work in progress.** Core pure-logic modules are implemented (types, graph, priority, parser, state machine, orchestrator, reconciler). I/O modules (GitHub API, Claude subprocess, git worktree) and TUI are stubbed.

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

## Build & test

```sh
dune build          # compile with strict warnings (most warnings are fatal)
dune runtest        # inline tests + standalone tests
dune exec bin/main.exe  # run the orchestrator
dune fmt            # auto-format via ocamlformat
```

## Architecture

```
gameplan.md ──> Gameplan_parser ──> Graph + Patches
                                        │
                    Orchestrator ────────┤
                    ├── Patch_agent (per patch, state machine)
                    ├── Poller (GitHub PR status)
                    ├── Reconciler (merge detection, rebases)
                    └── TUI (terminal display)
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
| `invariants` | Runtime spec invariant checker |
| `prompt` | Agent prompt rendering |
| `worktree` | Git worktree CRUD |
| `github` | GitHub GraphQL API client (cohttp-eio) |
| `claude_process` | Claude CLI subprocess management |
| `term` | ANSI terminal primitives |
| `markdown_render` | Markdown to ANSI renderer |
| `tui` | Terminal UI with status display |

### Design principles

- **Eio for structured concurrency** — one fiber per patch, semaphore-gated Claude spawning
- **Pure logic core** — parser, graph, priority, state machine are pure functions with no I/O
- **Strict compiler feedback** — all warnings fatal (except 44/70), `.mli` files enforce module boundaries
- **Pantagruel spec alignment** — state machine transitions match the formal spec in `anton.pant`
- **Single source of truth** — priority ordering and `is_feedback` defined once in `Priority`, used everywhere

## Formal spec

The state machine is specified in [Pantagruel](https://github.com/subsetpark/pantagruel) (`anton.pant`). Key properties:

- Sessions are never lost (`has_session p -> has_session' p`)
- Merged is absorbing (terminal state)
- Queue isolation (responding to `k` only removes `k`)
- CI failure cap (3 failures triggers intervention)
- Liveness (all fireable actions fire)

## License

MIT
