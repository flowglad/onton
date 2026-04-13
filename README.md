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

Its control path is replay-safe: a pure controller derives the same follow-up
effects and the same patch-agent messages from the same durable snapshot on any
tick. Patch-agent delivery is effectively-once: messages have stable logical
IDs, are persisted in an outbox, are acknowledged durably before work begins,
and can be resumed after crashes without reapplying the queue-consuming state
transition that originally accepted them.

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

## Dependencies

Onton shells out to several external tools and talks to the GitHub API. All
of these must be installed and configured before onton can run.

### Runtime dependencies

| Tool | Purpose | Install |
|------|---------|---------|
| `git` | Worktree CRUD, branch detection, rebase | `brew install git` (or system package manager) |
| `gh` (GitHub CLI) | Token resolution, PR discovery (`gh pr list`), and the main vehicle agents use to interact with GitHub (`gh pr create`, `gh pr edit`, `gh pr view`, `gh api`, `gh api graphql`) | `brew install gh`, then `gh auth login` |
| Coding-agent CLI | Drives the actual patches. One of: `claude` ([Claude Code](https://docs.anthropic.com/en/docs/claude-code)), `codex` ([OpenAI Codex CLI](https://github.com/openai/codex)), `opencode`, `pi`. Selected via `--backend` (default `claude`) and must be on `PATH` | See each tool's docs |
| `/usr/bin/script` | Used to allocate a pseudo-TTY around the Claude CLI so `-p` streaming works. Ships with macOS and most Linux distros — no install needed | — |

Onton is built and tested on macOS (ARM64 and x86_64). Linux should work but is
not part of the release pipeline.

### GitHub authentication

Onton talks to GitHub two ways and both need credentials:

1. **The OCaml binary** opens its own HTTPS connection to `api.github.com`
   (REST + GraphQL) for everything it does itself: polling, PR discovery at
   startup, merge-state lookups, draft toggles, base updates, etc. It needs a
   token in `Authorization: bearer …`. The only `gh` invocation from the
   binary itself is `gh auth token` during startup, used solely as a fallback
   to source that token.
2. **The agent processes** shell out to `gh` for higher-level operations like
   creating PRs (`gh pr create`), editing PR bodies (`gh pr edit`), and
   calling `gh api` / `gh api graphql`. `gh` uses its own credential store
   seeded by `gh auth login`.

#### Token resolution order

The token used by the OCaml binary is resolved in this order (see
`bin/main.ml:47`):

1. `--token` CLI flag
2. `GITHUB_TOKEN` environment variable
3. `gh auth token` (executed as a subprocess)

If all three are empty, onton refuses to start with
`--token / GITHUB_TOKEN is required`.

The simplest setup is therefore: install `gh`, run `gh auth login`, and let
onton pick up the token automatically. Confirm with `gh auth token` that a
non-empty value is printed.

#### Token scopes

The token must be able to read and write PRs, issue comments, and check runs
on the target repository.

- **Classic personal access token**: enable `repo` (full control of private
  repositories — needed because we read PR state, post comments, change base
  branches, and toggle draft status) and `read:org` if the repo lives under a
  GitHub organization with SSO. `workflow` is required if any patch touches
  files under `.github/workflows/`.
- **Fine-grained personal access token** (recommended): scope it to the
  specific repository and grant these repository permissions:
  - **Contents**: Read and write (worktree pushes, branch metadata)
  - **Pull requests**: Read and write (create/update PRs, change base, toggle
    draft, merge state)
  - **Issues**: Read and write (PR review/issue comments share the issues API)
  - **Checks**: Read (CI status polling)
  - **Commit statuses**: Read (legacy CI status polling)
  - **Metadata**: Read (always required)
  - **Workflows**: Read and write — only if patches will modify
    `.github/workflows/*`
- **GitHub App installation token**: same permissions as the fine-grained PAT
  list above. Pass it via `--token` or `GITHUB_TOKEN`; `gh` will not produce
  installation tokens for you.
- **SSO-protected orgs**: after creating the token, click "Configure SSO" on
  the token page and authorize it for the org, otherwise every API call returns
  `401`.

If you see `403`/`401` from the poller, the most common causes are: missing
SSO authorization, missing `Pull requests: write` (for draft/base updates), or
a fine-grained token that wasn't granted access to the specific repository.

#### `gh` configuration

`gh auth login` (choose HTTPS, authenticate via browser or paste a token)
configures `gh` itself. Verify with:

```sh
gh auth status        # shows which host and scopes are active
gh auth token         # prints the token onton will pick up
gh pr list --limit 1  # smoke-test repo access
```

If you'd rather keep `gh`'s token separate from onton's, set `GITHUB_TOKEN`
explicitly and `gh` will prefer that variable too — keeping both in sync.

### Optional: per-repo and project state directories

Onton writes to two locations on disk. Neither requires setup but both are
worth knowing about:

- `~/.local/share/onton/<project>/` — durable project state (snapshot,
  message ledger, transcripts). Created on first run.
- `~/.config/onton/<owner>/<repo>/` — per-repo user configuration, including
  the `on_worktree_create` hook described below. You create this directory by
  hand if you want a hook.

### Building from source (development only)

In addition to the runtime dependencies above, building from source needs the
OCaml toolchain listed under [Option C](#option-c-from-source): OCaml 5.4.0,
dune 3.21, and opam. `opam install . --deps-only` installs the rest
(`base`, `eio`, `cmarkit`, `re`, `cmdliner`, `qcheck`, etc.).

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
| `--main-branch` | (auto-detected) | Main branch name (inferred from remote HEAD if omitted) |
| `--poll-interval` | `30.0` | GitHub polling interval in seconds |
| `--max-concurrency` | `5` / `$ONTON_MAX_CONCURRENCY` | Maximum concurrent Claude processes |
| `--headless` | off | Run without TUI (plain log output to stdout) |

Project config and state are persisted to `~/.local/share/onton/<project>/`.
Resuming a project reloads the saved snapshot (including agent transcripts) and
reconciles against GitHub. The snapshot includes the durable patch-agent
message ledger, so accepted but incomplete work can resume after restart.

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
                  Patch_controller ──────┤
                    ├── poll ingestion + reconciliation
                    ├── GitHub lifecycle effects
                    ├── durable patch-agent message planning
                    └── runnable work derivation
                                         │
                    Orchestrator ────────┤
                    ├── Patch_agent (per-patch state machine)
                    ├── Patch_decision (pure decision logic)
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

### Claude backend session management

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

### Patch-agent message delivery

Runnable patch work is materialized as durable messages, not just ephemeral
runner actions. The controller writes those messages to an outbox with stable
logical IDs. The runner then:

1. accepts a pending message exactly once, which durably acknowledges it and
   fires the corresponding patch transition
2. executes the Claude or rebase work for that accepted message
3. marks the message completed when the patch finishes the operation

If onton crashes after acceptance but before completion, the same acknowledged
message is resumed on the next tick instead of creating a new one or replaying
the original queue-consuming transition.

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
| `patch_agent` | Per-patch state machine: start, respond, complete, rebase transitions (private type). Tracks `current_op`, current accepted message, and generation |
| `patch_controller` | Pure evergreen controller: poll ingestion, lifecycle reconciliation, GitHub effects, and durable patch-agent message planning |
| `patch_decision` | Pure decision logic: disposition, CI cap, review comment filtering, merge conflict handling. Extracted from main.ml for testability |
| `llm_backend` | Backend interface: process spawning, stream event parsing, session management |
| `claude_backend` | Claude Code backend implementation |
| `codex_backend` | OpenAI Codex backend implementation |
| `opencode_backend` | OpenCode backend implementation |
| `pi_backend` | Pi coding agent backend implementation |
| `claude_process` | Claude CLI session state machine (No_session -> Has_session) |
| `claude_runner` | Claude subprocess spawning with PTY wrapping, NDJSON streaming, ANSI stripping, `got_events` resume-failure detection |
| `spawn_logic` | Pure spawn/scheduling logic: which patches to run next |
| `orchestrator` | Durable patch state plus primitive transitions, including the patch-agent outbox |
| `reconciler` | Pure merge detection, rebase cascading, stale base detection, liveness enforcement |
| `startup_reconciler` | PR discovery, worktree recovery, stale busy reset at startup |
| `poller` | GitHub polling: comments, CI, merge conflicts, merge/approval state |
| `state` | Spec context maps (PatchCtx, Comments) for invariant checking |
| `runtime` | Mutex-protected shared snapshot across fibers (orchestrator + activity log + gameplan + transcripts) |
| `activity_log` | Per-patch event, transition, and stream entry feed |
| `event_log` | Structured event log for persistence and replay |
| `pr_state` | Pull request state tracking and derived status |
| `run_classification` | Classify agent run outcomes (success, failure, needs intervention) |
| `forge` | Git forge (GitHub) abstraction |
| `invariants` | Pure spec invariant checker over `State.t` — used by property tests and ad-hoc snapshot inspection (no production call site) |
| `persistence` | JSON snapshot save/load for the current durable schema, including transcripts and the message ledger |
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
  controller reconciliation, and message planning are pure functions with no
  I/O
- **Strict compiler feedback** — all warnings fatal (except 44/70), `.mli`
  files enforce module boundaries
- **Pantagruel spec alignment** — state machine transitions match the formal
  spec
- **Single source of truth** — priority ordering defined once in `Priority`;
  sorted patch display via shared `sorted_patch_ids` ref; `patch_controller`
  owns deterministic follow-up decisions; `current_op` plus the durable outbox
  track active and resumable work
- **Property-based testing** — QCheck2 tests for graph, patch agent,
  controller, orchestrator liveness, reconciler, delivery-aware state-machine
  behavior, persistence roundtrip, stream parsing, TUI input, poller, and
  patch decision

## CI

GitHub Actions runs on every push and PR:

- **Build & Test** — `dune build` + `dune runtest` with compiler error annotations on PR diffs
- **Property tests** — QCheck2 with 10,000 iterations
- **Format check** — `ocamlformat` via `ocaml/setup-ocaml/lint-fmt`

## CLI

Backend selection uses `--backend`:

- `onton --backend claude`
- `onton --backend codex`
- `onton --backend opencode`
- `onton --backend pi`

If omitted for a new project, `claude` is the default. The selected backend is
persisted in project config and reused on resume unless you pass `--backend`
again to override it.

Pushing a `v*` tag builds a macOS ARM64 binary, creates a GitHub release, and
updates the Homebrew formula.

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

## Docs site

The project documentation is hosted at [write-gameplan.dev](https://write-gameplan.dev)
and deployed to Vercel under the Flowglad org.

The site is static HTML in the `docs/` directory. To update it:

1. Edit files in `docs/` (HTML pages, `assets/styles.css`, etc.)
2. Deploy with the Vercel CLI:
   ```sh
   vercel --scope flowglad --prod
   ```

A `.vercelignore` ensures only `docs/` and `vercel.json` are uploaded.

## License

BSD-3
