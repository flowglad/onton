# Gameplan: onton-port

## Problem Statement

The Anton orchestrator (Elixir/OTP) manages parallel Claude Code agents executing gameplan patches — parsing dependency graphs, spawning agents in git worktrees, polling GitHub for PR status, and reacting to merges/reviews/CI. It works, but it's in Elixir. We want this in OCaml to leverage the type system as a correctness tool and to co-locate the orchestrator with the Pantagruel spec checker (also OCaml).

## Solution Summary

Port Anton to OCaml 5.4 using Eio for structured concurrency. Replace OTP GenServers with Eio fibers, DynamicSupervisor with `Eio.Semaphore`, ETS with mutex-protected state, and Erlang Ports with `Eio.Process`. The Pantagruel spec (`anton.pant`) defines functional correctness — each patch includes the spec fragment it implements. Pure logic modules (parser, graph, priority queue) come first; I/O and orchestration follow.

## Mergability Strategy

### Feature Flagging Strategy

**No feature flag needed.** This is a greenfield port into a new codebase. Each patch adds new modules — nothing existing breaks.

### Patch Ordering Strategy

**Early (INFRA):** Core types, pure logic modules (parser, graph, priority), test stubs for orchestration.
**Middle (INFRA):** I/O modules (git, GitHub, Claude subprocess) — they add capability but no orchestration behavior.
**Late (BEHAVIOR):** State machine, orchestration loop, TUI — these implement the spec's actions.

## Current State Analysis

The OCaml project has: dune 3.21, OCaml 5.4, `open Base`, ppx ecosystem (deriving, sexp, compare, hash, expect, inline_test, assert), strict warnings. The `lib/onton.ml` has a placeholder `point` type. We need to replace this with the full module set.

New opam dependencies needed: `eio_main eio cohttp-eio tls-eio yojson ppx_yojson_conv qcheck-core` (cmdliner and yojson already installed). bisect_ppx deferred — incompatible with OCaml 5.4, revisit when updated. No TUI library — raw ANSI + Eio (see Explicit Opinions).

## Required Changes

### New modules (all in `lib/`)

| Module | Purpose | Spec coverage |
|--------|---------|---------------|
| `types.ml` | Core algebraic types | Domains: Patch, OperationKind, Branch, Comment |
| `gameplan_parser.ml` | Markdown → `Gameplan.t` | None (parsing is outside spec) |
| `graph.ml` | Dependency analysis | `deps-satisfied`, `open-pr-deps`, `initial-base`, `branch-of` |
| `priority.ml` | Operation priority queue | `priority`, `highest-priority`, `is-feedback` |
| `prompt.ml` | Agent prompt rendering | None (UX concern) |
| `worktree.ml` | Git worktree CRUD | None (infrastructure) |
| `github.ml` | GitHub GraphQL API | `world-*` predicates (data source) |
| `claude_process.ml` | Claude CLI subprocess | None (infrastructure) |
| `state.ml` | Mutable state store | `PatchCtx`, `WorldCtx`, `Comments` contexts |
| `patch_agent.ml` | Per-patch state machine | `Respond`, `Rebase`, `Start`, `Complete` actions |
| `poller.ml` | GitHub PR polling | `Poll` action |
| `reconciler.ml` | Merge detection + rebase | `Rebase` action, merge cascading |
| `orchestrator.ml` | Top-level wiring | Liveness property, spawn logic |
| `term.ml` | ANSI terminal primitives | None (rendering infra) |
| `markdown_render.ml` | Markdown → ANSI renderer | None (rendering infra) |
| `tui.ml` | Terminal UI | Derived status display |

### Modifications

- `lib/dune` — add `(libraries base eio eio_main cohttp-eio yojson qcheck-core)`
- `lib/onton.ml` — replace placeholder with re-export module
- `bin/main.ml` — cmdliner CLI entry point
- `dune-project` — add opam deps
- `dune-workspace` — re-enable warning 70 (missing .mli): change `-w +a-44-70` to `-w +a-44` now that we mandate `.mli` files

## Acceptance Criteria

### Compile-time guarantees
- [ ] `dune build` compiles all modules with zero warnings (including warning 70: missing .mli)
- [ ] Every module with invariants has an `.mli` file enforcing its public interface
- [ ] `Patch_status.t` is `private` — only smart constructors can create values
- [ ] Newtype wrappers (`Patch_id`, `Pr_number`, `Session_id`, `Branch`) prevent type confusion
- [ ] (Deferred) Coverage gating once bisect_ppx supports OCaml 5.4

### Functional correctness
- [ ] Can parse the sample gameplan from `../orchestrate-gameplan/test/fixtures/sample_gameplan.md`
- [ ] Dependency graph correctly identifies unblocked patches, base branches, topological order
- [ ] Priority queue orders operations: rebase < human < merge-conflict < ci < review-comments
- [ ] State machine transitions match the Pantagruel spec for all 6 actions
- [ ] `check_invariants` passes for all valid states, catches all invalid states

### Property-based verification
- [ ] QCheck2 state machine tests pass 10,000 random command sequences
- [ ] All 7 properties (P1-P7) verified: sessions never lost, merged absorbing, queue isolation, CI cap, human clears intervention, liveness, invariants hold
- [ ] Parser fuzzing: 10,000 random inputs, no crashes
- [ ] Graph properties: unblocked correctness, topo sort, base branch rules

### Integration
- [ ] Can create/remove git worktrees
- [ ] Can spawn Claude CLI process and capture output
- [ ] Can query GitHub GraphQL API for PR status
- [ ] Orchestrator runs concurrent patch agents with configurable concurrency cap
- [ ] Reconciler detects merges, triggers rebases, spawns unblocked patches
- [ ] Poller detects new comments, CI failures, merge conflicts, approvals
- [ ] TUI displays patch status table with markdown rendering, colored status, scrolling, and input
- [ ] System recovers from crash via persisted state file

## Open Questions

None — all resolved:
1. **GitHub auth**: `GITHUB_TOKEN` env var with fallback to `gh auth token` (matching Elixir impl).
2. **State persistence**: JSON via yojson for debuggability.
3. **TUI**: Raw ANSI + Eio. No TUI library — none support Eio + OCaml 5.4. We build `term.ml` (ANSI primitives) and `markdown_render.ml` (markdown→ANSI) ourselves, which gives us full control over rendering quality.

## Explicit Opinions

1. **Eio over Lwt**: OCaml 5 native, structured concurrency matches the supervision tree pattern better than promises.
2. **`private` types + `.mli` files for state machine enforcement**: `Patch_status.t` is a `private` sum type — external code can pattern-match but must use smart constructors (`start`, `respond`, `complete`, `mark_merged`) that enforce the spec's preconditions at compile time. Every module with meaningful invariants gets an `.mli` file.
3. **Pure logic modules with no I/O**: Parser, graph, priority are pure functions. Test them with `%test` and `%expect_test`. No mocking needed.
4. **Direct GitHub GraphQL via `cohttp-eio`**: Avoids managing `gh` CLI subprocesses. One HTTP client, direct control over request/response, proper error types.
5. **One fiber per patch**: Each patch gets a long-lived Eio fiber. Semaphore gates Claude process spawning, not fiber creation.
6. **State behind Eio.Mutex, not message passing**: OCaml's type system prevents the bugs that OTP's process isolation guards against. Shared mutable state with a mutex is simpler.
7. **Newtype wrappers everywhere**: `Patch_id.t`, `Pr_number.t`, `Session_id.t`, `Branch.t` are all private newtypes — prevents mixing up stringly/intly-typed values. The compiler catches misuse.
8. **QCheck2 state machine tests**: The Pantagruel spec's actions and invariants are encoded directly as a QCheck2 state machine model. Random command sequences verify properties like "sessions are never lost", "merged is absorbing", "queue isolation".
9. **`check_invariants` after every mutation**: A single function encoding all spec invariants, called from property tests and optionally at runtime (via `ONTON_CHECK_INVARIANTS` env var).
10. **Coverage gating (deferred)**: bisect_ppx incompatible with OCaml 5.4. Revisit when updated. In the meantime, QCheck2 properties + exhaustive unit tests + `.mli` enforcement provide strong coverage guarantees.

## Patches

### Patch 1 [INFRA]: Core types

**Pantagruel spec fragment:**
```pant
Patch.
OperationKind.
Branch.
Comment.

rebase => OperationKind.
human => OperationKind.
merge-conflict => OperationKind.
ci => OperationKind.
review-comments => OperationKind.
```

**Prerequisites:** `opam install eio_main eio cohttp-eio tls-eio ppx_yojson_conv qcheck-core` and update `lib/dune`:
- Add `(libraries base eio eio_main cohttp-eio yojson qcheck-core)`

**Files to create:**
- `lib/types.ml` — type definitions
- `lib/types.mli` — public interface with private types

**Changes:**

1. **Newtype wrappers** (all `private` with smart constructors):
   ```ocaml
   module Patch_id : sig
     type t = private string [@@deriving show, eq, ord, sexp_of, compare, hash]
     val of_string : string -> t
     val to_string : t -> string
   end

   module Pr_number : sig
     type t = private int [@@deriving show, eq, ord, sexp_of, compare, hash]
     val of_int : int -> t
     val to_int : t -> int
   end

   module Session_id : sig
     type t = private string [@@deriving show, eq, sexp_of, compare, hash]
     val of_string : string -> t
     val to_string : t -> string
   end

   module Branch : sig
     type t = private string [@@deriving show, eq, sexp_of, compare, hash]
     val of_string : string -> t
     val to_string : t -> string
     val main : t
   end
   ```

2. Define `Operation_kind.t` as a variant: `Rebase | Human | Merge_conflict | Ci | Review_comments` with `[@@deriving show, eq, ord, sexp_of, compare, hash]`

3. **Operation queue as a set** (not a list): `type operation_queue = Operation_kind.Set.t` — makes "highest-priority" and "drain only selected" obviously correct

4. Define `Patch_classification.t`: `Infra | Gated | Behavior`

5. Define `Patch.t` record: `{ id : Patch_id.t; title; classification; description; files; depends_on : Patch_id.t list }`

6. Define `Gameplan.t` record: `{ project_name; problem_statement; solution_summary; patches; dependency_graph : Patch_id.t list Patch_id.Map.t }`

7. **`Patch_status.t` as a `private` sum type** — external code can pattern-match but must use smart constructors:
   ```ocaml
   (* types.mli *)
   type patch_status = private
     | Pending
     | Blocked of { waiting_on : Patch_id.t list }
     | Agent_running of { operation : Operation_kind.t; session_id : Session_id.t option }
     | Awaiting_review of { pr_number : Pr_number.t; branch : Branch.t }
     | Approved of { pr_number : Pr_number.t }
     | Merged of { pr_number : Pr_number.t; sha : string }
     | Needs_intervention of { reason : string }

   (* Smart constructors — the ONLY way to create status values *)
   val pending : unit -> patch_status
   val blocked : waiting_on:Patch_id.t list -> patch_status
   val start : patch_status -> base_branch:Branch.t -> pr_number:Pr_number.t -> (patch_status, string) result
     (** Requires: Pending or Blocked. Returns: Agent_running. *)
   val respond : patch_status -> Operation_kind.t -> session_id:Session_id.t option -> (patch_status, string) result
     (** Requires: has-pr, not merged, not busy. Returns: Agent_running. *)
   val complete : patch_status -> ci_failure_count:int -> session_failed:bool -> has_human:bool -> (patch_status, string) result
     (** Requires: Agent_running. Returns: Awaiting_review or Needs_intervention. *)
   val mark_merged : patch_status -> sha:string -> (patch_status, string) result
     (** Requires: has-pr, not already merged. Returns: Merged. Terminal — no function takes Merged as input. *)
   ```
   Absorptivity enforced by omission: there is no `val unmerge` or any constructor that accepts `Merged` as input.

8. Define `Patch_state.t` record: `{ patch : Patch.t; status : patch_status; worktree_path : string option; base_branch : Branch.t; ci_failure_count : int; has_conflict : bool; queue : operation_queue; inbox : string list; pending_comments : Comment.t list; pending_ci : Ci_check.t list; addressed_comment_ids : String.Set.t }`
   - PR number, branch, session_id live inside `patch_status` variants — no redundant optional fields
   - Accessor functions: `val pr_number : Patch_state.t -> Pr_number.t option`, `val session_id : Patch_state.t -> Session_id.t option` (derived from status variant)

9. Define `Comment.t` record: `{ id : string; body : string; thread_id : string; author : string }`

10. Define `Ci_check.t` record: `{ name : string; conclusion : string; details_url : string option; description : string option }`

11. Define `transition_entry` for logging: `{ timestamp : float; patch_id : Patch_id.t; from_status : patch_status; to_status : patch_status; action : string }`

12. Introduce test stubs: `let%test "PENDING Patch 10 - state transitions" = true`

### Patch 2 [INFRA]: Gameplan parser

**Pantagruel spec fragment:**
```pant
> (Outside spec — parsing is not a state transition.)
> Parser produces a Gameplan.t from structured markdown.
> Invariant: parsed patches match dependency graph keys.

Gameplan.
gameplan => Gameplan + Nothing.
deps p: Patch => [Patch].
```

**Files to create:**
- `lib/gameplan_parser.ml`
- `lib/gameplan_parser.mli`

**Changes:**
1. `val parse_file : string -> (Gameplan.t, string) result` — read file, split by `##` headers, extract sections
2. `val parse_string : string -> (Gameplan.t, string) result` — for testing
3. Extract project name from `# Gameplan: {name}` or `# {name}`
4. Extract patches from `### Patch N [CLASS]: Title` blocks using regex
5. Extract dependency graph from `- Patch N [CLASS] -> [deps]` lines
6. Extract test map from markdown table
7. Validate: all dependency targets exist, no self-deps, no cycles

**Tests:**
- `let%expect_test "parse sample gameplan"` — parse `../orchestrate-gameplan/test/fixtures/sample_gameplan.md`, print patch count and dep graph
- `let%test "reject cyclic deps"` — circular dependency returns error
- `let%test "reject missing dep target"` — dep on nonexistent patch returns error
- **QCheck2 fuzzing**: `let%test_unit "parser never crashes on arbitrary input"` — generate random strings, verify parser returns `Ok _` or `Error _` but never raises
- **QCheck2 structural invariant**: `let%test_unit "parsed gameplan is internally consistent"` — for any `Ok gameplan`, all dep targets exist in patches list, no self-deps, dep graph keys = patch IDs

### Patch 3 [INFRA]: Dependency graph

**Pantagruel spec fragment:**
```pant
deps p: Patch => [Patch].
depends-on p: Patch, d: Patch => Bool.
deps-satisfied p: Patch => Bool.
open-pr-deps p: Patch => [Patch].
sole-open-dep p: Patch => Patch.
initial-base p: Patch => Branch.
branch-of p: Patch => Branch.
main => Branch.

> Definition: depends-on p d  <->  d in deps p

> deps-satisfied p  <->  #(open-pr-deps p) <= 1,
>                        all deps merged or have PR

all p: Patch | #(open-pr-deps p) = 1 ->
    initial-base p = branch-of (sole-open-dep p).
all p: Patch | #(open-pr-deps p) = 0 ->
    initial-base p = main.

> Added patches have no deps.
all p: Patch | ~in-gameplan p -> #(deps p) = 0.
```

**Files to create:**
- `lib/graph.ml`
- `lib/graph.mli`

**Changes:**
1. `val unblocked : dep_graph:Patch_id.t list Patch_id.Map.t -> merged:Patch_id.Set.t -> open_pr:Patch_id.Set.t -> excluded:Patch_id.Set.t -> Patch_id.t list`
   — Patches whose deps are all merged or have open PR, and at most 1 open-PR dep in transitive closure
2. `val base_branch_for : dep_graph:_ -> open_pr_branches:Patch_id.t -> string -> string Map.t -> default_branch:string -> Patch_id.t -> string option`
   — `None` if blocked (2+ open PR deps), `Some branch` otherwise
3. `val topo_sort : Patch_id.t list Patch_id.Map.t -> Patch_id.t list` — Kahn's algorithm
4. `val dependents : dep_graph:_ -> Patch_id.t -> Patch_id.t list` — reverse lookup

**Tests:**
- `let%expect_test "diamond dependency"` — A→B,C→D, verify unblocked order
- `let%test "base branch with one open dep"` — returns dep's branch
- `let%test "base branch with zero open deps"` — returns "main"
- `let%test "blocked with two open deps"` — returns None
- `let%expect_test "topo sort"` — verify ordering
- **QCheck2**: `"unblocked patches have all deps satisfied"` — generate random DAGs + merged/open sets, verify every returned patch has all deps merged or with open PR
- **QCheck2**: `"topo sort respects all edges"` — for every edge (a→b) in the graph, a appears after b in the sorted output
- **QCheck2**: `"base_branch_for never returns blocked for ≤1 open dep"` — generate graphs with at most 1 open-PR dep, verify always returns `Some _`

### Patch 4 [INFRA]: Priority queue and operation ordering

**Pantagruel spec fragment:**
```pant
priority k: OperationKind => Nat0.
highest-priority p: Patch, k: OperationKind => Bool.
is-feedback k: OperationKind => Bool.

priority rebase = 0.
priority human = 1.
priority merge-conflict = 2.
priority ci = 3.
priority review-comments = 4.

is-feedback human.
is-feedback merge-conflict.
is-feedback ci.
is-feedback review-comments.
~is-feedback rebase.

all p: Patch, k: OperationKind | highest-priority p k <->
    (queue p k and (all j: OperationKind, queue p j | priority k <= priority j)).
```

**Files to create:**
- `lib/priority.ml`
- `lib/priority.mli`

**Changes:**
1. `val priority : Operation_kind.t -> int` — rebase=0, human=1, merge_conflict=2, ci=3, review_comments=4
2. `val is_feedback : Operation_kind.t -> bool` — all except Rebase
3. `val highest_priority : Operation_kind.t list -> Operation_kind.t option` — lowest priority number wins
4. `val queue_drain_order : Patch_state.t -> Operation_kind.t option` — given a patch's pending work, which operation to handle next (inbox → conflict → ci → comments)

**Inline tests:**
- `let%test "rebase has highest priority"` — priority 0
- `let%test "drain order: inbox before CI"` — human message takes precedence
- `let%test "all feedback kinds"` — is_feedback true for all except Rebase
- `let%expect_test "priority ordering"` — print sorted list

### Patch 5 [INFRA]: Prompt renderer

**Pantagruel spec fragment:**
```pant
> (Outside spec — UX concern, not a state transition.)
```

**Files to create:**
- `lib/prompt.ml`

**Changes:**
1. `val render_patch_prompt : Patch.t -> Gameplan.t -> base_branch:string -> project_name:string -> string`
   — Produce the initial agent prompt from patch + gameplan context
2. `val render_review_prompt : Comment.t list -> string` — format pending review comments
3. `val render_ci_failure_prompt : Ci_check.t list -> string` — format failed CI checks
4. `val render_merge_conflict_prompt : base_branch:string -> string` — rebase instructions
5. `val render_human_message_prompt : string list -> string` — join human messages

**Inline tests:**
- `let%expect_test "patch prompt includes title and deps"` — verify structure
- `let%expect_test "review prompt formats comments"` — verify comment formatting

### Patch 6 [INFRA]: Git worktree operations

**Pantagruel spec fragment:**
```pant
> (Infrastructure — worktrees are the execution environment for agents.)
> Postcondition of Start: a worktree exists for the patch.
```

**Files to create:**
- `lib/worktree.ml`

**Changes:**
1. `val create : repo_root:string -> project_name:string -> patch_id:Patch_id.t -> base_branch:string -> (string, string) result`
   — `git worktree add -b {branch} {path} {base}`, returns worktree path
2. `val remove : repo_root:string -> worktree_path:string -> (unit, string) result`
3. `val list : repo_root:string -> (string list, string) result` — `git worktree list --porcelain`
4. `val detect_branch : worktree_path:string -> (string, string) result` — `git rev-parse --abbrev-ref HEAD`
5. `val worktree_path : project_name:string -> patch_id:Patch_id.t -> string` — `~/worktrees/{project}/patch-{id}`
6. `val branch_name : project_name:string -> patch_id:Patch_id.t -> string` — `{project}/patch-{id}`
7. Helper: `val run_git : ?cwd:string -> string list -> (string, string) result` — shell out to git

**Inline tests:**
- `let%test "branch_name format"` — verify naming convention
- `let%test "worktree_path format"` — verify path convention

### Patch 7 [INFRA]: GitHub API client

**Pantagruel spec fragment:**
```pant
> (Data source for WorldCtx predicates.)

{WorldCtx} merged p: Patch => Bool.
{WorldCtx} mergeable p: Patch => Bool.
{WorldCtx} checks-passing p: Patch => Bool.
{WorldCtx} no-unresolved-comments p: Patch => Bool.

world-has-comment c: Comment, p: Patch => Bool.
world-has-conflict p: Patch => Bool.
world-ci-failed p: Patch => Bool.
world-merged p: Patch => Bool.
world-mergeable p: Patch => Bool.
world-checks-passing p: Patch => Bool.
```

**Files to create:**
- `lib/github.ml`

**Changes:**
1. `val configure : repo_path:string -> (Github_config.t, string) result` — resolve token (env var → `gh auth token`), extract owner/repo from git remote
2. `val get_pr_details : config:Github_config.t -> pr_number:int -> (Pr_info.t, string) result` — GraphQL query for PR with reviews, comments, CI
3. `val list_project_prs : config:Github_config.t -> project_name:string -> state:[`Open | `Merged] -> ((Patch_id.t * Pr_info.t) list, string) result` — search by PR title pattern `[project] Patch N:`
4. `val poll_open_prs : config:Github_config.t -> project_name:string -> (Pr_snapshot.t list, string) result`
5. Define `Pr_info.t`, `Pr_snapshot.t`, `Review_thread.t`, `Ci_status.t` records
6. Helper: `val graphql : config:Github_config.t -> query:string -> variables:(string * Yojson.Safe.t) list -> (Yojson.Safe.t, string) result` — POST to `https://api.github.com/graphql` via `cohttp-eio`, bearer auth from config

**Inline tests:**
- `let%test "extract owner/repo from ssh remote"` — parse `git@github.com:owner/repo.git`
- `let%test "extract owner/repo from https remote"` — parse `https://github.com/owner/repo.git`
- `let%test "PR title pattern matching"` — extract patch ID from `[onton-port] Patch 3: Dependency graph`

### Patch 8 [INFRA]: Claude subprocess management

**Pantagruel spec fragment:**
```pant
> (Infrastructure — Claude sessions are the execution mechanism.)

{PatchCtx} has-session p: Patch => Bool.
{PatchCtx} busy p: Patch => Bool.
{PatchCtx} session-failed p: Patch => Bool.

> Sessions are never lost.
all p: Patch | has-session p -> has-session' p.
```

**Files to create:**
- `lib/claude_process.ml`

**Changes:**
1. `val run : sw:Eio.Switch.t -> process_mgr:_ Eio.Process.mgr -> cwd:string -> prompt:string -> session_id:string option -> (Claude_result.t, string) result`
   — Spawn `claude -p <prompt> --output-format stream-json [--session-id X] --dangerously-skip-permissions`
   — Stream stdout lines, parse NDJSON, extract session_id
   — Return `{ session_id; exit_code; output }`
2. `val resume : sw:Eio.Switch.t -> process_mgr:_ -> cwd:string -> prompt:string -> session_id:string -> (Claude_result.t, string) result`
   — `claude --continue --session-id X -p <prompt>`
3. Define `Claude_result.t`: `{ session_id : string; exit_code : int }`
4. Define `Claude_event.t` for stream-json parsing (message_start, content_block_delta, etc.)
5. Handle: timeout (configurable), process exit, ANSI stripping

**Inline tests:**
- `let%test "parse stream-json session_id"` — extract from sample NDJSON line
- `let%test "strip ANSI escapes"` — verify cleanup

### Patch 9 [INFRA]: Persistent state store + invariant checker

**Pantagruel spec fragment:**
```pant
context PatchCtx.
context WorldCtx.
context Comments.

> PatchCtx contains all per-patch mutable state.
> WorldCtx contains external world observations.
> Comments tracks comment resolution state.

{PatchCtx} queue p: Patch, k: OperationKind => Bool.
{PatchCtx} busy p: Patch => Bool.
{PatchCtx} has-pr p: Patch => Bool.
{PatchCtx} has-session p: Patch => Bool.
{PatchCtx} needs-intervention p: Patch => Bool.
{PatchCtx} ci-failure-count p: Patch => Nat.
{PatchCtx} base-branch p: Patch => Branch.

{Comments} resolved c: Comment => Bool.
{Comments} pending c: Comment, p: Patch => Bool.
```

**Files to create:**
- `lib/state.ml`
- `lib/state.mli`
- `lib/invariants.ml` — spec invariant checker
- `lib/invariants.mli`

**Changes:**

`lib/state.ml`:
1. `State.t` — `Eio.Mutex.t` protecting `Patch_id.t -> Patch_state.t` hashtable + metadata + transition log
2. `val create : Gameplan.t -> State.t` — initialize all patches as Pending
3. `val get : State.t -> Patch_id.t -> Patch_state.t option`
4. `val update : State.t -> Patch_id.t -> (Patch_state.t -> Patch_state.t) -> unit` — atomic update under mutex; logs transition; optionally runs `check_invariants` when `ONTON_CHECK_INVARIANTS` is set
5. `val all : State.t -> Patch_state.t list`
6. `val persist : State.t -> filepath:string -> unit` — JSON to disk via yojson
7. `val load : filepath:string -> State.t` — restore from JSON
8. `val put_gameplan : State.t -> Gameplan.t -> unit`
9. `val get_gameplan : State.t -> Gameplan.t option`
10. `val transition_log : State.t -> transition_entry list` — for debugging and TUI

`lib/invariants.ml` — encodes ALL Pantagruel spec invariants as executable checks:
1. `val check_invariants : Patch_state.t list -> dep_graph:Patch_id.t list Patch_id.Map.t -> (unit, string list) result`
2. Checks:
   - No patch is both busy and merged
   - Merged patches stay merged (cross-reference with transition log)
   - Sessions are monotonic (once has_session, always has_session)
   - Queue contents are consistent with status (no queue on Pending patches)
   - Dependency graph is acyclic
   - All dependency targets exist
   - ci_failure_count ≥ 3 implies Needs_intervention (unless human message pending)
   - No patch has 2+ open-PR deps active simultaneously

**Tests:**
- `let%test "create initializes all pending"` — verify initial state
- `let%test "update is atomic"` — concurrent updates don't lose data
- `let%expect_test "persist round-trip"` — save + load preserves state
- `let%test "invariant checker catches merged-then-unmerged"` — manually construct bad state, verify checker rejects
- `let%test "invariant checker passes for valid state"` — construct spec-compliant state, verify checker accepts

### Patch 10 [BEHAVIOR]: Patch agent state machine

**Pantagruel spec fragment:**
```pant
PatchCtx, Comments ~> Respond | p: Patch, k: OperationKind,
    is-feedback k,
    has-pr p, ~merged p, ~busy p, ~needs-intervention p,
    queue p k, highest-priority p k.
---
has-session' p.
busy' p.
~queue' p k.
all j: OperationKind, j != k | queue' p j = queue p j.
k != human and satisfies p -> satisfies' p.
k = ci -> changed' p.
k = merge-conflict -> ~has-conflict' p.
k = review-comments -> all c: Comment, pending c p |
    resolved' c and (valid' c -> changed' p)
    and (~valid' c -> replied' c).

PatchCtx ~> Start | p: Patch, in-gameplan p, deps-satisfied p, ~has-pr p.
---
has-pr' p.
has-session' p.
busy' p.
satisfies' p.
base-branch' p = initial-base p.

PatchCtx ~> Complete | p: Patch, busy p.
---
~busy' p.
all p: Patch | ci-failure-count p >= 3 -> needs-intervention' p.
all p: Patch | session-failed p -> needs-intervention' p.
all p: Patch | queue p human -> ~needs-intervention' p.
```

**Files to create:**
- `lib/patch_agent.ml`
- `lib/patch_agent.mli`

**Changes:**
1. Pure transition function:
   ```ocaml
   val next_action : Patch_state.t -> graph_context -> action option
   type action =
     | Start_claude of { prompt : string; base_branch : string }
     | Resume_claude of { prompt : string; operation : Operation_kind.t }
     | Wait
   ```
2. `val apply_complete : Patch_state.t -> Patch_state.t` — agent finished turn: clear busy, check CI cap, check session failure
3. `val apply_respond : Patch_state.t -> Operation_kind.t -> Patch_state.t` — drain highest-priority queue item, mark busy
4. `val apply_start : Patch_state.t -> base_branch:string -> Patch_state.t` — set has_pr, busy, session
5. `val run_patch : sw:Eio.Switch.t -> env:_ -> state:State.t -> semaphore:Eio.Semaphore.t -> patch_id:Patch_id.t -> unit`
   — Long-lived fiber loop: read state → compute action → acquire semaphore → execute → update state → repeat

**Tests:**

*Unit + expect tests:*
- `let%test "start sets has_pr and busy"` — verify postconditions
- `let%test "complete clears busy"` — verify postcondition
- `let%test "ci failure cap at 3"` — 3 failures → needs_intervention
- `let%test "human message clears needs_intervention"` — recovery path
- `let%test "respond drains only selected queue"` — other queues unchanged
- `let%test "sessions never lost"` — has_session stable once set
- `let%expect_test "full lifecycle"` — Pending → Running → Awaiting_review → Merged

*QCheck2 state machine tests (the core correctness verification):*

Define a model mirroring the Pantagruel spec, generate random command sequences, verify properties:

```ocaml
type model = {
  has_pr : bool; busy : bool; merged : bool; has_session : bool;
  needs_intervention : bool; queue : Operation_kind.Set.t;
  ci_failure_count : int; has_conflict : bool;
}
type command = Start | Respond of Operation_kind.t | Complete of { session_failed : bool }
             | Enqueue of Operation_kind.t | Mark_merged
```

Properties verified:
- **P1 "Sessions never lost"**: `has_session p -> has_session' p` after any valid transition
- **P2 "Merged is absorbing"**: once Merged, status never changes
- **P3 "Queue isolation"**: Respond for `k` removes only `k`, other queue entries preserved
- **P4 "CI failure cap"**: 3 consecutive CI completions → Needs_intervention
- **P5 "Human clears intervention"**: queue Human + Respond Human → clears Needs_intervention
- **P6 "Liveness"**: queued + has_pr + not busy/merged/needs_intervention → next_action ≠ Wait
- **P7 "Invariants hold after every step"**: `check_invariants` passes after every command

### Patch 11 [BEHAVIOR]: GitHub poller

**Pantagruel spec fragment:**
```pant
PatchCtx, WorldCtx ~> Poll.
---
all c: Comment, p: Patch |
    world-has-comment c p and ~resolved c -> queue' p review-comments.
all p: Patch | world-has-conflict p -> queue' p merge-conflict.
all p: Patch | world-has-conflict p -> has-conflict' p.
all p: Patch | world-ci-failed p -> queue' p ci.
all p: Patch | world-merged p -> merged' p.
all p: Patch | merged p -> merged' p.
all p: Patch | mergeable' p = world-mergeable p.
all p: Patch | checks-passing' p = world-checks-passing p.
```

**Files to create:**
- `lib/poller.ml`
- `lib/poller.mli`

**Changes:**
1. Pure evaluation function:
   ```ocaml
   type poll_action =
     | Notify_comments of Patch_id.t * Comment.t list
     | Notify_ci_failure of Patch_id.t * Ci_check.t list
     | Notify_merge_conflict of Patch_id.t
     | Clear_merge_conflict of Patch_id.t
     | Notify_approved of Patch_id.t
     | Revoke_approved of Patch_id.t
     | Mark_merged of Patch_id.t

   val evaluate : Pr_snapshot.t -> Patch_state.t -> poll_action list
   ```
2. `val run : sw:Eio.Switch.t -> env:_ -> state:State.t -> config:Github_config.t -> project_name:string -> unit`
   — 30s loop: poll GitHub, evaluate each PR, apply actions to state
3. Merged is terminal and absorbing: once merged, never un-merged

**Inline tests:**
- `let%test "new comment queues review-comments"` — unresolved comment → queue
- `let%test "resolved comment not queued"` — already resolved → skip
- `let%test "CI failure queues ci"` — failed check → queue
- `let%test "merge conflict queues and sets flag"` — both queue and has_conflict
- `let%test "merged is absorbing"` — once merged, stays merged
- `let%expect_test "evaluate multiple signals"` — PR with comments + CI failure

### Patch 12 [BEHAVIOR]: Reconciler

**Pantagruel spec fragment:**
```pant
PatchCtx ~> Rebase | p: Patch,
    has-pr p, ~merged p, ~busy p,
    queue p rebase, highest-priority p rebase.
---
busy' p.
~queue' p rebase.
has-session p -> has-session' p.
base-branch' p = merge-target (base-branch p).
all j: OperationKind, j != rebase | queue' p j = queue p j.

merge-target b: Branch => Branch.

> Liveness: pending operations on an idle, living patch must be addressed.
all p: Patch, k: OperationKind |
    queue p k and has-pr p and ~merged p and ~busy p and ~needs-intervention p ->
    busy' p.
```

**Files to create:**
- `lib/reconciler.ml`
- `lib/reconciler.mli`

**Changes:**
1. Pure logic functions:
   ```ocaml
   type reconcile_action =
     | Mark_merged of Patch_id.t
     | Rebase of Patch_id.t * string (* patch, onto branch *)
     | Spawn of Patch_id.t * string  (* patch, base branch *)

   val detect_merges : Patch_state.t list -> merged_prs:Pr_info.t Patch_id.Map.t -> reconcile_action list
   val detect_rebases : Patch_state.t list -> merged_patches:Patch_id.Set.t -> reconcile_action list
   val plan_spawns : Patch_state.t list -> dep_graph:_ -> semaphore_available:int -> reconcile_action list
   ```
2. `val run : sw:Eio.Switch.t -> env:_ -> state:State.t -> config:Github_config.t -> gameplan:Gameplan.t -> repo_root:string -> unit`
   — 30s loop: detect merges, detect rebases, spawn unblocked patches
3. Startup reconciliation: on first tick, sync state with GitHub (recover from crash)

**Inline tests:**
- `let%test "detect merge marks patch"` — GitHub shows merged → Mark_merged
- `let%test "detect rebase when dep merges"` — dep merged, dependent has stale base → Rebase
- `let%test "spawn respects concurrency cap"` — only spawn up to available slots
- `let%test "no rebase while busy"` — don't rebase a running agent
- `let%expect_test "full reconciliation cycle"` — merge → rebase → spawn sequence

### Patch 13 [BEHAVIOR]: Orchestrator (top-level wiring)

**Pantagruel spec fragment:**
```pant
> The orchestrator ensures liveness: all actions that can fire, do fire.
> It wires together the state store, patch agents, poller, and reconciler.

all p: Patch, k: OperationKind |
    queue p k and has-pr p and ~merged p and ~busy p and ~needs-intervention p ->
    busy' p.

> Start is biconditional with its preconditions.
all p: Patch | in-gameplan p and deps-satisfied p and ~has-pr p <->
    has-pr' p and satisfies' p.
```

**Files to create/modify:**
- `lib/orchestrator.ml`
- `lib/onton.ml` (re-export)
- `bin/main.ml` (CLI entry point)

**Changes:**
1. `val run : env:_ Eio.Stdenv.t -> gameplan_path:string -> repo_root:string -> max_concurrent:int -> unit`
   — Parse gameplan, create/load state, create semaphore, spawn fibers:
   ```
   Eio_main.run @@ fun env ->
     Eio.Switch.run @@ fun sw ->
       let state = State.create gameplan in
       let sem = Eio.Semaphore.make max_concurrent in
       (* Spawn one fiber per patch *)
       List.iter patches ~f:(fun p ->
         Eio.Fiber.fork ~sw (fun () -> Patch_agent.run_patch ~sw ~env ~state ~semaphore:sem ~patch_id:p.id));
       (* Spawn infrastructure fibers *)
       Eio.Fiber.fork ~sw (fun () -> Reconciler.run ~sw ~env ~state ~config ~gameplan ~repo_root);
       Eio.Fiber.fork ~sw (fun () -> Poller.run ~sw ~env ~state ~config ~project_name);
       Eio.Fiber.fork ~sw (fun () -> Tui.run ~sw ~state ~input_stream)
   ```
2. `bin/main.ml`: cmdliner CLI with `--repo`, `--max-concurrent`, positional gameplan path
3. `lib/onton.ml`: `module Types = Types`, `module Graph = Graph`, etc. (re-exports)

### Patch 14 [INFRA]: Terminal rendering primitives

**Pantagruel spec fragment:**
```pant
> (Outside spec — rendering infrastructure.)
```

**Files to create:**
- `lib/term.ml` — low-level ANSI terminal primitives
- `lib/markdown_render.ml` — markdown-to-ANSI renderer

**Changes:**

`lib/term.ml` — ANSI terminal abstraction:
1. **Raw mode**: `val with_raw_mode : (unit -> 'a) -> 'a` — set terminal to raw mode, restore on exit/exception
2. **Alternate screen**: `val enter_alt_screen : unit -> unit` / `val leave_alt_screen : unit -> unit`
3. **Cursor**: `val move_to : row:int -> col:int -> unit`, `val hide_cursor / show_cursor`
4. **Colors**: `val fg : color -> string -> string`, `val bg : color -> string -> string`, `val bold / dim / italic / underline / reset`
   - `type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Bright of color | Rgb of int * int * int`
5. **Screen**: `val clear_screen : unit -> unit`, `val terminal_size : unit -> int * int`
6. **Buffered output**: `val with_buffer : (Buffer.t -> unit) -> string` — build full frame in buffer, write once (no flicker)
7. **Box drawing**: `val hline : int -> string`, `val vline`, `val box : width:int -> height:int -> string list -> string`
8. **Input**: `val read_key : unit -> key_event` — parse escape sequences into structured key events
   - `type key_event = Char of char | Up | Down | Left | Right | PageUp | PageDown | Enter | Escape | Ctrl of char`

`lib/markdown_render.ml` — markdown → styled terminal output:
1. `val render : width:int -> string -> string` — convert markdown to ANSI-styled text
2. Handles: `# headers` (bold + color), `**bold**`, `*italic*`, `` `inline code` `` (dim bg), ``` ```code blocks``` ``` (indented + syntax-colored), `- lists` (with bullets), `> blockquotes` (dimmed + bar), `[links](url)` (underlined + blue)
3. Word-wrapping to terminal width
4. Simple approach: regex-based pass over lines, no full AST needed (the markdown we render is agent output and gameplan descriptions — well-structured)

**Inline tests:**
- `let%expect_test "bold rendering"` — `**hello**` → bold ANSI
- `let%expect_test "code block rendering"` — fenced code → indented + dimmed
- `let%expect_test "header rendering"` — `# Title` → bold + colored
- `let%test "terminal_size returns positive"` — sanity check
- `let%test "key parsing: escape sequence"` — `\x1b[A` → Up

### Patch 15 [BEHAVIOR]: Terminal UI

**Pantagruel spec fragment:**
```pant
> Derived status (display only, not mutable state):
>   merged               -> merged
>   needs-intervention   -> needs-help
>   approved?            -> approved
>   busy, ~is-feedback   -> rebasing
>   busy, is-feedback    -> running
>   has-pr, ~busy, ~approved?, ~needs-intervention -> awaiting-review
>   ~has-pr              -> pending

approved? p: Patch => Bool.
is-feedback-busy p: Patch => Bool.
```

**Files to create:**
- `lib/tui.ml`

**Changes:**

**Architecture**: Elm-style Model-View-Update running as an Eio fiber. Alternate screen buffer for clean entry/exit.

1. **Model**:
   ```ocaml
   type view = Overview | Patch_detail of Patch_id.t | Event_log
   type model = {
     view : view;
     table_scroll : int;
     log_scroll : int;  (* `bottom` = follow, int = pinned *)
     input_buf : string;
     event_log : (float * Patch_id.t option * string) list;  (* timestamp, source, message *)
     terminal_size : int * int;
   }
   ```

2. **Derived status** (from spec):
   ```ocaml
   val derive_display_status : Patch_state.t -> status_display
   type status_display = {
     label : string;    (* "merged", "running", "needs-help", etc. *)
     color : Term.color; (* green for merged, red for needs-help, yellow for running, etc. *)
   }
   ```

3. **Views**:
   - **Overview** — patch status table (colored status chips, dep counts, PR links, titles) + truncated event log at bottom + input line
     ```
     ┌─ onton-port ──────────────────────────────────────┐
     │ #  │ Status          │ Deps │ PR   │ Title         │
     │ 1  │ ● merged        │ —    │ #42  │ Core types    │
     │ 2  │ ● running       │ 1    │ #43  │ Parser        │
     │ 3  │ ○ pending       │ 1    │ —    │ Graph         │
     ├────────────────────────────────────────────────────┤
     │ [Patch 2] Parsing dependency graph section...      │
     │ [Patch 1] PR merged                               │
     ├────────────────────────────────────────────────────┤
     │ 2> _                                              │
     └──────────────────────────────────────────────────────┘
     ```
   - **Patch detail** — full patch info: status, PR link, branch, base, agent activity log with markdown-rendered agent output, pending comments, CI status
   - **Event log** — full scrollable event history with timestamps

4. **Input handling**:
   - `N<Enter>` — focus patch N (switch to Patch_detail view)
   - `0` or `Esc` — back to Overview
   - `N> message` — send human message to patch N's inbox
   - `+PR_NUMBER` — add ad-hoc PR to session
   - `+path/to/worktree` — add ad-hoc worktree
   - `q` — quit
   - `Up/Down` — scroll patch table
   - `PgUp/PgDn` — scroll event log
   - `Tab` — cycle views (Overview → Event log → Overview)
   - `?` — help overlay

5. **Render loop**:
   ```ocaml
   val run : sw:Eio.Switch.t -> clock:_ Eio.Time.t -> state:State.t ->
     event_stream:event Eio.Stream.t -> unit
   ```
   - Enter alternate screen + raw mode on start
   - 2s refresh: read state, diff against previous frame, render to buffer, flush
   - Input fiber: `read_key` in tight loop, dispatch to model update
   - Clean exit: leave alternate screen, restore terminal on quit/exception/signal

6. **Event log integration**:
   - `val log_event : State.t -> ?patch:Patch_id.t -> string -> unit` — append to shared event log
   - Other modules (patch_agent, reconciler, poller) call this to surface activity

**Inline tests:**
- `let%test "derive merged status"` — Merged → `{ label = "merged"; color = Green }`
- `let%test "derive needs-help status"` — Needs_intervention → `{ label = "needs-help"; color = Red }`
- `let%test "parse message input"` — `"3> fix the tests"` → `Send_message (Patch_id.of_string "3", "fix the tests")`
- `let%test "parse add PR"` — `"+123"` → `Add_pr 123`
- `let%test "parse focus"` — `"5"` → `Focus (Patch_id.of_string "5")`
- `let%expect_test "render overview table"` — verify table layout with colors stripped
- `let%expect_test "render patch detail"` — verify detail view structure

## Test Map

### Unit + expect tests

| Test Name | File | Stub Patch | Impl Patch |
|-----------|------|------------|------------|
| state transitions | lib/types.ml | 1 | 10 |
| parse sample gameplan | lib/gameplan_parser.ml | — | 2 |
| reject cyclic deps | lib/gameplan_parser.ml | — | 2 |
| diamond dependency | lib/graph.ml | — | 3 |
| base branch rules | lib/graph.ml | — | 3 |
| topo sort | lib/graph.ml | — | 3 |
| priority ordering | lib/priority.ml | — | 4 |
| drain order | lib/priority.ml | — | 4 |
| patch prompt structure | lib/prompt.ml | — | 5 |
| branch_name format | lib/worktree.ml | — | 6 |
| owner/repo parsing | lib/github.ml | — | 7 |
| stream-json parsing | lib/claude_process.ml | — | 8 |
| state round-trip | lib/state.ml | — | 9 |
| invariant checker rejects bad state | lib/invariants.ml | — | 9 |
| invariant checker accepts valid state | lib/invariants.ml | — | 9 |
| start sets has_pr | lib/patch_agent.ml | — | 10 |
| complete clears busy | lib/patch_agent.ml | — | 10 |
| ci failure cap | lib/patch_agent.ml | — | 10 |
| full lifecycle | lib/patch_agent.ml | — | 10 |
| new comment queues | lib/poller.ml | — | 11 |
| merged is absorbing | lib/poller.ml | — | 11 |
| detect merge | lib/reconciler.ml | — | 12 |
| spawn respects cap | lib/reconciler.ml | — | 12 |
| bold rendering | lib/markdown_render.ml | — | 14 |
| code block rendering | lib/markdown_render.ml | — | 14 |
| key parsing | lib/term.ml | — | 14 |
| derive status | lib/tui.ml | — | 15 |
| parse input | lib/tui.ml | — | 15 |
| render overview | lib/tui.ml | — | 15 |
| parse add PR | lib/tui.ml | — | 15 |

### Property-based tests (QCheck2)

| Property | File | Patch | Spec reference |
|----------|------|-------|----------------|
| parser never crashes on arbitrary input | lib/gameplan_parser.ml | 2 | — |
| parsed gameplan is internally consistent | lib/gameplan_parser.ml | 2 | `deps p: Patch => [Patch]` |
| unblocked patches have all deps satisfied | lib/graph.ml | 3 | `deps-satisfied` |
| topo sort respects all edges | lib/graph.ml | 3 | — |
| base_branch_for correct for ≤1 open dep | lib/graph.ml | 3 | `initial-base`, `sole-open-dep` |
| P1: sessions never lost | lib/patch_agent.ml | 10 | `has-session p -> has-session' p` |
| P2: merged is absorbing | lib/patch_agent.ml | 10 | `merged p -> merged' p` |
| P3: queue isolation | lib/patch_agent.ml | 10 | `queue' p j = queue p j` (j≠k) |
| P4: CI failure cap | lib/patch_agent.ml | 10 | `ci-failure-count p >= 3 -> needs-intervention' p` |
| P5: human clears intervention | lib/patch_agent.ml | 10 | `queue p human -> ~needs-intervention' p` |
| P6: liveness | lib/patch_agent.ml | 10 | `queue p k and ... -> busy' p` |
| P7: invariants hold after every step | lib/patch_agent.ml | 10 | (all invariants) |

## Dependency Graph

```
- Patch 1 [INFRA] -> []
- Patch 2 [INFRA] -> [1]
- Patch 3 [INFRA] -> [1]
- Patch 4 [INFRA] -> [1]
- Patch 5 [INFRA] -> [1]
- Patch 6 [INFRA] -> [1]
- Patch 7 [INFRA] -> [1]
- Patch 8 [INFRA] -> [1]
- Patch 9 [INFRA] -> [1]
- Patch 10 [BEHAVIOR] -> [1, 3, 4, 5, 8, 9]
- Patch 11 [BEHAVIOR] -> [1, 7, 9]
- Patch 12 [BEHAVIOR] -> [1, 3, 6, 7, 9]
- Patch 13 [BEHAVIOR] -> [10, 11, 12, 15]
- Patch 14 [INFRA] -> []
- Patch 15 [BEHAVIOR] -> [1, 9, 14]
```

**Mergability insight**: 10 of 15 patches are `[INFRA]` and can ship without changing observable behavior. Patches 2-9 and 14 can all be parallelized after Patch 1 (14 has no deps at all).

## Mergability Checklist

- [x] Feature flag strategy documented (not needed — greenfield port)
- [x] Early patches contain only non-functional changes (`[INFRA]`)
- [x] Test stubs are in early `[INFRA]` patches where applicable
- [x] Test implementations are co-located with the code they test (same patch)
- [x] Test Map is complete
- [x] Test Map Impl Patch matches the patch that implements the tested code
- [x] `[BEHAVIOR]` patches are as small as possible (5 of 15)
- [x] Dependency graph shows `[INFRA]` patches early, `[BEHAVIOR]` patches late
- [x] Each `[BEHAVIOR]` patch is clearly justified (implements spec actions or wires components)
