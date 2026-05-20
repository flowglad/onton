# onton — OCaml project

## Build & test commands
- `dune build` — type-check + compile with fatal warnings. Run after every edit.
- `dune runtest` — run inline tests (`%test`, `%expect_test`) and standalone tests. Run after logic changes.
- `dune exec bin/main.exe` — run the main executable.
- `dune fmt` — auto-format all OCaml files via ocamlformat.
- `dune build @check` — type-check only (no linking), faster for quick feedback.
- `dune clean && dune build` — full rebuild when cache seems stale.

## Workflow
- Always run `dune build` after edits and fix all errors before moving on.
- An agent hook may run `dune build` automatically after edits — check its output.
- All warnings except 44 (open-shadow) and 70 (missing-mli) are fatal errors.
- The pre-commit hook runs build + test + format check — commits will be rejected if any fail.

## Code patterns

- Never use `*_exn` (`Map.find_exn`, `List.hd_exn`, `Option.value_exn`) on data from external sources (GitHub API, JSON, user input). Use `Map.find`, `List.hd`, `Option.value ~default` or pattern matching. `*_exn` is fine on internal invariants (e.g. a map you just built from a known-complete list).
- Wrap `Eio.Semaphore.acquire`/`release` in `Fun.protect ~finally` — never hold across error paths.
- Prefer `Map.add` (returns `` `Ok | `Duplicate ``) over `Map.set` when key uniqueness is expected. Silent overwrites mask bugs.
- QCheck2 property bodies that call functions with raise paths: wrap with `try ... with _ -> false` so unexpected exceptions become falsifications, not runner crashes. Catch `Invalid_argument _` specifically when testing precondition violations. Do not wrap pure/total predicates (e.g. `Poller.poll` properties) that have no raise paths.
- Every `Patch_agent.busy = true` must have a corresponding `complete` on all exit paths (success, error, cancel). Use `Fun.protect` when the busy→idle transition spans async work.
- bisect_ppx is not compatible with our OCaml version — do not add it as a dependency.

## Pure decision modules

The codebase is split into two dune libraries:

- `lib_core/` — pure decision logic. Modules here are total functions of their inputs (parsers, state algebras, predicates, decision functions). The split is about excluding effects, not about avoiding non-Base libraries: pure support libraries such as `re` are fine in `lib_core/dune`. The linker rejects any pure module that reaches for `eio`, `unix`, `cohttp-eio`, `tls-eio`, `mirage-crypto-rng.unix`, `ca-certs`, `lwt`, or any other effectful library.
- `lib/` — effectful handlers. Process spawning, Eio fibers, FS, HTTP, locks. Handlers decode inputs from the effect world, call pure decisions, thread the returned state, and emit effects.

Reference example: `Codex_cost` (pure, in `lib_core/`) plus `Codex_backend` (handler, in `lib/`). The handler reads `codex exec --json` lines, calls `Codex_cost.on_turn_completed`, threads the returned state, and emits stream events.

### Rules for new code

- Anything that is a pure function of its inputs — parsing, state transitions, predicates, decisions, classifications — goes in `lib_core/`. Default to the pure side; only put code in `lib/` when it needs an effect.
- Do NOT add `eio`, `unix`, `cohttp-eio`, `tls-eio`, `mirage-crypto-rng.unix`, `ca-certs`, `lwt`, or any process/IO library to `lib_core/dune`. Pure helper dependencies are allowed when they keep decision logic total and testable. If a pure decision needs an effectful input (clock, env var, file contents), the handler in `lib/` reads it and passes it as an argument.
- Pure modules must be **total** — decoders return zero/default values on malformed input, never raise. Use private types and `.mli` files to make invariants explicit (e.g. `cost_state` is private so monotonicity is enforced at construction).
- Handler modules in `lib/` are thin: decode → call pure decision → thread state → emit. Resist putting decision logic inline in the handler; extract it into the pure module instead.

### Property tests for pure modules

Each pure module gets a property test exe at `test/test_<module>_properties.ml` and a `test/dune` stanza that links **`onton_core` only** (not `onton`). The pure-only link is a second-tier linker check: if the module accidentally depends on something from `onton`, the test exe stops compiling.

Required property families:

- **Totality**: the function never raises over arbitrary generated inputs.
- **Algebraic properties**: monotonicity, idempotence, commutativity, bounds — whatever the pure semantics promise.
- **Interleaving properties**: when the module exposes a state machine, generate sequences of operations and verify invariants hold across all interleavings (cumulative monotone, sums match, permutation invariance, error stickiness, etc.).
- **Boundary conditions**: closed-vs-open intervals, zero/negative values, empty collections, schema-priority order.

Worked example: `test/test_codex_cost_properties.ml` (26 properties × 100–500 cases each, covering pricing math, usage decoding totality, schema priority, cap-decision boundary, single-step decisions, and full interleavings).

## Debug case investigation

User-reported issues arrive with a case ID (e.g. `c1bd-9d8d-1732`). Bundles are
uploaded by `lib/debug_upload.ml` to the Cloudflare R2 bucket `onton-debug`
under the key `uploads/<case-id>.json`.

Fetching:

```bash
wrangler r2 object get "onton-debug/uploads/<case-id>.json" --remote --file /tmp/case.json
```

`--remote` is required — without it wrangler reads the local simulator and the
key won't be found.

Bundle shape: top-level `{version, timestamp, project_name, onton_version,
files}`. The `files` map carries string-encoded `config.json`, `events.jsonl`,
`snapshot.json`, and (when present) `gameplan`. Extract with
`jq -r '.files["events.jsonl"]' > /tmp/events.jsonl` etc.

Pitfalls when reading a bundle:

- `events.jsonl` is **append-only across onton restarts**. The same file can
  carry events from multiple sessions separated by long gaps. Always check
  the timestamp range first (`jq -r .ts | sort -u | head/tail`) and look for
  multi-minute gaps — they mark a process restart, and config (e.g.
  `main_branch`) may have changed across the gap. Don't conflate epochs.
- `snapshot.json` is a point-in-time runtime view written every 5s by
  `persistence_fiber`. It can lag the live runtime, and in some setups it
  appears stale (empty `agents`, empty `activity_log`) while `events.jsonl`
  clearly shows agent activity. When the snapshot disagrees with events,
  trust the `agent_before`/`agent_after` fields embedded in each event for
  ground-truth agent state at that timestamp.
- `activity_log.events` (inside the snapshot) carries the human-readable
  per-session error strings — `"Session failed (<backend>) — exit 1: …"`,
  rebase failure detail, etc. Check it before concluding that `complete`
  events are opaque; `complete.result` only carries the structured
  `session_result` (with `detail : string option` post-error-capture-fix).
- `config.json` shows the persisted config. Compare its fields (especially
  `main_branch` and `repo_root`) against the same fields in `agent_before`
  across epochs — a mismatch usually means the user changed CLI flags
  between runs.

## Reference
- Reference implementation (Elixir): `../orchestrate-gameplan/`
- Reference specification: `../orchestrate-gameplan/spec/anton.pant`

## Stack
- OCaml 5.4.1, dune 3.21, local opam switch
- `open Base` in lib modules for Jane Street ppx compatibility
- PPX: ppx_deriving (show/eq/ord), ppx_sexp_conv, ppx_compare, ppx_hash, ppx_expect, ppx_inline_test, ppx_assert
