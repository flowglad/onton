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
- A Claude Code hook runs `dune build` automatically after Edit/Write — check its output.
- All warnings except 44 (open-shadow) and 70 (missing-mli) are fatal errors.
- The pre-commit hook runs build + test + format check — commits will be rejected if any fail.

## Code patterns

- Never use `*_exn` (`Map.find_exn`, `List.hd_exn`, `Option.value_exn`) on data from external sources (GitHub API, JSON, user input). Use `Map.find`, `List.hd`, `Option.value ~default` or pattern matching. `*_exn` is fine on internal invariants (e.g. a map you just built from a known-complete list).
- Wrap `Eio.Semaphore.acquire`/`release` in `Fun.protect ~finally` — never hold across error paths.
- Prefer `Map.add` (returns `` `Ok | `Duplicate ``) over `Map.set` when key uniqueness is expected. Silent overwrites mask bugs.
- QCheck2 property bodies that call functions with raise paths: wrap with `try ... with _ -> false` so unexpected exceptions become falsifications, not runner crashes. Catch `Invalid_argument _` specifically when testing precondition violations. Do not wrap pure/total predicates (e.g. `Poller.poll` properties) that have no raise paths.
- Every `Patch_agent.busy = true` must have a corresponding `complete` on all exit paths (success, error, cancel). Use `Fun.protect` when the busy→idle transition spans async work.
- bisect_ppx is not compatible with our OCaml version — do not add it as a dependency.

## Stack
- OCaml 5.4.0, dune 3.21, local opam switch
- `open Base` in lib modules for Jane Street ppx compatibility
- PPX: ppx_deriving (show/eq/ord), ppx_sexp_conv, ppx_compare, ppx_hash, ppx_expect, ppx_inline_test, ppx_assert
