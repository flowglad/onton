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

## Stack
- OCaml 5.4.0, dune 3.21, local opam switch
- `open Base` in lib modules for Jane Street ppx compatibility
- PPX: ppx_deriving (show/eq/ord), ppx_sexp_conv, ppx_compare, ppx_hash, ppx_expect, ppx_inline_test, ppx_assert
