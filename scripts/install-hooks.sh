#!/bin/sh
# install-hooks — install onton's git hooks into the repo's hooks dir
#
# Currently installs:
#   - post-checkout: runs sync-skills.sh after branch switches so ~/.claude/skills
#     always points at the current checkout
#   - pre-commit: runs dune build / runtest / fmt against the project's local
#     opam switch (5.4.0), not whatever the user's default switch happens to be
#
# Safe to run repeatedly. Refuses to overwrite a pre-existing hook that isn't
# onton's own (detected via a sentinel comment). Also runs sync-skills.sh once
# immediately so skills are available without switching branches.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Use `git rev-parse --git-path hooks` so linked worktrees install into their
# own hooks directory rather than the main checkout's.
HOOKS_DIR="$(git -C "$REPO_ROOT" rev-parse --git-path hooks 2>/dev/null)" || HOOKS_DIR=""

if [ -z "$HOOKS_DIR" ]; then
  echo "error: could not resolve git hooks directory — is this a git clone of onton?" >&2
  exit 1
fi

# `--git-path` may return a path relative to REPO_ROOT; anchor it.
case "$HOOKS_DIR" in
  /*) ;;
  *)  HOOKS_DIR="$REPO_ROOT/$HOOKS_DIR" ;;
esac

mkdir -p "$HOOKS_DIR"

# install_hook <hook_name> <sentinel> reads the hook body from stdin.
# Detects onton-installed hooks via the sentinel so renaming the body
# doesn't break re-installation, and a third-party hook that merely
# mentions onton in a comment isn't mistaken for ours.
install_hook() {
  hook_name="$1"
  sentinel="$2"
  hook_path="$HOOKS_DIR/$hook_name"

  if [ -e "$hook_path" ] && ! grep -q "$sentinel" "$hook_path"; then
    echo "error: $hook_path already exists and is not onton's hook." >&2
    echo "       Back it up and remove it, then re-run install-hooks.sh." >&2
    echo "       The desired body is inlined in scripts/install-hooks.sh." >&2
    exit 1
  fi

  cat > "$hook_path"
  chmod +x "$hook_path"
  echo "[installed] $hook_name hook at $hook_path"
}

# Single-quoted heredoc delimiters so $vars are resolved at hook runtime,
# not baked in at install time (the repo may later be moved or cloned elsewhere).

install_hook post-checkout 'onton-managed-post-checkout' <<'HOOK'
#!/bin/sh
# onton-managed-post-checkout — do not edit this line; install-hooks.sh uses it
# to detect that this hook is owned by onton and may be safely overwritten.
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || exit 0
[ -x "$REPO_ROOT/scripts/sync-skills.sh" ] && "$REPO_ROOT/scripts/sync-skills.sh"
HOOK

install_hook pre-commit 'onton-managed-pre-commit' <<'HOOK'
#!/bin/sh
# onton-managed-pre-commit — do not edit this line; install-hooks.sh uses it
# to detect that this hook is owned by onton and may be safely overwritten.
set -e

# Activate the project's local opam switch (5.4.0) rather than whatever the
# user's default switch happens to be. git-common-dir resolves to the main
# repo's .git from any worktree; its parent is the project root that holds _opam.
PROJECT_ROOT="$(dirname "$(git rev-parse --path-format=absolute --git-common-dir)")"
eval "$(opam env --switch="$PROJECT_ROOT" --set-switch)"

echo "==> dune build"
dune build 2>&1

echo "==> dune runtest"
dune runtest 2>&1

echo "==> format check"
dune build @fmt 2>&1
HOOK

"$SCRIPT_DIR/sync-skills.sh"
