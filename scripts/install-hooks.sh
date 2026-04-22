#!/bin/sh
# install-hooks — install onton's git hooks into the repo's hooks dir
#
# Currently installs:
#   - post-checkout: runs sync-skills.sh after branch switches so ~/.claude/skills
#     always points at the current checkout
#
# Safe to run repeatedly. Refuses to overwrite a pre-existing post-checkout hook
# that isn't onton's own. Also runs sync-skills.sh once immediately so skills
# are available without switching branches.

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

HOOK_PATH="$HOOKS_DIR/post-checkout"

if [ -e "$HOOK_PATH" ] && ! grep -q 'sync-skills.sh' "$HOOK_PATH"; then
  echo "error: $HOOK_PATH already exists and is not onton's hook." >&2
  echo "       Merge the following into it manually, then re-run:" >&2
  echo "         REPO_ROOT=\"\$(git rev-parse --show-toplevel 2>/dev/null)\" || exit 0" >&2
  echo "         [ -x \"\$REPO_ROOT/scripts/sync-skills.sh\" ] && \"\$REPO_ROOT/scripts/sync-skills.sh\"" >&2
  exit 1
fi

# Single-quoted heredoc delimiter so $REPO_ROOT is resolved at hook runtime,
# not baked in at install time (the repo may later be moved or cloned elsewhere).
cat > "$HOOK_PATH" <<'HOOK'
#!/bin/sh
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || exit 0
[ -x "$REPO_ROOT/scripts/sync-skills.sh" ] && "$REPO_ROOT/scripts/sync-skills.sh"
HOOK
chmod +x "$HOOK_PATH"
echo "[installed] post-checkout hook at $HOOK_PATH"

"$SCRIPT_DIR/sync-skills.sh"
