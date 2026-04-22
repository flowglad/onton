#!/bin/sh
# install-hooks — install onton's git hooks into .git/hooks/
#
# Currently installs:
#   - post-checkout: runs sync-skills.sh after branch switches so ~/.claude/skills
#     always points at the current checkout
#
# Safe to run repeatedly. Overwrites any existing post-checkout hook. Also runs
# sync-skills.sh once immediately so skills are available without switching
# branches.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
HOOKS_DIR="$REPO_ROOT/.git/hooks"

if [ ! -d "$HOOKS_DIR" ]; then
  echo "error: $HOOKS_DIR not found — is this a git clone of onton?" >&2
  exit 1
fi

cat > "$HOOKS_DIR/post-checkout" <<HOOK
#!/bin/sh
[ -x "$SCRIPT_DIR/sync-skills.sh" ] && "$SCRIPT_DIR/sync-skills.sh"
HOOK
chmod +x "$HOOKS_DIR/post-checkout"
echo "[installed] post-checkout hook"

"$SCRIPT_DIR/sync-skills.sh"
