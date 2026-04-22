#!/bin/sh
# sync-skills — symlink repo skills/ into ~/.claude/skills/ for cross-project discovery
#
# Run once after cloning (or whenever onton's skills change) to make the skills
# globally available to Claude Code regardless of which repo you're working in.
# Also invoked automatically by the post-checkout hook installed via
# scripts/install-hooks.sh.
#
# Per-skill behavior:
#   - no entry: create a symlink pointing at this checkout
#   - broken symlink: replace it
#   - symlink already pointing at this checkout: leave alone
#   - symlink pointing at a different onton checkout: SKIP (do not hijack —
#     another worktree may be actively using it)
#   - regular file or directory: SKIP

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SKILLS_DIR="$REPO_ROOT/skills"
TARGET_DIR="$HOME/.claude/skills"

[ -d "$SKILLS_DIR" ] || exit 0

mkdir -p "$TARGET_DIR"

for skill_path in "$SKILLS_DIR"/*/SKILL.md; do
  [ -f "$skill_path" ] || continue

  skill_dir="$(dirname "$skill_path")"
  skill_name="$(basename "$skill_dir")"
  target_link="$TARGET_DIR/$skill_name"

  if [ -L "$target_link" ] && [ ! -e "$target_link" ]; then
    rm "$target_link"
    ln -s "$skill_dir" "$target_link"
    echo "[fix] $skill_name → $skill_dir (replaced broken symlink)"
  elif [ ! -e "$target_link" ] && [ ! -L "$target_link" ]; then
    ln -s "$skill_dir" "$target_link"
    echo "[new] $skill_name → $skill_dir"
  elif [ -L "$target_link" ]; then
    # Resolve the stored symlink target to an absolute path before comparing.
    # `readlink` (without -f, which isn't portable) returns the raw stored
    # target; relative targets must be interpreted relative to the symlink's
    # own directory.
    link_raw="$(readlink "$target_link")"
    case "$link_raw" in
      /*) link_target="$link_raw" ;;
      *)  link_target="$(cd "$TARGET_DIR" && cd "$(dirname "$link_raw")" 2>/dev/null && pwd)/$(basename "$link_raw")" ;;
    esac
    if [ "$link_target" = "$skill_dir" ]; then
      echo "[ok] $skill_name"
    else
      echo "[skip] $skill_name — already linked to $link_target (owned by another checkout)"
    fi
  else
    echo "[skip] $skill_name — exists as regular file/directory at $target_link"
  fi
done
