#!/bin/sh
# sync-skills — symlink repo skills/ into ~/.claude/skills/ for cross-project discovery
#
# Run once after cloning (or whenever onton's skills change) to make the skills
# globally available to Claude Code regardless of which repo you're working in.
# Also invoked automatically by the post-checkout hook installed via
# scripts/install-hooks.sh.

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
    link_target="$(readlink "$target_link")"
    if [ "$link_target" = "$skill_dir" ]; then
      echo "[ok] $skill_name"
    else
      rm "$target_link"
      ln -s "$skill_dir" "$target_link"
      echo "[update] $skill_name → $skill_dir (was $link_target)"
    fi
  else
    echo "[SKIP] $skill_name — exists as regular directory at $target_link"
  fi
done
