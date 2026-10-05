#!/bin/sh
set -eu

ocaml=$1
generator=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
temp_dir=$(mktemp -d)
trap 'rm -rf "$temp_dir"' EXIT HUP INT TERM
# Fixture Git operations must not inherit a commit hook's repository selection.
unset GIT_DIR GIT_WORK_TREE GIT_COMMON_DIR GIT_INDEX_FILE
export GIT_CEILING_DIRECTORIES="$temp_dir"
source_root="$temp_dir/source checkout"
mkdir -p "$source_root" "$temp_dir/build tree" "$temp_dir/no-git"
git -C "$source_root" init -q
git -C "$source_root" config user.name 'Version test'
git -C "$source_root" config user.email version-test@example.invalid
git -C "$source_root" commit --allow-empty -qm initial

check_version() {
  # Reuse the same output location, with a cwd that has no source Git metadata.
  (cd "$temp_dir/build tree" && DUNE_SOURCEROOT="$1" "$ocaml" -I +unix unix.cma "$generator") >"$temp_dir/version.ml"
  printf '\nlet () = print_string s\n' >>"$temp_dir/version.ml"
  actual=$("$ocaml" "$temp_dir/version.ml")
  if [ "$actual" != "$2" ]; then
    printf 'Expected version %s, got %s\n' "$2" "$actual" >&2
    exit 1
  fi
}

check_version "$source_root" "$(git -C "$source_root" rev-parse --short=7 HEAD)"
git -C "$source_root" tag v1.2.3
check_version "$source_root" v1.2.3
git -C "$source_root" commit --allow-empty -qm next
check_version "$source_root" "v1.2.3-1-g$(git -C "$source_root" rev-parse --short=7 HEAD)"
git -C "$source_root" tag -a v2.0.0 -m release
check_version "$source_root" v2.0.0
git -C "$source_root" commit --allow-empty -qm quoted
git -C "$source_root" tag 'v3.0.0-"quoted"'
check_version "$source_root" 'v3.0.0-"quoted"'
git -C "$source_root" worktree add --detach -q "$temp_dir/worktree" HEAD
check_version "$temp_dir/worktree" 'v3.0.0-"quoted"'
check_version "$temp_dir/no-git" dev
