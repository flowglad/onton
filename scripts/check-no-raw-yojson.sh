#!/bin/sh
# check-no-raw-yojson — fail if Yojson.Safe.Util is used outside the allowlist.
#
# Yojson.Safe.Util's member/to_string/to_* are partial: they raise at runtime,
# invisibly to the type checker. That is the failure class behind PR #333's poll
# crash. lib_core/json.ml exposes total, option-returning accessors; all other
# decoding should go through Onton_core.Json (or ppx_yojson_conv). This guard
# stops new raw call sites from creeping in.
#
# The allowlist (scripts/yojson-util-allowlist.txt) names the files still
# permitted to reference Yojson.Safe.Util — long-term only lib_core/json.ml; the
# rest are pre-existing sites being migrated, each removed as it is converted.
#
# Exit 0 = clean. Exit 1 = an unallowlisted reference, or a stale allowlist
# entry (file gone, or no longer references the pattern — prune it).

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ALLOWLIST="$SCRIPT_DIR/yojson-util-allowlist.txt"
PATTERN='Yojson\.Safe\.Util'

cd "$REPO_ROOT" || exit 1

# Allowlisted paths, comments/blank lines stripped and trailing space trimmed.
allowed="$(grep -vE '^[[:space:]]*(#|$)' "$ALLOWLIST" | sed 's/[[:space:]]*$//')"

# Tracked OCaml sources in the libraries/binaries we police, including nested
# modules — the leading-anchored grep keeps it to those top-level directories.
files="$(git ls-files '*.ml' | grep -E '^(lib|lib_core|api|bin)/')"

status=0

# 1. Any file that references the pattern must be allowlisted.
# shellcheck disable=SC2086 # git paths are newline-separated and space-free
for f in $files; do
  grep -q "$PATTERN" "$f" || continue
  if ! printf '%s\n' "$allowed" | grep -qxF "$f"; then
    echo "ERROR: $f references Yojson.Safe.Util but is not allowlisted." >&2
    echo "       Decode through Onton_core.Json instead, or (last resort)" >&2
    echo "       add $f to scripts/yojson-util-allowlist.txt." >&2
    status=1
  fi
done

# 2. Every allowlisted entry must still exist and reference the pattern —
#    otherwise it is stale and should be pruned so the list ratchets to json.ml.
# shellcheck disable=SC2086 # allowlist paths are space-free
for f in $allowed; do
  if [ ! -f "$f" ]; then
    echo "ERROR: allowlist entry '$f' does not exist — remove it." >&2
    status=1
  elif ! grep -q "$PATTERN" "$f"; then
    echo "ERROR: allowlist entry '$f' no longer references Yojson.Safe.Util" >&2
    echo "       — remove it from scripts/yojson-util-allowlist.txt." >&2
    status=1
  fi
done

if [ "$status" -eq 0 ]; then
  echo "check-no-raw-yojson: OK"
fi
exit "$status"
