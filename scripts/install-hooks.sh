#!/bin/sh
# install-hooks — install onton's git hooks into the repo's hooks dir
#
# Currently installs:
#   - post-checkout: runs sync-skills.sh after branch switches so ~/.agents/skills
#     always points at the current checkout
#   - pre-commit: bounded type-check and format check against the project's
#     local opam switch; the full build and test suite run in CI
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
  *) HOOKS_DIR="$REPO_ROOT/$HOOKS_DIR" ;;
esac

mkdir -p "$HOOKS_DIR"

# Detect onton-installed hooks via a sentinel so renaming the body doesn't
# break re-installation, and a third-party hook that merely mentions onton
# in a comment isn't mistaken for ours.
check_hook_writable() {
  hook_name="$1"
  sentinel="$2"
  hook_path="$HOOKS_DIR/$hook_name"

  if [ -e "$hook_path" ] && ! grep -q "$sentinel" "$hook_path"; then
    echo "error: $hook_path already exists and is not onton's hook." >&2
    return 1
  fi
}

# Validate all hook paths up front so a conflict on the second hook doesn't
# leave the first one already written to disk in a partial-install state.
conflicts=0
check_hook_writable post-checkout 'onton-managed-post-checkout' || conflicts=1
check_hook_writable pre-commit 'onton-managed-pre-commit' || conflicts=1
if [ "$conflicts" -ne 0 ]; then
  echo "       Back up the conflicting file(s) and remove them, then re-run install-hooks.sh." >&2
  echo "       The desired hook bodies are inlined in scripts/install-hooks.sh." >&2
  exit 1
fi

# install_hook <hook_name> reads the hook body from stdin.
install_hook() {
  hook_name="$1"
  hook_path="$HOOKS_DIR/$hook_name"
  cat >"$hook_path"
  chmod +x "$hook_path"
  echo "[installed] $hook_name hook at $hook_path"
}

# Single-quoted heredoc delimiters so $vars are resolved at hook runtime,
# not baked in at install time (the repo may later be moved or cloned elsewhere).

install_hook post-checkout <<'HOOK'
#!/bin/sh
# onton-managed-post-checkout — do not edit this line; install-hooks.sh uses it
# to detect that this hook is owned by onton and may be safely overwritten.
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || exit 0
[ -x "$REPO_ROOT/scripts/sync-skills.sh" ] && "$REPO_ROOT/scripts/sync-skills.sh"
HOOK

install_hook pre-commit <<'HOOK'
#!/bin/sh
# onton-managed-pre-commit — do not edit this line; install-hooks.sh uses it
# to detect that this hook is owned by onton and may be safely overwritten.
set -e

# Activate the project's local opam switch (5.5.1) rather than whatever the
# user's default switch happens to be. git-common-dir resolves to the main
# repo's .git from any worktree; its parent is the project root that holds _opam.
PROJECT_ROOT="$(dirname "$(git rev-parse --path-format=absolute --git-common-dir)")"

# Hook Git variables describe the committing repository. Build/test subprocesses
# operate on their own repositories; inheriting these can make a fixture's
# `git init --bare` reinitialize this checkout instead.
for name in $(git rev-parse --local-env-vars); do
  unset "$name"
done

# Leave five seconds for hook startup and cleanup within the 90-second budget.
# A shorter local deadline is allowed; it cannot increase the cap.
exec python3 - "$PROJECT_ROOT" <<'PY'
import math
import os
import signal
import subprocess
import sys
import time

try:
    timeout = float(os.environ.get("ONTON_PRE_COMMIT_TIMEOUT_SECONDS", "85"))
    if not math.isfinite(timeout) or timeout <= 0:
        raise ValueError()
except ValueError:
    sys.exit("Invalid ONTON_PRE_COMMIT_TIMEOUT_SECONDS: expected positive seconds")
timeout = min(timeout, 85)
deadline = time.monotonic() + timeout
print("==> dune build @check @fmt (90-second pre-commit budget)", flush=True)
process = subprocess.Popen(
    ["opam", "exec", "--switch=" + sys.argv[1], "--", "dune", "build", "@check", "@fmt"],
    stdin=subprocess.DEVNULL, start_new_session=True)
expired = False
try:
    status = process.wait(timeout=max(0, deadline - time.monotonic()))
except subprocess.TimeoutExpired:
    expired = True
    print("Pre-commit deadline exceeded; full validation runs in CI.", file=sys.stderr)
    status = 1
finally:
    if expired or process.poll() is None:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.wait()
sys.exit(status if status >= 0 else 1)
PY
HOOK

"$SCRIPT_DIR/sync-skills.sh"
