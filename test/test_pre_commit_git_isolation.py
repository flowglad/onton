"""Exercise the installed hook with repository-scoping variables like Git sets."""
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time


def run(args, **kwargs):
    result = subprocess.run(args, stdin=subprocess.DEVNULL,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                            text=True, timeout=20, **kwargs)
    if result.returncode != 0:
        raise AssertionError(
            f"Command {args!r} failed with exit status {result.returncode}\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}")
    return result


def main(installer):
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_")}
    with tempfile.TemporaryDirectory(prefix="onton-hook-isolation-") as directory:
        root = Path(directory)
        repo = root / "repo"
        run(["git", "init", "-q", str(repo)], env=env)
        scripts = repo / "scripts"
        scripts.mkdir()
        shutil.copyfile(installer, scripts / "install-hooks.sh")
        sync = scripts / "sync-skills.sh"
        sync.write_text("#!/bin/sh\nexit 0\n")
        sync.chmod(0o755)
        run(["sh", str(scripts / "install-hooks.sh")], cwd=repo, env=env)
        before = (repo / ".git/config").read_bytes()
        fake_bin = root / "bin"
        fake_bin.mkdir()
        for name, body in {
            "opam": 'shift 3\nexec "$@"\n',
            "dune": '''printf '%s\\n' "$@" > "$HOOK_TEST_ARGS"
case "${HOOK_TEST_MODE:-success}" in
  failure) exit 7 ;;
  timeout) (sleep 1; echo escaped > "$HOOK_TEST_MARKER") & wait ;;
  *) git -C "$HOOK_TEST_REMOTE" init -q --bare ;;
esac
''',
        }.items():
            path = fake_bin / name
            path.write_text("#!/bin/sh\nset -eu\n" + body)
            path.chmod(0o755)
        remote = root / "fixture.git"
        remote.mkdir()
        hook_env = dict(env, PATH=str(fake_bin) + os.pathsep + env["PATH"],
                        HOOK_TEST_REMOTE=str(remote), GIT_DIR=str(repo / ".git"),
                        GIT_INDEX_FILE=str(repo / ".git/index"),
                        HOOK_TEST_ARGS=str(root / "args"))
        hook = ["sh", str(repo / ".git/hooks/pre-commit")]
        run(hook, cwd=repo, env=hook_env)
        assert (root / "args").read_text().splitlines() == ["build", "@check", "@fmt"]
        assert (repo / ".git/config").read_bytes() == before, "hook changed parent Git config"
        assert run(["git", "-C", str(repo), "rev-parse", "--is-bare-repository"],
                   env=env).stdout.strip() == "false"
        assert run(["git", "--git-dir=" + str(remote), "rev-parse", "--is-bare-repository"],
                   env=env).stdout.strip() == "true"
        failure = subprocess.run(hook, cwd=repo, env=dict(hook_env, HOOK_TEST_MODE="failure"),
                                 stdin=subprocess.DEVNULL, capture_output=True, timeout=5)
        assert failure.returncode == 7, failure
        marker = root / "escaped"
        started = time.monotonic()
        expired = subprocess.run(hook, cwd=repo,
                                 env=dict(hook_env, HOOK_TEST_MODE="timeout",
                                          HOOK_TEST_MARKER=str(marker),
                                          ONTON_PRE_COMMIT_TIMEOUT_SECONDS="0.1"),
                                 stdin=subprocess.DEVNULL, capture_output=True, timeout=5)
        assert expired.returncode == 1, expired
        assert b"deadline exceeded" in expired.stderr, expired
        assert time.monotonic() - started < 5
        time.sleep(1.2)
        assert not marker.exists(), "timed-out check left a running descendant"
        print("pre-commit checks, Git isolation, failure status and bounded process cleanup: OK")


if __name__ == "__main__":
    main(Path(sys.argv[1]).resolve())
