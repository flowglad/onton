"""Exercise the installed hook with repository-scoping variables like Git sets."""
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


def run(args, **kwargs):
    return subprocess.run(args, check=True, stdin=subprocess.DEVNULL,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                          text=True, timeout=20, **kwargs)


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
            "opam": "exit 0\n",
            "dune": 'git -C "$HOOK_TEST_REMOTE" init -q --bare\n',
        }.items():
            path = fake_bin / name
            path.write_text("#!/bin/sh\nset -eu\n" + body)
            path.chmod(0o755)
        remote = root / "fixture.git"
        remote.mkdir()
        hook_env = dict(env, PATH=str(fake_bin) + os.pathsep + env["PATH"],
                        HOOK_TEST_REMOTE=str(remote), GIT_DIR=str(repo / ".git"),
                        GIT_INDEX_FILE=str(repo / ".git/index"))
        run(["sh", str(repo / ".git/hooks/pre-commit")], cwd=repo, env=hook_env)
        assert (repo / ".git/config").read_bytes() == before, "hook changed parent Git config"
        assert run(["git", "-C", str(repo), "rev-parse", "--is-bare-repository"],
                   env=env).stdout.strip() == "false"
        assert run(["git", "--git-dir=" + str(remote), "rev-parse", "--is-bare-repository"],
                   env=env).stdout.strip() == "true"
        print("pre-commit fixture Git isolation: OK")


if __name__ == "__main__":
    main(Path(sys.argv[1]).resolve())
