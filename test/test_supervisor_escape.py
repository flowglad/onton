"""Exercise the production Linux subreaper, including checkout guard ordering."""
import fcntl
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time


def wait_until(predicate):
    deadline = time.monotonic() + 10
    while not predicate():
        if time.monotonic() > deadline:
            raise AssertionError("timed out waiting for supervisor")
        time.sleep(0.005)


def descendant(root, depth, escape):
    if escape == "session":
        os.setsid()
    else:
        os.setpgid(0, 0)
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    (root / f"pid-{depth}").write_text(str(os.getpid()))
    if depth:
        if os.fork() == 0:
            descendant(root, depth - 1, escape)
        # Leave nested live descendants that will be adopted only after this
        # process dies. They all escape the original group independently.
    while True:
        signal.pause()


def leader(root, escape, code):
    if os.fork() == 0:
        descendant(root, 3, escape)
    wait_until(lambda: (root / "release-leader").exists())
    sys.exit(code)


def scenario(shim, escape, mode):
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        guard = root / "guard"
        streaming = mode == "stream"
        args = (["--supervise-stream-parent", str(os.getpid())]
                if streaming else ["--supervise"])
        process = subprocess.Popen(
            [shim, *args, "--command-guards", "1", str(guard),
             sys.executable, __file__, "leader", str(root), escape,
             "23" if mode == "failure" else "0"],
            stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        pids = []
        try:
            wait_until(lambda: len(list(root.glob("pid-*"))) == 4
                       and all(p.stat().st_size for p in root.glob("pid-*")))
            pids = [int(p.read_text()) for p in root.glob("pid-*")]
            with guard.open("r+") as handle:
                try:
                    fcntl.lockf(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
                except BlockingIOError:
                    pass
                else:
                    raise AssertionError("checkout guard released with live descendants")
                if mode == "cancel":
                    process.send_signal(signal.SIGTERM)
                else:
                    (root / "release-leader").touch()
                # Test the actual release boundary, not just eventual cleanup.
                def released():
                    try:
                        fcntl.lockf(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
                        return True
                    except BlockingIOError:
                        return False
                wait_until(released)
                assert all(not Path(f"/proc/{pid}").exists() for pid in pids), \
                    "adopted descendant survived checkout guard release"
            _, stderr = process.communicate(timeout=10)
            assert not stderr, stderr
            assert process.returncode == ({"failure": 23, "cancel": -signal.SIGTERM}
                                          .get(mode, 0)), process.returncode
        finally:
            for pid in pids:
                try:
                    os.kill(pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            if process.poll() is None:
                process.kill()
            process.wait()


if __name__ == "__main__":
    if sys.argv[1] == "leader":
        leader(Path(sys.argv[2]), sys.argv[3], int(sys.argv[4]))
    elif sys.platform != "linux":
        print("escaped descendant ownership: Linux-only (skipped)")
    else:
        for escape in ("session", "group"):
            for mode in ("success", "failure", "cancel", "stream"):
                scenario(sys.argv[1], escape, mode)
        print("escaped descendant ownership and guard release: 8 scenarios OK")
