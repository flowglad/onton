#!/usr/bin/env python3
"""Atomic descriptor-relative gameplan publication. Input is one JSON request on stdin."""
import base64
import json
import os
import secrets
import stat
import sys


def commit(request):
    name = request["name"]
    if not name or name in (".", "..") or os.path.basename(name) != name:
        raise ValueError("Invalid gameplan filename")
    root = os.open(request["root"], os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    temporary = None
    try:
        identity = os.fstat(root)
        if (str(identity.st_dev), str(identity.st_ino)) != (request["device"], request["inode"]):
            raise ValueError("Admitted gameplan directory changed before commit")
        temporary = ".gameplan-" + secrets.token_hex(16)
        fd = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600, dir_fd=root)
        with os.fdopen(fd, "wb") as replacement:
            replacement.write(request["text"].encode("utf-8"))
            replacement.flush()
            os.fsync(replacement.fileno())
        expected = request["expected"]
        if expected is None:
            # Keep staging private, then publish with normal creation permissions.
            mask = os.umask(0)
            os.umask(mask)
            os.chmod(temporary, 0o666 & ~mask, dir_fd=root)
            # Atomic no-clobber publication for a new file.
            os.link(temporary, name, src_dir_fd=root, dst_dir_fd=root, follow_symlinks=False)
        else:
            old = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=root)
            with os.fdopen(old, "rb") as previous:
                previous_stat = os.fstat(previous.fileno())
                if not stat.S_ISREG(previous_stat.st_mode):
                    raise ValueError("Gameplan commit requires a regular file")
                if previous.read() != base64.b64decode(expected, validate=True):
                    raise ValueError("Gameplan changed before commit; retry the operation")
                os.chmod(temporary, stat.S_IMODE(previous_stat.st_mode), dir_fd=root)
                # Replacing a leaf symlink would not follow it, but refuse changed leaves.
                current = os.stat(name, dir_fd=root, follow_symlinks=False)
                if (current.st_dev, current.st_ino) != (previous_stat.st_dev, previous_stat.st_ino):
                    raise ValueError("Gameplan changed before commit; retry the operation")
                os.replace(temporary, name, src_dir_fd=root, dst_dir_fd=root)
                temporary = None
    finally:
        if temporary is not None:
            os.unlink(temporary, dir_fd=root)
        os.close(root)


if __name__ == "__main__":
    try:
        commit(json.load(sys.stdin))
    except Exception as error:
        print(f"{type(error).__name__}: {error}", file=sys.stderr)
        sys.exit(1)
