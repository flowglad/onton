import { execFileSync } from "node:child_process";
import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdtemp, mkdir, open, readFile, realpath, readdir, rename, rm, stat, chmod, symlink, writeFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { pythonCommand } from "../src/validator.js";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { commitGameplan, readGameplanSnapshot, resolveGameplanWrite } from "../src/files.js";

test("writes use the canonical configured root and reject file symlinks, including dangling links", async () => {
  const dir = await mkdtemp(join(tmpdir(), "gameplan-path-"));
  try {
    await mkdir(join(dir, "actual"));
    await symlink(join(dir, "actual"), join(dir, "gameplans"));
    const path = (await resolveGameplanWrite(dir, "gameplans", "gameplans/new.yaml")).path;
    assert.equal(path, join(await realpath(join(dir, "actual")), "new.yaml"));
    await writeFile(path, "original");
    await writeFile(join(dir, "outside.yaml"), "outside");
    for (const [name, target] of [["escape", join(dir, "outside.yaml")], ["inside", path], ["dangling", join(dir, "missing.yaml")]]) {
      await symlink(target!, join(dir, "actual", `${name}.yaml`));
      await assert.rejects(resolveGameplanWrite(dir, "gameplans", `gameplans/${name}.yaml`), /non-regular/);
    }
    await symlink(dir, join(dir, "actual", "nested"));
    await assert.rejects(resolveGameplanWrite(dir, "gameplans", "gameplans/nested/outside.yaml"), /Refusing/);
    assert.equal(await readFile(join(dir, "outside.yaml"), "utf8"), "outside");
    await assert.rejects(resolveGameplanWrite(dir, "gameplans", "gameplans/new.json"), /Refusing/);
    assert.equal((await resolveGameplanWrite(dir, "gameplans", "gameplans/new.yaml")).path, path);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});


test("descriptor commits create files, replace complete contents and refuse concurrent changes", async () => {
  const dir = await mkdtemp(join(tmpdir(), "gameplan-commit-"));
  const path = join(dir, "plan.yaml");
  try {
    const admission = await resolveGameplanWrite(dir, ".", "plan.yaml");
    await commitGameplan(admission, "a long original document", undefined);
    assert.equal(await readFile(path, "utf8"), "a long original document");
    await commitGameplan(admission, "short", "a long original document");
    assert.equal(await readFile(path, "utf8"), "short");
    await assert.rejects(commitGameplan(admission, "overwrite", "stale"), /changed before commit/);
    await assert.rejects(commitGameplan(admission, "overwrite", undefined), /File exists/);
    assert.equal(await readFile(path, "utf8"), "short");
  } finally { await rm(dir, { recursive: true, force: true }); }
});


test("snapshots reject FIFOs, directories, symlinks, oversized files and paths outside the root", async () => {
  const dir = await mkdtemp(join(tmpdir(), "gameplan-snapshot-"));
  try {
    await mkdir(join(dir, "gameplans"));
    const fifo = join(dir, "gameplans", "pipe.yaml");
    execFileSync("mkfifo", [fifo], { timeout: 5000 });
    await mkdir(join(dir, "gameplans", "directory.yaml"));
    await writeFile(join(dir, "outside.yaml"), "outside");
    await symlink(join(dir, "outside.yaml"), join(dir, "gameplans", "link.yaml"));
    const large = await open(join(dir, "gameplans", "large.yaml"), "w");
    try { await large.truncate(16 * 1024 * 1024 + 1); } finally { await large.close(); }
    const rejectedSnapshots = [
      ["gameplans/pipe.yaml", /regular file/],
      ["gameplans/directory.yaml", /regular file/],
      ["gameplans/link.yaml", /regular file/],
      ["gameplans/large.yaml", /oversized/],
      ["outside.yaml", /Refusing gameplan snapshot/],
    ] as const;
    for (const [path, reason] of rejectedSnapshots) {
      await assert.rejects(readGameplanSnapshot(dir, "gameplans", path), reason);
    }
    const bytes = Buffer.from([0x80, 0xff, 0x00]);
    await writeFile(join(dir, "gameplans", "raw.yaml"), bytes);
    assert.deepEqual(await readGameplanSnapshot(dir, "gameplans", "gameplans/raw.yaml"), bytes);
  } finally { await rm(dir, { recursive: true, force: true }); }
});


test("atomic commits preserve the old document when replacement writes fail", async () => {
  const dir = await mkdtemp(join(tmpdir(), "gameplan-full-"));
  try {
    const path = join(dir, "plan.yaml");
    await writeFile(path, "original");
    const admission = await resolveGameplanWrite(dir, ".", "plan.yaml");
    const helper = fileURLToPath(new URL("../src/commit_gameplan.py", import.meta.url));
    const wrapper = join(dir, "limit child %.py");
    await writeFile(wrapper, "import resource, signal, runpy, sys\nresource.setrlimit(resource.RLIMIT_FSIZE, (1024, 1024))\nsignal.signal(signal.SIGXFSZ, signal.SIG_IGN)\nrunpy.run_path(sys.argv[1], run_name='__main__')\n");
    const [command = "python3", ...prefix] = pythonCommand();
    // Exercise the real helper with an OS-enforced file-size limit: a partial temporary
    // write raises EFBIG, just as a failed disk write would. No production fault hooks.
    assert.throws(() => execFileSync(command, [...prefix, wrapper, helper], {
      input: JSON.stringify({ ...admission, name: "plan.yaml", text: "x".repeat(8192), expected: Buffer.from("original").toString("base64") }),
      timeout: 120_000, stdio: ["pipe", "pipe", "pipe"],
    }), /File too large/);
    assert.equal(await readFile(path, "utf8"), "original");
    assert.deepEqual((await readdir(dir)).sort(), ["limit child %.py", "plan.yaml"]);
  } finally { await rm(dir, { recursive: true, force: true }); }
});

test("replacement of the admitted directory cannot redirect a commit", async () => {
  for (const replacement of ["symlink", "directory"]) {
    const dir = await mkdtemp(join(tmpdir(), "gameplan-parent-"));
    try {
      await mkdir(join(dir, "gameplans"));
      await mkdir(join(dir, "outside"));
      await writeFile(join(dir, "gameplans", "plan.yaml"), "original");
      await writeFile(join(dir, "outside", "plan.yaml"), "original");
      const admission = await resolveGameplanWrite(dir, "gameplans", "gameplans/plan.yaml");
      await rename(join(dir, "gameplans"), join(dir, "retained"));
      if (replacement === "symlink") await symlink(join(dir, "outside"), join(dir, "gameplans"));
      else {
        await mkdir(join(dir, "gameplans"));
        await writeFile(join(dir, "gameplans", "plan.yaml"), "original");
      }
      await assert.rejects(commitGameplan(admission, "changed", "original"), /Not a directory|Too many levels|directory changed/);
      assert.equal(await readFile(join(dir, "retained", "plan.yaml"), "utf8"), "original");
      assert.equal(await readFile(join(dir, "outside", "plan.yaml"), "utf8"), "original");
      assert.equal(await readFile(join(dir, "gameplans", "plan.yaml"), "utf8"), "original");
    } finally { await rm(dir, { recursive: true, force: true }); }
  }
});


test("new documents use the child umask and replacement preserves existing permissions", async () => {
  const dir = await mkdtemp(join(tmpdir(), "gameplan-mode-"));
  try {
    const helper = fileURLToPath(new URL("../src/commit_gameplan.py", import.meta.url));
    const wrapper = join(dir, "mode child %.py");
    await writeFile(wrapper, "import os, runpy, sys\nos.umask(int(sys.argv[2], 8))\nrunpy.run_path(sys.argv[1], run_name='__main__')\n");
    const [command = "python3", ...prefix] = pythonCommand();
    for (const [mask, mode] of [["022", 0o644], ["027", 0o640], ["077", 0o600]] as const) {
      const name = `plan-${mask}.yaml`;
      const admission = await resolveGameplanWrite(dir, ".", name);
      execFileSync(command, [...prefix, wrapper, helper, mask], {
        input: JSON.stringify({ ...admission, name, text: "original", expected: null }),
        timeout: 120_000, stdio: ["pipe", "pipe", "pipe"],
      });
      assert.equal((await stat(admission.path)).mode & 0o777, mode);
      await chmod(admission.path, 0o660);
      await commitGameplan(admission, "updated", "original");
      assert.equal((await stat(admission.path)).mode & 0o777, 0o660);
    }
  } finally { await rm(dir, { recursive: true, force: true }); }
});


test("staging symlink substitution cannot chmod an outside file", async () => {
  for (const existing of [false, true]) {
    const dir = await mkdtemp(join(tmpdir(), "gameplan-staging-"));
    try {
      const name = "plan.yaml";
      if (existing) await writeFile(join(dir, name), "original");
      const outside = join(dir, "outside");
      await writeFile(outside, "outside");
      await chmod(outside, 0o600);
      const admission = await resolveGameplanWrite(dir, ".", name);
      const helper = fileURLToPath(new URL("../src/commit_gameplan.py", import.meta.url));
      const wrapper = join(dir, "swap.py");
      await writeFile(wrapper, `import os, runpy, sys
original_fsync = os.fsync
def swap(fd):
    original_fsync(fd)
    for name in os.listdir(sys.argv[2]):
        if name.startswith('.gameplan-'):
            path = os.path.join(sys.argv[2], name)
            os.unlink(path)
            os.symlink(sys.argv[3], path)
os.fsync = swap
runpy.run_path(sys.argv[1], run_name='__main__')
`);
      const [command = "python3", ...prefix] = pythonCommand();
      assert.throws(() => execFileSync(command, [...prefix, wrapper, helper, dir, outside], {
        input: JSON.stringify({ ...admission, name, text: "replacement", expected: existing ? Buffer.from("original").toString("base64") : null }),
        timeout: 120_000, stdio: ["pipe", "pipe", "pipe"],
      }), /staging file changed/);
      assert.equal((await stat(outside)).mode & 0o777, 0o600);
      assert.equal(await readFile(outside, "utf8"), "outside");
      if (existing) assert.equal(await readFile(join(dir, name), "utf8"), "original");
      else await assert.rejects(stat(join(dir, name)), { code: "ENOENT" });
    } finally { await rm(dir, { recursive: true, force: true }); }
  }
});


test("overlapping commits serialize validation and publication", async () => {
  const dir = await mkdtemp(join(tmpdir(), "gameplan-concurrent-"));
  try {
    const path = join(dir, "plan.yaml");
    await writeFile(path, "original");
    const admission = await resolveGameplanWrite(dir, ".", "plan.yaml");
    const helper = fileURLToPath(new URL("../src/commit_gameplan.py", import.meta.url));
    const harness = join(dir, "concurrent.py");
    await writeFile(harness, `import fcntl, json, multiprocessing, os, runpy, sys
request = json.load(sys.stdin)
commit = runpy.run_path(sys.argv[1])['commit']
context = multiprocessing.get_context('fork')
def worker(connection, first):
    try:
        if first:
            replace = os.replace
            def paused_replace(*args, **kwargs):
                connection.send('ready-to-publish')
                assert connection.recv() == 'publish'
                return replace(*args, **kwargs)
            os.replace = paused_replace
        else:
            flock = fcntl.flock
            def observed_lock(*args):
                connection.send('attempting-lock')
                result = flock(*args)
                connection.send('acquired-lock')
                return result
            fcntl.flock = observed_lock
        commit({**request, 'text': 'first' if first else 'second'})
        connection.send('committed')
    except Exception as error:
        connection.send(str(error))
def receive(connection):
    assert connection.poll(10), 'worker timed out'
    return connection.recv()
a, child_a = context.Pipe()
b, child_b = context.Pipe()
first = context.Process(target=worker, args=(child_a, True))
second = context.Process(target=worker, args=(child_b, False))
try:
    first.start()
    assert receive(a) == 'ready-to-publish'
    # Prove a real lock is held after the old content/inode checks and before rename.
    descriptor = os.open(request['root'], os.O_RDONLY | os.O_DIRECTORY)
    try:
        try:
            fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            pass
        else:
            raise AssertionError('publication window is not locked')
    finally:
        os.close(descriptor)
    second.start()
    assert receive(b) == 'attempting-lock'
    a.send('publish')
    assert receive(a) == 'committed'
    assert receive(b) == 'acquired-lock'
    assert 'changed before commit' in receive(b)
finally:
    for process in (first, second):
        if process.pid:
            process.join(5)
            if process.is_alive():
                process.terminate()
                process.join()
`);
    const [command = "python3", ...prefix] = pythonCommand();
    execFileSync(command, [...prefix, harness, helper], {
      input: JSON.stringify({ ...admission, name: "plan.yaml", expected: Buffer.from("original").toString("base64") }),
      timeout: 120_000, stdio: ["pipe", "pipe", "pipe"],
    });
    assert.equal(await readFile(path, "utf8"), "first");
  } finally { await rm(dir, { recursive: true, force: true }); }
});
