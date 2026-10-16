import { execFileSync } from "node:child_process";
import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdtemp, mkdir, open, readFile, realpath, readdir, rename, rm, symlink, writeFile } from "node:fs/promises";
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
    const helper = new URL("../src/commit_gameplan.py", import.meta.url).pathname;
    // Exercise the real helper with an OS-enforced file-size limit: a partial temporary
    // write raises EFBIG, just as a failed disk write would. No production fault hooks.
    assert.throws(() => execFileSync("python3", ["-c",
      "import resource, signal, runpy, sys; resource.setrlimit(resource.RLIMIT_FSIZE, (1024, 1024)); signal.signal(signal.SIGXFSZ, signal.SIG_IGN); runpy.run_path(sys.argv[1], run_name='__main__')", helper], {
      input: JSON.stringify({ ...admission, name: "plan.yaml", text: "x".repeat(8192), expected: Buffer.from("original").toString("base64") }),
      timeout: 5000, stdio: ["pipe", "pipe", "pipe"],
    }), /File too large/);
    assert.equal(await readFile(path, "utf8"), "original");
    assert.deepEqual(await readdir(dir), ["plan.yaml"]);
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
