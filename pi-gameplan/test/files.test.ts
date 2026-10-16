import { execFileSync } from "node:child_process";
import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdtemp, mkdir, open, readFile, realpath, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { commitGameplan, readGameplanSnapshot, resolveGameplanWrite } from "../src/files.js";

test("writes use the canonical configured root and reject file symlinks, including dangling links", async () => {
  const dir = await mkdtemp(join(tmpdir(), "gameplan-path-"));
  try {
    await mkdir(join(dir, "actual"));
    await symlink(join(dir, "actual"), join(dir, "gameplans"));
    const path = await resolveGameplanWrite(dir, "gameplans", "gameplans/new.yaml");
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
    assert.equal(await resolveGameplanWrite(dir, "gameplans", "gameplans/new.yaml"), path);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});


test("descriptor commits create files, replace complete contents and refuse concurrent changes", async () => {
  const dir = await mkdtemp(join(tmpdir(), "gameplan-commit-"));
  const path = join(dir, "plan.yaml");
  try {
    await commitGameplan(path, "a long original document", undefined);
    assert.equal(await readFile(path, "utf8"), "a long original document");
    await commitGameplan(path, "short", "a long original document");
    assert.equal(await readFile(path, "utf8"), "short");
    await assert.rejects(commitGameplan(path, "overwrite", "stale"), /changed before commit/);
    await assert.rejects(commitGameplan(path, "overwrite", undefined), /EEXIST/);
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
    for (const path of ["gameplans/pipe.yaml", "gameplans/directory.yaml", "gameplans/link.yaml", "gameplans/large.yaml", "outside.yaml"]) {
      await assert.rejects(readGameplanSnapshot(dir, "gameplans", path));
    }
    const bytes = Buffer.from([0x80, 0xff, 0x00]);
    await writeFile(join(dir, "gameplans", "raw.yaml"), bytes);
    assert.deepEqual(await readGameplanSnapshot(dir, "gameplans", "gameplans/raw.yaml"), bytes);
  } finally { await rm(dir, { recursive: true, force: true }); }
});
