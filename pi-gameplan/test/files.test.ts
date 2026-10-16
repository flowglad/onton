import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdtemp, mkdir, readFile, realpath, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { commitGameplan, resolveGameplanWrite } from "../src/files.js";

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
