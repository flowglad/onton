import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { decodeDelivery, responsePath } from "../src/protocol.js";
import { artifactPath, readChunk, writeResponse } from "../src/files.js";

const envelope = { version: 1, project: "p", patch_id: "a", session_uuid: "s", prompt: "current human guidance", artifacts_root: "/tmp/artifacts", artifact_dir: "/tmp/artifacts/a", gameplan_path: "/tmp/artifacts/gameplan.json" };
test("delivery validates identity, schema and absolute supervisor paths", () => {
  assert.deepEqual(decodeDelivery(envelope), envelope);
  for (const value of [null, [], 1, "bad", {}, { ...envelope, version: 2 }, { ...envelope, artifact_dir: "relative" }, { ...envelope, prompt: "" }]) {
    assert.throws(() => decodeDelivery(value));
  }
});
test("responses use the supervisor's existing artifact names", () => {
  assert.equal(responsePath("implementation_notes"), "pr-body.md");
  assert.equal(responsePath("opt_out"), "WONTDO.md");
  assert.equal(responsePath("review_response", "123"), "comment_responses/123.md");
  assert.equal(responsePath("finding_wontfix", "svc_a.b_1_x.md"), "findings_wontfix/svc_a.b_1_x.md");
  for (const id of [undefined, "", "../../secret", "/tmp/a", "a/b", "a\n"]) {
    assert.throws(() => responsePath("review_response", id));
    assert.throws(() => responsePath("finding_wontfix", id));
  }
});
test("artifact writes are complete, isolated and readable by the supervisor", async () => {
  const root = await mkdtemp(join(tmpdir(), "pi-onton-"));
  try {
    const patch = join(root, "patch");
    await mkdir(patch);
    const path = await writeResponse(patch, responsePath("review_response", "42"), "Fixed and tested.");
    assert.equal(await readFile(path, "utf8"), "Fixed and tested.");
    await writeResponse(patch, responsePath("review_response", "42"), "Updated.");
    assert.equal(await readFile(path, "utf8"), "Updated.");
    assert.deepEqual(await readChunk(path, 0, 4), { text: "Upda", next_offset: 4, eof: false });
    assert.deepEqual(await readChunk(path, 4, 64), { text: "ted.", next_offset: 8, eof: true });
    await assert.rejects(writeResponse(patch, "../escape", "bad"));
    await assert.rejects(writeResponse(patch, "WONTDO.md", "  "));
    await writeFile(join(root, "secret"), "secret");
    await symlink(root, join(patch, "link"));
    await assert.rejects(artifactPath(patch, "link/secret"));
    await assert.rejects(artifactPath(patch, "../secret"));
    await assert.rejects(writeResponse(patch, "link/secret", "bad"));
    assert.equal(await readFile(join(root, "secret"), "utf8"), "secret");
  } finally { await rm(root, { recursive: true, force: true }); }
});
