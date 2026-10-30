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

test("delivery paths stay inside the normalized artifact root", () => {
  for (const key of ["artifact_dir", "gameplan_path"]) {
    for (const path of ["/tmp/elsewhere", "/tmp/artifacts/../secret", "/tmp/artifacts-other/file"]) {
      assert.throws(() => decodeDelivery({ ...envelope, [key]: path }));
    }
  }
  assert.equal(decodeDelivery({ ...envelope, artifact_dir: "/tmp/artifacts/a/../b" }).artifact_dir, "/tmp/artifacts/b");
});

test("review responses require positive decimal IDs while findings keep slugs", () => {
  for (const id of ["abc", "0", "-1", "1.5", "1e3", "01", "42\n", "42_suffix"]) {
    assert.throws(() => responsePath("review_response", id));
  }
  assert.equal(responsePath("review_response", "4425138835"), "comment_responses/4425138835.md");
  assert.equal(responsePath("finding_wontfix", "service_finding-7.md"), "findings_wontfix/service_finding-7.md");
});

test("UTF-8 chunk iteration preserves text and byte offsets even with tiny limits", async () => {
  const root = await mkdtemp(join(tmpdir(), "pi-onton-utf8-"));
  try {
    const path = join(root, "text");
    for (const text of ["", "plain", "é漢😀tail", "aé漢😀z", "\uFEFFé😀"]) {
      await writeFile(path, text);
      for (let limit = 1; limit <= 12; limit++) {
        let offset = 0;
        let combined = "";
        for (;;) {
          const chunk = await readChunk(path, offset, limit);
          combined += chunk.text;
          assert.equal(chunk.next_offset, offset + Buffer.byteLength(chunk.text));
          assert.ok(chunk.next_offset - offset <= limit + 3);
          if (chunk.eof) {
            assert.equal(chunk.next_offset, Buffer.byteLength(text));
            break;
          }
          assert.ok(chunk.next_offset > offset);
          offset = chunk.next_offset;
        }
        assert.equal(combined, text);
      }
    }
    await writeFile(path, "😀");
    await assert.rejects(readChunk(path, 1, 4), /offset splits/);
    await writeFile(path, Buffer.from([0xff]));
    await assert.rejects(readChunk(path, 0, 4));
  } finally { await rm(root, { recursive: true, force: true }); }
});
