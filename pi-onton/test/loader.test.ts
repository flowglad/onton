import { test } from "node:test";
import assert from "node:assert/strict";
import { copyFile, mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { loadExtensions } from "../node_modules/@earendil-works/pi-coding-agent/dist/core/extensions/loader.js";

test("Pi loads the bundled sources outside the checkout without node_modules", async () => {
  const directory = await mkdtemp(join(tmpdir(), "onton-bundled-extension-"));
  const previous = process.env.ONTON_PI_CONTEXT;
  process.env.ONTON_PI_CONTEXT = join(directory, "context.json");
  try {
    for (const name of ["index.ts", "protocol.ts", "files.ts"]) {
      await copyFile(new URL(`../src/${name}`, import.meta.url), join(directory, name));
    }
    const loaded = await loadExtensions([join(directory, "index.ts")], directory);
    assert.deepEqual(loaded.errors, []);
    assert.equal(loaded.extensions.length, 1);
    assert.deepEqual([...loaded.extensions[0]!.tools.keys()].sort(), ["onton_context", "onton_read_artifact", "onton_respond"]);
  } finally {
    if (previous === undefined) delete process.env.ONTON_PI_CONTEXT;
    else process.env.ONTON_PI_CONTEXT = previous;
    await rm(directory, { recursive: true, force: true });
  }
});
