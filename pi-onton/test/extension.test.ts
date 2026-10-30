import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import extension from "../src/index.js";

test("supervised lifecycle and tools follow a new execution on conversation resume", async () => {
  const root = await mkdtemp(join(tmpdir(), "pi-onton-host-"));
  const previous = process.env.ONTON_PI_CONTEXT;
  try {
    const context = join(root, "context.json");
    process.env.ONTON_PI_CONTEXT = context;
    const patch = join(root, "patch-1");
    await mkdir(patch);
    const gameplan = join(root, "gameplan.json");
    await writeFile(gameplan, '{"patches":[]}');
    const setDelivery = (prompt: string, session_uuid: string) => writeFile(context, JSON.stringify({ version: 1, project: "project", patch_id: "patch-1", session_uuid, prompt, artifacts_root: root, artifact_dir: patch, gameplan_path: gameplan }));
    await setDelivery("Implement the patch", "execution-1");
    const hooks = new Map<string, (...args: any[]) => any>();
    const tools = new Map<string, any>();
    const messages: unknown[] = [];
    extension({ on: (name: string, hook: any) => hooks.set(name, hook), registerTool: (tool: any) => tools.set(tool.name, tool), sendMessage: (message: unknown) => messages.push(message) } as unknown as ExtensionAPI);
    await hooks.get("session_start")!({}, { hasUI: false });
    const start = await hooks.get("before_agent_start")!();
    assert.match(start.message.content, /execution-1/);
    assert.equal((await tools.get("onton_context").execute()).details.prompt, "Implement the patch");
    for (const prompt of ["Address review comment 42", "Fix CI", "Human: explain the design", "Recover the captured Git operation", "Update publication metadata", "Base branch changed", "Clean up uncommitted changes", "Address these findings"]) {
      await setDelivery(prompt, "execution-2");
      assert.equal((await tools.get("onton_context").execute()).details.prompt, prompt);
    }
    await hooks.get("session_compact")!();
    assert.equal(messages.length, 1);
    assert.equal((await tools.get("onton_read_artifact").execute("call", { source: "gameplan" })).details.text, '{"patches":[]}');
    for (const [kind, id, path] of [
      ["implementation_notes", undefined, "pr-body.md"],
      ["review_response", "42", "comment_responses/42.md"],
      ["finding_wontfix", "svc_f1.md", "findings_wontfix/svc_f1.md"],
      ["opt_out", undefined, "WONTDO.md"],
    ]) {
      await tools.get("onton_respond").execute("call", { kind, id, content: "Response text" });
      assert.equal(await readFile(join(patch, path!), "utf8"), "Response text");
    }
    const cancelled = new AbortController();
    cancelled.abort();
    await assert.rejects(tools.get("onton_respond").execute("call", { kind: "opt_out", content: "cancelled" }, cancelled.signal));
    assert.equal(await readFile(join(patch, "WONTDO.md"), "utf8"), "Response text");
  } finally {
    if (previous === undefined) delete process.env.ONTON_PI_CONTEXT;
    else process.env.ONTON_PI_CONTEXT = previous;
    await rm(root, { recursive: true, force: true });
  }
});

test("globally installed mode is inert outside Onton", () => {
  const previous = process.env.ONTON_PI_CONTEXT;
  delete process.env.ONTON_PI_CONTEXT;
  try { extension({} as ExtensionAPI); }
  finally { if (previous !== undefined) process.env.ONTON_PI_CONTEXT = previous; }
});
