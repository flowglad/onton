import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { ExtensionRunner, SessionManager, type ModelRegistry } from "@earendil-works/pi-coding-agent";
import { loadExtensions } from "../node_modules/@earendil-works/pi-coding-agent/dist/core/extensions/loader.js";

test("Onton composes with another extension in either load order", async () => {
  const directory = await mkdtemp(join(tmpdir(), "onton-coexistence-"));
  const previous = process.env.ONTON_PI_CONTEXT;
  process.env.ONTON_PI_CONTEXT = join(directory, "context.json");
  try {
    await writeFile(process.env.ONTON_PI_CONTEXT, JSON.stringify({ version: 1, project: "p", patch_id: "patch-1", session_uuid: "s", prompt: "Implement the patch", artifacts_root: directory, artifact_dir: directory, gameplan_path: join(directory, "gameplan.json") }));
    const sibling = join(directory, "sibling.ts");
    await writeFile(sibling, `
      import { Type } from "typebox";
      export default function (pi) {
        let starts = 0;
        let switches = 0;
        pi.on("before_agent_start", () => {
          starts++;
          return { systemPrompt: "Sibling extension system prompt", message: { customType: "sibling", content: "Sibling guidance", display: false } };
        });
        pi.on("session_before_switch", () => { switches++; });
        pi.on("tool_call", (event) => {
          if (event.toolName === "sibling_guarded") return { block: true, reason: "Sibling policy" };
        });
        pi.registerTool({ name: "sibling_tool", label: "Sibling tool", description: "Another extension's tool", parameters: Type.Object({}),
          async execute() { return { content: [{ type: "text", text: "Sibling ran" }], details: { starts, switches } }; }
        });
      }
    `);
    const onton = fileURLToPath(new URL("../src/index.ts", import.meta.url));
    for (const paths of [[onton, sibling], [sibling, onton]]) {
      const loaded = await loadExtensions(paths, directory);
      assert.deepEqual(loaded.errors, []);
      // This fixture makes no model calls; the event runner never uses its registry.
      const runner = new ExtensionRunner(loaded.extensions, loaded.runtime, directory, SessionManager.inMemory(directory), {} as ModelRegistry);
      const errors: unknown[] = [];
      runner.onError((error) => errors.push(error));
      await runner.emit({ type: "session_start", reason: "startup" });
      const tools = ["read", "write", "edit", "bash", "sibling_tool", "onton_context", "onton_read_artifact", "onton_respond"];
      const start = await runner.emitBeforeAgentStart({ messageType: "user", prompt: "Implement the patch" }, { cwd: directory, selectedTools: tools });
      assert.equal(start.systemPromptOptions.forceSystemPrompt, "Sibling extension system prompt");
      assert.deepEqual(start.systemPromptOptions.selectedTools, tools);
      assert.deepEqual(start.messages.map((message) => message.customType).sort(), ["onton-mode", "sibling"]);
      assert.equal(await runner.emitToolCall({ type: "tool_call", toolCallId: "write", toolName: "write", input: { path: "scratch.txt", content: "ordinary coding work" } }), undefined);
      assert.equal(await runner.emitToolCall({ type: "tool_call", toolCallId: "sibling", toolName: "sibling_tool", input: {} }), undefined);
      assert.deepEqual(await runner.emitToolCall({ type: "tool_call", toolCallId: "guarded", toolName: "sibling_guarded", input: {} }), { block: true, reason: "Sibling policy" });
      assert.equal(await runner.emit({ type: "session_before_switch", reason: "new" }), undefined);
      const tool = runner.getToolDefinition("sibling_tool")!;
      const result = await tool.execute("call", {}, undefined, undefined, runner.createToolContext("call", undefined));
      assert.deepEqual(result.details, { starts: 1, switches: 1 });
      assert.deepEqual(errors, []);
    }
  } finally {
    if (previous === undefined) delete process.env.ONTON_PI_CONTEXT;
    else process.env.ONTON_PI_CONTEXT = previous;
    await rm(directory, { recursive: true, force: true });
  }
});
