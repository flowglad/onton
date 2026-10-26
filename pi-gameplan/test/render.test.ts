import assert from "node:assert/strict";
import { test } from "node:test";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import gameplan from "../src/index.js";

test("spec results preserve diagnostics and allow expanded success details", () => {
  type Tool = Parameters<ExtensionAPI["registerTool"]>[0];
  const tools = new Map<string, Tool>();
  gameplan({
    registerTool: (tool: Tool) => tools.set(tool.name, tool),
    registerFlag: () => {},
    registerCommand: () => {},
    on: () => {},
  } as unknown as ExtensionAPI);
  const render = tools.get("gameplan_write_spec")!.renderResult!;
  const theme = { fg: (_color: string, text: string) => text } as Parameters<typeof render>[2];
  const context = {} as Parameters<typeof render>[3];
  const messages = [
    "Nothing written; pant rejected the spec:\nline 61:0: Duplicate rule 'retired?'\n    retired? s: Sleep => Bool.",
    "Nothing written; pant --check failed:\nFAIL: Invariant not satisfied in initial state\n  Before:\n    registered? Suite_0 = false",
  ];
  for (const text of messages) {
    for (const expanded of [false, true]) {
      const component = render(
        { content: [{ type: "text", text }], details: {}, isError: true },
        { expanded, isPartial: false }, theme, context,
      );
      assert.equal(component.render(200).map((line) => line.trimEnd()).join("\n"), text);
    }
  }
  const text = "Spec parsed and written\nSMT check passed.";
  for (const expanded of [false, true]) {
    const component = render(
      { content: [{ type: "text", text }], details: {}, isError: false },
      { expanded, isPartial: false }, theme, context,
    );
    assert.equal(component.render(200).map((line) => line.trimEnd()).join("\n"), expanded ? text : text.split("\n")[0]);
  }
});
