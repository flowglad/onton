import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdtemp, mkdir, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, dirname, join } from "node:path";
import { parse, stringify } from "yaml";
import type { ExtensionAPI, ExtensionToolContext } from "@earendil-works/pi-coding-agent";
import gameplan from "../src/index.js";
import { resolveSkillPaths } from "../src/validator.js";

// Drive the registered tools with a host adapter. Files and documents are real; validator
// success is controlled so these cases isolate confirmation and path ownership behavior.
async function fixture(run: (f: {
  call: (name: string, params: unknown) => Promise<unknown>;
  ctx: ExtensionToolContext;
  path: string;
  saved: { gameplanPath?: string; finalizedPath?: string }[];
  restore: (data: unknown) => Promise<unknown>;
  emit: (event: string, input?: unknown) => Promise<unknown>;
  onConfirm: (fn: (index: number) => Promise<boolean>) => void;
  onExec: (fn: (command: string, args: string[]) => Promise<void>) => void;
}) => Promise<void>) {
  const dir = await mkdtemp(join(tmpdir(), "gameplan-host-"));
  const path = join(dir, "gameplans", "example.yaml");
  await mkdir(dirname(path));
  const example = parse(await readFile(join(resolveSkillPaths().dir, "references/example.yaml"), "utf8"));
  example.architectureDesign.decisions = [];
  example.openQuestions = [];
  await writeFile(path, stringify(example));
  const tools = new Map<string, { execute: (...args: any[]) => Promise<unknown> }>();
  const events = new Map<string, (...args: any[]) => unknown>();
  const saved: { gameplanPath?: string; finalizedPath?: string }[] = [];
  let confirm = async (_index: number) => true;
  let exec = async (_command: string, _args: string[]) => {};
  let dialogs = 0;
  let entries: unknown[] = [];
  const api = {
    registerTool: (tool: { name: string; execute: (...args: any[]) => Promise<unknown> }) => tools.set(tool.name, tool),
    registerFlag: () => {}, registerCommand: () => {},
    on: (event: string, handler: (...args: any[]) => unknown) => events.set(event, handler),
    getFlag: () => true, getActiveTools: () => [], getAllTools: () => [], setActiveTools: () => {},
    appendEntry: (_type: string, state: object) => saved.push({ ...state }),
    exec: async (command: string, args: string[]) => {
      await exec(command, args);
      return { code: 0, killed: false, stdout: command === "pant" && args.includes("--json") ? '{"module":"TEST"}' : "PASS", stderr: "" };
    },
  } as unknown as ExtensionAPI;
  const ctx = {
    cwd: dir, hasUI: true,
    sessionManager: { getBranch: () => entries },
    ui: { notify: () => {}, setStatus: () => {}, theme: { fg: (_color: string, value: string) => value },
      confirm: () => confirm(++dialogs) },
  } as unknown as ExtensionToolContext;
  try {
    gameplan(api);
    await events.get("session_start")!({}, ctx);
    await run({
      call: (name, params) => tools.get(name)!.execute("test", params, undefined, undefined, ctx),
      ctx, path, saved,
      restore: async (data) => {
        entries = [{ type: "custom", customType: "gameplan-mode", data }];
        return events.get("session_start")!({}, ctx);
      },
      emit: async (event, input = {}) => events.get(event)?.(input, ctx),
      onConfirm: (fn) => { confirm = fn; }, onExec: (fn) => { exec = fn; },
    });
  } finally { await rm(dir, { recursive: true, force: true }); }
}

test("refused writes do not replace the session's remembered gameplan", async () => {
  await fixture(async ({ call, path, saved, ctx }) => {
    await call("gameplan_write", { path, sections: { problemStatement: "Original" } });
    await assert.rejects(call("gameplan_write", { path: "../outside.yaml", sections: { problemStatement: "Rejected" } }), /refusing/i);
    const outside = join(ctx.cwd, "outside.yaml");
    await writeFile(outside, "untouched");
    await symlink(outside, join(ctx.cwd, "gameplans", "escape.yaml"));
    await assert.rejects(call("gameplan_write", { path: "gameplans/escape.yaml", sections: { problemStatement: "Rejected" } }), /non-regular/);
    assert.equal(await readFile(outside, "utf8"), "untouched");
    assert.equal(saved.at(-1)!.gameplanPath, path);
  });
});

test("patch strings never become temporary filesystem paths", async () => {
  await fixture(async ({ call, onExec }) => {
    let pantCalls = 0;
    onExec(async (command, args) => {
      if (command !== "pant") return;
      pantCalls++;
      const file = args.at(-1)!;
      assert.equal(basename(file), "spec.pant");
      assert.ok(basename(dirname(file)).startsWith("pi-gameplan-spec-"));
      assert.equal(await readFile(file, "utf8"), "module TEST.\n");
    });
    await assert.rejects(call("gameplan_write_spec", { path: "gameplans/example.yaml", patch: "../../../escape", spec: "module TEST." }), /No patch/);
    assert.equal(pantCalls, 2);
  });
});

test("edits during either confirmation cannot finalize a different document", async () => {
  for (const dialog of [1, 2]) {
    await fixture(async ({ call, path, saved, onConfirm }) => {
      const doc = parse(await readFile(path, "utf8"));
      doc.architectureDesign.decisions = [{ id: "AD-C", question: "Existing?", choice: "Yes", alternatives: [], resolution: { kind: "constrained", evidence: "Pinned" } }];
      await writeFile(path, stringify(doc));
      onConfirm(async (index) => {
        if (index === dialog) {
          doc.openQuestions = ["New question"];
          await writeFile(path, stringify(doc));
        }
        return true;
      });
      await assert.rejects(call("gameplan_finalize", { path }), /changed during confirmation/);
      assert.ok(saved.every((s) => !s.finalizedPath));
    });
  }
});

test("post-format open questions and changed constraints cannot finalize", async () => {
  for (const mutation of ["questions", "constraints"]) {
    await fixture(async ({ call, path, saved, onExec }) => {
      onExec(async (_command, args) => {
        if (!args.some((a) => a.endsWith("format_yaml.py"))) return;
        const temporary = args[args.indexOf("--in-place") - 1]!;
        assert.notEqual(temporary, path);
        const doc = parse(await readFile(temporary, "utf8"));
        if (mutation === "questions") doc.openQuestions = ["Added during formatting"];
        else doc.architectureDesign.decisions = [{ id: "AD-C", question: "New?", choice: "Yes", resolution: { kind: "constrained", evidence: "New constraint" } }];
        await writeFile(temporary, stringify(doc));
      });
      await assert.rejects(call("gameplan_finalize", { path }), mutation === "questions" ? /open questions/ : /constrained decisions changed/);
      assert.ok(saved.every((s) => !s.finalizedPath));
    });
  }
});

test("an unchanged approved document finalizes", async () => {
  await fixture(async ({ call, path, saved }) => {
    await call("gameplan_finalize", { path });
    assert.equal(saved.at(-1)!.finalizedPath, path);
  });
});


test("formatter-time symlink swaps never overwrite outside files for writes or finalize", async () => {
  for (const tool of ["gameplan_write", "gameplan_finalize"]) {
    await fixture(async ({ call, ctx, path, saved, onExec }) => {
      const outside = join(ctx.cwd, "outside.yaml");
      await writeFile(outside, "outside must survive");
      onExec(async (_command, args) => {
        if (!args.some((a) => a.endsWith("format_yaml.py"))) return;
        const temporary = args[args.indexOf("--in-place") - 1]!;
        assert.notEqual(temporary, path);
        await rm(path);
        await symlink(outside, path);
        // Emulate the formatter reopening its input with a symlink-following write.
        await writeFile(temporary, await readFile(temporary, "utf8"));
      });
      await assert.rejects(call(tool, { path, sections: { problemStatement: "Updated" } }), /Too many levels of symbolic links/);
      assert.equal(await readFile(outside, "utf8"), "outside must survive");
      assert.ok(saved.every((s) => !s.finalizedPath));
    });
  }
});

test("a refused finalize path does not replace the remembered gameplan", async () => {
  await fixture(async ({ call, path, saved }) => {
    await call("gameplan_write", { path, sections: { problemStatement: "Original" } });
    await assert.rejects(call("gameplan_finalize", { path: "../outside.yaml" }), /Refusing/);
    assert.equal(saved.at(-1)!.gameplanPath, path);
  });
});

test("tool writes and detected hand edits clear finalization", async () => {
  for (const change of ["tool", "write", "edit", "shell", "external"]) {
    await fixture(async ({ call, path, saved, emit }) => {
      await call("gameplan_finalize", { path });
      assert.equal(saved.at(-1)!.finalizedPath, path);
      if (change === "tool") await call("gameplan_write", { path, sections: { problemStatement: "Updated" } });
      else if (change === "write" || change === "edit") await emit("tool_call", { toolName: change, input: { path } });
      else {
        await writeFile(path, (await readFile(path, "utf8")) + "\n# hand edit\n");
        await emit(change === "shell" ? "tool_result" : "before_agent_start", { messageType: "user" });
      }
      assert.equal(saved.at(-1)!.finalizedPath, undefined, change);
    });
  }
});


test("restored disallowed finalized paths are invalidated during startup", async () => {
  await fixture(async ({ restore, saved }) => {
    await restore({ enabled: true, gameplanDir: "gameplans", decisions: [], finalizedPath: "/outside/large.yaml", finalizedDigest: "fake" });
    assert.equal(saved.at(-1)!.finalizedPath, undefined);
  });
});

test("finalization snapshots distinguish byte sequences with identical UTF-8 decoding", async () => {
  await fixture(async ({ call, path, saved, emit }) => {
    const prefix = Buffer.from((await readFile(path, "utf8")) + "\n# byte: ");
    const original = Buffer.concat([prefix, Buffer.from([0x80, 0x0a])]);
    const changed = Buffer.concat([prefix, Buffer.from([0x81, 0x0a])]);
    assert.equal(original.toString("utf8"), changed.toString("utf8"));
    await writeFile(path, original);
    await call("gameplan_finalize", { path });
    await emit("tool_result");
    assert.equal(saved.at(-1)!.finalizedPath, path);
    // The formatter writes UTF-8, so its replacement character and an invalid byte still
    // decode identically. A raw-byte digest must distinguish them.
    const finalized = await readFile(path);
    const location = finalized.indexOf(Buffer.from("\ufffd"));
    assert.ok(location >= 0);
    const replaced = Buffer.concat([finalized.subarray(0, location), Buffer.from([0x81]), finalized.subarray(location + 3)]);
    assert.equal(finalized.toString("utf8"), replaced.toString("utf8"));
    await writeFile(path, replaced);
    await emit("tool_result");
    assert.equal(saved.at(-1)!.finalizedPath, undefined);
  });
});
