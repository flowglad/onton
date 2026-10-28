import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdtemp, mkdir, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { tmpdir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI, ExtensionToolContext } from "@earendil-works/pi-coding-agent";
import workstream from "../src/index.js";
import { answerDecision, ENTRY, fromEntries, type Question, type State } from "../src/state.js";

const question: Question = {
  id: "AD-1", topic: "Storage", question: "How should the spike store its data?",
  options: [{ choice: "Local file", tradeoffs: "Simple, one operator" }, { choice: "Database", tradeoffs: "Shared access, service maintenance" }],
  recommendation: 0, why: "The user described a disposable local spike.",
};

type Handler = (...args: any[]) => any;
async function fixture(run: (f: {
  call: (name: string, params?: unknown) => Promise<any>;
  emit: (name: string, event?: unknown) => Promise<any>;
  command: (args: string) => Promise<any>;
  ctx: ExtensionToolContext;
  path: string;
  saved: State[];
  active: () => string[];
  restore: (state: State, event?: string) => Promise<any>;
  confirm: (fn: () => Promise<boolean>) => void;
}) => Promise<void>) {
  const cwd = await mkdtemp(join(tmpdir(), "pi-workstream-test-"));
  const path = join(cwd, "docs/workstreams/example.md");
  await mkdir(join(cwd, "docs/workstreams"), { recursive: true });
  await writeFile(path, "# Workstream: spike\n\n## Open Questions\nFuture scale is deferred.\n");
  const tools = new Map<string, Handler>();
  const events = new Map<string, Handler>();
  const commands = new Map<string, Handler>();
  const saved: State[] = [];
  let entries: any[] = [];
  let active = ["read", "write", "bash", "gameplan_decide"];
  let confirm = async () => true;
  const api = {
    registerTool: (tool: any) => tools.set(tool.name, tool.execute),
    registerCommand: (name: string, command: any) => commands.set(name, command.handler),
    registerFlag: () => {}, getFlag: () => false,
    on: (name: string, fn: Handler) => events.set(name, fn),
    appendEntry: (_type: string, state: State) => saved.push(state),
    getActiveTools: () => active, setActiveTools: (names: string[]) => { active = names; },
  } as unknown as ExtensionAPI;
  const ctx = {
    cwd, hasUI: true, sessionManager: { getBranch: () => entries },
    ui: { setStatus: () => {}, notify: () => {}, confirm: () => confirm(),
      select: async () => "1. Local file (recommended)", editor: async () => "Please discuss operational costs", input: async () => "local only" },
  } as unknown as ExtensionToolContext;
  workstream(api);
  try {
    await events.get("session_start")!({}, ctx);
    await commands.get("workstream")!("on", ctx);
    await run({
      call: (name, params = {}) => tools.get(name)!("test", params, undefined, undefined, ctx),
      emit: async (name, event = {}) => events.get(name)?.(event, ctx),
      command: async (args) => commands.get("workstream")!(args, ctx),
      ctx, path, saved, active: () => active,
      restore: async (state, event = "session_start") => {
        entries = [{ type: "custom", customType: ENTRY, data: state }];
        return events.get(event)!({}, ctx);
      },
      confirm: (fn) => { confirm = fn; },
    });
  } finally { await rm(cwd, { recursive: true, force: true }); }
}

test("mode toggles only its own tools, advertises the skill, and restores branch state", async () => {
  await fixture(async ({ command, active, emit, restore, saved }) => {
    assert.ok(active().includes("workstream_decide"));
    assert.ok(active().includes("gameplan_decide"));
    assert.match(JSON.stringify(await emit("resources_discover")), /write-workstream/);
    const enabled = saved.at(-1)!;
    assert.match(JSON.stringify(await emit("before_agent_start")), /lifespan/);
    await command("off");
    assert.ok(!active().some((name) => ["workstream_open", "workstream_decide", "workstream_finalize"].includes(name)));
    assert.ok(active().includes("gameplan_decide"));
    assert.equal(await emit("before_agent_start"), undefined);
    await restore(enabled, "session_tree");
    assert.ok(active().includes("workstream_decide"));
  });
});

test("selection records approval; discussion survives reload and blocks final review", async () => {
  await fixture(async ({ call, path, ctx, saved, restore }) => {
    await call("workstream_open", { path });
    await call("workstream_decide", question);
    assert.deepEqual(saved.at(-1)!.decisions[0]!.resolution, { kind: "approved", choice: "Local file" });
    ctx.ui.select = async () => "Discuss further / propose another option (no approval)";
    const response = await call("workstream_decide", question);
    assert.match(JSON.stringify(response), /not approval/);
    const state = saved.at(-1)!;
    assert.equal(state.decisions.length, 1);
    assert.equal(state.decisions[0]!.resolution.kind, "unresolved");
    await restore(state);
    await assert.rejects(call("workstream_finalize"), /Unresolved decisions/);
    assert.match(JSON.stringify(fromEntries([{ type: "custom", customType: ENTRY, data: state }])), /operational costs/);
  });
});

test("dismissal and cancelled delegation never grant approval", async () => {
  await fixture(async ({ call, path, ctx, saved }) => {
    await call("workstream_open", { path });
    ctx.ui.select = async () => undefined;
    await call("workstream_decide", question);
    assert.equal(saved.at(-1)!.decisions[0]!.resolution.kind, "unresolved");
    ctx.ui.select = async () => "Delegate the recommendation to the agent";
    ctx.ui.input = async () => undefined;
    await call("workstream_decide", question);
    assert.equal(saved.at(-1)!.decisions[0]!.resolution.kind, "unresolved");
    for (const scope of ["", "   ", "\t\n"]) {
      ctx.ui.input = async () => scope;
      await call("workstream_decide", question);
      assert.equal(saved.at(-1)!.decisions[0]!.resolution.kind, "unresolved");
      await assert.rejects(call("workstream_finalize"), /Unresolved decisions/);
    }
    ctx.ui.input = async () => "  local file only  ";
    await call("workstream_decide", question);
    assert.deepEqual(saved.at(-1)!.decisions[0]!.resolution, { kind: "delegated", choice: "Local file", scope: "local file only" });
  });
});

test("new and existing drafts clear approvals only after confirmation, without editing files", async () => {
  await fixture(async ({ call, path, saved, confirm, ctx }) => {
    await call("workstream_open", { path });
    await call("workstream_decide", question);
    const original = await readFile(path);
    const next = join(ctx.cwd, "docs/workstreams/next.md");
    confirm(async () => false);
    await call("workstream_open", { path: next });
    assert.equal(saved.at(-1)!.path, path);
    assert.equal(saved.at(-1)!.decisions.length, 1);
    confirm(async () => true);
    await call("workstream_open", { path: next });
    assert.equal(saved.at(-1)!.path, next);
    assert.deepEqual(saved.at(-1)!.decisions, []);
    await assert.rejects(readFile(next), /ENOENT/);
    await call("workstream_open", { path });
    assert.deepEqual(await readFile(path), original);
    assert.deepEqual(saved.at(-1)!.decisions, []);
  });
});

test("final review allows deferred questions and invalidates approval on edits", async () => {
  await fixture(async ({ call, path, saved, emit, restore }) => {
    await call("workstream_open", { path });
    await call("workstream_decide", question);
    await call("workstream_finalize");
    const finalized = saved.at(-1)!;
    assert.equal(finalized.finalized!.path, path);
    await restore(finalized);
    assert.ok(saved.at(-1)!.finalized);
    await writeFile(path, "# Changed workstream\n");
    await emit("tool_result");
    assert.equal(saved.at(-1)!.finalized, undefined);
    await restore(finalized);
    assert.equal(saved.at(-1)!.finalized, undefined);
  });
});

test("file edits and decision changes during review reject finalization", async () => {
  await fixture(async ({ call, path, saved, confirm }) => {
    await call("workstream_open", { path });
    confirm(async () => { await writeFile(path, "# Edited during review\n"); return true; });
    await assert.rejects(call("workstream_finalize"), /changed during review/);
    assert.equal(saved.at(-1)!.finalized, undefined);
    confirm(async () => { await call("workstream_decide", question); return true; });
    await assert.rejects(call("workstream_finalize"), /changed during review/);
    assert.equal(saved.at(-1)!.finalized, undefined);
  });
});

test("declined final review and empty documents are not finalized", async () => {
  await fixture(async ({ call, path, saved, confirm }) => {
    await call("workstream_open", { path });
    confirm(async () => false);
    await call("workstream_finalize");
    assert.equal(saved.at(-1)!.finalized, undefined);
    await writeFile(path, " \n");
    await assert.rejects(call("workstream_finalize"), /empty/);
  });
});

test("UI, mode, draft selection and recommendation errors leave no approvals", async () => {
  await fixture(async ({ call, path, ctx, saved, command }) => {
    await assert.rejects(call("workstream_decide", question), /Select a draft/);
    await call("workstream_open", { path });
    await assert.rejects(call("workstream_decide", { ...question, recommendation: 5 }), /out of range/);
    await assert.rejects(call("workstream_open", { path: "elsewhere.md" }), /Markdown file/);
    ctx.hasUI = false;
    await assert.rejects(call("workstream_decide", question), /interactive user/);
    await assert.rejects(call("workstream_finalize"), /interactive user/);
    assert.deepEqual(saved.at(-1)!.decisions, []);
    await command("off");
    await assert.rejects(call("workstream_decide", question), /mode is off/);
  });
});

test("malformed session entries are ignored, and only active branch entries restore", () => {
  const valid: State = { enabled: true, directory: "plans", decisions: [] };
  const entries = [null, {}, { ...valid, decisions: [null] }, { ...valid, finalized: true }];
  for (const data of entries) assert.deepEqual(fromEntries([{ type: "custom", customType: ENTRY, data }]), { enabled: false, directory: "docs/workstreams", decisions: [] });
  assert.deepEqual(fromEntries([{ type: "custom", customType: ENTRY, data: valid }, { type: "custom", customType: ENTRY, data: {} }]), valid);
  assert.equal(fromEntries([]).enabled, false);
});


test("the decision reducer rejects blank delegation scopes", () => {
  for (const scope of ["", " ", "\t\n"]) {
    assert.equal(answerDecision(question, { kind: "delegated", scope }).resolution.kind, "unresolved");
  }
  assert.deepEqual(answerDecision(question, { kind: "delegated", scope: " local only " }).resolution,
    { kind: "delegated", choice: "Local file", scope: "local only" });
});

for (const kind of ["symlink", "fifo", "directory", "oversized"] as const) {
  test(`draft admission and review reject ${kind} entries`, { timeout: 10_000 }, async () => {
    await fixture(async ({ call, path, ctx, saved, confirm, emit, restore }) => {
      const outside = join(ctx.cwd, "outside.md");
      await writeFile(outside, "Outside content must not appear in review");
      const replace = async () => {
        await rm(path, { recursive: true, force: true });
        if (kind === "symlink") await symlink(outside, path);
        else if (kind === "fifo") await promisify(execFile)("mkfifo", [path], { timeout: 2000 });
        else if (kind === "directory") await mkdir(path);
        else await writeFile(path, Buffer.alloc(16 * 1024 * 1024 + 1, 65));
      };
      await call("workstream_open", { path });
      await call("workstream_finalize");
      const approved = saved.at(-1)!;
      await replace();
      let reviews = 0;
      confirm(async () => { reviews++; return true; });
      await assert.rejects(call("workstream_finalize"), /regular file|exceeds 16 MiB/);
      assert.equal(reviews, 0);
      await emit("tool_result");
      assert.equal(saved.at(-1)!.finalized, undefined);
      await restore(approved);
      assert.equal(saved.at(-1)!.finalized, undefined);
      if (kind !== "oversized") {
        await assert.rejects(call("workstream_open", { path }), /regular file/);
      }
      await rm(path, { recursive: true, force: true });
      await writeFile(path, "# Regular draft");
      confirm(async () => { await replace(); return true; });
      await assert.rejects(call("workstream_finalize"), /regular file|exceeds 16 MiB/);
      assert.equal(saved.at(-1)!.finalized, undefined);
    });
  });
}


test("directory changes affect new selections while retaining the admitted draft across reloads", async () => {
  await fixture(async ({ call, path, command, saved, restore, emit, ctx }) => {
    await call("workstream_open", { path });
    await call("workstream_decide", question);
    await command("dir docs/roadmaps");
    await restore(saved.at(-1)!);
    await call("workstream_finalize");
    const finalized = saved.at(-1)!.finalized;
    assert.equal(finalized!.path, path);
    await command("dir docs/plans");
    await emit("tool_result");
    await command("status");
    await restore(saved.at(-1)!, "session_tree");
    assert.deepEqual(saved.at(-1)!.finalized, finalized);
    assert.equal(saved.at(-1)!.decisions[0]!.resolution.kind, "approved");
    await call("workstream_finalize");
    await assert.rejects(call("workstream_open", { path }), /Markdown file directly in docs\/plans/);
    const next = join(ctx.cwd, "docs/plans/next.md");
    await call("workstream_open", { path: next });
    assert.equal(saved.at(-1)!.path, next);
    assert.equal(saved.at(-1)!.finalized, undefined);
    assert.deepEqual(saved.at(-1)!.decisions, []);
  });
});
