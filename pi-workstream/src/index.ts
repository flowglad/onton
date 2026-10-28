/** Lean workstream authoring: the skill owns judgment, dialogs record user choices. */
import { lstat } from "node:fs/promises";
import { readDraft } from "./files.js";
import { createHash } from "node:crypto";
import { dirname, extname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";
import { answerDecision, describeDecisions, ENTRY, fromEntries, initialState, recordDecision, startDraft,
  type Answer, type Question, type State } from "./state.js";

const TOOLS = ["workstream_open", "workstream_decide", "workstream_finalize"];
const DISCUSS = "Discuss further / propose another option (no approval)";
const DELEGATE = "Delegate the recommendation to the agent";
const digest = (bytes: Buffer) => createHash("sha256").update(bytes).digest("hex");
const result = (text: string, details: unknown = undefined) => ({ content: [{ type: "text" as const, text }], details });

export default function workstream(pi: ExtensionAPI) {
  const skill = resolve(process.env.ONTON_WORKSTREAM_SKILL_DIR
    ?? fileURLToPath(new URL("../../skills/write-workstream", import.meta.url)), "SKILL.md");
  let state: State = initialState;

  function status(ctx: ExtensionContext) {
    const unresolved = state.decisions.filter((d) => d.resolution.kind === "unresolved").length;
    ctx.ui.setStatus("workstream", state.enabled
      ? `workstream · ${state.decisions.length} decisions${unresolved ? ` · ${unresolved} unresolved` : ""}${state.finalized ? " · finalized" : ""}`
      : undefined);
  }
  function save(ctx: ExtensionContext) { pi.appendEntry(ENTRY, state); status(ctx); }
  function activate() {
    const other = pi.getActiveTools().filter((name) => !TOOLS.includes(name));
    pi.setActiveTools(state.enabled ? [...other, ...TOOLS] : other);
  }
  function requireMode() {
    if (!state.enabled) throw new Error("Workstream mode is off. Run /workstream on first.");
  }
  async function documentPath(ctx: ExtensionContext, path: string): Promise<string> {
    const absolute = resolve(ctx.cwd, path);
    if (dirname(absolute) !== resolve(ctx.cwd, state.directory) || extname(absolute) !== ".md") {
      throw new Error(`Choose a Markdown file directly in ${state.directory}.`);
    }
    try {
      if (!(await lstat(absolute)).isFile()) throw new Error("The workstream draft must be a regular file.");
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
    }
    return absolute;
  }
  function invalidate(ctx: ExtensionContext) {
    if (state.finalized) { state = { ...state, finalized: undefined }; save(ctx); }
  }
  async function refresh(ctx: ExtensionContext) {
    if (!state.finalized) return;
    try {
      if (digest(await readDraft(await documentPath(ctx, state.finalized.path))) !== state.finalized.digest) invalidate(ctx);
    } catch { invalidate(ctx); }
  }
  async function restore(ctx: ExtensionContext) {
    state = fromEntries(ctx.sessionManager.getBranch());
    await refresh(ctx);
    activate();
    status(ctx);
  }

  pi.registerFlag("workstream", { description: "Start in interactive workstream authoring mode", type: "boolean", default: false });
  pi.on("resources_discover", () => ({ skillPaths: [skill] }));
  pi.registerCommand("workstream", {
    description: "Workstream authoring: toggle | on | off | status | dir <path>",
    getArgumentCompletions: (prefix) => ["on", "off", "status", "dir"].filter((s) => s.startsWith(prefix)).map((value) => ({ value, label: value })),
    async handler(args, ctx) {
      await refresh(ctx);
      const [action = "", ...rest] = args.trim().split(/\s+/);
      if (action === "status") {
        ctx.ui.notify(`mode: ${state.enabled ? "on" : "off"}\ndirectory: ${state.directory}\nfile: ${state.path ?? "not selected"}\nskill: ${skill}\n${describeDecisions(state.decisions)}${state.finalized ? "\nfinalized" : ""}`, "info");
        return;
      }
      if (action === "dir") {
        const directory = rest.join(" ");
        if (!directory) { ctx.ui.notify(`Workstream directory: ${state.directory}`, "info"); return; }
        state = { ...state, directory }; save(ctx); return;
      }
      if (!["", "on", "off"].includes(action)) { ctx.ui.notify("Usage: /workstream [on|off|status|dir <path>]", "error"); return; }
      state = { ...state, enabled: action === "" ? !state.enabled : action === "on" };
      activate(); save(ctx);
      ctx.ui.notify(`Workstream mode ${state.enabled ? "on" : "off"}.`, "info");
    },
  });

  pi.on("before_agent_start", async (_event, ctx) => {
    await refresh(ctx);
    if (!state.enabled) return;
    return { message: { customType: "workstream-mode-context", display: false, content: `[WORKSTREAM MODE ACTIVE]
You are interviewing the user and composing a workstream, not implementing it.
Read and follow ${skill}, including its format reference. Ask a few focused questions, wait for answers, and adapt the interview. Establish lifespan (spike through mission-critical infrastructure), stakes, failure tolerance, time and maintenance budgets, and non-goals before recommending implementation approaches. Reuse answers already given in chat.
Inspect the checkout independently to establish current facts. Discuss implementation tradeoffs with the user; use workstream_decide to record each implementation choice before composing milestones that depend on it. Never add correctness, security, scale, or abstraction layers without validating their need and cost with the user. Recommendations and discussion are not approval. Explicit delegation applies only to that decision and its stated scope.
Use workstream_open to select a new or existing ${state.directory}/<name>.md before recording decisions. Opening another draft clears the active ledger; existing documents do not import approvals. Write Markdown with ordinary write/edit tools. Ordinary editing and shell tools remain available.
${state.path ? `Active draft: ${state.path}. Read it before editing; it is the source of truth after compaction.` : "No draft selected yet."}
Summarize agreed goals and choices before composing milestones. Keep unchosen approaches as open questions, never settled deliverables, precedents, or acceptance criteria. Later-milestone questions may remain deferred; resolve choices needed for the current composition with the user.
When the draft is ready, summarize its scope, decisions, and deferred questions in chat, then call workstream_finalize for user review. Final review does not replace the interview or approve unrecorded implementation choices. After finalization, stop planning unless the user requests revisions or another draft. To plan an individual milestone, switch to /gameplan; do not execute the workstream.
Recorded decisions (session branch only):
${describeDecisions(state.decisions)}` } };
  });

  pi.registerTool({
    name: "workstream_open", label: "Open workstream",
    description: "Select a new or existing Markdown workstream for authoring. Asks the user before replacing the active draft and clearing its decision ledger. Leaves files unchanged; existing prose does not import approvals. Call before interviewing implementation choices for a different draft.",
    exposure: "model-only", executionMode: "sequential",
    parameters: Type.Object({ path: Type.String({ minLength: 1 }) }),
    async execute(_id, params, signal, _update, ctx) {
      requireMode();
      if (!ctx.hasUI) throw new Error("Opening a workstream requires an interactive user.");
      const path = await documentPath(ctx, params.path);
      if (state.path === path) return result(`Already authoring ${path}; decisions preserved.`);
      const before = state;
      const accepted = await ctx.ui.confirm("Open workstream?", `${path}\nStart with an empty decision ledger. Existing files remain unchanged; any existing implementation choices must be discussed again before adopting them.`, { signal });
      signal?.throwIfAborted();
      if (!accepted) return result("Cancelled; active draft unchanged.");
      if (state !== before) throw new Error("Mode state changed during confirmation; retry.");
      state = startDraft(state, path); save(ctx);
      return result(`Authoring ${path}. Interview the user before composing milestones.`, { path });
    },
  });

  pi.registerTool({
    name: "workstream_decide", label: "Workstream decision",
    description: "Ask the user to choose an implementation approach. Give concrete tradeoffs tied to their project context. Selection approves, discussion or dismissal stays unresolved; explicit delegation records only the recommendation and scope. Reuse the same id to revise a decision.",
    exposure: "model-only", executionMode: "sequential",
    parameters: Type.Object({
      id: Type.String({ minLength: 1 }), topic: Type.String({ minLength: 1 }), question: Type.String({ minLength: 1 }),
      options: Type.Array(Type.Object({ choice: Type.String({ minLength: 1 }), tradeoffs: Type.String({ minLength: 1 }) }), { minItems: 2 }),
      recommendation: Type.Integer({ minimum: 0 }), why: Type.String({ minLength: 1 }),
    }),
    async execute(_id, params: Question, signal, _update, ctx) {
      requireMode();
      if (!state.path) throw new Error("Select a draft with workstream_open first.");
      if (!ctx.hasUI) throw new Error("Recording a decision requires an interactive user.");
      if (!params.options[params.recommendation]) throw new Error("Recommendation index is out of range.");
      const before = state;
      const labels = params.options.map((o, i) => `${i + 1}. ${o.choice}${i === params.recommendation ? " (recommended)" : ""}`);
      const prompt = `${params.id} · ${params.topic}: ${params.question}\n\n${params.options.map((o, i) => `${i + 1}. ${o.choice}: ${o.tradeoffs}`).join("\n")}\n\nRecommendation: ${params.why}`;
      const selected = await ctx.ui.select(prompt, [...labels, DISCUSS, DELEGATE], { signal });
      let answer: Answer = { kind: "cancelled" };
      if (selected === DISCUSS) {
        const text = await ctx.ui.editor("Questions or alternatives to discuss (not approval)", "");
        if (text?.trim()) answer = { kind: "discussion", text: text.trim() };
      } else if (selected === DELEGATE) {
        const scope = await ctx.ui.input("Scope of delegation (cancel to leave unresolved)", "e.g. keep storage local", { signal });
        if (scope?.trim()) answer = { kind: "delegated", scope: scope.trim() };
      } else {
        const index = selected === undefined ? -1 : labels.indexOf(selected);
        if (index >= 0) answer = { kind: "selected", index };
      }
      signal?.throwIfAborted();
      if (state !== before) throw new Error("Active draft changed while answering; ask again.");
      const decision = answerDecision(params, answer);
      state = recordDecision(state, decision); save(ctx);
      return result(`${describeDecisions([decision])}${decision.resolution.kind === "unresolved" ? "\nThis is not approval. Address the user's feedback and ask again with the same id." : ""}`, decision);
    },
  });

  pi.registerTool({
    name: "workstream_finalize", label: "Finalize workstream",
    description: "Show the active Markdown draft and recorded decisions for final user review. Refuses while recorded decisions remain unresolved. Deferred future-milestone questions may remain in prose. Does not validate Markdown semantics, write files, or execute milestones.",
    exposure: "model-only", executionMode: "sequential", parameters: Type.Object({}),
    async execute(_id, _params, signal, _update, ctx) {
      requireMode();
      if (!ctx.hasUI) throw new Error("Final review requires an interactive user.");
      if (!state.path) throw new Error("Select a draft with workstream_open first.");
      const unresolved = state.decisions.filter((d) => d.resolution.kind === "unresolved");
      if (unresolved.length) throw new Error(`Unresolved decisions: ${unresolved.map((d) => d.id).join(", ")}. Discuss them before finalizing.`);
      const before = state;
      const bytes = await readDraft(await documentPath(ctx, state.path));
      if (!bytes.toString("utf8").trim()) throw new Error("The workstream draft is empty.");
      const accepted = await ctx.ui.confirm(`Finalize ${state.path}?`, `${bytes.toString("utf8")}\n\nRecorded decisions:\n${describeDecisions(state.decisions)}\n\nConfirm this draft, including any explicitly deferred questions?`, { signal });
      signal?.throwIfAborted();
      if (!accepted) return result("Not finalized. Ask the user what should change.");
      if (state !== before || !(await readDraft(await documentPath(ctx, before.path!))).equals(bytes)) {
        throw new Error("Draft or decisions changed during review; review again.");
      }
      state = { ...state, finalized: { path: before.path!, digest: digest(bytes) } }; save(ctx);
      return result(`Finalized ${state.path}. No milestones were executed. Use gameplan mode when the user is ready to plan a milestone.`, state.finalized);
    },
  });

  pi.on("session_start", async (_event, ctx) => {
    await restore(ctx);
    if (pi.getFlag("workstream") === true && !state.enabled) {
      state = { ...state, enabled: true }; activate(); save(ctx);
    }
  });
  pi.on("session_tree", async (_event, ctx) => { await restore(ctx); });
  pi.on("tool_result", async (_event, ctx) => { await refresh(ctx); });
  pi.on("session_shutdown", async (_event, ctx) => { ctx.ui.setStatus("workstream", undefined); });
}
