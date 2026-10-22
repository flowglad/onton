/**
 * pi-gameplan: a Pi extension that turns the write-gameplan skill's load-bearing rules into
 * tool contracts.
 *
 * - `/gameplan` toggles an authoring mode: write/edit may only touch `gameplans/*.yaml`, and
 *   the skill is loaded into context.
 * - `gameplan_decide` asks the engineer an architectural question in a dialog and records the
 *   answer. It is the only way an `engineer_approved` or `delegated` resolution can be produced.
 * - `gameplan_check` runs the skill's validator and audits every resolution against the ledger.
 * - `gameplan_finalize` refuses while anything is unresolved, asks the engineer to accept
 *   agent-asserted `constrained` decisions, formats the file, and records the result.
 */

import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { existsSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { relative, resolve } from "node:path";
import type { ExtensionAPI, ExtensionContext, ExtensionToolContext } from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import { Type } from "typebox";
import { parse as parseYaml } from "yaml";
import { auditDecisions, type AuditResult } from "./audit.js";
import { createHash } from "node:crypto";
import { commitGameplan, readGameplanSnapshot, resolveGameplanWrite } from "./files.js";
import { isGameplanWrite } from "./guard.js";
import { ASK_USER_TOOL, answerFromAskUser, buildAskUserRequest, fitsAskUser } from "./ask.js";
import {
  applySections,
  composeDecisions,
  createValidator,
  importDecisions,
  type JsonObject,
  loadGameplanDocument,
  missingSections,
  patchCount,
  projectName,
  renderDocument,
  type SchemaValidator,
  setPatchSpec,
  unwrittenReferencedPatches,
  upsertPatch,
} from "./document.js";
import { describeDiagnostics, expectedModuleName, moduleNameFromJson, normalizeSpec, parsePantDiagnostics, type SpecTarget } from "./spec.js";
import { describeGroups, missingGroups, noRequirements, parseSkillRequirements, type SkillRequirements } from "./requirements.js";
import {
  type Answer,
  CUSTOM_ENTRY_TYPE,
  type DecideParams,
  type DecisionRecord,
  DEFAULT_GAMEPLAN_DIR,
  fromEntries,
  type GameplanState,
  initialState,
  recordDecision,
  removeDecision,
  resolveDecision,
  startGameplan,
} from "./state.js";
import {
  missingSkillFiles,
  pythonCommand,
  resolveSkillPaths,
  runFormatter,
  runValidator,
  type ValidationReport,
} from "./validator.js";

const OUR_TOOLS = ["gameplan_new", "gameplan_load", "gameplan_remove_decision", "gameplan_decide", "gameplan_write", "gameplan_write_patch", "gameplan_write_spec", "gameplan_check", "gameplan_finalize"];
const WRITE_TOOLS = new Set(["write", "edit"]);
const OTHER_LABEL = "Discuss further / propose another option (no approval)";
const DELEGATE_LABEL = "Delegate: let the agent choose (records the recommendation as delegated)";

export default function gameplan(pi: ExtensionAPI) {
  const skill = resolveSkillPaths();
  const python = pythonCommand();
  let state: GameplanState = initialState;
  let requirements: SkillRequirements = noRequirements;
  try {
    requirements = parseSkillRequirements(readFileSync(skill.skillMd, "utf8"));
  } catch (error) {
    requirements = { ...noRequirements, error: String(error) };
  }
  let schemaValidator: SchemaValidator | undefined;
  let schemaError: string | undefined;
  try {
    schemaValidator = createValidator(JSON.parse(readFileSync(skill.schema, "utf8")) as JsonObject);
  } catch (error) {
    schemaError = error instanceof Error ? error.message : String(error);
  }

  function validator(): SchemaValidator {
    if (!schemaValidator) throw new Error(`gameplan schema unavailable (${schemaError ?? "unknown"}); check ${skill.schema}`);
    return schemaValidator;
  }

  function registeredTools(): string[] {
    return pi.getAllTools().map((t) => t.name);
  }

  function missingRequired() {
    return missingGroups(requirements.required, registeredTools());
  }

  function missingRecommended() {
    return missingGroups(requirements.recommended, registeredTools());
  }

  function reportMissingTools(ctx: ExtensionContext): void {
    if (requirements.error) ctx.ui.notify(`gameplan: cannot read skill requirements: ${requirements.error}`, "warning");
    const required = missingRequired();
    if (required.length > 0) {
      ctx.ui.notify(`gameplan: the write-gameplan skill expects tools that are not registered: ${describeGroups(required)}`, "warning");
    }
    const recommended = missingRecommended();
    if (recommended.length > 0) {
      ctx.ui.notify(`gameplan: optional tools not installed: ${describeGroups(recommended)}`, "info");
    }
  }

  function hasTool(name: string): boolean {
    return registeredTools().includes(name);
  }

  function persist(): void {
    pi.appendEntry(CUSTOM_ENTRY_TYPE, state);
  }

  function digest(bytes: Buffer): string {
    return createHash("sha256").update(bytes).digest("hex");
  }

  function clearFinalized(ctx: ExtensionContext): void {
    if (!state.finalizedPath) return;
    state = { ...state, finalizedPath: undefined, finalizedDigest: undefined };
    persist();
    updateStatus(ctx);
  }

  async function refreshFinalized(ctx: ExtensionContext): Promise<void> {
    if (!state.finalizedPath) return;
    try {
      const bytes = await readGameplanSnapshot(ctx.cwd, state.gameplanDir, state.finalizedPath);
      if (digest(bytes) !== state.finalizedDigest) clearFinalized(ctx);
    } catch {
      clearFinalized(ctx);
    }
  }

  function updateStatus(ctx: ExtensionContext): void {
    if (!state.enabled) {
      ctx.ui.setStatus("gameplan", undefined);
      return;
    }
    const unresolved = state.decisions.filter((d) => d.kind === "unresolved").length;
    const parts = [`gameplan`, `${state.decisions.length} decision${state.decisions.length === 1 ? "" : "s"}`];
    if (unresolved > 0) parts.push(`${unresolved} unresolved`);
    if (state.finalizedPath) parts.push("finalized");
    ctx.ui.setStatus("gameplan", ctx.ui.theme.fg(unresolved > 0 ? "warning" : "accent", parts.join(" · ")));
  }

  function activateTools(): void {
    const active = pi.getActiveTools();
    pi.setActiveTools([...new Set([...active, ...OUR_TOOLS])]);
  }

  function deactivateTools(): void {
    pi.setActiveTools(pi.getActiveTools().filter((name) => !OUR_TOOLS.includes(name)));
  }

  function enable(ctx: ExtensionContext): void {
    state = { ...state, enabled: true, toolsBeforeMode: pi.getActiveTools() };
    activateTools();
    persist();
    updateStatus(ctx);
    ctx.ui.notify(`Gameplan mode on. Gameplans go to ${state.gameplanDir}/*.yaml.`, "info");
    reportMissingTools(ctx);
  }

  /** Remember the gameplan file the session is working on; it is the pointer that survives compaction. */
  function rememberGameplanPath(path: string): void {
    if (state.gameplanPath === path) return;
    state = { ...state, gameplanPath: path };
    persist();
  }

  function resolveGameplanPath(path: string | undefined): string {
    const chosen = path?.trim() || state.gameplanPath;
    if (!chosen) throw new Error("No gameplan file known yet. Pass path, e.g. gameplans/<project-name>.yaml.");
    return chosen;
  }

  /**
   * Read-modify-write of the gameplan file. The file is the source of truth: hand edits and
   * earlier tool writes are parsed first, `mutate` applies the change, and the result is
   * formatted with the skill's formatter when the Python wrapper is available.
   */
  async function writeGameplan(
    ctx: ExtensionToolContext,
    path: string,
    signal: AbortSignal | undefined,
    mutate: (doc: ReturnType<typeof loadGameplanDocument>) => void,
  ): Promise<{ path: string; formatting?: string; missing: string[]; unwrittenPatches: string[]; patches: number }> {
    if (!isGameplanWrite(ctx.cwd, state.gameplanDir, path)) {
      throw new Error(`Gameplan files live at ${state.gameplanDir}/<project-name>.yaml; refusing to write ${path}`);
    }
    const admission = await resolveGameplanWrite(ctx.cwd, state.gameplanDir, path);
    const abs = admission.path;
    const existing = existsSync(abs) ? await readFile(abs) : undefined;
    const doc = loadGameplanDocument(existing?.toString("utf8"));
    mutate(doc);
    const formatted = await formatText(ctx, renderDocument(doc), signal);
    await commitGameplan(admission, formatted.text, existing);
    clearFinalized(ctx);
    rememberGameplanPath(path);
    return {
      path,
      formatting: formatted.error,
      missing: missingSections(doc, validator().sections),
      unwrittenPatches: unwrittenReferencedPatches(doc),
      patches: patchCount(doc),
    };
  }

  /** Only a private temporary copy is ever handed to the formatter. */
  async function formatText(ctx: ExtensionToolContext, text: string, signal: AbortSignal | undefined): Promise<{ text: string; error?: string }> {
    const dir = await mkdtemp(join(tmpdir(), "pi-gameplan-format-"));
    try {
      const path = join(dir, "gameplan.yaml");
      await writeFile(path, text, "utf8");
      const result = await runFormatter((c, a, o) => pi.exec(c, a, o), skill, python, path, { cwd: ctx.cwd, signal });
      return result.ok ? { text: await readFile(path, "utf8") } : { text, error: result.error };
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  }

  function describeWrite(
    result: { path: string; formatting?: string; missing: string[]; unwrittenPatches: string[]; patches: number },
    what: string,
  ): string {
    const lines = [`${what} in ${result.path} (${result.patches} patch${result.patches === 1 ? "" : "es"}).`];
    if (result.missing.length > 0) lines.push(`Required sections still missing: ${result.missing.join(", ")}.`);
    if (result.unwrittenPatches.length > 0) {
      lines.push(`Patches referenced but not yet written: ${result.unwrittenPatches.join(", ")} (gameplan_write_patch).`);
    }
    if (result.missing.length === 0 && result.unwrittenPatches.length === 0) lines.push("All required sections and referenced patches are present; run gameplan_check.");
    if (result.formatting) lines.push(`Formatting skipped (${result.formatting}); gameplan_check will report formatting until it runs.`);
    return lines.join("\n");
  }

  function disable(ctx: ExtensionContext): void {
    const restore = state.toolsBeforeMode;
    state = { ...state, enabled: false, toolsBeforeMode: undefined };
    if (restore) pi.setActiveTools(restore.filter((name) => !OUR_TOOLS.includes(name)));
    else deactivateTools();
    persist();
    updateStatus(ctx);
    ctx.ui.notify("Gameplan mode off. Full tool access restored.", "info");
  }

  function requireMode(): void {
    if (!state.enabled) throw new Error("Gameplan mode is off. Ask the user to run /gameplan first.");
  }

  pi.registerFlag("gameplan", { description: "Start in gameplan authoring mode", type: "boolean", default: false });

  // Register the write-gameplan skill with Pi so it is advertised in the system prompt and
  // available as /skill:write-gameplan, whether or not the mode is on.
  pi.on("resources_discover", () => ({ skillPaths: [skill.skillMd] }));

  pi.registerCommand("gameplan", {
    description: "Gameplan authoring mode: toggle | on | off | status | dir <path>",
    getArgumentCompletions(prefix) {
      return ["on", "off", "status", "dir"].filter((v) => v.startsWith(prefix)).map((value) => ({ value, label: value }));
    },
    async handler(args, ctx) {
      await refreshFinalized(ctx);
      const [action = "", ...rest] = args.trim().split(/\s+/);
      switch (action) {
        case "":
          return state.enabled ? disable(ctx) : enable(ctx);
        case "on":
          return state.enabled ? ctx.ui.notify("Gameplan mode is already on", "info") : enable(ctx);
        case "off":
          return state.enabled ? disable(ctx) : ctx.ui.notify("Gameplan mode is already off", "info");
        case "dir": {
          const dir = rest.join(" ").trim();
          if (!dir) return ctx.ui.notify(`Gameplan directory: ${state.gameplanDir}`, "info");
          state = { ...state, gameplanDir: dir };
          persist();
          return ctx.ui.notify(`Gameplan directory set to ${dir}`, "info");
        }
        case "status": {
          const lines = [
            `mode: ${state.enabled ? "on" : "off"}`,
            `directory: ${state.gameplanDir}`,
            `skill: ${skill.dir}`,
            `python: ${python.join(" ")}`,
            `gameplan file: ${state.gameplanPath ?? "not yet written"}`,
            `missing required tools: ${missingRequired().length === 0 ? "none" : describeGroups(missingRequired())}`,
            `missing optional tools: ${missingRecommended().length === 0 ? "none" : describeGroups(missingRecommended())}`,
            `decision dialog: ${hasTool(ASK_USER_TOOL) ? ASK_USER_TOOL : "built-in selector"}`,
            ...state.decisions.map((d) => `${d.id} (${d.kind}): ${d.choice}`),
          ];
          if (state.finalizedPath) lines.push(`finalized: ${state.finalizedPath}`);
          return ctx.ui.notify(lines.join("\n"), "info");
        }
        default:
          return ctx.ui.notify("Usage: /gameplan [on|off|status|dir <path>]", "error");
      }
    },
  });

  // Writes are not policed: the mode exists to record decisions and gate finalize, not to stop
  // the agent from editing the workstream or anything else. A hand edit of a gameplan document
  // just becomes the remembered gameplan file.
  pi.on("tool_call", async (event, ctx) => {
    if (!state.enabled || !WRITE_TOOLS.has(event.toolName)) return;
    const target = String((event.input as { path?: unknown }).path ?? "");
    if (isGameplanWrite(ctx.cwd, state.gameplanDir, target)) {
      if (state.finalizedPath && resolve(ctx.cwd, target) === resolve(ctx.cwd, state.finalizedPath)) clearFinalized(ctx);
      rememberGameplanPath(relative(ctx.cwd, resolve(ctx.cwd, target)));
    }
  });

  pi.on("tool_result", async (_event, ctx) => { await refreshFinalized(ctx); });

  // Context for every user-started run while the mode is on.
  pi.on("before_agent_start", async (event, ctx) => {
    await refreshFinalized(ctx);
    if (!state.enabled || event.messageType !== "user") return;
    const ledger =
      state.decisions.length === 0
        ? "none yet"
        : state.decisions.map((d) => `${d.id} (${d.kind}): ${d.choice}`).join("\n");
    const notes: string[] = [];
    notes.push(
      state.gameplanPath
        ? `- The gameplan file is ${state.gameplanPath}. It is the source of truth; read it before editing rather than relying on memory of earlier turns.`
        : `- No gameplan file exists yet. Write it to ${state.gameplanDir}/<project-name>.yaml.`,
    );
    const requiredMissing = missingRequired();
    notes.push(
      requiredMissing.length === 0
        ? "- Where the skill says WebFetch, use fetch_content; where it says WebSearch, use web_search."
        : `- Web tools the skill expects are not available (${describeGroups(requiredMissing)}). Do not cite precedent URLs or external docs you could not fetch; mark them as unverified or leave url null.`,
    );
    notes.push(
      hasTool("lsp_navigation")
        ? "- Where the skill says LSP, use lsp_navigation (and ast_grep_search for structural search)."
        : "- There is no LSP tool here: ground efficacy with grep, ast-grep, and the call-graph tools the skill lists.",
    );
    notes.push(
      hasTool("subagent")
        ? "- Use the subagent tool (scout, researcher) for independent research such as reachability traces and prior-milestone grounding; keep architectural consultation in this session."
        : "- No subagent tool is installed; do research sequentially in this session.",
    );
    notes.push(
      hasTool(ASK_USER_TOOL)
        ? `- For scope and open-question clarification that is not an architectural decision, use ${ASK_USER_TOOL}. Architectural decisions still go through gameplan_decide, which records the answer.`
        : "- For scope and open-question clarification, ask in chat and wait for the answer. Architectural decisions go through gameplan_decide.",
    );
    const toolNote = notes.join("\n");
    return {
      message: {
        customType: "gameplan-mode-context",
        display: false,
        content: `[GAMEPLAN MODE ACTIVE]
You are authoring a gameplan for this repository, not implementing changes.

- Follow the write-gameplan skill. If its instructions are not already in this session, read ${skill.skillMd} now. The JSON Schema it names is the sole shape contract.
- Build the gameplan with gameplan_write (top-level sections, a few per call), gameplan_write_patch (one patch per call, spec may be "" at first), and gameplan_write_spec (one Pantagruel spec per call, parsed by pant before it lands) at ${state.gameplanDir}/<project-name>.yaml. Do not write the YAML by hand; the tools validate against the schema and keep the file canonical. Use bash freely to inspect, search, build, and test.
- For every architectural choice with meaningful alternatives, call gameplan_decide and wait for the answer before designing anything that depends on it. In architectureDesign.decisions, list each answered decision as { id, rationale }; choice, alternatives, kind, and evidence are filled from the recorded answer and cannot be authored by you. Decisions you assert yourself must be constrained (evidence and rationale) and the engineer will be asked to accept each one at finalize.
- Each write reports which required sections and referenced patches are still missing. When none are, call gameplan_check and fix every error it reports. Checking earlier only returns INCOMPLETE with that same list.
- When gameplan_check passes and openQuestions is empty, call gameplan_finalize.
- Finalization completes only the current plan, not the session. When the user requests a different gameplan, call gameplan_new with a fresh path before recording decisions or writing it. This clears the active ledger without editing the previous file. Use gameplan_load instead to explicitly resume an existing plan.
${toolNote}

Decisions recorded so far:
${ledger}`,
      },
    };
  });

  pi.registerTool({
    name: "gameplan_new",
    label: "New gameplan",
    description: "Start a new independent gameplan, including after finalizing another in this session. Requires an unused path and engineer confirmation. Clears the active decision ledger and finalization marker, preserves mode settings, and leaves all files unchanged. Use gameplan_load to resume an existing plan instead.",
    promptSnippet: "Start a fresh gameplan without inheriting the previous plan's decisions",
    exposure: "model-only",
    executionMode: "sequential",
    parameters: Type.Object({ path: Type.String({ minLength: 1, description: "Unused gameplan path within the configured gameplan directory" }) }),
    async execute(_id, params, signal, _onUpdate, ctx) {
      requireMode();
      signal?.throwIfAborted();
      if (!ctx.hasUI) throw new Error("Starting a gameplan requires engineer confirmation in an interactive UI");
      const before = state;
      const admission = await resolveGameplanWrite(ctx.cwd, before.gameplanDir, params.path);
      if (existsSync(admission.path)) throw new Error("Gameplan already exists; choose a fresh path or use gameplan_load");
      const accepted = await ctx.ui.confirm("Start a new gameplan?", `${params.path}\nReplace the active ledger (${before.decisions.length} decisions) with an empty draft? Previous gameplan files remain unchanged.`);
      signal?.throwIfAborted();
      if (!accepted) return { content: [{ type: "text", text: "New gameplan cancelled; state unchanged." }], details: { started: false } };
      const current = await resolveGameplanWrite(ctx.cwd, before.gameplanDir, params.path);
      if (state !== before || current.path !== admission.path || current.device !== admission.device || current.inode !== admission.inode || existsSync(current.path)) {
        throw new Error("Gameplan path or mode state changed during confirmation; retry with a fresh path");
      }
      state = startGameplan(state, params.path);
      persist();
      updateStatus(ctx);
      return { content: [{ type: "text", text: `Started a fresh draft at ${params.path} with no recorded decisions. Previous files unchanged; record new choices with gameplan_decide and create the file with gameplan_write.` }], details: { started: true, path: params.path } };
    },
  });

  pi.registerTool({
    name: "gameplan_load",
    label: "Load gameplan",
    description: "Load an existing gameplan into mode state after engineer confirmation. Replaces the session decision ledger and active file path, clears finalization, and leaves the file unchanged. Imported approval evidence is trusted only after confirmation; constrained decisions still require acceptance at finalize. Run gameplan_check afterward for full validation.",
    exposure: "model-only",
    executionMode: "sequential",
    parameters: Type.Object({ path: Type.String({ description: "Existing gameplan path within the configured gameplan directory" }) }),
    async execute(_id, params, signal, _onUpdate, ctx) {
      requireMode();
      signal?.throwIfAborted();
      if (!ctx.hasUI) throw new Error("Loading a gameplan requires engineer confirmation in an interactive UI");
      const bytes = await readGameplanSnapshot(ctx.cwd, state.gameplanDir, params.path);
      const records = importDecisions(parseYaml(bytes.toString("utf8")), validator(), new Date().toISOString());
      const before = state;
      const accepted = await ctx.ui.confirm("Load gameplan?", `${params.path}\nReplace the current ledger (${state.decisions.length} decisions) and clear finalization?\nConfirm importing these recorded answers and their evidence:\n${JSON.stringify(records, null, 2)}\nConstrained decisions remain subject to finalize confirmation.`);
      signal?.throwIfAborted();
      if (!accepted) return { content: [{ type: "text", text: "Load cancelled; state unchanged." }], details: { loaded: false } };
      const current = await readGameplanSnapshot(ctx.cwd, state.gameplanDir, params.path);
      if (state !== before || !bytes.equals(current)) throw new Error("Gameplan or mode state changed during confirmation; load again");
      state = { ...state, decisions: records, gameplanPath: params.path, finalizedPath: undefined, finalizedDigest: undefined };
      persist();
      updateStatus(ctx);
      return { content: [{ type: "text", text: `Loaded ${params.path} with ${records.length} ledger decisions. File unchanged; run gameplan_check next.` }], details: { loaded: true, path: params.path, decisions: records } };
    },
  });

  pi.registerTool({
    name: "gameplan_remove_decision",
    label: "Remove gameplan decision",
    description: "Remove a decision from the session ledger after engineer confirmation and clear finalization. Does not edit the YAML: remove the corresponding decision, open question, and stale references with gameplan_write afterward. Unknown IDs are a no-op.",
    exposure: "model-only",
    executionMode: "sequential",
    parameters: Type.Object({ id: Type.String({ minLength: 1, description: "Exact decision id to remove" }) }),
    async execute(_id, params, signal, _onUpdate, ctx) {
      requireMode();
      signal?.throwIfAborted();
      const record = state.decisions.find((d) => d.id === params.id);
      if (!record) return { content: [{ type: "text", text: `${params.id} is not in the ledger; nothing changed.` }], details: { removed: false } };
      if (!ctx.hasUI) throw new Error("Removing a decision requires engineer confirmation in an interactive UI");
      const before = state;
      const accepted = await ctx.ui.confirm("Remove gameplan decision?", `${record.id}: ${record.question}\n${record.kind}: ${record.choice}\nRemove from the ledger and clear finalization? The YAML will not be edited.`);
      signal?.throwIfAborted();
      if (!accepted) return { content: [{ type: "text", text: "Removal cancelled; state unchanged." }], details: { removed: false } };
      if (state !== before) throw new Error("Mode state changed during confirmation; retry removal");
      state = removeDecision(state, params.id);
      persist();
      updateStatus(ctx);
      return { content: [{ type: "text", text: `${params.id} removed from the ledger. Use gameplan_write to remove its YAML entry and any stale references or open questions.` }], details: { removed: true, id: params.id } };
    },
  });

  pi.registerTool({
    name: "gameplan_decide",
    label: "Gameplan decision",
    description:
      "Ask the engineer to resolve one architectural decision for the gameplan being authored. Presents the question with the offered choices in a dialog; records the answer and returns the exact YAML resolution block to paste into architectureDesign.decisions. This is the only source of engineer_approved and delegated resolutions.",
    promptSnippet: "Ask the engineer to resolve an architectural decision and record the answer",
    promptGuidelines: [
      "Call gameplan_decide for each choice that changes ownership, state lifetime, execution semantics, validation boundaries, or data flow. Ask one decision at a time when answers depend on each other.",
      "Present real alternatives with concrete tradeoffs from the inspected code, and state a recommendation. Do not offer straw options.",
      "Paste the returned YAML block verbatim. Write the rationale yourself; never alter choice, kind, or evidence.",
      "Free-text responses request discussion, not approval. Address the engineer's comments, keep the AD unresolved, and call gameplan_decide again with the same id and updated options when ready for an explicit choice.",
    ],
    exposure: "model-only",
    executionMode: "sequential",
    parameters: Type.Object({
      id: Type.String({ pattern: "^AD-[0-9]+$", description: "Decision id, AD-N, unique within the gameplan" }),
      topic: Type.String({ description: "Short label: ownership, persistence, execution, validation, data flow, ..." }),
      question: Type.String({ description: "The architectural question, one sentence" }),
      options: Type.Array(
        Type.Object({
          choice: Type.String({ description: "A credible design, one sentence" }),
          tradeoffs: Type.String({ description: "What this choice costs and buys in this codebase" }),
        }),
        { minItems: 2, maxItems: 5 },
      ),
      recommendation: Type.Integer({ minimum: 0, description: "Index into options of the recommended choice" }),
      why: Type.String({ description: "Why the recommendation fits, grounded in inspected code" }),
    }),
    async execute(_id, params, signal, _onUpdate, ctx) {
      requireMode();
      if (params.recommendation >= params.options.length) {
        throw new Error(`recommendation ${params.recommendation} is out of range for ${params.options.length} options`);
      }
      signal?.throwIfAborted();
      const answer = await askDecision(ctx, params, hasTool(ASK_USER_TOOL), signal);
      signal?.throwIfAborted();
      const record = resolveDecision(params, answer, new Date());
      clearFinalized(ctx);
      state = recordDecision(state, record);
      persist();
      updateStatus(ctx);
      const text =
        record.kind === "unresolved"
          ? `${record.id} is UNRESOLVED: ${record.reason} This is not approval. Address any questions or proposed alternatives in conversation, then call gameplan_decide again with the same id when ready for an explicit choice. Keep it in openQuestions, leave the dependent design provisional, and include { id: "${record.id}" } in architectureDesign.decisions so the file records it as unresolved.`
          : `${record.id} recorded as ${record.kind}: ${JSON.stringify(record.choice)}\n\nOn your next gameplan_write, include { id: "${record.id}", rationale: "<why this choice fits this codebase>" } in architectureDesign.decisions. Choice, alternatives, kind, and evidence are filled from this record.`;
      return { content: [{ type: "text", text }], details: record };
    },
    renderCall(args, theme) {
      const lines = [theme.fg("toolTitle", theme.bold(`${args.id} · ${args.topic}`)), theme.fg("text", args.question), ""];
      args.options.forEach((option, index) => {
        const marker = index === args.recommendation ? theme.fg("accent", "★") : theme.fg("muted", "•");
        lines.push(`${marker} ${index + 1}. ${option.choice}`);
        lines.push(theme.fg("muted", `     ${option.tradeoffs}`));
      });
      lines.push("", theme.fg("muted", `recommendation: ${args.why}`));
      return new Text(lines.join("\n"), 0, 0);
    },
    renderResult(result, _options, theme) {
      const record = result.details as DecisionRecord | undefined;
      if (!record) return new Text(textOf(result.content), 0, 0);
      const color = record.kind === "unresolved" ? "warning" : "success";
      return new Text(theme.fg(color, `${record.id} ${record.kind}: ${record.reason ?? record.choice}`), 0, 0);
    },
  });

  pi.registerTool({
    name: "gameplan_write",
    label: "Gameplan write",
    description:
      "Write top-level sections of the gameplan file. Pass one or more sections as JSON matching the write-gameplan JSON Schema; each is validated and replaces the section in the file (other sections and comments are kept). For architectureDesign.decisions, list each engineer-answered decision as { id, rationale }: choice, alternatives, kind, and evidence are filled from the recorded answer. Decisions you assert yourself must be constrained (with evidence and rationale) or unresolved. Use gameplan_write_patch for entries of patches.",
    promptSnippet: "Write validated gameplan sections to the YAML file",
    promptGuidelines: [
      "Build the gameplan with gameplan_write and gameplan_write_patch instead of writing YAML by hand; they validate shape and compose decisions from the engineer's answers.",
      "Write a few related sections per call. Fix every reported error before moving on.",
    ],
    executionMode: "sequential",
    parameters: Type.Object({
      path: Type.Optional(Type.String({ description: "Gameplan file path relative to the project root, e.g. gameplans/<project-name>.yaml. Defaults to the file this session is working on." })),
      sections: Type.Record(Type.String(), Type.Unknown(), {
        description: "Top-level section name to value, e.g. { problemStatement: \"...\", functionalChanges: [...] }. Values follow references/gameplan-schema.json.",
      }),
    }),
    async execute(_id, params, signal, _onUpdate, ctx) {
      requireMode();
      const path = resolveGameplanPath(params.path);
      const v = validator();
      const sections: JsonObject = { ...params.sections };
      const errors: string[] = [];
      if (Object.keys(sections).length === 0) throw new Error("sections is empty; pass at least one top-level section.");
      if ("architectureDesign" in sections) {
        const composed = composeDecisions(sections.architectureDesign, state.decisions);
        sections.architectureDesign = composed.design;
        errors.push(...composed.errors);
      }
      for (const [name, value] of Object.entries(sections)) {
        if (!v.sections.includes(name)) {
          errors.push(`${name}: not a gameplan section (valid: ${v.sections.join(", ")})`);
          continue;
        }
        if (name === "patches") {
          errors.push("patches: write each patch with gameplan_write_patch");
          continue;
        }
        errors.push(...v.section(name, value));
      }
      if (errors.length > 0) throw new Error(`Nothing written; fix these:\n${errors.map((e) => `- ${e}`).join("\n")}`);
      const result = await writeGameplan(ctx, path, signal, (doc) => applySections(doc, sections, v.sections));
      const names = Object.keys(sections);
      return {
        content: [{ type: "text", text: describeWrite(result, `Wrote ${names.length} section${names.length === 1 ? "" : "s"} (${names.join(", ")})`) }],
        details: { path, sections: names, missing: result.missing },
      };
    },
    renderResult(result, _options, theme) {
      return new Text(theme.fg(result.isError ? "error" : "success", textOf(result.content).split("\n")[0] ?? ""), 0, 0);
    },
  });

  pi.registerTool({
    name: "gameplan_write_patch",
    label: "Gameplan write patch",
    description:
      "Insert or replace one entry of the gameplan's patches array, matched by number. The patch is validated against the write-gameplan JSON Schema's patch definition before the file is touched; the array stays sorted by number.",
    promptSnippet: "Write one validated patch into the gameplan file",
    executionMode: "sequential",
    parameters: Type.Object({
      path: Type.Optional(Type.String({ description: "Gameplan file path relative to the project root. Defaults to the file this session is working on." })),
      patch: Type.Object({ number: Type.Union([Type.Integer(), Type.String()]) }, {
        additionalProperties: true,
        description: "A complete patch object per references/gameplan-schema.json ($defs.patch): number, classification, complexity, title, files, changes, requiredContext, testStubsIntroduced, testStubsImplemented, spec, optional precedents. spec may be \"\" here and supplied through gameplan_write_spec, which parses it.",
      }),
    }),
    async execute(_id, params, signal, _onUpdate, ctx) {
      requireMode();
      const path = resolveGameplanPath(params.path);
      const errors = validator().patch(params.patch);
      if (errors.length > 0) throw new Error(`Nothing written; fix these:\n${errors.map((e) => `- ${e}`).join("\n")}`);
      let outcome: "added" | "replaced" = "added";
      const result = await writeGameplan(ctx, path, signal, (doc) => {
        outcome = upsertPatch(doc, params.patch as JsonObject);
      });
      return {
        content: [{ type: "text", text: describeWrite(result, `Patch ${String(params.patch.number)} ${outcome}`) }],
        details: { path, number: params.patch.number, outcome },
      };
    },
    renderResult(result, _options, theme) {
      return new Text(theme.fg(result.isError ? "error" : "success", textOf(result.content).split("\n")[0] ?? ""), 0, 0);
    },
  });

  /** Run `pant` on a spec written to a temp file. Returns diagnostics text, or undefined when it passes. */
  async function runPant(spec: string, args: string[], cwd: string, signal: AbortSignal | undefined): Promise<{ ok: boolean; output: string }> {
    const dir = await mkdtemp(join(tmpdir(), "pi-gameplan-spec-"));
    try {
      const file = join(dir, "spec.pant");
      await writeFile(file, spec, "utf8");
      const result = await pi.exec("pant", [...args, file], { cwd, signal, timeout: 120_000 });
      if (result.killed) return { ok: false, output: "pant timed out" };
      return { ok: result.code === 0, output: `${result.stdout}\n${result.stderr}`.trim() };
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  }

  pi.registerTool({
    name: "gameplan_write_spec",
    label: "Gameplan write spec",
    description:
      "Write one Pantagruel spec into the gameplan: a patch's spec (by patch number) or finalStateSpec. The spec is parsed with pant first and nothing is written on a parse error; diagnostics quote the offending line. The module name is compared with the skill's convention (<PROJECT>_PATCH_<N> or <PROJECT>). With check: true, pant --check also runs SMT verification (contradictions, invariants, preconditions) and blocks the write on failure.",
    promptSnippet: "Parse a Pantagruel spec with pant and write it into a patch or finalStateSpec",
    promptGuidelines: [
      "Write every spec with gameplan_write_spec so pant parses it before it lands.",
      "pant cannot construct, compare, or test membership of a sum-typed value (`A + B`): a rule returning a sum can appear in no proposition, so `decode d in Malformed` and `decode d = m` are type errors. Model an outcome as a domain with one nullary rule per arm (`Outcome. ok => Outcome. invalid => Outcome.`), assert the arms distinct (`ok ~= invalid.`), close the domain (`all x: Outcome | x = ok or x = invalid.`), and constrain the rule with `cond … , true => …` over those values.",
      "Use check: true for specs with cond case analysis or outcome enums. Put enum assertions after the chapter separator. The default pant bound is 3; an isolated closed two-arm enum needs an isolated enum-only pant --check --bound 2 run to avoid a domain-size contradiction. This bound limits every domain; a whole-spec bound-2 pass can miss larger counterexamples and must be reported as reduced coverage.",
    ],
    executionMode: "sequential",
    parameters: Type.Object({
      path: Type.Optional(Type.String({ description: "Gameplan file path relative to the project root. Defaults to the file this session is working on." })),
      patch: Type.Optional(Type.Union([Type.Integer(), Type.String()], { description: "Patch number whose spec to set. Omit to set finalStateSpec." })),
      spec: Type.String({ description: "Pantagruel source, starting with `module <NAME>.`" }),
      check: Type.Optional(Type.Boolean({ description: "Also run pant --check (SMT). Default false." })),
    }),
    async execute(_id, params, signal, _onUpdate, ctx) {
      requireMode();
      const path = resolveGameplanPath(params.path);
      const spec = normalizeSpec(params.spec);
      if (!spec.trim()) throw new Error("spec is empty.");
      const target: SpecTarget = params.patch === undefined ? { kind: "final" } : { kind: "patch", number: String(params.patch) };

      const parsed = await runPant(spec, [], ctx.cwd, signal);
      if (!parsed.ok) {
        throw new Error(`Nothing written; pant rejected the spec:\n${describeDiagnostics(spec, parsePantDiagnostics(parsed.output))}`);
      }
      const notes: string[] = [];
      const json = await runPant(spec, ["--json"], ctx.cwd, signal);
      const moduleName = json.ok ? moduleNameFromJson(json.output) : undefined;
      if (params.check) {
        const checked = await runPant(spec, ["--check"], ctx.cwd, signal);
        if (!checked.ok) throw new Error(`Nothing written; pant --check failed:\n${describeDiagnostics(spec, parsePantDiagnostics(checked.output))}`);
        notes.push("SMT check passed.");
      }

      let project: string | undefined;
      const result = await writeGameplan(ctx, path, signal, (doc) => {
        project = projectName(doc);
        if (target.kind === "final") {
          applySections(doc, { finalStateSpec: spec }, validator().sections);
        } else if (!setPatchSpec(doc, target.number, spec)) {
          throw new Error(`No patch ${target.number} in ${path}; write it first with gameplan_write_patch.`);
        }
      });
      if (project) {
        const expected = expectedModuleName(project, target);
        if (moduleName === undefined) notes.push(`Warning: the spec declares no module; the skill expects \`module ${expected}.\``);
        else if (moduleName !== expected) notes.push(`Warning: module is ${moduleName}; the skill expects ${expected}.`);
      }
      const what = target.kind === "final" ? "finalStateSpec parsed and written" : `Spec for patch ${target.number} parsed and written`;
      return {
        content: [{ type: "text", text: [describeWrite(result, what), ...notes].join("\n") }],
        details: { path, target, module: moduleName, checked: params.check === true },
      };
    },
    renderResult(result, _options, theme) {
      return new Text(theme.fg(result.isError ? "error" : "success", textOf(result.content).split("\n")[0] ?? ""), 0, 0);
    },
  });

  const checkOutput = Type.Object({
    ok: Type.Boolean(),
    validatorErrors: Type.Array(Type.String()),
    decisionErrors: Type.Array(Type.String()),
    openQuestions: Type.Array(Type.String()),
    constrained: Type.Array(Type.String()),
    fatal: Type.Optional(Type.String()),
    incomplete: Type.Optional(Type.Object({ missingSections: Type.Array(Type.String()), unwrittenPatches: Type.Array(Type.String()) })),
  });

  interface CheckDetails {
    path: string;
    validation: ValidationReport;
    audit?: AuditResult;
    /** Set when the document is not complete enough to validate meaningfully; the validator did not run. */
    incomplete?: { missingSections: string[]; unwrittenPatches: string[] };
  }

  /** Completeness gate: an incomplete document yields a cascade of dangling-reference errors, so say what is missing instead. */
  async function incompleteness(ctx: ExtensionToolContext, path: string): Promise<CheckDetails["incomplete"]> {
    const abs = resolve(ctx.cwd, path);
    if (!existsSync(abs)) return undefined;
    try {
      const doc = loadGameplanDocument(await readFile(abs, "utf8"));
      const missingSectionsList = missingSections(doc, validator().sections);
      const unwrittenPatches = unwrittenReferencedPatches(doc);
      if (missingSectionsList.length === 0 && unwrittenPatches.length === 0) return undefined;
      return { missingSections: missingSectionsList, unwrittenPatches };
    } catch {
      return undefined; // Let the validator report the parse problem.
    }
  }

  async function check(ctx: ExtensionToolContext, path: string, signal: AbortSignal | undefined): Promise<CheckDetails> {
    const incomplete = await incompleteness(ctx, path);
    if (incomplete) return { path, validation: { ok: false, errors: [] }, incomplete };
    const validation = await runValidator((c, a, o) => pi.exec(c, a, o), skill, python, path, { cwd: ctx.cwd, signal });
    let audit: AuditResult | undefined;
    if (!validation.fatal) {
      try {
        const doc: unknown = parseYaml(await readFile(resolve(ctx.cwd, path), "utf8"));
        audit = auditDecisions(doc, state.decisions);
      } catch (error) {
        validation.fatal = `could not parse gameplan for the decision audit: ${error instanceof Error ? error.message : String(error)}`;
        validation.ok = false;
      }
    }
    return { path, validation, audit };
  }

  type Json = string | number | boolean | null | Json[] | { [key: string]: Json };

  function summarize(details: CheckDetails): { ok: boolean; text: string; structured: { [key: string]: Json } } {
    const { validation, audit } = details;
    const decisionErrors = audit?.errors ?? [];
    const ok = validation.ok && decisionErrors.length === 0;
    const lines: string[] = [];
    if (details.incomplete) {
      const { missingSections: ms, unwrittenPatches: up } = details.incomplete;
      lines.push(`INCOMPLETE: ${details.path} was not validated.`);
      if (ms.length > 0) lines.push(`Required sections not written yet: ${ms.join(", ")}.`);
      if (up.length > 0) lines.push(`Patches referenced but not yet written: ${up.join(", ")}.`);
      lines.push("Finish them with gameplan_write, gameplan_write_patch, and gameplan_write_spec, then run gameplan_check.");
      const structured: { [key: string]: Json } = {
        ok: false,
        validatorErrors: [],
        decisionErrors: [],
        openQuestions: [],
        constrained: [],
        incomplete: { missingSections: ms, unwrittenPatches: up },
      };
      return { ok: false, text: lines.join("\n"), structured };
    }
    if (validation.fatal) lines.push(`FATAL: ${validation.fatal}`);
    for (const e of validation.errors) lines.push(`validator: ${e}`);
    for (const e of decisionErrors) lines.push(`decision: ${e}`);
    if (ok) {
      lines.push(`PASS: ${details.path}`);
      if (audit && audit.openQuestions.length > 0) {
        lines.push(`${audit.openQuestions.length} open question(s) remain; resolve them with the engineer before finalizing.`);
      }
      if (audit && audit.constrained.length > 0) {
        lines.push(`${audit.constrained.length} constrained decision(s) will need the engineer's acceptance at finalize.`);
      }
    } else if (validation.fatal) {
      lines.unshift(`FATAL: ${details.path} could not be fully checked`);
    } else {
      lines.unshift(`FAIL: ${details.path} (${validation.errors.length + decisionErrors.length} error(s))`);
    }
    const structured: { [key: string]: Json } = {
      ok,
      validatorErrors: validation.errors,
      decisionErrors,
      openQuestions: audit?.openQuestions ?? [],
      constrained: audit?.constrained.map((c) => c.id) ?? [],
    };
    if (validation.fatal) structured.fatal = validation.fatal;
    return { ok, text: lines.join("\n"), structured };
  }

  pi.registerTool({
    name: "gameplan_check",
    label: "Gameplan check",
    description:
      "Validate a complete gameplan file with the write-gameplan skill's validator (schema, formatting, specs, routing, dependency graph, reachability) and audit every architectural resolution against the engineer's recorded answers. Returns PASS or the list of errors to fix. On a document that is still missing required sections or referenced patches it returns INCOMPLETE with what to write next instead of running the validator.",
    promptSnippet: "Validate a complete gameplan and audit its decisions against the engineer's answers",
    promptGuidelines: [
      "Run gameplan_check once the write tools report that all required sections and referenced patches are present, and again after any later edit. On a half-written gameplan it only returns INCOMPLETE with the same missing list the write tools already gave, so finish writing first unless the engineer asks for a check.",
    ],
    parameters: Type.Object({
      path: Type.Optional(
        Type.String({ description: "Gameplan file path relative to the project root, e.g. gameplans/<name>.yaml. Defaults to the file this session last wrote or checked." }),
      ),
    }),
    outputSchema: checkOutput,
    annotations: { readOnlyHint: true },
    async execute(_id, params, signal, _onUpdate, ctx) {
      requireMode();
      const path = resolveGameplanPath(params.path);
      rememberGameplanPath(path);
      const details = await check(ctx, path, signal);
      const summary = summarize(details);
      return {
        content: [{ type: "text", text: summary.text }],
        structuredContent: summary.structured,
        details,
        isError: !summary.ok,
      };
    },
    renderResult(result, _options, theme) {
      const details = result.details as CheckDetails | undefined;
      const text = textOf(result.content);
      const first = text.split("\n")[0] ?? "";
      const ok = details ? details.validation.ok && (details.audit?.errors.length ?? 0) === 0 : !result.isError;
      return new Text(theme.fg(ok ? "success" : "error", first), 0, 0);
    },
  });

  pi.registerTool({
    name: "gameplan_finalize",
    label: "Gameplan finalize",
    description:
      "Finalize a validated gameplan: refuses while the validator fails, a decision is unresolved, or openQuestions is non-empty; asks the engineer to accept every constrained decision and to confirm; then formats the file in place and re-validates.",
    promptSnippet: "Finalize a validated gameplan with the engineer's confirmation",
    promptGuidelines: ["Call gameplan_finalize only after gameplan_check passes and openQuestions is empty."],
    exposure: "model-only",
    executionMode: "sequential",
    parameters: Type.Object({
      path: Type.Optional(Type.String({ description: "Gameplan file path relative to the project root. Defaults to the file this session last wrote or checked." })),
    }),
    async execute(_id, rawParams, signal, _onUpdate, ctx) {
      requireMode();
      if (!ctx.hasUI) throw new Error("gameplan_finalize needs an interactive engineer; no UI is available.");
      const params = { path: resolveGameplanPath(rawParams.path) };
      const admission = await resolveGameplanWrite(ctx.cwd, state.gameplanDir, params.path);
      const finalPath = admission.path;
      rememberGameplanPath(params.path);
      const approvedBytes = await readFile(finalPath);
      const approvedText = approvedBytes.toString("utf8");
      const before = await check(ctx, finalPath, signal);
      const summary = summarize(before);
      if (!summary.ok) throw new Error(`Cannot finalize; gameplan_check fails:\n${summary.text}`);
      const audit = before.audit;
      if (!audit) throw new Error("Cannot finalize; the decision audit did not run.");
      if (audit.openQuestions.length > 0) {
        throw new Error(`Cannot finalize; openQuestions still lists:\n${audit.openQuestions.map((q) => `- ${q}`).join("\n")}\nResolve each with the engineer, then remove it.`);
      }
      const unresolved = state.decisions.filter((d) => d.kind === "unresolved");
      if (unresolved.length > 0) {
        throw new Error(`Cannot finalize; unresolved decisions: ${unresolved.map((d) => d.id).join(", ")}. Ask again with gameplan_decide.`);
      }
      if (audit.constrained.length > 0) {
        const list = audit.constrained.map((c) => `${c.id}: ${c.choice}\n   constraint: ${c.evidence}`).join("\n");
        const accepted = await ctx.ui.confirm(
          `Accept ${audit.constrained.length} agent-asserted constrained decision(s)?`,
          `The agent claims binding constraints leave no alternative for:\n${list}`,
        );
        if (!accepted) {
          throw new Error(
            `Engineer did not accept the constrained decisions (${audit.constrained.map((c) => c.id).join(", ")}). Present each as a real choice with gameplan_decide.`,
          );
        }
      }
      const counts = countKinds(state.decisions);
      const confirmed = await ctx.ui.confirm(
        `Finalize ${params.path}?`,
        `${counts}\nThe file will be formatted in place and re-validated.`,
      );
      if (!confirmed) throw new Error("Engineer declined to finalize. Ask what should change.");

      if (!(await readFile(finalPath)).equals(approvedBytes)) {
        throw new Error("Cannot finalize; the gameplan changed during confirmation. Review it and finalize again.");
      }
      const formatted = await formatText(ctx, approvedText, signal);
      if (formatted.error) throw new Error(`Formatter failed: ${formatted.error}`);
      await commitGameplan(admission, formatted.text, approvedBytes);
      clearFinalized(ctx);
      const after = await check(ctx, finalPath, signal);
      const final = summarize(after);
      if (!final.ok) throw new Error(`Validation failed after formatting:\n${final.text}`);

      if (!after.audit || after.audit.openQuestions.length > 0 || state.decisions.some((d) => d.kind === "unresolved")) {
        throw new Error("Cannot finalize; open questions or unresolved decisions remain after formatting.");
      }
      if (JSON.stringify(after.audit.constrained) !== JSON.stringify(audit.constrained)) {
        throw new Error("Cannot finalize; constrained decisions changed. Review and approve them again.");
      }
      const finalBytes = await readGameplanSnapshot(ctx.cwd, state.gameplanDir, params.path);
      const finalText = finalBytes.toString("utf8");
      if (JSON.stringify(parseYaml(finalText)) !== JSON.stringify(parseYaml(approvedText))) {
        throw new Error("Cannot finalize; the document changed after confirmation. Review and finalize again.");
      }
      state = { ...state, finalizedPath: params.path, finalizedDigest: digest(finalBytes) };
      persist();
      updateStatus(ctx);
      const abs = resolve(ctx.cwd, params.path);
      return {
        content: [
          {
            type: "text",
            text: `Finalized ${params.path} (${counts}).\nTo execute it: onton --gameplan ${relative(ctx.cwd, abs)} (run from ${ctx.cwd}).\nIf this gameplan belongs to a workstream, propose the write-back to the engineer now and apply it to the workstream file once approved.`,
          },
        ],
        details: { path: params.path, finalizedAt: new Date().toISOString() },
      };
    },
  });

  pi.on("session_start", async (_event, ctx) => {
    state = fromEntries(ctx.sessionManager.getBranch());
    await refreshFinalized(ctx);
    if (pi.getFlag("gameplan") === true && !state.enabled) {
      state = { ...state, enabled: true, toolsBeforeMode: pi.getActiveTools() };
      persist();
    }
    if (state.enabled) activateTools();
    else deactivateTools();
    const missing = missingSkillFiles(skill);
    if (missing.length > 0 && state.enabled) {
      ctx.ui.notify(`gameplan: skill files not found (${missing.join(", ")}). Set ONTON_GAMEPLAN_SKILL_DIR.`, "error");
    }
    if (state.enabled) reportMissingTools(ctx);
    updateStatus(ctx);
  });

  pi.on("session_shutdown", async (_event, ctx) => {
    ctx.ui.setStatus("gameplan", undefined);
  });
}

async function askDecision(ctx: ExtensionToolContext, params: DecideParams, askUserAvailable: boolean, signal: AbortSignal | undefined): Promise<Answer> {
  if (!ctx.hasUI) return { kind: "no-ui" };
  if (askUserAvailable && fitsAskUser(params)) {
    const outcome = await ctx.executeTool(ASK_USER_TOOL, buildAskUserRequest(params), { signal });
    // A tool failure (not a decline) falls back to the built-in selector so the engineer is still asked.
    if (!outcome.isError) return answerFromAskUser(params, outcome.result.details);
  }
  const labels = params.options.map((o, i) => `${i + 1}. ${o.choice}${i === params.recommendation ? " (recommended)" : ""}`);
  const picked = await ctx.ui.select(`${params.id} · ${params.topic}: ${params.question}`, [...labels, OTHER_LABEL, DELEGATE_LABEL]);
  if (picked === undefined) return { kind: "cancelled" };
  if (picked === OTHER_LABEL) {
    const text = await ctx.ui.editor(`${params.id}: questions or alternatives to discuss (not approval)`, "");
    if (!text?.trim()) return { kind: "cancelled" };
    return { kind: "discussion", text };
  }
  if (picked === DELEGATE_LABEL) {
    const scope = (await ctx.ui.input(`${params.id}: scope of the delegation (optional)`, "e.g. any option that keeps the request path synchronous")) ?? "";
    return { kind: "delegated", scope };
  }
  const index = labels.indexOf(picked);
  if (index < 0) return { kind: "cancelled" };
  return { kind: "selected", index };
}

function countKinds(decisions: readonly DecisionRecord[]): string {
  const by = { engineer_approved: 0, delegated: 0, unresolved: 0 };
  for (const d of decisions) by[d.kind] += 1;
  return `${decisions.length} decision(s): ${by.engineer_approved} engineer-approved, ${by.delegated} delegated, ${by.unresolved} unresolved`;
}

function textOf(content: { type: string; text?: string }[]): string {
  return content
    .filter((c) => c.type === "text")
    .map((c) => c.text ?? "")
    .join("\n");
}

export { DEFAULT_GAMEPLAN_DIR };
