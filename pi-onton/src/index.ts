import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";
import { readFile } from "node:fs/promises";
import { decodeDelivery, instructions, responsePath } from "./protocol.js";
import { artifactPath, readChunk, writeResponse } from "./files.js";

export default function (pi: ExtensionAPI) {
  // A globally installed package is inert outside supervised Onton processes.
  const contextPath = process.env.ONTON_PI_CONTEXT;
  if (!contextPath) return;
  const delivery = async () => decodeDelivery(JSON.parse(await readFile(contextPath, "utf8")));
  const result = (value: unknown) => ({ content: [{ type: "text" as const, text: JSON.stringify(value) }], details: value });

  pi.on("session_start", async (_event, ctx) => {
    const current = await delivery();
    if (ctx.hasUI) ctx.ui.setStatus("onton", `Onton: ${current.project} / ${current.patch_id}`);
  });
  pi.on("before_agent_start", async () => {
    const current = await delivery();
    return { message: { customType: "onton-mode", display: false,
      content: `${instructions}\nProject: ${current.project}\nPatch: ${current.patch_id}\nExecution: ${current.session_uuid}` } };
  });
  // This hook also runs for overflow compaction, which may retry without a new
  // before_agent_start. Keep a recovery pointer rather than copying large prompts.
  pi.on("session_compact", async () => {
    pi.sendMessage({ customType: "onton-mode", content: instructions, display: false });
  });

  pi.registerTool({
    name: "onton_context", label: "Onton context",
    description: "Read the exact current supervisor prompt and execution identity. Covers start, human guidance, review, findings, CI, cleanup, branch changes, publication and recovery turns. Resumed turns retain their earlier conversation context.",
    parameters: Type.Object({}),
    async execute() { return result(await delivery()); },
  });
  pi.registerTool({
    name: "onton_read_artifact", label: "Read Onton artifact",
    description: "Read published gameplan or artifact evidence in bounded byte chunks. For artifacts use a path relative to artifacts_root from onton_context, e.g. <dependency-id>/pr-body.md or <patch-id>/ci/<check>/.... Follow the exact paths in the prompt.",
    parameters: Type.Object({
      source: Type.Union([Type.Literal("gameplan"), Type.Literal("artifact")]),
      path: Type.Optional(Type.String()),
      offset: Type.Optional(Type.Integer({ minimum: 0 })),
      limit: Type.Optional(Type.Integer({ minimum: 1, maximum: 65536 })),
    }),
    async execute(_id, params, signal) {
      signal?.throwIfAborted();
      const current = await delivery();
      const path = params.source === "gameplan" ? current.gameplan_path
        : await artifactPath(current.artifacts_root, params.path ?? "");
      return result(await readChunk(path, params.offset ?? 0, params.limit ?? 32768));
    },
  });
  pi.registerTool({
    name: "onton_respond", label: "Respond to Onton",
    description: "Atomically write an existing Onton response artifact. implementation_notes: notes only, no header. review_response: response text with exact comment_id as id. finding_wontfix: reason with exact wontfix_file as id. opt_out: reason only when the supervisor offers pre-commit opt-out. Does not commit, publish, resolve, or declare success; supervisor acceptance rules still apply.",
    parameters: Type.Object({
      kind: Type.Union([Type.Literal("implementation_notes"), Type.Literal("review_response"), Type.Literal("finding_wontfix"), Type.Literal("opt_out")]),
      id: Type.Optional(Type.String()),
      content: Type.String({ minLength: 1 }),
    }),
    async execute(_id, params, signal) {
      signal?.throwIfAborted();
      const current = await delivery();
      const path = await writeResponse(current.artifact_dir, responsePath(params.kind, params.id), params.content);
      return result({ path, written: true, message: "Response saved for supervisor processing." });
    },
  });
}
