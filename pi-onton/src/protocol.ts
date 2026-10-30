import { isAbsolute, relative, resolve, sep } from "node:path";

export interface Delivery {
  version: 1;
  project: string;
  patch_id: string;
  session_uuid: string;
  prompt: string;
  artifacts_root: string;
  artifact_dir: string;
  gameplan_path: string;
}

/** Reject malformed supervisor deliveries rather than guessing project paths. */
export function decodeDelivery(value: unknown): Delivery {
  if (!value || typeof value !== "object") throw new Error("Invalid Onton delivery");
  const v = { ...value } as Record<string, unknown>;
  if (v.version !== 1) throw new Error("Unsupported Onton delivery version");
  for (const key of ["project", "patch_id", "session_uuid", "prompt", "artifacts_root", "artifact_dir", "gameplan_path"]) {
    if (typeof v[key] !== "string" || v[key].length === 0) throw new Error(`Missing Onton ${key}`);
  }
  for (const key of ["artifacts_root", "artifact_dir", "gameplan_path"]) {
    if (!isAbsolute(v[key] as string)) throw new Error(`Onton ${key} must be absolute`);
  }
  const root = resolve(v.artifacts_root as string);
  for (const key of ["artifact_dir", "gameplan_path"]) {
    const path = resolve(v[key] as string);
    const rel = relative(root, path);
    if (isAbsolute(rel) || rel === ".." || rel.startsWith(`..${sep}`)) {
      throw new Error(`Onton ${key} escapes artifacts_root`);
    }
    v[key] = path;
  }
  return { ...v, artifacts_root: root } as unknown as Delivery;
}

export type ResponseKind = "implementation_notes" | "review_response" | "finding_wontfix" | "opt_out";

export function responsePath(kind: ResponseKind, id?: string): string {
  switch (kind) {
    case "implementation_notes": return "pr-body.md";
    case "opt_out": return "WONTDO.md";
    case "review_response":
      if (!id || id !== id.trim() || !/^[1-9][0-9]*$/.test(id)) throw new Error("Copy the comment_id from the current prompt");
      return `comment_responses/${id}.md`;
    case "finding_wontfix":
      if (!id || !/^[A-Za-z0-9_.-]+\.md$/.test(id)) throw new Error("Copy the wontfix_file from the current prompt");
      return `findings_wontfix/${id}`;
  }
}

export const instructions = `Onton agent mode is active. Follow the supervisor's current prompt.
Use onton_context to recover the exact current delivery, including implementation requirements,
review comments, findings, CI failures, human guidance, branch changes, publication feedback,
scheduler repair or Git recovery instructions. Resumed deliveries may contain only the new turn;
retain earlier requirements and read the published gameplan and dependency notes when needed.
Use onton_read_artifact for the gameplan, dependency implementation notes and CI evidence.
Use onton_respond for implementation notes, review replies, finding wontfix reasons, or an
explicitly offered pre-commit opt-out. Copy IDs and filenames from the delivered prompt.
A finding fixed in code needs no wontfix file. Artifact writes do not publish or resolve anything:
Onton validates the session and performs the requested forge actions. Follow the current prompt's
Git ownership and commit instructions. End your turn normally when done; never claim that a tool
response proves successful CI, publication, reconciliation, or completion. If blocked, report the
blocker in your final response. Human messages arrive as supervisor turns, not interactive dialogs.`;
