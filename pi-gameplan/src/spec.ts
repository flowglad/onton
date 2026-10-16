/**
 * Pure helpers for Pantagruel specs: the module name the skill expects, and parsing of the
 * `pant` CLI's diagnostics. Running `pant` itself is the caller's job.
 */

export type SpecTarget = { kind: "final" } | { kind: "patch"; number: string };

/** `<PROJECT_NAME>` for the final spec, `<PROJECT_NAME>_PATCH_<N>` for a patch, per the skill. */
export function expectedModuleName(projectName: string, target: SpecTarget): string {
  const project = projectName.trim().toUpperCase().replace(/[^A-Z0-9]+/g, "_").replace(/^_+|_+$/g, "");
  return target.kind === "final" ? project : `${project}_PATCH_${target.number}`;
}

export interface PantDiagnostic {
  line?: number;
  column?: number;
  message: string;
}

/** `path:line:col: error: message` lines from pant; anything else becomes one message. */
export function parsePantDiagnostics(output: string): PantDiagnostic[] {
  const diagnostics: PantDiagnostic[] = [];
  const other: string[] = [];
  for (const raw of output.split("\n")) {
    const line = raw.trimEnd();
    if (!line.trim()) continue;
    const m = /^(.*?):(\d+):(\d+): (?:error|warning): (.*)$/.exec(line);
    if (m) diagnostics.push({ line: Number(m[2]), column: Number(m[3]), message: m[4] ?? "" });
    else other.push(line);
  }
  if (other.length > 0) diagnostics.push({ message: other.join("\n") });
  return diagnostics;
}

/** Render diagnostics with the offending source line quoted, so the model can fix it without re-reading. */
export function describeDiagnostics(spec: string, diagnostics: readonly PantDiagnostic[]): string {
  const lines = spec.split("\n");
  return diagnostics
    .map((d) => {
      if (d.line === undefined) return d.message;
      const source = lines[d.line - 1];
      const where = `line ${d.line}${d.column !== undefined ? `:${d.column}` : ""}`;
      return source === undefined ? `${where}: ${d.message}` : `${where}: ${d.message}\n    ${source}`;
    })
    .join("\n");
}

/** The module name `pant --json` reports, or undefined when the spec declares none. */
export function moduleNameFromJson(json: string): string | undefined {
  try {
    const parsed: unknown = JSON.parse(json);
    const name = (parsed as { module?: unknown })?.module;
    return typeof name === "string" && name.length > 0 ? name : undefined;
  } catch {
    return undefined;
  }
}

/** Specs are stored as block literals; keep exactly one trailing newline. */
export function normalizeSpec(spec: string): string {
  return `${spec.replace(/\s+$/, "")}\n`;
}
