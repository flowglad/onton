/**
 * Skill requirements declared in SKILL.md frontmatter.
 *
 * The Agent Skills spec has no dependency field, so the skill declares what it needs in
 * `metadata` (string keys to string values, as the spec requires). Each value lists packages
 * with the tools they provide, `package: tool tool; package: tool`:
 *
 *   metadata:
 *     pi-requires: "npm:pi-web-access: fetch_content web_search"
 *     pi-recommends: "npm:pi-lens: lsp_navigation ast_grep_search; npm:pi-subagents: subagent"
 *
 * The extension checks the registered tools against both lists. Required gaps are warnings
 * with the install command; recommended gaps are informational. Nothing blocks.
 */

import { parse as parseYaml } from "yaml";

export interface PackageTools {
  /** Pi package source, e.g. `npm:pi-web-access`. */
  package: string;
  /** Tool names the package registers that the skill uses. */
  tools: string[];
}

export interface SkillRequirements {
  required: PackageTools[];
  recommended: PackageTools[];
}

export const REQUIRES_KEY = "pi-requires";
export const RECOMMENDS_KEY = "pi-recommends";

export const noRequirements: SkillRequirements = { required: [], recommended: [] };

export function parseSkillRequirements(skillMd: string): SkillRequirements {
  const match = /^---\r?\n([\s\S]*?)\r?\n---(?:\r?\n|$)/.exec(skillMd);
  if (!match) return noRequirements;
  let frontmatter: unknown;
  try {
    frontmatter = parseYaml(match[1] ?? "");
  } catch {
    return noRequirements;
  }
  if (typeof frontmatter !== "object" || frontmatter === null) return noRequirements;
  const metadata = (frontmatter as { metadata?: unknown }).metadata;
  if (typeof metadata !== "object" || metadata === null) return noRequirements;
  const m = metadata as Record<string, unknown>;
  return { required: parseGroups(m[REQUIRES_KEY]), recommended: parseGroups(m[RECOMMENDS_KEY]) };
}

/** `pkg: tool tool; pkg: tool` → groups. Entries without a package or tools are dropped. */
export function parseGroups(value: unknown): PackageTools[] {
  if (typeof value !== "string") return [];
  const groups: PackageTools[] = [];
  for (const entry of value.split(";")) {
    const m = /^\s*(\S+?):\s+(.+?)\s*$/.exec(entry);
    if (!m) continue;
    const tools = (m[2] ?? "").split(/[\s,]+/).filter((t) => t.length > 0);
    if (tools.length > 0) groups.push({ package: m[1] ?? "", tools });
  }
  return groups;
}

/** Groups with at least one unregistered tool, keeping only the missing tools. */
export function missingGroups(groups: readonly PackageTools[], registered: readonly string[]): PackageTools[] {
  const have = new Set(registered);
  return groups
    .map((g) => ({ package: g.package, tools: g.tools.filter((t) => !have.has(t)) }))
    .filter((g) => g.tools.length > 0);
}

/** `fetch_content, web_search (pi install npm:pi-web-access); subagent (pi install npm:pi-subagents)` */
export function describeGroups(groups: readonly PackageTools[]): string {
  return groups.map((g) => `${g.tools.join(", ")} (pi install ${g.package})`).join("; ");
}
