/**
 * Pure rule for which paths the gameplan tools treat as gameplan documents. Gameplan mode does
 * not police write/edit or shell commands; this only decides what the tools themselves write
 * and which hand edits should be remembered as the session's gameplan file.
 */

import { isAbsolute, relative, resolve, sep } from "node:path";

const GAMEPLAN_EXTENSIONS = new Set([".yaml", ".yml", ".json"]);

/**
 * Whether a path is a gameplan document directly under the configured gameplan directory.
 * Paths are resolved against the project root; traversal out of the directory is rejected.
 */
export function isGameplanWrite(cwd: string, gameplanDir: string, target: string): boolean {
  const root = resolve(cwd, gameplanDir);
  const path = resolve(cwd, target);
  const rel = relative(root, path);
  if (rel === "" || rel.startsWith("..") || isAbsolute(rel)) return false;
  if (rel.includes(sep)) return false;
  const dot = rel.lastIndexOf(".");
  return dot > 0 && GAMEPLAN_EXTENSIONS.has(rel.slice(dot));
}
