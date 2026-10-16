/**
 * Runs the write-gameplan skill's own validator and formatter. There is exactly one
 * authoritative validator (the skill's `scripts/validate.py`); this module shells out to it
 * and parses its output rather than reimplementing any check.
 */

import { existsSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

export interface ExecLike {
  (command: string, args: string[], options?: { cwd?: string; timeout?: number; signal?: AbortSignal }): Promise<{
    stdout: string;
    stderr: string;
    code: number;
    killed: boolean;
  }>;
}

export interface SkillPaths {
  dir: string;
  skillMd: string;
  schema: string;
  validate: string;
  format: string;
}

export const SKILL_DIR_ENV = "ONTON_GAMEPLAN_SKILL_DIR";
export const PYTHON_ENV = "ONTON_GAMEPLAN_PYTHON";

/** The skill directory: an explicit override, else the sibling `skills/write-gameplan` in this repo. */
export function resolveSkillPaths(env: NodeJS.ProcessEnv = process.env): SkillPaths {
  const dir = env[SKILL_DIR_ENV]?.trim() || fileURLToPath(new URL("../../skills/write-gameplan", import.meta.url));
  return {
    dir,
    skillMd: resolve(dir, "SKILL.md"),
    schema: resolve(dir, "references", "gameplan-schema.json"),
    validate: resolve(dir, "scripts", "validate.py"),
    format: resolve(dir, "scripts", "format_yaml.py"),
  };
}

/** Missing files that make the skill unusable, as messages. Empty when everything is present. */
export function missingSkillFiles(paths: SkillPaths): string[] {
  return [paths.skillMd, paths.schema, paths.validate, paths.format].filter((p) => !existsSync(p)).map((p) => `missing ${p}`);
}

/**
 * The command that runs the skill's Python scripts, as argv to prepend to `[script, ...args]`.
 *
 * Precedence: `ONTON_GAMEPLAN_PYTHON` when set (an interpreter or a wrapper); otherwise `uv run`
 * when `uv` is on PATH, because the scripts carry inline (PEP 723) dependency metadata and uv
 * resolves it with no setup; otherwise `python3`, which must already have the dependencies.
 */
export function pythonCommand(env: NodeJS.ProcessEnv = process.env, uvAvailable: boolean = findOnPath("uv", env) !== undefined): string[] {
  const raw = env[PYTHON_ENV]?.trim();
  if (raw) {
    const argv = splitArgv(raw);
    if (argv.length > 0) return argv;
  }
  return uvAvailable ? ["uv", "run", "--quiet"] : ["python3"];
}

/** The first PATH entry containing an executable with this name, without spawning anything. */
export function findOnPath(name: string, env: NodeJS.ProcessEnv = process.env): string | undefined {
  const sep = process.platform === "win32" ? ";" : ":";
  for (const dir of (env.PATH ?? "").split(sep)) {
    if (!dir) continue;
    const candidate = resolve(dir, name);
    if (existsSync(candidate)) return candidate;
  }
  return undefined;
}

const MISSING_MODULE = /No module named '([^']+)'/;

/** An actionable message when a script failed for want of a Python dependency. */
export function dependencyHint(output: string): string | undefined {
  const m = MISSING_MODULE.exec(output);
  if (!m) return undefined;
  return `Python dependency missing (${m[1]}) for the validator. Fix one of: install uv (the extension then runs the skill's scripts with their inline dependencies), set ${PYTHON_ENV} to an interpreter or wrapper that has the skill's requirements, or install scripts/requirements.txt into python3.`;
}

/** Minimal shell-like splitting: whitespace separates, single or double quotes group. */
export function splitArgv(text: string): string[] {
  const out: string[] = [];
  let current = "";
  let quote: '"' | "'" | undefined;
  let pending = false;
  for (const ch of text) {
    if (quote) {
      if (ch === quote) quote = undefined;
      else current += ch;
      continue;
    }
    if (ch === '"' || ch === "'") {
      quote = ch;
      pending = true;
    } else if (/\s/.test(ch)) {
      if (pending || current) out.push(current);
      current = "";
      pending = false;
    } else {
      current += ch;
      pending = true;
    }
  }
  if (pending || current) out.push(current);
  return out;
}

export interface ValidationReport {
  ok: boolean;
  /** `ERROR:` lines from the validator, without the prefix. */
  errors: string[];
  /** A failure before checks ran: missing file, unparseable YAML, missing Python dependency. */
  fatal?: string;
}

export function parseValidatorOutput(result: { stdout: string; stderr: string; code: number; killed: boolean }): ValidationReport {
  if (result.killed) return { ok: false, errors: [], fatal: "validator timed out" };
  if (result.code === 0) return { ok: true, errors: [] };
  const errors = result.stdout
    .split("\n")
    .filter((line) => line.startsWith("ERROR: "))
    .map((line) => line.slice("ERROR: ".length).trim());
  // A missing dependency is an environment problem, not a gameplan error, even when the
  // validator reports it as one (the formatting check does).
  const hint = dependencyHint(`${result.stdout}\n${result.stderr}`);
  if (hint) return { ok: false, errors: errors.filter((e) => !MISSING_MODULE.test(e)), fatal: hint };
  if (result.code === 1 && errors.length > 0) return { ok: false, errors };
  const stderr = result.stderr.trim();
  const fatal = stderr || `validator exited with code ${result.code} and no diagnostics`;
  return { ok: false, errors, fatal: fatal.split("\n").slice(-3).join("\n") };
}

export async function runValidator(
  exec: ExecLike,
  paths: SkillPaths,
  python: string[],
  file: string,
  options: { cwd: string; signal?: AbortSignal; timeout?: number },
): Promise<ValidationReport> {
  const [command, ...prefix] = python;
  if (!command) return { ok: false, errors: [], fatal: "no Python command configured" };
  const result = await exec(command, [...prefix, paths.validate, resolve(options.cwd, file)], {
    cwd: options.cwd,
    timeout: options.timeout ?? 120_000,
    signal: options.signal,
  });
  return parseValidatorOutput(result);
}

export async function runFormatter(
  exec: ExecLike,
  paths: SkillPaths,
  python: string[],
  file: string,
  options: { cwd: string; signal?: AbortSignal; timeout?: number },
): Promise<{ ok: boolean; error?: string }> {
  const [command, ...prefix] = python;
  if (!command) return { ok: false, error: "no Python command configured" };
  const result = await exec(command, [...prefix, paths.format, resolve(options.cwd, file), "--in-place"], {
    cwd: options.cwd,
    timeout: options.timeout ?? 60_000,
    signal: options.signal,
  });
  if (result.killed) return { ok: false, error: "formatter timed out" };
  if (result.code === 0) return { ok: true };
  const hint = dependencyHint(`${result.stdout}\n${result.stderr}`);
  if (hint) return { ok: false, error: hint };
  return { ok: false, error: (result.stderr.trim() || result.stdout.trim() || `formatter exited ${result.code}`).split("\n").slice(-3).join("\n") };
}
