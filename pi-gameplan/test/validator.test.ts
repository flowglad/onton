import assert from "node:assert/strict";
import { execFile } from "node:child_process";
import { existsSync } from "node:fs";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { promisify } from "node:util";
import {
  dependencyHint,
  findOnPath,
  missingSkillFiles,
  parseValidatorOutput,
  pythonCommand,
  resolveSkillPaths,
  runFormatter,
  runValidator,
  splitArgv,
} from "../src/validator.js";

test("the skill directory defaults to the sibling skill and can be overridden", () => {
  const paths = resolveSkillPaths({});
  assert.match(paths.dir, /skills\/write-gameplan$/);
  assert.deepEqual(missingSkillFiles(paths), []);
  const overridden = resolveSkillPaths({ ONTON_GAMEPLAN_SKILL_DIR: "/nowhere/skill" });
  assert.equal(overridden.validate, "/nowhere/skill/scripts/validate.py");
  assert.equal(missingSkillFiles(overridden).length, 4);
});

test("the python command prefers an explicit override, then uv, then python3", () => {
  assert.deepEqual(pythonCommand({}, false), ["python3"]);
  assert.deepEqual(pythonCommand({}, true), ["uv", "run", "--quiet"]);
  assert.deepEqual(pythonCommand({ ONTON_GAMEPLAN_PYTHON: "   " }, true), ["uv", "run", "--quiet"]);
  assert.deepEqual(pythonCommand({ ONTON_GAMEPLAN_PYTHON: "/opt/py/bin/python3" }, true), ["/opt/py/bin/python3"]);
  assert.equal(findOnPath("definitely-not-a-binary-xyz", { PATH: "/usr/bin:/bin" }), undefined);
  assert.ok(findOnPath("sh", { PATH: "/nonexistent:/bin" })?.endsWith("/bin/sh"));
  assert.deepEqual(splitArgv(`uv run --with "ruamel.yaml<0.19" --with 'PyYAML' python3`), [
    "uv", "run", "--with", "ruamel.yaml<0.19", "--with", "PyYAML", "python3",
  ]);
});

test("validator output is classified as pass, errors, or fatal", () => {
  assert.deepEqual(parseValidatorOutput({ stdout: "PASS: x\n", stderr: "", code: 0, killed: false }), { ok: true, errors: [] });
  assert.deepEqual(
    parseValidatorOutput({ stdout: "ERROR: a\nERROR: b\n", stderr: "\nFAIL: 2 error(s)\n", code: 1, killed: false }),
    { ok: false, errors: ["a", "b"] },
  );
  const fatal = parseValidatorOutput({ stdout: "", stderr: "invalid gameplan: YAML support requires PyYAML\n", code: 2, killed: false });
  assert.equal(fatal.ok, false);
  assert.match(fatal.fatal!, /requires PyYAML/);
  assert.equal(parseValidatorOutput({ stdout: "", stderr: "", code: 1, killed: true }).fatal, "validator timed out");
});

test("a missing Python module becomes an environment hint, not a gameplan error", () => {
  const report = parseValidatorOutput({
    stdout: "ERROR: formatting check failed: No module named 'ruamel'\nERROR: real problem\n",
    stderr: "\nFAIL: 2 error(s)\n",
    code: 1,
    killed: false,
  });
  assert.equal(report.ok, false);
  assert.deepEqual(report.errors, ["real problem"]);
  assert.match(report.fatal!, /Python dependency missing \(ruamel\)/);
  assert.match(report.fatal!, /ONTON_GAMEPLAN_PYTHON/);
  assert.match(dependencyHint("ModuleNotFoundError: No module named 'yaml'")!, /\(yaml\)/);
  assert.equal(dependencyHint("all good"), undefined);
});

// Integration against the real validator. Runs when uv is on PATH (the scripts carry inline
// dependency metadata) or when ONTON_GAMEPLAN_PYTHON names an interpreter with the deps.
const run = promisify(execFile);
const exec = async (command: string, args: string[], options?: { cwd?: string; timeout?: number }) => {
  try {
    const { stdout, stderr } = await run(command, args, { cwd: options?.cwd, timeout: options?.timeout, maxBuffer: 16 * 1024 * 1024 });
    return { stdout, stderr, code: 0, killed: false };
  } catch (error) {
    const e = error as { stdout?: string; stderr?: string; code?: number | string; killed?: boolean };
    return { stdout: e.stdout ?? "", stderr: e.stderr ?? "", code: typeof e.code === "number" ? e.code : 1, killed: e.killed ?? false };
  }
};

const python = pythonCommand();
const skill = resolveSkillPaths();
const example = join(skill.dir, "references", "example.yaml");
const integration = { skip: (!process.env.ONTON_GAMEPLAN_PYTHON && !findOnPath("uv")) || !existsSync(example) };

test("the real validator passes the skill's example and reports a broken copy", integration, async () => {
  const dir = await mkdtemp(join(tmpdir(), "pi-gameplan-"));
  try {
    const pass = await runValidator(exec, skill, python, example, { cwd: dir });
    assert.deepEqual(pass, { ok: true, errors: [] });

    const broken = join(dir, "broken.yaml");
    const source = await readFile(example, "utf8");
    // Drop the version line (and its trailing blank line, to stay canonically formatted).
    assert.match(source, /^formatVersion: 3\n\n/);
    await writeFile(broken, source.replace(/^formatVersion: 3\n\n/, ""));
    const fail = await runValidator(exec, skill, python, "broken.yaml", { cwd: dir });
    assert.equal(fail.ok, false);
    assert.equal(fail.fatal, undefined);
    assert.ok(fail.errors.some((e) => /formatVersion/.test(e)), fail.errors.join("\n"));
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});

test("the real formatter rewrites in place and leaves the example valid", integration, async () => {
  const dir = await mkdtemp(join(tmpdir(), "pi-gameplan-"));
  try {
    const copy = join(dir, "copy.yaml");
    await writeFile(copy, (await readFile(example, "utf8")).replace(/\n\n/g, "\n"));
    const formatted = await runFormatter(exec, skill, python, copy, { cwd: dir });
    assert.deepEqual(formatted, { ok: true });
    assert.deepEqual(await runValidator(exec, skill, python, copy, { cwd: dir }), { ok: true, errors: [] });
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});
