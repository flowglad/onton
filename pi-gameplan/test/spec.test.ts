import assert from "node:assert/strict";
import { test } from "node:test";
import { describeDiagnostics, expectedModuleName, moduleNameFromJson, normalizeSpec, parsePantDiagnostics } from "../src/spec.js";

test("module names follow the skill's convention", () => {
  assert.equal(expectedModuleName("extract-decision", { kind: "final" }), "EXTRACT_DECISION");
  assert.equal(expectedModuleName("extract-decision", { kind: "patch", number: "3" }), "EXTRACT_DECISION_PATCH_3");
  assert.equal(expectedModuleName("  Branch Reconciliation!  ", { kind: "patch", number: "01" }), "BRANCH_RECONCILIATION_PATCH_01");
});

test("pant diagnostics are parsed with positions and quoted source lines", () => {
  const spec = "module X.\nFoo.\n---\nf x: Foo => .\n";
  const output = "/tmp/a.pant:4:12: error: Parse error: unexpected '.', expected identifier\n";
  const diagnostics = parsePantDiagnostics(output);
  assert.deepEqual(diagnostics, [{ line: 4, column: 12, message: "Parse error: unexpected '.', expected identifier" }]);
  assert.equal(describeDiagnostics(spec, diagnostics), "line 4:12: Parse error: unexpected '.', expected identifier\n    f x: Foo => .");
});

test("non-positional output such as SMT findings is kept as one message", () => {
  const output = "FAIL: Invariants are contradictory\n  Conflicting constraints:\n    Invariant: all x: Foo | ok? x\n";
  assert.deepEqual(parsePantDiagnostics(output), [{ message: output.trimEnd() }]);
  assert.equal(describeDiagnostics("", parsePantDiagnostics(output)), output.trimEnd());
  assert.deepEqual(parsePantDiagnostics("\n\n"), []);
});

test("the module name is read from pant --json and specs end with one newline", () => {
  assert.equal(moduleNameFromJson('{"module":"X_PATCH_1","rules":{}}'), "X_PATCH_1");
  assert.equal(moduleNameFromJson('{"module":null}'), undefined);
  assert.equal(moduleNameFromJson("not json"), undefined);
  assert.equal(normalizeSpec("module X.\n\n\n"), "module X.\n");
  assert.equal(normalizeSpec("module X."), "module X.\n");
});
