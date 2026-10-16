import assert from "node:assert/strict";
import { test } from "node:test";
import { isGameplanWrite } from "../src/guard.js";

test("gameplan documents are files directly under the gameplan directory", () => {
  const cwd = "/repo";
  assert.ok(isGameplanWrite(cwd, "gameplans", "gameplans/auth.yaml"));
  assert.ok(isGameplanWrite(cwd, "gameplans", "/repo/gameplans/auth.yml"));
  assert.ok(!isGameplanWrite(cwd, "gameplans", "./gameplans/auth.json"));
  assert.ok(isGameplanWrite(cwd, "docs/plans", "docs/plans/auth.yaml"));
  assert.ok(!isGameplanWrite(cwd, "gameplans", "src/index.ts"));
  assert.ok(!isGameplanWrite(cwd, "gameplans", "gameplans/../src/index.ts"));
  assert.ok(!isGameplanWrite(cwd, "gameplans", "gameplans/nested/auth.yaml"));
  assert.ok(!isGameplanWrite(cwd, "gameplans", "gameplans/notes.md"));
  assert.ok(!isGameplanWrite(cwd, "gameplans", "gameplans"));
  assert.ok(!isGameplanWrite(cwd, "gameplans", "/elsewhere/gameplans/auth.yaml"));
  assert.ok(!isGameplanWrite(cwd, "gameplans", "gameplans/.yaml"));
});
