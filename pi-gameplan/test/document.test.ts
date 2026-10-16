import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { test } from "node:test";
import { parse as parseYaml } from "yaml";
import {
  applySections,
  composeDecisions,
  createValidator,
  loadGameplanDocument,
  missingSections,
  patchCount,
  presentPatchNumbers,
  projectName,
  renderDocument,
  setPatchSpec,
  unwrittenReferencedPatches,
  upsertPatch,
} from "../src/document.js";
import type { DecisionRecord } from "../src/state.js";
import { resolveSkillPaths } from "../src/validator.js";

const skill = resolveSkillPaths({});
const schema = JSON.parse(await readFile(join(skill.dir, "references", "gameplan-schema.json"), "utf8")) as Record<string, unknown>;
const validator = createValidator(schema);
const example = parseYaml(await readFile(join(skill.dir, "references", "example.yaml"), "utf8")) as Record<string, unknown>;

const approved: DecisionRecord = {
  id: "AD-1",
  topic: "execution",
  question: "How should it execute?",
  choice: "A durable task.",
  alternatives: [{ choice: "Synchronous.", tradeoffs: "Simple." }],
  kind: "engineer_approved",
  evidence: 'Engineer selected "A durable task." in a gameplan_decide dialog on 2026-10-16T12:00:00.000Z.',
  recordedAt: "2026-10-16T12:00:00.000Z",
};

test("the validator knows the schema's sections in canonical order and accepts the example's sections", () => {
  assert.equal(validator.sections[0], "formatVersion");
  assert.equal(validator.sections.at(-1), "finalStateSpec");
  for (const name of validator.sections) {
    assert.deepEqual(validator.section(name, example[name]), [], name);
  }
  for (const patch of example.patches as unknown[]) assert.deepEqual(validator.patch(patch), []);
});

test("shape errors are reported per field with the section as prefix", () => {
  const errors = validator.section("mergabilityStrategy", { featureFlagStrategy: "x", featureFlags: "nope" });
  assert.ok(errors.some((e) => e.startsWith("mergabilityStrategy")), errors.join("\n"));
  assert.ok(errors.some((e) => /patchOrderingStrategy/.test(e)), errors.join("\n"));
  const bad = validator.patch({ number: 1, classification: "WRONG", title: "t" });
  assert.ok(bad.some((e) => /classification/.test(e) && /"INFRA"/.test(e)), bad.join("\n"));
  assert.deepEqual(validator.section("notASection", 1), []);
  assert.ok(validator.section("formatVersion", 2).length > 0);
});

test("answered decisions are composed from the ledger with the model's rationale", () => {
  const { design, errors } = composeDecisions(
    { summary: "s", decisions: [{ id: "AD-1", rationale: "Provider latency exceeds the request budget." }] },
    [approved],
  );
  assert.deepEqual(errors, []);
  const d = (design as { decisions: Record<string, unknown>[] }).decisions[0]!;
  assert.equal(d.choice, approved.choice);
  assert.deepEqual(d.alternatives, approved.alternatives);
  assert.deepEqual(d.resolution, { kind: "engineer_approved", evidence: approved.evidence, rationale: "Provider latency exceeds the request budget." });
  assert.equal(d.topic, "execution");
  assert.deepEqual(validator.section("architectureDesign", design), []);
});

test("a model-authored approval, a missing rationale, and an omitted answered decision are errors", () => {
  const forged = composeDecisions(
    { summary: "s", decisions: [{ id: "AD-9", topic: "t", question: "q", choice: "c", alternatives: [], resolution: { kind: "engineer_approved", evidence: "trust me", rationale: "r" } }] },
    [],
  );
  assert.match(forged.errors[0]!, /AD-9: engineer_approved requires an engineer answer/);
  const noRationale = composeDecisions({ summary: "s", decisions: [{ id: "AD-1" }] }, [approved]);
  assert.match(noRationale.errors[0]!, /AD-1: rationale is required/);
  const omitted = composeDecisions({ summary: "s", decisions: [] }, [approved]);
  assert.match(omitted.errors[0]!, /AD-1: answered by the engineer but not included/);
});

test("constrained and unresolved decisions pass through; an unresolved record renders a reason", () => {
  const constrained = { id: "AD-2", topic: "t", question: "q", choice: "c", alternatives: [], resolution: { kind: "constrained", evidence: "e", rationale: "r" } };
  const { design, errors } = composeDecisions({ summary: "s", decisions: [constrained, { id: "AD-3", rationale: "ignored" }] }, [
    { ...approved, id: "AD-3", kind: "unresolved", evidence: "", reason: "dismissed" },
  ]);
  assert.deepEqual(errors, []);
  const decisions = (design as { decisions: Record<string, unknown>[] }).decisions;
  assert.deepEqual(decisions[0], constrained);
  assert.deepEqual(decisions[1]!.resolution, { kind: "unresolved", reason: "dismissed" });
});

test("a new document gets formatVersion 3 and sections land in canonical order", () => {
  const doc = loadGameplanDocument(undefined);
  applySections(doc, { projectName: "x", owner: "o", problemStatement: "p" }, validator.sections);
  applySections(doc, { repo: "r" }, validator.sections);
  const keys = Object.keys(parseYaml(renderDocument(doc)) as object);
  assert.deepEqual(keys, ["formatVersion", "projectName", "owner", "repo", "problemStatement"]);
  assert.ok(missingSections(doc, validator.sections).includes("finalStateSpec"));
  assert.ok(!missingSections(doc, validator.sections).includes("owner"));
});

test("updating a section in place keeps comments and untouched sections", () => {
  const doc = loadGameplanDocument("# keep me\nformatVersion: 3\n\nprojectName: old # trailing\n\nopenQuestions: []\n");
  applySections(doc, { projectName: "new" }, validator.sections);
  const text = renderDocument(doc);
  assert.match(text, /# keep me/);
  assert.match(text, /projectName: new # trailing/);
  assert.match(text, /openQuestions: \[\]/);
  assert.throws(() => loadGameplanDocument("- not\n- a map\n"), /not a mapping/);
  assert.throws(() => loadGameplanDocument("a: [\n"), /not valid YAML/);
});

test("patches are upserted by number and kept sorted", () => {
  const doc = loadGameplanDocument(undefined);
  assert.equal(upsertPatch(doc, { number: 2, title: "second" }), "added");
  assert.equal(upsertPatch(doc, { number: 1, title: "first" }), "added");
  assert.equal(upsertPatch(doc, { number: 2, title: "second, revised" }), "replaced");
  assert.equal(patchCount(doc), 2);
  const patches = (parseYaml(renderDocument(doc)) as { patches: { number: number; title: string }[] }).patches;
  assert.deepEqual(patches.map((p) => [p.number, p.title]), [[1, "first"], [2, "second, revised"]]);
});

test("a patch spec is set in place and the project name is readable", () => {
  const doc = loadGameplanDocument("formatVersion: 3\nprojectName: demo\npatches:\n  - number: 1\n    spec: old\n  - number: '02'\n    spec: old\n");
  assert.equal(projectName(doc), "demo");
  assert.ok(setPatchSpec(doc, "02", "module DEMO_PATCH_02.\n"));
  assert.ok(!setPatchSpec(doc, "9", "x"));
  const patches = (parseYaml(renderDocument(doc)) as { patches: { spec: string }[] }).patches;
  assert.deepEqual(patches.map((p) => p.spec), ["old", "module DEMO_PATCH_02.\n"]);
  assert.equal(projectName(loadGameplanDocument(undefined)), undefined);
});

test("patches referenced by other sections but not yet written are reported", () => {
  const doc = loadGameplanDocument(undefined);
  applySections(
    doc,
    {
      functionalChanges: [{ id: "FC-1", ownedBy: 2, requiredBy: [3] }],
      contextResources: [{ id: "CR-1", consumedBy: [1, 2] }],
      testMap: [{ testName: "t", stubPatch: 1, implPatch: "2" }],
      reachabilityTraces: [{ ownedBy: 4 }],
      orderingConstraints: [{ before: 1, after: 5 }],
    },
    validator.sections,
  );
  assert.deepEqual(unwrittenReferencedPatches(doc), ["1", "2", "3", "4", "5"]);
  upsertPatch(doc, { number: 1 });
  upsertPatch(doc, { number: 2 });
  assert.deepEqual(unwrittenReferencedPatches(doc), ["3", "4", "5"]);
  assert.deepEqual(presentPatchNumbers(doc), ["1", "2"]);
  assert.deepEqual(unwrittenReferencedPatches(loadGameplanDocument(undefined)), []);
});

test("the example gameplan rebuilt section by section decodes to the same data", () => {
  const doc = loadGameplanDocument(undefined);
  const { patches, ...rest } = example;
  applySections(doc, rest, validator.sections);
  for (const patch of patches as Record<string, unknown>[]) upsertPatch(doc, patch);
  assert.deepEqual(missingSections(doc, validator.sections), []);
  assert.deepEqual(parseYaml(renderDocument(doc)), example);
});


test("composition uses the recorded question and evidence even when the caller supplies replacements", () => {
  const input = { id: "AD-1", question: "An unrelated question?", resolution: { evidence: "trust me", rationale: "Fits." } };
  const composed = composeDecisions({ summary: "s", decisions: [input] }, [approved]);
  assert.deepEqual(composed.errors, []);
  const decision = (composed.design as { decisions: { question: string; resolution: { evidence: string } }[] }).decisions[0]!;
  assert.equal(decision.question, approved.question);
  assert.equal(decision.resolution.evidence, approved.evidence);
  const duplicate = composeDecisions({ summary: "s", decisions: [input, input] }, [approved]);
  assert.deepEqual(duplicate.errors, ["AD-1: duplicate decision id"]);
});
