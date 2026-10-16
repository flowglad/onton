import assert from "node:assert/strict";
import { test } from "node:test";
import { auditDecisions } from "../src/audit.js";
import { type DecisionRecord } from "../src/state.js";

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

function doc(decisions: unknown[], openQuestions: unknown[] = []) {
  return { architectureDesign: { summary: "x", decisions }, openQuestions };
}

function yamlDecision(overrides: Partial<Record<string, unknown>> = {}, resolution: Record<string, unknown> = {}) {
  return {
    id: "AD-1",
    topic: "execution",
    question: "How should it execute?",
    choice: "A durable task.",
    alternatives: approved.alternatives,
    resolution: { kind: "engineer_approved", evidence: approved.evidence, rationale: "fits", ...resolution },
    ...overrides,
  };
}

test("an engineer_approved resolution without a recorded answer is rejected", () => {
  const result = auditDecisions(doc([yamlDecision()]), []);
  assert.equal(result.errors.length, 1);
  assert.match(result.errors[0]!, /AD-1: resolution\.kind engineer_approved is not backed/);
});

test("a resolution matching the ledger passes, tolerating whitespace differences", () => {
  const result = auditDecisions(
    doc([yamlDecision({ choice: "A  durable\n task." }, { evidence: approved.evidence.replace(" in a ", "  in a\n") })]),
    [approved],
  );
  assert.deepEqual(result.errors, []);
});

test("a choice or evidence that differs from the engineer's answer is rejected", () => {
  const changedChoice = auditDecisions(doc([yamlDecision({ choice: "Synchronous." })]), [approved]);
  assert.equal(changedChoice.errors.length, 1);
  assert.match(changedChoice.errors[0]!, /AD-1: choice differs/);
  const changedEvidence = auditDecisions(doc([yamlDecision({}, { evidence: "Engineer approved in discussion." })]), [approved]);
  assert.equal(changedEvidence.errors.length, 1);
  assert.match(changedEvidence.errors[0]!, /AD-1: resolution\.evidence must be exactly/);
});

test("the recorded kind must match: a delegated answer cannot be written as engineer_approved", () => {
  const result = auditDecisions(doc([yamlDecision()]), [{ ...approved, kind: "delegated" }]);
  assert.match(result.errors[0]!, /resolution\.kind is engineer_approved but the engineer's answer recorded delegated/);
});

test("an answered decision missing from the gameplan is reported; an unresolved one is not", () => {
  const result = auditDecisions(doc([]), [approved, { ...approved, id: "AD-2", kind: "unresolved", evidence: "", reason: "dismissed" }]);
  assert.deepEqual(result.errors, ["AD-1: answered by the engineer but missing from architectureDesign.decisions"]);
});

test("constrained decisions are collected for the engineer's acceptance, not treated as errors", () => {
  const result = auditDecisions(
    doc([yamlDecision({ id: "AD-3", choice: "Use the existing migration tool." }, { kind: "constrained", evidence: "The repo pins one migration tool." })]),
    [],
  );
  assert.deepEqual(result.errors, []);
  assert.deepEqual(result.constrained, [
    { id: "AD-3", question: "How should it execute?", choice: "Use the existing migration tool.", evidence: "The repo pins one migration tool." },
  ]);
});

test("open questions are surfaced and non-mapping documents are rejected", () => {
  assert.deepEqual(auditDecisions(doc([], ["Which flag?"]), []).openQuestions, ["Which flag?"]);
  assert.deepEqual(auditDecisions("nope", []).errors, ["gameplan is not a mapping"]);
  assert.deepEqual(auditDecisions({}, []).errors, []);
});


test("questions, alternatives and unique IDs are bound to the recorded dialog", () => {
  for (const [change, message] of [
    [{ question: "Which database?" }, /question differs/],
    [{ alternatives: [] }, /alternatives differ/],
    [{ alternatives: [{ choice: "Synchronous.", tradeoffs: "Invented." }] }, /alternatives differ/],
  ] as const) {
    const result = auditDecisions(doc([yamlDecision(change)]), [approved]);
    assert.equal(result.errors.length, 1);
    assert.match(result.errors[0]!, message);
  }
  assert.deepEqual(auditDecisions(doc([yamlDecision(), yamlDecision()]), [approved]).errors, ["AD-1: duplicate decision id"]);
});

test("answered decisions cannot be downgraded to constrained or unresolved", () => {
  for (const kind of ["constrained", "unresolved"]) {
    const result = auditDecisions(doc([yamlDecision({}, { kind })]), [approved]);
    assert.equal(result.errors.length, 1);
    assert.match(result.errors[0]!, /engineer's answer recorded engineer_approved/);
  }
});
