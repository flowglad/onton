import assert from "node:assert/strict";
import { test } from "node:test";
import { answerFromAskUser, buildAskUserRequest, DELEGATE_LABEL, fitsAskUser } from "../src/ask.js";
import type { DecideParams } from "../src/state.js";

const params: DecideParams = {
  id: "AD-7",
  topic: "persistence",
  question: "Where should qualification evidence live?",
  options: [
    { choice: "Relational rows in the app database.", tradeoffs: "Queryable; grows the hot table." },
    { choice: "Immutable objects in the artifact bucket.", tradeoffs: "Cheap; needs a lookup index." },
  ],
  recommendation: 1,
};

test("the request marks the recommendation, carries tradeoffs, and adds a delegate row when there is room", () => {
  const request = buildAskUserRequest(params);
  const q = request.questions[0];
  assert.equal(q.header, "AD-7");
  assert.equal(q.question, params.question);
  assert.deepEqual(
    q.options.map((o) => o.label),
    ["1. Relational rows in the app database.", "2. Immutable objects in the artifact bucket. (recommended)", DELEGATE_LABEL],
  );
  assert.equal(q.options[0]!.description, "Queryable; grows the hot table.");
});

test("four options leave no room for the delegate row; five do not fit the tool", () => {
  const four = { ...params, options: [...params.options, ...params.options] };
  assert.ok(fitsAskUser(four));
  assert.equal(buildAskUserRequest(four).questions[0].options.length, 4);
  assert.ok(!buildAskUserRequest(four).questions[0].options.some((o) => o.label === DELEGATE_LABEL));
  const five = { ...params, options: [...four.options, params.options[0]!] };
  assert.ok(!fitsAskUser(five));
});

test("a chosen label maps back to its option index, with the note preserved", () => {
  const label = buildAskUserRequest(params).questions[0].options[1]!.label;
  assert.deepEqual(answerFromAskUser(params, { answers: [{ questionIndex: 0, answer: label, custom: false }] }), { kind: "selected", index: 1 });
  assert.deepEqual(
    answerFromAskUser(params, { answers: [{ questionIndex: 0, answer: label, custom: false, notes: "only for the new flow" }] }),
    { kind: "selected", index: 1, note: "only for the new flow" },
  );
});

test("the delegate row becomes a delegation scoped by the note", () => {
  assert.deepEqual(
    answerFromAskUser(params, { answers: [{ questionIndex: 0, answer: DELEGATE_LABEL, custom: false, notes: "either store, keep it async" }] }),
    { kind: "delegated", scope: "either store, keep it async" },
  );
});

test("typed answers and unknown labels request discussion; declines and errors cancel", () => {
  assert.deepEqual(
    answerFromAskUser(params, { answers: [{ questionIndex: 0, answer: "Reuse the outbox table", custom: true }] }),
    { kind: "discussion", text: "Reuse the outbox table" },
  );
  assert.deepEqual(
    answerFromAskUser(params, { answers: [{ questionIndex: 0, answer: "Something else", custom: false }] }),
    { kind: "discussion", text: "Something else" },
  );
  assert.deepEqual(answerFromAskUser(params, { cancelled: true, answers: [] }), { kind: "cancelled" });
  assert.deepEqual(answerFromAskUser(params, { error: "no_ui", answers: [] }), { kind: "cancelled" });
  assert.deepEqual(answerFromAskUser(params, { answers: [] }), { kind: "cancelled" });
  assert.deepEqual(answerFromAskUser(params, "garbage"), { kind: "cancelled" });
});


test("typed option labels and delegation labels cannot grant consent", () => {
  for (const answer of [buildAskUserRequest(params).questions[0].options[0]!.label, DELEGATE_LABEL]) {
    assert.deepEqual(answerFromAskUser(params, { answers: [{ answer, custom: true, notes: "Why?" }] }),
      { kind: "discussion", text: `${answer} (Why?)` });
  }
});

test("duplicate choices still identify the selected option", () => {
  const duplicate = { ...params, recommendation: 2, options: [
    { choice: "Same", tradeoffs: "First" },
    { choice: "Same", tradeoffs: "Second" },
    { choice: "Different", tradeoffs: "Third" },
  ] };
  const labels = buildAskUserRequest(duplicate).questions[0].options;
  assert.equal(new Set(labels.map((o) => o.label)).size, labels.length);
  assert.deepEqual(answerFromAskUser(duplicate, { answers: [{ answer: labels[1]!.label }] }), { kind: "selected", index: 1 });
});
