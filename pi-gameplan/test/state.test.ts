import assert from "node:assert/strict";
import { test } from "node:test";
import {
  CUSTOM_ENTRY_TYPE,
  type DecideParams,
  fromEntries,
  initialState,
  recordDecision,
  resolveDecision,
} from "../src/state.js";

const params: DecideParams = {
  id: "AD-1",
  topic: "execution",
  question: "How should qualification execute outside the request?",
  options: [
    { choice: "A durable asynchronous task with retries.", tradeoffs: "More machinery; survives restarts." },
    { choice: "Synchronous execution in the admission request.", tradeoffs: "Simple; couples latency to the provider." },
    { choice: "A cron sweep every five minutes.", tradeoffs: "No new queue; up to five minutes of lag." },
  ],
  recommendation: 0,
};
const now = new Date("2026-10-16T12:00:00.000Z");

test("a selected option is engineer_approved with tool-generated evidence and the other options as alternatives", () => {
  const record = resolveDecision(params, { kind: "selected", index: 1 }, now);
  assert.equal(record.kind, "engineer_approved");
  assert.equal(record.choice, params.options[1]!.choice);
  assert.deepEqual(
    record.alternatives.map((a) => a.choice),
    [params.options[0]!.choice, params.options[2]!.choice],
  );
  assert.match(record.evidence, /Engineer selected "Synchronous execution in the admission request\." in a gameplan_decide dialog on 2026-10-16T12:00:00\.000Z\./);
});

test("a typed answer is engineer_approved and keeps every offered option as an alternative", () => {
  const record = resolveDecision(params, { kind: "other", text: "  Reuse the existing outbox worker.  " }, now);
  assert.equal(record.kind, "engineer_approved");
  assert.equal(record.choice, "Reuse the existing outbox worker.");
  assert.equal(record.alternatives.length, 3);
  assert.match(record.evidence, /Engineer answered "Reuse the existing outbox worker\."/);
});

test("delegation records the recommendation as delegated with the stated scope", () => {
  const record = resolveDecision(params, { kind: "delegated", scope: "anything that keeps admission synchronous" }, now);
  assert.equal(record.kind, "delegated");
  assert.equal(record.choice, params.options[0]!.choice);
  assert.match(record.evidence, /delegated this choice .* anything that keeps admission synchronous\./);
});

test("a dismissed dialog or missing UI leaves the decision unresolved with no evidence", () => {
  for (const answer of [{ kind: "cancelled" } as const, { kind: "no-ui" } as const]) {
    const record = resolveDecision(params, answer, now);
    assert.equal(record.kind, "unresolved");
    assert.equal(record.evidence, "");
    assert.ok(record.reason && record.reason.length > 0);
  }
});

test("an out-of-range recommendation or selection is rejected", () => {
  assert.throws(() => resolveDecision({ ...params, recommendation: 9 }, { kind: "selected", index: 0 }, now));
  assert.throws(() => resolveDecision(params, { kind: "selected", index: 9 }, now));
});

test("recording the same id again replaces it in place", () => {
  const first = resolveDecision(params, { kind: "cancelled" }, now);
  const second = resolveDecision(params, { kind: "selected", index: 0 }, now);
  let state = recordDecision(initialState, first);
  state = recordDecision(state, resolveDecision({ ...params, id: "AD-2" }, { kind: "selected", index: 1 }, now));
  state = recordDecision(state, second);
  assert.deepEqual(state.decisions.map((d) => [d.id, d.kind]), [["AD-1", "engineer_approved"], ["AD-2", "engineer_approved"]]);
});

test("state is rebuilt from the last gameplan-mode entry on the branch and ignores malformed ones", () => {
  const older = recordDecision({ ...initialState, enabled: true }, resolveDecision(params, { kind: "selected", index: 0 }, now));
  const newer = { ...older, gameplanDir: "plans" };
  const entries = [
    { type: "message" },
    { type: "custom", customType: CUSTOM_ENTRY_TYPE, data: older },
    { type: "custom", customType: "other-extension", data: { enabled: false } },
    { type: "custom", customType: CUSTOM_ENTRY_TYPE, data: { bogus: true } },
    { type: "custom", customType: CUSTOM_ENTRY_TYPE, data: newer },
  ];
  assert.deepEqual(fromEntries(entries), newer);
  assert.deepEqual(fromEntries([]), initialState);
});
