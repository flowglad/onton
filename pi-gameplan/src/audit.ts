/**
 * Cross-check a parsed gameplan document against the decision ledger.
 *
 * The Python validator proves the YAML is well-formed and internally consistent. This audit
 * proves the one thing it cannot: that every `engineer_approved` or `delegated` resolution was
 * actually answered by the engineer through `gameplan_decide`, not written by the model.
 */

import type { DecisionRecord } from "./state.js";

export interface ConstrainedDecision {
  id: string;
  question: string;
  choice: string;
  evidence: string;
}

export interface AuditResult {
  /** Resolutions the ledger does not back, or that disagree with it. */
  errors: string[];
  /** Agent-asserted `constrained` decisions; the finalize gate asks the engineer to accept them. */
  constrained: ConstrainedDecision[];
  openQuestions: string[];
}

function normalize(text: unknown): string {
  return typeof text === "string" ? text.replace(/\s+/g, " ").trim() : "";
}

function asRecord(value: unknown): Record<string, unknown> | undefined {
  return typeof value === "object" && value !== null && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : undefined;
}

export function auditDecisions(doc: unknown, ledger: readonly DecisionRecord[]): AuditResult {
  const result: AuditResult = { errors: [], constrained: [], openQuestions: [] };
  const root = asRecord(doc);
  if (!root) {
    result.errors.push("gameplan is not a mapping");
    return result;
  }

  const open = root.openQuestions;
  if (Array.isArray(open)) {
    result.openQuestions = open.filter((q): q is string => typeof q === "string");
  }

  const design = asRecord(root.architectureDesign);
  const decisions = Array.isArray(design?.decisions) ? design.decisions : [];
  const seen = new Set<string>();

  for (const raw of decisions) {
    const decision = asRecord(raw);
    if (!decision) continue;
    const id = typeof decision.id === "string" ? decision.id : "<missing id>";
    seen.add(id);
    const resolution = asRecord(decision.resolution);
    const kind = resolution?.kind;

    if (kind === "constrained") {
      result.constrained.push({
        id,
        question: normalize(decision.question),
        choice: normalize(decision.choice),
        evidence: normalize(resolution?.evidence),
      });
      continue;
    }
    if (kind !== "engineer_approved" && kind !== "delegated") continue;

    const record = ledger.find((d) => d.id === id);
    if (!record) {
      result.errors.push(
        `${id}: resolution.kind ${kind} is not backed by a gameplan_decide answer; ask the engineer with gameplan_decide`,
      );
      continue;
    }
    if (record.kind !== kind) {
      result.errors.push(`${id}: resolution.kind is ${kind} but the engineer's answer recorded ${record.kind}`);
    }
    if (normalize(decision.choice) !== normalize(record.choice)) {
      result.errors.push(`${id}: choice differs from the engineer's answer (${JSON.stringify(record.choice)})`);
    }
    if (normalize(resolution?.evidence) !== normalize(record.evidence)) {
      result.errors.push(`${id}: resolution.evidence must be exactly the text gameplan_decide returned`);
    }
  }

  for (const record of ledger) {
    if (record.kind !== "unresolved" && !seen.has(record.id)) {
      result.errors.push(`${record.id}: answered by the engineer but missing from architectureDesign.decisions`);
    }
  }

  return result;
}
