/**
 * Pure gameplan-mode state: the decision ledger and its session persistence shape.
 *
 * Nothing here touches Pi, the file system, or the UI. Everything is a total function of
 * its inputs so it can be property-tested and reconstructed from a session branch.
 */

export type ResolutionKind = "engineer_approved" | "delegated" | "unresolved";

/** One architectural decision as answered (or not) by the engineer through `gameplan_decide`. */
export interface DecisionRecord {
  id: string;
  topic: string;
  question: string;
  /** The choice the gameplan must carry. For `delegated`, the agent's recommendation. */
  choice: string;
  /** Alternatives the gameplan must list (every offered option except `choice`). */
  alternatives: DecisionOption[];
  kind: ResolutionKind;
  /** Provenance text the gameplan's `resolution.evidence` must reproduce verbatim. */
  evidence: string;
  /** Why the decision is unresolved; only for `kind: "unresolved"`. */
  reason?: string;
  recordedAt: string;
}

export interface DecisionOption {
  choice: string;
  tradeoffs: string;
}

export interface DecideParams {
  id: string;
  topic: string;
  question: string;
  options: DecisionOption[];
  /** Index into `options` of the agent's recommendation. */
  recommendation: number;
}

/** What the engineer did in the dialog. */
export type Answer =
  | { kind: "selected"; index: number; note?: string }
  | { kind: "other"; text: string }
  | { kind: "delegated"; scope: string }
  | { kind: "cancelled" }
  | { kind: "no-ui" };

export interface GameplanState {
  enabled: boolean;
  /** Directory, relative to the project root, where the gameplan tools write gameplan documents. */
  gameplanDir: string;
  decisions: DecisionRecord[];
  /** Active tool names captured when the mode was enabled, restored when it is disabled. */
  toolsBeforeMode?: string[];
  /** The gameplan file this session last wrote or checked, relative to the project root. */
  gameplanPath?: string;
  /** Set once `gameplan_finalize` succeeds. */
  finalizedPath?: string;
}

export const CUSTOM_ENTRY_TYPE = "gameplan-mode";
export const DEFAULT_GAMEPLAN_DIR = "gameplans";

export const initialState: GameplanState = {
  enabled: false,
  gameplanDir: DEFAULT_GAMEPLAN_DIR,
  decisions: [],
};

/** Replace the record with the same id, or append. Order is first-recorded first. */
export function recordDecision(state: GameplanState, record: DecisionRecord): GameplanState {
  const index = state.decisions.findIndex((d) => d.id === record.id);
  const decisions =
    index < 0 ? [...state.decisions, record] : state.decisions.map((d, i) => (i === index ? record : d));
  return { ...state, decisions };
}

/**
 * Rebuild state from the active session branch: the last persisted entry wins. Abandoned
 * branches are not consulted, so a decision answered on another branch is not inherited.
 */
export function fromEntries(entries: readonly { type: string; customType?: string; data?: unknown }[]): GameplanState {
  let state = initialState;
  for (const entry of entries) {
    if (entry.type !== "custom" || entry.customType !== CUSTOM_ENTRY_TYPE) continue;
    const data = entry.data;
    if (!isGameplanState(data)) continue;
    state = data;
  }
  return state;
}

function isGameplanState(value: unknown): value is GameplanState {
  if (typeof value !== "object" || value === null) return false;
  const v = value as Record<string, unknown>;
  return typeof v.enabled === "boolean" && typeof v.gameplanDir === "string" && Array.isArray(v.decisions);
}

function quote(text: string): string {
  return `"${text.replace(/\s+/g, " ").trim()}"`;
}

/**
 * Turn the engineer's answer into the record the gameplan must carry. The evidence string is
 * generated here, never by the model, so a matching `resolution.evidence` in the YAML proves
 * the dialog happened.
 */
export function resolveDecision(params: DecideParams, answer: Answer, now: Date): DecisionRecord {
  const date = now.toISOString();
  const base = { id: params.id, topic: params.topic, question: params.question, recordedAt: date };
  const recommended = params.options[params.recommendation];
  if (!recommended) throw new Error(`recommendation index ${params.recommendation} is out of range`);
  switch (answer.kind) {
    case "selected": {
      const chosen = params.options[answer.index];
      if (!chosen) throw new Error(`selected index ${answer.index} is out of range`);
      const note = answer.note?.trim() ? ` Note: ${answer.note.trim()}` : "";
      return {
        ...base,
        choice: chosen.choice,
        alternatives: params.options.filter((_, i) => i !== answer.index),
        kind: "engineer_approved",
        evidence: `Engineer selected ${quote(chosen.choice)} in a gameplan_decide dialog on ${date}.${note}`,
      };
    }
    case "other":
      return {
        ...base,
        choice: answer.text.trim(),
        alternatives: params.options,
        kind: "engineer_approved",
        evidence: `Engineer answered ${quote(answer.text)} in a gameplan_decide dialog on ${date}.`,
      };
    case "delegated":
      return {
        ...base,
        choice: recommended.choice,
        alternatives: params.options.filter((_, i) => i !== params.recommendation),
        kind: "delegated",
        evidence: `Engineer delegated this choice to the agent in a gameplan_decide dialog on ${date}: ${answer.scope.trim() || "no scope stated"}.`,
      };
    case "cancelled":
      return {
        ...base,
        choice: recommended.choice,
        alternatives: params.options.filter((_, i) => i !== params.recommendation),
        kind: "unresolved",
        evidence: "",
        reason: `Engineer dismissed the gameplan_decide dialog on ${date} without answering.`,
      };
    case "no-ui":
      return {
        ...base,
        choice: recommended.choice,
        alternatives: params.options.filter((_, i) => i !== params.recommendation),
        kind: "unresolved",
        evidence: "",
        reason: "No interactive UI was available to ask the engineer.",
      };
  }
}

