/** Pure session state. Only UI answers may resolve implementation decisions. */
export interface Option { choice: string; tradeoffs: string }
export interface Question {
  id: string;
  topic: string;
  question: string;
  options: Option[];
  recommendation: number;
  why: string;
}
export type Answer =
  | { kind: "selected"; index: number }
  | { kind: "discussion"; text: string }
  | { kind: "delegated"; scope: string }
  | { kind: "cancelled" };
export interface Decision extends Question {
  resolution:
    | { kind: "approved"; choice: string }
    | { kind: "delegated"; choice: string; scope: string }
    | { kind: "unresolved"; reason: string };
}
export interface State {
  enabled: boolean;
  directory: string;
  path?: string;
  decisions: Decision[];
  finalized?: { path: string; digest: string };
}
export const ENTRY = "workstream-mode";
export const initialState: State = { enabled: false, directory: "docs/workstreams", decisions: [] };

export function answerDecision(question: Question, answer: Answer): Decision {
  const recommended = question.options[question.recommendation];
  let resolution: Decision["resolution"];
  switch (answer.kind) {
    case "selected": {
      const option = question.options[answer.index];
      resolution = option ? { kind: "approved", choice: option.choice }
        : { kind: "unresolved", reason: "No offered option was selected." };
      break;
    }
    case "delegated":
      resolution = recommended && answer.scope.trim()
        ? { kind: "delegated", choice: recommended.choice, scope: answer.scope.trim() }
        : { kind: "unresolved", reason: "Delegation requires a valid recommendation and a non-empty scope." };
      break;
    case "discussion": resolution = { kind: "unresolved", reason: answer.text }; break;
    case "cancelled": resolution = { kind: "unresolved", reason: "Dismissed without answering." }; break;
  }
  return { ...question, resolution };
}

export function recordDecision(state: State, decision: Decision): State {
  const found = state.decisions.some((d) => d.id === decision.id);
  return { ...state, finalized: undefined, decisions: found
    ? state.decisions.map((d) => d.id === decision.id ? decision : d)
    : [...state.decisions, decision] };
}

export function startDraft(state: State, path: string): State {
  return { ...state, path, decisions: [], finalized: undefined };
}

function object(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}
function strings(value: Record<string, unknown>, keys: string[]): boolean {
  return keys.every((key) => typeof value[key] === "string");
}
function isDecision(value: unknown): value is Decision {
  if (!object(value) || !strings(value, ["id", "topic", "question", "why"]) || !Array.isArray(value.options)
    || !value.options.every((o) => object(o) && strings(o, ["choice", "tradeoffs"]))
    || !Number.isInteger(value.recommendation) || typeof value.recommendation !== "number"
    || value.recommendation < 0 || value.recommendation >= value.options.length || !object(value.resolution)) return false;
  const r = value.resolution;
  return (r.kind === "approved" && typeof r.choice === "string")
    || (r.kind === "delegated" && strings(r, ["choice", "scope"]))
    || (r.kind === "unresolved" && typeof r.reason === "string");
}
function isState(value: unknown): value is State {
  return object(value) && typeof value.enabled === "boolean" && typeof value.directory === "string"
    && (value.path === undefined || typeof value.path === "string")
    && Array.isArray(value.decisions) && value.decisions.every(isDecision)
    && new Set(value.decisions.map((d) => d.id)).size === value.decisions.length
    && (value.finalized === undefined || (object(value.finalized) && strings(value.finalized, ["path", "digest"])));
}
export function fromEntries(entries: readonly { type: string; customType?: string; data?: unknown }[]): State {
  let state = initialState;
  for (const entry of entries) {
    if (entry.type === "custom" && entry.customType === ENTRY && isState(entry.data)) state = entry.data;
  }
  return state;
}

export function describeDecisions(decisions: readonly Decision[]): string {
  return decisions.map((d) => `${d.id} · ${d.topic}: ${JSON.stringify(d.resolution)}`).join("\n") || "None recorded yet.";
}
