/**
 * Pure mapping between a gameplan decision and the `ask_user_question` tool
 * (@hank-warren/pi-ask-user-question, envelope shared with @juicesharp/rpiv-ask-user-question).
 *
 * When that tool is registered, `gameplan_decide` delegates the dialog to it for the richer
 * tabbed UI with descriptions and notes. These functions build its request and read its answer
 * back into an `Answer`; they never touch the UI themselves.
 */

import type { Answer, DecideParams } from "./state.js";

export const ASK_USER_TOOL = "ask_user_question";
/** The tool allows at most four single-select options; one is reserved for delegation when it fits. */
const MAX_OPTIONS = 4;
export const DELEGATE_LABEL = "Delegate to the agent";
const RECOMMENDED_SUFFIX = " (recommended)";

export interface AskUserOption {
  label: string;
  description: string;
}

export interface AskUserRequest {
  questions: [{ question: string; header: string; options: AskUserOption[] }];
}

/** Whether the decision fits the tool's option limits at all. */
export function fitsAskUser(params: DecideParams): boolean {
  return params.options.length >= 2 && params.options.length <= MAX_OPTIONS;
}

/** Build the single-question request. The delegate row is included only when there is room. */
export function buildAskUserRequest(params: DecideParams): AskUserRequest {
  const options: AskUserOption[] = params.options.map((o, i) => ({
    label: `${i + 1}. ${o.choice}${i === params.recommendation ? RECOMMENDED_SUFFIX : ""}`,
    description: o.tradeoffs,
  }));
  if (options.length < MAX_OPTIONS) {
    options.push({
      label: DELEGATE_LABEL,
      description: "Record the recommendation as delegated. Add a note to bound the delegation.",
    });
  }
  return {
    questions: [{ question: params.question, header: params.id.slice(0, 16), options }],
  };
}

/** The shape of the tool's `details` we rely on; everything else is ignored. */
export interface AskUserDetails {
  answers?: { questionIndex?: number; answer?: string; custom?: boolean; notes?: string }[];
  cancelled?: boolean;
  error?: string;
}

/**
 * Read the answer back. A chosen label maps to its option index; the delegate row maps to a
 * delegation whose scope is the user's note; free text requests discussion, never approval. Anything
 * unreadable is treated as cancelled so the decision stays unresolved rather than guessed.
 */
export function answerFromAskUser(params: DecideParams, details: unknown): Answer {
  const d = (typeof details === "object" && details !== null ? details : {}) as AskUserDetails;
  if (d.cancelled || d.error) return { kind: "cancelled" };
  const first = d.answers?.find((a) => (a.questionIndex ?? 0) === 0);
  const text = first?.answer?.trim();
  if (!text) return { kind: "cancelled" };
  const note = first?.notes?.trim();
  if (first?.custom) {
    return { kind: "discussion", text: note ? `${text} (${note})` : text };
  }
  if (text === DELEGATE_LABEL) return { kind: "delegated", scope: note ?? "" };
  const request = buildAskUserRequest(params);
  const index = request.questions[0].options.findIndex((o) => o.label === text);
  if (index < 0 || index >= params.options.length) {
    // The label was not one we authored; keep the user's words without granting approval.
    return { kind: "discussion", text: note ? `${text} (${note})` : text };
  }
  return note ? { kind: "selected", index, note } : { kind: "selected", index };
}
