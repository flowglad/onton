# Prompting patch agents

Design notes for the prompts onton sends to its patch-agent workers.

## Why this is its own problem

Every prompt in `lib/prompt.ml` is consumed by an autonomous worker, not a
human. There is no follow-up turn, no clarifying question, no "let me know if
you need more info." The orchestrator hands a patch agent a single
self-contained brief and expects it to land a buildable, test-passing,
spec-satisfying commit on its branch.

That changes what good prompting looks like. Most public prompt-engineering
advice still assumes a human is on the other end. This doc captures the
patterns that are specifically about *programmatic* prompting — orchestrator
to worker, no human in the loop — and audits onton's current prompts against
them.

Three goals, in priority order:

1. **Independence.** The worker has to make and document decisions, not stop
   to ask. Anything that could be a clarification request needs to be
   rewritten as an assume-and-record instruction.
2. **Rigor.** No human catches errors mid-session, so completion must be
   defined mechanically (build green, tests pass, spec clauses mapped) and
   the worker must verify against that definition before stopping.
3. **Token efficiency.** Patch agents run for many turns and may be retried.
   Every redundant token in the prompt multiplies across the session and
   across retries.

## Principles

### Independence

- **Replace ask-style with assume-and-document.** Anywhere a prompt invites
  the worker to ask a question, replace it with a directive to make a
  reasonable assumption, proceed, and record the assumption in the
  implementation notes. The orchestrator can correct via a follow-up message
  if the assumption was wrong.
- **Equip with self-context tools.** Workers should retrieve their own
  context (grep, file reads, `git log`, `gh api`) rather than receive a
  pre-loaded dump. The patch prompt's job is to point at the relevant
  surfaces, not to inline them.
- **Negative instructions over positive examples.** Models overfit to
  examples. "Do not run `git push`" is more durable than "here's an example
  of what to commit." Onton's current prompts already do this for the
  push/PR boundary; the pattern should generalize.
- **Agentic persistence reminders.** From OpenAI's GPT-5/Codex prompting
  guide: "persist until the task is fully handled end-to-end within the
  current turn whenever feasible: do not stop at analysis or partial
  fixes." This is the single highest-leverage line to add to autonomous
  prompts.
- **Force completion through structured signaling.** The `done` state
  should be reachable only through an explicit signal — a final commit, a
  written artifact file, a parseable result block — not through
  conversational "I'm done." This already exists implicitly via the
  push/commit boundary; the PR-body and review prompts should make it
  explicit.

### Rigor

- **Define "done" mechanically.** Acceptance is `dune build` clean +
  `dune runtest` clean + every spec clause mapped + every acceptance
  criterion ticked. If a goal cannot be reduced to a mechanical check, it
  should not be in the acceptance list — it should be in the description.
- **Rules-based feedback beats prose feedback.** Linters, type checkers,
  and tests are the verification substrate. Onton already pipes these to
  the worker via CI failures, but the patch prompt should also tell the
  worker how to run the same checks locally before declaring done.
- **JSON tracking artifacts > Markdown.** Anthropic's long-running-agent
  research notes that models are measurably less likely to overwrite or
  tamper with JSON than Markdown. For artifacts the orchestrator owns
  (progress logs, feature checklists), prefer JSON.
- **Forbid editing tests.** Workers will delete or weaken failing tests
  if not explicitly forbidden. This belongs in the patch prompt verbatim.
- **End-to-end, not just unit.** Without explicit prompting, agents
  declare done after the unit test passes even when the feature is broken
  end-to-end. The acceptance criteria for behavioral patches should
  include an integration-level check.
- **Adversarial verifier (future).** SOTA harnesses run a separate
  verifier subagent that adversarially tests the implementation and
  returns PASS/FAIL. Onton's review-comment loop is a weaker version of
  this; a dedicated pre-PR self-review pass is a candidate.

### Token efficiency

- **Static system prompt, dynamic user message.** Anthropic's prompt
  cache has a 5-minute TTL, but it is also keyed on prefix stability. Any
  changing state (current SHA, timestamp, branch HEAD, PR number) in the
  system prompt invalidates the cache on every turn. Onton currently
  sends everything as a single user message; if we ever add a system
  prompt, the dynamic state must stay in the user turn.
- **Tools return references, not payloads.** The CI-failure prompt links
  to logs by URL rather than inlining them — good. The merge-conflict
  prompt inlines `git status` and `git diff` — less good for large
  conflicts. Consider a length budget with truncation that preserves
  prefix and suffix (where conflict markers and stack traces live), not
  the middle.
- **Subagents for filtered retrieval.** When the worker needs to scan a
  large surface (every caller of a function, every test file matching a
  pattern), it should spawn a subagent that returns a distilled summary,
  not load everything into its own context.
- **Prompt-cache-friendly growth.** Within a worker session, follow-up
  messages should *append* rather than replace. Onton's design — separate
  prompts per event (review, CI, merge conflict, human message) sent as
  fresh user turns to a resumed Claude session — is already
  cache-friendly. Don't break this by editing prior turns.

### Patterns specific to orchestrator-driven workers

- **Self-contained per-task prompts.** Workers cannot ask follow-ups, so
  the per-task prompt must brief them like a colleague who just walked
  in: goal, what's been ruled out, what counts as done, where to write
  the result. The `render_patch_prompt` template already does this well;
  the per-event prompts (review, CI, merge conflict) inherit context
  from the resumed session, which is fine but fragile if the session
  is ever lost.
- **Workers report via structured output, not prose.** The PR-body
  prompt has the worker write to an artifact file at a known path —
  this is exactly the right pattern. Other prompts that produce
  inspectable results (e.g., a "did you address all comments" check)
  should follow the same artifact pattern rather than relying on the
  orchestrator to scrape the transcript.
- **Attention distribution.** Models weight tokens roughly:
  user-message > start-of-context > middle. Critical instructions
  (don't push, don't edit tests, don't change the PR base) should land
  near the top or bottom of the prompt, not buried in the middle.

## Audit of onton's current prompts

The renderers live in `lib/prompt.ml`. Each is consumed by a fresh or
resumed Claude session via `run_claude_and_handle` in `bin/main.ml`.

### `render_patch_prompt` — initial implementation

Strong points:
- Sections ordered by relevance: problem, solution, dependencies, task,
  changes, files, spec, acceptance criteria, git instructions.
- Push/PR boundary is explicit and uses negative instructions ("do NOT
  run `git push`").
- Spec section includes a Pantagruel syntax key — gives the worker
  enough to read the spec without an external doc.
- Base-branch note explains the dependency-stack situation when the
  worker is rebased onto a non-main branch.

Gaps to address:
- No explicit "done" definition. Add a "## How to know you're done"
  section listing: build clean, tests clean, every spec clause mapped,
  every acceptance criterion ticked, and a final commit on the branch.
- No persistence reminder. Add a top-of-prompt line: "Work end-to-end
  within this session. Do not stop at analysis or a partial fix."
- No forbid-editing-tests clause. Add it.
- No "what to do if blocked" guidance. Today, a stuck worker either
  loops or stops. Recommended: "If you cannot satisfy a spec clause or
  acceptance criterion, complete what you can, document the blocker
  in the implementation notes artifact, and exit. Do not weaken the
  spec or skip tests."
- Patches list is rendered in full for every patch in the gameplan.
  For large gameplans this is wasted tokens — the worker only needs
  its own dependencies. Consider listing only `dependencies` plus the
  current patch.

### `render_pr_body_prompt` — implementation notes

Strong points:
- Artifact-file pattern (write to absolute path, supervisor reads it)
  is exactly the right autonomous-agent shape: structured handoff, not
  scraped transcript.
- Negative instruction against running `gh`/`git` is clear.
- Explicit "what to include" list, with instruction to not duplicate
  the description.

Gaps:
- "If you have nothing material to add … write a single line" is good,
  but workers tend to over-write. Consider an explicit length budget
  ("under 200 words" or "5 bullets max").
- No mention of the assumptions-recording channel. If the patch prompt
  starts requiring workers to document assumptions (per the
  Independence section above), the implementation-notes artifact is
  the right place — say so here.

### `render_review_prompt` — address review comments

Strong points:
- The `[outdated]`/`[at=…]` SHA-anchoring is the gold-standard pattern
  for resilience against later commits — keeps the worker from
  re-doing fixes that already landed.
- Reply/resolve is supervisor-owned, following the pr-body artifact
  model: the worker writes one response file per comment
  (`comment_responses/<comment_id>.md`, response text only) and is
  never asked to reply or resolve itself (gh stays available for its
  own investigation). After the post-session push succeeds, the
  supervisor posts each response as a thread reply and resolves that
  thread (`Comment_responder.respond_after_session`), so a "fixed in
  <sha>" reply can never precede the fix reaching the remote, and a
  worker can no longer resolve a thread without responding.
- Comments without a response file stay unresolved and re-deliver on
  the next poll — convergence is gated by the orchestrator, not by
  trusting the worker to have run CLI commands.

Gaps:
- The prompt does not constrain the worker against re-implementing
  previously-resolved threads or reverting earlier fixes. A short
  "Do not modify code outside the scope of these comments" line would
  help.
- Disagreement ("the current approach is correct") lands in the
  response file and thus on the GitHub thread, but the orchestrator
  does not act on its content — a structured disagreement field could
  let it distinguish "fixed" from "wontfix" replies.

### `render_ci_failure_prompt` and `render_ci_failure_unknown_prompt`

Strong points:
- Per-check formatting with name, conclusion, URL, description.
- Push boundary is restated (negative instruction).

Gaps:
- The unknown-failure variant is essentially "go figure it out." This
  is the right shape — workers have grep and gh — but a starter list
  ("try `dune build`, `dune runtest`, then `gh run view`") would save
  tokens of exploration.
- No diagnosis-vs-fix split. Sometimes the right answer is "this CI
  check is flaky" or "this is an infra problem, not my patch." Today
  the worker has no structured way to report that — it'll keep
  trying. Add: "If you determine the failure is unrelated to your
  patch (flaky test, infra outage), write that conclusion to
  `<artifact_path>` and stop. Do not loop."

### `render_merge_conflict_prompt`

Strong points:
- Explicit "rebase already in progress" framing prevents the
  worker from running `git rebase origin/<base>` and re-introducing
  dependency commits.
- Inlines `git status` and conflict-marker diff, so the worker has
  the context immediately.
- Optional task-context section reminds the worker what the patch
  was about — important if the conflict is in unfamiliar code.

Gaps:
- No length budget on the inlined diff. A pathological conflict
  (large refactor against the patch) could blow up the prompt. Add a
  truncation strategy: keep first N and last N lines, drop middle
  with a "[truncated K lines]" marker.
- No fallback if `--continue` keeps failing. Workers will loop.
  Add: "If after three resolution attempts the rebase still fails,
  write the unresolved conflicts to `<artifact_path>` and stop."

### `render_human_message_prompt`

Strong points:
- Pass-through is the right design — the orchestrator does not try
  to interpret the human message, just delivers it numbered.
- Distinct singular/plural framing is a small but real quality
  improvement.

Gaps:
- This is the one prompt where a human *is* in the loop, indirectly.
  Per memory `feedback_human_message_no_commit_suffix.md`, do not
  append commit-and-push instructions here.

### `render_base_branch_changed`

This is a notification, not a task — kept short and instructional.
The negative instruction ("Do NOT change the PR base branch") is
correctly placed. No changes recommended.

## Proposed changes, prioritized

P0 — high leverage, low risk:

1. Add a "How to know you're done" section to `render_patch_prompt`
   listing the mechanical completion criteria.
2. Add a forbid-editing-tests clause to `render_patch_prompt`.
3. Add a persistence reminder to `render_patch_prompt`.
4. Add a length budget to `render_pr_body_prompt`.

P1 — moderate leverage, requires a small protocol change:

5. Add an assumptions-and-blockers channel: a section in the
   implementation-notes artifact where the worker records decisions it
   made under uncertainty. Surfaces in the PR body and gives the
   orchestrator (or human reviewer) a place to look.
6. Truncate inlined `git diff` in `render_merge_conflict_prompt` with
   prefix-suffix preservation.
7. Add a "give up after N attempts" instruction to the merge-conflict
   and CI-failure prompts, with a structured exit artifact.

P2 — larger investments, worth scoping separately:

8. Trim the patches-list in `render_patch_prompt` to dependencies +
   self, not the whole gameplan.
9. Pre-PR self-review pass: a verifier subagent that runs after
   implementation and before the orchestrator opens the PR, returning
   PASS/FAIL with structured diagnostics.
10. Move the static, never-changing parts of `render_patch_prompt`
    (Pantagruel syntax key, push/PR boundary, persistence reminder,
    "done" definition) into a system prompt passed via
    `--append-system-prompt`, so they stay cache-stable across worker
    sessions and stop counting against per-message token budgets.

## Open questions

- **Should we adopt a per-prompt template-override convention?** The
  override system in `prompt.ml` already supports loading from
  `prompts/<name>.md`. We could ship a default template per prompt
  type and version them, so projects can opt in to changes rather
  than have them rolled out implicitly.
- **Is there value in a verifier subagent that runs *during*
  implementation, not just after?** Trade-off: more tokens, more
  latency, but catches drift earlier. Probably worth piloting on a
  single high-stakes patch class (BEHAVIOR with a non-empty spec)
  before generalizing.
- **How do we measure prompt changes?** We need a regression suite
  for prompts — a corpus of past patches with known outcomes — so we
  can A/B prompt revisions without flying blind. Today `lib_test/`
  tests the renderer's output shape, not its end-to-end effect on
  agent behavior.

## References

- [Effective harnesses for long-running agents — Anthropic](https://www.anthropic.com/engineering/effective-harnesses-for-long-running-agents)
- [Effective context engineering for AI agents — Anthropic](https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents)
- [Building agents with the Claude Agent SDK — Anthropic](https://claude.com/blog/building-agents-with-the-claude-agent-sdk)
- [11 prompting techniques for better AI agents — Augment Code](https://www.augmentcode.com/blog/how-to-build-your-agent-11-prompting-techniques-for-better-ai-agents)
- [Best practices for coding with agents — Cursor](https://cursor.com/blog/agent-best-practices)
- [GPT-5 prompting guide — OpenAI](https://developers.openai.com/cookbook/examples/gpt-5/gpt-5_prompting_guide)
- [Codex Subagents — OpenAI](https://developers.openai.com/codex/subagents)
- [Agents: Loop Control — Vercel AI SDK](https://ai-sdk.dev/docs/agents/loop-control)
- [Context Engineering for Coding Agents — Martin Fowler](https://martinfowler.com/articles/exploring-gen-ai/context-engineering-coding-agents.html)
