# Workstream: Patch-agent prompting overhaul

## Vision

Restructure the prompts onton sends to its patch agents around the principles
of programmatic agent prompting — independence (no human-in-the-loop), rigor
(mechanical completion), and token efficiency (cache-stable system prompts,
referenced rather than inlined context). Every prompt change is measurable: a
modular evaluation harness scores prompt revisions against a corpus of past
patches via Braintrust. Static text moves into system prompts on backends that
support it (Claude, Codex), and a unified artifact protocol replaces ad-hoc
per-event handoffs between worker and orchestrator.

The verifier subagent (a separate adversarial worker that audits a patch
before the PR opens) is explicitly out of scope here and deserves its own
workstream.

## Current State

All prompts live in `lib/prompt.ml` and are concatenated into a single
user-message string. No system prompt is sent — every byte of static
boilerplate (Pantagruel syntax key, push/PR boundary, persistence reminders)
is paid per-turn and re-tokenized on every retry. The PR-body artifact is the
only structured worker→orchestrator handoff; review replies, CI diagnoses, and
merge-conflict outcomes are inferred from transcripts or git state.

Braintrust is set up at the org level but is not wired into onton. There is
no measurable answer to "did this prompt change help?" — every prompt edit
ships on intuition.

A design audit of each prompt renderer lives at
[`docs/design/patch-agent-prompting.md`](./patch-agent-prompting.md), with a
P0/P1/P2 backlog of concrete edits. This workstream sequences and ships that
backlog, gated on the eval harness.

## Key Challenges

- **Modular eval.** The Braintrust integration must not pollute the
  open-source distribution. The eval interface is in onton; the Braintrust
  binding lives in a separate, optional module that the OSS build does not
  depend on.
- **Backend asymmetry.** System-prompt support is a functional requirement
  for Claude and Codex. Gemini and OpenCode must fall back gracefully —
  inlining the static text into the user message — without breaking.
- **Worker contract evolution.** Each new artifact (assumptions log,
  give-up exit, structured disagreement) expands the worker's contract.
  Resumed sessions running against an older prompt version may not honor
  newer conventions; protocol changes must be opt-in or backwards
  compatible.
- **Eval signal vs corpus cost.** A useful corpus has to be large enough
  to discriminate between prompt revisions but small enough to maintain.
  The right size and metric set is itself an open question.
- **In-flight gameplans.** onton already has live patch worktrees. Default
  prompt edits land for them mid-flight. Each milestone must keep the
  existing contract working until the worker either finishes or restarts.

## Milestones

### Milestone 1: patch-agent-eval-harness

**Definition of Done**:
- An eval interface in `lib/eval.mli` defines: corpus loader, prompt
  rendering, scoring, comparison reports.
- A Braintrust binding lives in a separate module (e.g. `lib_eval_braintrust/`)
  that is not pulled in by the default OSS build — opt-in via a dune
  flag or separate workspace target.
- A starter corpus of 5–10 past patches with known outcomes is checked
  in (or referenced from a separate, possibly private repo if any
  patches are sensitive).
- `dune exec` (or a script) runs the eval against current prompts and
  produces a baseline report.
- The OSS build does not require Braintrust credentials to compile or
  pass `dune runtest`.

**Why this is a safe pause point**: No behavior change. The eval harness
is plumbing only; current prompts are unchanged. If we stop here, onton
ships with optional measurement infrastructure but no prompt edits.

**Unlocks**: Every subsequent milestone can validate its changes against
a baseline. Without this, prompt edits are speculative.

**Open Questions**:
- Where does the corpus live? In-repo (public) or external (potentially
  private)?
- Which metrics? Test pass/fail of the patch, spec-clause coverage,
  token cost, turn count, retry rate? Probably a small bundle.
- How is corpus drift handled — when prompts change so much that old
  ground-truth patches no longer represent the target behavior?

---

### Milestone 2: agent-artifact-protocol

**Definition of Done**:
- A unified protocol document defines `agent-artifacts/<patch-id>/<type>.json`
  with a versioned JSON schema per artifact type.
- `lib/artifacts.ml` provides read/write/validate helpers used by the
  orchestrator and referenced from prompt templates.
- The existing PR-body artifact migrates onto this convention. The
  supervisor reads from either the old absolute path or the new
  protocol path during a deprecation window.
- The schema is extensible — adding a new artifact type is a code
  change to `lib/artifacts.ml` plus a new template clause, not a
  protocol redesign.

**Why this is a safe pause point**: Backwards-compatible — old workers
keep writing the old PR-body artifact path; new workers can use either.
No prompt template changes that affect agent behavior beyond the
PR-body artifact migration.

**Unlocks**: Subsequent milestones can introduce new artifacts
(assumptions log, give-up exits) without each one re-litigating
"where does this file go and what does it contain."

**Open Questions**:
- Do we keep the old absolute-path PR-body artifact indefinitely or
  set a deprecation date?
- Should the artifact directory live inside the worktree (visible in
  the diff) or outside it (truly out-of-band)? The current PR-body
  artifact is outside.

---

### Milestone 3: patch-prompt-hardening

**Definition of Done**:
- `render_patch_prompt` gains:
  - A "How to know you're done" section listing mechanical completion
    criteria (build clean, tests clean, every spec clause mapped, every
    acceptance criterion ticked, final commit on branch).
  - A persistence reminder near the top.
  - An explicit "do not edit or remove tests" clause.
  - A direction to verify end-to-end, not just unit-level.
- `render_pr_body_prompt` gains a length budget (~5 bullets / 200 words)
  and a directive to record assumptions made under uncertainty.
- Each edit is scored against the M1 eval harness; any change that
  regresses on the chosen metrics is reverted.
- Inline tests in `lib/prompt.ml` cover the new clauses' presence in
  rendered output.

**Why this is a safe pause point**: Changes are localized to the
default templates. Projects with custom overrides in `prompts/<name>.md`
are unaffected. Eval shows the change is a Pareto improvement or
neutral.

**Unlocks**: The patch prompt is the highest-traffic prompt — locking
in its baseline lets later milestones (per-event prompts, system-prompt
move) build on a stable foundation.

---

### Milestone 4: per-event-prompt-hardening

**Definition of Done**:
- `render_review_prompt`: scope-limit clause ("do not modify code
  outside the scope of these comments") and a structured disagreement
  artifact (uses M2 protocol) for comments the worker actively
  disagrees with.
- `render_ci_failure_unknown_prompt`: starter-checks list (`dune build`,
  `dune runtest`, `gh run view`) to save tokens of exploration.
- `render_ci_failure_prompt`: an unrelated-failure exit that writes a
  structured conclusion to the artifact directory and stops, instead
  of looping on flaky/infra failures.
- `render_merge_conflict_prompt`: prefix-suffix-preserving truncation
  for inlined `git diff`, plus a "give up after N attempts" exit that
  writes unresolved conflicts to the artifact directory.
- `render_pr_body_prompt`: an assumptions-and-blockers section
  populated from the assumptions artifact (M3 + M2).
- Eval baseline maintained or improved across all per-event scenarios
  in the corpus.

**Why this is a safe pause point**: Each prompt change is independent
and individually revertable. Give-up exits are additive — workers that
don't hit them behave as before.

**Unlocks**: With per-event prompts hardened, the only remaining
ambition is structural (system prompt move) and cleanup.

**Open Questions**:
- What's the right N for "give up after N attempts" — 2, 3, 5? Probably
  per-event-type rather than a single number.
- How does the orchestrator surface a give-up exit? Add a dedicated
  patch-agent state, or treat it as a soft failure that pages a human?

---

### Milestone 5: backend-system-prompt-capability

**Definition of Done**:
- The LLM backend interface (`lib/llm_backend.ml` /
  `lib/llm_backend.mli`) accepts an optional `system_prompt : string`
  parameter.
- `claude_backend.ml` threads it through to `--append-system-prompt`.
- `codex_backend.ml` threads it through to the equivalent Codex CLI
  flag (audit needed — may be a system message in the input stream).
- `gemini_backend.ml` and `opencode_backend.ml` accept the parameter
  and inline it into the user message as a fallback. They compile and
  run unchanged at runtime when the parameter is `None`.
- All callsites in `bin/main.ml` pass `None` initially — no behavior
  change at this milestone.
- Tests cover that each backend either uses the system-prompt
  mechanism or correctly falls back.

**Why this is a safe pause point**: Plumbing only. Every callsite
passes `None`; no static text moves yet. The interface change is
backwards-compatible.

**Unlocks**: Milestone 6 can move static text into system prompts
on Claude and Codex without touching backend code.

**Open Questions**:
- Does Codex's CLI actually support an append-system-prompt mode, or
  do we need to inject a system message into the input stream? Audit
  needed.
- Are there any other backends users have wired in via the override
  hooks that we'd be breaking?

---

### Milestone 6: system-prompt-restructure

**Definition of Done**:
- A new `render_patch_system_prompt` renderer in `lib/prompt.ml`
  contains the static, never-changing text from `render_patch_prompt`:
  Pantagruel syntax key, push/PR boundary, persistence reminder,
  completion criteria, forbid-editing-tests clause.
- `render_patch_prompt` (the user-message renderer) is correspondingly
  shortened — only patch-specific body remains.
- `bin/main.ml` passes the system prompt to Claude and Codex backends;
  Gemini and OpenCode continue receiving the full inlined form (the
  fallback path from M5).
- Eval shows: per-turn token cost down on Claude/Codex, prompt-cache
  hit rate up across multi-turn sessions, no quality regression vs
  baseline.
- The same restructure is evaluated for `render_review_prompt`,
  `render_ci_failure_prompt`, and `render_merge_conflict_prompt` — but
  only applied where the static fraction is meaningful.

**Why this is a safe pause point**: Gemini/OpenCode users see no
change. Claude/Codex users see lower token cost without behavior
change. The restructure is invisible to projects with template
overrides because overrides remain in the user-message position.

**Unlocks**: Cache-stable prefixes mean retries and follow-up turns
become significantly cheaper. Future static additions (more boilerplate,
new safety clauses) become free per-turn for the supported backends.

**Open Questions**:
- For users with `prompts/<name>.md` overrides — do we offer a separate
  `prompts/<name>-system.md` override, or keep system-prompt content
  fixed and ask projects to override the user-message portion only?

---

### Milestone 7: trim-and-stabilize

**Definition of Done**:
- `render_patch_prompt` lists only the current patch and its
  declared dependencies in the patches-list section, not the whole
  gameplan.
- Dead or redundant text exposed during M3–M6 is removed.
- The design doc (`patch-agent-prompting.md`) is updated to reflect
  what shipped, with "open questions" pruned to those still genuinely
  open.
- A short migration note documents what changed for users who
  override prompts via `prompts/<name>.md`.
- Eval shows the workstream's cumulative effect against the M1
  baseline: per-session token cost, retry rate, completion rate.

**Why this is a safe pause point**: Terminal milestone. The
workstream is documented and measurable.

**Unlocks**: Future prompt work has a measured baseline, a clean
artifact protocol, and a system-prompt slot to grow into. The
verifier subagent workstream can build on top.

## Dependency Graph

```text
1 (patch-agent-eval-harness)        → []
2 (agent-artifact-protocol)         → []
3 (patch-prompt-hardening)          → [1]
4 (per-event-prompt-hardening)      → [1, 2]
5 (backend-system-prompt-capability)→ [1]
6 (system-prompt-restructure)       → [5]
7 (trim-and-stabilize)              → [3, 4, 6]
```

Milestones 1 and 2 can run in parallel — they touch independent
parts of the codebase. Milestones 3, 4, and 5 can run in parallel
once their prerequisites are met. Milestone 7 is a single-track
cleanup.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| Eval corpus location | In-repo (public) vs private external repo. Determines whether the corpus itself contains anything sensitive. | Milestone 1 |
| Eval metric set | Test pass/fail, spec-clause coverage, token cost, turn count, retry rate — which subset, weighted how? | Milestone 1 |
| Codex append-system-prompt mechanism | CLI flag vs injected system message in the input stream. | Milestone 5 |
| Give-up attempt thresholds | Per-event-type N for "give up after N attempts." | Milestone 4 |
| Override path for system prompts | Whether projects can override system-prompt content separately from user-message content. | Milestone 6 |
| Corpus drift policy | How to refresh ground-truth patches when prompts diverge enough that old outcomes no longer represent the target. | Post-workstream |

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| Eval first, before any prompt edits | Without measurement, every later milestone is speculative. Cost is a slower start; benefit is every change is justified by data. |
| Modular eval harness, Braintrust binding kept out of OSS build | Keeps the OSS distribution clean and credential-free; lets the team use Braintrust internally without forcing it on adopters. |
| Direct default-template edits, not versioned templates | Simplicity. Projects with `prompts/<name>.md` overrides are insulated. Versioning is real work and not yet justified. |
| System-prompt support is a functional requirement for Claude and Codex; Gemini and OpenCode fall back to inlined text | These are the primary backends. Falling back keeps the other backends working without blocking the Claude/Codex token win. |
| Verifier subagent excluded from this workstream | It's an architectural change to the orchestrator (a new worker type with its own lifecycle), not a prompt edit. Deserves its own workstream once this one lands. |
| Unified artifact protocol introduced early (M2) | Several later milestones (assumptions log, give-up exits, structured disagreement) need it. Front-loading it avoids each milestone reinventing a path-and-protocol. |
| Patches-list trim deferred to the cleanup milestone | It's a token-saving change but not behavior-changing. Cheaper to do once with eval coverage than mid-stream. |
