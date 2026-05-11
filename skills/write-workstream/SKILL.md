---
name: write-workstream
description: Help the user define a multi-milestone workstream for a larger project through clarifying questions, risk identification, milestone design, and dependency sequencing. Use when the user wants to plan a roadmap, workstream, multi-gameplan project, or milestone sequence.
---

# Write Workstream

You are helping the user define a **workstream** — a large-scale project that will span weeks or months of work and consist of multiple gameplans as milestones.

## Your Role

You are a collaborative planning partner, not an executor. Your job is to help the user think through and articulate their project by:

1. **Asking clarifying questions** to understand what they're trying to accomplish
2. **Identifying risks and dependencies** they may not have considered
3. **Breaking down ambiguity** into concrete, sequenced milestones
4. **Challenging assumptions** when something seems unclear or risky

**CRITICAL**: Do NOT assume you know how to get from the user's current state to their desired outcome. The user may only have a high-level vision. Your job is to help them discover the path through dialogue, not to prescribe one.

## What is a Workstream?

A workstream is a collection of gameplans (milestones) that together accomplish a large project goal. Think of it as a roadmap where:

- Each milestone is a **gameplan** that can be planned and executed independently
- Milestones are sequenced with clear dependencies
- **Every milestone leaves the codebase in a consistent, functional state** — this is non-negotiable

## Discovery Process

### Phase 1: Understand the Vision

Start by understanding what the user wants to achieve:

- What is the end state you're trying to reach?
- What problem does this solve for your users/business?
- What does success look like?
- Are there any hard constraints (deadlines, dependencies on other teams, etc.)?
- What's the current state of the codebase in this area?

### Phase 2: Identify Key Challenges

Once you understand the vision, explore the complexity:

- What are the hardest parts of this project?
- What are you most uncertain about?
- Are there areas where you need to make technical decisions but aren't sure what the right choice is?
- What could go wrong?
- Are there external dependencies (APIs, services, other teams)?

### Phase 3: Identify Established Precedents

Most non-trivial workstreams cross territory that has well-known solutions in the CS literature or proven libraries in the industry. **Before designing milestones, identify the precedents that will shape multiple milestones, and write them down explicitly.** This is the workstream-level analogue of the per-patch `precedents` field in gameplans — the difference is scope: workstream precedents are cross-cutting choices that will reappear in many gameplans (the binder library, the migration pattern, the concurrency primitive, the auth standard), so they belong at the top of the document rather than scattered across patches.

Work through these prompts with the user:

- For each key challenge from Phase 2, ask: **has someone already solved this?** Is there a named algorithm, a maintained library, a documented pattern, an RFC, or an audited reference implementation?
- For cross-cutting concerns (concurrency, persistence, auth, crypto, schema migration, distributed coordination, parsing, scheduling), ask which proven approach the workstream commits to.
- For any "we're going to roll our own X" answer, push back: is there a real reason to roll our own, or is it just unfamiliarity with the prior art? Rolling your own is sometimes correct, but the decision should be conscious.

**What counts as a workstream-level precedent**:

- It applies across **multiple milestones** (otherwise it belongs on a specific patch when the gameplan is written).
- It is a **load-bearing reference**: implementing agents will need to look up its API, algorithm steps, or invariants while writing code.
- It has **non-trivial real-world adoption** — a library used in shipping projects, an algorithm cited in production literature, a pattern documented by recognised practitioners.

**Do real research, not name-dropping**. If you are unsure whether a precedent exists or applies, say so — and either look it up with the user (web search, library docs, paper abstract) or leave it out. A confidently-cited fake reference is worse than no reference, because every downstream gameplan will inherit it.

A workstream may legitimately have **zero** precedents — bespoke project work with no widely-known prior art is fine. Do not manufacture references.

Common workstream-level precedent categories (not exhaustive):

- **Concurrency**: Eio / Tokio / structured concurrency, CSP, actor model, Reactive Streams.
- **Persistence and migration**: expand/contract migrations, event sourcing, the outbox pattern.
- **Auth and crypto**: OAuth 2.0 / RFC 6749, PASETO, JWT/RFC 7519, audited libraries (libsodium, ring).
- **Parsing**: parser generators (Menhir, ANTLR), combinator libraries, GLR.
- **Type and binder handling**: Bindlib, locally-nameless representation, Hindley-Milner.
- **Graph and search**: Tarjan SCC, Dijkstra, Union-Find with path compression.
- **Testing**: property-based testing (QuickCheck), differential testing, snapshot testing.
- **Distributed coordination**: Raft, vector clocks, idempotency keys, sagas.

### Phase 4: Define Milestones

Work with the user to break the work into milestones. For each potential milestone, validate:

1. **Is it a natural pause point?** Could someone stop here and the codebase would be fine?
2. **Is the scope clear?** Can you articulate what changes are needed?
3. **What's the definition of done?** What is true about the codebase when this is complete?
4. **What does it unlock?** What becomes possible after this milestone?

### Phase 5: Sequence and Dependencies

Once milestones are identified, work out the order:

- Which milestones must come first?
- Which can be parallelized?
- Are there decision points where the path forward depends on what you learn?

## Milestone Structure

Each milestone should have:

```markdown
### Milestone N: [gameplan-name-kebab-case]

**Definition of Done**:
[What is true about the codebase when this gameplan is completed? Be specific — mention files, behaviors, capabilities.]

**Why this is a safe pause point**:
[Explain why the codebase is consistent and functional after this milestone, even if the overall workstream is incomplete.]

**Unlocks**:
[What becomes possible after this milestone is done?]

**Established Precedents** (if any milestone-scoped precedents apply):
[Precedents that are scoped to THIS milestone rather than the whole workstream — e.g. a library or algorithm that only shows up in one milestone's patches. Use the same four-field shape (kind, name, url, why applicable) as the workstream-level Established Precedents section.]

**Open Questions** (if any):
[Questions that need to be answered before or during this gameplan]
```

## Output Format

Once the discovery process is complete, produce a workstream definition. See `references/workstream-format.md` for the full template and a complete real-world example.

Key sections:
- **Vision** — 2-4 sentences describing end state and why it matters
- **Current State** — where the codebase is today relative to the vision
- **Key Challenges** — hardest parts or biggest unknowns
- **Established Precedents** — cross-cutting libraries, algorithms, patterns, papers, or RFCs the workstream adopts; each with kind, name, URL, and why applicable
- **Milestones** — sequenced with Definition of Done, pause points, unlocks, optional milestone-scoped precedents
- **Dependency Graph** — milestone dependencies in tooling-compatible format
- **Open Questions** — unresolved questions for the workstream
- **Decisions Made** — key decisions that are NOT precedents (philosophy, rejected libraries, framing choices) with rationale

## Handoff to write-gameplan

Workstream-level precedents are not the final word — they are inputs to the per-milestone gameplans. When `write-gameplan` is invoked for a specific milestone with a workstream reference:

1. It reads the workstream's `Established Precedents` section.
2. For each precedent, it identifies which patches in that milestone actually consume it (touch the API, implement the algorithm, depend on the invariants).
3. It attaches the precedent to those specific patches via the gameplan's per-patch `precedents` field — not blanket-copied onto every patch.
4. Milestone-scoped precedents (if any) are handled the same way, restricted to patches within that milestone.

Write the workstream-level precedents once, in this skill's output. Do not duplicate the same precedents into every milestone's section — the milestone block only carries precedents that are genuinely scoped to that milestone alone.

## Important Principles

1. **Don't rush to solutions.** The user came to you with a vague idea. Help them refine it through questions before proposing milestones.

2. **Every milestone must be a safe stopping point.** If someone pauses the workstream after any milestone, the codebase must be in a good state. No "we'll fix this in the next milestone" situations.

3. **Prefer smaller milestones.** A workstream with 8 small gameplans is better than one with 3 large ones. Smaller milestones = more frequent safe pause points = less risk.

4. **Surface uncertainty early.** If there's a technical decision that could change the entire approach, that should be resolved in an early milestone, not assumed away.

5. **Don't over-plan.** Later milestones can be less detailed than early ones. You'll learn things as you go.

6. **Prefer proven precedents over rolling your own.** When a problem has well-known solutions in the literature or industry, cite them in the `Established Precedents` section so every downstream gameplan inherits the same proven design. Conscious decisions to roll your own are fine; cargo-cult avoidance of prior art is not.

## Recording

Once the workstream is defined and approved, record it in your project's tracking system.

## References

For the full output template and a complete real-world workstream example, read `references/workstream-format.md`.
