---
name: write-gameplan
description: Create or update a structured JSON gameplan for a codebase change, including patch sequencing, dependency graph, acceptance criteria, and formal per-patch and final-state specs. Use when the user asks for a gameplan, implementation plan, milestone plan, or structured change plan.
---

# Write Gameplan

Create a structured, machine-readable gameplan for a complex codebase change. The gameplan is a JSON object with typed sections, and each patch includes a formal specification articulating the invariants that must hold after that patch is applied. A final-state spec describes what must be true when the entire gameplan is complete.

**Core principle**: It should be 5-10x easier to review a gameplan than the code it produces.

## Specification File (Optional)

The user may provide a path to a **specification file** (typically a `.pant` file or any text file) containing behavioural invariants that apply to the overall gameplan. These are pre-written formal or informal constraints the implementation must satisfy.

**If a spec file is provided:**
1. Read the full contents of the file. If the file does not exist or is not readable, **abort with a clear error message** (e.g., "Spec file not found: <path>") — do not silently fall back to generating specs from first principles
2. Parse out the individual invariants, rules, or propositions (look for named chapters, rules, predicates, or bullet points). If parsing yields no recognizable invariants, **abort with a validation error** listing what was found and why it could not be parsed
3. For each patch, analyze which invariants are relevant to the changes that patch introduces — an invariant is relevant if the patch creates, modifies, or depends on any entity the invariant references
4. Include those relevant invariants in the patch's `spec` field by restating any referenced domains, predicates, or rules inside that patch's spec module, then encoding the invariant as a precondition or postcondition the patch must preserve. Per-patch specs are verified standalone — do not merely cite or import external invariants; all referenced declarations must be self-contained within the patch's spec
5. Include **all** invariants from the spec file in the `finalStateSpec`

**If no spec file is provided**, generate specs from first principles as usual.

## Workstream Context (Optional)

A gameplan can be **standalone** or part of a **workstream** (a larger project spanning multiple gameplans as milestones).

**If the user provides a workstream reference** (URL, file path, or name):
1. Retrieve the workstream definition to understand the broader context
2. Identify which milestone this gameplan corresponds to
3. Review the milestone's "Definition of Done" — this informs your acceptance criteria
4. Ensure your gameplan leaves the codebase in a consistent state
5. Read the workstream's `Established Precedents` section (plus any milestone-scoped precedents). For each precedent, identify the specific patches in this gameplan that consume it — touch its API, implement its algorithm, depend on its invariants — and attach it to those patches' `precedents` arrays. Do **not** blanket-copy workstream precedents onto every patch; only the ones that actually use the technique. See [Leveraging Established Precedents](#leveraging-established-precedents) for the per-patch shape.

**If no workstream is provided**, treat this as a standalone gameplan.

## Output Format

**MANDATORY FIRST STEP**: Before writing any JSON, read `references/gameplan-schema.json` (relative to this skill's directory). It is a formal [JSON Schema (draft 2020-12)](https://json-schema.org/draft/2020-12/schema) defining every required field, its type, constraints, and structure. Do NOT generate JSON from memory — the schema is the sole source of truth for the output shape.

The gameplan is a **JSON object** written to `gameplans/<project-name>.json`. Every section is a named attribute.

### Required Top-Level Fields

All of these fields are **required** and must be present in every gameplan:

| Field | Type | Description |
|-------|------|-------------|
| `projectName` | `string` | Kebab-case, used in branch names and PR titles |
| `specFile` | `string \| null` | Path to the specification file the user provided, if any |
| `workstream` | `object \| null` | `{ name, milestone, priorMilestones, unlocks }` — null if standalone |
| `problemStatement` | `string` | 2-4 sentences: what problem, why it matters |
| `solutionSummary` | `string` | 3-5 sentences: high-level approach |
| `currentStateAnalysis` | `string` | Where the codebase is now vs. where it needs to be |
| `mergabilityStrategy` | `object` | `{ featureFlagStrategy, featureFlags, patchOrderingStrategy }` |
| `requiredChanges` | `array` | `[{ file, line, description, signature }]` |
| `acceptanceCriteria` | `string[]` | Each is a "done" condition |
| `openQuestions` | `string[]` | Decisions for the team (empty array if none) |
| `explicitOpinions` | `array` | `[{ opinion, rationale }]` |
| `patches` | `array` | See Patch Object in schema |
| `testMap` | `array` | `[{ testName, file, stubPatch, implPatch }]` |
| `dependencyGraph` | `array` | `[{ patch, classification, dependsOn }]` |
| `mergabilityChecklist` | `object` | 9 boolean fields (see schema) |
| `mergabilityInsight` | `string` | E.g. "X of Y patches are INFRA/GATED…" |
| `finalStateSpec` | `string` | Formal specification source for the completed gameplan |

### Required Patch Fields

Each patch object must have: `number`, `classification` (INFRA\|GATED\|BEHAVIOR), `complexity` (1\|2\|3), `title`, `files` (array of `{ path, action, description }`), `changes` (string array), `testStubsIntroduced` (string array or null), `testStubsImplemented` (string array or null), `spec` (string). Patches may also include an optional `precedents` array citing established libraries, algorithms, or patterns the patch should adopt — see [Leveraging Established Precedents](#leveraging-established-precedents).

The inline `spec` and `finalStateSpec` string fields in the JSON are the **sole source of truth** for formal specifications. Do not maintain separate spec files alongside the gameplan. For verification, extract the strings and validate them with the spec language's toolchain (see [Specification Language](#specification-language) below). Do not persist the extracted files.

## Formal Specifications

Each gameplan includes two levels of formal specification. The spec language is pluggable (see [Specification Language](#specification-language)), but the structural requirements are fixed:

### Final-State Spec (`finalStateSpec`)

A spec module describing the invariants that must hold when ALL patches have been applied. This is the "acceptance criteria" expressed formally. It should capture:

- Domain entities introduced or modified by the gameplan
- Rules (properties/functions) that the gameplan establishes or changes
- Invariants that the completed system must satisfy
- Initial-state propositions where relevant

### Per-Patch Specs (`spec` on each patch)

Each patch has a spec module describing the invariants that must hold after THAT patch is applied. These are incremental — they describe the delta, not the full system. They should capture:

- New types, rules, or predicates introduced by this patch
- Invariants that become true after this patch (and must remain true for all subsequent patches)
- Preconditions that the patch assumes (from prior patches)

**Spec-writing guidance**: Use progressive disclosure (top-down structure). Never guess domain details — if something is unclear, note it in `openQuestions`.

## Patch Classification

Each patch includes a `classification` field:

- `INFRA` — No observable behavior change. Types, schemas, helpers, test stubs, feature flag additions. Safe to merge anytime.
- `GATED` — New behavior behind a feature flag. Observable behavior unchanged until flag is enabled.
- `BEHAVIOR` — Changes observable behavior. Requires careful review. Should be as small as possible.

**Goal**: Maximize `INFRA` and `GATED` patches. Minimize `BEHAVIOR` patches.

## Patch Complexity

Each patch includes a `complexity` field — an integer in `1`/`2`/`3` estimating how hard the patch is to implement correctly. This is used by orchestrators (e.g. onton's `--model auto`) to route harder patches to stronger models.

- `1` — **Mechanical / shallow / well-precedented.** A rename, a single-call-site signature change, adding a field that already has an obvious default, copy-pasting a pattern that already exists nearby. Could be done by reading only the patch description and a few surrounding lines.
- `2` — **Moderate.** Requires reading the surrounding code to understand context, designing a small abstraction, writing non-trivial tests, or coordinating two or three files. The shape of the solution is clear once you've read the relevant code, but a careless implementation would miss something.
- `3` — **Deep.** Requires reasoning about subtle invariants, concurrency, distributed state, novel algorithms, performance trade-offs, security boundaries, or unfamiliar third-party APIs whose contracts must be researched. Implementations that "look right" can still be wrong.

### Be conservative

**When in doubt, choose the higher value.** Under-estimating complexity costs more than over-estimating it: a too-weak model on a complex patch silently produces broken code, while a too-strong model on a simple patch only costs extra tokens. The bias is intentional.

### Read the code, then research, then decide

Do not assign complexity from the patch title alone. Before scoring:

1. **Read the relevant files** named in `files` and any code the patch touches transitively. If the patch modifies a function with many callers, scan the callers.
2. **If the patch involves a third-party API or unfamiliar library**, fetch the relevant docs (e.g. `WebFetch` against the library's reference) so you know whether the integration is mechanical or has gotchas.
3. **If the patch touches an area the codebase has historical bugs in** (concurrency primitives, migrations, auth, billing, anything safety-critical), bias up.
4. **Score based on the worst case among the changes the patch introduces**, not the median. A patch that mostly renames things but also adds one tricky lock should be scored on the lock.

If after reading you cannot tell whether something is `2` or `3`, it is `3`. If you cannot tell whether something is `1` or `2`, it is `2`.

## Leveraging Established Precedents

Most non-trivial engineering problems have well-known solutions: a CS algorithm with a name, a library that already solves the hard part, a design pattern with documented trade-offs, an RFC that pins down the wire format. **When a patch is solving a problem that has established prior art, identify the precedent and attach it to that patch via the optional `precedents` field.** Prefer proven, robust techniques over rolling our own — and give the implementing agent enough of a reference that it can fetch more detail when it needs to.

### When precedents apply

A patch likely has a precedent worth citing whenever it touches an area with mature, named solutions. Common examples (not exhaustive):

- **Variable binding / scope handling** — capture-avoiding substitution, alpha renaming, free-variable computation (named libraries exist for most languages; locally-nameless and de Bruijn indices are documented techniques).
- **Parsing and lexing** — established parser generators and combinator libraries; standard error-recovery strategies (panic-mode, GLR).
- **Graph algorithms** — Tarjan SCC, Dijkstra/A* shortest path, Kahn topological sort, Union-Find with path compression.
- **Type systems** — Hindley–Milner / Algorithm W; bidirectional type-checking; row polymorphism.
- **Distributed coordination** — Raft, consistent hashing, idempotency keys, the outbox pattern, sagas, vector clocks.
- **Cryptography** — never roll your own. Cite the standard (RFC, NIST suite, IETF draft) and an audited library implementation.
- **Concurrency** — battle-tested constructs (Michael–Scott queues, Treiber stacks, structured concurrency, CSP/actor model).
- **Streaming / backpressure** — Reactive Streams, async iterator protocols, credit-based flow control.
- **Schema and data migration** — expand/contract migrations, accretive change, dual-write/backfill/cutover patterns.
- **State machines / workflow engines** — hierarchical state charts, event sourcing, deterministic replay.

A patch may have **zero** precedents. Bespoke project glue (wiring two existing modules together, renaming a field, adding a config flag) usually does not warrant any citation. Do not manufacture references where none apply.

### What to write

Each precedent entry has four fields:

- **`kind`** — one of `library`, `algorithm`, `pattern`, `paper`, `rfc-spec`, `documentation`, `blog-post`. Pick the most specific kind that fits.
- **`name`** — the most precise identifier a reader will recognise. `"Tarjan 1972 strongly connected components"` is better than `"a graph algorithm"`; `"RFC 7519 JSON Web Tokens"` is better than `"JWT spec"`.
- **`url`** — the canonical link (library homepage or repo, paper DOI or arXiv, RFC URL, official docs page). Required for kinds where a URL is the durable reference (`paper`, `rfc-spec`, `documentation`, `blog-post`); strongly preferred for libraries. Do not guess URLs — fetch the real one (e.g. `WebFetch`) or leave it `null`.
- **`whyApplicable`** — 1-2 sentences explaining what part of *this specific patch* the precedent informs and the concrete shape it imposes. Not generic praise: name the API call, the algorithm step, or the invariant that the precedent supplies. The implementing agent reads this to decide whether to fetch the reference, so be specific.

### Do real research, not name-dropping

- **Do not invent precedents.** A false reference is worse than none — the implementing agent will waste tokens chasing something that doesn't exist or doesn't apply. If you are not confident a precedent applies, omit it.
- **Verify the reference exists before citing.** Fetch the library docs, the paper abstract, or the RFC index when in doubt. Do not cite from memory if the project depends on the precedent being real.
- **Prefer precedents with non-trivial real-world adoption** — libraries used in shipping projects, algorithms cited in production literature. Abandoned or experimental references are weak evidence.
- **Cite at the level of the technique, not the buzzword.** If three libraries implement the same algorithm, the precedent might be the algorithm (kind: `algorithm`) with the recommended library named in `whyApplicable`. Conversely, if the value is the specific library's API design, the precedent is the library.

### Where to attach

Attach precedents at the **patch** level, on the specific patches the precedent informs — typically the patch that introduces a new dependency or implements the named technique, plus any consumers that need to call its API. A precedent that drives the whole gameplan should still be replicated on each patch that depends on it, so an implementing agent picking up Patch N alone has the reference in hand.

## Mergability Strategy

### Feature Flagging

If the gameplan introduces new behavior that should be gated:

**Runtime flag service** (percentage rollout / allowlist):
- Use your project's feature flag infrastructure (e.g., LaunchDarkly, Unleash, a database-backed flag table, or a custom service)
- Declare the flag, gate new behavior behind it, and document the rollout strategy
- Use this for gradual rollouts or per-user gating

**Configuration flag** (global on/off, simple cases only):
- Pattern: an environment variable, config file entry, or build-time constant
- Use only when a runtime flag service is overkill (e.g., dev-only toggles)

Document the flag in the `featureFlagStrategy` field.

### Patch Ordering

Order patches to ship non-functional changes early:

- **Early** (`INFRA`): Types, schemas, helpers, test stubs, migrations
- **Middle** (`GATED`): Business logic behind flags, new gated endpoints
- **Late** (`BEHAVIOR`): Wire up UI/API, enable flags, remove old code

### Database Migration Rules

**Co-location**: Any patch that modifies a database schema must also include generating/writing the corresponding migration in that same patch. Never defer migration generation to a later patch — each patch must leave the DB schema and migration files in sync so it is independently mergeable.

**Sequential execution**: Patches that include database migrations MUST be ordered sequentially in the dependency graph — they cannot be parallelized. Migration files typically have sequential numbering, and parallel branches that each generate migrations will produce numbering conflicts that cause merge failures. This negates the benefit of parallelization.

**Practical guidance**: If a gameplan requires multiple schema changes, either:
1. Bundle them into one patch (if they're related), or
2. Chain the migration-containing patches sequentially in the dependency graph

Reserve parallelization for patches that don't touch database schemas/migrations.

Different projects may use different migration tools and workflows. When planning patches that involve schema changes, research how migrations work in the relevant part of the codebase and include explicit migration instructions (commands to run, files to create/generate) in those patches. The implementing agent will follow the gameplan as-written — do not assume it will discover the migration workflow on its own.

## Test-First Pattern

Write test stubs with skip/pending markers BEFORE implementation:

**Stub patches** (`INFRA`):
- Add tests with the framework's skip/pending mechanism (e.g., `.skip` in JS test runners, `@pytest.mark.skip` in Python, `[@tags "pending"]` in OCaml ppx_inline_test, `#[ignore]` in Rust)
- Include a comment or annotation indicating which patch will implement the test (e.g., `PENDING: Patch N`)
- Document setup and expectations in comments

**Implementation patches** (`GATED` or `BEHAVIOR`):
- Remove the skip/pending marker
- Implement the test body
- Must be in the SAME patch as the code being tested

## Verification

After writing the gameplan JSON, verify:

1. **Schema compliance** — Validate the output against the JSON Schema in `references/gameplan-schema.json`. If `ajv` or another JSON Schema validator is available, run it programmatically. Otherwise, re-read the schema and manually confirm:
   - All `required` fields are present at every level (top-level, patch, featureFlag, etc.)
   - `additionalProperties: false` is enforced — no extra fields anywhere
   - Enum values match exactly (`INFRA`/`GATED`/`BEHAVIOR`, `create`/`modify`/`delete`, etc.)
   - `oneOf` discriminants are correct (feature flag `type`, nullable `workstream`)
   - Patch numbers are positive integers (`minimum: 1`)
   - `projectName` matches the kebab-case `pattern`
   - Do NOT use non-schema field names (e.g. `project` instead of `projectName`, `summary` instead of `solutionSummary`, `id` instead of `number`, `description` instead of `changes`)
2. **Each spec MUST parse cleanly** — extract `spec`/`finalStateSpec` strings to temp files and validate them with the spec language's toolchain (see [Specification Language](#specification-language)). Fix any parse errors before finalizing. If the toolchain is not installed, flag it as a blocker — do NOT skip validation or fall back to manual review
3. The dependency graph is a valid DAG
4. All test stubs have corresponding implementations
5. The mergability checklist passes

## Resolving Open Questions

After the gameplan is written and verified, automatically proceed to this step without waiting for an additional prompt — work through the `openQuestions` array with the programmer **one question at a time** until the array is empty.

Why this step is mandatory:

- **Gating**: onton will refuse to begin executing a gameplan whose `openQuestions` array is non-empty. Leaving questions unresolved blocks the entire plan.
- **Control**: this dialogue ensures the programmer understands and has direct authority over the most consequential and thorny design decisions before any code is written.

For each open question, in order:

1. **Present the question** with the context needed to decide it: which patches it affects, the entities/invariants it touches, and any constraints from the spec file or workstream that bear on it.
2. **Propose 2-4 candidate resolutions** with brief tradeoffs. State your own recommendation and why; do not hide behind false neutrality. If you genuinely have no preference, say so.
3. **Wait for the programmer's decision** before moving on. If they ask for deeper analysis, provide it. If they pick an option you didn't list, accept it. If their resolution conflicts with the spec file or workstream constraints, flag the conflict explicitly and offer to regenerate the gameplan with updated assumptions before proceeding.
4. **Record the resolution** by:
   - Editing the gameplan to reflect the chosen path — update affected patches (`changes`, `spec`, `files`, signatures), `acceptanceCriteria`, `finalStateSpec`, `dependencyGraph`, and any other fields the decision touches.
   - Appending an entry to `explicitOpinions` — an object with non-empty `opinion` (the chosen resolution) and `rationale` (why it was chosen) keys — so the reasoning is preserved in the gameplan itself.
   - Removing the question from `openQuestions`.
5. **Move to the next question.** Do not batch — questions are presented sequentially because later questions often depend on earlier answers, and batching prevents the programmer from reasoning about each decision in isolation.

After the last question is resolved, **re-run verification** (schema validation and spec parsing) since edits made during this dialogue may have introduced regressions.

The end state is a gameplan with `openQuestions: []` and an `explicitOpinions` array that captures every decision made during the dialogue.

## Specification Language

The `spec` and `finalStateSpec` fields contain source code in a formal specification language. The language is a project-level choice — the gameplan structure is the same regardless of which language you use.

### Pantagruel (default)

We use [Pantagruel](https://github.com/subsetpark/pantagruel) — a language for writing formal specifications with domains, rules, and invariants organized into progressive-disclosure chapters.

- **Language reference**: `https://raw.githubusercontent.com/subsetpark/pantagruel/refs/heads/master/REFERENCE.md` — fetch this for syntax details
- **Validation (parse + type-check)**: extract spec strings to `.pant` files and run `pant <file.pant>`. Bare `pant` type-checks; exit code 0 means the spec is well-formed. On pant 0.22, success produces no output. If `pant` is not installed, install via Homebrew: `brew tap subsetpark/pantagruel https://github.com/subsetpark/pantagruel && brew install pantagruel`
- **SMT verification (optional)**: `pant --check <file.pant>` runs SMT-based invariant/precondition checks (requires `z3` or `cvc5`). Use this to catch contradictions, unreachable states, and violated invariants — not a replacement for the parse check. `--bound N` sets the domain-element bound (default 3); `--solver z3|cvc5` picks a solver
- **Module naming**: final-state spec uses `<PROJECT_NAME>` (e.g., `EXTRACT_DECISION`); per-patch specs use `<PROJECT_NAME>_PATCH_<N>`
- **Style**:
  - Progressive disclosure (top-down chapter structure); keep per-patch specs self-contained (redeclare referenced domains/rules rather than importing)
  - **Uppercase** identifiers are domains/types (`Patch`, `Disposition`); **lowercase** identifiers are rules/values (`disposition`, `skip`), optionally suffixed with `?` or `!`
  - Enum-like values are best modelled as nullary lowercase rules returning the domain: `skip => Disposition.` (not `Skip => Disposition.` — uppercase rule names will not parse)
  - Rules with multiple parameters use **comma-separated typed params**, not arrows: `inline-decisions rf: RunnerFiber, p: Patch => Nat0.` (not `rf: RunnerFiber -> Patch => Nat0.`)
  - Every chapter needs at least one declaration in its head before the `---` separator; if a patch introduces no new invariants, redeclare the prior chapter's domains/rules so the module is self-contained

### Using a different spec language

If your project uses a different specification language (TLA+, Alloy, Z, etc.), write specs in that language instead. The structural requirements remain:
- Each patch's `spec` field must be a valid, self-contained spec in the chosen language
- The `finalStateSpec` must cover all invariants from all patches
- Specs must be machine-verifiable (parseable/checkable by the language's toolchain)

## Execution

V2 JSON gameplans are consumed programmatically via the `patches` and `dependencyGraph` arrays. Orchestrators like onton can parse the dependency graph directly to identify parallelizable patches and execute them concurrently in isolated git worktrees.

## Guidelines

- **Be explicit** — easy to execute patch-by-patch by a coding agent with no context window
- **Include function signatures** for new/modified functions
- **Keep it concise** — 10x easier to review than the resulting code
- **Workstream alignment** — if part of a workstream, acceptance criteria must align with milestone's "Definition of Done"
- **Specs are normative** — the formal specs are the source of truth for what "done" means; prose acceptance criteria are a human-readable summary

## References

- `references/gameplan-schema.json` — Formal JSON Schema (draft 2020-12)
- `references/example.json` — Complete real-world example (uses Pantagruel for specs)
