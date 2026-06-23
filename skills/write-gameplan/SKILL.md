---
name: write-gameplan
description: Create or update a structured JSON gameplan for a codebase change, including patch sequencing, dependency graph, acceptance criteria, and formal per-patch and final-state specs. Use when the user asks for a gameplan, implementation plan, milestone plan, or structured change plan.
---

# Write Gameplan

Create a structured, machine-readable gameplan for a complex codebase change. The gameplan is a JSON object with typed sections, and each patch includes a formal specification articulating the invariants that must hold after that patch is applied. A final-state spec describes what must be true when the entire gameplan is complete.

**Core principle**: It should be 5-10x easier to review a gameplan than the code it produces.

## Atomicity Constraint (Read This First)

A gameplan is, by definition, a bundle of work with two non-negotiable properties:

1. **Atomic.** Either every patch lands or the gameplan is reverted as a unit. There are no partial outcomes the team is supposed to evaluate and then decide whether to continue.
2. **Autonomously parallelizable.** Once the gameplan is approved, an orchestrator (or a swarm of agents) can execute the patches concurrently, respecting only the `dependencyGraph`. No human is in the loop between patches.

The following structures are therefore **prohibited inside a single gameplan**:

- **Conditional patches.** A patch that "only happens if the previous patch's behavior looks right in staging" is not a patch — it's a separate milestone. Every patch in a gameplan is committed to before execution begins.
- **Inter-patch human operations.** Anything that asks an operator to flip a flag, run a one-time script, query a dashboard, copy a value out of one system into another, or otherwise *act* between patches. The only human action a gameplan permits is the eventual review/merge of the patches themselves.
- **Inter-patch observation windows.** "Let the new metric soak for 24 hours after Patch 3 before applying Patch 4" is a milestone boundary, not a patch boundary. So is "wait for the next billing cycle," "wait for the canary to bake," or "wait until next Monday."
- **Inter-patch decisions based on outcomes.** "If the query plan looks fine after Patch 2, do A; otherwise do B" cannot live inside one gameplan — neither branch can be planned formally, and the dependency graph cannot encode the conditional.

If you find yourself drafting *any* of those — **stop and use [[write-workstream]] instead**. The work you are describing is a multi-milestone workstream, not a gameplan. Each side of the pause/decision/observation becomes its own milestone with:

- a clear Definition of Done,
- explicit instructions for the human operator(s) to follow between milestones (the flag flip, the dashboard check, the manual backfill, the decision criteria),
- and a separate gameplan generated per milestone once the prior milestone is complete.

It is **expected and healthy** that real projects contain soaking, observation, flag flips, and judgement calls. The error is not that those things exist; the error is trying to encode them inside a gameplan. They live at milestone boundaries.

**Practical test, applied while drafting**: read your patch sequence and ask, "could a fully autonomous orchestrator execute every one of these back-to-back, with no human pause and no run-time choice, and would the outcome still be correct?" If the answer is no — for any reason — you are looking at a workstream, not a gameplan. Stop drafting and switch to `/write-workstream`.

## One Repo Per Gameplan

Every gameplan pertains to **exactly one repository on a git forge** (GitHub, GitLab, Gitea, etc.), declared by the required top-level `owner` and `repo` fields. The orchestrator (e.g. onton) clones that repo and runs every patch's agent inside isolated worktrees off of it — so paths in `patches[].files[].path` and `requiredChanges[].file` are resolved **relative to the repo root** and may not escape it (e.g. `../other-repo/x.ts` is disallowed; the worktree has no notion of "next to my checkout").

`owner` and `repo` are forge-agnostic at this layer — the schema only checks that they are non-empty strings. The forge backend the orchestrator is configured for will enforce any format rules it requires (for example, GitHub rejects handles longer than 39 characters); that validation happens at session start, not at gameplan-authoring time.

**If the change spans multiple repositories**, write **multiple gameplans** — one per repo — each with a distinct `projectName` (e.g. `auth-shared` and `auth-app`) and the appropriate `owner`/`repo`. If the gameplans coordinate (one must merge before another can be implemented), express that coupling at the workstream level: name each gameplan as a separate milestone in the workstream and use `priorMilestones` / `unlocks` to capture the ordering. Never bundle cross-repo work into a single gameplan and never use repo-relative `../sibling-repo/...` paths.

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
4. Read the workstream's terminal **Definition of Done (Acceptance Suite)** and pull out every assertion whose `Traces to` names **this** milestone as owner. These are the concrete, observable obligations this gameplan must make true; they should map onto your `acceptanceCriteria` and `finalStateSpec`. You will sharpen them with real artifact names during the [write-back](#writing-back-to-the-parent-workstream).
5. Ensure your gameplan leaves the codebase in a consistent state
6. Read the workstream's `Established Precedents` section (plus any milestone-scoped precedents). For each precedent, identify the specific patches in this gameplan that consume it — touch its API, implement its algorithm, depend on its invariants — and attach it to those patches' `precedents` arrays. Do **not** blanket-copy workstream precedents onto every patch; only the ones that actually use the technique. See [Leveraging Established Precedents](#leveraging-established-precedents) for the per-patch shape.

**If no workstream is provided**, treat this as a standalone gameplan.

## Output Format

**MANDATORY FIRST STEP**: Before writing any JSON, read `references/gameplan-schema.json` (relative to this skill's directory). It is a formal [JSON Schema (draft 2020-12)](https://json-schema.org/draft/2020-12/schema) defining every required field, its type, constraints, and structure. Do NOT generate JSON from memory — the schema is the sole source of truth for the output shape.

The gameplan is a **JSON object** written to `gameplans/<project-name>.json`. Every section is a named attribute.

### Required Top-Level Fields

All of these fields are **required** and must be present in every gameplan:

| Field | Type | Description |
|-------|------|-------------|
| `projectName` | `string` | Kebab-case, used in branch names and PR titles |
| `owner` | `string` | Repository owner on the git forge (user, org, group). Non-empty; forge-specific format rules are enforced by the orchestrator at session start. See [One Repo Per Gameplan](#one-repo-per-gameplan) |
| `repo` | `string` | Repository name on the git forge (paired with `owner`). All file paths in this gameplan are interpreted relative to this repo's root |
| `specFile` | `string \| null` | Path to the specification file the user provided, if any |
| `workstream` | `object \| null` | `{ name, milestone, priorMilestones, unlocks }` — null if standalone |
| `problemStatement` | `string` | 2-4 sentences: what problem, why it matters |
| `solutionSummary` | `string` | 3-5 sentences: high-level approach |
| `currentStateAnalysis` | `string` | Where the codebase is now vs. where it needs to be |
| `operationalConsiderations` | `object` | `{ externalSystemAccess, crossRuntimeContracts, failureBehavior, concurrencyAndIdempotency, rollbackStrategy }` — see [Operational Considerations](#operational-considerations) |
| `mergabilityStrategy` | `object` | `{ featureFlagStrategy, featureFlags, patchOrderingStrategy }` |
| `requiredChanges` | `array` | `[{ file, line, description, signature }]` |
| `functionalChanges` | `array` | `[{ id, description, ownedBy }]` — exhaustive, every entry assigned to exactly one patch. See [Functional Change Ownership](#functional-change-ownership). |
| `contextResources` | `array` | `[{ id, kind, paths, why, consumedBy }]` — authoritative context specific patches must read before editing. See [Context Resources](#context-resources). |
| `acceptanceCriteria` | `string[]` | Each is a "done" condition |
| `openQuestions` | `string[]` | Decisions for the team (empty array if none) |
| `explicitOpinions` | `array` | `[{ opinion, rationale }]` |
| `patches` | `array` | See Patch Object in schema |
| `testMap` | `array` | `[{ testName, file, stubPatch, implPatch }]` |
| `dependencyGraph` | `array` | `[{ patch, classification, dependsOn }]` |
| `mergabilityChecklist` | `object` | boolean fields including `gameplanIsAtomicAndAutonomous` (see schema for full list) |
| `mergabilityInsight` | `string` | E.g. "X of Y patches are INFRA/GATED…" |
| `finalStateSpec` | `string` | Formal specification source for the completed gameplan |

### Required Patch Fields

Each patch object must have: `number`, `classification` (INFRA\|GATED\|BEHAVIOR), `complexity` (1\|2\|3), `title`, `files` (array of `{ path, action, description }`), `changes` (string array), `requiredContext` (string array), `testStubsIntroduced` (string array or null), `testStubsImplemented` (string array or null), `spec` (string). Patches may also include an optional `precedents` array citing established libraries, algorithms, or patterns the patch should adopt — see [Leveraging Established Precedents](#leveraging-established-precedents).

The inline `spec` and `finalStateSpec` string fields in the JSON are the **sole source of truth** for formal specifications. Do not maintain separate spec files alongside the gameplan. For verification, extract the strings and validate them with the spec language's toolchain (see [Specification Language](#specification-language) below). Do not persist the extracted files.

## Ground Every Reference in Real Code

The gameplanning agent runs **inside a checkout of the target repo** (see [One Repo Per Gameplan](#one-repo-per-gameplan)), so it can open any repo file and inspect any installed or vendored dependency. The two most common mechanical defects in executed gameplans — **a named path that does not exist** and **a symbol referenced under the wrong name** — are eliminable at authoring time by reading the workspace. Read it before naming anything.

### Rule 1 — Ground every reference you can resolve

Before you write a file path, module, function, type, field, constant, enum value, command name, or signature into *any* field — `requiredChanges[].file` / `.signature`, `patches[].files[].path`, `contextResources[].paths`, a `changes` step, or a `spec` — **resolve it against the actual code**:

- **Repo source.** Open the file; confirm the path exists. For a file the gameplan *creates*, confirm its parent directory and sibling naming convention are real. Confirm every symbol is spelled exactly as it appears in the code — the real export name, the real enum/constant value (not a display string or a paraphrase), and the real test-file convention (`foo.test.ts` vs `foo.unit.test.ts` is a recurring miss).
- **Inspectable dependencies.** If a patch calls into a third-party library that is installed or vendored in the checkout (`node_modules`, vendored modules, type stubs, generated clients), read its actual declarations before specifying the call shape. Do not reconstruct an API from memory when the real types are on disk.
- **Signatures.** When you give a `signature` for a new or modified function, make it consistent with the real types it must accept and return — look those types up; do not invent them.

If you assert a path or symbol you did not verify, you are guessing, and the patch agent inherits the guess with no way to know it was one.

### Rule 2 — Mark, don't invent, what you cannot ground

Some references are genuinely *not* resolvable from the workspace, and those must not be silently invented either:

- A file the gameplan will create does not exist yet — name it and mark it `action: create`. That is grounding the *convention*, not asserting the file is present.
- A fact that lives in an **external system** the agent cannot inspect — whether a live SaaS integration actually exposes a particular API/tool, the shape of a third-party webhook, a value held only in a dashboard or secret store — is not a thing to guess into a `spec`. Route it to `openQuestions` or the relevant `operationalConsiderations` sub-field (e.g. `externalSystemAccess`) so it is resolved deliberately.

The dividing line is precisely **inspectability from the gameplanning state**: ground what the checkout can answer; surface what it cannot. Do not let an un-inspectable external fact masquerade as a grounded one.

### Rule 3 — Give multi-patch surfaces one shared anchor

When more than one patch touches the same file or symbol — patch 1 introduces a type that patches 3 and 5 consume, two patches edit the same registry, a stub patch and its implementation patch share a test file — **name that file/symbol with one concrete, identical reference everywhere it appears.** Choose the exact path and exported identifier once, then reuse it verbatim across every patch's `files`, `changes`, `requiredChanges`, and `contextResources`. Prefer routing the shared surface through a `contextResources` entry whose `consumedBy` lists every patch that depends on it, so they all read the same authoritative description. The failure this prevents: two patch agents, working concurrently in isolated worktrees with no view of each other, each inventing a slightly different name for the same thing — and the pieces failing to fit together at merge.

## Context Resources

`contextResources` names authoritative context that an implementing patch agent must read before editing. This is for existing code, contracts, docs, tests, or predecessor surfaces that constrain implementation. It is not a dumping ground for general background.

Allowed `kind` values:

- `existing-implementation` — current code path or helper whose behavior should be reused or preserved.
- `contract` — API, protocol, schema, interface, spec clause, or cross-runtime contract the patch must honor.
- `predecessor` — old surface being retired, replaced, or migrated away from.
- `reference-doc` — repo documentation or canonical maintainer docs describing the intended behavior.
- `test-or-static-check` — tests, fixtures, evals, linters, or static checks that define expected behavior.
- `external-reference` — external URL, standard, or vendor document that is authoritative for this patch.

Each resource has `id`, `kind`, `paths`, `why`, and `consumedBy`. Each patch has `requiredContext`, an array of resource IDs. The routing must match in both directions: if `contextResources[].consumedBy` includes patch `3`, then patch `3` must include that resource ID in `requiredContext`, and vice versa. Attach resources only to patches that actually consume them.

### When context is required

Require context resources for patches that:

- write docs, evals, test harnesses, adapters, replacement implementations, or policy logic;
- retire, replace, or preserve behavior from an old surface;
- describe an implementation or contract in documentation;
- implement one side of a cross-runtime/API/schema contract;
- depend on an existing test/static check as the source of truth.

Docs, evals, and reference patches must name the implementation or contract they describe. Adapter/replacement patches must name the predecessor and the target contract. If a context resource defines a contract that a patch preserves, the relevant per-patch `spec` should cite that contract in its invariants.

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

Every functional change should map to at least one per-patch or final-state spec clause when the chosen spec language can express it. If a context resource defines a contract the patch preserves, name that contract in the spec's invariants so implementers and reviewers can trace the resource to the code obligation.

#### Complete the contract in the spec

The per-patch `spec` is the **sole source of truth for the behavioral contract** — sharpen it in the spec itself; do not restate the contract in `changes` or elsewhere. After wrong references (see [Ground Every Reference in Real Code](#ground-every-reference-in-real-code)), the largest class of executed-gameplan defects is a contract that named the right things but left a case unspecified: an unhandled error, the inverse of a specified operation, an undefined boundary. These are observable behavior at the interface — what preconditions, postconditions, and invariants exist to pin down — not implementation detail. Specify that behavior; leave the mechanism that satisfies it to the implementer.

A spec is complete when, for every rule it introduces or changes, the contract is **total over that rule's input domain**. In Pantagruel:

- **Fallible results are sum types with every arm covered.** If a grounded function can fail, model the result as a sum (`Outcome = Ok + RateLimited + Invalid.`) and constrain the rule into the whole sum — never spec only the success arm. The failure arms must match the real error union you grounded in the code, not a guessed subset.
- **Case analysis is exhaustive.** Use `cond … , true => …` so the final arm closes coverage; `pant --check` flags a `cond` whose arms miss inputs. Every variant of a grounded enum/sum is handled or explicitly excluded.
- **Partiality is a written precondition, not an omission.** A rule with no guard asserts totality (`owner d: Document => User.`); if a rule is partial, the guard *is* the precondition (`f x: T, valid? x => …`) — write it so "what must hold of the input" is on the page. A missing guard is a claim of totality; mean it.
- **Inverse and sibling operations are specified together.** Spec `create` ⇒ say what `update`/`delete` do (or that they are structurally rejected); spec `add` ⇒ `remove`. Declaring one member of an operation family and leaving the rest to the implementer is the single most common omission.
- **Invariants quantify over the whole domain.** A property that must hold at several sites (every place a secret is logged, every consumer of a changed row) is `all x: Site | …`, not an assertion about one representative — the universal *is* the claim that no site is unhandled.

Ground each of these against the code you already opened — the enum's real members, the function's real error union, the actual callers — so the completeness check has an **external oracle** (the grounded types plus `pant --check`'s exhaustiveness and contradiction analysis), not just re-reading. A case you cannot resolve from the workspace goes to `openQuestions` (Rule 2 above); never close it by guessing it into the contract.

## Patch Classification

Each patch includes a `classification` field:

- `INFRA` — No observable behavior change. Types, schemas, helpers, test stubs, feature flag additions. Safe to merge anytime.
- `GATED` — New behavior behind a feature flag. Observable behavior unchanged until flag is enabled.
- `BEHAVIOR` — Changes observable behavior. Requires careful review. Should be as small as possible.

**Goal**: Maximize `INFRA` and `GATED` patches. Minimize `BEHAVIOR` patches.

## Functional Change Ownership

A gameplan whose patches share responsibility for a behavioral change vaguely fails in a predictable way: each patch implementer reads the prose narrative, sees the change mentioned, and assumes a different patch owns it. The change falls through the cracks. The whole point of decomposing a gameplan into mergeable patches is defeated when a behavior is described as a gameplan-level concept that has no single named owner.

The `functionalChanges` array prevents this. It is an **exhaustive enumeration** of every functional or behavioural delta the gameplan introduces, with each entry assigned to exactly one owning patch.

### What goes here

- Every observable behavior the system gains, loses, or changes as a result of this gameplan.
- Every user-visible or API-visible change (new endpoint shape, new return value, new error path, removed deprecation).
- Every change in protocol, contract, or invariant that downstream code can detect.

What does **not** go here:

- Pure refactors that have no observable effect (those still belong in `patches[].changes` as implementation steps).
- File or signature edits (those belong in `requiredChanges`).
- Internal helper introductions that are not callable from outside the module being changed.

### The mapping

Each `functionalChange` has `id` (`FC-1`, `FC-2`, …), a single-outcome `description`, and an `ownedBy` patch id. The mapping is:

- **Total**: every functional change has an owner. No orphans.
- **Single-valued**: exactly one patch owns each change. No shared ownership; co-owning a change is the failure mode this section is designed to prevent.
- **Not strictly surjective**: an INFRA-only patch that introduces types or test stubs need not own any functional change. Most observable changes land on GATED or BEHAVIOR patches.

If the same behavior is co-implemented by two patches, the change description is too coarse — split it into two changes (one per patch), each describing the slice that patch delivers.

### How it surfaces to the patch agent

Downstream consumers (notably onton's patch prompt renderer) read `functionalChanges` and inject the subset `ownedBy` each patch into that patch's agent prompt as an explicit "Functional Changes You Own" section. The implementing agent therefore sees the precise list of user-visible behaviors it is responsible for delivering, separate from its `changes` implementation steps. This is what closes the loophole — there is no longer prose-only behavior that no patch acknowledges.

### Authoring guidance

- Write each entry as the **outcome**, not the mechanism. "Merged patches are skipped instead of queued" is correct; "Add a merged-check branch to disposition" is an implementation step and belongs in `patches[].changes`.
- Cross-check against `problemStatement`, `solutionSummary`, and `acceptanceCriteria`: every behavioral promise made there must correspond to at least one `functionalChange` entry. If you cannot point at the owning patch for a sentence in the problem statement, the gameplan has a gap.
- Cross-check against `finalStateSpec`: every behavioral invariant in the spec should map to a functional change that introduces it (the spec says *what is true at the end*; the functional change says *which patch made it true*).

## Patch Boundaries (Frames and No-Ops)

[Functional Change Ownership](#functional-change-ownership) makes the *behavior* partition correct — every observable change has exactly one owning patch (total and disjoint). The same discipline must hold for the *file* partition, and each patch must make a real change. Two recurring defects come from skipping this: a patch whose change spills into files it never listed, and a patch whose change was already true (a no-op). Both are detectable at authoring time against the grounded code.

**A patch's `files` array is its frame condition.** In contract terms a routine has not only pre/postconditions but a *frame* — the exclusive set of locations it may write (this is JML's `assignable`/`modifies` clause; separation logic calls the touched region the *footprint*). The `files` list is exactly that: the patch's complete and exclusive write-set. Validate it as one:

- **Complete** — walking the patch's `changes` and `spec` against the grounded code, every file that must be edited to deliver the change is in `files`. If delivering the functional change forces an edit to a consumer, a registry, a barrel export, or a type the patch didn't list, the frame is incomplete — add the file or rescope the patch. The "consumers" axis of [Complete the contract in the spec](#complete-the-contract-in-the-spec) feeds this: every consumer you must update is part of the frame.
- **Exclusive / disjoint** — no two patches that can run concurrently (no dependency edge between them) may write the same file or symbol. Overlapping frames are the merge collision the isolated-worktree execution model cannot reconcile. If two patches must touch one surface, either serialize them with a `dependencyGraph` edge or route the shared surface through one owning patch (cf. [Rule 3 — shared anchor](#ground-every-reference-in-real-code)).

**Each patch must be non-vacuous.** A patch whose postcondition already holds in the grounded pre-state is a no-op — satisfied *vacuously*, the way "every request is followed by a grant" holds in a system that makes no requests. Mechanical test: remove the patch and check whether its postcondition still holds against the grounded code; if it does, the patch is empty. If the field already exists, the route is already registered, or the type already has the variant, drop the patch or rescope it to the work actually missing.

**Decompose by what changes together, not by execution flow.** Parnas's module criterion applies to patches: partitioning by flow ("first do A, then B, then C") tends to produce patches with overlapping frames and vague ownership, because one surface gets touched at several flow steps. Partitioning by *what changes together* — a type with its consumers, a registry with its entries — yields disjoint frames and clean single ownership, which is what makes concurrent worktree execution safe.

## Operational Considerations

Beyond *what changes*, a gameplan must engage with *how the system behaves operationally* under the change. These concerns share a failure shape: vague descriptions get scattered across patches, every patch author assumes some other patch owns the decision, and the question surfaces in production. The `operationalConsiderations` schema field is required and contains five sub-fields — each is a required string. The schema enforces presence; the rubric below makes each response substantive. A sub-field may state "not applicable" with a brief justification when the gameplan genuinely does not touch that surface, but it must be present and engage with this gameplan's actual code.

### `externalSystemAccess`

When a gameplan newly depends on an external system (object storage, database the runtime doesn't currently reach, third-party API, queue, secret store, internal service), the runtime executing the new code must be able to reach it in production. Audit the existing access posture of that runtime (direct SDK with ambient IAM, presigned URL handed in by another service, broker proxy, VPC endpoint, etc.), pick an access mode, and assign one patch to own the wiring — IAM grant, new endpoint, presigned-URL minting path, network policy, secret rotation. Be especially suspicious of newly-invented `*Client` / `*Transport` interfaces — they are where an undecided access-mode question hides. Sentinel classes whose existence encodes a *lack* of access (e.g. a `PresignedOnly*` adapter) are signals that the runtime cannot hold the underlying credentials. The chosen capability should appear as a `functionalChange` owned by that patch, not just as an interface parameter.

### `crossRuntimeContracts`

Any data format crossing a runtime boundary (queue payloads, DB rows read or written by separate services, S3 object schemas, RPC return types, event payloads) is a contract. When the gameplan changes one side, identify the producer and consumer runtimes, name which side this gameplan touches, and explain how the other side stays in sync — patched in the same gameplan, or made forward/backward-compatible with an explicit migration plan. Failure mode: producer ships, consumer breaks silently, and the gap is not visible from any single patch's `files` array.

### `failureBehavior`

For each new dependency or runtime path, describe behavior under realistic failure: dependency slow/throttling/5xx/garbage, retry storms, partial writes, timeouts, oversized inputs, expired credentials. State which failures are handled deliberately (documented recovery path or error surface) and which are intentionally left to the caller or operator. Failure mode: only the happy path is tested against a fake, and production discovers the rest.

### `concurrencyAndIdempotency`

Any new code path entered concurrently or under retry (queue workers, scheduled jobs, race-able write paths, parallel access to the same DB row or S3 key) has a concurrency contract: locking, idempotency keys, ordering guarantees, deduplication. State it explicitly. Failure mode: a quiet double-write or deadlock that only manifests under production load.

### `rollbackStrategy`

For stateful changes (DB schema, data writes, materialized artifacts, mutations to external systems), describe the rollback story: if a patch must be reverted after data is written, what's the recovery path? Expand/contract migrations, backfill plans, opt-in flags that wind down gracefully, compensating writes. Failure mode: assuming forward-only and stranding data in a half-migrated state.

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

When a gameplan introduces new behavior, there are **three** possible gating strategies — and which one is right is a decision for the programmer, not an assumption for you to make:

1. **Create a new feature flag** and gate the new behavior behind it.
2. **Gate behind an existing feature flag** the project already defines.
3. **Don't gate at all** — ship the behavior directly (appropriate for pure refactors with no observable change, low-risk additions, or work that is itself enabling a flag that already exists).

**Do not default to "create a new flag."** Creating a new flag is one option among three, not the presumed answer. Reflexively minting a new flag for every gameplan litters the codebase with dead toggles, fragments rollouts that should share a flag, and gates changes that never needed gating.

**Before deciding, investigate and clarify:**

- **Search the codebase for existing flag infrastructure and flags.** Find how this project gates behavior (a flag service, a database-backed flag table, environment variables, build constants) and enumerate the flags that already exist. A new feature frequently belongs under an existing flag that already gates the surrounding surface or an in-progress rollout.
- **Decide whether gating is warranted at all.** If the change is a pure refactor with no observable behavior change, or is otherwise safe to ship unconditionally, option 3 is correct — record *why* no flag is needed.
- **If the user has not already specified which strategy to use and the answer is not unambiguous from the codebase, do not guess — surface it.** Add an entry to `openQuestions` (e.g. "Gate behind a new flag, reuse existing flag `X`, or ship ungated?") so it is resolved with the programmer during [Resolving Open Questions](#resolving-open-questions), presenting the three options with the tradeoffs you found. Only treat the decision as settled when the user has stated it or the codebase makes it unambiguous.

Once the strategy is decided, implement the mechanics:

**Runtime flag service** (percentage rollout / allowlist):
- Use your project's feature flag infrastructure (e.g., LaunchDarkly, Unleash, a database-backed flag table, or a custom service)
- Declare the flag (or reference the existing one), gate new behavior behind it, and document the rollout strategy
- Use this for gradual rollouts or per-user gating

**Configuration flag** (global on/off, simple cases only):
- Pattern: an environment variable, config file entry, or build-time constant
- Use only when a runtime flag service is overkill (e.g., dev-only toggles)

Record the chosen strategy in the `featureFlagStrategy` field — naming whether the flag is new or reused, or stating plainly why no flag is needed — and populate `featureFlags` accordingly (an empty array when ungated).

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

Run the validator before finalising:

```
python3 scripts/validate.py <path/to/gameplan.json>
```

It exits 0 on PASS and 1 with explicit error lines on FAIL. Fix every reported error; do not ship a gameplan that has validator failures or WARNs.

Soft dependencies — install both so nothing is skipped:
- `jsonschema` (pip) — enables JSON Schema shape validation. Without it, only semantic checks run.
- `pant` 0.22+ — enables Pantagruel spec parsing. Install: `brew tap subsetpark/pantagruel https://github.com/subsetpark/pantagruel && brew install pantagruel`.

The validator covers everything mechanisable: schema shape, spec parsing, context-routing reciprocity, functional-change ID and ownership integrity, dependency-graph DAG correctness and classification consistency, testMap consistency, and repo-relative path safety. See `scripts/validate.py` for the exact set.

The rest is human judgement. Walk these before setting the relevant `mergabilityChecklist` booleans to `true`:

1. **Functional-change coverage** (`functionalChangesOwnedByExactlyOnePatch`) — the validator confirms each FC has a single resolving `ownedBy`. It cannot confirm the *set* of FCs covers every observable change. Re-read `problemStatement`, `solutionSummary`, `acceptanceCriteria`, and `finalStateSpec`; every behavioural promise there must map to an FC.

2. **Context-resource fit** — the validator confirms routing is bidirectional and references resolve. Manually confirm that docs/evals/reference patches actually name the implementation or contract they describe — a resource attached to a patch that never reads it is dead weight.

3. **Spec/evidence completeness** — for each functional change, confirm a spec or acceptance criterion expresses it, a patch implements it, the required context (if any) is attached, and a test/static check is expected to prove it. Then confirm each rule a patch's spec introduces or changes is **total over its input domain** — see [Complete the contract in the spec](#complete-the-contract-in-the-spec). Unresolvable cases belong in `openQuestions`, not guessed into the contract.

4. **Operational considerations are substantive** (`operationalConsiderationsSubstantive`) — every `operationalConsiderations` sub-field is required by the schema, but presence ≠ engagement. Confirm each sub-field names concrete surfaces in *this* gameplan (specific runtimes, formats, code paths, stateful writes) rather than platitudes. "Not applicable" only with a brief gameplan-specific justification. See [Operational Considerations](#operational-considerations).

5. **Atomicity** (`gameplanIsAtomicAndAutonomous`) — re-read [Atomicity Constraint](#atomicity-constraint-read-this-first) and walk the patch list. Confirm no patch is conditional on another's outcome, no `changes` step expects a human between patches (flag flip, manual script, dashboard check, decision branch), and no patch implies an observation/soak window. If any slipped in, re-decompose as a multi-milestone workstream via [[write-workstream]].

6. **Reference grounding** — for every file path and symbol the gameplan names (`requiredChanges`, `files`, `signature`s, `contextResources[].paths`, and symbol references inside `changes`/`spec` prose), confirm it resolves against the real workspace per [Ground Every Reference in Real Code](#ground-every-reference-in-real-code): existing paths open, created paths follow a real sibling convention, symbols match the actual exports/enums/test-file names, and dependency call shapes match on-disk declarations. Confirm any fact you *couldn't* ground (external-system behavior) lives in `openQuestions`/`operationalConsiderations` rather than asserted in a spec. Confirm every surface touched by more than one patch is named with one identical reference across those patches.

7. **Patch boundaries** — for each patch, confirm its `files` frame is **complete** (delivers the functional change with no edits spilling into unlisted files) and **exclusive** (no patch that can run concurrently writes the same file/symbol), and that the patch is **non-vacuous** (its postcondition is not already true of the grounded current surface). See [Patch Boundaries (Frames and No-Ops)](#patch-boundaries-frames-and-no-ops).

8. **Remaining checklist booleans** — once the items above hold, set every other `mergabilityChecklist` boolean honestly based on the gameplan content and the validator's PASS.

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

After the last question is resolved, **re-run `scripts/validate.py`** since edits made during this dialogue may have introduced regressions.

The end state is a gameplan with `openQuestions: []` and an `explicitOpinions` array that captures every decision made during the dialogue.

## Writing Back to the Parent Workstream

The handoff from [[write-workstream]] is one-directional by default: this skill *reads* the workstream's `Established Precedents` and the milestone's `Definition of Done`. But the gameplan brings the milestone sketch into executable detail — and per the workstream skill's "don't over-plan" principle, later milestones are deliberately left thin until they are unpacked. The knowledge that unpacking produces (resolved questions, discovered prior art, real scope) must flow back **up** to the workstream, or the downstream milestones that would benefit never see it.

This matters because the two artifacts have different lifespans. **A workstream is always persisted** — locally, in Notion, or both — and outlives the whole project. **A gameplan often is not**: once its patches land, the JSON has served its purpose and may be discarded. So the write-back is not merely a convenience that mirrors knowledge into a second place; for anything learned while planning this milestone, the workstream is frequently the *only* lasting home. Capture the substance in the workstream prose itself — do not write a workstream entry that says "see the gameplan for details," because the gameplan may be gone.

**This step applies only when the gameplan is part of a workstream** (the `workstream` field is non-null). For a standalone gameplan, skip it entirely.

After the gameplan is finalised, validated, and its `openQuestions` are resolved, **propose a write-back to the workstream and wait for the programmer's approval before applying it.** Do not mutate the workstream silently: it is a shared planning artifact and may live in an external system (e.g. Notion via [[upsert-notion-gameplan]]). Present the intended edits as a diff or a tight summary, get sign-off, then apply them in place — updating existing entries rather than appending duplicates, and never pushing milestone-local detail up to the workstream level.

The list below is not a sequence or priority order. Treat these five categories as independent; collect every applicable item before presenting the write-back summary, regardless of category.

Propose write-backs for:

1. **Decisions from the open-question dialogue.** Every entry added to `explicitOpinions` during [Resolving Open Questions](#resolving-open-questions) that answers a *workstream-level* question or constrains a *later* milestone. Add it to the workstream's `Decisions Made` and close the corresponding row in its `Open Questions`. This is the highest-value write-back — workstream open questions are frequently the ones that can only be resolved once a milestone is actually planned.

2. **Cross-cutting precedents discovered during planning.** The inverse of [Handoff to write-gameplan](#leveraging-established-precedents): if planning surfaced prior art absent from the workstream's `Established Precedents` **and it spans multiple milestones**, promote it up using the workstream's four-field shape (kind, name, url, why applicable). Inherit the same discipline as the downward handoff — a precedent scoped to this one gameplan stays on its patch and does **not** go up.

3. **Scope and Definition-of-Done reconciliation.** The finalised gameplan rarely matches the milestone's original `Definition of Done` exactly — work gets deferred, pulled forward, or split out. Reconcile the milestone record so its `Definition of Done`, `Unlocks`, and (if the milestone ends GATED) `Operator Actions Before Next Milestone` describe what the gameplan actually commits to. Now that flag names and gated state are concrete, fill in placeholder operator actions with the real flag name and the actual signals to watch. **If planning revealed follow-on work that does not fit this milestone — a deferred cutover, a flag flip and old-code removal, a newly-discovered dependency — propose adding it as a new or later milestone** (with a Definition of Done and dependency edges), rather than letting it evaporate. This is the one write-back permitted to suggest structural changes to the workstream; flag it as such so the programmer can weigh it deliberately.

4. **Status, and a pointer if one is durable.** Mark this milestone as planned so the workstream stays a live index of which milestones are unplanned, planned, or executed. You may add a link to the artifact (`gameplans/<project-name>.json`, or the Notion URL if it was synced) as a convenience, but treat it as potentially dangling — the gameplan may not survive once its patches land. The status itself, and the substance captured in the other write-backs (1–3 and 5), must stand on their own without the link resolving.

5. **Acceptance-suite reconciliation.** The workstream ends with a terminal **Definition of Done (Acceptance Suite)** — concrete, observable assertions, each owned by exactly one milestone via its `Traces to`. Planning this gameplan produces the real artifact names the suite needs, and the previous milestone's work has now landed, so make two passes over the suite:

   - **Current milestone — conform the assertions to the *planned* implementation.** For each assertion owned by this milestone, rewrite it to reference the concrete names this gameplan introduces — the actual feature, file/module, class/function, endpoint, table, flag, and command — replacing any placeholder or abstraction left in at workstream-authoring time. Confirm each assertion is genuinely observable via its `api` / `db` / `ux` / `cmd` method against what the gameplan builds, and that its `Traces to` points at a real artifact a patch in this gameplan creates. If the gameplan delivers an observable behavior the suite does not yet assert, add an assertion; if an owned assertion describes behavior this gameplan deliberately does *not* deliver, fix or re-own it (and surface the scope change, per write-back 3). Every owned assertion must map to at least one `acceptanceCriteria` / `finalStateSpec` clause.

   - **Previous milestone — conform the assertions to the *landed* code.** The previous milestone's patches are merged, but its gameplan JSON may be gone — so verify its owned assertions against the **actual codebase**, not against the old gameplan. Open the files, run the queries/commands, hit the endpoints the assertions name. Where reality has drifted from what the assertion says — a renamed file or class, a changed endpoint or flag, a command that no longer exists — correct the assertion to match the landed code so it stays runnable and its `Traces to` resolves. If an assertion now *fails* against landed code (not just drifted naming, but actually-broken behavior), do not quietly rewrite it: flag it as a regression for the programmer.

   This keeps the acceptance suite a living, runnable contract: always conformant to landed code behind the frontier, and always conformant to the planned implementation at the frontier.

## Specification Language

The `spec` and `finalStateSpec` fields contain source code in a formal specification language. The language is a project-level choice — the gameplan structure is the same regardless of which language you use.

### Pantagruel (default)

We use [Pantagruel](https://github.com/subsetpark/pantagruel) — a language for writing formal specifications with domains, rules, and invariants organized into progressive-disclosure chapters.

- **Language reference**: `https://raw.githubusercontent.com/subsetpark/pantagruel/refs/heads/master/REFERENCE.md` — fetch this for syntax details
- **Parse validation**: covered by `scripts/validate.py` (see [Verification](#verification)). Bare `pant <file.pant>` type-checks; exit 0 = well-formed; on 0.22, success is silent.
- **SMT verification (optional)**: `pant --check <file.pant>` runs SMT-based invariant/precondition checks (requires `z3` or `cvc5`). Use this to catch contradictions, unreachable states, and violated invariants — not a replacement for the parse check. Run it whenever a per-patch spec uses `cond` case analysis or sum-type coverage, to confirm the contract is total over its input domain (see [Complete the contract in the spec](#complete-the-contract-in-the-spec)). `--bound N` sets the domain-element bound (default 3); `--solver z3|cvc5` picks a solver. The validator does not run `--check`; invoke it manually when you want SMT coverage.
- **Module naming**: final-state spec uses `<PROJECT_NAME>` (e.g., `EXTRACT_DECISION`); per-patch specs use `<PROJECT_NAME>_PATCH_<N>`
- **Style**:
  - Progressive disclosure (top-down chapter structure); keep per-patch specs self-contained (redeclare referenced domains/rules rather than importing)
  - **Uppercase** identifiers are domains/types (`Patch`, `Disposition`); **lowercase** identifiers are rules/values (`disposition`, `skip`), optionally suffixed with `?` or `!`
  - Enum-like values are best modelled as nullary lowercase rules returning the domain: `skip => Disposition.` (not `Skip => Disposition.` — uppercase rule names will not parse)
  - Rules with multiple parameters use **comma-separated typed params**, not arrows: `inline-decisions rf: RunnerFiber, p: Patch => Nat0.` (not `rf: RunnerFiber -> Patch => Nat0.`)
  - Every chapter needs at least one declaration in its head before the `---` separator; if a patch introduces no new invariants, redeclare the prior chapter's domains/rules so the module is self-contained
  - Make contracts total: model fallible results as sum types (`Outcome = Ok + RateLimited.`) and constrain the rule across all arms; close `cond` case analysis with a final `true => …` arm; declare rules total (no guard) unless partiality is intended, where the guard states the precondition. See [Complete the contract in the spec](#complete-the-contract-in-the-spec)

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
- **Ground every reference** — resolve every file path and symbol against the real checkout before naming it, and give any surface multiple patches touch one shared, identical reference. See [Ground Every Reference in Real Code](#ground-every-reference-in-real-code)
- **Keep it concise** — 10x easier to review than the resulting code
- **Workstream alignment** — if part of a workstream, acceptance criteria must align with the milestone's "Definition of Done" and cover every terminal Acceptance-Suite assertion this milestone owns; reconcile both during [write-back](#writing-back-to-the-parent-workstream)
- **Specs are normative** — the formal specs are the source of truth for what "done" means; prose acceptance criteria are a human-readable summary

## References

- `references/gameplan-schema.json` — Formal JSON Schema (draft 2020-12)
- `references/example.json` — Complete real-world example (uses Pantagruel for specs)
- `scripts/validate.py` — End-to-end validator (schema + pant + routing + DAG + testMap + paths). Run `python3 scripts/validate.py <gameplan.json>` before finalising.
