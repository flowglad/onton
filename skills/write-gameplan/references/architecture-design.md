# Architecture design and delegation

Requirements describe desired behavior. Architecture describes the responsibilities,
state lifetimes, execution semantics and boundaries carrying that behavior. Patch
plans and code describe its realization. Resolve meaningful architecture alternatives
with the engineer before deriving patch tasks. Present a recommendation and concrete
tradeoffs; record their answer, rather than treating lack of objection as approval.

## Altitude

For connector qualification, “retain durable evidence” is a requirement. Which service
owns qualification, whether evidence is relational state or immutable objects, how work
is dispatched/retried, where admission is enforced, and how evidence reaches publication
are architecture. Exact digest algorithms, index definitions, timeout constants, IDs and
helper decomposition usually belong to implementation. Assess the consequence of a
choice, rather than its noun: a timeout that changes a promised completion guarantee
can be architectural. Existing code is evidence of feasibility, not engineer approval.

Ask: could alternatives satisfy the requested outcome while changing who owns work,
what state survives, when work happens, how failures recover, who validates authority,
or how data moves? If so, consult unless prior approval, binding constraints, or explicit
scoped delegation settles it. Group related independent choices and explain their
combined system design. Do not ask the engineer to adjudicate interchangeable mechanics.

## Format and execution

```yaml
architectureDesign:
  summary: >-
    The application owns admission and stores qualification state. A durable task
    executes qualification and returns evidence to the application's publication gate.
  decisions:
    - id: AD-1
      topic: execution
      question: How should qualification execute outside the request?
      choice: A durable asynchronous task with retry and idempotency guarantees.
      alternatives:
        - choice: Synchronous execution in the admission request.
          tradeoffs: Simpler dispatch but couples request latency and provider failure.
      resolution:
        kind: engineer_approved
        evidence: Engineer selected durable tasks in the design discussion, with retries.
        rationale: Provider latency exceeds the request budget; admission awaits evidence.
```

This is illustrative; never copy its approval evidence into a real plan. `topic` is a
short free-form label (ownership, persistence, execution, validation, data flow, etc.).
`alternatives` records actual credible alternatives, not straw options; `[]` is suitable
when binding constraints or an already approved decision leave none to reconsider.
`resolution.kind` is one of `engineer_approved`, `constrained`, `delegated` (each requires
non-empty `evidence` and `rationale`), or `unresolved` (requires `reason`). While drafting,
`choice` may state a provisional recommendation, marked unresolved. All decisions require
unique IDs. Routine plans can have no decisions, with a summary explaining unchanged
architecture. This is one authoritative architectural inventory; other opinions stay in
`explicitOpinions`.

Newly authored plans use `formatVersion: 3`. Both authoring validation and Onton
admission require the section for v3, even when `openQuestions` is empty. The authoring
validator rejects older or missing versions even without the optional JSON Schema library,
so downgrading is not an authoring escape hatch. Onton retains compatibility with
existing v2 and unversioned plans; when a section is present in any version, it rejects
malformed or unresolved decisions. Admitted design, tradeoffs and resolution evidence are carried into
typed runtime records and persisted session state. Renderers turn those records into
patch context and patch/feature PR descriptions; unresolved resolutions have no runtime
variant. The gate checks the declaration, not whether
a conversation actually occurred or the design inventory is complete. Authoring must
still inspect the whole plan for unrecorded choices. A schema pass is not human approval.

## Research basis

Sources reviewed 2026-10-06.

- [Anthropic: Building effective agents](https://www.anthropic.com/engineering/building-effective-agents)
  describes interactive task clarification followed by independent execution, environmental
  feedback, and human review for broader system alignment. Its programmatic workflow gates
  motivate validating the handoff instead of relying only on prompt instructions.
- [OpenAI: Harness engineering](https://openai.com/index/harness-engineering/)
  describes repository-resident design knowledge and mechanical architectural constraints,
  while allowing agents latitude in implementation. This motivates preserving the resolved
  architecture in execution context without prescribing every helper or library.
- [GitHub Spec Kit: Agentic SDD](https://github.github.com/spec-kit/reference/agentic-sdd.html)
  separates clarification, technical planning, task decomposition and execution. Answers
  are written back into specs; artifact consistency is checked before implementation.
  This motivates dialogue before decomposition and consistency across design, patches and specs.
- [Anthropic: How AI is transforming work](https://www.anthropic.com/research/how-ai-is-transforming-work-at-anthropic)
  reports a staff survey, interviews and usage analysis. Respondents describe delegation
  as easier for well-defined, self-contained and easily verified tasks, with active
  supervision for complex work. This supports using verifiability and consequence to
  select the human/autonomous boundary, rather than a fixed approval step for every action.

These are practitioner accounts, tooling guidance and observational research, not a
controlled evaluation of this gameplan format. The architectural altitude rule and
resolution schema are Onton's design choices informed by them. Assess effectiveness by
reviewing whether newly authored plans expose consequential alternatives before execution,
whether consultation happens at that stage, and whether agents execute mechanics without
unnecessary questions. Avoid measuring success by the number of filled decision entries.
