# pi-workstream

A [Pi](https://pi.dev) extension for interactive workstream authoring with the
[write-workstream skill](../skills/write-workstream/SKILL.md). The user and agent
establish what to build through an interview, then compose milestones together.

The mode provides:

- `/workstream` and `--workstream` to enable the skill and per-run interview context.
- `workstream_open` to select a new or existing Markdown draft. User confirmation
  clears the previous draft's decision ledger without changing either file.
- `workstream_decide` to present implementation alternatives, concrete tradeoffs,
  and a recommendation. Selection records approval. Discussion and dismissal leave
  the choice unresolved; delegation records the recommendation with the user's scope.
- `workstream_finalize` to review the draft and recorded decisions with the user.
  Unresolved recorded decisions prevent finalization. Deferred questions about later
  milestones can remain in the document.

The skill owns planning judgment. Ordinary Markdown editing, shell commands, and
other tools stay available. There is no structured write API or semantic validator:
this mode cannot prove that every implementation choice in prose was discussed.
Final confirmation supplements the interview; it does not replace consultation.

## Install

Requires Pi 1.2+ and an interactive session for decision and review dialogs.

```sh
cd /path/to/onton/pi-workstream
npm ci
pi -e /path/to/onton/pi-workstream/src/index.ts
```

Or install globally, then `/reload`:

```sh
pi install /path/to/onton/pi-workstream
```

The skill is located relative to the extension at `../skills/write-workstream`.
Set `ONTON_WORKSTREAM_SKILL_DIR` to use a different skill directory. The extension
also advertises the skill as `/skill:write-workstream` when the mode is off.

## Use

```text
/gameplan off                 if gameplan mode is currently enabled
/workstream on                or start Pi with --workstream
/workstream status            show the active draft and recorded decisions
/workstream dir docs/roadmaps  choose the directory for subsequent drafts
/workstream off
```

Drafts default to direct `.md` children of `docs/workstreams/`. The mode's tools
select and read files; the agent uses ordinary tools to create directories and edit
the Markdown. Changing the directory affects subsequent draft selections, while
keeping the current draft and its decisions.

Ask the agent to plan a workstream. It selects the draft with `workstream_open`,
interviews you about goals, lifespan, stakes, budgets, and non-goals, and inspects
the existing code. It must discuss implementation choices before committing them
to milestones. `workstream_decide` records those choices through Pi's built-in
selector, with discussion and delegation options. Revisiting an id replaces that
answer and clears finalization.

When the draft is ready, the agent summarizes its scope and deferred questions,
then calls `workstream_finalize`. The review dialog includes the Markdown and the
recorded decisions. Dismissing or declining it does not finalize. Changes to the
file or ledger during review require another review.

The active draft, decisions, mode settings, and finalization survive reload/resume.
Session-tree navigation restores only the selected branch's state. The finalized
marker clears when the draft bytes change or the file becomes unavailable. The
footer shows the decision count, unresolved count, and finalized status.

Opening another draft starts a fresh ledger. Existing prose never imports approval;
revisit its implementation choices with the user before adopting them. Finalization
runs no milestones and performs no Git or tracking-system writes. When ready to
plan a milestone, turn off workstream mode and enable gameplan mode.

## Develop

```sh
npm ci
npm run check
npm test
```

Tests drive the registered commands, tools, and lifecycle callbacks through a host
adapter with real draft files and scripted user responses.
