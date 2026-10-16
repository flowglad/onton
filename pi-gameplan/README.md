# pi-gameplan

A [Pi](https://pi.dev) extension for authoring Onton gameplans with the
[write-gameplan skill](../skills/write-gameplan/SKILL.md). The skill keeps the judgment;
this extension owns the rules the skill cannot enforce by prose:

- **Engineer approval is unforgeable.** `gameplan_decide` is the only way a decision can be
  recorded as `engineer_approved` or `delegated`. It asks the engineer in a dialog, generates
  the `resolution.evidence` text itself, and the check tool rejects any resolution in the YAML
  that does not match a recorded answer.
- **The model never writes YAML by hand.** `gameplan_write` and `gameplan_write_patch` take
  JSON sections, validate them against the skill's JSON Schema, compose
  `architectureDesign.decisions` from the recorded answers, and update the file in place
  through the skill's formatter. Shape errors come back per field before anything is written.
  `gameplan_write_spec` parses each Pantagruel spec with `pant` before it lands.
- **Nothing finalizes with open questions.** `gameplan_finalize` refuses while the validator
  fails, a decision is unresolved, or `openQuestions` is non-empty. Agent-asserted
  `constrained` decisions are shown to the engineer for acceptance.
- **It stays out of the way otherwise.** The mode never blocks `write`, `edit`, or shell
  commands; the skill's workstream write-back and any other edit go through the ordinary tools.

## Requirements

- Pi 1.2+ (interactive mode for the decision and finalize dialogs)
- Python 3 with the skill's dependencies (`skills/write-gameplan/scripts/requirements.txt`),
  or `uv`, see below
- `pant` on `PATH` so specs are checked (the validator skips them otherwise)

## Install

First install the extension's dependencies in your checkout:

```sh
cd /path/to/onton/pi-gameplan
npm ci
```

Then, from any repository you are planning a change for:

```sh
pi -e /path/to/onton/pi-gameplan/src/index.ts
```

Or install it globally and `/reload`:

```sh
pi install /path/to/onton/pi-gameplan
```

The extension locates the skill as `../skills/write-gameplan` relative to itself. Set
`ONTON_GAMEPLAN_SKILL_DIR` to point elsewhere.

The skill's Python scripts carry inline (PEP 723) dependency metadata. When `uv` is on
`PATH` the extension runs them with `uv run`, which resolves those dependencies with no
setup. Otherwise it uses `python3`, which must already have `scripts/requirements.txt`
installed. `ONTON_GAMEPLAN_PYTHON` overrides both with an interpreter or wrapper of your
choice. A missing module is reported as an environment problem with these three options,
not as a gameplan error.

## Skill requirements and recommended packages

The Agent Skills spec has no dependency field, so the skill declares what it needs in its
frontmatter `metadata` (string values only, per the spec), grouped by the package that
provides each tool:

```yaml
metadata:
  pi-requires: "npm:pi-web-access: fetch_content web_search"
  pi-recommends: "npm:pi-lens: lsp_navigation ast_grep_search; npm:pi-subagents: subagent; npm:@hank-warren/pi-ask-user-question: ask_user_question"
```

When the mode is enabled, the extension compares both lists against the registered tools.
Missing required tools produce a warning with the install command; missing recommended tools
produce an informational notice. Nothing blocks. `/gameplan status` shows both.

| Package | Status | Why the skill wants it |
|---|---|---|
| [pi-web-access](https://github.com/nicobailon/pi-web-access) | required | `fetch_content` and `web_search` stand in for WebFetch when verifying precedents and library docs. Without them the agent is told not to cite URLs it could not fetch. |
| [pi-lens](https://github.com/apmantza/pi-lens) | recommended | `lsp_navigation` and `ast_grep_search` for the skill's efficacy grounding (call hierarchy, references). Without them the agent falls back to grep, ast-grep, and call-graph CLIs. Heavy package; try it in one repo first. |
| [pi-subagents](https://github.com/nicobailon/pi-subagents) | recommended | `subagent` with the builtin `scout` and `researcher` agents for parallel reachability tracing and prior-milestone grounding. |
| [@hank-warren/pi-ask-user-question](https://pi.dev/packages/@hank-warren/pi-ask-user-question) | recommended | `ask_user_question` for scope and open-question clarification, and `gameplan_decide` delegates its dialog to it when present (see below). |

```sh
pi install npm:pi-web-access
pi install npm:pi-lens npm:pi-subagents npm:@hank-warren/pi-ask-user-question   # optional
```

The run context tells the agent which of these are available and what to use instead when
they are not.

## Decision dialog

`gameplan_decide` asks through `ask_user_question` when that tool is registered: a tabbed
form with each option's tradeoffs as its description, the recommendation marked, a
"Delegate to the agent" row when there are three or fewer options, notes on any choice
(a note on the delegate row becomes the delegation scope), and the tool's own free-text row.
Otherwise it uses Pi's built-in selector with **Other** and **Delegate** rows. Either way the
answer is mapped to the same ledger record, and a decline leaves the decision unresolved.

## The gameplan file pointer

The extension remembers the gameplan file the session last wrote or checked and names it in
every run's context, so the agent re-reads the file rather than trusting its memory after
compaction. `gameplan_check` and `gameplan_finalize` default to that file when called without
a path.

## Use

```text
/gameplan                 toggle authoring mode (or start Pi with --gameplan)
/gameplan status          mode, directory, recorded decisions
/gameplan dir docs/plans  change the directory writes are admitted to (default: gameplans)
```

The extension registers the write-gameplan skill with Pi, so it is advertised in the system
prompt and available as `/skill:write-gameplan` in any session that loads the extension. You
do not need to invoke it by hand: with the mode on, each run gets a hidden context message
that tells the agent to follow the skill (and read it if it has not yet), states the write
restriction, and lists the recorded decisions. The agent:

1. inspects the code and scopes the change;
2. calls `gameplan_decide` for each architectural choice. You see the question, the options
   with tradeoffs, and the recommendation in the transcript, then a selector with the options,
   **Other** (free text in the editor), and **Delegate** (records the recommendation as
   `delegated` with your stated scope). Dismissing the selector records the decision as
   `unresolved`;
3. builds the file with `gameplan_write` (a few top-level sections per call) and
   `gameplan_write_patch` (one patch per call). Each call validates its input against
   `references/gameplan-schema.json`, writes nothing on error, and otherwise updates the
   file in place, keeping comments and untouched sections, then runs the formatter. For
   answered decisions the model passes only `{ id, rationale }`; the tool fills choice,
   alternatives, kind, and evidence from the ledger and rejects any `engineer_approved` or
   `delegated` entry that has no recorded answer. The result lists which required sections
   are still missing;
   Specs go in through `gameplan_write_spec`, one per call, for a patch (by number) or for
   `finalStateSpec`. The spec is parsed with `pant` first and nothing is written on a parse
   error; diagnostics quote the offending line. The module name is compared with the
   skill's convention and a mismatch is reported as a warning. With `check: true`,
   `pant --check` runs SMT verification and a contradiction blocks the write;
4. calls `gameplan_check` once the write tools report nothing missing, and fixes what it
   reports. Every write result lists the required sections not yet written and the patch
   numbers other sections reference but `patches` does not contain. On such a document
   `gameplan_check` returns `INCOMPLETE` with the same lists instead of running the validator,
   because a half-written plan only yields a cascade of dangling-reference errors;
5. calls `gameplan_finalize`, which asks you to accept any `constrained` decisions and to
   confirm, then formats the file in place and re-validates. The mode never blocks `write`,
   `edit`, or shell commands: the skill's workstream write-back and any other edit the
   engineer asks for go through the ordinary tools.

The file remains the source of truth: you can edit it by hand (the raw `write`/`edit`
tools are still admitted for `gameplans/*.yaml`), and the next tool write parses what is
there before applying its change. The ledger audit in `gameplan_check` still runs as
defense in depth.

The footer shows `gameplan · N decisions [· M unresolved] [· finalized]`.

Decisions are stored in the session file as custom entries and rebuilt from the active branch
on resume, so `/fork` or `/tree` to explore a different architecture does not inherit answers
given on the abandoned branch.

## What is deliberately not here

- No second schema. The write tools validate with Ajv against the skill's own
  `references/gameplan-schema.json`; nothing is mirrored into TypeBox. Their tool
  declarations stay small (`{ section: value }`), so the model reads the schema file as the
  skill already requires rather than paying for it in every request.
- No third semantic validator. `gameplan_check` shells out to the skill's
  `scripts/validate.py` and parses its output; the write tools validate shape and audit recorded decisions.
  `gameplan_write_spec` also requires successful `pant` parsing, reports module-name
  mismatches as warnings, and blocks writes when optional `pant --check` verification fails.
- No execution handoff. The finalize result prints the `onton --gameplan` command.

## Develop

```sh
npm install
npm run check
npm test                                  # unit tests; validator integration runs when uv is on PATH
```

The configured gameplan directory may be a symlink; its canonical directory is the write
root. Gameplan tool writes reject file symlinks, including links within that root. Tool writes
accept only direct `.yaml` or `.yml` children.

Finalization snapshots hash raw file bytes and accept only regular gameplan files up to
16 MiB. Invalid, missing, or oversized snapshot files clear the finalized marker.

Gameplan commits prepare a complete replacement in the admitted directory before atomic
publication. A small Python helper uses descriptor-relative filesystem operations (which
Node does not expose) and verifies the directory identity captured at admission. It uses
the same configured Python/uv command as the formatter.
