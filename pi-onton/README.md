# pi-onton

Pi agent mode for coding sessions supervised by Onton. Requires Pi 1.2+.

Onton bundles this extension and automatically loads it on every `--backend pi`
execution, including resumed sessions. No global Pi installation or npm setup is
needed. The extension activates only when Onton supplies `ONTON_PI_CONTEXT`;
ordinary Pi sessions are unaffected.

For development, override the bundled entrypoint:

```sh
cd /path/to/onton/pi-onton
npm ci
ONTON_PI_EXTENSION=/absolute/path/to/onton/pi-onton/src/index.ts onton --backend pi ...
```

Do not also install this package globally in Pi: the backend already loads it.
No interactive UI or approval dialog is needed.

| Existing communication | Mode interface |
| --- | --- |
| Initial implementation and gameplan/patch requirements | Original prompt plus `onton_context` |
| Human guidance and base-branch changes | Current supervisor turn via `onton_context` |
| Review comments and external review findings | Current prompt with original IDs and anchors |
| CI failures, missing CI observations, uncommitted changes | Current prompt plus `onton_read_artifact` for evidence |
| Git reconciliation, scheduler repair, workspace recovery | Original captured repair prompt, without reinterpretation |
| Publication feedback | Original supervisor instructions |
| Published gameplan and dependency implementation notes | `onton_read_artifact` |
| Implementation notes | `onton_respond(kind: "implementation_notes", content)` |
| Review reply | `onton_respond(kind: "review_response", id: comment_id, content)` |
| Unaddressed finding | `onton_respond(kind: "finding_wontfix", id: wontfix_file, content)` |
| Offered pre-commit opt-out | `onton_respond(kind: "opt_out", content)` |
| Text, tool activity, session identity, end-of-turn, process errors | Native Pi JSON stream and process exit, consumed by existing Onton backend |

`onton_context` returns the exact prompt for this execution, not a reconstructed
summary. On resume this may be just the new turn; preceding context remains in
Pi's conversation. `before_agent_start` adds protocol guidance and execution identity.
After compaction, `session_compact` restores a pointer to the recovery tools.
`session_start` validates the delivery and shows status when a UI exists.

Artifacts are addressed relative to the returned `artifacts_root`; use paths
from the prompt. Reads support byte `offset` and `limit` (maximum 65536), returning
`next_offset` and `eof`. Missing evidence is a tool error, never an invented result.
Responses are atomic replacements of the same files Onton already consumes.
Copy the exact comment ID or `wontfix_file` from the prompt. Code-fixed findings
need no wontfix file. Opt-out is accepted only under Onton's existing pre-commit
rules. Report other blockers in the normal final response.

The extension does not commit, push, post replies, resolve threads, or grant
completion authority. Follow each prompt's Git instructions; Onton owns session
validation, reconciliation, publication, and forge updates. Feedback is delivered
on supervisor turns, not through an additional polling loop.

Dune embeds the TypeScript sources in the Onton binary. The backend materializes
them under each execution’s session directory and passes `--extension` to Pi.
The sources use only Node built-ins and TypeBox supplied by Pi’s extension loader.

The backend writes a version-1 `pi-context.json` into the execution's session
artifact directory and passes its absolute path in `ONTON_PI_CONTEXT`. It contains
project, patch and execution identity, the current prompt, and artifact locations.
It is not a credential or a sandbox: extensions share the agent's OS permissions.
No source text parsing or inferred home-directory layout is required.

Development checks:

```sh
npm run check
npm test
```

The lifecycle integration follows Pi's [extension API](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/extensions.md).
