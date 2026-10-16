# pi-gameplan

Pi extension that enforces the write-gameplan skill's non-prose rules: engineer-answered
architecture decisions and a finalize gate. It does not police writes, edits, or shell
commands; keep interventions in the agent loop minimal.

- `src/state.ts`, `src/audit.ts`, `src/guard.ts`, `src/ask.ts`, `src/requirements.ts`,
  `src/document.ts`, and `src/spec.ts` are pure: total functions, no Pi, no file I/O. Keep
  them that way and test them in `test/`.
- `pant` is invoked only from `src/index.ts` (temp file, since `pi.exec` has no stdin). Its
  diagnostics format (`path:line:col: error: msg`) is parsed in `src/spec.ts`.
- `src/document.ts` validates with Ajv against the skill's `references/gameplan-schema.json`
  (loaded by the caller) and edits the YAML through the `yaml` Document API so comments and
  untouched sections survive. Do not regenerate the whole file from objects.
- `src/ask.ts` mirrors the `ask_user_question` schema and envelope of
  `@hank-warren/pi-ask-user-question` (shared with `@juicesharp/rpiv-ask-user-question`).
  If that package changes its option limits or `details` shape, update `ask.ts` and its test.
- Skill requirements live in the skill's frontmatter (`pi-requires`, `pi-recommends`), parsed
  by `src/requirements.ts`. Add packages there, not in code.
- `src/validator.ts` shells out to `../skills/write-gameplan/scripts/{validate,format_yaml}.py`.
  Never reimplement a validator check here; add it to the skill's script instead.
- `src/index.ts` is the only module that touches `ExtensionAPI`, `ctx.ui`, or the session.
- Evidence strings for `engineer_approved` and `delegated` are generated in `resolveDecision`
  and compared verbatim by `auditDecisions`. Changing their wording invalidates ledgers in
  existing sessions; do not change it casually.
- Read the installed Pi docs (`docs/extensions.md`, `docs/tui.md` in the
  `@earendil-works/pi-coding-agent` package) before changing host integration.
- Run `npm run check` and `npm test`. Set `ONTON_GAMEPLAN_PYTHON` to a `uv run --with ...`
  wrapper to include the validator integration tests.
