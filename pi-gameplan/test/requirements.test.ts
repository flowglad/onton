import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { test } from "node:test";
import { describeGroups, missingGroups, noRequirements, parseGroups, parseSkillRequirements } from "../src/requirements.js";
import { resolveSkillPaths } from "../src/validator.js";

const skillMd = `---
name: write-gameplan
description: Create a gameplan.
metadata:
  pi-requires: "npm:pi-web-access: fetch_content web_search"
  pi-recommends: "npm:pi-lens: lsp_navigation ast_grep_search; npm:@hank-warren/pi-ask-user-question: ask_user_question"
---

# Body
`;

test("required and recommended groups are read from the frontmatter metadata", () => {
  assert.deepEqual(parseSkillRequirements(skillMd), {
    required: [{ package: "npm:pi-web-access", tools: ["fetch_content", "web_search"] }],
    recommended: [
      { package: "npm:pi-lens", tools: ["lsp_navigation", "ast_grep_search"] },
      { package: "npm:@hank-warren/pi-ask-user-question", tools: ["ask_user_question"] },
    ],
  });
});

test("group parsing tolerates separators and drops malformed entries", () => {
  assert.deepEqual(parseGroups("npm:a: x, y ;; git:github.com/o/r@v1: z;  broken  ; npm:b:"), [
    { package: "npm:a", tools: ["x", "y"] },
    { package: "git:github.com/o/r@v1", tools: ["z"] },
  ]);
  assert.deepEqual(parseGroups(3), []);
});

test("missing or malformed frontmatter yields no requirements", () => {
  assert.deepEqual(parseSkillRequirements("# no frontmatter"), noRequirements);
  assert.deepEqual(parseSkillRequirements("---\nname: x\n---\n"), noRequirements);
  assert.deepEqual(parseSkillRequirements("---\nname: [\n---\n"), noRequirements);
});

test("missing groups keep only the unregistered tools and describe the install per package", () => {
  const { required, recommended } = parseSkillRequirements(skillMd);
  assert.deepEqual(missingGroups(required, ["read", "web_search"]), [{ package: "npm:pi-web-access", tools: ["fetch_content"] }]);
  assert.deepEqual(missingGroups(required, ["fetch_content", "web_search"]), []);
  assert.equal(
    describeGroups(missingGroups(recommended, ["ast_grep_search"])),
    "lsp_navigation (pi install npm:pi-lens); ask_user_question (pi install npm:@hank-warren/pi-ask-user-question)",
  );
});

test("the real skill requires the web tools and recommends LSP, subagents, and ask-user", async () => {
  const text = await readFile(resolveSkillPaths({}).skillMd, "utf8");
  const { required, recommended } = parseSkillRequirements(text);
  assert.deepEqual(required, [{ package: "npm:pi-web-access", tools: ["fetch_content", "web_search"] }]);
  assert.deepEqual(
    recommended.map((g) => g.package),
    ["npm:pi-lens", "npm:pi-subagents", "npm:@hank-warren/pi-ask-user-question"],
  );
  assert.ok(recommended.flatMap((g) => g.tools).includes("ask_user_question"));
});
