/**
 * Pure gameplan document operations: compose architecture decisions from the ledger, validate
 * sections against the skill's JSON Schema, and apply section or patch updates to a YAML
 * document in place (comments and untouched sections survive).
 *
 * Nothing here reads or writes files. The schema object and the document text come in as
 * arguments; the caller owns I/O.
 */

import { Ajv2020, type ErrorObject } from "ajv/dist/2020.js";
import { Document, isMap, isSeq, type Pair, parseDocument, type YAMLMap, type YAMLSeq } from "yaml";
import type { DecisionRecord } from "./state.js";

export type JsonObject = Record<string, unknown>;

function isObject(value: unknown): value is JsonObject {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

// ---------------------------------------------------------------------------------------------
// Schema validation

export interface SchemaValidator {
  /** Top-level section names in canonical (schema `required`) order. */
  sections: string[];
  /** Errors for one top-level section's value; empty when valid or when the section is unknown. */
  section(name: string, value: unknown): string[];
  /** Errors for one entry of `patches`. */
  patch(value: unknown): string[];
}

export function createValidator(schema: JsonObject): SchemaValidator {
  const ajv = new Ajv2020({ allErrors: true, strict: false });
  const defs = schema.$defs;
  const properties = isObject(schema.properties) ? schema.properties : {};
  const sections = Array.isArray(schema.required) ? schema.required.filter((s): s is string => typeof s === "string") : [];
  const cache = new Map<string, ReturnType<typeof ajv.compile>>();

  function compiled(key: string, sub: unknown) {
    let fn = cache.get(key);
    if (!fn) {
      fn = ajv.compile({ ...(isObject(sub) ? sub : {}), $defs: defs });
      cache.set(key, fn);
    }
    return fn;
  }

  function format(errors: ErrorObject[] | null | undefined, prefix: string): string[] {
    return (errors ?? []).map((e) => {
      const where = e.instancePath ? `${prefix}${e.instancePath}` : prefix || "/";
      const allowed = Array.isArray((e.params as { allowedValues?: unknown[] }).allowedValues)
        ? ` (${(e.params as { allowedValues: unknown[] }).allowedValues.map((v) => JSON.stringify(v)).join(", ")})`
        : "";
      const extra = typeof (e.params as { additionalProperty?: string }).additionalProperty === "string"
        ? ` '${(e.params as { additionalProperty: string }).additionalProperty}'`
        : "";
      return `${where}: ${e.message ?? "invalid"}${extra}${allowed}`;
    });
  }

  return {
    sections,
    section(name, value) {
      if (!(name in properties)) return [];
      const fn = compiled(`section:${name}`, properties[name]);
      return fn(value) ? [] : format(fn.errors, name);
    },
    patch(value) {
      const fn = compiled("patch", { $ref: "#/$defs/patch" });
      return fn(value) ? [] : format(fn.errors, "patch");
    },
  };
}

// ---------------------------------------------------------------------------------------------
// Decisions

/** Decode an existing ledger only after schema validation; caller must obtain import consent. */
export function importDecisions(doc: unknown, validator: SchemaValidator, recordedAt: string): DecisionRecord[] {
  if (!isObject(doc) || !isObject(doc.architectureDesign)) throw new Error("Missing architectureDesign");
  const design = doc.architectureDesign;
  const errors = validator.section("architectureDesign", design);
  if (errors.length) throw new Error(errors.join("\n"));
  const records: DecisionRecord[] = [];
  const seen = new Set<string>();
  for (const raw of design.decisions as JsonObject[]) {
    const id = raw.id as string;
    if (seen.has(id)) throw new Error(`${id}: duplicate decision id`);
    seen.add(id);
    const resolution = raw.resolution as JsonObject;
    if (resolution.kind === "constrained") continue;
    records.push({
      id, topic: raw.topic as string, question: raw.question as string,
      choice: raw.choice as string,
      alternatives: raw.alternatives as DecisionRecord["alternatives"],
      kind: resolution.kind as DecisionRecord["kind"],
      evidence: typeof resolution.evidence === "string" ? resolution.evidence : "",
      reason: resolution.kind === "unresolved" ? "Unresolved in imported gameplan" : undefined,
      recordedAt,
    });
  }
  return records;
}

/**
 * Fill `architectureDesign.decisions` from the ledger. For an answered decision the model
 * supplies only `id` and `rationale` (optionally `topic`); choice, alternatives, kind
 * and evidence come from the record. Decisions without a record must be `constrained` or
 * `unresolved`. Answered decisions left out are errors, so the file cannot silently drop one.
 */
export function composeDecisions(design: unknown, ledger: readonly DecisionRecord[]): { design: unknown; errors: string[] } {
  if (!isObject(design) || !Array.isArray(design.decisions)) return { design, errors: [] };
  const errors: string[] = [];
  const seen = new Set<string>();
  const decisions = design.decisions.map((raw) => {
    if (!isObject(raw) || typeof raw.id !== "string") return raw;
    const id = raw.id;
    if (seen.has(id)) errors.push(`${id}: duplicate decision id`);
    seen.add(id);
    const record = ledger.find((d) => d.id === id);
    const resolution = isObject(raw.resolution) ? raw.resolution : {};
    if (!record) {
      const kind = resolution.kind;
      if (kind === "engineer_approved" || kind === "delegated") {
        errors.push(`${id}: ${kind} requires an engineer answer; call gameplan_decide for ${id} first`);
      }
      return raw;
    }
    const topic = typeof raw.topic === "string" && raw.topic.trim() ? raw.topic : record.topic;
    const question = record.question;
    if (record.kind === "unresolved") {
      return {
        id,
        topic,
        question,
        choice: record.choice,
        alternatives: record.alternatives,
        resolution: { kind: "unresolved", reason: record.reason ?? "The engineer has not answered yet." },
      };
    }
    const rationale = [resolution.rationale, raw.rationale].find((r) => typeof r === "string" && r.trim().length > 0);
    if (typeof rationale !== "string") {
      errors.push(`${id}: rationale is required; say why the engineer's choice fits this codebase`);
    }
    return {
      id,
      topic,
      question,
      choice: record.choice,
      alternatives: record.alternatives,
      resolution: { kind: record.kind, evidence: record.evidence, rationale: rationale ?? "" },
    };
  });
  for (const record of ledger) {
    if (record.kind !== "unresolved" && !seen.has(record.id)) {
      errors.push(`${record.id}: answered by the engineer but not included; add { id: "${record.id}", rationale: "..." }`);
    }
  }
  return { design: { ...design, decisions }, errors };
}

// ---------------------------------------------------------------------------------------------
// Document updates

/** Parse existing text, or start an empty mapping for a new gameplan. */
export function loadGameplanDocument(text: string | undefined): Document {
  if (text && text.trim()) {
    const doc = parseDocument(text);
    if (doc.errors.length > 0) throw new Error(`existing gameplan is not valid YAML: ${doc.errors[0]?.message ?? "parse error"}`);
    if (!isMap(doc.contents)) throw new Error("existing gameplan is not a mapping");
    return doc;
  }
  return new Document({});
}

function keyName(pair: Pair): string | undefined {
  const key: unknown = pair.key;
  if (typeof key === "string") return key;
  if (typeof key === "object" && key !== null && "value" in key) {
    const v = (key as { value: unknown }).value;
    return typeof v === "string" ? v : undefined;
  }
  return undefined;
}

function rootMap(doc: Document): YAMLMap {
  if (!isMap(doc.contents)) throw new Error("gameplan document is not a mapping");
  return doc.contents;
}

/** Set top-level sections, then order the mapping by `order` with unknown keys last. */
export function applySections(doc: Document, sections: JsonObject, order: readonly string[]): void {
  const root = rootMap(doc);
  if (!root.has("formatVersion") && !("formatVersion" in sections)) root.set("formatVersion", 3);
  for (const [name, value] of Object.entries(sections)) {
    // Scalars update the existing node in place (keeping its comment); collections get fresh nodes.
    const scalar = value === null || ["string", "number", "boolean"].includes(typeof value);
    root.set(name, scalar ? value : doc.createNode(value));
  }
  const rank = (pair: Pair) => {
    const i = order.indexOf(keyName(pair) ?? "");
    return i < 0 ? order.length : i;
  };
  root.items = root.items
    .map((pair, index) => ({ pair, index }))
    .sort((a, b) => rank(a.pair) - rank(b.pair) || a.index - b.index)
    .map((x) => x.pair);
}

function patchNumber(node: unknown): string | undefined {
  if (isMap(node)) {
    const n: unknown = node.get("number", true);
    const v = typeof n === "object" && n !== null && "value" in n ? (n as { value: unknown }).value : n;
    return v === undefined || v === null ? undefined : String(v);
  }
  return undefined;
}

/** Insert or replace the patch with the same `number`; the sequence stays sorted by number. */
export function upsertPatch(doc: Document, patch: JsonObject): "added" | "replaced" {
  const root = rootMap(doc);
  const number = String(patch.number);
  let seq = root.get("patches", true) as unknown;
  if (!isSeq(seq)) {
    seq = doc.createNode([]);
    root.set("patches", seq);
  }
  const list = seq as YAMLSeq;
  const node = doc.createNode(patch);
  const index = list.items.findIndex((item) => patchNumber(item) === number);
  const outcome = index < 0 ? "added" : "replaced";
  if (index < 0) list.items.push(node);
  else list.items[index] = node;
  const numeric = list.items.every((item) => /^\d+$/.test(patchNumber(item) ?? ""));
  if (numeric) list.items.sort((a, b) => Number(patchNumber(a)) - Number(patchNumber(b)));
  return outcome;
}

/** Set `spec` on the patch with this number. Returns false when no such patch exists. */
export function setPatchSpec(doc: Document, number: string, spec: string): boolean {
  const seq = rootMap(doc).get("patches", true) as unknown;
  if (!isSeq(seq)) return false;
  const node = seq.items.find((item) => patchNumber(item) === number);
  if (!isMap(node)) return false;
  node.set("spec", spec);
  return true;
}

/** The `projectName` scalar, when present. */
export function projectName(doc: Document): string | undefined {
  const value: unknown = rootMap(doc).get("projectName");
  return typeof value === "string" ? value : undefined;
}

/** Patch numbers present in `patches`, as strings. */
export function presentPatchNumbers(doc: Document): string[] {
  const seq = rootMap(doc).get("patches", true) as unknown;
  if (!isSeq(seq)) return [];
  return seq.items.map((item) => patchNumber(item)).filter((n): n is string => n !== undefined);
}

/**
 * Patch numbers that other sections point at (functional-change owners and consumers, context
 * resource consumers, test map stub/impl patches, trace owners, ordering constraints) but that
 * `patches` does not contain yet. While any exist, the validator reports a cascade of
 * dangling-reference errors that mean nothing more than "write that patch".
 */
export function unwrittenReferencedPatches(doc: Document): string[] {
  const present = new Set(presentPatchNumbers(doc));
  const referenced = new Set<string>();
  const add = (value: unknown) => {
    if (typeof value === "number" || (typeof value === "string" && value.trim())) referenced.add(String(value));
  };
  const addAll = (value: unknown) => {
    if (Array.isArray(value)) value.forEach(add);
  };
  const js = doc.toJS() as Record<string, unknown> | null;
  if (!js || typeof js !== "object") return [];
  const rows = (key: string): Record<string, unknown>[] =>
    Array.isArray(js[key]) ? (js[key] as unknown[]).filter(isObject) : [];
  for (const fc of rows("functionalChanges")) {
    add(fc.ownedBy);
    addAll(fc.requiredBy);
  }
  for (const cr of rows("contextResources")) addAll(cr.consumedBy);
  for (const tm of rows("testMap")) {
    add(tm.stubPatch);
    add(tm.implPatch);
  }
  for (const tr of rows("reachabilityTraces")) add(tr.ownedBy);
  for (const oc of rows("orderingConstraints")) {
    add(oc.before);
    add(oc.after);
  }
  return [...referenced].filter((n) => !present.has(n)).sort((a, b) => Number(a) - Number(b) || a.localeCompare(b));
}

/** Top-level sections the schema requires that the document does not have yet. */
export function missingSections(doc: Document, order: readonly string[]): string[] {
  const root = rootMap(doc);
  return order.filter((name) => !root.has(name));
}

export function patchCount(doc: Document): number {
  const seq = rootMap(doc).get("patches", true) as unknown;
  return isSeq(seq) ? seq.items.length : 0;
}

export function renderDocument(doc: Document): string {
  return doc.toString({ lineWidth: 88 });
}
