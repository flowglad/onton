#!/usr/bin/env python3
"""Validate a gameplan JSON.

Runs every check enumerated in SKILL.md's Verification section that can be
mechanised: JSON Schema shape, Pantagruel spec parsing, context-routing
reciprocity, functional-change ownership, dependency-graph integrity, and
testMap consistency.

Usage:
    python3 scripts/validate.py path/to/gameplan.json

Exit codes:
    0 — all checks pass
    1 — one or more checks failed
    2 — usage / I/O error

Soft dependencies:
    jsonschema  — enables shape validation; without it, only semantic checks run
    pant        — enables Pantagruel spec parsing; without it, specs are skipped
"""
from __future__ import annotations

import json
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any

SCHEMA_PATH = Path(__file__).resolve().parent.parent / "references" / "gameplan-schema.json"


def _pid(x: Any) -> str:
    """Normalise patch IDs to strings (schema permits int or str)."""
    return str(x)


def validate_schema(inst: dict, errors: list[str]) -> None:
    try:
        import jsonschema  # type: ignore
    except ImportError:
        print("WARN: jsonschema not installed; skipping shape validation", file=sys.stderr)
        return
    schema = json.loads(SCHEMA_PATH.read_text())
    for e in jsonschema.Draft202012Validator(schema).iter_errors(inst):
        loc = "/".join(str(p) for p in e.absolute_path) or "<root>"
        errors.append(f"schema [{loc}]: {e.message}")


def validate_patch_numbers(inst: dict, errors: list[str]) -> dict[str, dict]:
    patches = inst.get("patches", [])
    nums = [_pid(p["number"]) for p in patches]
    seen: set[str] = set()
    dupes: set[str] = set()
    for n in nums:
        if n in seen:
            dupes.add(n)
        seen.add(n)
    if dupes:
        errors.append(f"duplicate patch numbers: {sorted(dupes)}")
    return {_pid(p["number"]): p for p in patches}


def validate_routing(inst: dict, patches_by_id: dict[str, dict], errors: list[str]) -> None:
    resources = {r["id"]: set(_pid(p) for p in r["consumedBy"]) for r in inst.get("contextResources", [])}
    rids = [r["id"] for r in inst.get("contextResources", [])]
    if len(rids) != len(set(rids)):
        errors.append("duplicate contextResource ids")
    patch_ctx = {_pid(p["number"]): set(p.get("requiredContext", []) or []) for p in inst.get("patches", [])}

    for rid, consumers in resources.items():
        for pid in consumers:
            if pid not in patches_by_id:
                errors.append(f"resource {rid!r} lists missing patch {pid} in consumedBy")
            elif rid not in patch_ctx.get(pid, set()):
                errors.append(
                    f"routing mismatch: resource {rid!r}.consumedBy contains patch {pid}, "
                    f"but patch {pid}.requiredContext does NOT contain {rid!r}"
                )
    for pid, ctx in patch_ctx.items():
        for rid in ctx:
            if rid not in resources:
                errors.append(f"patch {pid}.requiredContext references unknown resource {rid!r}")
            elif pid not in resources[rid]:
                errors.append(
                    f"routing mismatch: patch {pid}.requiredContext contains {rid!r}, "
                    f"but resource {rid!r}.consumedBy does NOT contain patch {pid}"
                )


def validate_functional_changes(inst: dict, patches_by_id: dict[str, dict], errors: list[str]) -> None:
    fcs = inst.get("functionalChanges", []) or []
    ids = [fc["id"] for fc in fcs]
    if len(ids) != len(set(ids)):
        errors.append("duplicate functionalChange ids")
    for fc in fcs:
        owner = _pid(fc["ownedBy"])
        if owner not in patches_by_id:
            errors.append(f"functionalChange {fc['id']!r} ownedBy missing patch {owner}")
    checklist = inst.get("mergabilityChecklist", {})
    if checklist.get("functionalChangesOwnedByExactlyOnePatch") is True:
        # Every FC having a single ownedBy is structurally enforced by the schema
        # (a single string/int, not a list). This check confirms each owner resolves.
        # The "covers every observable change" half is a human judgement we cannot
        # mechanise; flag if there are no FCs at all when checklist claims true.
        if not fcs:
            print(
                "INFO: functionalChanges is empty; the 'covers every observable change' "
                "half of the checklist remains a human judgement",
                file=sys.stderr,
            )


def validate_dependency_graph(inst: dict, patches_by_id: dict[str, dict], errors: list[str]) -> None:
    dg = inst.get("dependencyGraph", []) or []
    dg_ids = [_pid(d["patch"]) for d in dg]
    if len(dg_ids) != len(set(dg_ids)):
        errors.append("duplicate dependencyGraph entries")
    missing = set(patches_by_id) - set(dg_ids)
    extra = set(dg_ids) - set(patches_by_id)
    if missing:
        errors.append(f"dependencyGraph missing patches: {sorted(missing, key=lambda s: (len(s), s))}")
    if extra:
        errors.append(f"dependencyGraph references unknown patches: {sorted(extra)}")

    # classification consistency: dependencyGraph[i].classification == patches[i].classification
    for d in dg:
        pid = _pid(d["patch"])
        if pid in patches_by_id:
            patch_class = patches_by_id[pid].get("classification")
            if patch_class != d.get("classification"):
                errors.append(
                    f"dependencyGraph patch {pid} classification {d.get('classification')!r} "
                    f"does not match patch.classification {patch_class!r}"
                )

    deps = {_pid(d["patch"]): [_pid(x) for x in d.get("dependsOn", [])] for d in dg}
    for p, ds in deps.items():
        for dep in ds:
            if dep not in patches_by_id:
                errors.append(f"patch {p} depends on unknown patch {dep}")

    # DFS cycle detection
    UNVISITED, VISITING, DONE = 0, 1, 2
    color: dict[str, int] = {n: UNVISITED for n in deps}
    cycle_found = [False]

    def dfs(node: str, stack: list[str]) -> None:
        if cycle_found[0]:
            return
        color[node] = VISITING
        for nxt in deps.get(node, []):
            if nxt not in color:
                continue
            if color[nxt] == VISITING:
                idx = stack.index(nxt) if nxt in stack else 0
                cycle = stack[idx:] + [nxt]
                errors.append(f"dependency cycle: {' -> '.join(cycle)}")
                cycle_found[0] = True
                return
            if color[nxt] == UNVISITED:
                dfs(nxt, stack + [nxt])
                if cycle_found[0]:
                    return
        color[node] = DONE

    for n in list(deps):
        if color.get(n) == UNVISITED:
            dfs(n, [n])
            if cycle_found[0]:
                break


def validate_test_map(inst: dict, patches_by_id: dict[str, dict], errors: list[str]) -> None:
    test_map = inst.get("testMap", []) or []
    test_names: dict[str, dict] = {}
    for tm in test_map:
        if tm["testName"] in test_names:
            errors.append(f"duplicate testMap entry {tm['testName']!r}")
        test_names[tm["testName"]] = tm
        for key in ("stubPatch", "implPatch"):
            pid = _pid(tm[key])
            if pid not in patches_by_id:
                errors.append(f"testMap {tm['testName']!r}.{key} references unknown patch {pid}")

    for p in inst.get("patches", []):
        pid = _pid(p["number"])
        for tn in p.get("testStubsIntroduced") or []:
            tm = test_names.get(tn)
            if tm is None:
                errors.append(f"patch {pid}.testStubsIntroduced has {tn!r} not in testMap")
            elif _pid(tm["stubPatch"]) != pid:
                errors.append(
                    f"patch {pid}.testStubsIntroduced lists {tn!r}, "
                    f"but testMap.stubPatch = {tm['stubPatch']}"
                )
        for tn in p.get("testStubsImplemented") or []:
            tm = test_names.get(tn)
            if tm is None:
                errors.append(f"patch {pid}.testStubsImplemented has {tn!r} not in testMap")
            elif _pid(tm["implPatch"]) != pid:
                errors.append(
                    f"patch {pid}.testStubsImplemented lists {tn!r}, "
                    f"but testMap.implPatch = {tm['implPatch']}"
                )


def validate_specs(inst: dict, errors: list[str]) -> None:
    pant = shutil.which("pant")
    if not pant:
        print("WARN: pant not installed; skipping spec parse validation", file=sys.stderr)
        return
    with tempfile.TemporaryDirectory(prefix="gameplan-specs-") as td:
        tdp = Path(td)
        for p in inst.get("patches", []):
            spec = p.get("spec", "")
            if not spec.strip():
                errors.append(f"patch {p['number']}.spec is empty")
                continue
            f = tdp / f"patch_{p['number']}.pant"
            f.write_text(spec)
            r = subprocess.run([pant, str(f)], capture_output=True, text=True)
            if r.returncode != 0:
                msg = (r.stderr or r.stdout).strip()
                errors.append(f"patch {p['number']}.spec failed pant:\n  {msg}")
        final = inst.get("finalStateSpec", "")
        if not final.strip():
            errors.append("finalStateSpec is empty")
        else:
            f = tdp / "final.pant"
            f.write_text(final)
            r = subprocess.run([pant, str(f)], capture_output=True, text=True)
            if r.returncode != 0:
                msg = (r.stderr or r.stdout).strip()
                errors.append(f"finalStateSpec failed pant:\n  {msg}")


def validate_path_safety(inst: dict, errors: list[str]) -> None:
    """Repo-relative paths only (no .. escapes, no absolute paths)."""
    def check(path: str, where: str) -> None:
        if not path:
            return
        if path.startswith("/"):
            errors.append(f"{where}: absolute path {path!r} not allowed")
        # split on / and check no '..' segment
        if any(seg == ".." for seg in path.split("/")):
            errors.append(f"{where}: path {path!r} escapes the repo root with '..'")

    for rc in inst.get("requiredChanges", []) or []:
        check(rc.get("file", ""), f"requiredChanges[file={rc.get('file')!r}]")
    for p in inst.get("patches", []) or []:
        for fobj in p.get("files", []) or []:
            check(fobj.get("path", ""), f"patch {p.get('number')}.files[path={fobj.get('path')!r}]")
    for r in inst.get("contextResources", []) or []:
        for path in r.get("paths", []) or []:
            # External URLs are permitted for external-reference / reference-doc / paper / etc.
            if path.startswith(("http://", "https://")):
                continue
            check(path, f"contextResources[id={r.get('id')!r}].paths")


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(f"usage: {Path(argv[0]).name} <gameplan.json>", file=sys.stderr)
        return 2
    gp_path = Path(argv[1])
    if not gp_path.exists():
        print(f"file not found: {gp_path}", file=sys.stderr)
        return 2
    try:
        inst = json.loads(gp_path.read_text())
    except json.JSONDecodeError as e:
        print(f"invalid JSON: {e}", file=sys.stderr)
        return 2

    errors: list[str] = []
    validate_schema(inst, errors)
    patches_by_id = validate_patch_numbers(inst, errors)
    validate_routing(inst, patches_by_id, errors)
    validate_functional_changes(inst, patches_by_id, errors)
    validate_dependency_graph(inst, patches_by_id, errors)
    validate_test_map(inst, patches_by_id, errors)
    validate_path_safety(inst, errors)
    validate_specs(inst, errors)

    if errors:
        for e in errors:
            print(f"ERROR: {e}")
        print(f"\nFAIL: {len(errors)} error(s) in {gp_path}", file=sys.stderr)
        return 1
    print(f"PASS: {gp_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
