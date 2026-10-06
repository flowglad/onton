#!/usr/bin/env python3
"""Validate a YAML or JSON gameplan.

Runs every check enumerated in SKILL.md's Verification section that can be
mechanised: canonical YAML formatting, JSON Schema shape, Pantagruel spec parsing, context-routing
reciprocity, functional-change ownership, dependency-graph integrity,
testMap consistency, and reachability-trace integrity (created-node /
creating-patch ordering and leaf-in-owning-patch-frame).

Usage:
    python3 scripts/validate.py path/to/gameplan.yaml

Exit codes:
    0 — all checks pass
    1 — one or more checks failed
    2 — usage / I/O error

Python dependencies: scripts/requirements.txt (PyYAML is required for YAML).

Soft dependencies:
    jsonschema  — enables shape validation; without it, only semantic checks run
    pant        — enables Pantagruel spec parsing; without it, specs are skipped
"""
from __future__ import annotations

import argparse
import json
import shlex
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any

from gameplan_document import load_document

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
    tests = {test["testName"]: test for test in inst.get("testMap", [])}
    for fc in fcs:
        owner = _pid(fc["ownedBy"])
        consumers = [_pid(p) for p in fc.get("requiredBy", [])]
        if len(consumers) != len(set(consumers)):
            errors.append(f"{fc['id']}: duplicate requiredBy patch IDs")
        proofs = fc.get("verifiedBy", [])
        if consumers and not proofs:
            errors.append(f"{fc['id']}: consumed guarantee requires verifiedBy evidence")
        for proof in proofs:
            if isinstance(proof, dict):
                if not proof.get("command", "").strip() or not proof.get("expectation", "").strip():
                    errors.append(f"{fc['id']}: check requires command and expectation")
                continue
            test = tests.get(proof)
            if test is None:
                errors.append(f"{fc['id']}: verifiedBy references unknown test {proof!r}")
            elif _pid(test["implPatch"]) != owner:
                errors.append(f"{fc['id']}: verification must be implemented by owner patch {owner}")
            elif owner in patches_by_id and test['file'] not in {
                f['path'] for f in patches_by_id[owner].get('files', [])
            }:
                errors.append(f"{fc['id']}: verification file must be in owner patch {owner}'s files")
    criteria = [criterion for criterion in inst.get("acceptanceCriteria", [])
                if isinstance(criterion, dict)]
    criterion_ids = [criterion["id"] for criterion in criteria]
    if len(criterion_ids) != len(set(criterion_ids)):
        errors.append("duplicate acceptance criterion IDs")
    for criterion in criteria:
        for reference in criterion.get("tracesTo", []):
            if reference not in ids:
                errors.append(f"{criterion['id']}: tracesTo unknown functional change {reference}")
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


def derived_dependencies(inst: dict) -> dict[str, list[str]]:
    """Planning-time projection; Onton independently derives the execution graph."""
    if inst.get("formatVersion") != 2:
        return {_pid(d["patch"]): [_pid(x) for x in d.get("dependsOn", [])]
                for d in inst.get("dependencyGraph", []) or []}
    deps: dict[str, set[str]] = {_pid(p["number"]): set() for p in inst.get("patches", [])}
    for fc in inst.get("functionalChanges", []):
        for consumer in fc.get("requiredBy", []):
            deps.setdefault(_pid(consumer), set()).add(_pid(fc["ownedBy"]))
    for constraint in inst.get("orderingConstraints", []):
        deps.setdefault(_pid(constraint["after"]), set()).add(_pid(constraint["before"]))
    return {p: sorted(values) for p, values in deps.items()}


def validate_dependency_graph(inst: dict, patches_by_id: dict[str, dict], errors: list[str]) -> None:
    if inst.get("formatVersion") == 2:
        if "dependencyGraph" in inst:
            errors.append("formatVersion 2 derives dependencies; remove dependencyGraph")
    else:
        graph = inst.get("dependencyGraph", []) or []
        ids = [_pid(entry["patch"]) for entry in graph]
        if len(ids) != len(set(ids)):
            errors.append("duplicate dependencyGraph entries")
        missing = set(patches_by_id) - set(ids)
        extra = set(ids) - set(patches_by_id)
        if missing:
            errors.append(f"dependencyGraph missing patches: {sorted(missing, key=lambda s: (len(s), s))}")
        if extra:
            errors.append(f"dependencyGraph references unknown patches: {sorted(extra)}")
        for entry in graph:
            pid = _pid(entry["patch"])
            if pid in patches_by_id and entry.get("classification") != patches_by_id[pid].get("classification"):
                errors.append(f"dependencyGraph patch {pid} classification {entry.get('classification')!r} "
                              f"does not match patch.classification {patches_by_id[pid].get('classification')!r}")
    deps = derived_dependencies(inst)
    for p, ds in deps.items():
        if p not in patches_by_id:
            errors.append(f"derived dependency references unknown consumer patch {p}")
        for dep in ds:
            if dep not in patches_by_id:
                errors.append(f"patch {p} depends on unknown patch {dep}")
            if dep == p:
                errors.append(f"patch {p} depends on itself")
    for constraint in inst.get("orderingConstraints", []):
        if not constraint.get("reason", "").strip():
            errors.append("ordering constraint requires a substantive reason")

    # DFS cycle detection: cover every node referenced by an edge (not just
    # dg keys) and report every distinct cycle.
    UNVISITED, VISITING, DONE = 0, 1, 2
    all_nodes = set(deps) | {d for ds in deps.values() for d in ds}
    color: dict[str, int] = {n: UNVISITED for n in all_nodes}
    seen_cycles: set[tuple[str, ...]] = set()

    def canon(cycle: list[str]) -> tuple[str, ...]:
        k = min(range(len(cycle)), key=lambda i: cycle[i:] + cycle[:i])
        return tuple(cycle[k:] + cycle[:k])

    def dfs(node: str, stack: list[str]) -> None:
        color[node] = VISITING
        for nxt in deps.get(node, []):
            if color[nxt] == VISITING:
                # VISITING is only set by an active dfs frame, so nxt is on stack.
                idx = stack.index(nxt)
                cycle_nodes = stack[idx:]
                key = canon(cycle_nodes)
                if key not in seen_cycles:
                    seen_cycles.add(key)
                    errors.append(f"dependency cycle: {' -> '.join(cycle_nodes + [nxt])}")
            elif color[nxt] == UNVISITED:
                dfs(nxt, stack + [nxt])
        color[node] = DONE

    for n in sorted(all_nodes):
        if color[n] == UNVISITED:
            dfs(n, [n])


def _transitive_deps(inst: dict):
    """Return reach(p) -> set of patch ids p transitively depends on."""
    direct = derived_dependencies(inst)
    memo: dict[str, set[str]] = {}

    def reach(p: str) -> set[str]:
        if p in memo:
            return memo[p]
        seen: set[str] = set()
        stack = list(direct.get(p, []))
        while stack:
            q = stack.pop()
            if q in seen:
                continue
            seen.add(q)
            stack.extend(direct.get(q, []))
        memo[p] = seen
        return seen

    return reach


def validate_reachability_traces(inst: dict, patches_by_id: dict[str, dict], errors: list[str]) -> None:
    traces = inst.get("reachabilityTraces", []) or []
    fcs_by_id = {fc["id"]: fc for fc in inst.get("functionalChanges", []) or []}

    created_by: dict[str, list[str]] = {}
    files_by_patch: dict[str, set[str]] = {}
    for p in inst.get("patches", []) or []:
        pid = _pid(p["number"])
        frame = files_by_patch.setdefault(pid, set())
        for f in p.get("files", []) or []:
            path = f.get("path", "")
            frame.add(path)
            if f.get("action") == "create":
                created_by.setdefault(path, []).append(pid)

    reach = _transitive_deps(inst)

    def check_node(node: dict, where: str, owner: str) -> None:
        path = node.get("file", "")
        status = node.get("status")
        if status == "created":
            creators = created_by.get(path, [])
            if not creators:
                errors.append(f"{where}: node marks {path!r} 'created', but no patch creates it (action:create)")
            else:
                for q in creators:
                    if q != owner and q not in reach(owner):
                        errors.append(
                            f"{where}: trace owned by patch {owner} traverses {path!r} created by patch {q}, "
                            f"but {owner} does not (transitively) depend on {q}"
                        )

    for i, tr in enumerate(traces):
        where = f"reachabilityTraces[{i}] ({tr.get('observable')!r})"
        owner = _pid(tr["ownedBy"])
        if owner not in patches_by_id:
            errors.append(f"{where}: ownedBy references unknown patch {owner}")

        traces_to = tr.get("tracesTo")
        if traces_to:
            fc = fcs_by_id.get(traces_to)
            if fc is None:
                errors.append(f"{where}: tracesTo references unknown functionalChange {traces_to!r}")
            elif _pid(fc["ownedBy"]) != owner:
                errors.append(
                    f"{where}: tracesTo {traces_to!r} is ownedBy patch {fc['ownedBy']}, "
                    f"but the trace is ownedBy patch {owner} — they must match"
                )

        path = tr.get("path", []) or []
        for j, node in enumerate(path):
            check_node(node, f"{where}.path[{j}]", owner)
        for j, node in enumerate(tr.get("testPath") or []):
            check_node(node, f"{where}.testPath[{j}]", owner)

        # Efficacy: the owning patch must edit at least one node on the path —
        # its change lands on the live path, not on a symbol off it. (The edited
        # node is the leaf for a new-feature exposure, or the entry for a wire-in.)
        if path and owner in patches_by_id:
            frame = files_by_patch.get(owner, set())
            if not any(node.get("file", "") in frame for node in path):
                errors.append(
                    f"{where}: owning patch {owner} edits no node on this path "
                    f"(files {sorted(frame)}) — its edit is not on the traced path "
                    f"(wrong-lever / dead-surface defect)"
                )


def validate_write_frames(inst: dict, patches_by_id: dict[str, dict], errors: list[str]) -> None:
    if inst.get("formatVersion") != 2:
        return
    reach = _transitive_deps(inst)
    writers: dict[str, list[str]] = {}
    for pid, patch in patches_by_id.items():
        for path in {entry['path'] for entry in patch.get('files', [])}:
            writers.setdefault(path, []).append(pid)
    overlaps: dict[tuple[str, str], list[str]] = {}
    for path, ids in writers.items():
        for i, a in enumerate(ids):
            for b in ids[i + 1:]:
                overlaps.setdefault((a, b), []).append(path)
    positions = {pid: i for i, pid in enumerate(patches_by_id)}
    for a, b in sorted(overlaps, key=lambda pair: (positions[pair[0]], positions[pair[1]])):
        if a not in reach(b) and b not in reach(a):
            errors.append(f"unordered write conflict between patches {a} and {b}: {sorted(overlaps[a, b])}")
    for change in inst.get('requiredChanges', []):
        if change['file'] not in writers:
            errors.append(f"required change {change['file']!r} is outside every patch's files")


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
    for i, tr in enumerate(inst.get("reachabilityTraces", []) or []):
        for seam in ("path", "testPath"):
            for node in tr.get(seam) or []:
                check(node.get("file", ""), f"reachabilityTraces[{i}].{seam}[file={node.get('file')!r}]")


def validate_formatting(path: Path, width: int, errors: list[str]) -> None:
    if path.suffix not in (".yaml", ".yml"):
        return
    try:
        from format_yaml import format_yaml
        text = path.read_text()
        formatted = format_yaml(text, width)
    except Exception as exc:
        errors.append(f"formatting check failed: {exc}")
        return
    if text != formatted:
        command = ["python3", str(Path(__file__).with_name("format_yaml.py")),
                   str(path), "--in-place"]
        if width != 88:
            command += ["--width", str(width)]
        errors.append(f"YAML formatting differs from the canonical output; run: {shlex.join(command)}")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("path", type=Path, help="YAML or JSON gameplan")
    parser.add_argument("--width", type=int, default=88,
                        help="preferred YAML line width (default: 88)")
    args = parser.parse_args(argv[1:])
    if args.width < 20:
        parser.error("width must be at least 20")
    gp_path = args.path
    if not gp_path.exists():
        print(f"file not found: {gp_path}", file=sys.stderr)
        return 2
    try:
        inst = load_document(gp_path)
    except Exception as e:
        print(f"invalid gameplan: {e}", file=sys.stderr)
        return 2

    if not isinstance(inst, dict):
        print("invalid gameplan: expected a mapping", file=sys.stderr)
        return 2

    errors: list[str] = []
    validate_schema(inst, errors)
    validate_formatting(gp_path, args.width, errors)
    # Schema errors need not prevent independent checks, but malformed shapes
    # must become diagnostics even when optional jsonschema is unavailable.
    try:
        patches_by_id = validate_patch_numbers(inst, errors)
    except (KeyError, TypeError, AttributeError) as exc:
        errors.append(f"patch numbers: cannot check malformed structure ({exc})")
        patches_by_id = {}
    for check, arguments in [
        (validate_routing, (inst, patches_by_id, errors)),
        (validate_functional_changes, (inst, patches_by_id, errors)),
        (validate_dependency_graph, (inst, patches_by_id, errors)),
        (validate_test_map, (inst, patches_by_id, errors)),
        (validate_write_frames, (inst, patches_by_id, errors)),
        (validate_reachability_traces, (inst, patches_by_id, errors)),
        (validate_path_safety, (inst, errors)),
        (validate_specs, (inst, errors)),
    ]:
        try:
            check(*arguments)
        except (KeyError, TypeError, AttributeError) as exc:
            errors.append(f"{check.__name__}: cannot check malformed structure ({exc})")

    if errors:
        for e in errors:
            print(f"ERROR: {e}")
        print(f"\nFAIL: {len(errors)} error(s) in {gp_path}", file=sys.stderr)
        return 1
    print(f"PASS: {gp_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
