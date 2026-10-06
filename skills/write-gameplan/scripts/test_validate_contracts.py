"""Exercise the authored-plan validator through its public CLI."""
import copy
import yaml
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from format_yaml import format_yaml
from gameplan_document import load_document

SCRIPTS = Path(__file__).resolve().parent
EXAMPLE = SCRIPTS.parent / "references" / "example.yaml"


class ContractValidation(unittest.TestCase):
    def setUp(self):
        self.plan = load_document(EXAMPLE)

    def validate(self, expected=0, message=None, without_schema=False):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "plan.yaml"
            path.write_text(format_yaml(yaml.safe_dump(self.plan, sort_keys=False, allow_unicode=True)))
            command = [sys.executable, str(SCRIPTS / "validate.py"), str(path)]
            if without_schema:
                command = [
                    sys.executable, "-c",
                    "import sys, runpy; sys.modules['jsonschema'] = None; "
                    "sys.path.insert(0, sys.argv[1]); sys.argv = sys.argv[2:]; "
                    "runpy.run_path(sys.argv[0], run_name='__main__')",
                    str(SCRIPTS), str(SCRIPTS / "validate.py"), str(path),
                ]
            result = subprocess.run(
                command,
                stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=30,
            )
        output = result.stdout + result.stderr
        self.assertEqual(result.returncode, expected, output)
        if message:
            self.assertIn(message, output)
        self.assertNotIn("cannot check malformed structure", output)
        return output

    def test_architecture_requires_section_for_new_authoring(self):
        self.plan.pop("architectureDesign")
        self.validate(1, "architectureDesign", without_schema=True)

    def test_architecture_unresolved_blocks_empty_open_questions(self):
        self.plan["architectureDesign"]["decisions"][0]["resolution"] = {
            "kind": "unresolved", "reason": "Awaiting the engineer's ownership choice"
        }
        self.plan["openQuestions"] = []
        self.validate(1, "is unresolved", without_schema=True)

    def test_architecture_resolution_modes(self):
        for kind in ["engineer_approved", "constrained", "delegated"]:
            with self.subTest(kind=kind):
                self.plan["architectureDesign"]["decisions"][0]["resolution"]["kind"] = kind
                self.validate()

    def test_architecture_malformed_without_jsonschema(self):
        original = copy.deepcopy(self.plan["architectureDesign"])
        for value in [None, [], "design", {"summary": "Design", "decisions": None}]:
            with self.subTest(value=value):
                self.plan["architectureDesign"] = value
                self.validate(1, "architectureDesign", without_schema=True)
        self.plan["architectureDesign"] = original
        self.plan["architectureDesign"]["decisions"][0]["resolution"]["evidence"] = "  "
        self.validate(1, "evidence", without_schema=True)

    def test_architecture_unknown_resolution_identifies_decision(self):
        decision = self.plan["architectureDesign"]["decisions"][0]
        decision["resolution"]["kind"] = "todo"
        for identifier, tag in [("AD-1", "AD-1"), ([], "<invalid-decision-id>")]:
            for without_schema in [False, True]:
                with self.subTest(identifier=identifier, without_schema=without_schema):
                    decision["id"] = identifier
                    self.validate(1, f"architectureDesign {tag} has unknown resolution kind 'todo'",
                                  without_schema=without_schema)

    def test_architecture_duplicate_ids(self):
        decisions = self.plan["architectureDesign"]["decisions"]
        decisions.append(copy.deepcopy(decisions[0]))
        self.validate(1, "duplicate decision ID")

    def test_architecture_routine_plan(self):
        self.plan["architectureDesign"]["decisions"] = []
        self.validate()

    def test_authoring_rejects_version_downgrade_without_jsonschema(self):
        self.plan.pop("architectureDesign")
        for version in [2, 1, 4, None, "3", True]:
            with self.subTest(version=version):
                self.plan["formatVersion"] = version
                self.validate(1, "require formatVersion 3", without_schema=True)

    def legacy_plan(self):
        self.plan.pop("formatVersion")
        self.plan.pop("architectureDesign", None)
        self.plan.pop("orderingConstraints", None)
        self.plan["acceptanceCriteria"] = ["Legacy observable behavior"]
        for change in self.plan["functionalChanges"]:
            change.pop("requiredBy")
            change.pop("verifiedBy")
        deps = {1: [], 2: [1], 3: [1, 2], 4: [3]}
        self.plan["dependencyGraph"] = [
            {"patch": p["number"], "classification": p["classification"],
             "dependsOn": deps[p["number"]]}
            for p in self.plan["patches"]
        ]

    def test_valid_guarantees_and_serialization(self):
        self.plan["orderingConstraints"] = [
            {"before": 1, "after": 4, "reason": "Serialize a generated manifest"}
        ]
        self.validate()

    def test_requirement_cycle(self):
        self.plan["functionalChanges"][4]["requiredBy"] = [1]
        self.validate(1, "dependency cycle")

    def test_mixed_cycle(self):
        self.plan["orderingConstraints"] = [
            {"before": 4, "after": 1, "reason": "Conflicting migration ordering"}
        ]
        self.validate(1, "dependency cycle")

    def test_ordering_cycle(self):
        for fc in self.plan["functionalChanges"]:
            fc["requiredBy"] = []
        self.plan["orderingConstraints"] = [
            {"before": 1, "after": 2, "reason": "First migration"},
            {"before": 2, "after": 1, "reason": "Second migration"},
        ]
        self.validate(1, "dependency cycle")

    def test_self_dependency(self):
        self.plan["functionalChanges"][0]["requiredBy"] = [3]
        self.validate(1, "depends on itself")

    def test_unknown_consumer(self):
        self.plan["functionalChanges"][0]["requiredBy"] = [99]
        self.validate(1, "unknown consumer patch 99")

    def test_mixed_format(self):
        self.plan["dependencyGraph"] = []
        self.validate(1, "Additional properties")

    def test_mixed_format_without_jsonschema(self):
        self.plan["dependencyGraph"] = []
        self.validate(1, "remove dependencyGraph", without_schema=True)

    def test_legacy_graph_and_string_criteria_without_jsonschema(self):
        self.legacy_plan()
        self.validate(1, "require formatVersion 3", without_schema=True)

    def test_legacy_graph_integrity_without_jsonschema(self):
        self.legacy_plan()
        self.plan["dependencyGraph"].pop()
        self.plan["dependencyGraph"][0]["classification"] = "wrong"
        self.plan["dependencyGraph"].append({"patch": 99, "dependsOn": []})
        output = self.validate(1, "dependencyGraph missing patches", without_schema=True)
        self.assertIn("dependencyGraph references unknown patches", output)
        self.assertIn("does not match patch.classification", output)

    def test_legacy_string_criteria_do_not_stop_other_checks(self):
        self.legacy_plan()
        self.plan["functionalChanges"][0]["ownedBy"] = 99
        self.validate(1, "ownedBy missing patch 99", without_schema=True)

    def test_legacy_duplicate_and_cyclic_graph(self):
        self.legacy_plan()
        self.plan["dependencyGraph"][0]["dependsOn"] = [4]
        self.plan["dependencyGraph"].append(copy.deepcopy(self.plan["dependencyGraph"][0]))
        output = self.validate(1, "duplicate dependencyGraph entries", without_schema=True)
        self.assertIn("dependency cycle", output)

    def test_unknown_verification(self):
        self.plan["functionalChanges"][0]["verifiedBy"] = ["Unknown proof"]
        self.validate(1, "unknown test")

    def test_malformed_verification_checks_without_jsonschema(self):
        for field in ["command", "expectation"]:
            for value in [7, None, True, [], {}, "", " \t "]:
                with self.subTest(field=field, value=value):
                    check = {"command": "dune build", "expectation": "Build succeeds"}
                    check[field] = value
                    self.plan["functionalChanges"][0]["verifiedBy"] = [check, "Unknown proof"]
                    output = self.validate(1, "check requires non-empty string command and expectation",
                                           without_schema=True)
                    self.assertIn("unknown test", output)
                    self.assertNotIn("Traceback", output)

    def test_missing_verification_check_fields_without_jsonschema(self):
        for check in [{}, {"command": "dune build"}, {"expectation": "Build succeeds"}]:
            with self.subTest(check=check):
                self.plan["functionalChanges"][0]["verifiedBy"] = [check]
                self.validate(1, "check requires non-empty string command and expectation",
                              without_schema=True)

    def test_verification_owned_by_consumer(self):
        self.plan["testMap"][0]["implPatch"] = 4
        self.validate(1, "verification must be implemented by owner")

    def test_missing_boundary_proof(self):
        self.plan["functionalChanges"][0]["verifiedBy"] = []
        self.validate(1, "consumed guarantee requires verifiedBy")

    def test_verification_file_not_in_producer(self):
        self.plan["testMap"][0]["file"] = "test/other.ml"
        self.validate(1, "verification file must be in owner")

    def test_unknown_acceptance_reference(self):
        self.plan["acceptanceCriteria"][0]["tracesTo"] = ["FC-999"]
        self.validate(1, "tracesTo unknown functional change")

    def test_v3_non_object_criteria_without_jsonschema(self):
        for criterion in ["String criterion", None, 42, True, []]:
            with self.subTest(criterion=criterion):
                self.plan["acceptanceCriteria"] = [criterion]
                self.validate(1, "acceptanceCriteria[0]: formatVersion 3 requires an object",
                              without_schema=True)

    def test_v2_invalid_entries_do_not_stop_object_checks(self):
        self.plan["acceptanceCriteria"][0]["tracesTo"] = ["FC-999"]
        self.plan["acceptanceCriteria"].insert(0, "String criterion")
        output = self.validate(1, "acceptanceCriteria[0]: formatVersion 3 requires an object",
                               without_schema=True)
        self.assertIn("tracesTo unknown functional change", output)

    def test_duplicate_acceptance_id(self):
        self.plan["acceptanceCriteria"].append(copy.deepcopy(self.plan["acceptanceCriteria"][0]))
        self.validate(1, "duplicate acceptance criterion IDs")

    def test_normalized_duplicate_consumers(self):
        self.plan["functionalChanges"][0]["requiredBy"] = [4, "4"]
        self.validate(1, "duplicate requiredBy patch IDs")

    def test_unordered_write_frames(self):
        self.plan["functionalChanges"][-1]["requiredBy"] = []
        self.validate(1, "unordered write conflict")

    def test_shared_paths_are_reported_together_once(self):
        for change in self.plan["functionalChanges"]:
            if change["ownedBy"] == 3:
                change["requiredBy"] = []
        self.plan["patches"][3]["files"].extend([
            {"path": "lib_core/dune", "action": "modify", "description": "Shared manifest"},
            {"path": "lib_core/patch_decision.mli", "action": "modify", "description": "Shared interface"},
            {"path": "lib_core/dune", "action": "modify", "description": "Repeated entry"},
        ])
        self.validate(1, "unordered write conflict between patches 1 and 4: ['lib_core/dune', 'lib_core/patch_decision.mli']")

    def test_transitive_order_allows_multiple_shared_paths(self):
        for change in self.plan["functionalChanges"]:
            if change["ownedBy"] == 3:
                change["requiredBy"] = []
        self.plan["patches"][3]["files"].extend([
            {"path": "lib_core/dune", "action": "modify", "description": "Shared manifest"},
            {"path": "lib_core/patch_decision.mli", "action": "modify", "description": "Shared interface"},
        ])
        self.plan["orderingConstraints"] = [
            {"before": 3, "after": 4, "reason": "Consumer follows implementation"}
        ]
        self.validate()

    def test_unowned_required_change(self):
        self.plan["requiredChanges"][0]["file"] = "unowned.ml"
        self.validate(1, "outside every patch's files")


if __name__ == "__main__":
    unittest.main()
