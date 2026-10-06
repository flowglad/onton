"""Exercise the authored-plan validator through its public CLI."""
import copy
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

SCRIPTS = Path(__file__).resolve().parent
EXAMPLE = SCRIPTS.parent / "references" / "example.json"


class ContractValidation(unittest.TestCase):
    def setUp(self):
        self.plan = json.loads(EXAMPLE.read_text())

    def validate(self, expected=0, message=None):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "plan.json"
            path.write_text(json.dumps(self.plan))
            result = subprocess.run(
                [sys.executable, str(SCRIPTS / "validate.py"), str(path)],
                stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=30,
            )
        output = result.stdout + result.stderr
        self.assertEqual(result.returncode, expected, output)
        if message:
            self.assertIn(message, output)

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

    def test_unknown_verification(self):
        self.plan["functionalChanges"][0]["verifiedBy"] = ["Unknown proof"]
        self.validate(1, "unknown test")

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

    def test_duplicate_acceptance_id(self):
        self.plan["acceptanceCriteria"].append(copy.deepcopy(self.plan["acceptanceCriteria"][0]))
        self.validate(1, "duplicate acceptance criterion IDs")

    def test_normalized_duplicate_consumers(self):
        self.plan["functionalChanges"][0]["requiredBy"] = [4, "4"]
        self.validate(1, "duplicate requiredBy patch IDs")

    def test_unordered_write_frames(self):
        self.plan["functionalChanges"][-1]["requiredBy"] = []
        self.validate(1, "unordered write conflict")

    def test_unowned_required_change(self):
        self.plan["requiredChanges"][0]["file"] = "unowned.ml"
        self.validate(1, "outside every patch's files")


if __name__ == "__main__":
    unittest.main()
