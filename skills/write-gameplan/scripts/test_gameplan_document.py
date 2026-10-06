"""Behavior checks for YAML input and the validator's public CLI."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from gameplan_document import load_document
from format_yaml import format_yaml

SCRIPTS = Path(__file__).resolve().parent
REFERENCES = SCRIPTS.parent / "references"


class GameplanDocuments(unittest.TestCase):
    def load(self, text, suffix=".yaml"):
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / ("plan" + suffix)
            path.write_text(text)
            return load_document(path)

    def test_scalar_types_and_blocks(self):
        self.assertEqual(self.load('a: on\nb: off\nc: 2026-10-02\nd: "001"\n'
                                   'e: true\nf: false\ng: null\nh: ~\ni:\n'
                                   'j: []\nk: {}\nspec: |-\n  one\n  two\n'
                                   'large: 9007199254740993\n'),
                         dict(a="on", b="off", c="2026-10-02", d="001", e=True,
                              f=False, g=None, h=None, i=None, j=[], k={},
                              spec="one\ntwo", large=9007199254740993))

    def test_unicode_and_nul(self):
        self.assertEqual(self.load('value: "before\\u0000after café 🌿"'),
                         {'value': 'before\0after café 🌿'})

    def test_rejected_documents(self):
        for text in ['a: 1\na: 2', '{"a": 1, "a": 2}', 'a: {x: 1, x: 2}', 'a: &id [1]\nb: *id',
                     'a: !custom value', 'a: !!str value', 'a: !!map {b: value}',
                     '%TAG !custom! tag:example.com,2026:\n---\na: value',
                     '%TAG !! tag:yaml.org,2002:\n---\na: value',
                     'a: [', '1: value', '[a]: value', '<<: {a: 1}', 'a: 1e999',
                     'a: 1\n---\nb: 2', 'a: 1\n...\ngarbage', '']:
            with self.subTest(text=text), self.assertRaises(Exception):
                self.load(text)

    def test_json_remains_strict(self):
        self.assertEqual(self.load('{"a": 1}', '.json'), {'a': 1})
        with self.assertRaises(json.JSONDecodeError):
            self.load('a: 1', '.json')

    def test_json_rejects_duplicate_keys_and_non_finite_numbers(self):
        for text in ['{"a": 1, "a": 2}', '{"a": {"b": 1, "b": 2}}',
                     '{"a": NaN}', '{"a": Infinity}', '{"a": -Infinity}',
                     '{"a": [1e999]}']:
            with self.subTest(text=text), self.assertRaises(ValueError):
                self.load(text, '.json')
        self.assertEqual(self.load('[{"a": 1}, {"a": 2}]', '.json'),
                         [{'a': 1}, {'a': 2}])

    def test_validator_collects_schema_formatting_and_semantic_errors(self):
        import importlib.util
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / 'plan.yaml'
            text = (REFERENCES / 'example.yaml').read_text()
            # Keep the shape usable by semantic checks but violate the schema's
            # required string type, routing rules, and canonical spacing.
            # Derive the original project name rather than depending on it.
            lines = text.splitlines(keepends=True)
            for i, line in enumerate(lines):
                if line.startswith('projectName:'):
                    lines[i] = 'projectName: 123\n'
                    break
            text = ''.join(lines).replace('requiredContext: []',
                                         'requiredContext: [missing-resource]', 1)
            # Canonicalize the schema/routing defects first, then introduce
            # only the missing top-level blank line as a formatting defect.
            canonical = format_yaml(text)
            text = canonical.replace('\n\nowner:', '\nowner:', 1)
            self.assertNotEqual(text, canonical)
            self.assertEqual(format_yaml(text), canonical)
            path.write_text(text)
            result = subprocess.run([sys.executable, str(SCRIPTS / 'validate.py'), str(path)],
                                    stdin=subprocess.DEVNULL, capture_output=True,
                                    text=True, timeout=30)
            self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
            if importlib.util.find_spec('jsonschema') is not None:
                self.assertIn('schema [projectName]', result.stdout)
            self.assertIn('format_yaml.py', result.stdout)
            self.assertIn('unknown resource', result.stdout)
            self.assertIn('FAIL:', result.stderr)
            self.assertNotIn('Traceback', result.stderr)

    def test_yaml_example_is_valid(self):
        result = subprocess.run([sys.executable, str(SCRIPTS / 'validate.py'),
                                 str(REFERENCES / 'example.yaml')],
                                stdin=subprocess.DEVNULL, capture_output=True,
                                text=True, timeout=30)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_validator_reports_invalid_shape_without_traceback(self):
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / 'plan.yaml'
            for text in ['[]', 'projectName: p\npatches: [broken]']:
                path.write_text(text)
                result = subprocess.run([sys.executable, str(SCRIPTS / 'validate.py'), str(path)],
                                        stdin=subprocess.DEVNULL, capture_output=True,
                                        text=True, timeout=30)
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn('Traceback', result.stderr)


if __name__ == '__main__':
    unittest.main()
