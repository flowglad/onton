"""Formatting behavior through parsed values and the public CLI."""
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from format_yaml import format_yaml
from gameplan_document import parse_yaml

SCRIPTS = Path(__file__).resolve().parent
EXAMPLE = SCRIPTS.parent / 'references' / 'example.yaml'


class FormatYaml(unittest.TestCase):
    def test_spacing_and_wrapping_preserve_values(self):
        prose = ' '.join(['Readable gameplan prose with several words.'] * 8)
        source = 'projectName: demo\nproblemStatement: ' + prose + '\npatches: []\n'
        formatted = format_yaml(source, width=60)
        self.assertEqual(parse_yaml(formatted), parse_yaml(source))
        self.assertIn('\n\nproblemStatement:', formatted)
        self.assertIn('\n\npatches:', formatted)
        self.assertTrue(all(len(line) <= 60 for line in formatted.splitlines()))
        self.assertEqual(format_yaml(formatted, width=60), formatted)

    def test_minimum_width_includes_indentation(self):
        prose = ' '.join(['several short words'] * 10)
        source = 'text: ' + prose + '\nnested:\n  text: ' + prose + '\n'
        formatted = format_yaml(source, width=20)
        self.assertEqual(parse_yaml(formatted), parse_yaml(source))
        self.assertTrue(all(len(line) <= 20 for line in formatted.splitlines()))
        self.assertEqual(format_yaml(formatted, width=20), formatted)

    def test_comments_specs_and_types_survive(self):
        source = '''# A plan
projectName: demo # identity
# Review this summary
solutionSummary: |- # rationale
  Long prose with enough words to wrap across several short lines in this readable gameplan.
  A second paragraph with  two spaces, which must survive formatting.
spec: |- # formal contract
  module EXAMPLE
  # Preserve indentation and this very long formal specification line without wrapping it.
    value => Value.
values: [001, on, off, Null, 2026-10-02, "true", "001", true, false, null, 1e3]
'''
        formatted = format_yaml(source, width=60)
        self.assertEqual(parse_yaml(formatted), parse_yaml(source))
        for comment in ['# A plan', '# identity', '# Review this summary', '# rationale', '# formal contract']:
            self.assertIn(comment, formatted)
        self.assertIn('  # Preserve indentation and this very long formal specification line without wrapping it.', formatted)
        self.assertEqual(format_yaml(formatted, width=60), formatted)

    def test_whitespace_and_unicode_roundtrip(self):
        for value in [' leading space ' + 'word ' * 50,
                      'word  ' * 50, 'line\n\n' + 'word ' * 50 + '\n\n',
                      'café 🌿 ' * 50, 'before\0after ' * 30]:
            import json
            with self.subTest(value=value):
                source = 'text: ' + json.dumps(value, ensure_ascii=False) + '\n'
                formatted = format_yaml(source, width=60)
                self.assertEqual(parse_yaml(formatted), parse_yaml(source))
                self.assertEqual(format_yaml(formatted, width=60), formatted)

    def test_example_is_stable(self):
        text = EXAMPLE.read_text()
        self.assertEqual(format_yaml(text), text)

    def test_validator_requires_formatting_without_rewriting(self):
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / 'plan.yaml'
            original = EXAMPLE.read_text()
            unformatted = original.replace('\n\nowner:', '\nowner:', 1)
            path.write_text(unformatted)
            command = [sys.executable, str(SCRIPTS / 'validate.py'), str(path)]
            rejected = subprocess.run(command, stdin=subprocess.DEVNULL,
                                      capture_output=True, text=True, timeout=30)
            self.assertEqual(rejected.returncode, 1, rejected.stdout + rejected.stderr)
            self.assertIn('format_yaml.py', rejected.stdout)
            self.assertIn('--in-place', rejected.stdout)
            self.assertEqual(path.read_text(), unformatted)
            for width in (88, 60):
                path.write_text(format_yaml(unformatted, width))
                accepted = subprocess.run(command + ['--width', str(width)],
                                          stdin=subprocess.DEVNULL, capture_output=True,
                                          text=True, timeout=30)
                self.assertEqual(accepted.returncode, 0, accepted.stdout + accepted.stderr)

    def test_cli_prints_by_default_and_rewrites_only_when_requested(self):
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / 'plan.yml'
            source = 'projectName: demo\npatches: []\n'
            path.write_text(source)
            command = [sys.executable, str(SCRIPTS / 'format_yaml.py'), str(path)]
            result = subprocess.run(command, stdin=subprocess.DEVNULL,
                                    capture_output=True, text=True, timeout=30)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(path.read_text(), source)
            result2 = subprocess.run(command + ['--in-place'], stdin=subprocess.DEVNULL,
                                     capture_output=True, text=True, timeout=30)
            self.assertEqual(result2.returncode, 0, result2.stderr)
            self.assertEqual(path.read_text(), result.stdout)
            path.write_text('a: 1\na: 2\n')
            failed = subprocess.run(command + ['--in-place'], stdin=subprocess.DEVNULL,
                                    capture_output=True, text=True, timeout=30)
            self.assertEqual(failed.returncode, 1)
            self.assertEqual(path.read_text(), 'a: 1\na: 2\n')
            self.assertNotIn('Traceback', failed.stderr)


if __name__ == '__main__':
    unittest.main()
