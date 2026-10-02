#!/usr/bin/env python3
"""Format gameplan YAML without changing its data, comments, or field order.

Usage: python3 scripts/format_yaml.py gameplans/example.yaml [--width 88]
Prints to stdout by default; --in-place rewrites the source file.
"""
from __future__ import annotations

import argparse
from io import StringIO
import json
from pathlib import Path
import sys
import textwrap

from gameplan_document import NUMBER, parse_yaml


def format_yaml(text: str, width: int = 88) -> str:
    from ruamel.yaml import YAML
    from ruamel.yaml.nodes import ScalarNode
    from ruamel.yaml.resolver import VersionedResolver
    from ruamel.yaml.scalarstring import FoldedScalarString, LiteralScalarString, ScalarString
    from ruamel.yaml.tag import Tag
    import yaml as pyyaml

    if width < 20:
        raise ValueError("width must be at least 20")
    original = parse_yaml(text)
    if not isinstance(original, dict):
        raise ValueError("expected a top-level mapping")

    class GameplanResolver(VersionedResolver):
        """Use onton's scalar types instead of YAML's timestamp/octal rules."""
        def resolve(self, kind, value, implicit):
            if kind is ScalarNode:
                if implicit[0]:
                    if value in ("", "~", "null"):
                        return Tag(suffix="tag:yaml.org,2002:null")
                    if value in ("true", "false"):
                        return Tag(suffix="tag:yaml.org,2002:bool")
                    if NUMBER.fullmatch(value):
                        numeric = "float" if any(c in value for c in ".eE") else "int"
                        return Tag(suffix="tag:yaml.org,2002:" + numeric)
                return self.DEFAULT_SCALAR_TAG
            return super().resolve(kind, value, implicit)

    yaml = YAML()
    yaml.Resolver = GameplanResolver
    yaml.preserve_quotes = True
    # Automatic quoted-scalar wrapping in ruamel 0.18 can split an escape
    # sequence and insert a space into the decoded value. Fold explicitly at
    # safe spaces below; leave quoted escapes and literal strings untouched.
    yaml.width = 2**31
    yaml.indent(mapping=2, sequence=4, offset=2)
    data = yaml.load(text)
    data.fa.set_block_style()

    def readable(value, key=None, indent=0):
        if isinstance(value, dict):
            value.fa.set_block_style()
            for child_key in value:
                value[child_key] = readable(value[child_key], child_key, indent + 2)
        elif isinstance(value, list):
            value.fa.set_block_style()
            for i, child in enumerate(value):
                value[i] = readable(child, indent=indent + 2)
        elif isinstance(value, str):
            if key in ("spec", "finalStateSpec"):
                replacement = LiteralScalarString(value) if "\n" in value else value
            elif any(len(line) + indent > width for line in value.splitlines()):
                # More-indented folded lines preserve line breaks instead of
                # folding them. Control characters also require quoted escapes.
                if any(line[:1].isspace() for line in value.splitlines()) or any(
                    ord(char) < 32 and char != "\n" for char in value
                ):
                    return value
                replacement = FoldedScalarString(value)
                replacement.fold_pos = []
                offset = 0
                for line in value.split("\n"):
                    parts = textwrap.wrap(
                        line, width=max(20, width - indent),
                        break_long_words=False, break_on_hyphens=False,
                        replace_whitespace=False, drop_whitespace=False,
                    )
                    position = offset
                    for part in parts[:-1]:
                        position += len(part)
                        if part.endswith(" "):
                            replacement.fold_pos.append(position - 1)
                        elif position < len(value) and value[position] == " ":
                            replacement.fold_pos.append(position)
                    offset += len(line) + 1
            else:
                return value
            # Scalar header comments belong to the scalar, rather than its map.
            if isinstance(value, ScalarString) and hasattr(value, "comment"):
                if isinstance(replacement, (FoldedScalarString, LiteralScalarString)):
                    replacement.comment = value.comment
            return replacement
        return value

    readable(data)
    stream = StringIO()
    yaml.dump(data, stream)
    rendered = stream.getvalue()

    # Locate real root keys via YAML nodes, never by matching lines inside specs.
    root = pyyaml.compose(rendered, Loader=pyyaml.BaseLoader)
    lines = rendered.splitlines(keepends=True)
    for key_node, _ in reversed(root.value[1:]):
        line = key_node.start_mark.line
        while line > 0 and lines[line - 1].startswith("#"):
            line -= 1
        if line > 0 and lines[line - 1].strip():
            lines.insert(line, "\n")
    result = "".join(lines)
    if json.dumps(parse_yaml(result), sort_keys=True) != json.dumps(original, sort_keys=True):
        raise ValueError("formatting would change the gameplan data")
    return result


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("path", type=Path)
    parser.add_argument("--width", type=int, default=88, help="preferred line width (default: 88)")
    parser.add_argument("--in-place", action="store_true", help="rewrite the source instead of printing")
    args = parser.parse_args(argv)
    try:
        if args.path.suffix not in (".yaml", ".yml"):
            raise ValueError("expected a .yaml or .yml file")
        result = format_yaml(args.path.read_text(), args.width)
        if args.in_place:
            args.path.write_text(result)
        else:
            sys.stdout.write(result)
    except Exception as exc:
        print(f"cannot format {args.path}: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
