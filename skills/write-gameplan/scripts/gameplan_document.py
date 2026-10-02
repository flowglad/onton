"""Load the same JSON-compatible YAML subset as onton's gameplan reader."""
from __future__ import annotations

import json
import math
import re
from pathlib import Path
from typing import Any

NUMBER = re.compile(r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?\Z")


def load_document(path: Path) -> Any:
    text = path.read_text()
    if path.suffix == ".json":
        def unique_object(pairs):
            result = {}
            for key, value in pairs:
                if key in result:
                    raise ValueError(f"Duplicate JSON key: {key}")
                result[key] = value
            return result

        def finite_float(value):
            number = float(value)
            if not math.isfinite(number):
                raise ValueError("JSON numbers must be finite")
            return number

        def reject_constant(value):
            raise ValueError(f"Non-standard JSON number: {value}")

        return json.loads(text, object_pairs_hook=unique_object,
                          parse_float=finite_float, parse_constant=reject_constant)
    return parse_yaml(text)


def parse_yaml(text: str) -> Any:
    try:
        import yaml
    except ImportError as exc:
        raise ValueError("YAML support requires PyYAML: pip install PyYAML") from exc

    # Inspect events before composing so aliases cannot expand into a graph.
    for event in yaml.parse(text, Loader=yaml.BaseLoader):
        if isinstance(event, yaml.events.DocumentStartEvent) and event.tags:
            raise ValueError("YAML tag directives are not supported")
        if isinstance(event, yaml.events.AliasEvent):
            raise ValueError("YAML aliases are not supported")
        if getattr(event, "anchor", None) or getattr(event, "tag", None):
            raise ValueError("YAML tags and anchors are not supported")
    nodes = list(yaml.compose_all(text, Loader=yaml.BaseLoader))
    if len(nodes) != 1:
        raise ValueError("YAML gameplans must contain exactly one document")

    def decode(node: Any) -> Any:
        if isinstance(node, yaml.nodes.ScalarNode):
            if node.style is not None:
                return node.value
            if node.value in ("", "~", "null"):
                return None
            if node.value == "true":
                return True
            if node.value == "false":
                return False
            if NUMBER.fullmatch(node.value):
                number = json.loads(node.value)
                if isinstance(number, float) and not math.isfinite(number):
                    raise ValueError("YAML numbers must be finite")
                return number
            return node.value
        if isinstance(node, yaml.nodes.SequenceNode):
            return [decode(value) for value in node.value]
        if isinstance(node, yaml.nodes.MappingNode):
            result = {}
            for key_node, value_node in node.value:
                key = decode(key_node)
                if not isinstance(key, str):
                    raise ValueError("YAML mapping keys must be strings")
                if key == "<<":
                    raise ValueError("YAML merge keys are not supported")
                if key in result:
                    raise ValueError(f"Duplicate YAML key: {key}")
                result[key] = decode(value_node)
            return result
        raise ValueError("Expected a YAML value")

    return decode(nodes[0])
