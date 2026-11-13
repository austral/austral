#!/usr/bin/env python3
"""Lists the functions that build an error and that no program executed.

  unexecuted-errors.py COBERTURA_XML

Called by coverage.sh --errors. A function "builds an error" if its body calls
austral_raise. It counts as executed if any instrumented line of its body was
hit. Prints one line per unexecuted function: file, line, name. The last line
gives the totals.
"""
import pathlib
import re
import sys
import xml.etree.ElementTree as ET

ROOT = pathlib.Path(__file__).resolve().parents[2]
DEFINITION = re.compile(r"^\s*(?:let rec|let|and) ([a-z_][A-Za-z0-9_']*)", re.M)

hits: dict[str, dict[int, int]] = {}
for cls in ET.parse(sys.argv[1]).iter("class"):
    lines = hits.setdefault(pathlib.Path(cls.get("filename")).name, {})
    for line in cls.iter("line"):
        number = int(line.get("number"))
        lines[number] = max(lines.get(number, 0), int(line.get("hits")))

total = unexecuted = 0
for path in sorted((ROOT / "lib").glob("*.ml")):
    if path.name == "BuiltInModules.ml":
        continue
    text = path.read_text()
    starts = [(m.start(), m.group(1)) for m in DEFINITION.finditer(text)]
    for (start, name), (end, _) in zip(starts, starts[1:] + [(len(text), "")]):
        body = text[start:end]
        if "austral_raise" not in body or name == "austral_raise":
            continue
        total += 1
        first = text.count("\n", 0, start) + 1
        last = first + body.rstrip().count("\n")
        file_hits = hits.get(path.name, {})
        if not any(file_hits.get(n, 0) > 0 for n in range(first, last + 1)):
            unexecuted += 1
            print(f"lib/{path.name}\t{first}\t{name}")
print(f"error functions {total}, unexecuted {unexecuted}")
