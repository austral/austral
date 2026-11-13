#!/usr/bin/env python3
"""Lists the places that raise an error to the user and that no program executed.

  unexecuted-errors.py COBERTURA_XML

Called by coverage.sh --errors. A place is a line that calls `austral_raise`
or `err` with a literal message. `internal_err` is left out: it marks a state
the code believes cannot happen.

A place counts as executed if its own line was hit. The coverage tool puts a
point on the expression that raises, so a hit elsewhere in the same function
says nothing about the place. Where the line carries no point, the nearest
earlier point in the same definition decides.

Prints one line per unexecuted place: file, line, the enclosing definition and
the start of the message. The last line gives the totals.
"""
import pathlib
import re
import sys
import xml.etree.ElementTree as ET

ROOT = pathlib.Path(__file__).resolve().parents[2]
DEFINITION = re.compile(r"^\s*(?:let rec|let|and) ([a-z_][A-Za-z0-9_']*)", re.M)
PLACE = re.compile(r"""austral_raise\b|(?<![_a-zA-Z])err [("]""")
LITERAL = re.compile(r'"((?:[^"\\]|\\.)*)"')

hits: dict[str, dict[int, int]] = {}
for cls in ET.parse(sys.argv[1]).iter("class"):
    lines = hits.setdefault(cls.get("filename"), {})
    for line in cls.iter("line"):
        number = int(line.get("number"))
        lines[number] = max(lines.get(number, 0), int(line.get("hits")))


def without_comments(text: str) -> list[str]:
    """The lines of the file, with comments blanked out."""
    chars, depth, i = list(text), 0, 0
    while i < len(chars):
        if text.startswith("(*", i):
            depth += 1
            chars[i] = chars[i + 1] = " "
            i += 2
        elif depth and text.startswith("*)", i):
            depth -= 1
            chars[i] = chars[i + 1] = " "
            i += 2
        elif text[i] == '"' and not depth:
            i += 1
            while text[i] != '"':
                i += 2 if text[i] == "\\" else 1
            i += 1
        else:
            if depth and chars[i] != "\n":
                chars[i] = " "
            i += 1
    return "".join(chars).split("\n")


total = unexecuted = 0
sources = [*(ROOT / "lib").glob("*.ml"), *(ROOT / "cli").glob("*.ml")]
for path in sorted(sources):
    if path.name == "BuiltInModules.ml":
        continue
    name = str(path.relative_to(ROOT))
    text = path.read_text()
    raw = text.split("\n")
    file_hits = hits.get(name, {})
    starts = [(text.count("\n", 0, m.start()) + 1, m.group(1)) for m in DEFINITION.finditer(text)]
    for number, line in enumerate(without_comments(text), start=1):
        if not PLACE.search(line) or re.match(r"\s*let (rec )?(austral_raise|err)\b", line):
            continue
        total += 1
        enclosing = [(first, definition) for first, definition in starts if first <= number]
        first, definition = enclosing[-1] if enclosing else (1, "?")
        # The point on the place's own line, or the nearest earlier one in the definition.
        point = next((n for n in range(number, first - 1, -1) if n in file_hits), None)
        if point is not None and file_hits[point] > 0:
            continue
        unexecuted += 1
        message = LITERAL.search("\n".join(raw[number - 1 : number + 6]))
        print(f"{name}\t{number}\t{definition}\t{message.group(1)[:60] if message else ''}")
print(f"error places {total}, unexecuted {unexecuted}")
