#!/usr/bin/env python3
"""Lists the exported functions of a stage module through which an exception can leave the stage.

  escape-check.py ROOT Module

Same name-based upper estimate as tools/refactoring/raising-functions.py. An
exported function passes if it cannot raise, or if its body calls `catch`.
"""
import pathlib, re, sys

root, module = pathlib.Path(sys.argv[1]), sys.argv[2]
RAISE = re.compile(r'austral_raise|(^|[^_a-zA-Z])err [("]|internal_err|Errors\.[a-z_]+|failwith|(^|[^_a-zA-Z])raise |assert false', re.M)
DEFINITION = re.compile(r"^(?:let rec|let|and) ([a-z_][A-Za-z0-9_']*)[^=\n]*=", re.M)

bodies = {}
for path in sorted([*(root / "lib").glob("*.ml"), *(root / "cli").glob("*.ml")]):
    if path.name == "BuiltInModules.ml":
        continue
    text = path.read_text()
    starts = [(m.start(), m.group(1)) for m in DEFINITION.finditer(text)]
    for (start, name), (end, _) in zip(starts, starts[1:] + [(len(text), "")]):
        bodies[(path.name, name)] = text[start:end]

# A function that catches is a boundary: it does not pass the exception on.
catches = {key for key, body in bodies.items() if re.search(r"\bcatch\b", body) and key[1] != "catch"}
raising = {key for key, body in bodies.items() if RAISE.search(body)} - catches
changed = True
while changed:
    changed = False
    pattern = re.compile(r"\b(" + "|".join(sorted({re.escape(n) for _, n in raising}, key=len, reverse=True)) + r")\b")
    for key, body in bodies.items():
        if key in raising or key in catches:
            continue
        if pattern.search(body.split("=", 1)[1] if "=" in body else body):
            raising.add(key)
            changed = True

exported = re.findall(r"^val ([a-z_][A-Za-z0-9_']*)", (root / "lib" / f"{module}.mli").read_text(), re.M)
escaping = [name for name in exported if (f"{module}.ml", name) in raising]
print(f"{module}: exported {exported}; an exception can leave through {escaping or 'none'}")
sys.exit(1 if escaping else 0)
