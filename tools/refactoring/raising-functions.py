#!/usr/bin/env python3
"""Estimates how many top-level functions of the compiler can raise.

  python3 raising-functions.py

A function "can raise" if its body contains a raise site, or mentions the name
of a function that can raise. Names are matched as words across all files, so
two functions with the same name in different modules are merged. The result
is an upper estimate. It answers one planning question: if every function that
can fail returned a result value, how many signatures would change?
"""
import pathlib
import re

ROOT = pathlib.Path(__file__).resolve().parents[2]
RAISE = re.compile(r'austral_raise|(^|[^_a-zA-Z])err [("]|internal_err|Errors\.[a-z_]+|failwith|(^|[^_a-zA-Z])raise |assert false', re.M)
DEFINITION = re.compile(r"^(?:let rec|let|and) ([a-z_][A-Za-z0-9_']*)[^=\n]*=", re.M)

bodies: dict[tuple[str, str], str] = {}
for path in sorted((ROOT / "lib").glob("*.ml")):
    if path.name == "BuiltInModules.ml":
        continue
    text = path.read_text()
    starts = [(m.start(), m.group(1)) for m in DEFINITION.finditer(text)]
    for (start, name), (end, _) in zip(starts, starts[1:] + [(len(text), "")]):
        bodies[(path.name, name)] = text[start:end]

raising = {key for key, body in bodies.items() if RAISE.search(body)}
direct = len(raising)
changed = True
while changed:
    changed = False
    names = {name for _, name in raising}
    pattern = re.compile(r"\b(" + "|".join(sorted(map(re.escape, names), key=len, reverse=True)) + r")\b")
    for key, body in bodies.items():
        if key in raising:
            continue
        # Skip the definition line itself, which names the function being defined.
        if pattern.search(body.split("=", 1)[1] if "=" in body else body):
            raising.add(key)
            changed = True

print(f"top-level functions: {len(bodies)}")
print(f"with a raise site in the body: {direct}")
print(f"can raise, directly or through a call: {len(raising)}")
print("per file (can raise / functions):")
per_file: dict[str, list[int]] = {}
for (file, _name) in bodies:
    per_file.setdefault(file, [0, 0])[1] += 1
for (file, _name) in raising:
    per_file[file][0] += 1
for file, (can, total) in sorted(per_file.items(), key=lambda kv: -kv[1][0])[:12]:
    print(f"  {file}\t{can}\t{total}")
