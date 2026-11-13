#!/usr/bin/env python3
"""Lists the pieces of error messages that no test expects.

  unpinned-messages.py            compare with expected/unpinned-messages.txt
  unpinned-messages.py --record   rewrite it

A place that raises an error to the user is a call of `austral_raise`, or of
`err` with a literal message (`internal_err` marks a state the code believes
cannot happen, and is left out). The pieces of its message are the string
literals it is built from. A piece is pinned if it appears in the expected
output of some test: the message in an `austral-stderr.txt` of the end-to-end
suite, or an `expected.txt` of the command-line tests.

Where the place is in a small definition, every literal of the definition
counts as a piece. That covers the functions that choose between wordings
before they raise. In a large definition, only the literals inside the raising
expression count.

A piece on the recorded list is one that no program can reach, or that a known
bug hides. docs/refactoring-plan.md (R3) says which. A new piece that is not on
the list fails the comparison: write a test for it.
"""
import json
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[1]
RECORDED = HERE / "expected" / "unpinned-messages.txt"
PLACE = re.compile(r"""austral_raise\b|(?<![_a-zA-Z])err [("]""")
LITERAL = re.compile(r'"((?:[^"\\]|\\.)*)"')
# A definition at the top of the file, or directly inside a module.
DEFINITION = re.compile(r"^ {0,2}(?:let rec|let|and) ([a-z_][A-Za-z0-9_']*)", re.M)
SMALL = 45  # lines


def expected_texts() -> str:
    texts = []
    for f in ROOT.glob("test-programs/suites/*/*/austral-stderr.txt"):
        raw = f.read_text()
        try:
            error = json.loads(raw)
            texts.append("".join(part.get("content", "\n") for part in error["text"]))
        except (ValueError, KeyError, TypeError):
            texts.append(raw)
    for f in ROOT.glob("test-programs/cli/*/expected.txt"):
        texts.append(f.read_text())
    return "\x00".join(texts)


def unescape(literal: str) -> str:
    return literal.encode().decode("unicode_escape") if "\\" in literal else literal


def raising_expression(body: str, start: int) -> str:
    """The text of the call that starts at `start`: up to where its brackets close."""
    depth, opened = 0, False
    i = start
    while i < len(body):
        c = body[i]
        if c == '"':
            i = LITERAL.match(body, i).end()
            if not opened and depth == 0:
                return body[start:i]  # err "..."
            continue
        if c in "([":
            depth, opened = depth + 1, True
        elif c in ")]":
            depth -= 1
            if depth <= 0:
                return body[start : i + 1]
        i += 1
    return body[start:]


def pieces():
    for path in sorted([*(ROOT / "lib").glob("*.ml"), *(ROOT / "cli").glob("*.ml")]):
        if path.name == "BuiltInModules.ml":
            continue
        text = path.read_text()
        starts = [(m.start(), m.group(1)) for m in DEFINITION.finditer(text)]
        for (start, name), (end, _) in zip(starts, starts[1:] + [(len(text), "")]):
            body = text[start:end]
            if name in ("austral_raise", "err") or not PLACE.search(body):
                continue
            if body.count("\n") <= SMALL:
                literals = LITERAL.findall(body)
            else:
                literals = [l for m in PLACE.finditer(body) for l in LITERAL.findall(raising_expression(body, m.start()))]
            for literal in literals:
                if len(literal.strip()) >= 4:
                    yield str(path.relative_to(ROOT)), name, unescape(literal)


def main() -> int:
    expected = expected_texts()
    all_pieces = sorted(set(pieces()))
    unpinned = [f"{file}\t{name}\t{json.dumps(piece, ensure_ascii=False)}" for file, name, piece in all_pieces if piece not in expected]
    report = "\n".join(unpinned) + "\n"
    summary = f"message pieces {len(all_pieces)}, unpinned {len(unpinned)}"
    if "--record" in sys.argv:
        RECORDED.write_text(report)
        print(f"recorded unpinned-messages.txt: {summary}")
        return 0
    recorded = set(RECORDED.read_text().splitlines()) if RECORDED.exists() else set()
    new = [row for row in unpinned if row not in recorded]
    gone = sorted(recorded - set(unpinned))
    for row in new:
        print(f"NEW   {row}")
    for row in gone:
        print(f"GONE  {row}")
    if new:
        print(f"{len(new)} message pieces have no test. Write one, or record why none can exist.")
        return 1
    if gone:
        print("pieces on the recorded list are now pinned or deleted; run with --record")
        return 1
    print(f"match unpinned-messages.txt: {summary}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
