#!/usr/bin/env python3
"""Lists the pieces of error messages that no test expects.

  unpinned-messages.py            compare with expected/unpinned-messages.txt
  unpinned-messages.py --record   rewrite it

An error to the user is a constructor of one of the types in lib/*Errors.ml,
and its wording is in that module's `render`. The pieces of a message are the
string literals it is built from: every literal in the arm of `render` for the
constructor, and every literal in the functions `render` calls for the words
of an enumeration. A piece is pinned if it appears in the expected output of
some test: the message in an `austral-stderr.txt` of the end-to-end suite, or
an `expected.txt` of the command-line tests.

A piece on the recorded list is one that no program can reach, or that a known
bug hides. docs/refactoring-plan.md (R3) says which. A new piece that is not on
the list fails the comparison: write a test for it.

R11 made seven pieces visible that programs can reach and that no test expects
yet: the four messages of the lexer in lib/ParseErrors.ml, and the words
"disjunction", "borrow (mutably)" and "borrowed (read-only)". They are on the
list until their tests exist.
"""
import json
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[1]
RECORDED = HERE / "expected" / "unpinned-messages.txt"
LITERAL = re.compile(r'"((?:[^"\\]|\\.)*)"')
# A definition at the top of the file, or directly inside a module.
DEFINITION = re.compile(r"^ {0,2}(?:let rec|let|and) ([a-z_][A-Za-z0-9_']*)", re.M)
# An arm of `render`: the constructor it is for.
ARM = re.compile(r"^  \| ([A-Z][A-Za-z0-9_]*)", re.M)


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


def without_comments(text: str) -> str:
    """The text with comments blanked out. Literals keep their place."""
    chars, depth, i = list(text), 0, 0
    while i < len(text):
        if text.startswith("(*", i):
            depth += 1
            chars[i] = chars[i + 1] = " "
            i += 2
        elif depth and text.startswith("*)", i):
            depth -= 1
            chars[i] = chars[i + 1] = " "
            i += 2
        elif depth:
            if text[i] != "\n":
                chars[i] = " "
            i += 1
        elif text[i] == '"':
            i = LITERAL.match(text, i).end()
        else:
            i += 1
    return "".join(chars)


def sections(text: str, pattern: re.Pattern):
    """The text cut at each match of the pattern: (name the match captured, text up to the next match)."""
    starts = [(m.start(), m.group(1)) for m in pattern.finditer(text)]
    for (start, name), (end, _) in zip(starts, starts[1:] + [(len(text), "")]):
        yield name, text[start:end]


def error_module_pieces(path: pathlib.Path):
    """Every literal of a module of errors, under the constructor or the function it is in."""
    text = without_comments(path.read_text())
    for name, body in sections(text, DEFINITION):
        parts = sections(body, ARM) if name == "render" else [(name, body)]
        for part_name, part in parts:
            for literal in LITERAL.findall(part):
                if len(literal.strip()) >= 4:
                    yield str(path.relative_to(ROOT)), part_name, unescape(literal)


def pieces():
    for path in sorted((ROOT / "lib").glob("*Errors.ml")):
        yield from error_module_pieces(path)


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
