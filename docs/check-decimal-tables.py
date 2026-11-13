#!/usr/bin/env python3
"""Recomputes the tables in docs/decimal.md that can be derived mechanically.

  python3 docs/check-decimal-tables.py

Checks the rounding examples (section 7.2) against Python's decimal module,
and the literal types (section 3.2, second table) against the rule in the text.
Exits 1 when a cell differs.
"""
import pathlib
import re
import sys
from decimal import (
    ROUND_CEILING,
    ROUND_DOWN,
    ROUND_FLOOR,
    ROUND_HALF_DOWN,
    ROUND_HALF_EVEN,
    ROUND_HALF_UP,
    ROUND_UP,
    Decimal,
)

MODES = {
    "NearestEven": ROUND_HALF_EVEN,
    "NearestAwayFromZero": ROUND_HALF_UP,
    "NearestTowardZero": ROUND_HALF_DOWN,
    "AwayFromZero": ROUND_UP,
    "TowardZero": ROUND_DOWN,
    "TowardGreater": ROUND_CEILING,
    "TowardLesser": ROUND_FLOOR,
}
ROUNDING_INPUTS = ["2.345", "2.355", "-2.345", "2.344"]

spec = (pathlib.Path(__file__).parent / "decimal.md").read_text()
mismatches = 0


def between(start: str, end: str) -> str:
    return spec[spec.index(start) : spec.index(end)]


cells = 0
for line in between("### 7.2", "### 7.3").splitlines():
    row = re.match(r"\| `(\w+)` \| (.*) \|$", line)
    if not row or row.group(1) not in MODES:
        continue
    written = [c.strip().replace("−", "-") for c in row.group(2).split("|")]
    for value, cell in zip(ROUNDING_INPUTS, written, strict=True):
        expected = str(Decimal(value).quantize(Decimal("0.01"), rounding=MODES[row.group(1)]))
        cells += 1
        if cell != expected:
            mismatches += 1
            print(f"7.2 {row.group(1)} {value}: document {cell}, computed {expected}")
print(f"rounding cells checked: {cells}")

rows = 0
for literal, precision, scale in re.findall(
    r"\| `([0-9.]+)` \| `Decimal\[(\d+), (\d+)\]` \|",
    between("| リテラル | 型 |", "38桁に収まらないリテラル"),
):
    fraction = literal.split(".")[1] if "." in literal else ""
    coefficient = int(literal.replace(".", ""))
    computed_scale = len(fraction)
    computed_precision = max(len(str(coefficient)), computed_scale)
    rows += 1
    if (computed_precision, computed_scale) != (int(precision), int(scale)):
        mismatches += 1
        print(
            f"3.2 {literal}: document Decimal[{precision}, {scale}], "
            f"computed Decimal[{computed_precision}, {computed_scale}]"
        )
print(f"literal rows checked: {rows}")

if cells != 28 or rows == 0:
    print("a table was not found where expected")
    sys.exit(1)
sys.exit(1 if mismatches else 0)
