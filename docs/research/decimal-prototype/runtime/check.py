#!/usr/bin/env python3
"""Differential test of the prototype Decimal runtime (issue #19).

Generates operations, computes each expected result with exact rational
arithmetic, runs the C harness on the same operations, and compares.

    check.py <harness-binary> [cases-per-operation]
"""
import decimal
import pathlib
import random
import struct
import subprocess
import sys
from fractions import Fraction

MODES = [
    "NearestEven",
    "NearestAwayFromZero",
    "NearestTowardZero",
    "AwayFromZero",
    "TowardZero",
    "TowardGreater",
    "TowardLesser",
]


def round_to_integer(x: Fraction, mode: str) -> int:
    """The decided rounding modes, written from their definitions."""
    lower = x.numerator // x.denominator
    if x == lower:
        return lower
    upper = lower + 1
    toward_zero, away_from_zero = (lower, upper) if x > 0 else (upper, lower)
    distance_to_lower = x - lower
    if mode == "TowardLesser":
        return lower
    if mode == "TowardGreater":
        return upper
    if mode == "TowardZero":
        return toward_zero
    if mode == "AwayFromZero":
        return away_from_zero
    if distance_to_lower != Fraction(1, 2):
        return lower if distance_to_lower < Fraction(1, 2) else upper
    if mode == "NearestEven":
        return lower if lower % 2 == 0 else upper
    if mode == "NearestAwayFromZero":
        return away_from_zero
    if mode == "NearestTowardZero":
        return toward_zero
    raise ValueError(mode)


def fits(coefficient: int, p: int) -> bool:
    return abs(coefficient) < 10**p


def to_scale(x: Fraction, p: int, s: int, mode: str):
    coefficient = round_to_integer(x * 10**s, mode)
    return coefficient if fits(coefficient, p) else None


def check_oracle_against_python_decimal(rng: random.Random) -> int:
    """The oracle's modes must agree with Python's decimal module."""
    python_modes = {
        "NearestEven": decimal.ROUND_HALF_EVEN,
        "NearestAwayFromZero": decimal.ROUND_HALF_UP,
        "NearestTowardZero": decimal.ROUND_HALF_DOWN,
        "AwayFromZero": decimal.ROUND_UP,
        "TowardZero": decimal.ROUND_DOWN,
        "TowardGreater": decimal.ROUND_CEILING,
        "TowardLesser": decimal.ROUND_FLOOR,
    }
    context = decimal.Context(prec=200)
    checked = 0
    for _ in range(20000):
        digits = rng.choice([1, 2, 3, 5, 8])
        coefficient = rng.choice([rng.randrange(-10**6, 10**6), rng.randrange(-200, 200) * 5 * 10 ** (digits - 1)])
        for mode, python_mode in python_modes.items():
            expected = decimal.Decimal(coefficient).scaleb(-digits, context).quantize(decimal.Decimal(1), rounding=python_mode, context=context)
            assert round_to_integer(Fraction(coefficient, 10**digits), mode) == int(expected), (coefficient, digits, mode)
            checked += 1
    return checked


def check_oracle_against_gnucobol() -> int:
    """The oracle's modes must agree with the GnuCOBOL run recorded for issue #3."""
    cobol_modes = {
        "NEAREST-EVEN": "NearestEven",
        "NEAREST-AWAY-FROM-ZERO": "NearestAwayFromZero",
        "NEAREST-TOWARD-ZERO": "NearestTowardZero",
        "AWAY-FROM-ZERO": "AwayFromZero",
        "TRUNCATION": "TowardZero",
        "TOWARD-GREATER": "TowardGreater",
        "TOWARD-LESSER": "TowardLesser",
    }
    recorded = pathlib.Path(__file__).resolve().parents[2] / "cobol-decimal-semantics" / "expected" / "rounding.txt"
    checked = 0
    for line in recorded.read_text().splitlines():
        name, _, rest = line.partition(" ")
        if name not in cobol_modes or "=" not in rest or "size-error" in rest:
            continue
        value, result = rest.split("=")
        assert round_to_integer(Fraction(value) * 100, cobol_modes[name]) == Fraction(result) * 100, line
        checked += 1
    return checked


def coefficient_for(rng: random.Random, p: int) -> int:
    limit = 10**p
    kind = rng.randrange(8)
    if kind == 0:
        n = limit - 1 - rng.randrange(3)
    elif kind == 1:
        n = rng.randrange(0, 3)
    elif kind == 2:
        n = 5 * 10 ** rng.randrange(0, p)
    elif kind == 3:
        n = rng.randrange(1, 10) * 10 ** rng.randrange(0, p)
    else:
        n = rng.randrange(0, 10 ** rng.randrange(1, p + 1))
    n = min(n, limit - 1)
    return -n if rng.random() < 0.5 else n


def decimal_type(rng: random.Random):
    p = rng.choice([1, 2, 5, 9, 18, 19, 20, 31, 37, 38, rng.randrange(1, 39)])
    s = rng.choice([0, p, rng.randrange(0, p + 1)])
    return p, s


def generate(rng: random.Random, count: int):
    for _ in range(count):
        p, _ = decimal_type(rng)
        a, b = coefficient_for(rng, p), coefficient_for(rng, p)
        yield f"add {a} {b} {p}", (a + b if fits(a + b, p) else None)
        yield f"sub {a} {b} {p}", (a - b if fits(a - b, p) else None)

    for _ in range(count):
        p1 = rng.randrange(1, 38)
        p2 = rng.randrange(1, 39 - p1)
        a, b = coefficient_for(rng, p1), coefficient_for(rng, p2)
        yield f"mul {a} {b}", a * b

    for _ in range(count * 3):
        (p1, s1), (p2, s2), (p3, s3) = decimal_type(rng), decimal_type(rng), decimal_type(rng)
        a, b = coefficient_for(rng, p1), coefficient_for(rng, p2)
        shape = rng.randrange(6)
        if shape == 0 and b != 0:
            # An exact half at the result scale, when the scales allow it.
            a = max(-(10**p1 - 1), min(10**p1 - 1, (2 * rng.randrange(0, 50) + 1) * b * 5))
        elif shape == 1:
            b = rng.choice([1, -1, 2, 3, 7, 10, 20, 2 * 10 ** min(p2 - 1, rng.randrange(0, 5))])
            b = b if fits(b, p2) else 1
        elif shape == 2 and b != 0:
            # The dropped digits are exactly a half, plus sometimes a nonzero remainder.
            (p1, s1), s2, s3 = (38, rng.randrange(2, 20)), 0, 0
            dropped = rng.randrange(1, s1 + 1)
            s3 = s1 - dropped
            b = rng.randrange(1, 1000) * rng.choice([1, -1])
            quotient = rng.randrange(0, 10**6) * 10**dropped + 5 * 10 ** (dropped - 1)
            a = (quotient * abs(b) + rng.choice([0, 0, 1, abs(b) - 1])) * rng.choice([1, -1])
            p3 = rng.randrange(max(1, s3), 39)
        mode = rng.randrange(7)
        expected = None
        if b != 0:
            expected = to_scale(Fraction(a, 10**s1) / Fraction(b, 10**s2), p3, s3, MODES[mode])
        yield f"div {a} {s1} {b} {s2} {p3} {s3} {mode}", expected

    for _ in range(count * 2):
        (p, s), (p2, s2) = decimal_type(rng), decimal_type(rng)
        x = coefficient_for(rng, p)
        if rng.randrange(4) == 0 and s > s2:
            # An exact half at the target scale.
            x = max(-(10**p - 1), min(10**p - 1, (2 * rng.randrange(0, 500) + 1) * 5 * 10 ** (s - s2 - 1)))
        value = Fraction(x, 10**s)
        mode = rng.randrange(7)
        yield f"round {x} {s} {p2} {s2} {mode}", to_scale(value, p2, s2, MODES[mode])
        exact = value * 10**s2
        yield f"exact {x} {s} {p2} {s2}", (int(exact) if exact.denominator == 1 and fits(int(exact), p2) else None)

    for _ in range(count):
        p, s = decimal_type(rng)
        s2 = rng.randrange(s, 39)
        p2 = rng.randrange(max(1, s2, p + s2 - s), 39) if max(1, s2, p + s2 - s) <= 38 else None
        if p2 is None:
            continue
        x = coefficient_for(rng, p)
        yield f"widen {x} {s} {s2}", x * 10 ** (s2 - s)

    special = [0.0, -0.0, 0.1, 0.5, 1.5, 2.5, 0.125, 0.375, 2.675, 1e22, 1e38, 9.999999999999999e37,
               5e-324, 2.2250738585072014e-308, float("inf"), float("-inf"), float("nan"), 1.7976931348623157e308]
    for i in range(count * 2):
        if i < len(special) * 8:
            f = special[i % len(special)]
        elif rng.randrange(3) == 0:
            # A binary fraction with few bits: often an exact half at some scale.
            f = rng.randrange(-4096, 4096) / 2 ** rng.randrange(0, 12)
        else:
            f = struct.unpack("<d", struct.pack("<Q", rng.getrandbits(64)))[0] if rng.randrange(4) == 0 \
                else rng.uniform(-1, 1) * 10 ** rng.randrange(-40, 40)
        p2, s2 = decimal_type(rng)
        mode = rng.randrange(7)
        bits = struct.unpack("<Q", struct.pack("<d", f))[0]
        expected = None
        if f == f and f not in (float("inf"), float("-inf")):
            expected = to_scale(Fraction(f), p2, s2, MODES[mode])
        yield f"fromdouble {bits:016x} {p2} {s2} {mode}", expected

    for _ in range(count):
        p, s = decimal_type(rng)
        x = coefficient_for(rng, p)
        nearest = x / 10**s  # int / int is correctly rounded in Python
        yield f"todouble {x} {s}", f"{struct.unpack('<Q', struct.pack('<d', nearest))[0]:016x}"

    for _ in range(count // 10):
        n = coefficient_for(rng, 38)
        yield f"lit {int(n < 0)} {abs(n) // 10**19} {abs(n) % 10**19}", n


def main() -> int:
    harness = sys.argv[1]
    count = int(sys.argv[2]) if len(sys.argv) > 2 else 20000
    rng = random.Random(19)
    print(f"oracle agrees with python decimal on {check_oracle_against_python_decimal(rng)} roundings")
    print(f"oracle agrees with gnucobol record on {check_oracle_against_gnucobol()} roundings")

    cases = list(generate(rng, count))
    stdin = "".join(line + "\n" for line, _ in cases)
    run = subprocess.run([harness], input=stdin, capture_output=True, text=True)
    if run.returncode != 0:
        print(f"harness exited with {run.returncode}: {run.stderr.strip()[-400:]}")
        return 1
    actual = run.stdout.splitlines()
    if len(actual) != len(cases):
        print(f"harness printed {len(actual)} lines for {len(cases)} operations")
        return 1

    totals, failures, mismatches = {}, {}, {}
    for (line, expected), got in zip(cases, actual):
        op = line.split()[0]
        totals[op] = totals.get(op, 0) + 1
        want = "fail" if expected is None else str(expected)
        if want == "fail":
            failures[op] = failures.get(op, 0) + 1
        if got != want:
            mismatches.setdefault(op, []).append(f"{line} -> {got}, expected {want}")
    for op in totals:
        bad = mismatches.get(op, [])
        print(f"{op}: {totals[op]} operations, {failures.get(op, 0)} expected to fail, {len(bad)} mismatches")
        for text in bad[:5]:
            print(f"  {text}")
    total_bad = sum(len(v) for v in mismatches.values())
    print(f"match {sum(totals.values()) - total_bad} / mismatch {total_bad}")
    return 1 if total_bad else 0


if __name__ == "__main__":
    sys.exit(main())
