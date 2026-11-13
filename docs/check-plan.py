#!/usr/bin/env python3
"""Cross-checks docs/plan.md, docs/refactoring-plan.md and docs/decimal.md against the open issues.

  python3 docs/check-plan.py 16 20 21 22 25 ...

The arguments are the numbers of the issues that are open on GitHub.
Checks that
  - every open issue is named in the plan (the tracking issues name themselves),
  - every issue the plans and the spec's sections 15 and 17 point at is open,
  - every row of the spec's "not included" table (section 15) names an issue,
  - the dependencies of both plans name existing items and have no cycle,
  - in the refactoring plan, every dependency on another R item points backward,
  - the overall plan lists every item of the refactoring plan,
  - the refactoring plan's table of measurements quotes the recorded numbers.
Prints the items Decimal needs (the closure of D7). Exits 1 on any finding.
"""
import pathlib
import re
import sys

# Closed decision tickets and merged pull requests that the documents cite on purpose.
CLOSED_ON_PURPOSE = set(range(1, 16)) | {17, 18, 19, 23, 24, 45}
TRACKING_ISSUES = (71, 84)

docs = pathlib.Path(__file__).parent
plan = (docs / "plan.md").read_text()
refactoring = (docs / "refactoring-plan.md").read_text()
spec = (docs / "decimal.md").read_text()
open_issues = {int(a) for a in sys.argv[1:]}
if not open_issues:
    sys.exit("pass the open issue numbers as arguments")

findings = []


def cited(text: str) -> set[int]:
    return {int(n) for n in re.findall(r"#(\d+)", text)}


for number in sorted(open_issues - cited(plan)):
    findings.append(f"open issue #{number} is not in the plan")
for number in TRACKING_ISSUES:
    if number not in open_issues:
        findings.append(f"tracking issue #{number} is not open")

not_included = spec[spec.index("## 15.") : spec.index("## 16.")]
known_problems = spec[spec.index("## 17.") : spec.index("## 18.")]
for name, text in (("plan", plan), ("refactoring plan", refactoring), ("spec 15", not_included), ("spec 17", known_problems)):
    for number in sorted(cited(text) - open_issues - CLOSED_ON_PURPOSE):
        findings.append(f"{name} cites #{number}, which is not open")

rows = [r for r in not_included.splitlines() if r.startswith("| ") and "---" not in r][1:]
for row in rows:
    if not re.search(r"#\d+", row):
        findings.append(f"spec 15 row has no issue: {row}")

def items_named(sentence: str) -> set[str]:
    """Item ids in a sentence; "D3 から D10" names every item from D3 to D10."""
    named = set(re.findall(r"\b([A-Z]\d+)\b", sentence))
    for letter, low, other, high in re.findall(r"\b([A-Z])(\d+) から ([A-Z])(\d+)\b", sentence):
        if letter == other:
            named |= {f"{letter}{n}" for n in range(int(low), int(high) + 1)}
    return named


deps: dict[str, set[str]] = {}
order: list[str] = []
for text in (plan, refactoring):
    current = None
    for line in text.splitlines():
        heading = re.match(r"### ([A-Z]\d+)\. ", line)
        if heading:
            current = heading.group(1)
            deps[current] = set()
            order.append(current)
        elif current and line.startswith("- **依存。**"):
            # Only the first sentence lists hard dependencies.
            deps[current] |= items_named(line[len("- **依存。**") :].split("。")[0])
for item, needs in deps.items():
    for need in sorted(needs - deps.keys()):
        findings.append(f"{item} depends on {need}, which is not an item")


def closure(item: str, seen: set[str]) -> set[str]:
    for need in deps.get(item, ()):
        if need not in seen:
            seen.add(need)
            closure(need, seen)
    return seen


for item in deps:
    if item in closure(item, set()):
        findings.append(f"dependency cycle through {item}")


def by_id(item: str) -> tuple[str, int]:
    return item[0], int(item[1:])


refactoring_items = [item for item in order if item[0] == "R" and item != "R0"]
for item in refactoring_items:
    for need in deps[item]:
        if need[0] == "R" and int(need[1:]) >= int(item[1:]):
            findings.append(f"{item} depends on {need}, which is not an earlier item")
    if not re.search(rf"^\| {item} \|", plan, re.M):
        findings.append(f"{item} is not in the overall plan's table")

# The refactoring plan's table of measurements must quote the numbers recorded when the plan was made.
baseline = docs.parent / "tools" / "refactoring" / "baseline"
totals = dict(line.split() for line in (baseline / "totals.txt").read_text().splitlines())
fragile = (baseline / "fragile-totals.txt").read_text()
coverage = (baseline / "coverage.txt").read_text()
unused = len((baseline / "unused-exports.txt").read_text().splitlines())
measured = refactoring[refactoring.index("## 6.") : refactoring.index("## 7.")]
fragile_total = re.search(r"fragile-matches (\d+)", fragile).group(1)
coverage_total = re.search(r"total-hand-written\t([0-9.]+)", coverage).group(1)
quoted = {
    f"| 例外を投げる箇所 | {totals['raise']} |": "raise",
    f"（`internal_err`） | {totals['internal']} |": "internal",
    f"（`err \"...\"`） | {totals['adhoc']} |": "adhoc",
    f"| 例外を捕まえる箇所 | {totals['catch']} |": "catch",
    f"| 可変の状態 | {totals['state']} |": "state",
    f"| 何でも受ける match の枝 | {totals['wildcard']} |": "wildcard",
    f"| 壊れやすい match | {fragile_total} |": "fragile",
    f"| 外から使われていない公開関数 | {unused}（": "unused exports",
    f"| テストが実行するコード | {coverage_total}% |": "coverage",
}
for text, name in quoted.items():
    if text not in measured:
        findings.append(f"refactoring plan section 6 does not quote the recorded {name}: expected {text!r}")

path = sorted(closure("D7", {"D7"}), key=by_id)
print(f"open issues: {len(open_issues)}, items with a heading: {len(deps)}, of which refactoring: {len(refactoring_items)}, spec 15 rows: {len(rows)}")
print(f"needed before Decimal is usable ({len(path)}): {' '.join(path)}")
for finding in findings:
    print("FINDING:", finding)
sys.exit(1 if findings else 0)
