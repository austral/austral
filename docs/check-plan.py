#!/usr/bin/env python3
"""Cross-checks docs/plan.md and docs/decimal.md against the open issues.

  python3 docs/check-plan.py 16 20 21 22 25 ...

The arguments are the numbers of the issues that are open on GitHub.
Checks that
  - every open issue is named in the plan (the tracking issue names itself),
  - every issue the plan and the spec's sections 15 and 17 point at is open,
  - every row of the spec's "not included" table (section 15) names an issue,
  - the plan's dependencies name existing items and have no cycle.
Prints the items Decimal needs (the closure of D7). Exits 1 on any finding.
"""
import pathlib
import re
import sys

# Closed decision tickets and merged pull requests that the documents cite on purpose.
CLOSED_ON_PURPOSE = set(range(1, 16)) | {17, 18, 19, 23, 24, 45}
TRACKING_ISSUE = 71

docs = pathlib.Path(__file__).parent
plan = (docs / "plan.md").read_text()
spec = (docs / "decimal.md").read_text()
open_issues = {int(a) for a in sys.argv[1:]}
if not open_issues:
    sys.exit("pass the open issue numbers as arguments")

findings = []


def cited(text: str) -> set[int]:
    return {int(n) for n in re.findall(r"#(\d+)", text)}


for number in sorted(open_issues - cited(plan)):
    findings.append(f"open issue #{number} is not in the plan")
if TRACKING_ISSUE not in open_issues:
    findings.append(f"tracking issue #{TRACKING_ISSUE} is not open")

not_included = spec[spec.index("## 15.") : spec.index("## 16.")]
known_problems = spec[spec.index("## 17.") : spec.index("## 18.")]
for name, text in (("plan", plan), ("spec 15", not_included), ("spec 17", known_problems)):
    for number in sorted(cited(text) - open_issues - CLOSED_ON_PURPOSE):
        findings.append(f"{name} cites #{number}, which is not open")

rows = [r for r in not_included.splitlines() if r.startswith("| ") and "---" not in r][1:]
for row in rows:
    if not re.search(r"#\d+", row):
        findings.append(f"spec 15 row has no issue: {row}")

deps: dict[str, set[str]] = {}
current = None
for line in plan.splitlines():
    heading = re.match(r"### ([A-Z]\d+)\. ", line)
    if heading:
        current = heading.group(1)
        deps[current] = set()
    elif current and line.startswith("- **依存。**"):
        # Only the first sentence lists hard dependencies.
        first_sentence = line[len("- **依存。**") :].split("。")[0]
        deps[current] |= set(re.findall(r"\b([A-Z]\d+)\b", first_sentence))
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


def order(item: str) -> tuple[str, int]:
    return item[0], int(item[1:])


path = sorted(closure("D7", {"D7"}), key=order)
print(f"open issues: {len(open_issues)}, plan items with a heading: {len(deps)}, spec 15 rows: {len(rows)}")
print(f"needed before Decimal is usable ({len(path)}): {' '.join(path)}")
for finding in findings:
    print("FINDING:", finding)
sys.exit(1 if findings else 0)
