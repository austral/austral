#!/usr/bin/env bash
# Fetches every URL in sources.tsv and checks that the quoted phrase is still
# on the page. Needs network access to the listed hosts. See README.md.
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

found=0 missing=0 unreachable=0
while IFS=$'\t' read -r id url phrase; do
  if ! curl -fsSL --max-time 60 -o "$T/page" "$url" 2>"$T/err"; then
    unreachable=$((unreachable + 1)); echo "unreachable $id: $(tail -n 1 "$T/err")"; continue
  fi
  if python3 - "$T/page" "$phrase" <<'PY'
import html, re, sys
page = open(sys.argv[1], encoding="utf-8", errors="replace").read()
page = re.sub(r"(?is)<(script|style).*?</\1>", " ", page)
text = re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", " ", page)))
sys.exit(0 if re.sub(r"\s+", " ", sys.argv[2]) in text else 1)
PY
  then
    found=$((found + 1)); echo "found $id"
  else
    missing=$((missing + 1)); echo "MISSING $id"
  fi
done < "$HERE/sources.tsv"

echo "found $found / missing $missing / unreachable $unreachable"
((missing == 0 && unreachable == 0))
