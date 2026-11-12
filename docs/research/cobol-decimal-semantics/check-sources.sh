#!/usr/bin/env bash
# Fetches every URL in sources.tsv and checks that the quoted phrase is still
# in the document. HTML is reduced to text, PDF goes through pdftotext, and
# anything else is read as plain text. Needs network access to the listed
# hosts and pdftotext. See README.md.
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

# Several rows quote the same manual, so each URL is fetched once.
fetch() { # url -> path of the extracted text, or nothing
  local key out
  key=$(printf '%s' "$1" | sha1sum | cut -c1-16)
  out="$T/$key.txt"
  if [[ ! -f $out ]]; then
    curl -fsSL --max-time 120 -o "$T/$key.raw" "$1" 2>"$T/$key.err" || return 1
    if head -c 5 "$T/$key.raw" | grep -q '%PDF'; then
      pdftotext -q "$T/$key.raw" "$out" || return 1
    else
      python3 - "$T/$key.raw" "$out" <<'PY'
import html, re, sys
page = open(sys.argv[1], encoding="utf-8", errors="replace").read()
if re.search(r"(?i)<html", page):
    page = re.sub(r"(?is)<(script|style).*?</\1>", " ", page)
    page = html.unescape(re.sub(r"<[^>]+>", " ", page))
open(sys.argv[2], "w").write(page)
PY
    fi
  fi
  echo "$out"
}

found=0 missing=0 unreachable=0
while IFS=$'\t' read -r id url phrase; do
  if ! text=$(fetch "$url") || [[ -z $text ]]; then
    unreachable=$((unreachable + 1)); echo "unreachable $id"; continue
  fi
  if python3 - "$text" "$phrase" <<'PY'
import re, sys
norm = lambda s: re.sub(r"\s+", " ", s.replace("’", "'"))
sys.exit(0 if norm(sys.argv[2]) in norm(open(sys.argv[1], errors="replace").read()) else 1)
PY
  then
    found=$((found + 1)); echo "found $id"
  else
    missing=$((missing + 1)); echo "MISSING $id"
  fi
done < "$HERE/sources.tsv"

echo "found $found / missing $missing / unreachable $unreachable"
((missing == 0 && unreachable == 0))
