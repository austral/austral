#!/usr/bin/env bash
# Lists values that an interface file exports and no other file mentions.
# A value on this list is either dead or should not be exported.
#   unused-exports.sh            compare with expected/unused-exports.txt
#   unused-exports.sh --record   rewrite it
# The search is by name, so a value that shares its name with another
# module's value is not reported. The list is a lower bound.
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../../.." && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

cd "$ROOT"
others() { # module: every source file except the module's own pair and the generated one
  ls lib/*.ml lib/*.mli lib/*.mll lib/*.mly bin/*.ml test/*.ml | grep -v -e "^lib/$1\.mli\?$" -e '^lib/BuiltInModules\.ml$'
}
for mli in lib/*.mli; do
  module=$(basename "$mli" .mli)
  grep -oE '^val [a-z_][A-Za-z0-9_'"'"']*' "$mli" | sed 's/^val //' | sort -u | while read -r name; do
    if ! others "$module" | xargs grep -qw -- "$name"; then
      printf '%s\t%s\n' "$module" "$name"
    fi
  done
done | sort > "$T/unused-exports.txt"

if [[ ${1:-} == --record ]]; then
  cp "$T/unused-exports.txt" "$HERE/expected/unused-exports.txt"; echo "recorded $(wc -l < "$T/unused-exports.txt") names"
elif diff -u "$HERE/expected/unused-exports.txt" "$T/unused-exports.txt"; then
  echo "match unused-exports.txt"
else
  echo "MISMATCH unused-exports.txt"; exit 1
fi
