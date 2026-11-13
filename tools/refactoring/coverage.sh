#!/usr/bin/env bash
# Measures how much of the compiler the corpus of equivalence.py executes.
# A line the corpus never runs is a line a refactoring can break unnoticed.
#   coverage.sh              compare the per-file summary with expected/coverage.txt
#   coverage.sh --record     rewrite expected/coverage.txt
#   coverage.sh --lines F    print the line numbers of lib/F that no program executes
# Environment:
#   ZUSTRAL_NIX_FLAGS  extra flags for `nix develop`
#
# The instrumented compiler is built in _build_coverage, beside the normal build.
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
read -r -a NIX_FLAGS <<< "${ZUSTRAL_NIX_FLAGS:-}"
MODE=compare FILE=
case ${1:-} in
  --record) MODE=record ;;
  --lines) MODE=lines; FILE=${2:?usage: coverage.sh --lines File.ml} ;;
esac

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/cov"
cd "$ROOT" || exit 2

# Inside a nix shell already (CI runs under nix-shell), run the command as it is.
in_shell() {
  if [[ -n ${IN_NIX_SHELL:-} ]]; then bash -c "$1"; else nix develop "$ROOT" "${NIX_FLAGS[@]}" --command bash -c "$1"; fi
}

in_shell 'python3 concat_builtins.py && dune build --root . --build-dir _build_coverage --instrument-with bisect_ppx ./bin/austral.exe 2>&1' > "$T/build.log" 2>&1 \
  || { echo "instrumented build failed: $(tail -n 8 "$T/build.log")" >&2; exit 2; }

BISECT_FILE="$T/cov/bisect" python3 "$HERE/equivalence.py" snapshot "$ROOT/_build_coverage/default/bin/austral.exe" "$T/snapshot" > "$T/snapshot.log" 2>&1 \
  || { echo "corpus run failed: $(tail -n 5 "$T/snapshot.log")" >&2; exit 2; }

if [[ $MODE == lines ]]; then
  in_shell "bisect-ppx-report cobertura --coverage-path '$T/cov' '$T/cobertura.xml'" > /dev/null 2> "$T/report.err" \
    || { echo "report failed: $(tail -n 5 "$T/report.err")" >&2; exit 2; }
  python3 - "$T/cobertura.xml" "$FILE" <<'PY'
import sys, xml.etree.ElementTree as ET
tree, wanted = ET.parse(sys.argv[1]), sys.argv[2]
for cls in tree.iter("class"):
    if cls.get("filename", "").endswith("/" + wanted) or cls.get("filename") == wanted:
        missed = [int(l.get("number")) for l in cls.iter("line") if l.get("hits") == "0"]
        print(" ".join(map(str, sorted(set(missed)))))
        break
else:
    sys.exit(f"{wanted}: not in the coverage report")
PY
  exit $?
fi

in_shell "bisect-ppx-report summary --per-file --coverage-path '$T/cov'" > "$T/report.txt" 2> "$T/report.err" \
  || { echo "report failed: $(tail -n 5 "$T/report.err")" >&2; exit 2; }

# The report's own total counts the generated parser and built-in modules too.
# Add a total over the hand-written files, which is the number the plan tracks.
summarize() {
  awk -F'\t' '
    $1 == "Project" { printf "total-with-generated\t%s\t%s\n", $2, $3; next }
    { split($3, a, "/"); hit += a[1]; all += a[2]; print }
    END { printf "total-hand-written\t%.2f\t%d/%d\n", 100 * hit / all, hit, all }'
}

# Lines look like " 87.50 %   14/16   lib/Foo.ml"; the generated modules are left out.
grep -E '%' "$T/report.txt" | grep -v -E 'BuiltInModules|Parser\.ml|Lexer\.ml' \
  | awk '{ pct = $1; frac = $3; file = $4; if (file == "") file = "total"; printf "%s\t%s\t%s\n", file, pct, frac }' \
  | sort | summarize > "$T/coverage.txt"

if [[ $MODE == record ]]; then
  cp "$T/coverage.txt" "$HERE/expected/coverage.txt"; echo "recorded coverage.txt: $(tail -n 1 "$T/coverage.txt")"
elif diff -u "$HERE/expected/coverage.txt" "$T/coverage.txt"; then
  echo "match coverage.txt: $(tail -n 1 "$T/coverage.txt")"
else
  echo "MISMATCH coverage.txt"; exit 1
fi
