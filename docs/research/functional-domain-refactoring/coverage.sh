#!/usr/bin/env bash
# Measures how much of the compiler the corpus of equivalence.py executes.
# A line the corpus never runs is a line a refactoring can break unnoticed.
#   coverage.sh            compare with expected/coverage.txt
#   coverage.sh --record   rewrite expected/coverage.txt
# Environment:
#   ZUSTRAL_NIX_FLAGS      extra flags for `nix develop`
#   ZUSTRAL_BUILD_TIMEOUT  seconds allowed for the build (default 1800)
#
# The build is instrumented with bisect_ppx in a temporary copy of the tree.
# The copy adds bisect_ppx to the development shell and to lib/dune; the
# repository itself is not changed.
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../../.." && pwd)
RECORD=0
[[ ${1:-} == --record ]] && RECORD=1
read -r -a NIX_FLAGS <<< "${ZUSTRAL_NIX_FLAGS:-}"

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
w="$T/tree"
mkdir -p "$w" "$T/cov"

(cd "$ROOT" && git ls-files -z lib bin dune-project concat_builtins.py austral.opam flake.nix flake.lock | xargs -0 tar -cf -) | tar -xf - -C "$w"
sed -i 's/ocamlPackages.zarith/ocamlPackages.zarith\n          ocamlPackages.bisect_ppx/' "$w/flake.nix"
sed -i 's/^(lang dune 2\.1)$/(lang dune 2.7)/' "$w/dune-project"
sed -i 's/^ (libraries unix str sexplib zarith yojson)$/ (libraries unix str sexplib zarith yojson)\n (instrumentation (backend bisect_ppx))/' "$w/lib/dune"
grep -q bisect_ppx "$w/flake.nix" && grep -q bisect_ppx "$w/lib/dune" && grep -q 'lang dune 2.7' "$w/dune-project" \
  || { echo "could not patch the copy" >&2; exit 2; }

# The report's own total counts the generated parser and built-in modules too.
# Add a total over the hand-written files, which is the number the plan tracks.
summarize() {
  awk -F'\t' '
    $1 == "Project" { printf "total-with-generated\t%s\t%s\n", $2, $3; next }
    { split($3, a, "/"); hit += a[1]; all += a[2]; print }
    END { printf "total-hand-written\t%.2f\t%d/%d\n", 100 * hit / all, hit, all }'
}

in_shell() { (cd "$w" && nix develop "path:$w" "${NIX_FLAGS[@]}" --command bash -c "$1"); }

timeout "${ZUSTRAL_BUILD_TIMEOUT:-1800}" bash -c "$(declare -f in_shell); w='$w'; NIX_FLAGS=(${NIX_FLAGS[*]@Q}); in_shell 'python3 concat_builtins.py && dune build --root . --profile release --instrument-with bisect_ppx ./bin/austral.exe 2>&1'" > "$T/build.log" 2>&1 \
  || { echo "instrumented build failed: $(tail -n 5 "$T/build.log")" >&2; exit 2; }

BISECT_FILE="$T/cov/bisect" python3 "$HERE/equivalence.py" snapshot "$w/_build/default/bin/austral.exe" "$T/snapshot" > "$T/snapshot.log" 2>&1 \
  || { echo "corpus run failed: $(tail -n 5 "$T/snapshot.log")" >&2; exit 2; }

in_shell "bisect-ppx-report summary --per-file --coverage-path '$T/cov'" > "$T/report.txt" 2> "$T/report.err" \
  || { echo "report failed: $(tail -n 5 "$T/report.err")" >&2; exit 2; }

# Lines look like " 87.50 %   14/16   lib/Foo.ml"; the generated modules are left out.
grep -E '%' "$T/report.txt" | grep -v -E 'BuiltInModules|Parser\.ml|Lexer\.ml' \
  | awk '{ pct = $1; frac = $3; file = $4; if (file == "") file = "total"; printf "%s\t%s\t%s\n", file, pct, frac }' \
  | sort | summarize > "$T/coverage.txt"

if ((RECORD)); then
  cp "$T/coverage.txt" "$HERE/expected/coverage.txt"; echo "recorded coverage.txt"
elif diff -u "$HERE/expected/coverage.txt" "$T/coverage.txt"; then
  echo "match coverage.txt"
else
  echo "MISMATCH coverage.txt"; exit 1
fi
