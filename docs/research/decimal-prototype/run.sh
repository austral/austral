#!/usr/bin/env bash
# Re-runs the whole prototype for issue #19. See README.md.
#   run.sh
# Environment:
#   ZUSTRAL_NIX_FLAGS      extra flags for `nix develop`, e.g. --override-input ...
#   ZUSTRAL_BUILD_TIMEOUT  seconds allowed for each compiler build (default 900)
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../../.." && pwd)
# The patch and the tests describe the compiler at this commit, not at HEAD.
BASE=f601bcb45f07e7b12a2ed026e5870ed4023fa8e3
BUILD_TIMEOUT=${ZUSTRAL_BUILD_TIMEOUT:-900}
read -r -a NIX_FLAGS <<< "${ZUSTRAL_NIX_FLAGS:-}"

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
failed=0

echo "== runtime: differential test and mutants"
"$HERE/runtime/run.sh" > "$T/runtime.log" 2>&1 || failed=1
grep -E '^(match|mutants|oracle)' "$T/runtime.log"

# `dune build --root .` keeps dune from adopting an enclosing checkout as its root.
build() { # tree
  (cd "$1" && timeout "$BUILD_TIMEOUT" nix develop "$ROOT" "${NIX_FLAGS[@]}" --command \
    bash -c 'python3 concat_builtins.py && dune build --root . 2>&1 && cp -f _build/default/bin/austral.exe austral') > "$1/build.log" 2>&1
}

tree="$T/compiler"
mkdir -p "$tree"
git -C "$ROOT" archive "$BASE" | tar -x -C "$tree"

echo "== compiler: tests against the unpatched compiler (they must fail)"
if build "$tree"; then
  "$HERE/compiler/run-tests.sh" "$tree/austral" | tail -n 1
else
  echo "build failed: $(tail -n 3 "$tree/build.log")"; failed=1
fi

echo "== compiler: slice applied to a clean export of the base commit"
(cd "$tree" && patch -s -p1 --no-backup-if-mismatch < "$HERE/compiler/compiler-slice.patch") || failed=1
python3 "$HERE/compiler/embed-runtime.py" "$tree" "$HERE/runtime" || failed=1
if build "$tree"; then
  "$HERE/compiler/run-tests.sh" "$tree/austral" > "$T/slice.log"
  [[ $? -eq 0 ]] || failed=1
  grep '^FAIL' "$T/slice.log"
  tail -n 1 "$T/slice.log"
else
  echo "build failed: $(tail -n 3 "$tree/build.log")"; failed=1
fi

echo "== compiler: slice plus the expected-type experiment (two tests that assert the error must flip)"
(cd "$tree" && patch -s -p1 --no-backup-if-mismatch < "$HERE/compiler/experiments/expected-type-flow.patch") || failed=1
if build "$tree"; then
  "$HERE/compiler/run-tests.sh" "$tree/austral" > "$T/experiment.log"
  flipped=$(grep '^FAIL' "$T/experiment.log" | sed -E 's/^FAIL  ([^:]+):.*/\1/' | sort | tr '\n' ' ')
  tail -n 1 "$T/experiment.log"
  echo "flipped: $flipped"
  [[ $flipped == "divide-no-context-argument divide-no-context-operand " ]] || failed=1
else
  echo "build failed: $(tail -n 3 "$tree/build.log")"; failed=1
fi

((failed == 0)) && echo "all checks as expected" || echo "SOME CHECKS DIFFER"
exit "$failed"
