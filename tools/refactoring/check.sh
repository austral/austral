#!/usr/bin/env bash
# Checks that the working tree behaves like an earlier commit.
#   check.sh BASE_REF [--normalize-ids] [--quick]
#
# Builds the compiler at BASE_REF (once; kept in a cache) and the compiler of
# the working tree, then runs, in this order:
#   1. the unit tests (dune runtest)
#   2. the end-to-end suite (test-programs/runner.py) and the command-line
#      tests (test-programs/cli-runner.py)
#   3. the equivalence check: every program in the repository compiled by both
#      compilers, with the generated C, stdout, stderr, exit status, error.html
#      and the names of the files left behind compared
#   4. the metrics: none of the counts in expected/totals.txt may go up
#   5. the interface files export nothing that no other file uses
#   6. every piece of an error message appears in some test's expected output,
#      or is on the recorded list of pieces no program can reach
# Exits 1 if a step fails, 2 if a compiler could not be built or run.
# --quick skips steps 1 and 2.
#
# Environment:
#   ZUSTRAL_NIX_FLAGS       extra flags for `nix develop`
#   ZUSTRAL_COMPILER_CACHE  where base compilers are kept
#                           (default ~/.cache/zustral-compilers)
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
CACHE=${ZUSTRAL_COMPILER_CACHE:-$HOME/.cache/zustral-compilers}
read -r -a NIX_FLAGS <<< "${ZUSTRAL_NIX_FLAGS:-}"

BASE_REF=${1:?usage: check.sh BASE_REF [--normalize-ids] [--quick]}
shift
NORMALIZE=() QUICK=0
for a in "$@"; do
  case $a in
    --normalize-ids) NORMALIZE=(--normalize-ids) ;;
    --quick) QUICK=1 ;;
    *) echo "unknown option: $a" >&2; exit 2 ;;
  esac
done

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
cd "$ROOT" || exit 2

# Inside a nix shell already (CI runs under nix-shell), run the command as it is.
in_shell() {
  if [[ -n ${IN_NIX_SHELL:-} ]]; then bash -c "$1"; else nix develop "$ROOT" "${NIX_FLAGS[@]}" --command bash -c "$1"; fi
}

# The base compiler is built with the packages its own commit asks for. They
# can differ from the working tree's (a commit may add or drop a dependency).
in_base_shell() { # directory command
  if [[ -n ${IN_NIX_SHELL:-} ]]; then
    nix-shell "$1/shell.nix" --run "$2"
  else
    nix develop "path:$1" "${NIX_FLAGS[@]}" --command bash -c "$2"
  fi
}

# Two commits with the same compiler sources and the same packages share one
# cached binary. The command-line library (cli/) exists from R9 on.
compiler_sources=(lib bin dune-project concat_builtins.py austral.opam flake.nix flake.lock shell.nix)
git cat-file -e "$BASE_REF:cli" 2> /dev/null && compiler_sources+=(cli)
key=$(for s in "${compiler_sources[@]}"; do git rev-parse "$BASE_REF:$s"; done | sha1sum | cut -c1-16)
base="$CACHE/$key/austral"
if [[ ! -x $base ]]; then
  echo "== building the compiler at $BASE_REF"
  mkdir -p "$T/base" "$CACHE/$key"
  git archive "$BASE_REF" "${compiler_sources[@]}" | tar -x -C "$T/base"
  in_base_shell "$T/base" "cd '$T/base' && python3 concat_builtins.py && dune build --root . ./bin/austral.exe 2>&1" > "$T/base.log" 2>&1 \
    || { echo "base build failed: $(tail -n 5 "$T/base.log")"; exit 2; }
  cp "$T/base/_build/default/bin/austral.exe" "$base"
fi

failed=0
step() { # name command...
  local name=$1; shift
  if "$@" > "$T/step.log" 2>&1; then
    echo "ok    $name: $(tail -n 1 "$T/step.log")"
  else
    echo "FAIL  $name"; tail -n 25 "$T/step.log"; failed=1
  fi
}

echo "== building the working tree"
in_shell 'make 2>&1' > "$T/head.log" 2>&1 || { echo "build failed:"; tail -n 40 "$T/head.log"; exit 2; }

if ((!QUICK)); then
  step "unit tests" in_shell 'dune runtest --force 2>&1'
  step "end-to-end suite" python3 test-programs/runner.py
  step "command-line tests" python3 test-programs/cli-runner.py
fi
python3 "$HERE/equivalence.py" snapshot "$base" "$T/a" > /dev/null || { echo "FAIL  base snapshot"; exit 2; }
python3 "$HERE/equivalence.py" snapshot "$ROOT/austral" "$T/b" > /dev/null || { echo "FAIL  head snapshot"; exit 2; }
step "equivalence with $BASE_REF" python3 "$HERE/equivalence.py" compare "${NORMALIZE[@]}" "$T/a" "$T/b"
if ((failed)); then python3 "$HERE/equivalence.py" compare "${NORMALIZE[@]}" "$T/a" "$T/b" | head -n 60; fi
step "metrics" "$HERE/measure.sh" --check
step "unused exports" "$HERE/unused-exports.sh"
step "error messages have tests" python3 "$HERE/unpinned-messages.py"

exit "$failed"
