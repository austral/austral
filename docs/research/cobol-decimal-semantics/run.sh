#!/usr/bin/env bash
# Compiles and runs every COBOL probe with GnuCOBOL and compares the result
# with expected/. See README.md.
#   run.sh            compare
#   run.sh --record   rewrite expected/ from the current outputs
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../../.." && pwd)
# Same nixpkgs revision as flake.lock. Override it (e.g. with path:<checkout>)
# when github: flake references cannot be fetched.
NIXPKGS=${ZUSTRAL_NIXPKGS:-github:NixOS/nixpkgs/$(python3 -c '
import json, sys
print(json.load(open(sys.argv[1]))["nodes"]["nixpkgs"]["locked"]["rev"])' "$ROOT/flake.lock")}

RECORD=0
[[ ${1:-} == --record ]] && RECORD=1

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

store() { nix build --no-link --print-out-paths "$NIXPKGS#$1" 2>"$T/nix.err" | head -n 1; }
COBOL_BIN=$(store gnu-cobol.bin) && COBOL_LIB=$(store gnu-cobol.lib) && GMP_DEV=$(store gmp.dev)
if [[ -z $COBOL_BIN || -z $COBOL_LIB || -z $GMP_DEV ]]; then
  echo "GnuCOBOL を取得できなかった: $(tail -n 1 "$T/nix.err")" >&2
  exit 2
fi
COBC=$COBOL_BIN/bin/cobc
export COB_CFLAGS="$("$COBOL_BIN/bin/cob-config" --cflags) -I$GMP_DEV/include"

# One record per probe: the compiler's verdict and diagnostics, then the
# program's output and exit status.
probe() { # name
  local src="$HERE/probes/$1.cob" exe="$T/$1"
  echo "cobc: $("$COBC" --version | head -n 1)"
  if (cd "$HERE/probes" && "$COBC" -free -x "$1.cob" -o "$exe") > "$T/$1.diag" 2>&1; then
    echo "compile: ok"
  else
    echo "compile: failed"
  fi
  sed -E 's/^[^ ]+\.cob:/line /' "$T/$1.diag"
  [[ -x $exe ]] || return 0
  LD_LIBRARY_PATH=$COBOL_LIB/lib "$exe"
  echo "exit: $?"
}

mkdir -p "$HERE/expected"
pass=0 fail=0
for src in "$HERE"/probes/*.cob; do
  name=$(basename "$src" .cob)
  probe "$name" > "$T/$name.txt" 2>&1
  if ((RECORD)); then
    cp "$T/$name.txt" "$HERE/expected/$name.txt"; echo "recorded $name"
  elif diff -u "$HERE/expected/$name.txt" "$T/$name.txt"; then
    pass=$((pass + 1)); echo "match $name"
  else
    fail=$((fail + 1)); echo "MISMATCH $name"
  fi
done

((RECORD)) && exit 0
echo "match $pass / mismatch $fail"
((fail == 0))
