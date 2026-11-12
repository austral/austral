#!/usr/bin/env bash
# Builds the harness, runs the differential test, then checks that the test
# notices deliberate bugs. See ../README.md.
#   run.sh [cases-per-operation]
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
COUNT=${1:-20000}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

cc -O1 -Wall -Wextra -fsanitize=undefined -fno-sanitize-recover=undefined -o "$T/harness" "$HERE/harness.c" || exit 2
python3 "$HERE/check.py" "$T/harness" "$COUNT" || exit 1

# Each mutant replaces one fragment of decimal.c. The differential test must report mismatches for it.
mutant() { # name old new
  mkdir -p "$T/$1"
  cp "$HERE/decimal.h" "$HERE/harness.c" "$T/$1/"
  python3 - "$HERE/decimal.c" "$2" "$3" > "$T/$1/decimal.c" <<'PY' || { echo "mutant $1: fragment not found"; missed=$((missed + 1)); return; }
import sys
source, old, new = open(sys.argv[1]).read(), sys.argv[2], sys.argv[3]
sys.exit(1) if source.count(old) != 1 else sys.stdout.write(source.replace(old, new))
PY
  cc -O1 -w -o "$T/$1/harness" "$T/$1/harness.c" || { echo "mutant $1: does not compile"; missed=$((missed + 1)); return; }
  local last
  last=$(python3 "$HERE/check.py" "$T/$1/harness" "$COUNT" | tail -n 1)
  if [[ $last == *"mismatch 0" ]]; then
    echo "mutant $1: NOT detected"; missed=$((missed + 1))
  else
    echo "mutant $1: detected ($last)"; caught=$((caught + 1))
  fi
}

caught=0 missed=0
mutant tie-to-odd '(d == ZD_HALF && kept_is_odd)' '(d == ZD_HALF && !kept_is_odd)'
mutant toward-greater-ignores-sign 'case ZD_TOWARD_GREATER:         return !negative;' 'case ZD_TOWARD_GREATER:         return 1;'
mutant times-ten-wraps 'for (int j = 0; j < 10; j++) {
        acc += r;
        if (acc >= nb) { acc -= nb; digit++; }
      }' 'acc = r * 10; digit = (unsigned)(acc / nb); acc = acc % nb;'
mutant no-early-overflow-exit '      if (q >= zd_pow10(37)) return 0;
' ''
mutant half-ignores-remainder '    else if (d == ZD_HALF) d = ZD_ABOVE_HALF;' ''
mutant exact-quotient-ignores-remainder '    if (d == ZD_NOTHING) d = ZD_BELOW_HALF;
    else' '    if (d == ZD_NOTHING) d = ZD_NOTHING;
    else'
mutant exact-allows-dropping '  if (must_be_exact && d != ZD_NOTHING) return 0;' ''
mutant float-half-ignores-lower-bits 'half_bit ? (below_half_bit ? ZD_ABOVE_HALF : ZD_HALF)' 'half_bit ? (below_half_bit ? ZD_HALF : ZD_HALF)'
mutant float-drops-high-limb '(unsigned long long)(hi >> 64), 0 };' '0, 0 };'

echo "mutants detected $caught / not detected $missed"
((missed == 0))
