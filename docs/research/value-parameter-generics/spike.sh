#!/usr/bin/env bash
# Measures where new type constructors would land in the compiler, and how
# the C backend emits generic instantiations. See README.md.
#   spike.sh            compare with expected/
#   spike.sh --record   rewrite expected/ from the current outputs
#
# Each patch in patches/ only adds constructors to the compiler's type
# definitions. The compiler then lists every match that no longer covers
# its type (warning 8). Warning 4 lists the matches whose wildcard would
# silently absorb the new constructors.
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../../.." && pwd)
BUILD_TIMEOUT=${ZUSTRAL_BUILD_TIMEOUT:-900}
# Extra flags for `nix develop`, e.g. --override-input when GitHub's API is unreachable.
read -r -a NIX_FLAGS <<< "${ZUSTRAL_NIX_FLAGS:-}"

RECORD=0
[[ ${1:-} == --record ]] && RECORD=1

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

# Builds lib/ at HEAD plus one patch and prints the raw compiler log.
build() { # name patch-or-empty
  local w="$T/$1"
  mkdir -p "$w"
  git -C "$ROOT" archive HEAD | tar -x -C "$w"
  if [[ -n $2 ]]; then
    (cd "$w" && patch -s -p1 < "$2") || { echo "patch failed: $2" >&2; return 1; }
  fi
  (cd "$w" && OCAMLPARAM="_,w=+4+8,warn-error=-a" timeout "$BUILD_TIMEOUT" \
    nix develop "$ROOT" "${NIX_FLAGS[@]}" --command \
    bash -c 'python3 concat_builtins.py && dune build --profile release ./lib 2>&1') > "$T/$1.log"
  local rc=$?
  ((rc == 0)) || { echo "build $1 failed ($rc), log: $(tail -n 3 "$T/$1.log")" >&2; return 1; }
}

# One line per warning: kind<TAB>file<TAB>line<TAB>detail, then the
# enclosing function is looked up. Fragile matches are kept only over the
# given types, written as Module.type (e.g. Type.ty).
sites() { # name types-regex
  local w="$T/$1"
  awk -v types="$2" '
    /^File "/ { split($0, a, "\""); file = a[2]; match($0, /lines? [0-9]+/); line = substr($0, RSTART, RLENGTH); sub(/^lines? /, "", line); next }
    /^Warning 8 / { kind = "partial"; ex = ""; grab = 1; next }
    /^Warning 4 / { kind = "fragile"; next }
    kind == "fragile" && /constructors are added to type/ {
      t = $0; sub(/.*to type /, "", t); sub(/\.$/, "", t); sub(/^Austral_core__/, "", t)
      if (t !~ /\./) { m = file; sub(/.*\//, "", m); sub(/\.ml$/, "", m); t = m "." t }
      if (t ~ "^(" types ")$") print "fragile\t" file "\t" line "\t" t
      kind = ""; next
    }
    kind == "partial" && grab && /^Here is an example/ { grab = 2; next }
    kind == "partial" && grab == 2 { print "partial\t" file "\t" line "\t" $0; kind = ""; grab = 0; next }
  ' "$T/$1.log" | sort -u | while IFS=$'\t' read -r k f l d; do
    fn=$(head -n "$l" "$w/$f" | grep -nE '^(let|and)( rec)? [a-z_][A-Za-z0-9_'"'"']*' | tail -n 1 | sed -E 's/^([0-9]+):(let|and)( rec)? ([a-z_][A-Za-z0-9_'"'"']*).*/\4/')
    printf '%s\t%s\t%s\t%s\t%s\n' "$k" "$f" "${fn:-?}" "$l" "$d"
  done | sort -t$'\t' -k1,1 -k2,2 -k4,4n
}

# Keeps the warnings a patch introduced: partial matches whose missing case
# names a new constructor, and every fragile match over the patched types.
new_sites() { # name constructors-regex types-regex
  sites "$1" "$3" | awk -F'\t' -v re="$2" '$1 == "fragile" || $5 ~ re'
}

summary() { # sites-file
  printf 'partial-matches %s\n' "$(grep -c '^partial' "$1")"
  printf 'fragile-matches %s\n' "$(grep -c '^fragile' "$1")"
  printf 'files %s\n' "$(cut -f2 "$1" | sort -u | wc -l)"
  printf 'functions %s\n' "$(cut -f2,3 "$1" | sort -u | wc -l)"
  echo '# partial matches per file'
  grep '^partial' "$1" | cut -f2 | sort | uniq -c | sort -rn | awk '{print $2, $1}'
  echo '# sites'
  cut -f1-3,5 "$1" | sort -u
}

declare -A NEW=(
  [primitive-decimal]='DecimalTy|MonoDecimal'
  [value-parameters]='TyNat|MonoNat|NaturalUniverse|ConcreteNatSpec|QNatSpec'
)
declare -A PATCHED=(
  [primitive-decimal]='Type\.ty|MonoType\.mono_ty'
  [value-parameters]='Type\.ty|Type\.universe|MonoType\.mono_ty|Cst\.typespec|Stages\.Ast\.qtypespec'
)

# Compiles probes/Mono.aum with the HEAD compiler: one generic record and one
# generic function, each instantiated at two type arguments that do not
# change the layout. Prints how many C structs and functions came out.
mono_c() {
  local w="$T/head"
  mkdir -p "$w"
  git -C "$ROOT" archive HEAD | tar -x -C "$w"
  (cd "$w" && timeout "$BUILD_TIMEOUT" nix develop "$ROOT" "${NIX_FLAGS[@]}" --command \
    bash -c 'python3 concat_builtins.py && dune build ./bin/austral.exe 2>&1') > "$T/head.log" \
    || { echo "build head failed, log: $(tail -n 3 "$T/head.log")" >&2; return 1; }
  "$w/_build/default/bin/austral.exe" compile "$HERE/probes/Mono.aum" \
    --entrypoint=Mono:main --target-type=c --output="$T/mono.c" || return 1
  cc -w -o "$T/mono" "$T/mono.c" || return 1
  "$T/mono"; echo "exit $?"
  printf 'structs-with-only-units %s\n' "$(grep -cE '^typedef struct mono_[0-9]+ \{au_int64_t units;\} mono_[0-9]+;$' "$T/mono.c")"
  printf 'function-monomorphs-of-raw %s\n' "$(grep -c 'Function monomorph: raw\[' "$T/mono.c")"
}

# Runs menhir's LR construction on lib/Parser.mly at HEAD, and with type
# arguments widened to a literal or to a full expression (patches/grammar-*,
# on top of value-parameters.patch). Prints the conflicts of each grammar.
# Menhir stops later at code generation for lack of types; the conflict
# report comes before that.
grammar() {
  local v w
  for v in head literal expression; do
    w="$T/grammar-$v"
    mkdir -p "$w"
    git -C "$ROOT" archive HEAD lib | tar -x -C "$w"
    if [[ $v != head ]]; then
      (cd "$w" && patch -s -p1 < "$HERE/patches/value-parameters.patch" \
        && patch -s -p1 < "$HERE/patches/grammar-$v.patch") || return 1
    fi
    (cd "$w/lib" && nix develop "$ROOT" "${NIX_FLAGS[@]}" --command \
      menhir --explain --base "$w/out" Parser.mly) > /dev/null 2>&1
    [[ -f $w/out.conflicts ]] || return 1
    echo "# $v"
    grep -E '^\*\* (Conflict|Tokens? involved)' "$w/out.conflicts" \
      | sed -E 's/ in state [0-9]+//' | paste - - | sort | uniq -c | sed 's/^ *//'
  done
}

check() { # name, with $T/<name>.txt already written
  if ((RECORD)); then
    cp "$T/$1.txt" "$HERE/expected/$1.txt"; echo "recorded $1"
  elif diff -u "$HERE/expected/$1.txt" "$T/$1.txt"; then
    pass=$((pass + 1)); echo "match $1"
  else
    fail=$((fail + 1)); echo "MISMATCH $1"
  fi
}

mkdir -p "$HERE/expected"
pass=0 fail=0 skipped=0
for name in primitive-decimal value-parameters; do
  if ! build "$name" "$HERE/patches/$name.patch"; then
    skipped=$((skipped + 1)); continue
  fi
  new_sites "$name" "${NEW[$name]}" "${PATCHED[$name]}" > "$T/$name.sites"
  summary "$T/$name.sites" > "$T/$name.txt"
  check "$name"
done
if mono_c > "$T/mono-c.txt"; then
  check mono-c
else
  skipped=$((skipped + 1)); echo "skipped mono-c"
fi
if grammar > "$T/grammar.txt"; then
  check grammar
else
  skipped=$((skipped + 1)); echo "skipped grammar"
fi

((RECORD)) && exit 0
echo "match $pass / mismatch $fail / skipped $skipped"
((fail == 0 && skipped == 0))
