#!/usr/bin/env bash
# Re-runs every probe and compares its output with expected/. See README.md.
#   run.sh            compare
#   run.sh --record   rewrite expected/ from the current outputs
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../../.." && pwd)
NIXPKGS=github:NixOS/nixpkgs/674a2028661e550a97400c1cc992b9cca9a668a1
LIBDFP_URL=https://github.com/libdfp/libdfp/archive/e898f1868e3fb0b703e508857bae0df08d4268d5.tar.gz
LIBDFP_SHA256=bfabeb4811d904b858fb414edf181da9b054791654576f48fbfd0bc4f5589163
INTEL_URL=https://netlib.org/misc/intel/IntelRDFPMathLib20U5.tar.gz
INTEL_SHA256=85dafd70f0fe2a8da218ade4233fca9d3228b0b04cd6d8527f7499926be01037
DEP_BUILD_TIMEOUT=600
ZUSTRAL_BUILD_TIMEOUT=${ZUSTRAL_BUILD_TIMEOUT:-600}

RECORD=0
[[ ${1:-} == --record ]] && RECORD=1

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

declare -A DEP DEP_ERR

nix_out() { # attr -> store path, or the last error line on stderr
  local p
  if p=$(nix build --no-link --print-out-paths "$NIXPKGS#$1" 2>"$T/nix.err"); then
    echo "$p"
  else
    tail -n 1 "$T/nix.err" >&2
    return 1
  fi
}

fail_tail() { tail -n 1 "$1" | tr -d '\r'; }

build_tarball() { # name url sha256 build-script nix-attrs...
  local name=$1 url=$2 sha=$3 script=$4; shift 4
  local d="$T/dep-$name" pkgs=() a
  mkdir -p "$d/src" "$d/build"
  for a in "$@"; do pkgs+=("$NIXPKGS#$a"); done
  if ! curl -fsSL -o "$d/src.tar.gz" "$url" 2>"$d/log"; then
    DEP_ERR[$name]="取得: $(fail_tail "$d/log")"; return
  fi
  if ! echo "$sha  $d/src.tar.gz" | sha256sum -c --status; then
    DEP_ERR[$name]="sha256: $(sha256sum "$d/src.tar.gz" | cut -d' ' -f1) が $sha と異なる"; return
  fi
  tar -xzf "$d/src.tar.gz" -C "$d/src" --strip-components=1
  (cd "$d/build" && timeout "$DEP_BUILD_TIMEOUT" nix shell "${pkgs[@]}" --command bash -c "$script") >"$d/log" 2>&1
  case $? in
    0) DEP[$name]=$d ;;
    124) DEP_ERR[$name]="上限 $DEP_BUILD_TIMEOUT 秒を超えた" ;;
    *) DEP_ERR[$name]="$(grep -o '^STAGE=.*' "$d/log" | tail -n 1 | cut -d= -f2): $(fail_tail "$d/log")" ;;
  esac
}

fetch() {
  local p
  case $1 in
    gcc|clang)
      p=$(nix_out "$1^out" 2>"$T/err") && DEP[$1]=$p || DEP_ERR[$1]=$(cat "$T/err") ;;
    clang-dfp)
      if ! need clang; then DEP_ERR[$1]="clang: ${DEP_ERR[clang]}"; return; fi
      echo '_Decimal128 x;' >"$T/dfp.c"
      if "${DEP[clang]}/bin/clang" -fsyntax-only "$T/dfp.c" 2>"$T/err"; then
        DEP[$1]=ok
      else
        DEP_ERR[$1]=$(grep -o 'error:.*' "$T/err" | head -n 1)
      fi ;;
    mpdecimal)
      local dev out
      if dev=$(nix_out mpdecimal.dev 2>"$T/err") && out=$(nix_out mpdecimal.out 2>"$T/err"); then
        DEP[$1]="$dev $out"
      else
        DEP_ERR[$1]=$(cat "$T/err")
      fi ;;
    libdfp)
      build_tarball libdfp "$LIBDFP_URL" "$LIBDFP_SHA256" \
        "echo STAGE=configure; ../src/configure CFLAGS='-m64 -O2' && echo STAGE=make && make -j8" \
        gcc gnumake binutils gawk python3 ;;
    intel-dfp)
      build_tarball intel-dfp "$INTEL_URL" "$INTEL_SHA256" \
        "echo STAGE=make; cd ../src/LIBRARY && make -j8 CC=gcc CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0 UNCHANGED_BINARY_FLAGS=0" \
        gcc gnumake ;;
    zustral)
      timeout "$ZUSTRAL_BUILD_TIMEOUT" nix develop "$ROOT" --command make -C "$ROOT" >"$T/zustral.log" 2>&1
      case $? in
        0) DEP[$1]=$ROOT/austral ;;
        124) DEP_ERR[$1]="上限 $ZUSTRAL_BUILD_TIMEOUT 秒を超えた" ;;
        *) DEP_ERR[$1]=$(fail_tail "$T/zustral.log") ;;
      esac ;;
  esac
}

need() {
  [[ -v DEP[$1] ]] && return 0
  [[ -v DEP_ERR[$1] ]] && return 1
  fetch "$1"
  [[ -v DEP[$1] ]]
}

FAMILIES=(i128 d128 mpd intel libdfp)
PROBES=()
for fam in "${FAMILIES[@]}"; do
  for cc in gcc clang; do
    for opt in O0 O2; do PROBES+=("$fam@$cc-$opt"); done
  done
done
PROBES+=(ffi ffi-unit-return)

deps_of() { # probe id -> dependency names, in the order they are reported
  local fam=${1%@*} cc=${1#*@}
  cc=${cc%-*}
  case $fam in
    i128) echo "$cc" ;;
    d128) [[ $cc == gcc ]] && echo gcc || echo clang clang-dfp ;;
    mpd) echo "$cc mpdecimal" ;;
    intel) echo "$cc intel-dfp" ;;
    libdfp) [[ $cc == gcc ]] && echo gcc libdfp || echo clang clang-dfp libdfp ;;
    ffi) echo zustral gcc mpdecimal ;;
    ffi-unit-return) echo zustral ;;
  esac
}

build() { # id dir -> $dir/bin
  local id=$1 d=$2 fam=${1%@*} v=${1#*@}
  local cc=${v%-*} opt=${v#*-}
  local CC=${DEP[$cc]:-}/bin/$cc
  case $fam in
    i128) "$CC" "-$opt" -fwrapv "$HERE/probes/i128.c" -o "$d/bin" ;;
    d128) "$CC" "-$opt" -fwrapv "$HERE/probes/d128.c" -lm -o "$d/bin" ;;
    mpd)
      local dev=${DEP[mpdecimal]% *} lib=${DEP[mpdecimal]#* }/lib
      "$CC" "-$opt" -fwrapv -I"$dev/include" "$HERE/probes/mpd.c" -L"$lib" -Wl,-rpath,"$lib" -lmpdec -lm -o "$d/bin" ;;
    intel)
      local s=${DEP[intel-dfp]}/src/LIBRARY
      "$CC" "-$opt" -fwrapv -I"$s/src" "$HERE/probes/intel.c" "$s/libbid.a" -lm -o "$d/bin" ;;
    libdfp)
      local s=${DEP[libdfp]}/src b=${DEP[libdfp]}/build
      "$CC" "-$opt" -fwrapv -D__STDC_WANT_DEC_FP__ -D__STDC_WANT_IEC_60559_DFP_EXT__ -I"$s/dfp" -I"$s/include" "$HERE/probes/libdfp.c" \
        -L"$b" -Wl,-rpath,"$b" -ldfp -lm -o "$d/bin" ;;
    ffi)
      local dev=${DEP[mpdecimal]% *} lib=${DEP[mpdecimal]#* }/lib
      # The compiler writes error.html into its working directory on failure, so it runs in the temp dir.
      cp "$HERE/probes/ffi/Probe.aum" "$d/" &&
        (cd "$d" && "${DEP[zustral]}" compile Probe.aum --entrypoint=Probe:main --target-type=c --output=ffi.c) &&
        "${DEP[gcc]}/bin/gcc" -fwrapv -Wno-builtin-declaration-mismatch "$d/ffi.c" "$HERE/probes/ffi/shim.c" \
          -I"$dev/include" -L"$lib" -Wl,-rpath,"$lib" -lmpdec -lm -o "$d/bin" ;;
  esac
}

execute() { # id dir out
  if [[ $1 == ffi-unit-return ]]; then
    cp "$HERE/probes/ffi/UnitReturn.aum" "$2/"
    (cd "$2" && "${DEP[zustral]}" compile UnitReturn.aum --entrypoint=UnitReturn:main --target-type=c --output=out.c)
  else
    (cd "$HERE" && "$2/bin")
  fi >"$3" 2>&1
  echo "exit=$?" >>"$3"
}

PASS=0 FAIL=0 SKIP=0
declare -A WRITTEN

if [[ $RECORD == 1 ]]; then
  mkdir -p "$HERE/expected"
  rm -f "$HERE"/expected/*@*.txt
fi

for id in "${PROBES[@]}"; do
  fam=${id%@*}
  d="$T/$id"
  mkdir -p "$d"

  missing=
  for dep in $(deps_of "$id"); do
    if ! need "$dep"; then missing="$dep: ${DEP_ERR[$dep]}"; break; fi
  done
  if [[ -n $missing ]]; then
    echo "未実行  $id: $missing"; SKIP=$((SKIP + 1)); continue
  fi

  if ! build "$id" "$d" >"$d/build.log" 2>&1; then
    echo "不一致  $id: ビルドに失敗した"; tail -n 20 "$d/build.log" | sed 's/^/    /'
    FAIL=$((FAIL + 1)); continue
  fi
  execute "$id" "$d" "$d/out1"
  execute "$id" "$d" "$d/out2"

  family_rec="$HERE/expected/$fam.txt"
  variant_rec=
  [[ $id == *@* ]] && variant_rec="$HERE/expected/$id.txt"
  if [[ $RECORD == 1 ]]; then
    if ! cmp -s "$d/out1" "$d/out2"; then
      echo "不一致  $id: 2回の実行の出力が異なる"; diff -u --label 1回目 --label 2回目 "$d/out1" "$d/out2" | sed 's/^/    /'
      FAIL=$((FAIL + 1)); continue
    fi
    if [[ ! -v WRITTEN[$fam] ]]; then
      cp "$d/out1" "$family_rec"; WRITTEN[$fam]=1
    elif ! cmp -s "$d/out1" "$family_rec"; then
      cp "$d/out1" "$variant_rec"
    fi
  fi

  rec=$family_rec; mark="一致    $id"
  if [[ -n $variant_rec && -f $variant_rec ]]; then rec=$variant_rec; mark="一致*  $id（変種別の記録）"; fi
  if [[ ! -f $rec ]]; then
    echo "不一致  $id: 記録ファイル ${rec#"$HERE/"} がない"; sed 's/^/    /' "$d/out1"
    FAIL=$((FAIL + 1)); continue
  fi
  ok=1
  for n in 1 2; do
    if ! cmp -s "$rec" "$d/out$n"; then
      [[ $ok == 1 ]] && echo "不一致  $id"
      ok=0
      diff -u --label "${rec#"$HERE/"}" --label "$n回目の出力" "$rec" "$d/out$n" | sed 's/^/    /'
    fi
  done
  if [[ $ok == 1 ]]; then
    echo "$mark"; PASS=$((PASS + 1))
    [[ $id == ffi ]] && sed 's/^/    /' "$d/out1"
  else
    FAIL=$((FAIL + 1))
  fi
done

echo "一致 $PASS / 不一致 $FAIL / 未実行 $SKIP"
[[ $FAIL == 0 ]]
