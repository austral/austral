#!/usr/bin/env bash
# usage: run-tests.sh <path-to-austral-binary>
#
# Each tests/NAME.aum has exactly one sibling that says what must happen:
#   NAME.expected  compiles, exits 0, and stdout equals the file
#   NAME.error     fails to compile, and the compiler output contains the file's text
#   NAME.trap      compiles, exits nonzero, and its output contains the file's text
# An optional NAME.with lists modules under tests/modules/ to compile before NAME.aum.
set -u

if [ $# -ne 1 ] || [ ! -x "$1" ]; then
  echo "usage: $0 <path-to-austral-binary>" >&2
  exit 2
fi

austral=$(realpath "$1")
tests=$(dirname "$(realpath "$0")")/tests
pass=0
fail=0

report() {
  if [ "$1" = ok ]; then
    pass=$((pass + 1))
    echo "ok    $2"
  else
    fail=$((fail + 1))
    echo "FAIL  $2: $3"
  fi
}

for src in "$tests"/*.aum; do
  name=$(basename "$src" .aum)
  module=$(sed -n 's/^module body \([A-Za-z0-9_.]*\) is.*/\1/p' "$src" | head -1)
  # The compiler writes error.html and calltree.html into the current directory,
  # and leaves its intermediate austral_XXXXXX.c in $TMPDIR.
  work=$(mktemp -d)
  with=()
  if [ -f "$tests/$name.with" ]; then
    cp -r "$tests/modules" "$work/modules"
    read -r -a with < "$tests/$name.with"
  fi
  compile_output=$(cd "$work" && TMPDIR="$work" "$austral" compile ${with[@]+"${with[@]}"} "$src" --entrypoint="$module:main" --output="$work/prog" 2>&1)
  compile_status=$?

  if [ -f "$tests/$name.error" ]; then
    want=$(cat "$tests/$name.error")
    if [ $compile_status -eq 0 ]; then
      report bad "$name" "compiled, but a compile error was expected"
    elif printf '%s' "$compile_output" | grep -qF -- "$want"; then
      report ok "$name"
    else
      report bad "$name" "compile error does not contain: $want"
    fi
  elif [ $compile_status -ne 0 ]; then
    report bad "$name" "did not compile: $(printf '%s' "$compile_output" | sed -n '/Description:/{n;p;}' | head -1)"
  elif [ -f "$tests/$name.trap" ]; then
    want=$(cat "$tests/$name.trap")
    run_output=$("$work/prog" 2>&1)
    run_status=$?
    if [ $run_status -eq 0 ]; then
      report bad "$name" "exited 0, but a trap was expected"
    elif printf '%s' "$run_output" | grep -qF -- "$want"; then
      report ok "$name"
    else
      report bad "$name" "abort message does not contain: $want"
    fi
  elif [ -f "$tests/$name.expected" ]; then
    "$work/prog" > "$work/stdout" 2> "$work/stderr"
    run_status=$?
    if [ $run_status -ne 0 ]; then
      report bad "$name" "exited $run_status: $(head -1 "$work/stderr")"
    elif cmp -s "$work/stdout" "$tests/$name.expected"; then
      report ok "$name"
    else
      report bad "$name" "stdout differs: $(diff "$tests/$name.expected" "$work/stdout" | head -3 | tr '\n' ' ')"
    fi
  else
    report bad "$name" "no .expected, .error or .trap file"
  fi
  rm -rf "$work"
done

echo "pass $pass / fail $fail"
[ $fail -eq 0 ]
