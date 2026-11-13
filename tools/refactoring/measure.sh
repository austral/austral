#!/usr/bin/env bash
# Counts, per source file of the compiler, the things the refactoring plan
# wants to shrink. See docs/research/functional-domain-refactoring/README.md.
#   measure.sh            compare with expected/
#   measure.sh --record   rewrite expected/ from the current tree
#   measure.sh --check    fail only if a total went up; print what changed
#   measure.sh --fragile  also count fragile matches (needs a compiler build)
# Environment for --fragile:
#   ZUSTRAL_NIX_FLAGS      extra flags for `nix develop`
#   ZUSTRAL_BUILD_TIMEOUT  seconds allowed for the build (default 900)
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
RECORD=0 FRAGILE=0 CHECK=0
for a in "$@"; do
  case $a in
    --record) RECORD=1 ;;
    --fragile) FRAGILE=1 ;;
    --check) CHECK=1 ;;
  esac
done

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

# The core is lib/, the command line is cli/. BuiltInModules.ml is generated
# from lib/builtin/ by concat_builtins.py.
sources() { (cd "$ROOT" && ls lib/*.ml lib/*.mll lib/*.mly cli/*.ml | grep -v '^lib/BuiltInModules\.ml$'); }

count() { # regex file
  grep -cE -- "$1" "$ROOT/$2" || true
}

# Lines inside a type or exception definition that use string as a field or a
# constructor argument. A definition starts at `type` or `exception` and ends
# at the next item that is not a type (`and` continues a type definition).
count_type_strings() { # file
  awk '
    /^[ \t]*(type|exception)[ \t]/ { in_type = 1 }
    /^[ \t]*(let|val|external|open|include|module|end|class)([ \t]|$)/ { in_type = 0 }
    in_type && /(of|\*|:) string( |;|$|\))/ { n++ }
    END { print n + 0 }
  ' "$ROOT/$1"
}

# One row per file. Columns:
#   raise     places that raise: raise_error, type_error and the other helpers of a category, internal_err,
#             failwith, raise, assert false
#   internal  the part of raise that is internal_err: a state the code believes cannot happen
#   adhoc     errors with no name: err with a bare string. R11 removed err, so this stays at 0
#   catch     places that catch or re-label: try, adorn_error_with_*, with_errors
#   state     mutable state: creating a ref cell, a Hashtbl or a top-level Buffer, declaring a mutable field
#   io        input, output and process control: channels, printing, anything in Sys, Unix or
#             Filename, exit, and the compiler's own file and process helpers
#   wildcard  match arms that catch everything: | _ ->
#   strings   string used as a field or constructor argument in a type definition
metrics() {
  printf 'file\tlines\traise\tcatch\tstate\tio\twildcard\tstrings\tinternal\tadhoc\n'
  sources | while read -r f; do
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$f" \
      "$(wc -l < "$ROOT/$f")" \
      "$(count 'raise_error|(^|[^_a-zA-Z])(parse|cli|type|linearity|declaration|entrypoint|generic)_error(_at)? |internal_err|failwith|(^|[^_a-zA-Z])raise |assert false' "$f")" \
      "$(count '(^|[^_a-zA-Z])try($|[^_a-zA-Z])|adorn_error_with_[a-z_]+|with_errors' "$f")" \
      "$(count '= *ref ([a-z0-9("\[]|\[\])|^\s*ref \(|Hashtbl\.(create|add|replace)|^\s*mutable [a-z_]+ *:|^let [a-z_]+ *(: *[A-Za-z_.]+ *)?= *Buffer\.create' "$f")" \
      "$(count 'open_out|open_in|(^|[^_a-zA-Z])(print_(string|endline|newline|int|char|float|bytes)|prerr_[a-z]+|read_line|input_(line|char|value|byte)|really_input[a-z_]*|output_(string|char|bytes|value|byte))|Printf\.[a-z]*printf|Format\.[a-z]*printf|(^|[^_a-zA-Z.])exit( |$|\()|std(in|out|err)[^_a-zA-Z]|In_channel|Out_channel|Sys\.|(^|[^_a-zA-Z])Unix($|[^_a-zA-Z])|Filename\.|write_string_to_file|read_file_to_string|run_command' "$f")" \
      "$(count '^\s*\| _ ->' "$f")" \
      "$(count_type_strings "$f")" \
      "$(count 'internal_err' "$f")" \
      "$(count '(^|[^_a-zA-Z])err [("]' "$f")"
  done
}

totals() { # metrics-file
  # coreio is the part of io that is in the core (lib/). The plan wants it at 0.
  awk -F'\t' 'NR > 1 { for (i = 2; i <= NF; i++) s[i] += $i; n++; if ($1 ~ /^lib\//) coreio += $6 }
    END { printf "files %d\nlines %d\nraise %d\ncatch %d\nstate %d\nio %d\nwildcard %d\nstrings %d\ninternal %d\nadhoc %d\ncoreio %d\n", n, s[2], s[3], s[4], s[5], s[6], s[7], s[8], s[9], s[10], coreio }' "$1"
}

# Fragile matches: a match that a new constructor would fall into without a
# compile error. The OCaml compiler reports each one as warning 4.
fragile() {
  local w="$T/tree"
  mkdir -p "$w"
  (cd "$ROOT" && git ls-files -z lib cli bin dune-project concat_builtins.py austral.opam | xargs -0 tar -cf - ) | tar -xf - -C "$w"
  read -r -a nix_flags <<< "${ZUSTRAL_NIX_FLAGS:-}"
  local build='python3 concat_builtins.py && dune build --root . --profile release ./lib ./cli 2>&1'
  if [[ -n ${IN_NIX_SHELL:-} ]]; then
    (cd "$w" && OCAMLPARAM="_,w=+4,warn-error=-a" timeout "${ZUSTRAL_BUILD_TIMEOUT:-900}" bash -c "$build") > "$T/build.log"
  else
    (cd "$w" && OCAMLPARAM="_,w=+4,warn-error=-a" timeout "${ZUSTRAL_BUILD_TIMEOUT:-900}" \
      nix develop "$ROOT" "${nix_flags[@]}" --command bash -c "$build") > "$T/build.log"
  fi
  local rc=$?
  ((rc == 0)) || { echo "build failed ($rc): $(tail -n 3 "$T/build.log")" >&2; return 1; }
  awk '
    /^File "/ { split($0, a, "\""); file = a[2]; next }
    /^Warning 4 / { want = 1; next }
    want && /constructors are added to type/ {
      t = $0; sub(/.*to type /, "", t); sub(/\.$/, "", t); sub(/^Austral_(core|cli)__/, "", t)
      want = 0
      # A type named without its module is defined in the file being compiled.
      if (t !~ /\./) { m = file; sub(/.*\//, "", m); sub(/\.ml$/, "", m); t = m "." t }
      print file "\t" t
    }' "$T/build.log" | sort | uniq -c | awk '{ print $2 "\t" $3 "\t" $1 }'
}

check() { # name
  if ((RECORD)); then
    cp "$T/$1" "$HERE/expected/$1"; echo "recorded $1"
  elif diff -u "$HERE/expected/$1" "$T/$1"; then
    echo "match $1"
  else
    echo "MISMATCH $1"; failed=1
  fi
}

failed=0
metrics > "$T/metrics.tsv"
totals "$T/metrics.tsv" > "$T/totals.txt"
if ((CHECK)); then
  # The size of the code may grow; the things the plan wants to shrink may not.
  awk 'NR == FNR { was[$1] = $2; next }
    $1 == "files" || $1 == "lines" { next }
    $2 != was[$1] { printf "%s %d -> %d\n", $1, was[$1], $2; changed = 1 }
    $2 > was[$1] { up = 1 }
    END { if (!changed) print "no total changed"; exit up }' "$HERE/expected/totals.txt" "$T/totals.txt" || { echo "a total went up"; exit 1; }
  exit 0
fi
check metrics.tsv
check totals.txt
if ((FRAGILE)); then
  if fragile > "$T/fragile.tsv"; then
    awk -F'\t' '{ s += $3; f[$1] = 1; t[$2] += $3 } END { print "fragile-matches " s; print "files " length(f); for (k in t) print "type " k " " t[k] }' "$T/fragile.tsv" | sort > "$T/fragile-totals.txt"
    check fragile.tsv
    check fragile-totals.txt
  else
    failed=1
  fi
fi
exit "$failed"
