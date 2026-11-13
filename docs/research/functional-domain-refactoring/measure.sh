#!/usr/bin/env bash
# Counts, per source file of the compiler, the things the refactoring plan
# wants to shrink. See README.md.
#   measure.sh            compare with expected/
#   measure.sh --record   rewrite expected/ from the current tree
#   measure.sh --fragile  also count fragile matches (needs a compiler build)
# Environment for --fragile:
#   ZUSTRAL_NIX_FLAGS      extra flags for `nix develop`
#   ZUSTRAL_BUILD_TIMEOUT  seconds allowed for the build (default 900)
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../../.." && pwd)
RECORD=0 FRAGILE=0
for a in "$@"; do
  case $a in
    --record) RECORD=1 ;;
    --fragile) FRAGILE=1 ;;
  esac
done

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

# BuiltInModules.ml is generated from lib/builtin/ by concat_builtins.py.
sources() { (cd "$ROOT" && ls lib/*.ml lib/*.mll lib/*.mly | grep -v '^lib/BuiltInModules\.ml$'); }

count() { # regex file
  grep -cE -- "$1" "$ROOT/$2" || true
}

# One row per file. Columns:
#   raise     places that raise: austral_raise, err, internal_err, Errors.x, failwith, raise, assert false
#   catch     places that catch or re-label: try, adorn_error_with_*, with_errors
#   state     mutable state: ref cells, Hashtbl, mutable fields
#   io        input, output and process control: files, printing, Sys.command, exit
#   wildcard  match arms that catch everything: | _ ->
#   strings   string used as a field or constructor argument in a type definition
metrics() {
  printf 'file\tlines\traise\tcatch\tstate\tio\twildcard\tstrings\n'
  sources | while read -r f; do
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$f" \
      "$(wc -l < "$ROOT/$f")" \
      "$(count 'austral_raise|(^|[^_a-zA-Z])err [("]|internal_err|Errors\.[a-z_]+|failwith|(^|[^_a-zA-Z])raise |assert false' "$f")" \
      "$(count '(^|[^_a-zA-Z])try($|[^_a-zA-Z])|adorn_error_with_[a-z_]+|with_errors' "$f")" \
      "$(count '= ref |ref \(|Hashtbl\.|(^|[^_a-zA-Z])mutable ' "$f")" \
      "$(count 'open_out|open_in|print_string|print_endline|prerr_|Printf\.e?printf|Sys\.command|Unix\.|(^|[^_a-zA-Z])exit |output_string|write_string_to_file|read_file_to_string|run_command|Sys\.remove|Filename\.temp' "$f")" \
      "$(count '^\s*\| _ ->' "$f")" \
      "$(count '(of|\*|:) string( |;|$|\))' "$f")"
  done
}

totals() { # metrics-file
  awk -F'\t' 'NR > 1 { for (i = 2; i <= NF; i++) s[i] += $i; n++ }
    END { printf "files %d\nlines %d\nraise %d\ncatch %d\nstate %d\nio %d\nwildcard %d\nstrings %d\n", n, s[2], s[3], s[4], s[5], s[6], s[7], s[8] }' "$1"
}

# Fragile matches: a match that a new constructor would fall into without a
# compile error. The OCaml compiler reports each one as warning 4.
fragile() {
  local w="$T/tree"
  mkdir -p "$w"
  (cd "$ROOT" && git ls-files -z lib bin dune-project concat_builtins.py austral.opam | xargs -0 tar -cf - ) | tar -xf - -C "$w"
  read -r -a nix_flags <<< "${ZUSTRAL_NIX_FLAGS:-}"
  (cd "$w" && OCAMLPARAM="_,w=+4,warn-error=-a" timeout "${ZUSTRAL_BUILD_TIMEOUT:-900}" \
    nix develop "$ROOT" "${nix_flags[@]}" --command \
    bash -c 'python3 concat_builtins.py && dune build --root . --profile release ./lib 2>&1') > "$T/build.log"
  local rc=$?
  ((rc == 0)) || { echo "build failed ($rc): $(tail -n 3 "$T/build.log")" >&2; return 1; }
  awk '
    /^File "/ { split($0, a, "\""); file = a[2]; next }
    /^Warning 4 / { want = 1; next }
    want && /constructors are added to type/ {
      t = $0; sub(/.*to type /, "", t); sub(/\.$/, "", t); sub(/^Austral_core__/, "", t)
      want = 0
      # Matches written by the sexp deriver are not ours to fix.
      if (t ~ /^Sexplib/) next
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
