#!/usr/bin/env bash
set -euxo pipefail

opam exec -- dune build

function compile() {
    ./_build/default/bin/austral.exe compile --module=$1/$2 --entrypoint=Example.$2:Main --output=code.c
    gcc -Wno-builtin-declaration-mismatch lib/prelude.c code.c -o testbin
    ./testbin > actual.txt
    echo -n -e "$3" > expected.txt
    diff actual.txt expected.txt
    rm code.c
    rm testbin
    rm actual.txt
    rm expected.txt
}

compile examples/constant Constant ""
compile examples/identity Identity ""
compile examples/ffi FFI "aHello, world!\n"
compile examples/fib Fibonacci ""
compile examples/named-argument NamedArgument ""
compile examples/memory Memory ""
compile examples/type-alias TypeAlias ""
compile examples/record Record ""
compile examples/union Union ""
compile examples/concrete-typeclass ConcreteTypeclass ""
compile examples/generic-record GenericRecord ""
compile examples/generic-union GenericUnion ""
compile examples/generic-typeclass GenericTypeclass ""
compile examples/string String "HHello, world!\nA\nA\nA\nA\nA\n    B\nA\n    B\nC\n"
compile examples/box Box "aaaaa"
compile examples/pointer-to-record PTR "a"
compile examples/ref-to-record RTR "aa"
compile examples/array Array "TFT"
compile examples/buffer Buffer "ae"
compile examples/haversine Haversine ""
compile examples/either Either "aa"
