#!/usr/bin/env bash
set -euxo pipefail

function compile() {
    ./austral --module=$1/$2 --entrypoint=Example.$2:Main > code.cpp
    g++ code.cpp -o testbin
    ./testbin > actual.txt
    echo -n -e "$3" > expected.txt
    diff actual.txt expected.txt
    rm code.cpp
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
compile examples/string String "HHello, world!\n"
compile examples/box Box "aaa"
compile examples/pointer-to-record PTR "a"
compile examples/ref-to-record RTR "aa"
compile examples/array Array "TFT"
