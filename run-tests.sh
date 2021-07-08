#!/usr/bin/env bash
set -euxo pipefail

function compile() {
    ./austral --module=$1/$2 --entrypoint=Example.$2:Main > code.cpp
    g++ code.cpp -o testbin
    rm code.cpp
    ./testbin
    rm testbin
}

compile examples/constant Constant
compile examples/identity Identity
compile examples/ffi FFI
#compile examples/fib Fibonacci
compile examples/named-argument NamedArgument
compile examples/memory Memory
#compile examples/record Record
compile examples/union Union
#compile examples/concrete-typeclass ConcreteTypeclass
#compile examples/generic-record GenericRecord
#compile examples/generic-union GenericUnion
#compile examples/generic-typeclass GenericTypeclass
