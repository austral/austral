#!/usr/bin/env bash
# Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
# See LICENSE file for details.
#
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

set -euxo pipefail

dune build

function compile() {
    ./austral compile $1/$2.aui,$1/$2.aum --entrypoint=Example.$2:main --output=testbin
    ./testbin > actual.txt
    echo -n -e "$3" > expected.txt
    diff actual.txt expected.txt
    rm testbin
    rm actual.txt
    rm expected.txt
}

compile examples/ffi FFI "aHello, world!\n"
compile examples/fib Fibonacci ""
compile examples/generic-record GenericRecord ""
compile examples/generic-union GenericUnion ""
compile examples/haversine Haversine ""
compile examples/hello-world HelloWorld "Hello, world!\n"
compile examples/identity Identity ""
compile examples/memory Memory ""
compile examples/named-argument NamedArgument ""
compile examples/record Record ""
compile examples/union Union ""
