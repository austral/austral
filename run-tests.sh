#!/usr/bin/env bash
set -euxo pipefail

function compile() {
    ./austral --module=$1 --entrypoint=Example.$2:Main > code.cpp
    g++ code.cpp -o testbin
    rm code.cpp
    ./testbin
    rm testbin
}

compile examples/constant/Const Constant
compile examples/identity/Identity Identity
compile examples/named-argument/NamedArgument NamedArgument
compile examples/memory/Memory Memory
#compile examples/record/Record Record
#compile examples/union/Union Union
