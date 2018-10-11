#!/usr/bin/env bash

make clean
make boreal
make test

echo "test.au"
./boreal test/valid/test.au --output=test/valid/test.cpp
clang++ -shared -fPIC -std=c++11 test/valid/test.cpp

echo "fib.au"
./boreal test/valid/fib.au --output=test/valid/fib.cpp
