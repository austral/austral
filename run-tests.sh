#!/usr/bin/env bash

make boreal
rm -rf vendor
make test

./boreal test/valid/test.au --output=test/valid/test.cpp
clang++ -shared -fPIC -std=c++11 test/valid/test.cpp
