#!/usr/bin/env bash

make boreal
rm -rf vendor
make test
./boreal test/valid/test.au --output=test/valid/test.cpp
