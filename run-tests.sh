#!/usr/bin/env bash

set -euxo pipefail

# Ensure the compiler binary is up to date.
make
# Run the OCaml tests.
make test
# Run the end-to-end tests.
python3 test-programs/runner.py
# Run the examples.
./run-examples.sh
# Build the stdlib tests.
make -C standard
# Run the stdlib tests.
./standard/test_bin
