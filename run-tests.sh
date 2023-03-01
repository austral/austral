#!/usr/bin/env bash
# Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
# See LICENSE file for details.
#
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

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
make -C standard clean
make -C standard
# Run the stdlib tests.
./standard/test_bin
