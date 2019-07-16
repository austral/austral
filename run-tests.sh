#!/usr/bin/env bash
set -e
set -o pipefail

make austral
make test
