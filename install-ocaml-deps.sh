#!/usr/bin/env bash
# Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
# See LICENSE file for details.
#
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

# Install a specific version of ppxlib.
opam install -y ppxlib.0.25.0
# Install dependencies.
opam install -y dune ppx_deriving ounit2 menhir sexplib ppx_sexp_conv zarith
