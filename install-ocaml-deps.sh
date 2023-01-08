#!/usr/bin/env bash

# Install a specific version of ppxlib.
opam install -y ppxlib.0.25.0
# Install dependencies.
opam install -y dune ppx_deriving ounit2 menhir sexplib ppx_sexp_conv
