# Austral

![Build status badge](https://github.com/austral/austral/actions/workflows/build-and-test.yml/badge.svg)

**Not done yet.**

Austral is a new language.

Features:

- **Linear types**: linear types allow resources to be handled in a
  provably-safe manner. Memory can be managed safely and without runtime
  overhead, avoiding double `free()`, use-after-`free` errors, and double fetch
  errors. Other resources like file or database handles can also be handled
  safely.

- **Capabilities**: linear capabilities enable fine-grained permissioned access
  to low-level facilities. Third-party dependencies can be constrained in what
  types of resources they can access. This makes the language less vulnerable to
  [supply chain attacks][sca].

- **Typeclasses**: typeclasses, borrowed from Haskell, allow for bounded ad-hoc
  polymorphism.

- **Safe Arithmetic**: Austral has well-defined semantics for all arithmetic
  operations on numeric types. There are distinct integer types for
  trap-on-overflow arithmetic, modular arithmetic, and saturating arithmetic, as
  in Ada.

- **Algebraic Data Types**: algebraic data types, as in ML or Haskell, with
  exhaustiveness checking.

Anti-features:

- No garbage collection.
- No destructors.
- No exceptions (and no surprise control flow).
- No implicit function calls.
- No implicit type conversions.
- No global state.
- No macros.
- No reflection.
- No Java-style @Annotations.
- No type inference, type information flows in one direction.
- No uninitialized variables.
- No pre/post increment/decrement (`x++` in C).
- No first-class async.
- No function overloading (except through typeclasses, where it is bounded).
- No arithmetic precedence.

## Building

Building the `austral` compiler requires `make` and the `dune `build system
for OCaml, and a C++ compiler for building the resulting output.

Instructions:

1. Install [opam][opam].
2. `opam install dune`
3. `make`

To run the tests:

```bash
$ ./run-tests.sh
```

## Status

1. The bootstrapping compiler, written in OCaml, is implemented. It has a couple
   of limitations, these are to speed up development so I can iterate on a
   working compiler as early as possible:

    1. The compiler outputs templated C++ so I don't have to bother implementing
       a monomorphization step.

    2. The compiler does not support separate compilation. In practice this is
       not a problem: there's not enough Austral code for this to matter.

2. A standard library with a few basic data structures and capability-based
   filesystem access is being designed.

# License

Copyright 2018–2021 Fernando Borretti.

Licensed under the GPLv3 license. See the COPYING file for details.

[opam]: https://opam.ocaml.org/doc/Install.html
[sca]: https://en.wikipedia.org/wiki/Supply_chain_attack
