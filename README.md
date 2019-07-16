# Austral

[![Build Status](https://travis-ci.com/austral/austral.svg?branch=master)](https://travis-ci.com/austral/austral)

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
  types of resources they can access.

- **Typeclasses**: typeclasses, borrowed from Haskell, allow for bounded ad-hoc
  polymorphism.

- **Safe Arithmetic**: Austral has well-defined semantics for all arithmetic
  operations on numeric types. There are distinct integer types for
  trap-on-overflow arithmetic and modular arithmetic, as in Ada.

- **Algebraic Data Types**: tuples and disjunctions, as in ML or Haskell, let us
  write simple and expressive inductive type definitions.

Anti-features:

- No garbage collection.
- No destructors.
- No exceptions (and no surprise control flow).
- No implicit function calls.
- No implicit type conversions.
- No global state.
- No reflection.
- No Java-style @Annotations.
- No type inference, type information flows in one direction.
- No uninitialized variables.

## Building

Building the `austral` executable requires `make`, [MLton][mlton], and a C
compiler for building the resulting output.

On Debian/Ubuntu, you can install the requisite dependencies with:

```bash
$ sudo apt-get install -y make mlton gcc
```

To build the executable:

```bash
$ make austral
```

To compile in "development mode" (which does not produce an executable binary)
we use [SML/NJ][smlnj], which is faster than MLton and features a REPL. You can
install it in Debian/Ubuntu with:

```bash
$ sudo apt-get install -y smlnj
```

Then, to compile the source code and load up a REPL, run:

```bash
$ make
```

Finally, to run the tests, you need both MLton and SML/NJ. Run this:

```bash
$ ./run-tests.sh
```

# License

Copyright 2018–2019 Fernando Borretti.

Licensed under the GPLv3 license. See the COPYING file for details.

[mlton]: http://www.mlton.org/
[smlnj]: https://www.smlnj.org/
