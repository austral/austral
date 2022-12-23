# Contributing

This document describes how and where to contribute to Austral.

Communication channels:

- [GithHub Issues][issues]

[issues]: https://github.com/austral/austral/issues

## How to Contribute

For bug reports, questions, and feature requests, please open an issue
[here][issues].

# Development Environment

Short version:

```bash
$ sudo apt-get install opam
$ git clone git@github.com:austral/austral.git
$ cd austral
$ ./install-ocaml-deps.sh
$ make
$ ./run-tests.sh
```

## Contribution Areas

This section documents the areas where contributions are currently needed.

### Compiler Diagnostics

[WIP]

### Tutorials

- **Where:** https://github.com/austral/austral.github.io/tree/master/tutorial

The [tutorials][tut] are likely to be people's first real encounter with the
language. Currently they're fairly spartan, and could use more examples and a
bit more prose.

It is also useful to simply read the tutorials, and point out any areas where
things could be explained more clearly.

Specific contribution areas:

- **Examples:** adding more examples to illustrate different concepts and
  features of the language.

- **Review:** reviewing the current tutorials for clarity and simplicity.

- **Omissions:** find places where language features or rules are not explained
  (the source for these is the [spec][spec]).

[tut]: https://austral-lang.org/tutorial/
[spec]: https://austral-lang.org/spec/

### Example Programs

- **Where:**
  - https://github.com/austral/austral/tree/master/examples
  - https://github.com/austral/austral.github.io/tree/master/_includes/examples
  - https://github.com/austral/austral.github.io/tree/master/examples

There is a small collection of example programs that demonstrate various
language features and how to solve common tasks (e.g. Fibonacci, hello
world). We are always looking for new programs to showcase Austral.

If you've written an Austral program you think is useful, interesting, or
representative; and want to share it, feel free to open a PR or an
[issue][issues].

If you have an existing small program you think would be a useful demo, you can
try porting it to Austral and opening an PR.

We appreciate any and all contributions. The current collection of examples is
small, but we hope it will grow and help new users understand the language.

### Standard Library

[WIP]

### Compiler Documentation

- **Where:**
  - High-level documentation: https://github.com/austral/austral/tree/master/docs
  - Source code documentation: https://github.com/austral/austral/tree/master/lib

Compiler documentation is important. Devs have to be able to understand how the
compiler works in order to contribute to it.

The current documentation is somewhat uneven, with some modules being very
thoroughly documented and others having little or no documentation. We are
seeking contributions from the community to help improve the documentation by
adding more information and clarifying existing documentation.

Most of the documentation is in the OCaml source code, using odoc. There are
some Markdown files in the `docs` folder containing higher-level compiler
documentation.

Specific contribution areas:

- **Writing:** adding documentation for modules that currently have little or no
  documentation.

- **Reviewing:** and clarifying existing documentation to make it easier to
  understand.

# Testing

Every push runs a [GitHub Actions workflow][ci]. This builds the compiler and
runs the OCaml tests, the end-to-end tests, the examples, and the standard
library tests.

To run the tests manually, just run:

```bash
$ ./run-tests.sh
```

[ci]: https://github.com/austral/austral/blob/master/.github/workflows/build-and-test.yml

# Reviews

Just assign [`eudoxia0`][eudoxia].

[eudoxia]: https://github.com/eudoxia0/
