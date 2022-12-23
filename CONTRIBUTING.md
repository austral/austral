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

[WIP]

# Testing

Every push runs a [GitHub Actions workflow][ci]. This builds the compiler and
runs the OCaml tests, the end-to-end tests, the examples, and the standard
library tests.

[ci]: https://github.com/austral/austral/blob/master/.github/workflows/build-and-test.yml

# Reviews

Just assign [`eudoxia0`][eudoxia].

[eudoxia]: https://github.com/eudoxia0/
