# Roadmap

## 2023Q1

There are a number of areas that need work:

- **Documentation:** while the spec is done, the tutorial is very spartan, and could use much more prose, especially explaining the novel concepts.
- **Tooling:** there isn't any. A build system and a package manager have to be built.
- **Standard Library:** at least a few basic container data types.
- **Editor Support:** there's a small Emacs mode. Writing a VS Code plugin should be easy enough, VS Code has really good documentation for adding
  language support. It also supports doing indentation using rules rather than code, unlike Emacs.
- **Separate Compilation:** for speed as well as for neatness.

In prioritizing these, I realized that the tutorial and the standard library involve overlapping work:

1. The standard library needs simple generic container datatypes.
2. The tutorial should walk the user through the process of implementing basic containers (linked lists, heap-allocated buffers) using linear types.

So, here's the broad roadmap for Q1, sorted by decreasing priority.

1. Improve the tutorial, add a lot more content on defining generic datatypes. At least an example of a linked list.
2. Write some of the standard library: broadly-useful type classes, generic containers and algorithms, file and terminal I/O. No need to go crazy with capabilities yet.
3. A really basic build system.

Stretch goals:

1. A simple package manager.
2. A simple VS Code mode.

Separate compilation support is likely to be a complicated endeavor. It will require careful upfront thinking and architecting, at the very least.    
