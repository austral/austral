# Implementation Notes

## General Principles

- Separate intermediate representations from passes. Define an SML module
  (`.sig` and `.sml` file) for the representation, another for the pass.

## Intermediate Representations

Code travels down the following intermediate representations:

- Syntax (module `Syntax`): this is the output straight from the parser, the basic abstract
  syntax tree.
- Ordered declarations AST.

## Linear API

Rough notes.

Pointers:

```
allocate: t -> pointer(t)
load: pointer(t) -> t // dereferencing deallocates
swap: pointer(t) -> t -> (pointer(t), t)
```

Arrays:

```
allocate_array: t -> size -> array(t)
replace: array(t) -> index -> t -> (array(t), t)
```
