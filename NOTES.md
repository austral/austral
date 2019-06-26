# Implementation Notes

## General Principles

- Separate intermediate representations from passes. Define an SML module
  (`.sig` and `.sml` file) for the representation, another for the pass.

## Intermediate Representations

Code travels down the following intermediate representations:

- Syntax (module `Syntax`): this is the output straight from the parser, the basic abstract
  syntax tree.
- Ordered declarations AST.
- Resolved declarations AST.
- Typed declarations AST.

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

# Language Notes

## Syntax

EBNF quick guide:

Definition is `=`, terminates with `;`. Concatenation is `,`. Alternation is
`|`. Optional is `[...]`. Repetition (zero or more) is `{...}`.

For brevity, we use `<...>` to mean comma-separated repetition, e.g. the empty
string, `A`, `A,B`, `A,B,C`, without a trailing comma.

Non-terminals are in `PascalCase`. Terminals are `lower case`. Whitespace is
implied.

```
Module = "module", module name, {Import}, {Declaration};
Import = "from", module name, "import", identifier, ["as" identifier];
Declaration = RecordDecl | UnionDecl | FunctionDef;

RecordDef = [TypeVis], ["linear"] "record", identifier, "{", <Slot>, "}";
UnionDef = [TypeVis], ["linear"], "union", identifier, "{", <Case>, "}";
TypeVis = "opaque" | "public";
Slot = identifier, ":", TypeSpec;
Case = identifier, [":", TypeSpec];

TypeSpec = identifier | "(", <TypeSpec>, ")" | identifier, "(", <TypeSpec>, ")"

FunctionDef = ["public"], "function", identifier, "(", <Param>, ")"
Param = identifier, ":", TypeSpec
```

Lexical elements:

```
module name = <TODO add regex>
identifier = <TODO add regex>
```
