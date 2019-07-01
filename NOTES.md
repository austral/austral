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
Module = [docstring], "module", module name, {Import}, {Declaration};
Import = "from", module name, "import", identifier, ["as" identifier];
Declaration = RecordDecl | UnionDecl | FunctionDef;
RecordDef = [docstring], [TypeVis], "record", identifier, [TypeParams], "{", <Slot>, "}";
UnionDef = [docstring], [TypeVis], "union", identifier, [TypeParams], "{", <Case>, "}";
TypeVis = "opaque" | "public";
TypeParams = "(", <identifier, [":", Universe"]>, "}", [":", "Universe"];
Universe = "Type1" | "Type*";
Slot = identifier, ":", TypeSpec, [docstring];
Case = identifier, [":", TypeSpec], [docstring];
TypeSpec = identifier | "(", <TypeSpec>, ")" | identifier, "(", <TypeSpec>, ")";
FunctionDef = [docstring], ["public"], "function", identifier, "(", <Param>, ")", Block;
Param = identifier, ":", TypeSpec, [docstring];
Block = "{", {Statement}, "}";
Statement = "abort"
          | "return", Expression;
Expression = Constant;
Constant = "nil" | integer constant | float constant | string constant;
module name = module identifier, { ".", module identifier };
module identifier = letter, { letter | digit }
identifier = letter, { letter | digit | symbol };
letter = uppercase | lowercase;
uppercase = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
lowercase = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
symbol = "$" | "?" | "'"
docstring = "`", any character ,"`";
integer constant = ["+" | "-"], digit, {digit | "_"};
```
