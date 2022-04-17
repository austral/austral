type arithmetic_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
[@@deriving show]

type comparison_operator =
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
[@@deriving show]

type docstring = Docstring of string

type pragma =
  | ForeignImportPragma of string
  | UnsafeModulePragma

type borrowing_mode =
  | ReadBorrow
  | WriteBorrow
[@@deriving show]

type module_kind =
  | SafeModule
  | UnsafeModule

type type_vis =
  | TypeVisPublic
  | TypeVisOpaque
  | TypeVisPrivate

type vis =
  | VisPublic
  | VisPrivate
