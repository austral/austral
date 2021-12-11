type arithmetic_operator =
  | Add
  | Subtract
  | Multiply
  | Divide

type comparison_operator =
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual

type docstring = Docstring of string

type pragma =
  | ForeignImportPragma of string
  | UnsafeModulePragma

type borrowing_mode =
  | ReadBorrow
  | WriteBorrow

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
