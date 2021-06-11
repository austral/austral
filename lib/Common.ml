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

