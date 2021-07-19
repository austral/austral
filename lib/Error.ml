exception Programmer_error of string
exception Austral_parse_error of int * int
exception Linearity_error of string

let err s =
  raise (Programmer_error s)
