exception Programmer_error of string
exception AustralParseError of int * int

let err s =
  raise (Programmer_error s)
