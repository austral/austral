exception Programmer_error of string

let err s =
  raise (Programmer_error s)
