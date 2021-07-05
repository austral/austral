open Compiler
open Util
open Error

let split_common_path (path: string): string * string =
  (path ^ ".aui", path ^ ".aum")

let main' (args: string list): unit =
  let paths = List.map split_common_path args in
  let contents = List.map (fun (i, b) -> (read_file_to_string i, read_file_to_string b)) paths in
  let c = compile_multiple empty_compiler contents in
  let _ = c in
  ()

let main (args: string list): unit =
  try
    main' args
  with Programmer_error msg ->
    print_endline msg
