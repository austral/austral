open ParserInterface
open Util
open Error

let split_common_path (path: string): string * string =
  (path ^ ".aui", path ^ ".aum")

let main' (args: string list): unit =
  let paths = List.map split_common_path args in
  let contents = List.map (fun (i, b) -> (read_file_to_string i, read_file_to_string b)) paths in
  let _ = List.map (fun (is, bs) -> (parse_module_int is, parse_module_body bs)) contents in
  ()

let main (args: string list): unit =
  try
    main' args
  with Programmer_error msg ->
    print_endline msg
