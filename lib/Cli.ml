open Identifier
open Compiler
open Util
open Error

type arg =
  | ModuleArg of string * string
  | EntrypointArg of module_name * identifier

let parse_module_arg (s: string): arg =
  ModuleArg (s ^ ".aui", s ^ ".aum")

let parse_entrypoint_arg (s: string): arg =
  let ss = String.split_on_char ':' s in
  match ss with
  | [mn; i] ->
     EntrypointArg (make_mod_name mn, make_ident i)
  | _ ->
     err "Invalid entrypoint format."

let parse_arg (s: string): arg =
  let ss = String.split_on_char '=' s in
  match ss with
  | ["--module"; s'] ->
     parse_module_arg s'
  | ["--entrypoint"; s'] ->
     parse_entrypoint_arg s'
  | _ ->
     err "Invalid command line argument."
let main' (args: string list): unit =
  let args' = List.map parse_arg args in
  let paths = List.filter_map (fun a -> match a with (ModuleArg (i,b)) -> Some (i,b) | _ -> None) args' in
  let contents = List.map (fun (i, b) -> (read_file_to_string i, read_file_to_string b)) paths in
  let _ = List.find (fun a -> match a with (EntrypointArg _) -> true | _ -> false) args' in
  let c = compile_multiple empty_compiler contents in
  let code = compiler_code c in
  print_endline code

let main (args: string list): unit =
  try
    main' args
  with Programmer_error msg ->
    print_endline msg
