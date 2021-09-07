open Identifier
open Compiler
open Util
open Error

type arg =
  | ModuleArg of string * string
  | EntrypointArg of module_name * identifier
  | OutputArg of string

let parse_module_arg (s: string): arg =
  let ss = String.split_on_char ':' s in
  match ss with
  | [first] ->
     ModuleArg (first ^ ".aui", first ^ ".aum")
  | [first; second] ->
     ModuleArg (first, second)
  | _ ->
     err "Invalid value for the --module option."

let parse_entrypoint_arg (s: string): arg =
  let ss = String.split_on_char ':' s in
  match ss with
  | [mn; i] ->
     EntrypointArg (make_mod_name mn, make_ident i)
  | _ ->
     err "Invalid entrypoint format."

let parse_output_arg (path: string): arg =
  OutputArg path

let parse_arg (s: string): arg =
  let ss = String.split_on_char '=' s in
  match ss with
  | ["--module"; s'] ->
     parse_module_arg s'
  | ["--entrypoint"; s'] ->
     parse_entrypoint_arg s'
  | ["--output"; s'] ->
     parse_output_arg s'
  | _ ->
     err "Invalid command line argument."

let compile_main (args: string list): unit =
  let args' = List.map parse_arg args in
  let paths = List.filter_map (fun a -> match a with (ModuleArg (i,b)) -> Some (i,b) | _ -> None) args' in
  let contents = List.map (fun (i, b) -> (read_file_to_string i, read_file_to_string b)) paths in
  let entrypoint = List.filter_map (fun a -> match a with (EntrypointArg (m,i)) -> Some (m, i) | _ -> None) args' in
  let output = List.filter_map (fun a -> match a with (OutputArg path) -> Some path | _ -> None) args' in
  let compiler = compile_multiple empty_compiler contents in
  let compiler = (match entrypoint with
                  | [(m,i)] ->
                     compile_entrypoint compiler m i
                  | [] ->
                    (* If there is not --entrypoint flag, it's a library. *)
                     compiler
                  | _ ->
                     err "Multiple --entrypoint flags.")
  in
  let code = compiler_code compiler in
  match output with
  | [output_path] ->
     write_string_to_file output_path code
  | [] ->
     err "Misisng --output flag."
  | _ ->
     err "Multiple --output flags."


let main' (args: string list): unit =
  match args with
  | first::args ->
     (match first with
      | "compile" ->
         compile_main args
      | _ ->
         err ("Unknown command: " ^ first))
  | _ ->
     err "Invalid invocation."

let main (args: string list): unit =
  try
    main' args;
    exit 0
  with Programmer_error msg ->
    Printf.eprintf "Error: %s" msg;
    exit (-1)
