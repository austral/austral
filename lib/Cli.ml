open Identifier
open Compiler
open Util
open Error

(* Map of filenames to file contents. *)
module SourceMap =
  Map.Make(
      struct
        type t = string
        let compare a b = compare a b
      end
    )

type arg =
  | ModuleArg of string * string
  | EntrypointArg of module_name * identifier
  | OutputArg of string

let parse_module_arg (s: string): arg =
  let ss = String.split_on_char ',' s in
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

let make_module_source int_filename body_filename =
  ModuleSource {
      int_filename = int_filename;
      int_code = read_file_to_string int_filename;
      body_filename = body_filename;
      body_code = read_file_to_string body_filename
    }

let parse_source_files (args: string list): (module_source list * string SourceMap.t) =
  (* Parse arg list *)
  let args' = List.map parse_arg args in
  let paths = List.filter_map (fun a -> match a with (ModuleArg (i,b)) -> Some (i,b) | _ -> None) args' in
  let contents = List.map (fun (i, b) -> make_module_source i b) paths in
  (* Build source map for error handling *)
  let source_maps =
    List.map (fun (ModuleSource { int_filename; int_code; body_filename; body_code }) ->
        let smap = SourceMap.empty in
        let smap = SourceMap.add int_filename int_code smap in
        let smap = SourceMap.add body_filename body_code smap in
        smap)
      contents
  in
  let source_map =
    List.fold_left
      (fun sm sm' -> SourceMap.union (fun _ v _ -> Some v) sm sm')
      (SourceMap.empty)
      source_maps
  in
  (contents, source_map)

let compile_main (args: string list): unit =
  let (contents, source_map) = parse_source_files args in
  try
    let args' = List.map parse_arg args in
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
  with Austral_error error ->
    let filename = error_filename error in
    let code: string option =
      match filename with
      | Some filename ->
         (SourceMap.find_opt filename source_map)
      | None ->
         None
    in
    Printf.eprintf "%s" (render_error error code);
    print_endline ("Backtrace:\n" ^ (Printexc.get_backtrace ()));
    exit (-1)

let typecheck_main (args: string list): unit =
  let (contents, source_map) = parse_source_files args in
  try
    let _ = compile_multiple empty_compiler contents in
    ()
  with Austral_error error ->
    let filename = error_filename error in
    let code: string option =
      match filename with
      | Some filename ->
         (SourceMap.find_opt filename source_map)
      | None ->
         None
    in
    Printf.eprintf "%s" (render_error error code);
    print_endline ("Backtrace:\n" ^ (Printexc.get_backtrace ()));
    exit (-1)

let main' (args: string list): unit =
  match args with
  | first::args ->
     (match first with
      | "compile" ->
         compile_main args
      | "typecheck" ->
         typecheck_main args
      | _ ->
         err ("Unknown command: " ^ first ^ "\n"))
  | _ ->
     err "Invalid invocation."

let main (args: string list): unit =
  try
    Printexc.record_backtrace true;
    main' args;
    exit 0
  with Austral_error error ->
    Printf.eprintf "%s" (render_error error None);
    print_endline ("Backtrace:\n" ^ (Printexc.get_backtrace ()));
    exit (-1)
