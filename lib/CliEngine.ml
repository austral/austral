(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open CliParser
open Version
open Compiler
open Util
open Error
open HtmlError
open SourceContext

(* Source map stuff *)

(* Map of filenames to file contents. *)
module SourceMap =
  Map.Make(
      struct
        type t = string
        let compare a b = compare a b
      end
    )

type source_map = string SourceMap.t

(* Parsing file contents *)

let make_module_source (m: mod_source): module_source =
  match m with
  | ModuleSource { inter_path; body_path; } ->
     TwoFileModuleSource {
         int_filename = inter_path;
         int_code = read_file_to_string inter_path;
         body_filename = body_path;
         body_code = read_file_to_string body_path
       }
  | ModuleBodySource { body_path; } ->
     BodyModuleSource {
         body_filename = body_path;
         body_code = read_file_to_string body_path
       }

let parse_source_files (mods: mod_source list): (module_source list * source_map) =
  let contents = List.map make_module_source mods in
  (* Build source map for error handling *)
  let source_maps =
    List.map (fun source ->
        match source with
        | TwoFileModuleSource { int_filename; int_code; body_filename; body_code; } ->
           let smap = SourceMap.empty in
           let smap = SourceMap.add int_filename int_code smap in
           let smap = SourceMap.add body_filename body_code smap in
           smap
        | BodyModuleSource { body_filename; body_code; } ->
           let smap = SourceMap.empty in
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

(* Execution *)

let rec exec (cmd: cmd): unit =
  match cmd with
  | HelpCommand ->
     print_usage ()
  | VersionCommand ->
     print_version ()
  | CompileHelp ->
     print_compile_usage ()
  | WholeProgramCompile { modules; target; error_reporting_mode } ->
     exec_compile modules target error_reporting_mode

and print_usage _: unit =
  print_endline ("austral " ^ version_string);
  print_endline "";
  print_endline "Usage:";
  print_endline "    austral [options] <command>";
  print_endline "";
  print_endline "Options:";
  print_endline "    --help     Print this text.";
  print_endline "    --version  Print the compiler's version.";
  print_endline "";
  print_endline "Commands:";
  print_endline "    compile    Compile modules."

and print_version _: unit =
  print_endline version_string

and print_compile_usage _: unit =
  print_endline "austral compile";
  print_endline "";
  print_endline "Usage:";
  print_endline "    austral compile [options] <module...>";
  print_endline "";
  print_endline "Options:";
  print_endline "    --help          Print this text.";
  print_endline "    --target-type   One of `bin`, `tc`, `c`. Default is `bin`.";
  print_endline "    --output        Path to the output file.";
  print_endline "    --entrypoint    The name of the entrypoint function, in the";
  print_endline "                    format `<module name>:<function name>`.";
  print_endline "    --no-entrypoint  Don't compile an entrypoint. Incompatible with";
  print_endline "                    `bin` target.";
  print_endline "";
  print_endline "Positional arguments:";
  print_endline "    module    Of the form 'file.aui,file.aum' for modules with";
  print_endline "              both an interface and body file, or 'file.aum' for";
  print_endline "              modules with only a body."

and exec_compile (modules: mod_source list) (target: target) (error_reporting_mode: error_reporting_mode): unit =
  (* Parse source files *)
  let (mods, source_map): (module_source list * source_map) = parse_source_files modules in
  (* Error handling setup *)
  try
    exec_target mods target
  with Austral_error error ->
    (* Print errors *)
    begin
      match error_reporting_mode with
      | ErrorReportPlain ->
         let error: austral_error = try_adding_source_ctx error source_map in
         Printf.eprintf "%s" (render_error_to_plain error);
         html_error_dump error;
         dump_and_die ()
      | ErrorReportJson ->
         let error: austral_error = try_adding_source_ctx error source_map in
         Printf.eprintf "%s" (Yojson.Basic.pretty_to_string (render_error_to_json error));
         html_error_dump error;
         dump_and_die ()
    end

and dump_and_die _: unit =
  print_endline "Compiler call tree printed to calltree.html";
  Reporter.dump ();
  exit (-1)

and try_adding_source_ctx (error: austral_error) (source_map: source_map): austral_error =
  let (AustralError { span; source_ctx; _ }) = error in
  match source_ctx with
  | Some _ ->
     (* Already have a context. *)
     error
  | None ->
     (match span with
      | Some span ->
         let (Span { filename; _ }) = span in
         (match (SourceMap.find_opt filename source_map) with
          | Some code ->
             add_source_ctx error (get_source_ctx code span)
          | None ->
             error)
      | None ->
         error)

and exec_target (mods: module_source list) (target: target): unit =
  match target with
  | TypeCheck ->
     (* Compile everything, emit no code. *)
     let _ = compile_multiple empty_compiler mods in
     ()
  | Executable { bin_path; entrypoint; } ->
     exec_compile_to_bin mods bin_path entrypoint
  | CStandalone { output_path; entrypoint; } ->
     exec_compile_to_c mods output_path entrypoint

and exec_compile_to_bin (mods: module_source list) (bin_path: string) (entrypoint: entrypoint): unit =
  (* Compile everything to a C file. *)
  let compiler = compile_multiple empty_compiler mods in
  (* Compile the wrapper functions *)
  let compiler = post_compile compiler in
  (* Compile the entrypoint. *)
  let compiler =
    let (Entrypoint (module_name, name)) = entrypoint in
    compile_entrypoint compiler module_name name
  in
  (* Write the output to a temporary file. *)
  let cfile: string = Filename.temp_file "austral_" ".c" in
  write_string_to_file cfile (compiler_code compiler);
  (* Invoke `cc`. *)
  let _ = compile_c_code cfile bin_path in
  ()

and exec_compile_to_c (mods: module_source list) (output_path: string) (entrypoint: entrypoint option): unit =
  (* Compile everything to a C file. *)
  let compiler = compile_multiple empty_compiler mods in
  (* Compile the wrapper functions *)
  let compiler = post_compile compiler in
  (* Compile the entrypoint, if needed. *)
  let compiler =
    match entrypoint with
    | Some (Entrypoint (module_name, name)) ->
       (* It's an executable. *)
       compile_entrypoint compiler module_name name
    | None ->
       (* It's a library. *)
       compiler
  in
  (* Write the output to the given file. *)
  write_string_to_file output_path (compiler_code compiler)
