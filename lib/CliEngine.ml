open CliParser
open Version

let rec exec (cmd: cmd): unit =
  match cmd with
  | HelpCommand ->
     print_usage ()
  | VersionCommand ->
     print_version ()
  | CompileHelp ->
     print_compile_usage ()
  | WholeProgramCompile { modules; target; } ->
     exec_compile modules target

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
  print_endline "                    format `<module nae>:<function name>`.";
  print_endline "    --no-entrypint  Don't compile an entrypoint. Incompatible with";
  print_endline "                    `bin` target.";
  print_endline "";
  print_endline "Positional arguments:";
  print_endline "    module    Of the form 'file.aui,file.aum' for modules with";
  print_endline "              both an interface and body file, or 'file.aum' for";
  print_endline "              modules with only a body."

and exec_compile (modules: mod_source list) (target: target): unit =
  let _ = (modules, target) in
  ()
