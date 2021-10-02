open Identifier

type compiler

val empty_compiler : compiler

val compiler_code : compiler -> string

type module_source = ModuleSource of {
      int_filename: string;
      int_code: string;
      body_filename: string;
      body_code: string
    }

val compile_mod : compiler -> module_source -> compiler

val compile_multiple : compiler -> module_source list -> compiler

val compile_entrypoint : compiler -> module_name -> identifier -> compiler

(* Test utility. Takes a list of modules (represented as pairs of the interface
   text and the body text), the name of the entrypoint, compiles the code, runs
   it, and returns the status code and output. *)
val compile_and_run : (string * string) list -> string -> (int * string)
