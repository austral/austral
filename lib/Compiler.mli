open Identifier

type compiler

val empty_compiler : compiler

val compiler_code : compiler -> string

val compile_mod : compiler -> string -> string -> compiler

val compile_multiple : compiler -> (string * string) list -> compiler

val compile_entrypoint : compiler -> module_name -> identifier -> compiler

(* Test utility. Takes a list of modules (represented as pairs of the interface
   text and the body text), the name of the entrypoint, compiles the code, runs
   it, and returns the status code and output. *)
val compile_and_run : (string * string) list -> string -> (int * string)
