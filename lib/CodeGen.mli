open Identifier
open Tast
open Cpp

val gen_ident : identifier -> string

val gen_module_name : module_name -> string

val gen_module : typed_module -> cpp_decl
