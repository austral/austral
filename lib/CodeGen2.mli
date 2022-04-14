open Identifier
open Mtast
open CRepr

val gen_ident : identifier -> string

val gen_module_name : module_name -> string

val gen_module : mono_module -> c_unit
