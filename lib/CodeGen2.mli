open Identifier
open Id
open Env
open Mtast
open CRepr

val gen_decl_id : decl_id -> string

val gen_mono_id : mono_id -> string

val gen_ident : identifier -> string

val gen_module_name : module_name -> string

val gen_module : env -> mono_module -> c_unit
