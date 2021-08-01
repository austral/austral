open Identifier
open Imports
open Cst
open Combined
open ModuleSystem

val match_decls : module_name -> import_map -> import_map -> concrete_decl -> concrete_def -> combined_definition

(* Convert a private definition to a combined one *)
val private_def : module_name -> import_map -> concrete_def -> combined_definition

val combine : menv -> concrete_module_interface -> concrete_module_body -> combined_module
