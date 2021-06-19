open Identifier
open Type
open Combined
open Semantic
open ModuleSystem

val extract_type_signatures : combined_module -> type_signature list

val extract_declarations : module_name -> menv -> type_signature list -> combined_module -> sem_decl list
