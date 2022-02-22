open Identifier
open Type
open Combined
open Semantic
open ModuleSystem

(** Go through all the types in a combined module, convert them into type
    signatures, and return those signatures as a list. *)
val extract_type_signatures : combined_module -> type_signature list

val extract_declarations : module_name -> menv -> type_signature list -> combined_module -> sem_decl list

val extract : menv -> combined_module -> semantic_module
