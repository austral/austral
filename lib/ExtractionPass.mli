open Type
open Combined
open Linked
open Env
open Id

(** Go through all the types in a combined module, convert them into type
    signatures, and return those signatures as a list. *)
val extract_type_signatures : combined_module -> type_signature list

(** Extract a combined module's declarations and put them in the environment,
    and returned a linked module. *)
val extract : env -> combined_module -> file_id -> file_id -> (env * linked_module)
