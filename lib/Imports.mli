open Identifier
open Id
open ModuleNameSet

(** Represents the set of all imports into a module. *)
type import_map

(** Create an empty map, given the name of the module that's we're importing
    into. *)
val empty_map : module_name -> import_map

(** Add a qualified identifier to the import map. *)
val add_symbol : import_map -> qident -> import_map

(** Add the ID of a typeclass instance into the import map. *)
val add_instance_to_imports : import_map -> decl_id -> import_map

(** The name of the module we're importing into. *)
val importing_module : import_map -> module_name

(** Find a symbol from its local nickname. Returns {!None} if not imported. *)
val get_symbol : import_map -> identifier -> qident option

(** The list of imported typeclass instances. *)
val imported_instances : import_map -> decl_id list

(** Return the set of module names we're importing from. *)
val modules_imported_from : import_map -> ModuleNameSet.t
