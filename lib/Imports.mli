open Identifier

(* Represents the imports into a module. *)
type import_map

val empty_map : module_name -> import_map

(* The name of the module we're importing into. *)
val importing_module : import_map -> module_name

(* Find a symbol from its local nickname. Returns None if not imported. *)
val get_symbol : import_map -> identifier -> qident option
