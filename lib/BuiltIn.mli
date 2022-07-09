open Identifier
open Cst

(* Austral.Pervasive *)

val pervasive_module_name : module_name

val pervasive_imports: concrete_import_list

(* Austral.Memory *)

val memory_module_name : module_name

val is_address_type : qident -> bool
val is_pointer_type : qident -> bool
