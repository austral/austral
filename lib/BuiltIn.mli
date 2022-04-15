open Identifier
open Cst

(* Austral.Pervasive *)

val pervasive_module_name : module_name

val option_type_name : identifier

val root_cap_type_name : identifier

val pervasive_imports: concrete_import_list

(* Austral.Memory *)

val memory_module_name : module_name

val is_pointer_type : qident -> bool
