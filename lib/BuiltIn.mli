open Identifier
open Cst
open Semantic

val memory_module_name : module_name

val pointer_type_name : identifier

val memory_module : semantic_module

val is_pointer_type : qident -> bool

val pervasive_module_name : module_name

val pervasive_source_text: string * string

val option_type_name : identifier

val pervasive_imports: concrete_import_list
