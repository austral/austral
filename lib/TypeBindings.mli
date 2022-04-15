open Identifier
open Type

(* A type bindings value represents a map of type parameter names to actual
   types. These are created as part of the type checking process. *)
type type_bindings

val binding_count : type_bindings -> int

val bindings_list : type_bindings -> (identifier * qident * ty) list

val empty_bindings : type_bindings

val bindings_from_list : (identifier * qident * ty) list -> type_bindings

val merge_bindings : type_bindings -> type_bindings -> type_bindings

val add_binding : type_bindings -> identifier -> qident -> ty -> type_bindings

val get_binding : type_bindings -> identifier -> qident -> ty option

(* Given a set of bindings, and a type expression, replace every type variable
   in the expression with the value from the bindings. If the variable is not
   found in the bindings, do nothing. *)
val replace_variables : type_bindings -> ty -> ty

val show_bindings : type_bindings -> string
