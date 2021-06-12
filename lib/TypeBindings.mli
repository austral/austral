(* A type bindings value represents a map of type parameter names to actual
   types. These are created as part of the type checking process. *)
type type_bindings

val empty_bindings : type_bindings

val merge_bindings : type_bindings -> type_bindings -> type_bindings
