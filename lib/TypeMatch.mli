open Type
open TypeBindings

val match_type : ty -> ty -> type_bindings

val match_type_list : ty list -> ty list -> type_bindings
