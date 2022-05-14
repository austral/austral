open Type
open TypeBindings
open TypeParameters
open Tast

val match_type_with_value : ty -> texpr -> type_bindings

val match_type : ty -> ty -> type_bindings

val match_type_list : ty list -> ty list -> type_bindings

val match_typarams : typarams -> ty list -> type_bindings
