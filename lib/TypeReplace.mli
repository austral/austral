open TypeBindings
open Tast

(** Replace the type variables in a typed expression. *)
val replace_tyvars_expr : type_bindings -> texpr -> texpr

(** Replace the type variables in a typed statement. *)
val replace_tyvars_stmt : type_bindings -> tstmt -> tstmt
