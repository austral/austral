open Identifier
open Type
open LexEnv
open ModuleSystem
open Ast
open Tast

val augment_expr : module_name -> menv -> lexenv -> ty option -> aexpr -> texpr

val augment_stmt: module_name -> menv -> type_parameter list -> lexenv -> astmt -> tstmt
