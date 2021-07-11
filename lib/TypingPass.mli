open Identifier
open Type
open LexEnv
open ModuleSystem
open Ast
open Combined
open Tast

val augment_expr : module_name -> menv -> lexenv -> ty option -> aexpr -> texpr

type stmt_ctx

val augment_stmt: stmt_ctx -> astmt -> tstmt

val augment_decl : module_name -> menv -> combined_definition -> typed_decl

val augment_module : menv -> combined_module -> typed_module
