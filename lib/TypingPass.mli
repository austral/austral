open Identifier
open Common
open ModuleSystem
open Ast
open Combined
open Tast

type stmt_ctx

val augment_stmt: stmt_ctx -> astmt -> tstmt

val augment_decl : module_name -> module_kind -> menv -> combined_definition -> typed_decl

val augment_module : menv -> combined_module -> typed_module
