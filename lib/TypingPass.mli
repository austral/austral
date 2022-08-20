open Identifier
open Common
open Env
open Ast
open Linked
open Tast
open TypeBindings
open Type

type stmt_ctx

val augment_stmt: stmt_ctx -> astmt -> tstmt

val augment_decl : module_name -> module_kind -> env -> linked_definition -> typed_decl

val augment_module : env -> linked_module -> typed_module

val cast_arguments : type_bindings -> value_parameter list -> texpr list -> texpr list
