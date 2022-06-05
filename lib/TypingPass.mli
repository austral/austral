open Identifier
open Common
open Id
open Env
open Ast
open Linked
open Tast
open Type
open TypeClasses
open TypeParameters

(** Find an instance of the given typeclass given the dispatch type.

Parameters are: environment, source module name, dispatch type, and ID of the
typeclass. *)
val get_instance : env -> module_name -> ty -> decl_id -> decl

type stmt_ctx

val augment_stmt: stmt_ctx -> astmt -> tstmt

val augment_decl : module_name -> module_kind -> env -> linked_definition -> typed_decl

val augment_module : env -> linked_module -> typed_module

val instance_arg_as_type : env -> instance_argument -> typarams -> ty
