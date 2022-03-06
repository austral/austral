open Identifier
open Common
open Env
open Ast
open Combined
open Tast

(** Find an instance of the given typeclass given the dispatch type.

Parameters are: environment, source module name, dispatch type, and ID of the
typeclass. *)
val get_instance : env -> module_name -> ty -> decl_id -> decl

type stmt_ctx

val augment_stmt: stmt_ctx -> astmt -> tstmt

val augment_decl : module_name -> module_kind -> env -> combined_definition -> typed_decl

val augment_module : env -> combined_module -> typed_module
