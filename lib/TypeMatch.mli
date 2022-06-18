open Id
open Identifier
open Type
open TypeBindings
open TypeParameters
open Tast
open Env
open EnvTypes

type ctx = env * module_name

val match_type_with_value : ctx -> ty -> texpr -> type_bindings

val match_type : ctx -> ty -> ty -> type_bindings

val match_type_list : ctx -> ty list -> ty list -> type_bindings

val match_typarams : ctx -> typarams -> ty list -> type_bindings

(** Find an instance of the given typeclass given the dispatch type.

Parameters are: environment, source module name, dispatch type, and ID of the
typeclass. *)
val get_instance : env -> module_name -> ty -> decl_id -> decl * type_bindings
