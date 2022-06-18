open Env
open Identifier
open Type
open Id
open EnvTypes
open TypeBindings

(** Find an instance of the given typeclass given the dispatch type.

Parameters are: environment, source module name, dispatch type, and ID of the
typeclass. *)
val get_instance : env -> module_name -> ty -> decl_id -> decl * type_bindings
