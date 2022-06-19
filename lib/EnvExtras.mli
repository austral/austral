(** Useful functions not part of the core environment API. *)
open Id
open Identifier
open Env
open EnvTypes

val get_decl_by_name_or_die : env -> sident -> decl

val get_decl_name_or_die : env -> decl_id -> string
