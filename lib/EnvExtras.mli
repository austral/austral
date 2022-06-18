(** Useful functions not part of the core environment API. *)
open Env
open Id

val get_decl_name_or_die : env -> decl_id -> string
