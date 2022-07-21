(** Entrypoint logic. *)
open Env
open Identifier

(** Given the env, and the name of the entrypoint function, generate the entrypoint code. *)
val entrypoint_code : env -> qident -> string
