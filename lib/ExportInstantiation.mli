(** This module creates wrappers around exported functions. *)
open Env
open Id
open CRepr

val make_wrapper : env -> decl_id -> string -> c_decl
