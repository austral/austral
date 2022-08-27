(** This module creates wrappers around exported functions. *)
open Env
open Id
open CRepr

(** Make a wrapper for an exported function. *)
val make_wrapper : env -> decl_id -> string -> c_decl

(** Make wrappers for all exported functions. *)
val all_wrappers : env -> c_decl list
