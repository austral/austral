(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** This module creates wrappers around exported functions. *)
open Env
open Id
open CRepr

(** Make a wrapper for an exported function. *)
val make_wrapper : env -> decl_id -> string -> c_decl

(** Make wrappers for all exported functions. *)
val all_wrappers : env -> c_decl list
