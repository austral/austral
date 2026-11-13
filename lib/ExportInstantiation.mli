(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** This module creates wrappers around exported functions. *)
open Env
open CRepr

(** Make wrappers for all exported functions. *)
val all_wrappers : env -> c_decl list
