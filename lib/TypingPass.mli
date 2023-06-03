(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The type checker. *)
open Identifier
open Common
open Env
open Stages.AstDB
open Linked
open Tast

(** The context for statement checking. *)
type stmt_ctx

val augment_stmt: stmt_ctx -> astmt -> tstmt

val augment_decl : module_name -> module_kind -> env -> linked_definition -> typed_decl

val augment_module : env -> linked_module -> typed_module
