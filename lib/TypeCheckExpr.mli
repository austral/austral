(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
(** Type checking of expressions. *)
open Ast
open Tast
open Type
open Identifier
open Env
open Region
open TypeParameters
open LexEnv

(** The type checking context. *)
type expr_ctx

val make_ctx : module_name -> env -> region_map -> typarams -> lexenv -> expr_ctx

(** Type check an expression. The second argument is the asserted type of the
    expression, this is used when type-checking `let` statements or `cast`
    expressions. *)
val augment_expr : expr_ctx -> ty option -> aexpr -> texpr
