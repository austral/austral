(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)

(** Desugar paths into reference transforms. *)
open Stages

val transform_expr : Ast.aexpr -> AstDP.aexpr

val transform_stmt : Ast.astmt -> AstDP.astmt
