(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)

(** Desugar anonymous borrows. *)
open Stages

val transform_expr : AstDP.aexpr -> AstDB.aexpr

val transform_stmt : AstDP.astmt -> AstDB.astmt
