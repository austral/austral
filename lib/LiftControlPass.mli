(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Stages

(** Lift complex expressions in control structures. *)
val lift: Ast.astmt -> AstLC.astmt

val transform: Ast.aexpr -> AstLC.aexpr