(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** In the abstraction pass, we:

    1. Qualify identifiers.
    2. Qualify type specifiers.
    3. Reshape let statements: put the code after a let
       under the let.
    4. Turn blocks from lists to pairs.
 *)
open Imports
open Cst
module Ast = Stages.Ast
open Ast

val abs_stmt : import_map -> cstmt -> astmt

val abs_expr : import_map -> cexpr -> aexpr
