open Imports
open Cst
open Ast

(* In the abstraction pass, we:

    1. Qualify identifiers.
    2. Qualify type specifiers.
    3. Reshape let statements: put the code after a let
       under the let.
    4. Turn blocks from lists to pairs.
 *)

val abs_stmt : import_map -> cstmt -> astmt

val abs_expr : import_map -> cexpr -> aexpr
