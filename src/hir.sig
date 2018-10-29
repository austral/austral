(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Boreal.

    Boreal is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Boreal is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Boreal.  If not, see <http://www.gnu.org/licenses/>.
*)

signature HIR = sig
    type name = string
    type ty = Type.ty

    datatype ast = BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | NullConstant
                 | Negation of ast
                 | Variable of string
                 | Let of string * ty * ast * ast
                 | Cond of ast * ast * ast * ty
                 | IntArithOp of Arith.oper * ast * ast
                 | FloatArithOp of Arith.oper * ast * ast
                 | ComparisonOp of Builtin.comp_op * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | Allocate of ast * ty
                 | Load of ast
                 | Store of ast * ast
                 | Cast of ty * ast
                 | Construct of ty * Symbol.symbol * ast option
                 | DisjunctionNth of ast * int
                 | TagEq of ast * int
                 | SizeOf of ty
                 | Seq of ast * ast
                 | Funcall of string * ty list * ast list

    datatype top_ast = Defun of name * param list * ty * ast
                     | Defgeneric of name * name list * param list * ty * ast
                     | Deftype of name * name list * ty
                     | Defdisjunction of name * name list * Type.variant list
                     | ToplevelProgn of top_ast list
         and param = Param of name * ty

    val escapeSymbol : Symbol.symbol -> string
    val escapeVariable : Symbol.variable -> string

    val transform : TAst.ast -> ast
    val transformTop : TAst.top_ast -> top_ast
end
