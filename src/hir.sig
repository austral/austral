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
    datatype ast = IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Variable of string
                 | Let of string * ast * ast
                 | Cond of ast * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | Cast of Type.typespec * ast
                 | Progn of ast list
                 | Funcall of string * ast list

    type typespec = Type.typespec

    datatype top_ast = DefunConcrete of string * (string * typespec) list * typespec * ast

    val escapeSymbol : Symbol.symbol -> string
    val escapeVariable : Symbol.variable -> string

    val transform : AST.ast -> ast
    val transformTop : AST.top_ast -> top_ast
end
