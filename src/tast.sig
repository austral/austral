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

signature TAST = sig
    type ty = Type.ty

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string * ty
                 | FloatConstant of string * ty
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable * ty
                 | Let of Symbol.variable * ast * ast
                 | Cond of ast * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | The of ty * ast
                 | Progn of ast list
                 | Funcall of Symbol.symbol * ast list * ty

    type name = string

    datatype top_ast = Defun of name * param list * ty * ast
         and param = Param of name * ty

    val typeOf : ast -> ty

    datatype binding = Binding of ty * mutability
         and mutability = Immutable
                        | Mutable

    type bindings = (Symbol.variable, binding) Map.map

    type context
    val mkContext : bindings -> Type.tenv -> Function.fenv -> context

    val augment : AST.ast -> context -> ast
    (*val augmentTop : AST.top_ast -> context -> top_ast*)
end
