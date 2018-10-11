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

signature ALPHA = sig
    type symbol = Symbol.symbol
    type variable = Symbol.variable
    type docstring = string option
    type typespec = Type.typespec

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable
                 | Let of Symbol.variable * ast * ast
                 | The of Type.typespec * ast
                 | Operation of Symbol.symbol * ast list

    datatype top_ast = Defun of symbol * param list * typespec * docstring * ast
                     | ToplevelForm of symbol * OAST.ast list
         and param = Param of variable * typespec

    type params = Symbol.symbol Set.set

    val transform : OAST.ast -> params -> ast
end
