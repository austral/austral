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

signature AST = sig
    datatype ast = IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Symbol of Symbol.symbol
                 | Keyword of Symbol.symbol_name
                 | Let of binding * ast
                 | The of RCST.rcst * ast
                 | Operator of Symbol.symbol * ast list
         and binding = Binding of Symbol.symbol * ast

    type name = Symbol.symbol
    type docstring = string option
    type symbol = Symbol.symbol
    type typespec = Type.typespec

    datatype top_ast = Defun of name * param list * typespec * docstring * ast
                     | Defclass of name * symbol * docstring * method list
                     | Definstance of name * typespec * docstring * method_def list
                     | Deftype of name * Type.param list * typespec * docstring
                     | Defdisjunction of name * Type.param list * disjunction_case list * docstring
                     | Deftemplate of name * docstring * template_case
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Module.module
                     | InModule of Symbol.symbol_name
         and param = Param of name * typespec
         and method = Method of name * param list * typespec * docstring
         and method_def = MethodDef of name * param list * typespec * docstring * ast
         and disjunction_case = DisjCase of name * typespec option

    val transform : RCST.rcst -> ast
end
