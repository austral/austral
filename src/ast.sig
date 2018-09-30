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
                 | Variable of Symbol.variable
                 | Let of Symbol.variable * ast * ast
                 | Cond of ast * ast * ast
                 | The of Type.typespec * ast
                 | Progn of ast list
                 | Operation of Symbol.symbol * ast list

    type name = Symbol.symbol
    type docstring = string option
    type symbol = Symbol.symbol
    type typespec = Type.typespec

    datatype top_ast = Defun of name * (name * typespec) list * typespec * docstring * ast
                     | Defclass of Function.typeclass
                     | Definstance of Function.instance
                     | Deftype of name * Type.param list * docstring * typespec
                     | Defdisjunction of name * Type.param list * disjunction_case list * docstring
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of defmodule_clause list
                     | InModule of Symbol.symbol_name
         and disjunction_case = DisjCase of name * typespec option
         and defmodule_clause = NicknamesClause of (Symbol.symbol_name * Symbol.module_name) list
                              | UseClause of Symbol.module_name list
                              | ImportFromClause of Symbol.module_name * (Symbol.symbol_name list)
                              | ExportClause of Symbol.symbol_name list
                              | DocstringClause of string

    val transform : Alpha.ast -> ast

    val transformTop : RCST.rcst -> top_ast
end
