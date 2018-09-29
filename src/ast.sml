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

structure AST :> AST = struct
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

    datatype top_ast = Defun of name * param list * Type.typespec * docstring * ast
                     | Defclass of name * Symbol.symbol * docstring * method list
                     | Definstance
                     | Deftype of name * Type.param list * Type.typespec * docstring
                     | Defdisjunction of name * Type.param list * disjunction_case list * docstring
                     | Defmacro
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Module.module
                     | InModule of Symbol.symbol_name
         and param = FuncParam of Symbol.symbol * Type.typespec
         and method = Method of name * param list * Type.typespec * docstring
         and disjunction_case = DisjCase of Symbol.symbol * Type.typespec option

    fun au name =
        Symbol.mkSymbol (Ident.mkIdentEx "austral",
                         Ident.mkIdentEx name)

    val theOp = au "the"

    fun transform (RCST.IntConstant i) = IntConstant i
      | transform (RCST.FloatConstant f) = FloatConstant f
      | transform (RCST.StringConstant s) = StringConstant s
      | transform (RCST.Symbol s) = Symbol s
      | transform (RCST.Keyword s) = Keyword s
      | transform (RCST.List l) = transformList l
    and transformList ((RCST.Symbol theOp)::ty::exp::nil) = The (ty, transform exp)
      | transformList ((RCST.Symbol f)::rest) = Operator (f, map transform rest)
      | transformList _ = raise Fail "Invalid form"
end
