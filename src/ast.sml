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

    datatype top_ast = Defun of name * param list * typespec * docstring * ast
                     | Defclass of name * symbol * docstring * method list
                     | Definstance of name * typespec * docstring * method_def list
                     | Deftype of name * Type.param list * typespec * docstring
                     | Defdisjunction of name * Type.param list * disjunction_case list * docstring
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Module.module
                     | InModule of Symbol.symbol_name
         and param = Param of name * typespec
         and method = Method of name * param list * typespec * docstring
         and method_def = MethodDef of name * param list * typespec * docstring * ast
         and disjunction_case = DisjCase of name * typespec option

    fun au name =
        Symbol.mkSymbol (Ident.mkIdentEx "austral",
                         Ident.mkIdentEx name)

    fun transform (RCST.IntConstant i) = IntConstant i
      | transform (RCST.FloatConstant f) = FloatConstant f
      | transform (RCST.StringConstant s) = StringConstant s
      | transform (RCST.Symbol s) = Symbol s
      | transform (RCST.Keyword s) = Keyword s
      | transform (RCST.Splice _) = raise Fail "Splices not allowed in expressions"
      | transform (RCST.List l) = transformList l
    and transformList ((RCST.Symbol f)::args) = transformOp f args
      | transformList _ = raise Fail "Invalid list form"
    and transformOp f args =
        if f = au "the" then
            case args of
                [ty, exp] => The (ty, transform exp)
              | _ => raise Fail "Invalid `the` form"
        else
            Operator (f, map transform args)

    (* Parse toplevel forms into the AST *)

    fun transformTop (RCST.List l) = transformTopList l
      | transformTop _ = raise Fail "Invalid toplevel form"
    and transformTopList ((RCST.Symbol f)::args) = transformT f args
      | transformTopList _ = raise Fail "Invalid toplevel form"
    and transformT f args =
        if f = au "defun" then
            transformDefun args
        else if f = au "defclass" then
            transformDefclass args
        else
            raise Fail "Unknown toplevel form"
    and transformDefun ((RCST.Symbol name)::params::rt::body) =
        raise Fail "defun not implemented"
      | transformDefun _ = raise Fail "Bad defun form"
    and transformDefclass ((RCST.Symbol name)::arg::body) =
        raise Fail "defclass not implemented"
      | transformDefclass _ = raise Fail "Bad defclass form"
end
