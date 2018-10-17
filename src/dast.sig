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

signature DAST = sig
    type ty = Type.ty
    type name = Symbol.symbol
    type param_name = name
    type docstring = string option
    type variable = Symbol.variable
    type ast = AST.ast

    datatype top_ast = Defun of name * param list * ty * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * Type.typarams * docstring * ty
                     | Defdisjunction of name * Type.typarams * docstring * Type.variant list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Symbol.module_name * Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
         and param = Param of variable * ty
         and method_decl = MethodDecl of name * param list * ty * docstring
         and method_def = MethodDef of name * param list * ty * docstring * ast
         and instance_arg = InstanceArg of name * Type.typarams

    type tenv = Type.tenv
    type fenv = Function.fenv

    val transformTop : AST.top_ast -> tenv -> fenv -> top_ast
end
