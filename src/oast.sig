(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Austral.

    Austral is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Austral is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Austral.  If not, see <http://www.gnu.org/licenses/>.
*)

signature OAST = sig
    type symbol = Symbol.symbol
    type name = symbol
    type typespec = Type.typespec

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Symbol of symbol
                 | Let of name * ast * ast
                 | Bind of name list * ast * ast
                 | Malloc of typespec * ast
                 | The of typespec * ast
                 | Construct of typespec * name * ast option
                 | MakeRecord of typespec * (name * ast) list
                 | Case of ast * variant_case list
                 | NullPointer of typespec
                 | SizeOf of typespec
                 | AddressOf of name
                 | Cast of typespec * ast
                 | Operation of name * ast list
         and variant_case = VariantCase of case_name * ast
         and case_name = NameOnly of name
                       | NameBinding of { casename: name, var: name }

    type docstring = string option
    type param_name = symbol

    datatype top_ast = Defun of name * param list * typespec * docstring * ast
                     | Defgeneric of name * param_name list * param list * typespec * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * param_name list * docstring * typespec
                     | Defdatatype of name * param_name list * docstring * variant list
                     | Defrecord of name * param_name list * docstring * slot list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Symbol.module_name * Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
                     | Defcfun of name * string * param list * Function.foreign_arity * typespec * docstring
         and param = Param of name * typespec
         and method_decl = MethodDecl of name * param list * typespec * docstring
         and method_def = MethodDef of name * param list * typespec * docstring * ast
         and instance_arg = InstanceArg of name * name list
         and variant = Variant of name * typespec option
         and slot = Slot of name * typespec * docstring

    val parseForeignParamList : RCST.rcst -> (Function.foreign_arity * param list)

    val transform : RCST.rcst -> ast
    val transformTop : RCST.rcst -> top_ast
end
