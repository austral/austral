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
    type name = Symbol.symbol
    type ty = Type.ty

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string * ty
                 | FloatConstant of string * ty
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable * ty
                 | Let of Symbol.variable * ast * ast
                 | Bind of Symbol.variable list * ast * ast
                 | Cond of ast * ast * ast
                 | ArithOp of Arith.kind * Arith.oper * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | ArrayLength of ast
                 | ArrayPointer of ast
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | The of ty * ast
                 | Construct of ty * name * ast option
                 | Case of ast * variant_case list * ty
                 | ForeignFuncall of string * ast list * ty
                 | ForeignNull of ty
                 | SizeOf of ty
                 | AddressOf of Symbol.variable * ty
                 | Cast of ty * ast
                 | Seq of ast * ast
                 | ConcreteFuncall of name * ast list * ty
                 | GenericFuncall of name * ty list * ast list * ty
                 | MethodFuncall of name * ty list * ast list * ty
         and variant_case = VariantCase of case_name * ast
         and case_name = NameOnly of name
                       | NameBinding of { casename: name, var: Symbol.variable, ty: ty }

    type param_name = name
    type docstring = string option
    type symbol = Symbol.symbol
    type variable = Symbol.variable

    datatype top_ast = Defun of name * param list * ty * docstring * ast
                     | Defgeneric of name * Type.typarams * param list * ty * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * Type.typarams * docstring * ty
                     | Defdisjunction of name * Type.typarams * docstring * Type.variant list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Symbol.module_name * Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
                     | Defcfun of name * string * param list * Function.foreign_arity * ty * docstring
         and param = Param of variable * ty
         and method_decl = MethodDecl of name * param list * ty * docstring
         and method_def = MethodDef of name * param list * ty * docstring * ast
         and instance_arg = InstanceArg of name * Type.typarams

    val typeOf : ast -> ty

    datatype binding = Binding of ty * mutability
         and mutability = Immutable
                        | Mutable

    type bindings = (Symbol.variable, binding) Map.map

    type context

    val augment : AST.ast -> context -> ast

    val augmentTop : DAST.top_ast -> Type.tenv -> Function.fenv -> top_ast
end
