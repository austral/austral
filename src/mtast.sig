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

signature MTAST = sig
    type name = Symbol.symbol
    type ty = MonoType.ty

    (* Expression AST *)

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
                 | CompOp of Builtin.comp_op * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | ArrayLength of ast
                 | ArrayPointer of ast
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | Construct of ty * name * ast option
                 | Case of ast * variant_case list * ty
                 | ForeignFuncall of string * ast list * ty
                 | ForeignNull of ty
                 | SizeOf of ty
                 | AddressOf of Symbol.variable * ty
                 | Cast of ty * ast
                 | Seq of ast * ast
                 | ConcreteFuncall of name * ast list * ty
                 | GenericFuncall of name * int * ast list * ty
         and variant_case = VariantCase of case_name * ast
         and case_name = NameOnly of name
                       | NameBinding of { casename: name, var: Symbol.variable, ty: ty }

    val typeOf : ast -> ty

    (* Toplevel AST *)

    datatype top_ast = Defun of name * param list * ty * ast
                     | DefunMonomorph of name * param list * ty * ast * int
                     | DeftypeMonomorph of name * ty * int
                     | ToplevelProgn of top_ast list
         and param = Param of Symbol.variable * ty

    (* Monomorphization *)

    type context

    val emptyContext : context

    val getMonomorph : context -> name -> ty list -> int option
    val addMonomorph : context -> name -> ty list -> int -> context
    val newFuncMonomorphs : context -> context -> (name * ty list * int) list
    val newTypeMonomorphs : context -> context -> (name * ty list * ty * int) list

    type replacements

    val monomorphize : context -> replacements -> TAST.ast -> (ast * context)
    val monomorphizeList : context -> replacements -> TAST.ast list -> (ast list * context)
    val monomorphizeTop : Function.fenv -> FDefs.fdefenv -> context -> TAST.top_ast -> (top_ast * context)
end
