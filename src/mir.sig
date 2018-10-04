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

signature MIR = sig
    datatype ty = Bool
                | UInt8
                | Int8
                | UInt16
                | Int16
                | UInt32
                | Int32
                | UInt64
                | Int64
                | NamedType of string
                | Pointer of ty
                | Tuple of ty list
                | TypeCons of string * ty list

    datatype exp_ast = BoolConstant of bool
                     | IntConstant of string
                     | FloatConstant of string
                     | NullConstant
                     | Variable of string
                     | Cast of ty * exp_ast
                     | Load of exp_ast
                     | AddressOf of exp_ast
                     | SizeOf of ty
                     | CreateTuple of exp_ast list
                     | AccessTuple of exp_ast * int
                     | Funcall of string * ty list * exp_ast list

    datatype block_ast = Sequence of block_ast list
                       | Block of block_ast list
                       | Declare of ty * string * exp_ast
                       | Assign of exp_ast * exp_ast
                       | Store of exp_ast * exp_ast
                       | Cond of exp_ast * block_ast * block_ast

    type name = string

    datatype top_ast = Defun of name * typaram list * param list * ty * block_ast * exp_ast
                     | ToplevelProgn of top_ast list
         and typaram = TypeParam of name
         and param = Param of name * ty
         and slot = Slot of name * ty

    val transformTy : Type.ty -> ty
    val transformExp : HIR.ast -> (block_ast list * exp_ast)
    val transformTop : HIR.top_ast -> top_ast
end
