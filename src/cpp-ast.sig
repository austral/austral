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

signature CPP_AST = sig
    datatype ty = NamedType of string
                | Pointer of ty
                | Struct of (ty * string) list
                | Union of (ty * string) list
                | TypeCons of string * ty list

    datatype exp_ast = BoolConstant of bool
                     | IntConstant of string
                     | FloatConstant of string
                     | StringConstant of string
                     | NullConstant
                     | Variable of string
                     | Binop of binop * exp_ast * exp_ast
                     | Cast of ty * exp_ast
                     | Deref of exp_ast
                     | AddressOf of exp_ast
                     | SizeOf of ty
                     | StructInitializer of string * (string * exp_ast) list
                     | StructAccess of exp_ast * string
                     | Funcall of string * ty list * exp_ast list
                     | Raw of string
         and binop = Add
                   | Sub
                   | Mul
                   | Div
                   | EqualTo
                   | NotEqualTo
                   | GreaterThan
                   | LessThan
                   | GreaterThanEq
                   | LessThanEq

    datatype block_ast = Sequence of block_ast list
                       | Block of block_ast list
                       | Declare of ty * string
                       | Assign of exp_ast * exp_ast
                       | Cond of exp_ast * block_ast * block_ast
                       | While of exp_ast * block_ast
                       | VoidFuncall of string * exp_ast list

    datatype top_ast = FunctionDef of string * typaram list * param list * ty * block_ast * exp_ast
                     | StructDef of string * typaram list * slot list
                     | ToplevelProgn of top_ast list
         and typaram = TypeParam of string
         and param = Param of string * ty
         and slot = Slot of string * ty

    val renderTop : top_ast -> string
end
