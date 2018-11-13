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

structure LIR :> LIR = struct
    type name = Symbol.symbol

    (* Type System *)

    datatype ty = Bool
                | Integer of Type.signedness * Type.width
                | Float of Type.float_type
                | Tuple of int
                | Pointer of ty
                | StaticArray of ty
                | Disjunction of name * int

    (* AST *)

    type register = int

    datatype operand = BoolConstant of bool
                     | IntConstant of string * ty
                     | FloatConstant of string * ty
                     | StringConstant of CST.escaped_string
                     | RegisterOp of register
                     | VariableOp of Symbol.variable * ty

    datatype operation = ArithOp of Arith.kind * Arith.oper * operand * operand
                       | TupleCreate of operand list
                       | TupleProj of operand * int
                       | ArrayLength of operand
                       | ArrayPointer of operand
                       | Load of operand
                       | Construct of ty * int * operand option
                       | UnsafeExtractCase of operand * int
                       | ForeignFuncall of string * operand list
                       | ForeignNull of ty
                       | SizeOf of ty
                       | AddressOf of Symbol.variable
                       | Cast of ty * operand
                       | ConcreteFuncall of name * operand list
                       | GenericFuncall of name * int * operand list

    datatype instruction = Assignment of register * operation * ty
                         | DeclareLocal of Symbol.variable * ty * operand
                         | Cond of { test : operand,
                                     consequent : instruction list,
                                     alternate : instruction list,
                                     result : register,
                                     ty : ty }
                         | Store of { ptr : operand,
                                      value : operand,
                                     result : register,
                                     ty : ty }
                         | Case of operand * variant_case list * register * ty
                         | VoidForeignFuncall of string * operand list
         and variant_case = VariantCase of name * instruction list * operand * ty
end
