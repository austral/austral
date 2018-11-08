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

structure MIR :> MIR = struct
    type name = Symbol.symbol

    (* Type System *)

    datatype ty = Bool
                | Integer of Type.signedness * Type.width
                | Float of Type.float_type
                | Tuple of ty list
                | Pointer of ty
                | Disjunction of name * int

    (* AST *)

    datatype operand = BoolConstant of bool
                     | IntConstant of string * ty
                     | FloatConstant of string * ty
                     | NullConstant of ty
                     | RegisterOp of int
                     | VariableOp of Symbol.variable

    datatype operation = ArithOp of Arith.kind * Arith.oper * operand * operand
                       | TupleCreate of operand list
                       | TupleProj of operand * int
                       | Load of operand
                       | Store of { ptr : operand, value : operand }
                       | SizeOf of ty
end
