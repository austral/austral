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

signature LIR_PASS = sig
    type ty = LIR.ty

    (* Tuples *)

    type tuple_types

    val emptyTupleTypes : tuple_types

    val getTuple : tuple_types -> ty list -> ty option
    val addTuple : tuple_types -> ty list -> (ty * tuple_types)

    (* Transform types *)

    val transformType : tuple_types -> MIR.ty -> ty * tuple_types

    (* Transform code *)

    val transformOperand : tuple_types -> MIR.operand -> LIR.operand * tuple_types
end
