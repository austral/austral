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

structure CBackend :> C_BACKEND = struct
    type ty = CAst.ty

    (* Tuples *)

    type tuple_types = (ty list, ty) Map.map

    val emptyTupleTypes =
        Map.empty

    fun getTuple tt tys =
        Map.get tt tys

    fun addTuple tt tys id =
        Map.iadd tt (tys,
                     CAst.NamedType ("_A_tuple_" ^ (Int.toString id)))

    (* Transform types *)

    val boolType = CAst.NamedType "_A_bool"

    local
        open CAst
    in
        fun transformType tt HIR.Unit =
            (boolType, tt)
          | transformType tt HIR.Bool =
            (boolType, tt)
          | transformType _ _ =
            raise Fail "Not implemented yet"
    end
end
