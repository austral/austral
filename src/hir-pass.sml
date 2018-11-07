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

structure HirPass :> HIR_PASS = struct
    open HIR
    structure MT = MonoType

    fun transformType MT.Unit =
        Unit
      | transformType MT.Bool =
        Bool
      | transformType (MT.Integer (s, w)) =
        Integer (s, w)
      | transformType (MT.Float f) =
        Float f
      | transformType (MT.Tuple tys) =
        Tuple (map transformType tys)
      | transformType (MT.Pointer ty) =
        Pointer (transformType ty)
      | transformType (MT.ForeignPointer ty) =
        Pointer (transformType ty)
      | transformType (MT.StaticArray ty) =
        StaticArray (transformType ty)
      | transformType (MT.Disjunction (name, id, _)) =
        Disjunction (name, id)

    structure M = MTAST

    fun transform M.UnitConstant =
        UnitConstant
      | transform (M.BoolConstant b) =
        BoolConstant b
      | transform (M.IntConstant (i, ty)) =
        IntConstant (i, transformType ty)

    fun transformTop _ =
        raise Fail "Not done yet"
end
