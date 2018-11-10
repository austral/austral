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

    val count = ref 0
    fun freshId () =
        (count := !count + 1;
         !count)

    fun addTuple tt tys =
        case getTuple tt tys of
            (SOME _) => tt
          | NONE => let val id = freshId ()
                    in
                        Map.iadd tt (tys,
                                     CAst.NamedType ("_A_tuple_" ^ (Int.toString id)))
                    end

    (* Transform types *)

    val boolType = CAst.NamedType "_A_bool"

    local
        open CAst
    in
        fun transformType tt HIR.Unit =
            (boolType, tt)
          | transformType tt HIR.Bool =
            (boolType, tt)
          | transformType tt (HIR.Integer (s, w)) =
            (NamedType (transformIntType s w), tt)
          | transformType tt (HIR.Float Type.Single) =
            (NamedType "float", tt)
          | transformType tt (HIR.Float Type.Double) =
            (NamedType "double", tt)
          | transformType tt (HIR.Tuple tys) =
            let val (tys', tt) = Util.foldThread (fn (ty, tt) =>
                                                     transformType tt ty)
                                                 tys
                                                 tt
            in
                case getTuple tt tys' of
                    (SOME ty) => (ty, NamedType tt')
                  | NONE => let val tt = addTuple tt tys
            end
          | transformType _ _ =
            raise Fail "Not implemented yet"

        and transformIntType Type.Unsigned Type.Int8 =
            "uint8_t"
          | transformIntType Type.Signed   Type.Int8 =
            "int8_t"
          | transformIntType Type.Unsigned Type.Int16 =
            "uint16_t"
          | transformIntType Type.Signed   Type.Int16 =
            "int16_t"
          | transformIntType Type.Unsigned Type.Int32 =
            "uint32_t"
          | transformIntType Type.Signed   Type.Int32 =
            "int32_t"
          | transformIntType Type.Unsigned Type.Int64 =
            "uint64_t"
          | transformIntType Type.Signed   Type.Int64 =
            "int64_t"
    end
end
