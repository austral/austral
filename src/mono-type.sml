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

structure MonoType :> MONO_TYPE = struct
    type name = Symbol.symbol

    (* Monomorphic types *)

    datatype ty = Unit
                | Bool
                | Integer of signedness * width
                | Float of float_type
                | Tuple of ty list
                | Pointer of ty
                | ForeignPointer of ty
                | StaticArray of ty
                | Disjunction of name * variant list
         and signedness = Unsigned | Signed
         and width = Int8 | Int16 | Int32 | Int64
         and float_type = Single | Double
         and variant = Variant of name * ty option

    (* Type monomorphization *)

    datatype type_monomorphs = TypeMonos of ((name * ty list), ty) Map.map

    type replacements = (name, ty) Map.map

    fun monomorphize tm _ Type.Unit =
        (Unit, tm)
      | monomorphize tm _ Type.Bool =
        (Bool, tm)
      | monomorphize tm _ (Type.Integer (s, w)) =
        (Integer (mapSignedness s, mapWidth w), tm)
      | monomorphize tm _ (Type.Float f) =
        (Float (mapFloat f), tm)
      | monomorphize m (Type.Tuple tys) =
        Tuple (map (monomorphize m) tys)
      | monomorphize m (Type.Pointer ty) =
        Pointer (monomorphize m ty)
      | monomorphize m (Type.ForeignPointer ty) =
        Pointer (monomorphize m ty)
      | monomorphize m (Type.StaticArray ty) =
        Array (monomorphize m ty)
      | monomorphize m (Type.Disjunction (name, _, variants)) =
        Disjunction (name, map (monomorphizeVariant m) variants)
      | monomorphize m (Type.TypeVariable name) =
        (case Map.get m name of
             SOME ty => ty
           | NONE => raise Fail ("Error during monomorphization: no replacement for the type variable '"
                                 ^ (Symbol.toString name)
                                 ^ "' found"))

    and mapSignedness Type.Unsigned = Unsigned
      | mapSignedness Type.Signed = Signed

    and mapWidth Type.Int8 = Int8
      | mapWidth Type.Int16 = Int16
      | mapWidth Type.Int32 = Int32
      | mapWidth Type.Int64 = Int64

    and mapFloat Type.Single = Single
      | mapFloat Type.Double = Double

    and monomorphizeVariant m (Type.Variant (name, SOME ty)) =
        Variant (name, SOME (monomorphize m ty))
      | monomorphizeVariant _ (Type.Variant (name, NONE)) =
        Variant (name, NONE)
end
