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

    fun addMonomorph (TypeMonos tm) name tyargs ty =
        TypeMonos (Map.iadd tm ((name, tyargs), ty))

    type replacements = (name, ty) Map.map

    fun monomorphize tm _ Type.Unit =
        (Unit, tm)
      | monomorphize tm _ Type.Bool =
        (Bool, tm)
      | monomorphize tm _ (Type.Integer (s, w)) =
        (Integer (mapSignedness s, mapWidth w), tm)
      | monomorphize tm _ (Type.Float f) =
        (Float (mapFloat f), tm)
      | monomorphize tm rs (Type.Tuple tys) =
        let val (tys', tm') = monomorphizeList tm rs tys
        in
            (Tuple tys', tm')
        end
      | monomorphize tm rs (Type.Pointer ty) =
        let val (ty', tm') = monomorphize tm rs ty
        in
            (Pointer ty', tm')
        end
      | monomorphize tm rs (Type.ForeignPointer ty) =
        let val (ty', tm') = monomorphize tm rs ty
        in
            (ForeignPointer ty', tm')
        end
      | monomorphize tm rs (Type.StaticArray ty) =
        let val (ty', tm') = monomorphize tm rs ty
        in
            (StaticArray ty', tm')
        end
      | monomorphize tm rs (Type.Disjunction (name, tyargs, variants)) =
        (* Check the table of type monomorphs for this name and type arguments *)
        (case Map.get tm (name, tyargs) of
             SOME ty => (ty, tm)
           | NONE =>
             (* If this pair of name+type args is not present in the table of
                monomorphs, add it *)
             let val (tyargs', tm') = monomorphizeList tm rs tyargs
             in
                 let val (variants', tm'') = monomorphizeVariants tm' rs variants
                 in
                     let val disj = Disjunction (name, variants')
                     in
                         let val tm''' = addMonomorph tm'' name tyargs' disj
                         in
                             (disj, tm''')
                         end
                     end
                 end
             end)
      | monomorphize tm rs (Type.TypeVariable name) =
        (case Map.get rs name of
             SOME ty => (ty, tm)
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

    and monomorphizeList tm rs (head::tail) =
        let val (mono, tm') = monomorphize tm rs head
        in
            let val (rest, tm'') = monomorphizeList tm' rs tail
            in
                (mono :: rest, tm'')
            end
        end
      | monomorphizeList tm rs nil =
        (nil, tm)

    and monomorphizeVariants tm rs (head::tail) =
        let val (mono, tm') = monomorphize tm rs head
        in
            let val (rest, tm'') = monomorphizeList tm' rs tail
            in
                (mono :: rest, tm'')
            end
        end
      | monomorphizeVariants tm rs nil =
        (nil, tm)

    and monomorphizeVariant tm rs (Type.Variant (name, SOME ty)) =
        let val (ty', tm') = monomorphize tm rs ty
        in
            (Variant (name, ty'), tm')
        end
      | monomorphizeVariant _ _ (Type.Variant (name, NONE)) =
        Variant (name, NONE)
end
