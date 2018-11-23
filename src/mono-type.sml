(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Austral.

    Austral is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Austral is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Austral.  If not, see <http://www.gnu.org/licenses/>.
*)

structure MonoType :> MONO_TYPE = struct
    type name = Symbol.symbol

    (* Monomorphic types *)

    datatype ty = Unit
                | Bool
                | Integer of Type.signedness * Type.width
                | Float of Type.float_type
                | Tuple of ty list
                | Address of ty
                | PositiveAddress of ty
                | StaticArray of ty
                | Pointer of ty
                | Disjunction of name * int * variant list
         and variant = Variant of name * ty option

    (* Fresh monomorph ids *)

    val id = ref 0

    fun freshId () =
        let
        in
            id := !id + 1;
            !id
        end

    (* Type monomorphization *)

    datatype type_monomorphs = TypeMonos of ((name * ty list), (ty * int)) Map.map

    type monomorph = name * ty list * ty

    val emptyMonomorphs =
        TypeMonos Map.empty

    fun getMonomorph (TypeMonos tm) name tyargs =
        Map.get tm (name, tyargs)

    fun addMonomorph (TypeMonos tm) name tyargs ty id =
        TypeMonos (Map.iadd tm ((name, tyargs), (ty, id)))

    fun newMonomorphs (TypeMonos old) (TypeMonos new) =
        let val oldKeys = Map.keys old
            and newKeys = Map.keys new
        in
            let val newKeys' = Set.minus newKeys oldKeys
            in
                map (fn k => let val (ty, id) = Option.valOf (Map.get new k)
                                 and (name, args) = k
                             in
                                 (name, args, ty, id)
                             end)
                    (Set.toList newKeys')
            end
        end

    type replacements = (name, ty) Map.map

    fun monomorphize tm _ Type.Unit =
        (Unit, tm)
      | monomorphize tm _ Type.Bool =
        (Bool, tm)
      | monomorphize tm _ (Type.Integer (s, w)) =
        (Integer (s, w), tm)
      | monomorphize tm _ (Type.Float f) =
        (Float f, tm)
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
      | monomorphize tm rs (Type.Address ty) =
        let val (ty', tm') = monomorphize tm rs ty
        in
            (Address ty', tm')
        end
      | monomorphize tm rs (Type.PositiveAddress ty) =
        let val (ty', tm') = monomorphize tm rs ty
        in
            (PositiveAddress ty', tm')
        end
      | monomorphize tm rs (Type.StaticArray ty) =
        let val (ty', tm') = monomorphize tm rs ty
        in
            (StaticArray ty', tm')
        end
      | monomorphize tm rs (Type.Disjunction (name, tyargs, variants)) =
        let val (tyargs', tm) = monomorphizeList tm rs tyargs
        in
            (*print ("SEARCH FOR MONOMORPH " ^ (Symbol.toString name) ^ " with args: {" ^ (String.concatWith ", " (map Type.toString tyargs)) ^ "}\n");*)
            (* Check the table of type monomorphs for this name and type arguments *)
            case getMonomorph tm name tyargs' of
                SOME (ty, _) => (ty, tm)
              | NONE =>
                (* If this pair of name+type args is not present in the table of
                   monomorphs, add it *)
                case getMonomorph tm name tyargs' of
                    (SOME (ty, _)) => (ty, tm)
                  | NONE => let val id = freshId ()
                            in
                                (*print ("NEW MONOMORPH: " ^ (Int.toString id) ^ "\n");*)
                                let val (variants', tm) = monomorphizeVariants tm rs variants
                                in
                                    let val disj = Disjunction (name, id, variants')
                                    in
                                        let val tm = addMonomorph tm name tyargs' disj id
                                        in
                                            (disj, tm)
                                        end
                                    end
                                end
                            end
        end
      | monomorphize tm rs (Type.TypeVariable name) =
        (case Map.get rs name of
             SOME ty => (ty, tm)
           | NONE => raise Fail ("Error during monomorphization: no replacement for the type variable '"
                                 ^ (Symbol.toString name)
                                 ^ "' found"))

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
        let val (variant, tm') = monomorphizeVariant tm rs head
        in
            let val (rest, tm'') = monomorphizeVariants tm' rs tail
            in
                (variant :: rest, tm'')
            end
        end
      | monomorphizeVariants tm rs nil =
        (nil, tm)

    and monomorphizeVariant tm rs (Type.Variant (name, SOME ty)) =
        let val (ty', tm') = monomorphize tm rs ty
        in
            (Variant (name, SOME ty'), tm')
        end
      | monomorphizeVariant tm _ (Type.Variant (name, NONE)) =
        (Variant (name, NONE), tm)
end
