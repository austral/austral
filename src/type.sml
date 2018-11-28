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

structure Type :> TYPE = struct
    type name = Symbol.symbol

    datatype param = TypeParam of name

    datatype ty = Unit
                | Bool
                | Integer of signedness * width
                | Float of float_type
                | Tuple of ty list
                | Address of ty
                | PositiveAddress of ty
                | StaticArray of ty
                | Pointer of ty
                | Disjunction of name * ty list * variant list
                | TypeVariable of name
         and signedness = Unsigned | Signed
         and width = Int8 | Int16 | Int32 | Int64
         and float_type = Single | Double
         and variant = Variant of name * ty option

    fun isInteger (Integer _) = true
      | isInteger _ = false

    fun isFloat (Float _) = true
      | isFloat _ = false

    fun isNumeric t =
        isInteger t orelse isFloat t

    fun isComparable Bool =
        true
      | isComparable (Address _) =
        true
      | isComparable (PositiveAddress _) =
        true
      | isComparable t =
        isNumeric t

    fun toString Unit =
        "unit"
      | toString Bool =
        "boolean"
      | toString (Integer (s, w)) =
        (signednessToString s) ^ (widthToString w)
      | toString (Float Single) =
        "f32"
      | toString (Float Double) =
        "f64"
      | toString (Tuple tys) =
        "(tuple "
        ^ (String.concatWith " " (map toString tys))
        ^ ")"
      | toString (Pointer t) =
        "(pointer " ^ (toString t) ^ ")"
      | toString (Address t) =
        "(address " ^ (toString t) ^ ")"
      | toString (PositiveAddress t) =
        "(paddress " ^ (toString t) ^ ")"
      | toString (StaticArray t) =
        "(static-array " ^ (toString t) ^ ")"
      | toString (Disjunction (name, tyargs, _)) =
        "("
        ^ (Symbol.toString name)
        ^ " "
        ^ (String.concatWith " " (map toString tyargs))
        ^ ")"
      | toString (TypeVariable name) =
        Symbol.toString name

    and signednessToString Unsigned =
        "u"
      | signednessToString Signed =
        "i"

    and widthToString Int8 =
        "8"
      | widthToString Int16 =
        "16"
      | widthToString Int32 =
        "32"
      | widthToString Int64 =
        "64"

    fun tyVars Unit = Set.empty
      | tyVars Bool = Set.empty
      | tyVars (Integer _) = Set.empty
      | tyVars (Float _) = Set.empty
      | tyVars (Tuple tys) = Set.unionList (map tyVars tys)
      | tyVars (Address ty) = tyVars ty
      | tyVars (PositiveAddress ty) = tyVars ty
      | tyVars (StaticArray ty) = tyVars ty
      | tyVars (Pointer ty) = tyVars ty
      | tyVars (Disjunction (_, tys, variants)) =
        Set.union (Set.unionList (map tyVars tys))
                  (Set.unionList (map variantVars variants))
      | tyVars (TypeVariable name) =
        Set.singleton (TypeParam name)

    and variantVars (Variant (_, SOME ty)) =
        tyVars ty
      | variantVars (Variant (_, NONE)) =
        Set.empty

    fun getVariantByName variants name =
        List.find (fn (Variant (n, _)) => n = name) variants

    fun posInVariants variants name =
        Util.position name (map (fn (Variant (name, _)) => name) variants)

    type typarams = param OrderedSet.set

    (* Type environment *)

    datatype decltype = AliasDecl
                      | DisjunctionDecl

    type declmap = (name, (typarams * decltype)) Map.map

    type defmap = (name, (typarams * ty)) Map.map

    type tenv = { decls : declmap, defs : defmap }

    val defaultTenv =
        { decls = Map.empty, defs = Map.empty }

    fun addDeclaration tenv (name, typarams, decltype) =
        let val { decls, defs } = tenv
        in
            case Map.add decls (name, (typarams, decltype)) of
                NONE => raise Fail "Duplicate type declaration"
              | (SOME decls) => { decls = decls, defs = defs }
        end

    fun addDefinition tenv (name, typarams, ty, decltype) =
        let val { decls, defs } = tenv
        in
            case Map.add defs (name, (typarams, decltype)) of
                NONE => raise Fail "Duplicate type declaration"
              | (SOME defs) => { decls = decls, defs = defs }
        end

    (* Type specifiers *)

    datatype typespec = TypeCons of name * (typespec list)

    fun parseTypespec (RCST.Symbol s) =
        TypeCons (s, [])
      | parseTypespec (RCST.List l) =
        parseTypespecList l
      | parseTypespec _ =
        raise Fail "Invalid type specifier"
    and parseTypespecList ((RCST.Symbol f)::args) =
        TypeCons (f, map parseTypespec args)
      | parseTypespecList _ =
        raise Fail "Invalid type constructor"

    (* Resolution *)

    fun replaceArgs params args =
        Map.fromList (Util.mapidx (fn (p, idx) => (p, List.nth (args, idx)))
                                  (OrderedSet.toList params))

    (* Return whether the given set and the given list have the same size *)
    fun sameSize set list =
        (OrderedSet.size set) = (List.length list)

    fun replaceVars m (TypeVariable name) =
        (case (Map.get m name) of
             SOME ty => ty
           | NONE => raise Fail "Type parameter not present in replacements")
      | replaceVars m (Tuple tys) =
        Tuple (map (replaceVars m) tys)
      | replaceVars m (Disjunction (name, args, variants)) =
        let fun replaceVariant (Variant (name, SOME ty)) =
                Variant (name, SOME (replaceVars m ty))
              | replaceVariant (Variant (name, NONE)) =
                Variant (name, NONE)
        in
            Disjunction (name, args, map replaceVariant variants)
        end
      | replaceVars _ ty =
        ty

    fun replacements typarams tyargs =
        Map.fromList (Util.mapidx (fn (TypeParam p, idx) => (p, List.nth (tyargs, idx)))
                                  (OrderedSet.toList typarams))

    fun resolve tenv params (TypeCons (name, tyargs)) =
        if name = Symbol.au "static-array" then
            resolveStaticArray tenv params tyargs
        else if name = Symbol.au "address" then
            resolveAddress tenv params tyargs
        else if name = Symbol.au "paddress" then
            resolvePAddress tenv params tyargs
        else
            if Set.isIn params (TypeParam name) then
                TypeVariable name
            else
                let val tyargs' = map (resolve tenv params) tyargs
                in
                    (case (getTypedef tenv name) of
                         SOME (BuiltInType (_, ty)) =>
                         ty
                       | SOME (TypeAlias (_, params, ty)) =>
                         (* The name refers to an alias of another type. Ensure the type
                            constructor has as many arguments as the type alias has
                            parameters *)
                         if sameSize params tyargs' then
                             (* Replace parameters in the aliased type with arguments from
                                the type constructor *)
                             replaceVars (replacements params tyargs') ty
                         else
                             raise Fail "Type constructor arity error"
                       | SOME (Datatype (_, params, variants)) =>
                         (* The name refers to an algebraic data type. Ensure the type
                            constructor has as many arguments as the type alias has
                            parameters *)
                         if sameSize params tyargs' then
                             (* Replace parameters in the type with arguments from
                                the type constructor *)
                             let val m = replacements params tyargs'
                             in
                                 Disjunction (name,
                                              tyargs',
                                              map (replaceVariant m) variants)
                             end
                         else
                             raise Fail "Type constructor arity error"
                       | NONE =>
                         raise Fail ("No type named " ^ (Symbol.toString name)))
                end

    and resolveStaticArray tenv params [typespec] =
        StaticArray (resolve tenv params typespec)
      | resolveStaticArray _ _ _ =
        raise Fail "Bad static-array type specifier"

    and resolveAddress tenv params [typespec] =
        Address (resolve tenv params typespec)
      | resolveAddress _ _ _ =
        raise Fail "Bad address type specifier"

    and resolvePAddress tenv params [typespec] =
        PositiveAddress (resolve tenv params typespec)
      | resolvePAddress _ _ _ =
        raise Fail "Bad paddress type specifier"

    and replaceVariant m (Variant (name, SOME ty)) =
        Variant (name, SOME (replaceVars m ty))
      | replaceVariant _ (Variant (name, NONE)) =
        Variant (name, NONE)
end
