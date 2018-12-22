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

    datatype kind = Linear
                  | Unrestricted

    datatype ty = Unit
                | Bool
                | Integer of signedness * width
                | Float of float_type
                | Tuple of ty list
                | Address of ty
                | PositiveAddress of ty
                | StaticArray of ty
                | Pointer of ty
                | Disjunction of name * kind * ty list
                | Record of name * kind * ty list
                | TypeVariable of name
         and signedness = Unsigned | Signed
         and width = Int8 | Int16 | Int32 | Int64 | IntSize
         and float_type = Single | Double

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
      | toString (Disjunction (name, tyargs)) =
        "("
        ^ (Symbol.toString name)
        ^ " "
        ^ (String.concatWith " " (map toString tyargs))
        ^ ")"
      | toString (Record (name, tyargs)) =
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
      | widthToString IntSize =
        "size"

    fun tyVars Unit = Set.empty
      | tyVars Bool = Set.empty
      | tyVars (Integer _) = Set.empty
      | tyVars (Float _) = Set.empty
      | tyVars (Tuple tys) = Set.unionList (map tyVars tys)
      | tyVars (Address ty) = tyVars ty
      | tyVars (PositiveAddress ty) = tyVars ty
      | tyVars (StaticArray ty) = tyVars ty
      | tyVars (Pointer ty) = tyVars ty
      | tyVars (Disjunction (_, tys)) =
        (Set.unionList (map tyVars tys))
      | tyVars (Record (_, tys)) =
        (Set.unionList (map tyVars tys))
      | tyVars (TypeVariable name) =
        Set.singleton (TypeParam name)

    type typarams = param OrderedSet.set

    (* Type environment *)

    datatype decltype = AliasDecl of ty
                      | DisjunctionDecl
                      | RecordDecl

    type declmap = (name, (typarams * decltype)) Map.map

    datatype typedef = AliasDef of ty
                     | DisjunctionDef of variant list
                     | RecordDef of (name, ty) Map.map
         and variant = Variant of name * ty option

    type defmap = (name, (typarams * typedef)) Map.map

    type tenv = { decls : declmap, defs : defmap }

    val defaultTenv =
        { decls = Map.empty, defs = Map.empty }

    fun getDeclaration (tenv: tenv) name =
        let val { decls, defs } = tenv
        in
            Map.get decls name
        end

    fun getDefinition (tenv: tenv) name =
        let val { decls, defs } = tenv
        in
            Map.get defs name
        end

    fun addDeclaration (tenv: tenv) (name, typarams, decltype) =
        let val { decls, defs } = tenv
        in
            case Map.add decls (name, (typarams, decltype)) of
                NONE => raise Fail "Duplicate type declaration"
              | (SOME decls) => { decls = decls, defs = defs }
        end

    fun addDefinition (tenv: tenv) (name, typarams, typedef) =
        let val { decls, defs } = tenv
        in
            case Map.add defs (name, (typarams, typedef)) of
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

    (* Builtin types *)

    val builtInScalars = Map.fromList
                             (map (fn (n, t) => (Symbol.au n, t)) [
                                   ("unit", Unit),
                                   ("boolean", Bool),
                                   ("u8", Integer (Unsigned, Int8)),
                                   ("i8", Integer (Signed, Int8)),
                                   ("u16", Integer (Unsigned, Int16)),
                                   ("i16", Integer (Signed, Int16)),
                                   ("u32", Integer (Unsigned, Int32)),
                                   ("i32", Integer (Signed, Int32)),
                                   ("u64", Integer (Unsigned, Int64)),
                                   ("i64", Integer (Signed, Int64)),
                                   ("usize", Integer (Unsigned, IntSize)),
                                   ("isize", Integer (Signed, IntSize)),
                                   ("f32", Float Single),
                                   ("f64", Float Double)
                               ])

    val scalarTypeNames =
        Set.toList (Map.keys builtInScalars)

    val builtInAggregateNames = map Symbol.au [
            "tuple",
            "address",
            "paddress",
            "static-array"
        ]

    fun isBuiltin name =
        List.exists (fn n => n = name)
                    (scalarTypeNames @ builtInAggregateNames)

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
      | replaceVars m (Disjunction (name, args)) =
        Disjunction (name, map (replaceVars m) args)
      | replaceVars _ ty =
        ty

    fun replacements typarams tyargs =
        Map.fromList (Util.mapidx (fn (TypeParam p, idx) => (p, List.nth (tyargs, idx)))
                                  (OrderedSet.toList typarams))

    fun resolve tenv params (TypeCons (name, tyargs)) =
        let val tyargs' = map (resolve tenv params) tyargs
        in
            if isBuiltin name then
                (* If the type specifier names a built-in type or built-in type
                   constructor, call resolveBuiltin *)
                resolveBuiltin tenv params name tyargs
            else
                (* Otherwise, we're dealing with (potentially) a user-defined
                   type or type variable. *)
                if Set.isIn params (TypeParam name) then
                    resolveTypeVariable name tyargs
                else
                    (* Since it's not a builtin and not a type variable, we're
                       dealing with a user-defined type. Try to find if it
                       exists. *)
                    resolveUserDefined tenv params name tyargs tyargs'
        end

    and resolveBuiltin tenv params name args =
        (case Map.get builtInScalars name of
             (SOME t) => t
           | NONE =>
             if List.exists (fn n => n = name) builtInAggregateNames then
                 if name = Symbol.au "tuple" then
                     resolveTuple tenv params args
                 else if name = Symbol.au "address" then
                     resolveAddress tenv params args
                 else if name = Symbol.au "paddress" then
                     resolvePAddress tenv params args
                 else
                     raise Fail "Unknown builtin"
             else
                 raise Fail "Internal compiler error: not a builtin")

    and resolveTypeVariable name tyargs =
        (* If the constructor name is in the set of type parameters, it's a type
           variable. We also have to make sure that there are no args, that is,
           that this type variable doesn't appear as a constructor, *)
        if List.length tyargs = 0 then
            TypeVariable name
        else
            (* TODO: higher-kinded types would be nice *)
            raise Fail "Type variables cannot be constructors"

    and resolveUserDefined tenv params name tyargs tyargs' =
        (case (getDeclaration tenv name) of
             (SOME (typarams, decltype)) =>
             if sameSize typarams tyargs' then
                 (* The arity matches, that is, we have exactly as many type
                    arguments as type parameters in the definition of this
                    type. *)
                 (case decltype of
                      (AliasDecl ty) => ty
                    (* If it's a disjunction, construct a ty instance from the
                       name and args *)
                    | (DisjunctionDecl) => resolveDisjunction name typarams tyargs'
                    (* If it's a record, construct a ty instance from the name
                       and args *)
                    | (RecordDecl) => resolveRecord name typarams tyargs')
             else
                 raise Fail "Type arity error"
           | NONE =>
             raise Fail ("No type named " ^ (Symbol.toString name)))

    and resolveAlias (tenv: tenv) (name: name) (tyargs: ty list) =
        (case getDefinition tenv name of
             (SOME (typarams, typedef)) =>
             (case typedef of
                  (AliasDef ty) => let val rs = replacements typarams tyargs
                                   in
                                       replaceVars rs ty
                                   end
                | _ => raise Fail "Internal compiler error: not a type alias")
           | NONE => raise Fail "Internal compiler error: no such type")

    and resolveDisjunction name typarams tyargs =
        let val rs = replacements typarams tyargs
        in
            let val tyargs' = map (replaceVars rs) tyargs
            in
                Disjunction (name, tyargs')
            end
        end

    and resolveRecord name typarams tyargs =
        let val rs = replacements typarams tyargs
        in
            let val tyargs' = map (replaceVars rs) tyargs
            in
                Record (name, tyargs')
            end
        end

    and resolveTuple tenv params tys =
        Tuple (map (resolve tenv params) tys)

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

    (* Utilities *)

    fun getDisjunctionVariants tenv name =
        (case getDefinition tenv name of
             (SOME (_, (DisjunctionDef vs))) => vs
           | _ => raise Fail ("Internal compiler error: getDisjunctionVariants: definition for type "
                              ^ (Symbol.toString name)
                              ^ " not found in the tenv"))

    fun getVariantByName variants name =
        List.find (fn (Variant (n, _)) => n = name) variants

    fun posInVariants variants name =
        Util.position name (map (fn (Variant (name, _)) => name) variants)

    fun getRecordSlots tenv name =
        (case getDefinition tenv name of
             (SOME (_, (RecordDef slots))) => slots
           | _ => raise Fail ("Internal compiler error: getRecordSlots: definition for type "
                              ^ (Symbol.toString name)
                              ^ " not found in the tenv"))
end
