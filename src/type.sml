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

structure Type :> TYPE = struct
    type name = Symbol.symbol

    datatype param = TypeParam of name

    datatype ty = Unit
                | Bool
                | Integer of signedness * width
                | Float of float_type
                | Tuple of ty list
                | Pointer of ty
                | ForeignPointer of ty
                | StaticArray of ty * int
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

    datatype typespec = TypeCons of name * (typespec list)
                      | TyIntArg of int

    type typarams = param OrderedSet.set

    datatype typedef = BuiltInType of name * ty
                     | TypeAlias of name * typarams * ty
                     | Datatype of name * typarams * variant list

    type tenv = (Symbol.symbol, typedef) Map.map

    val defaultTenv =
        let fun toMap l = Map.fromList (map (fn (n, t) =>
                                                (Symbol.au n,
                                                 BuiltInType (Symbol.au n, t)))
                                            l)
        in
            toMap [("unit",    Unit),
                   ("boolean", Bool),
                   ("u8",      Integer (Unsigned, Int8)),
                   ("i8",      Integer (Signed,   Int8)),
                   ("u16",     Integer (Unsigned, Int16)),
                   ("i16",     Integer (Signed,   Int16)),
                   ("u32",     Integer (Unsigned, Int32)),
                   ("i32",     Integer (Signed,   Int32)),
                   ("u64",     Integer (Unsigned, Int64)),
                   ("i64",     Integer (Signed,   Int64)),
                   ("f32",     Float Single),
                   ("f64",     Float Double)]
        end

    fun getTypedef tenv name =
        Map.get tenv name

    fun addTypeAlias tenv (name, params, def) =
        case (Map.get tenv name) of
            SOME _ => NONE (* Another type with this name exists *)
          | NONE => SOME (Map.iadd tenv (name, TypeAlias (name, params, def)))

    fun addDisjunction tenv (name, params, variants) =
        case (Map.get tenv name) of
            SOME _ => NONE (* Another type with this name exists *)
          | NONE => SOME (Map.iadd tenv (name, Datatype (name, params, variants)))

    fun parseTypespec (RCST.IntConstant i) =
        TyIntArg (Option.valOf (Int.fromString i))
      | parseTypespec (RCST.Symbol s) = TypeCons (s, [])
      | parseTypespec (RCST.List l) = parseTypespecList l
      | parseTypespec _ = raise Fail "Invalid type specifier"
    and parseTypespecList ((RCST.Symbol f)::args) =
        TypeCons (f, map parseTypespec args)
      | parseTypespecList _ = raise Fail "Invalid type constructor"

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
    | resolve _ _ (TyIntArg _) =
      raise Fail "Invalid type spec"

    and resolveStaticArray tenv params [typespec, TyIntArg len] =
        StaticArray (resolve tenv params typespec, len)
      | resolveStaticArray _ _ _ =
        raise Fail "Bad static-array type specifier"

    and replaceVariant m (Variant (name, SOME ty)) =
        Variant (name, SOME (replaceVars m ty))
      | replaceVariant _ (Variant (name, NONE)) =
        Variant (name, NONE)
end
