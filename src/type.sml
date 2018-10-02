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
                | Disjunction of name * ty list * variant list
                | TypeVariable of name
         and signedness = Unsigned | Signed
         and width = Int8 | Int16 | Int32 | Int64
         and float_type = Single | Double
         and variant = Variant of name * ty

    datatype typespec = TypeCons of name * (typespec list)

    type typarams = param OrderedSet.set

    datatype typedef = BuiltInType of name * ty
                     | TypeAlias of name * typarams * typespec
                     | Datatype of name * typarams * variant list

    type tenv = (Symbol.symbol, typedef) Map.map

    val defaultTenv =
        let fun au name =
                Symbol.mkSymbol (Ident.mkIdentEx "austral",
                                 Ident.mkIdentEx name)
            and toMap l = Map.fromList (map (fn (n, t) =>
                                                (au n, BuiltInType (au n, t)))
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

    fun parseTypespec (RCST.Symbol s) = TypeCons (s, [])
      | parseTypespec (RCST.List l) = parseTypespecList l
      | parseTypespec _ = raise Fail "Invalid type specifier"
    and parseTypespecList ((RCST.Symbol f)::args) = TypeCons (f, map parseTypespec args)
      | parseTypespecList _ = raise Fail "Invalid type constructor"

    fun replaceArgs params args =
        Map.fromList (Util.mapidx (fn (p, idx) => (p, List.nth (args, idx)))
                                  (OrderedSet.toList params))

    (* Given a type specifier, and a map of type parameter names to type
       specifiers, replace all instances of the type parameters with their
       associated type specifiers *)
    fun replace m (TypeCons (name, tyargs)) =
        case (Map.get m (TypeParam name)) of
            SOME tyspec => if (List.length tyargs) > 0 then
                               raise Fail "Type parameter cannot be a constructor"
                           else
                               tyspec
          | NONE => TypeCons (name, map (replace m) tyargs)

    fun bothEmpty list set =
        let val ll = List.length list
            and sl = OrderedSet.size set
        in
            (ll = 0) andalso (sl = 0)
        end

    (* Given a type environment and a type constructor, resolve it to an actual type *)
    fun resolve tenv (TypeCons (name, tyargs)) =
        (case (getTypedef tenv name) of
             SOME (BuiltInType (_, ty)) => ty
           | SOME (TypeAlias (_, p, tys)) =>
             (* The name refers to an alias to another type specifier. Ensure
             the type constructor has as many arguments as the type alias has
             parameters *)
             if ((OrderedSet.size p) <> (List.length tyargs)) then
                 raise Fail "Type constructor arity error"
             else
                 (* Replace parameters in the type specifier with arguments from
                    the type constructor and resolve the resulting type
                    specifier
                 *)
                 resolve tenv (replace (replaceArgs p tyargs) tys)
           | SOME (Datatype (_, p, tyargs)) =>
             raise Fail "derp"
           | NONE => raise Fail "No such type")
end
