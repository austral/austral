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

    type 'a set = 'a Set.set

    datatype typedef = BuiltInType of name * ty
                     | TypeAlias of name * param set * typespec
                     | Datatype of name * param set * variant list

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

    fun parseTypespec (RCST.Symbol s) = NamedType s
      | parseTypespec (RCST.List l) = parseTypespecList l
      | parseTypespec _ = raise Fail "Invalid type specifier"
    and parseTypespecList ((RCST.Symbol f)::args) = TypeCons (f, map parseTypespec args)
      | parseTypespecList _ = raise Fail "Invalid type constructor"

    fun resolve tenv (NamedType name) =
        (case (getTypedef tenv name) of
             SOME (BuiltInType (_, ty)) => ty
           | SOME (TypeAlias (_, p, tys)) =>
             if p <> Set.empty then
                 raise Fail "Error: bare named type cannot identify a "
             else
                 resolve tenv tys
           | SOME (DataType (_, p, ty)) =>
             if p <> Set.empty then
                 raise Fail "Type mismatch: a named type cannot alias a parameterized type alias"
             else
                 ty
end
