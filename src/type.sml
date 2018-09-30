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
                | Disjunction of name * ty list * variant list
         and signedness = Unsigned | Signed
         and width = Int8 | Int16 | Int32 | Int64
         and float_type = Single | Double
         and variant = Variant of name * ty

    datatype typespec = NamedType of Symbol.symbol
                      | TypeCons of Symbol.symbol * (typespec list)

    datatype typedef = BuiltInType of name * ty
                     | TypeAlias of name * param list * typespec
                     | Datatype of name * param list * variant list

    type tenv = (Symbol.symbol, typedef) Map.map

    val defaultTenv =
        let fun au name =
                Symbol.mkSymbol (Ident.mkIdentEx "austral",
                                 Ident.mkIdentEx name)
            and toMap l = Map.fromList (map (fn (n, t) => (au n, BuiltInType (au n, t))) l)
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

    fun parseTypespec (RCST.Symbol s) = NamedType s
      | parseTypespec (RCST.List l) = parseTypespecList l
      | parseTypespec _ = raise Fail "Invalid type specifier"
    and parseTypespecList ((RCST.Symbol f)::args) = TypeCons (f, map parseTypespec args)
      | parseTypespecList _ = raise Fail "Invalid type constructor"
end
