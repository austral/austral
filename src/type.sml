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
    datatype param = TypeParam of Symbol.symbol

    datatype typespec = Unit
                      | Bool
                      | Integer of signedness * width
                      | Float of float_type
                      | NamedType of Symbol.symbol
                      | TypeCons of Symbol.symbol * (typespec list)
         and signedness = Unsigned | Signed
         and width = Int8 | Int16 | Int32 | Int64
         and float_type = Single
                        | Double

    datatype typedef = TypeAlias of param list * typespec
                     | Datatype of param list * typespec

    type tenv = (Symbol.symbol, typedef) Map.map

    val symbolTypes =
        let fun au name =
                Symbol.mkSymbol (Ident.mkIdentEx "austral",
                                 Ident.mkIdentEx name)
        in
            map (fn (n, t) => (au n, t))
                [("unit",    Unit),
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

    fun parseTypespec (RCST.Symbol s) = parseSymbolSpec s
      | parseTypespec _ = raise Fail "Invalid type specifier"
    and parseSymbolSpec sym =
        NamedType sym

end
