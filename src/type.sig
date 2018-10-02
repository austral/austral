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

signature TYPE = sig
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
         and variant = Variant of name * ty option

    type typespec

    type typarams = param OrderedSet.set

    datatype typedef = BuiltInType of name * ty
                     | TypeAlias of name * typarams * typespec
                     | Datatype of name * typarams * variant_spec list
         and variant_spec = VariantSpec of name * typespec option

    type tenv

    val defaultTenv : tenv
    val getTypedef : tenv -> name -> typedef option
    val addTypeAlias : tenv -> (name * typarams * typespec) -> tenv option
    val addDisjunction : tenv -> (name * typarams * variant_spec list) -> tenv option

    val parseTypespec : RCST.rcst -> typespec
    val replaceArgs : typarams -> typespec list -> (param, typespec) Map.map
    val replace : (param, typespec) Map.map -> typespec -> typespec
    val resolve : tenv -> typespec -> ty
end
