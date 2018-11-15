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
                | ForeignPointer of ty
                | StaticArray of ty
                | Disjunction of name * ty list * variant list
                | TypeVariable of name
         and signedness = Unsigned | Signed
         and width = Int8 | Int16 | Int32 | Int64
         and float_type = Single | Double
         and variant = Variant of name * ty option

    val isInteger : ty -> bool
    val isFloat : ty -> bool
    val isNumeric : ty -> bool
    val isComparable : ty -> bool
    val toString : ty -> string

    val tyVars : ty -> param Set.set

    val getVariantByName : variant list -> name -> variant option
    val posInVariants : variant list -> name -> int option

    datatype typespec = TypeCons of name * (typespec list)

    type typarams = param OrderedSet.set

    datatype typedef = BuiltInType of name * ty
                     | TypeAlias of name * typarams * ty
                     | Datatype of name * typarams * variant list

    type tenv

    val defaultTenv : tenv
    val getTypedef : tenv -> name -> typedef option
    val addTypeAlias : tenv -> (name * typarams * ty) -> tenv option
    val addDisjunction : tenv -> (name * typarams * variant list) -> tenv option

    (* Given a map of type variable names to types, and a type, replace all type
       variables in the given type with the given names with the
       replacements. If a type variable present in the type has no replacement
       in the map, an exception is thrown *)
    val replaceVars : (name, ty) Map.map -> ty -> ty

    (* Given an ordered set of type parameters, and a list of types, return a
       map of type parameter names to types *)
    val replacements : typarams -> ty list -> (name, ty) Map.map

    (* Given a type environment, a set of generic type parameters, and a type
       specifier, resolve the type specifier to a type *)
    val resolve : tenv -> param Set.set -> typespec -> ty

    val parseTypespec : RCST.rcst -> typespec
end
