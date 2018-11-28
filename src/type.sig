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

signature TYPE = sig
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
                | Disjunction of name * ty list
                | TypeVariable of name
         and signedness = Unsigned | Signed
         and width = Int8 | Int16 | Int32 | Int64
         and float_type = Single | Double

    val isInteger : ty -> bool
    val isFloat : ty -> bool
    val isNumeric : ty -> bool
    val isComparable : ty -> bool
    val toString : ty -> string

    val tyVars : ty -> param Set.set

    type typarams = param OrderedSet.set

    (* The type environment has two components: declarations and definitions.

       The declarations part is a map from type names to: an ordered set of type
       parameters, and a value of type decltype that describes whether it's a
       type alias or a disjunction.

       The definitions part is a map from type names to: an ordered set of type
       parameters, a `ty` instance which is the type definition, and decltype
       instance that again specifies whether they are an alias or a
       disjunction. *)
    type tenv
    type decltype
    type typedef
    datatype variant = Variant of name * ty option

    val defaultTenv : tenv

    val getDeclaration : tenv -> name -> (typarams * decltype) option
    val getDefinition : tenv -> name -> (typarams * typedef) option
    val addDeclaration : tenv -> (name * typarams * decltype) -> tenv
    val addDefinition : tenv -> (name * typarams * typedef) -> tenv

    (* Type specifiers *)

    datatype typespec = TypeCons of name * (typespec list)

    val parseTypespec : RCST.rcst -> typespec

    (* Resolution *)

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

    (* Variant utilities *)

    val getVariantByName : variant list -> name -> variant option
    val posInVariants : variant list -> name -> int option
end
