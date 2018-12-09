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

signature MONO_TYPE = sig
    type name = Symbol.symbol

    (* Monomorphic types *)

    datatype ty = Unit
                | Bool
                | Integer of Type.signedness * Type.width
                | Float of Type.float_type
                | Tuple of ty list
                | Address of ty
                | PositiveAddress of ty
                | StaticArray of ty
                | Pointer of ty
                | Disjunction of name * int
                | Record of name * int

    datatype variant = Variant of name * ty option

    datatype slot = Slot of name * ty

    val disjName : ty -> name

    (* Type monomorphization *)

    type type_monomorphs

    val emptyMonomorphs : type_monomorphs

    val getMonomorph : type_monomorphs -> name -> ty list -> (ty * int) option
    val addMonomorph : type_monomorphs -> name -> ty list -> ty -> int -> type_monomorphs

    (* Return a list of all the monomorphs that are in the second argument but
       not in the first *)
    val newMonomorphs : type_monomorphs -> type_monomorphs -> (name * ty list * ty * int) list

    type replacements = (name, ty) Map.map

    val monomorphize : type_monomorphs -> replacements -> Type.ty -> (ty * type_monomorphs)
    val monomorphizeVariants : type_monomorphs -> replacements -> Type.variant list -> (variant list * type_monomorphs)
    val monomorphizeSlots : type_monomorphs -> replacements -> Type.slot list -> (slot list * type_monomorphs)
end
