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

signature MONO_TYPE = sig
    type name = Symbol.symbol

    (* Monomorphic types *)

    datatype ty = Unit
                | Bool
                | Integer of Type.signedness * Type.width
                | Float of Type.float_type
                | Tuple of ty list
                | Pointer of ty
                | ForeignPointer of ty
                | StaticArray of ty
                | Disjunction of name * int * variant list
         and variant = Variant of name * ty option

    (* Type monomorphization *)

    type type_monomorphs

    val emptyMonomorphs : type_monomorphs

    val getMonomorph : type_monomorphs -> name -> ty list -> ty option
    val addMonomorph : type_monomorphs -> name -> ty list -> ty -> type_monomorphs

    (* Return a list of all the monomorphs that are in the second argument but
       not in the first *)
    val newMonomorphs : type_monomorphs -> type_monomorphs -> (name * ty list * ty) list

    type replacements = (name, ty) Map.map

    val monomorphize : type_monomorphs -> replacements -> Type.ty -> (ty * type_monomorphs)
end
