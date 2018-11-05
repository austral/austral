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

signature SET = sig
    type ''a set

    val empty : ''a set
    val singleton : ''a -> ''a set

    val add : ''a set -> ''a -> ''a set
    val addList : ''a set -> ''a list -> ''a set

    val eq : ''a set -> ''a set -> bool

    val union : ''a set -> ''a set -> ''a set
    val unionList : ''a set list -> ''a set

    val intersection : ''a set -> ''a set -> ''a set

    (* A-B : all the elements in A, except those also in B *)
    val minus : ''a set -> ''a set -> ''a set

    val isIn : ''a set -> ''a -> bool
    val size : ''a set -> int

    val fromList : ''a list -> ''a set
    val toList : ''a set -> ''a list
end
