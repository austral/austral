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

signature ORDERED_SET = sig
  type ''a set

  val empty : ''a set
  val singleton : ''a -> ''a set

  val add : ''a set -> ''a -> ''a set
  val addList : ''a set -> ''a list -> ''a set

  val union : ''a set -> ''a set -> ''a set
  val unionList : ''a set list -> ''a set

  val difference : ''a set -> ''a set -> ''a set

  val size : ''a set -> int
  (* positions start at 1 *)
  val positionOf : ''a set -> ''a -> int option
  val nth : ''a set -> int -> ''a

  val filter : ''a set -> (''a -> bool) -> ''a set

  val fromList : ''a list -> ''a set
  val toList : ''a set -> ''a list

  val toUnordered : ''a set -> ''a Set.set
end
