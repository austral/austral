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

signature UTIL = sig
    datatype 'a result = Result of 'a
                       | Failure of string

    val valOf : 'a result -> 'a

    type path = string
    val readFileToString : path -> string
    val writeStringToFile : path -> string -> unit

    val member : ''a -> ''a list -> bool
    val position : ''a -> ''a list -> int option

    val mapidx : (('a * int) -> 'b) -> 'a list -> 'b list
    val butlast : 'a list -> 'a list

    type prefix = string

    (* If string starts with prefix, return the remainder of string. Otherwise
       return NONE. *)
    val afterPrefix : string -> prefix -> string option

    val foldThread : ('a -> 'b -> ('c * 'b)) -> 'a list -> 'b -> ('c list * 'b)
end
