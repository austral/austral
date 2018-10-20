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

structure Set :> SET = struct
    type ''a set = ''a list

    val empty = []

    fun singleton a =
        [a]

    fun add set elem =
        if Util.member elem set then
            set
        else
            elem :: set

    fun addList set (x::xs) = add (addList set xs) x
      | addList set nil = set

    fun union a b = addList (addList empty a) b

    fun unionList l = foldl (fn (a, b) => union a b)
                            empty
                            l


    fun isIn set elem = Util.member elem set

    fun size set = List.length set

    fun fromList (x::xs) = add (fromList xs) x
      | fromList nil = empty

    fun toList l = l
end
