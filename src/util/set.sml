(*
    Copyright 2018–2019 Fernando Borretti <fernando@borretti.me>

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

    fun isIn set elem = Util.member elem set

    fun eq a b =
        (List.all (fn aelem => isIn b aelem) a)
        andalso (length a = length b)

    fun union a b = addList (addList empty a) b

    fun unionList l = foldl (fn (a, b) => union a b)
                            empty
                            l

    fun intersection a b =
        let val a' = List.filter (fn elem => isIn a elem) b
            and b' = List.filter (fn elem => isIn b elem) a
        in
            union a' b'
        end

    fun minus b a =
        List.filter (fn belem => not (isIn a belem)) b

    fun size set = List.length set

    fun fromList (x::xs) = add (fromList xs) x
      | fromList nil = empty

    fun toList l = l
end
