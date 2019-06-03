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

structure Map :> MAP = struct
    datatype (''k, 'v) map = Map of (''k * 'v) list

    val empty = Map []

    fun mapl (Map m) = m

    fun get (Map ((k', v)::rest)) k = if k = k' then SOME v else get (Map rest) k
      | get (Map nil) _ = NONE

    fun add m (k, v) =
        case (get m k) of
            SOME _ => NONE
          | NONE => SOME (Map ((k, v) :: (mapl m)))

    fun iadd m (k, v) =
        case (get m k) of
            SOME _ => m
          | NONE => Map ((k, v) :: (mapl m))

    fun set (Map m) k v =
        case get (Map m) k of
            SOME _ => Map (map (fn (k', v') =>
                                   if k' = k then
                                       (k, v)
                                   else
                                       (k', v))
                               m)
          | NONE => iadd (Map m) (k, v)

    fun iaddList m (head::tail) =
        iadd (iaddList m tail) head
      | iaddList m nil = m

    fun size (Map m) = length m

    fun keys (Map l) =
        Set.fromList (map (fn (k, v) => k) l)

    fun fromList l =
        iaddList empty l

    fun toList (Map l) =
        l

    fun mergeMaps (Map a) (Map b) =
        Map (a @ b)
end
