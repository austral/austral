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

signature ORDERED_MAP = sig
    type (''k, 'v) map

    val empty : (''k, 'v) map
    val get : (''k, 'v) map -> ''k -> 'v option
    val add : (''k, 'v) map -> (''k * 'v) -> (''k, 'v) map option

   (* Add a key-value pair to the map if it is not already present, otherwise,
      return the map as-is *)
    val iadd : (''k, 'v) map -> (''k * 'v) -> (''k, 'v) map

    val keys : (''k, 'v) map -> ''k OrderedSet.set
    val keyPosition : (''k, 'v) map -> ''k -> int option
end

structure OrderedMap :> ORDERED_MAP = struct
    datatype (''k, 'v) map = Map of (''k * 'v) list

    val empty =
        Map []

    fun get (Map ((k', v)::rest)) k =
        if k = k' then
            SOME v
        else
            get (Map rest) k
      | get (Map nil) _ =
        NONE

    fun add m (k, v) =
        case (get m k) of
            SOME _ => NONE
          | NONE => let val (Map l) = m
                    in
                        SOME (Map ((k, v) :: l))
                    end

    fun iadd m (k, v) =
        case add m (k, v) of
            SOME m' => m'
          | NONE => m

    fun keys (Map l) =
        OrderedSet.fromList (map (fn (k, _) => k) l)

    fun keyPosition (Map l) k =
        Util.position k (map (fn (k, _) => k) l)
end
