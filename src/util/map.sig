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

signature MAP = sig
    type (''k, 'v) map

    val empty : (''k, 'v) map
    val get : (''k, 'v) map -> ''k -> 'v option
    val add : (''k, 'v) map -> (''k * 'v) -> (''k, 'v) map option
    val iadd : (''k, 'v) map -> (''k * 'v) -> (''k, 'v) map
    val set : (''k, 'v) map -> ''k -> 'v -> (''k, 'v) map
    val iaddList : (''k, 'v) map -> (''k * 'v) list -> (''k, 'v) map
    val size : (''k, 'v) map -> int

    val keys : (''k, 'v) map -> ''k Set.set

    val fromList : (''k * 'v) list -> (''k, 'v) map
    val toList : (''k, 'v) map -> (''k * 'v) list

    val mergeMaps : (''k, 'v) map -> (''k, 'v) map -> (''k, 'v) map
end
